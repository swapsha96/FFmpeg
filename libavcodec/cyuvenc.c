/*
 * CYUV encoder
 * Copyright (C) 2006 Loren Meritt.
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 */

/**
 * @file
 * Creative YUV (CYUV) Video Encoder.
 */

#include "avcodec.h"
#include "dsputil.h"

typedef struct CyuvContext {
    AVCodecContext *avctx;
    AVFrame frame;
    int8_t deltas[3][16];
    uint8_t **y_frames;
    uint8_t **uv_frames;
    int n_frames, n_alloced;
} CyuvContext;

#define put_pack4to8(a,b) (*buf++ = (a) + ((b)<<4))

static int quantize_sample(int sample, int *pred, const int8_t *table, const uint8_t *itable)
{
    int p = *pred;
    int bi = itable[(sample - p) & 0xff];
    int res = (p + table[bi]) & 0xff;
    int error = res - sample;
    if((unsigned)(error + 0x80) > 0xff){
        if(error > 0)
            bi++;
        else
            bi--;
        bi &= 15;
        res = (p + table[bi]) & 0xff;
    }
    *pred = res;
    return bi;
}

typedef struct TrellisPath {
    int nibble;
    int prev;
} TrellisPath;

typedef struct TrellisNode {
    uint32_t ssd;
    int path;
    int pred;
} TrellisNode;

static int quantize_row_trellis(AVCodecContext *avctx, const uint8_t *samples,
                                uint8_t *dst, const int8_t *table, const uint8_t *itable, int n)
{
    const int frontier = 1 << avctx->trellis;
    const int max_paths = frontier*n;
    TrellisPath paths[max_paths], *p;
    TrellisNode node_buf[2][frontier];
    TrellisNode *nodep_buf[2][frontier];
    TrellisNode **nodes = nodep_buf[0]; // nodes[] is always sorted by .ssd
    TrellisNode **nodes_next = nodep_buf[1];
    int pathn = 0, i, j, k, d;

    memset(nodep_buf, 0, sizeof(nodep_buf));
    nodes[0] = &node_buf[0][0];
    paths[0].nibble = samples[0] >> 4;
    nodes[0]->pred = paths[0].nibble << 4;
    nodes[0]->path = pathn++;
    d = samples[0] - nodes[0]->pred;
    nodes[0]->ssd = d*d;
    if(samples[0] < 0xf0) {
        nodes[1] = &node_buf[0][1];
        paths[1].nibble = (samples[0] >> 4) + 1;
        nodes[1]->pred = paths[1].nibble << 4;
        nodes[1]->path = pathn++;
        d = samples[0] - nodes[1]->pred;
        nodes[1]->ssd = d*d;
        if(nodes[1]->ssd < nodes[0]->ssd)
            FFSWAP(TrellisNode *, nodes[1], nodes[0]);
    }

    for(i=1; i<n; i++) {
        TrellisNode *t = node_buf[i&1];
        const int sample = samples[i];
        int range = 2;
        memset(nodes_next, 0, frontier*sizeof(TrellisNode*));
        for(j=0; j<frontier && nodes[j]; j++) {
            const int pred = nodes[j]->pred;
            const int div = itable[(sample - pred) & 0xff];
            const int nmin = FFMAX(div-range, 0);
            const int nmax = FFMIN(div+range, 15);
            int nibble;
            range = 1;
            for(nibble=nmin; nibble<=nmax; nibble++) {
                int dec_sample = (pred + table[nibble]) & 0xff;
                int d = sample - dec_sample;
                uint32_t ssd = nodes[j]->ssd + d*d;
                if(nodes_next[frontier-1] && ssd >= nodes_next[frontier-1]->ssd)
                    continue;
                for(k=0; k<frontier && nodes_next[k]; k++) {
                    if(dec_sample == nodes_next[k]->pred) {
                        assert(ssd >= nodes_next[k]->ssd);
                        goto next_nibble;
                    }
                }
                for(k=0; k<frontier; k++) {
                    if(!nodes_next[k] || ssd < nodes_next[k]->ssd) {
                        TrellisNode *u = nodes_next[frontier-1];
                        if(!u) {
                            u = t++;
                            u->path = pathn++;
                        }
                        u->ssd = ssd;
                        u->pred = dec_sample;
                        paths[u->path].nibble = nibble;
                        paths[u->path].prev = nodes[j]->path;
                        memmove(&nodes_next[k+1], &nodes_next[k], (frontier-k-1)*sizeof(TrellisNode*));
                        nodes_next[k] = u;
                        break;
                    }
                }
                next_nibble:;
            }
        }
        FFSWAP(TrellisNode **, nodes_next, nodes);
    }

    p = &paths[nodes[0]->path];
    for(i=n-1; i>=0; i--) {
        dst[i] = p->nibble;
        p = &paths[p->prev];
    }
    return nodes[0]->ssd;
}

static void build_inverse_table(uint8_t *itable, const int8_t *table)
{
    int d, i;
    for(i=1; i<16; i++)
        assert(table[i] > table[i-1]);
    for(d=-128; d<128; d++){
        int bscore = INT_MAX;
        for(i=0; i<16; i++){
            int score = FFABS(d - table[i]);
            if(score > bscore)
                break;
            bscore = score;
        }
        itable[d&0xff] = i-1;
    }
}

static int try_row(const uint8_t *samples, const int8_t *table,
                   const uint8_t *itable, int n)
{
    // trellis doesn't make much difference here, so just use the fast method
    int pred = av_clip_uint8(samples[0]+8) & -16;
    int d = samples[0] - pred;
    int ssd = d*d;
    int i;
    for(i=1; i<n; i++){
        quantize_sample(samples[i], &pred, table, itable);
        d = samples[i] - pred;
        ssd += d*d;
    }
    return ssd;
}

static uint64_t try_table(const int8_t *table, int8_t *dst, uint64_t *score,
                          uint8_t **pix, int pixn, int w, int h, int stride)
{
    uint64_t ssd = 0;
    uint8_t itable[256];
    int y, i;
    build_inverse_table(itable, table);
    for(i=0; i<pixn; i++)
        for(y=0; y<h; y++)
            ssd += try_row(pix[i]+y*stride, table, itable, w);
    if(ssd < *score){
        *score = ssd;
        memcpy(dst, table, 16);
    }
    return ssd;
}

#define N_TABLES 4
static const int8_t some_tables[2][N_TABLES][16] = {
   {{-120, -77, -51, -36, -22, -13, -8, -3, 0, 3, 8, 16, 28, 41, 60, 96},
    {-104, -73, -48, -29, -16,  -9, -4, -1, 0, 1, 4,  9, 16, 26, 42, 68},
    { -47, -28, -17, -10,  -5,  -2, -1,  0, 1, 2, 5,  8, 13, 20, 31, 47},
    { -30, -19, -12,  -7,  -4,  -3, -2, -1, 0, 1, 2,  3,  6, 10, 17, 29},
},{
    { -25, -14,  -9,  -6,  -3,  -2, -1,  0, 1, 2, 3,  6,  9, 14, 21, 34},
    { -26, -17, -12,  -9,  -6,  -3, -2, -1, 0, 1, 2,  5,  8, 11, 16, 23},
    { -43, -28, -19, -12,  -7,  -4, -1,  0, 1, 4, 7, 12, 17, 24, 33, 46},
    { -52, -38, -27, -19, -12,  -7, -4, -1, 0, 1, 4,  9, 16, 25, 36, 52},
}};
static const int8_t a_table[2][16] = {
    {-116, -75, -48, -31, -18,  -9, -4, -1, 0, 1, 4,  9, 18, 32, 54, 95},
    { -39, -22, -13,  -8,  -5,  -2, -1,  0, 1, 2, 5,  8, 13, 20, 31, 48},
};

static uint64_t build_table(CyuvContext *s, int8_t *table, uint8_t **pix, int pixn, int plane, int w,
int stride)
{
    const int h = s->avctx->height;
    int i, j;
    uint64_t ssd, bssd = 1ULL<<63;

    for(i=0; i<N_TABLES; i++){
        ssd = try_table(some_tables[!!plane][i], table, &bssd, pix, pixn, w, h, stride);
        if(ssd == 0)
            return ssd;
    }

    // Iterative refinement:
    // 16-dimensional diamond search, encoding the whole frame at each step
    if(s->avctx->context_model >= 2){
        int8_t tmp_buf[18];
        int8_t *tmp = tmp_buf+1;
        tmp[-1] = -128;
        tmp[16] = 127;
        try_table(s->deltas[plane], table, &bssd, pix, pixn, w, h, stride);

        // modify 1 entry at a time
        for(j=0; j<99; j++){
            int bssd_bakj = bssd;
            for(i=0; i<16; i++){
                int v = table[i];
                int bssd_baki = bssd;
#if 0 // FIXME 0.21 PSNR gain with context == 2
                if(v>=-2 && v<=2)
                    continue;
#endif
                memcpy(tmp, table, 16);

                while(++tmp[i] < tmp[i+1]){
                    ssd = try_table(tmp, table, &bssd, pix, pixn, w, h, stride);
                    if(ssd > bssd)
                        break;
                }
                if(bssd < bssd_baki)
                    continue;

                tmp[i] = v;
                while(--tmp[i] > tmp[i-1]){
                    ssd = try_table(tmp, table, &bssd, pix, pixn, w, h, stride);
                    if(ssd > bssd)
                        break;
                }
            }
            if(bssd == bssd_bakj)
                break;
        }

        // modify 2 entries at a time
        if(s->avctx->context_model >= 3){
            for(j=0; j<99; j++){
                int bssd_bakj = bssd;
                for(i=0; i<16*16*4; i++){
                    int i0= i&15;
                    int i1= (i>>4)&15;
                    int d0= ((i>>8)&1)*2-1;
                    int d1= ((i>>9)&1)*2-1;

                    if(i0>=i1)
                        continue;
                    memcpy(tmp, table, 16);
                    tmp[i0] += d0;
                    tmp[i1] += d1;
                    if(   tmp[i0] >= tmp[i0+1] || tmp[i0] <= tmp[i0-1]
                       || tmp[i1] >= tmp[i1+1] || tmp[i1] <= tmp[i1-1])
                        continue;

                    try_table(tmp, table, &bssd, pix, pixn, w, h, stride);
                }
                if(bssd == bssd_bakj)
                    break;
            }
        }
        memcpy(s->deltas[plane], table, 16);
    }
    return bssd;
}

static void train_tables(CyuvContext *s, int plane)
{
    const int k = 4;
    const int w = s->avctx->width >> 2*plane;
    const int h = s->avctx->height;
    const int n = s->n_frames << plane;
    uint8_t **all_frames = plane ? s->uv_frames : s->y_frames;
    int i, j, pass;
    int8_t tables[k][16];
    memcpy(tables, some_tables[!!plane], sizeof(tables));

    s->avctx->context_model = 3;

    for(pass=0; pass<99; pass++){
        int8_t tables_bak[k][16], tmp[16];
        uint8_t *pix[k][n];
        int pixn[k], pixk[n];
        uint64_t ssdj[k], ssdi[n], ssdsum=0;
        memcpy(tables_bak, tables, sizeof(tables));
        memset(pixn, 0, sizeof(pixn));
        memset(ssdj, 0, sizeof(ssdj));
        // assign each frame to the table that best codes it
        for(i=0; i<n; i++){
            uint64_t bssd = 1ULL<<63;
            int bj = 0;
            for(j=0; j<k; j++){
                uint64_t ssd = try_table(tables[j], tmp, &bssd, &all_frames[i], 1, w, h, w);
                if(ssd == bssd)
                    bj = j;
            }
            ssdj[bj] += bssd;
            ssdi[i] = bssd;
            pixk[i] = bj;
            pix[bj][pixn[bj]++] = all_frames[i];
        }
        // check for tables with no assigned frames,
        // and assign them to the frames with the worst score
        for(j=0; j<k; j++){
            if(pixn[j] == 0){
                uint64_t wssd = 0;
                int wi = 0, wj, wn;
                for(i=0; i<n; i++){
                    if(ssdi[i] > wssd && pixn[pixk[i]] > 1){
                        wssd = ssdi[i];
                        wi = i;
                    }
                }
                assert(wssd>0);
                wj = pixk[wi];
                wn = pixn[wj];
                for(i=0; i<wn; i++){
                    if(pix[wj][i] == all_frames[wi]){
                        FFSWAP(uint8_t *, pix[wj][i], pix[wj][wn-1]);
                        break;
                    }
                }
                assert(i<wn);
                pix[j][0] = all_frames[wi];
                pixn[j]++;
                pixk[wi] = j;
                pixn[wj]--;
                memcpy(tables[j], tables[wj], 16); //dunno if this is good, but it's needed for guaranteed convergence
            }
        }
        // modify each table to better code the frames assigned to it
        for(j=0; j<k; j++){
            memcpy(s->deltas[plane], tables[j], 16);
            ssdsum += ssdj[j] = build_table(s, tables[j], pix[j], pixn[j], plane, w, w);
        }
        if(!memcmp(tables_bak, tables, sizeof(tables)))
            break;
    }
}

static int cyuv_encode_frame(AVCodecContext *avctx, unsigned char *buf, int buf_size, void *data)
{
    CyuvContext *s = avctx->priv_data;
    AVFrame *pict = data;
    int8_t *tables[3] = {buf, buf+16, buf+32};
    uint8_t itables[3][256];
    int x, y, p;
    const int coded_size = 48 + s->avctx->height * s->avctx->width * 3/4;
    assert(buf_size >= coded_size);
    s->frame = *pict;
    pict->error[0] = pict->error[1] = pict->error[2] = 0;

    if(avctx->context_model > 0){
        for(p=0; p<3; p++)
            build_table(s, tables[p], &s->frame.data[p], 1, p, s->avctx->width>>(p?2:0), s->frame.linesize[p]);
    }else{
        for(p=0; p<3; p++)
            memcpy(tables[p], a_table[!!p], 16);
    }
    for(p=0; p<3; p++)
        build_inverse_table(itables[p], tables[p]);
    buf += 48;

    for(y=0; y<s->avctx->height; y++){
        uint8_t *ptrs[3];
        int y0, y1;

        for(p=0; p<3; p++)
            ptrs[p] = pict->data[p] + y*pict->linesize[p];

        if(avctx->trellis > 0){
            uint8_t buf2[s->avctx->width*3/2];
            uint8_t *bufs[3] = {buf2, buf2+s->avctx->width, buf2+s->avctx->width*5/4};
            for(p=0; p<3; p++)
                pict->error[p] += quantize_row_trellis(avctx, ptrs[p], bufs[p], tables[p], itables[p], s->avctx->width>>(p?2:0));
            for(x=0; x<s->avctx->width; x+=4){
                put_pack4to8(bufs[0][x+0], bufs[1][x>>2]);
                put_pack4to8(bufs[0][x+1], bufs[2][x>>2]);
                put_pack4to8(bufs[0][x+2], bufs[0][x+3]);
            }
        }else{
            int y_pred = av_clip_uint8(ptrs[0][0]+8) & -16;
            int u_pred = av_clip_uint8(ptrs[1][0]+8) & -16;
            int v_pred = av_clip_uint8(ptrs[2][0]+8) & -16;
            put_pack4to8(y_pred>>4, u_pred>>4);
            y0 = quantize_sample(ptrs[0][1], &y_pred, tables[0], itables[0]);
            put_pack4to8(y0, v_pred>>4);
            y0 = quantize_sample(ptrs[0][2], &y_pred, tables[0], itables[0]);
            y1 = quantize_sample(ptrs[0][3], &y_pred, tables[0], itables[0]);
            put_pack4to8(y0, y1);
            for(x=4; x<s->avctx->width; x+=4){
                ptrs[0]+=4;
                ptrs[1]++;
                ptrs[2]++;
                y0 = quantize_sample(ptrs[0][0], &y_pred, tables[0], itables[0]);
                y1 = quantize_sample(ptrs[1][0], &u_pred, tables[1], itables[1]);
                put_pack4to8(y0, y1);
                y0 = quantize_sample(ptrs[0][1], &y_pred, tables[0], itables[0]);
                y1 = quantize_sample(ptrs[2][0], &v_pred, tables[2], itables[2]);
                put_pack4to8(y0, y1);
                y0 = quantize_sample(ptrs[0][2], &y_pred, tables[0], itables[0]);
                y1 = quantize_sample(ptrs[0][3], &y_pred, tables[0], itables[0]);
                put_pack4to8(y0, y1);
            }
        }
    }

    if(avctx->flags & CODEC_FLAG_PASS1){
        if(s->n_frames >= s->n_alloced){
            s->n_alloced = 2*s->n_alloced+1;
            s->y_frames = av_realloc(s->y_frames, s->n_alloced*sizeof(uint8_t*));
            s->uv_frames = av_realloc(s->uv_frames, 2*s->n_alloced*sizeof(uint8_t*));
        }
        s->y_frames[s->n_frames] = av_malloc(s->avctx->width*s->avctx->height);
        s->uv_frames[s->n_frames*2] = av_malloc(s->avctx->width*s->avctx->height/4);
        s->uv_frames[s->n_frames*2+1] = av_malloc(s->avctx->width*s->avctx->height/4);
        for(y=0; y<s->avctx->height; y++){
            memcpy(s->y_frames[s->n_frames]+y*s->avctx->width,
                   pict->data[0] + y*pict->linesize[0], s->avctx->width);
            memcpy(s->uv_frames[s->n_frames*2]+y*s->avctx->width/4,
                   pict->data[1] + y*pict->linesize[1], s->avctx->width/4);
            memcpy(s->uv_frames[s->n_frames*2+1]+y*s->avctx->width/4,
                   pict->data[2] + y*pict->linesize[2], s->avctx->width/4);
        }
        s->n_frames++;
    }

    s->frame = *pict;
    avctx->coded_frame = &s->frame;
    for(p=0; p<3; p++)
        avctx->error[p] += pict->error[p];
    return coded_size;
}

static int cyuv_encode_init(AVCodecContext *avctx)
{
    int i;
    CyuvContext *s = avctx->priv_data;

    s->avctx = avctx;
    /* width needs to be divisible by 4 for this codec to work */
    if((s->avctx->width & 0x3) || avctx->pix_fmt != PIX_FMT_YUV411P){
        av_log(avctx, AV_LOG_ERROR, "Cyuv requires 411p input\n");
        return -1;
    }
    avctx->bits_per_sample= 12;
    avctx->coded_frame= &s->frame;

    for(i=0; i<3; i++)
        memcpy(s->deltas[i], some_tables[!!i][0], 16);

    dsputil_static_init();

    return 0;
}

static int cyuv_encode_end(AVCodecContext *avctx)
{
    if(avctx->flags & CODEC_FLAG_PASS1){
        train_tables(avctx->priv_data, 0);
        train_tables(avctx->priv_data, 1);
    }
    return 0;
}

AVCodec ff_cyuv_encoder = {
    .name           = "cyuv",
    .type           = AVMEDIA_TYPE_VIDEO,
    .id             = CODEC_ID_CYUV,
    .priv_data_size = sizeof(CyuvContext),
    .init           = cyuv_encode_init,
    .encode         = cyuv_encode_frame,
    .close          = cyuv_encode_end,
    .pix_fmts       = (const enum PixelFormat[]) { PIX_FMT_YUV411P,
                                                   PIX_FMT_NONE },
    .long_name      = NULL_IF_CONFIG_SMALL("Creative YUV (CYUV)")
};
