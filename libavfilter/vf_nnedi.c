/*
 * Copyright (C) 2010-2011 Kevin Stone
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with FFmpeg; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <float.h>

#include "libavutil/common.h"
#include "libavutil/float_dsp.h"
#include "libavutil/imgutils.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "video.h"

typedef struct FrameData {
    uint8_t *paddedp[3];
    int padded_stride[3];
    int padded_width[3];
    int padded_height[3];

    uint8_t *dstp[3];
    int dst_stride[3];

    int field[3];

    int32_t *lcount[3];
    float *input;
    float *temp;
} FrameData;

typedef struct NNEDIContext {
    const AVClass *class;

    int fieldbased;
    char *weights_file;

    AVFloatDSPContext *fdsp;
    int nb_planes;
    int linesize[4];
    int planeheight[4];

    float *weights0;
    float *weights1[2];
    int asize;
    int nns;
    int xdia;
    int ydia;

    // Parameters.
    int field;
    int dh; // double height
    int process_plane;
    int nsize;
    int nnsparam;
    int qual;
    int etype;
    int pscrn;
    int fapprox;

    int max_value;

    void (*copyPad)(const AVFrame *, FrameData *, struct NNEDIContext *, int);
    void (*evalFunc_0)(struct NNEDIContext *, FrameData *);
    void (*evalFunc_1)(struct NNEDIContext *, FrameData *);

    // Functions used in evalFunc_0
    void (*readPixels)(const uint8_t *, const int, float *);
    void (*computeNetwork0)(struct NNEDIContext *s, const float *, const float *, uint8_t *);
    int32_t (*processLine0)(const uint8_t *, int, uint8_t *, const uint8_t *, const int, const int, const int);

    // Functions used in evalFunc_1
    void (*extract)(const uint8_t *, const int, const int, const int, float *, float *);
    void (*dotProd)(struct NNEDIContext *, const float *, const float *, float *, const int, const int, const float *);
    void (*expfunc)(float *, const int);
    void (*wae5)(const float *, const int, float *);
} NNEDIContext;

#define OFFSET(x) offsetof(NNEDIContext, x)
#define FLAGS AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_FILTERING_PARAM

static const AVOption nnedi_options[] = {
    {"weights_file",  NULL, OFFSET(weights_file),  AV_OPT_TYPE_STRING, {.str="nnedi3_weights.bin"}, 0, 0, FLAGS },
    {"field",         NULL, OFFSET(field),         AV_OPT_TYPE_INT, {.i64=0}, 0, 3, FLAGS },
    {"planes",        NULL, OFFSET(process_plane), AV_OPT_TYPE_INT, {.i64=7}, 0, 7, FLAGS },
    {"nsize",         NULL, OFFSET(nsize),         AV_OPT_TYPE_INT, {.i64=6}, 0, 6, FLAGS },
    {"nns",           NULL, OFFSET(nnsparam),      AV_OPT_TYPE_INT, {.i64=1}, 0, 4, FLAGS },
    {"qual",          NULL, OFFSET(qual),          AV_OPT_TYPE_INT, {.i64=1}, 1, 2, FLAGS },
    {"etype",         NULL, OFFSET(etype),         AV_OPT_TYPE_INT, {.i64=0}, 0, 1, FLAGS },
    {"pscrn",         NULL, OFFSET(pscrn),         AV_OPT_TYPE_INT, {.i64=2}, 0, 2, FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(nnedi);

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    NNEDIContext *s = ctx->priv;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);
    int ret;

    s->nb_planes = av_pix_fmt_count_planes(inlink->format);
    if ((ret = av_image_fill_linesizes(s->linesize, inlink->format, inlink->w)) < 0)
        return ret;

    s->planeheight[1] = s->planeheight[2] = FF_CEIL_RSHIFT(inlink->h, desc->log2_chroma_h);
    s->planeheight[0] = s->planeheight[3] = inlink->h;

    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_YUV410P, AV_PIX_FMT_YUV411P,
        AV_PIX_FMT_YUV420P, AV_PIX_FMT_YUV422P,
        AV_PIX_FMT_YUV440P, AV_PIX_FMT_YUV444P,
        AV_PIX_FMT_YUVJ444P, AV_PIX_FMT_YUVJ440P,
        AV_PIX_FMT_YUVJ422P, AV_PIX_FMT_YUVJ420P,
        AV_PIX_FMT_YUVJ411P,
        AV_PIX_FMT_GBRP,
        AV_PIX_FMT_GRAY8,
        AV_PIX_FMT_NONE
    };

    AVFilterFormats *fmts_list = ff_make_format_list(pix_fmts);
    if (!fmts_list)
        return AVERROR(ENOMEM);
    return ff_set_common_formats(ctx, fmts_list);
}

static void copyPad(const AVFrame *src, FrameData *frameData, NNEDIContext *d, int fn)
{
    const int off = 1 - fn;
    int plane, y, x;

    for (plane = 0; plane < d->nb_planes; ++plane) {
        if (!(d->process_plane & (1 << plane)))
            continue;

        const uint8_t *srcp = (const uint8_t *)src->data[plane];
        uint8_t *dstp = (uint8_t *)frameData->paddedp[plane];

        const int src_stride = src->linesize[plane];
        const int dst_stride = frameData->padded_stride[plane];

        const int src_height = d->planeheight[plane];
        const int dst_height = frameData->padded_height[plane];

        const int src_width = d->linesize[plane];
        const int dst_width = frameData->padded_width[plane];

        // Copy.
        if (!d->dh) {
            for (y = off; y < src_height; y += 2)
                memcpy(dstp + 32 + (6 + y) * dst_stride,
                       srcp + y * src_stride,
                       src_width * sizeof(uint8_t));
        } else {
            for (y = 0; y < src_height; y++)
                memcpy(dstp + 32 + (6 + y * 2 + off) * dst_stride,
                       srcp + y * src_stride,
                       src_width * sizeof(uint8_t));
        }

        // And pad.
        dstp += (6 + off) * dst_stride;
        for (y = 6 + off; y < dst_height - 6; y += 2) {
            for (x = 0; x < 32; ++x)
                dstp[x] = dstp[64 - x];

            int c = 2;
            for (x = dst_width - 32; x < dst_width; ++x, c += 2)
                dstp[x] = dstp[x - c];

            dstp += dst_stride * 2;
        }

        dstp = (uint8_t *)frameData->paddedp[plane];
        for (y = off; y < 6; y += 2)
            memcpy(dstp + y * dst_stride,
                   dstp + (12 + 2 * off - y) * dst_stride,
                   dst_width * sizeof(uint8_t));

        int c = 4;
        for (y = dst_height - 6 + off; y < dst_height; y += 2, c += 4)
            memcpy(dstp + y * dst_stride,
                   dstp + (y - c) * dst_stride,
                   dst_width * sizeof(uint8_t));
    }
}

static void elliott(float *data, const int n)
{
    int i;

    for (i = 0; i < n; ++i)
        data[i] = data[i] / (1.0f + FFABS(data[i]));
}

static void dotProd(NNEDIContext *s, const float *data, const float *weights, float *vals, const int n, const int len, const float *scale)
{
    int i;

    for (i = 0; i < n; ++i) {
        float sum;

        sum = s->fdsp->scalarproduct_float(data, &weights[i * len], len);

        vals[i] = sum * scale[0] + weights[n * len + i];
    }
}

static void dotProdS(NNEDIContext *s, const float *dataf, const float *weightsf, float *vals, const int n, const int len, const float *scale)
{
    const int16_t *data = (int16_t *)dataf;
    const int16_t *weights = (int16_t *)weightsf;
    const float *wf = (float *)&weights[n * len];
    int i, j;

    for (i = 0; i < n; ++i) {
        int sum = 0, off = ((i >> 2) << 3) + (i & 3);
        for (j = 0; j < len; ++j)
            sum += data[j] * weights[i * len + j];

        vals[i] = sum * wf[off] * scale[0] + wf[off + 4];
    }
}

static void computeNetwork0(NNEDIContext *s, const float *input, const float *weights, uint8_t *d)
{
    float temp[12], scale = 1.0f;
    dotProd(s, input, weights, temp, 4, 48, &scale);
    const float t = temp[0];
    elliott(temp, 4);
    temp[0] = t;
    dotProd(s, temp, weights + 4 * 49, temp + 4, 4, 4, &scale);
    elliott(temp + 4, 4);
    dotProd(s, temp, weights + 4 * 49 + 4 * 5, temp + 8, 4, 8, &scale);
    if (FFMAX(temp[10], temp[11]) <= FFMAX(temp[8], temp[9]))
        d[0] = 1;
    else
        d[0] = 0;
}

static void computeNetwork0_i16(NNEDIContext *s, const float *inputf, const float *weightsf, uint8_t *d)
{
    const float *wf = weightsf + 2 * 48;
    float temp[12], scale = 1.0f;
    dotProdS(s, inputf, weightsf, temp, 4, 48, &scale);
    const float t = temp[0];
    elliott(temp, 4);
    temp[0] = t;
    dotProd(s, temp, wf + 8, temp + 4, 4, 4, &scale);
    elliott(temp + 4, 4);
    dotProd(s, temp, wf + 8 + 4 * 5, temp + 8, 4, 8, &scale);
    if (FFMAX(temp[10], temp[11]) <= FFMAX(temp[8], temp[9]))
        d[0] = 1;
    else
        d[0] = 0;
}

static void pixel2float48(const uint8_t *t8, const int pitch, float *p)
{
    const uint8_t *t = (const uint8_t *)t8;
    int y, x;

    for (y = 0; y < 4; ++y)
        for (x = 0; x < 12; ++x)
            p[y * 12 + x] = t[y * pitch * 2 + x];
}

static void byte2word48(const uint8_t *t, const int pitch, float *pf)
{
    int16_t *p = (int16_t *)pf;
    int y, x;

    for (y = 0; y < 4; ++y)
        for (x = 0; x < 12; ++x)
            p[y * 12 + x] = t[y * pitch * 2 + x];
}

static int32_t processLine0(const uint8_t *tempu, int width, uint8_t *dstp8, const uint8_t *src3p8, const int src_pitch, const int max_value, const int chroma)
{
    uint8_t *dstp = (uint8_t *)dstp8;
    const uint8_t *src3p = (const uint8_t *)src3p8;

    int minimum = 0;
    int maximum = max_value - 1;
    // Technically the -1 is only needed for 8 and 16 bit input.

    int count = 0, x;
    for (x = 0; x < width; ++x) {
        if (tempu[x]) {
            int tmp = 19 * (src3p[x + src_pitch * 2] + src3p[x + src_pitch * 4]) - 3 * (src3p[x] + src3p[x + src_pitch * 6]);
            tmp /= 32;
            dstp[x] = FFMAX(FFMIN(tmp, maximum), minimum);
        } else {
            memset(dstp + x, 255, sizeof(uint8_t));
            ++count;
        }
    }
    return count;
}

// new prescreener functions
static void byte2word64(const uint8_t *t, const int pitch, float *p)
{
    int16_t *ps = (int16_t *)p;
    int y, x;

    for (y = 0; y < 4; ++y)
        for (x = 0; x < 16; ++x)
            ps[y * 16 + x] = t[y * pitch * 2 + x];
}

static void computeNetwork0new(NNEDIContext *s, const float *datai, const float *weights, uint8_t *d)
{
    int16_t *data = (int16_t *)datai;
    int16_t *ws = (int16_t *)weights;
    float *wf = (float *)&ws[4 * 64];
    float vals[8];
    int i, j;

    for (i = 0; i < 4; ++i) {
        int sum = 0;
        for (j = 0; j < 64; ++j)
            sum += data[j] * ws[(i << 3) + ((j >> 3) << 5) + (j & 7)];
        const float t = sum * wf[i] + wf[4 + i];
        vals[i] = t / (1.0f + FFABS(t));
    }
    for (i = 0; i < 4; ++i) {
        float sum = 0.0f;
        for (j = 0; j < 4; ++j)
            sum += vals[j] * wf[8 + i + (j << 2)];
        vals[4 + i] = sum + wf[8 + 16 + i];
    }
    int mask = 0;
    for (i = 0; i < 4; ++i) {
        if (vals[4 + i] > 0.0f)
            mask |= (0x1 << (i << 3));
    }
    ((int *)d)[0] = mask;
}

static void evalFunc_0(NNEDIContext *d, FrameData *frameData)
{
    float *input = frameData->input;
    const float *weights0 = d->weights0;
    float *temp = frameData->temp;
    uint8_t *tempu = (uint8_t *)temp;
    int plane, x, y;

    // And now the actual work.
    for (plane = 0; plane < d->nb_planes; ++plane) {
        if (!(d->process_plane & (1 << plane)))
            continue;

        const uint8_t *srcp = (const uint8_t *)frameData->paddedp[plane];
        const int src_stride = frameData->padded_stride[plane] / sizeof(uint8_t);

        const int width = frameData->padded_width[plane];
        const int height = frameData->padded_height[plane];

        uint8_t *dstp = (uint8_t *)frameData->dstp[plane];
        const int dst_stride = frameData->dst_stride[plane] / sizeof(uint8_t);

        for (y = 1 - frameData->field[plane]; y < height - 12; y += 2)
            memcpy(dstp + y * dst_stride,
                   srcp + 32 + (6 + y) * src_stride,
                   (width - 64) * sizeof(uint8_t));

        const int ystart = 6 + frameData->field[plane];
        const int ystop = height - 6;
        srcp += ystart * src_stride;
        dstp += (ystart - 6) * dst_stride - 32;
        const uint8_t *src3p = srcp - src_stride * 3;
        int32_t *lcount = frameData->lcount[plane] - 6;
        if (d->pscrn == 1) {// original
            for (y = ystart; y < ystop; y += 2) {
                for (x = 32; x < width - 32; ++x) {
                    d->readPixels((const uint8_t *)(src3p + x - 5), src_stride, input);
                    d->computeNetwork0(d, input, weights0, tempu+x);
                }
                lcount[y] += d->processLine0(tempu + 32, width - 64, (uint8_t *)(dstp + 32), (const uint8_t *)(src3p + 32), src_stride, d->max_value, plane);
                src3p += src_stride * 2;
                dstp += dst_stride * 2;
            }
        } else if (sizeof(uint8_t) == 1 && d->pscrn >= 2) {// new
            for (y = ystart; y < ystop; y += 2) {
                for (x = 32; x < width - 32; x += 4) {
                    d->readPixels((const uint8_t *)(src3p + x - 6), src_stride, input);
                    d->computeNetwork0(d, input, weights0, tempu + x);
                }
                lcount[y] += d->processLine0(tempu + 32, width - 64, (uint8_t *)(dstp + 32), (const uint8_t *)(src3p + 32), src_stride, d->max_value, plane);
                src3p += src_stride * 2;
                dstp += dst_stride * 2;
            }
        } else {// no prescreening
            for (y = ystart; y < ystop; y += 2) {
                memset(dstp + 32, 255, (width - 64) * sizeof(uint8_t));
                lcount[y] += width - 64;
                dstp += dst_stride * 2;
            }
        }
    }
}

static void extract_m8(const uint8_t *srcp8, const int stride, const int xdia, const int ydia, float *mstd, float *input)
{
    // uint8_t or uint16_t or float
    const uint8_t *srcp = (const uint8_t *)srcp8;

    // int32_t or int64_t or double
    int64_t sum = 0, sumsq = 0;
    int y, x;

    for (y = 0; y < ydia; ++y) {
        const uint8_t *srcpT = srcp + y * stride * 2;

        for (x = 0; x < xdia; ++x) {
            sum += srcpT[x];
            sumsq += (uint32_t)srcpT[x] * (uint32_t)srcpT[x];
            input[x] = srcpT[x];
        }
        input += xdia;
    }
    const float scale = 1.0f / (xdia * ydia);
    mstd[0] = sum * scale;
    const double tmp = (double)sumsq * scale - (double)mstd[0] * mstd[0];
    mstd[3] = 0.0f;
    if (tmp <= FLT_EPSILON)
        mstd[1] = mstd[2] = 0.0f;
    else {
        mstd[1] = sqrt(tmp);
        mstd[2] = 1.0f / mstd[1];
    }
}

static void extract_m8_i16(const uint8_t *srcp, const int stride, const int xdia, const int ydia, float *mstd, float *inputf)
{
    int16_t *input = (int16_t *)inputf;
    int sum = 0, sumsq = 0;
    int y, x;

    for (y = 0; y < ydia; ++y) {
        const uint8_t *srcpT = srcp + y * stride * 2;
        for (x = 0; x < xdia; ++x) {
            sum += srcpT[x];
            sumsq += srcpT[x] * srcpT[x];
            input[x] = srcpT[x];
        }
        input += xdia;
    }
    const float scale = 1.0f / (float)(xdia * ydia);
    mstd[0] = sum * scale;
    mstd[1] = sumsq * scale - mstd[0] * mstd[0];
    mstd[3] = 0.0f;
    if (mstd[1] <= FLT_EPSILON)
        mstd[1] = mstd[2] = 0.0f;
    else {
        mstd[1] = sqrt(mstd[1]);
        mstd[2] = 1.0f / mstd[1];
    }
}


static const float exp_lo = -80.0f;
static const float exp_hi = +80.0f;

static void e2_m16(float *s, const int n)
{
    int i;

    for (i = 0; i < n; ++i)
        s[i] = exp(av_clipf(s[i], exp_lo, exp_hi));
}

const float min_weight_sum = 1e-10f;

static void weightedAvgElliottMul5_m16(const float *w, const int n, float *mstd)
{
    float vsum = 0.0f, wsum = 0.0f;
    int i;

    for (i = 0; i < n; ++i) {
        vsum += w[i] * (w[n + i] / (1.0f + FFABS(w[n + i])));
        wsum += w[i];
    }
    if (wsum > min_weight_sum)
        mstd[3] += ((5.0f * vsum) / wsum) * mstd[1] + mstd[0];
    else
        mstd[3] += mstd[0];
}


static void evalFunc_1(NNEDIContext *d, FrameData *frameData)
{
    float *input = frameData->input;
    float *temp = frameData->temp;
    float **weights1 = d->weights1;
    const int qual = d->qual;
    const int asize = d->asize;
    const int nns = d->nns;
    const int xdia = d->xdia;
    const int xdiad2m1 = (xdia / 2) - 1;
    const int ydia = d->ydia;
    const float scale = 1.0f / (float)qual;
    int plane, y, x, i;

    for (plane = 0; plane < d->nb_planes; ++plane) {
        if (!(d->process_plane & (1 << plane)))
            continue;

        const uint8_t *srcp = (const uint8_t *)frameData->paddedp[plane];
        const int src_stride = frameData->padded_stride[plane] / sizeof(uint8_t);

        const int width = frameData->padded_width[plane];
        const int height = frameData->padded_height[plane];

        uint8_t *dstp = (uint8_t *)frameData->dstp[plane];
        const int dst_stride = frameData->dst_stride[plane] / sizeof(uint8_t);

        const int ystart = frameData->field[plane];
        const int ystop = height - 12;

        srcp += (ystart + 6) * src_stride;
        dstp += ystart * dst_stride - 32;
        const uint8_t *srcpp = srcp - (ydia - 1) * src_stride - xdiad2m1;

        for (y = ystart; y < ystop; y += 2) {
            for (x = 32; x < width - 32; ++x) {
                uint32_t pixel = 0;
                memcpy(&pixel, dstp + x, sizeof(uint8_t));

                uint32_t all_ones = 0;
                memset(&all_ones, 255, sizeof(uint8_t));

                if (pixel != all_ones)
                    continue;

                float mstd[4];
                d->extract((const uint8_t *)(srcpp + x), src_stride, xdia, ydia, mstd, input);
                for (i = 0; i < qual; ++i) {
                    d->dotProd(d, input, weights1[i], temp, nns * 2, asize, mstd + 2);
                    d->expfunc(temp, nns);
                    d->wae5(temp, nns, mstd);
                }

                dstp[x] = FFMIN(FFMAX((int)(mstd[3] * scale + 0.5f), 0), d->max_value);
            }
            srcpp += src_stride * 2;
            dstp += dst_stride * 2;
        }
    }
}

#define NUM_NSIZE 7
#define NUM_NNS 5

static int roundds(const double f)
{
    if (f - floor(f) >= 0.5)
        return FFMIN((int)ceil(f), 32767);
    return FFMAX((int)floor(f), -32768);
}

static void selectFunctions(NNEDIContext *d)
{
    d->copyPad = copyPad;
    d->evalFunc_0 = evalFunc_0;
    d->evalFunc_1 = evalFunc_1;

    // evalFunc_0
    d->processLine0 = processLine0;

    if (d->pscrn < 2) { // original prescreener
        if (d->fapprox & 1) { // int16 dot products
            d->readPixels = byte2word48;
            d->computeNetwork0 = computeNetwork0_i16;
        } else {
            d->readPixels = pixel2float48;
            d->computeNetwork0 = computeNetwork0;
        }
    } else { // new prescreener
        // only int16 dot products
        d->readPixels = byte2word64;
        d->computeNetwork0 = computeNetwork0new;
    }

    // evalFunc_1
    d->wae5 = weightedAvgElliottMul5_m16;

    if (d->fapprox & 2) { // use int16 dot products
        d->extract = extract_m8_i16;
        d->dotProd = dotProdS;
    } else { // use float dot products
        d->extract = extract_m8;
        d->dotProd = dotProd;
    }

    d->expfunc = e2_m16;
}

static int modnpf(const int m, const int n)
{
    if ((m % n) == 0)
        return m;
    return m + n - (m % n);
}

static int filter_frame(AVFilterLink *inlink, AVFrame *src)
{
    AVFilterContext *ctx = inlink->dst;
    NNEDIContext *d = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    int effective_field = d->field;
    int field_n;
    int plane;

    if (effective_field > 1)
        effective_field -= 2;

    if (d->fieldbased == 1)
        effective_field = 0;
    else if (d->fieldbased == 2)
        effective_field = 1;

    if (d->field > 1) {
        if (outlink->frame_count & 1) {
            field_n = (effective_field == 0);
        } else {
            field_n = (effective_field == 1);
        }
    } else {
        field_n = effective_field;
    }

    AVFrame *dst = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    if (!dst) {
        av_frame_free(&src);
        return AVERROR(ENOMEM);
    }

    dst->interlaced_frame = 0;

    FrameData *frameData = av_calloc(1, sizeof(FrameData));

    for (plane = 0; plane < d->nb_planes; plane++) {
        if (!(d->process_plane & (1 << plane)))
            continue;

        const int min_pad = 10;
        const int min_alignment = 16;

        int dst_width = d->linesize[plane];
        int dst_height = d->planeheight[plane];

        frameData->padded_width[plane]  = dst_width + 64;
        frameData->padded_height[plane] = dst_height + 12;
        frameData->padded_stride[plane] = modnpf(frameData->padded_width[plane] + min_pad, min_alignment); // TODO: maybe min_pad is in pixels too?
        frameData->paddedp[plane] = av_malloc((size_t)frameData->padded_stride[plane] * (size_t)frameData->padded_height[plane]);

        frameData->dstp[plane] = dst->data[plane];
        frameData->dst_stride[plane] = dst->linesize[plane];

        frameData->lcount[plane] = av_calloc(dst_height, sizeof(int32_t) * 16);

        frameData->field[plane] = field_n;
    }

    frameData->input = av_malloc(512 * sizeof(float));
    // evalFunc_0 requires at least padded_width[0] bytes.
    // evalFunc_1 requires at least 512 floats.
    size_t temp_size = FFMAX((size_t)frameData->padded_width[0], 512 * sizeof(float));
    frameData->temp = av_malloc(temp_size);

    // Copy src to a padded "frame" in frameData and mirror the edges.
    d->copyPad(src, frameData, d, field_n);

    // Handles prescreening and the cubic interpolation.
    d->evalFunc_0(d, frameData);

    // The rest.
    d->evalFunc_1(d, frameData);

    // Clean up.
    for (plane = 0; plane < d->nb_planes; plane++) {
        if (!(d->process_plane & (1 << plane)))
            continue;

        av_freep(&frameData->paddedp[plane]);
        av_freep(&frameData->lcount[plane]);
    }
    av_freep(&frameData->input);
    av_freep(&frameData->temp);
    av_free(frameData);

    dst->pts = src->pts;
    av_frame_free(&src);

    return ff_filter_frame(outlink, dst);
}

static av_cold void uninit(AVFilterContext *ctx)
{
    NNEDIContext *s = ctx->priv;
    int i;

    av_freep(&s->weights0);

    for (i = 0; i < 2; i++)
        av_freep(&s->weights1[i]);
}

static av_cold int init(AVFilterContext *ctx)
{
    NNEDIContext *s = ctx->priv;
    FILE *weights_file = NULL;
    int64_t expected_size = 13574928;
    int64_t weights_size;
    float *bdata;
    size_t bytes_read;
    const int xdiaTable[NUM_NSIZE] = { 8, 16, 32, 48, 8, 16, 32 };
    const int ydiaTable[NUM_NSIZE] = { 6, 6, 6, 6, 4, 4, 4 };
    const int nnsTable[NUM_NNS] = { 16, 32, 64, 128, 256 };
    const int dims0 = 49 * 4 + 5 * 4 + 9 * 4;
    const int dims0new = 4 * 65 + 4 * 5;
    const int dims1 = nnsTable[s->nnsparam] * 2 * (xdiaTable[s->nsize] * ydiaTable[s->nsize] + 1);
    int dims1tsize = 0;
    int dims1offset = 0;

    int i, j, k;

    weights_file = fopen(s->weights_file, "rb");

    if (!weights_file) {
        return AVERROR(EINVAL);
    }

    if (fseek(weights_file, 0, SEEK_END)) {
        fclose(weights_file);
        return AVERROR(EINVAL);
    }

    weights_size = ftell(weights_file);

    if (weights_size == -1) {
        fclose(weights_file);
        return AVERROR(EINVAL);
    } else if (weights_size != expected_size) {
        fclose(weights_file);
        return AVERROR(EINVAL);
    }

    if (fseek(weights_file, 0, SEEK_SET)) {
        fclose(weights_file);
        return AVERROR(EINVAL);
    }

    bdata = (float *)av_malloc(expected_size);
    if (!bdata)
        return AVERROR(ENOMEM);

    bytes_read = fread(bdata, 1, expected_size, weights_file);

    if (bytes_read != (size_t)expected_size) {
        fclose(weights_file);
        av_free(bdata);
        return AVERROR(EINVAL);
    }

    fclose(weights_file);

    for (j = 0; j < NUM_NNS; ++j) {
        for (i = 0; i < NUM_NSIZE; ++i) {
            if (i == s->nsize && j == s->nnsparam)
                dims1offset = dims1tsize;
            dims1tsize += nnsTable[j] * 2 * (xdiaTable[i] * ydiaTable[i] + 1) * 2;
        }
    }

    s->weights0 = av_malloc(FFMAX(dims0, dims0new) * sizeof(float));

    for (i = 0; i < 2; ++i)
        s->weights1[i] = av_malloc(dims1 * sizeof(float));


    // Adjust prescreener weights
    if (s->pscrn >= 2) {// using new prescreener
        int *offt = (int *)av_calloc(4 * 64, sizeof(int));
        for (j = 0; j < 4; ++j)
            for (k = 0; k < 64; ++k)
                offt[j * 64 + k] = ((k >> 3) << 5) + ((j & 3) << 3) + (k & 7);
        const float *bdw = bdata + dims0 + dims0new * (s->pscrn - 2);
        int16_t *ws = (int16_t *)s->weights0;
        float *wf = (float *)&ws[4 * 64];
        double mean[4] = { 0.0, 0.0, 0.0, 0.0 };
        // Calculate mean weight of each first layer neuron
        for (j = 0; j < 4; ++j) {
            double cmean = 0.0;
            for (k = 0; k < 64; ++k)
                cmean += bdw[offt[j * 64 + k]];
            mean[j] = cmean / 64.0;
        }
        // Factor mean removal and 1.0/127.5 scaling
        // into first layer weights. scale to int16 range
        for (j = 0; j < 4; ++j) {
            double mval = 0.0;
            for (k = 0; k < 64; ++k)
                mval = FFMAX(mval, FFABS((bdw[offt[j * 64 + k]] - mean[j]) / 127.5));
            const double scale = 32767.0 / mval;
            for (k = 0; k < 64; ++k)
                ws[offt[j * 64 + k]] = roundds(((bdw[offt[j * 64 + k]] - mean[j]) / 127.5) * scale);
            wf[j] = (float)(mval / 32767.0);
        }
        memcpy(wf + 4, bdw + 4 * 64, (dims0new - 4 * 64) * sizeof(float));
        av_free(offt);
    } else { // using old prescreener
        double mean[4] = { 0.0, 0.0, 0.0, 0.0 };
        // Calculate mean weight of each first layer neuron
        for (j = 0; j < 4; ++j) {
            double cmean = 0.0;
            for (k = 0; k < 48; ++k)
                cmean += bdata[j * 48 + k];
            mean[j] = cmean / 48.0;
        }
        if (s->fapprox & 1) {// use int16 dot products in first layer
            int16_t *ws = (int16_t *)s->weights0;
            float *wf = (float *)&ws[4 * 48];
            // Factor mean removal and 1.0/127.5 scaling
            // into first layer weights. scale to int16 range
            for (j = 0; j < 4; ++j) {
                double mval = 0.0;
                for (k = 0; k < 48; ++k)
                    mval = FFMAX(mval, FFABS((bdata[j * 48 + k] - mean[j]) / 127.5));
                const double scale = 32767.0 / mval;
                for (k = 0; k < 48; ++k)
                    ws[j * 48 + k] = roundds(((bdata[j * 48 + k] - mean[j]) / 127.5) * scale);
                wf[j] = (float)(mval / 32767.0);
            }
            memcpy(wf + 4, bdata + 4 * 48, (dims0 - 4 * 48) * sizeof(float));
        } else {// use float dot products in first layer
            double half = (1 << 8) - 1;

            half /= 2;

            // Factor mean removal and 1.0/half scaling
            // into first layer weights.
            for (j = 0; j < 4; ++j)
                for (k = 0; k < 48; ++k)
                    s->weights0[j * 48 + k] = (float)((bdata[j * 48 + k] - mean[j]) / half);
            memcpy(s->weights0 + 4 * 48, bdata + 4 * 48, (dims0 - 4 * 48) * sizeof(float));
        }
    }

    // Adjust prediction weights
    for (i = 0; i < 2; ++i) {
        const float *bdataT = bdata + dims0 + dims0new * 3 + dims1tsize * s->etype + dims1offset + i * dims1;
        const int nnst = nnsTable[s->nnsparam];
        const int asize = xdiaTable[s->nsize] * ydiaTable[s->nsize];
        const int boff = nnst * 2 * asize;
        double *mean = (double *)av_calloc(asize + 1 + nnst * 2, sizeof(double));
        // Calculate mean weight of each neuron (ignore bias)
        for (j = 0; j < nnst * 2; ++j) {
            double cmean = 0.0;
            for (k = 0; k < asize; ++k)
                cmean += bdataT[j * asize + k];
            mean[asize + 1 + j] = cmean / (double)asize;
        }
        // Calculate mean softmax neuron
        for (j = 0; j < nnst; ++j) {
            for (k = 0; k < asize; ++k)
                mean[k] += bdataT[j * asize + k] - mean[asize + 1 + j];
            mean[asize] += bdataT[boff + j];
        }
        for (j = 0; j < asize + 1; ++j)
            mean[j] /= (double)(nnst);

        if (s->fapprox & 2) { // use int16 dot products
            int16_t *ws = (int16_t *)s->weights1[i];
            float *wf = (float *)&ws[nnst * 2 * asize];
            // Factor mean removal into weights, remove global offset from
            // softmax neurons, and scale weights to int16 range.
            for (j = 0; j < nnst; ++j) { // softmax neurons
                double mval = 0.0;
                for (k = 0; k < asize; ++k)
                    mval = FFMAX(mval, FFABS(bdataT[j * asize + k] - mean[asize + 1 + j] - mean[k]));
                const double scale = 32767.0 / mval;
                for (k = 0; k < asize; ++k)
                    ws[j * asize + k] = roundds((bdataT[j * asize + k] - mean[asize + 1 + j] - mean[k]) * scale);
                wf[(j >> 2) * 8 + (j & 3)] = (float)(mval / 32767.0);
                wf[(j >> 2) * 8 + (j & 3) + 4] = (float)(bdataT[boff + j] - mean[asize]);
            }
            for (j = nnst; j < nnst * 2; ++j) { // elliott neurons
                double mval = 0.0;
                for (k = 0; k < asize; ++k)
                    mval = FFMAX(mval, FFABS(bdataT[j * asize + k] - mean[asize + 1 + j]));
                const double scale = 32767.0 / mval;
                for (k = 0; k < asize; ++k)
                    ws[j * asize + k] = roundds((bdataT[j * asize + k] - mean[asize + 1 + j]) * scale);
                wf[(j >> 2) * 8 + (j & 3)] = (float)(mval / 32767.0);
                wf[(j >> 2) * 8 + (j & 3) + 4] = bdataT[boff + j];
            }
        } else { // use float dot products
            // Factor mean removal into weights, and remove global
            // offset from softmax neurons.
            for (j = 0; j < nnst * 2; ++j) {
                for (k = 0; k < asize; ++k) {
                    const double q = j < nnst ? mean[k] : 0.0;
                    s->weights1[i][j * asize + k] = (float)(bdataT[j * asize + k] - mean[asize + 1 + j] - q);
                }
                s->weights1[i][boff + j] = (float)(bdataT[boff + j] - (j < nnst ? mean[asize] : 0.0));
            }
        }
        av_free(mean);
    }

    s->nns = nnsTable[s->nnsparam];
    s->xdia = xdiaTable[s->nsize];
    s->ydia = ydiaTable[s->nsize];
    s->asize = xdiaTable[s->nsize] * ydiaTable[s->nsize];

    av_free(bdata);
    s->max_value = 65535 >> 8;

    selectFunctions(s);

    s->fdsp = avpriv_float_dsp_alloc(0);
    if (!s->fdsp)
        return AVERROR(ENOMEM);

    return 0;
}

static const AVFilterPad inputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .filter_frame  = filter_frame,
        .config_props  = config_input,
    },
    { NULL }
};

static const AVFilterPad outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
    },
    { NULL }
};

AVFilter ff_vf_nnedi = {
    .name          = "nnedi",
    .description   = NULL_IF_CONFIG_SMALL("Apply neural network edge directed interpolation."),
    .priv_size     = sizeof(NNEDIContext),
    .priv_class    = &nnedi_class,
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = inputs,
    .outputs       = outputs,
};
