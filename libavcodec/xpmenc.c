/*
 * XPM image format
 *
 * Copyright (c) 2012 Paul B Mahol
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
 */

#include "libavutil/intreadwrite.h"
#include "avcodec.h"
#include "internal.h"

typedef struct XPMEncContext {
    AVFrame picture;
    char    *cpixels;
    int     cpixels_size;
} XPMEncContext;

static av_cold int xpm_encode_init(AVCodecContext *avctx)
{
    XPMEncContext *x = avctx->priv_data;

    avcodec_get_frame_defaults(&x->picture);
    avctx->coded_frame = &x->picture;

    return 0;
}

static void index2ascii(int index, int cpp, char *cpixel)
{
    int n = index, charnum, i;
    char *p = cpixel;

    for (i = 0; i < cpp; i++) {
        charnum = n % ('z' - '#');
        n       = n / ('z' - '#');
        *p++    = '#' + charnum;
    }
    *p = '\0';
}

static int xpm_encode_frame(AVCodecContext *avctx, AVPacket *pkt,
                            const AVFrame *p, int *got_packet)
{
    XPMEncContext *x = avctx->priv_data;
    int i, j, ret, ncolors, cpp, size;
    uint8_t *buf, *ptr;

    switch (avctx->pix_fmt) {
    case PIX_FMT_RGB4_BYTE:
    case PIX_FMT_BGR4_BYTE:
        ncolors = 16;
        cpp = 1;
        break;
    case PIX_FMT_PAL8:
    case PIX_FMT_RGB8:
    case PIX_FMT_BGR8:
        ncolors = 256;
        cpp = 2;
        break;
    default:
        av_log(avctx, AV_LOG_INFO, "unsupported pixel format\n");
        return AVERROR(EINVAL);
    }

    av_fast_padded_malloc(&x->cpixels, &x->cpixels_size, ncolors * (cpp + 1));
    if (!x->cpixels)
        return AVERROR(ENOMEM);

    size = 12 + 25 + 50 + ncolors * (cpp + 16) + avctx->height * (avctx->width * cpp + 5);
    if ((ret = ff_alloc_packet2(avctx, pkt, size)) < 0) {
        av_freep(&x->cpixels);
        return ret;
    }

    buf = pkt->data;
    ptr = p->data[0];

    buf += snprintf(buf, 12, "/* XPM */\n");
    buf += snprintf(buf, 25, "static char *out[] = {\n");
    buf += snprintf(buf, 50, "\"%u %u %u %u\",\n", avctx->width, avctx->height, ncolors, cpp);

    for (i = 0; i < ncolors; i++) {
        uint8_t red, green, blue, alpha;
        uint32_t val;

        val   = AV_RN32A(p->data[1] + i * 4);
        alpha = (val >> 24) & 0xFF;
        red   = (val >> 16) & 0xFF;
        green = (val >>  8) & 0xFF;
        blue  =  val        & 0xFF;

        index2ascii(i, cpp, x->cpixels + i * (cpp + 1));
        buf += snprintf(buf, 2 + cpp, "\"%s", x->cpixels + i * (cpp + 1));
        if (alpha)
            buf += snprintf(buf, 16, " c #%02X%02X%02X\",\n", red, green, blue);
        else
            buf += snprintf(buf, 12, " c none\",\n");
    }
    for (i = 0; i < avctx->height; i++) {
        buf += snprintf(buf, 2, "\"");
        for (j = 0; j < avctx->width; j++) {
            buf += snprintf(buf, 1 + cpp, "%s", x->cpixels + *ptr++ * (cpp + 1));
        }
        ptr += p->linesize[0] - avctx->width;
        buf += snprintf(buf, 4, "\",\n");
    }
    buf += snprintf(buf, 4, "};\n");

    pkt->size   = buf - pkt->data;
    pkt->flags |= AV_PKT_FLAG_KEY;
    *got_packet = 1;
    return 0;
}

static av_cold int xpm_encode_close(AVCodecContext *avctx)
{
    XPMEncContext *x = avctx->priv_data;

    av_freep(&x->cpixels);

    return 0;
}

AVCodec ff_xpm_encoder = {
    .name           = "xpm",
    .type           = AVMEDIA_TYPE_VIDEO,
    .id             = CODEC_ID_XPM,
    .priv_data_size = sizeof(XPMEncContext),
    .init           = xpm_encode_init,
    .encode2        = xpm_encode_frame,
    .close          = xpm_encode_close,
    .pix_fmts       = (const enum PixelFormat[]) { PIX_FMT_RGB8,
                                                   PIX_FMT_BGR8,
                                                   PIX_FMT_RGB4_BYTE,
                                                   PIX_FMT_BGR4_BYTE,
                                                   PIX_FMT_PAL8,
                                                   PIX_FMT_NONE },
    .long_name      = NULL_IF_CONFIG_SMALL("XPM (X PixMap) image"),
};
