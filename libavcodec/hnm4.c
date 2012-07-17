/*
 * Cryo HNM4 video decoder
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

#define BITSTREAM_READER_LE

#include "avcodec.h"
#include "bytestream.h"
#include "get_bits.h"

typedef struct {
    AVCodecContext  *avctx;
    AVFrame         pic;
    GetBitContext   gb;
    uint32_t        queue;
    int             index;

    uint8_t         *frame[3];
} HNM4VideoContext;

static av_cold int decode_init(AVCodecContext *avctx)
{
    HNM4VideoContext * const c = avctx->priv_data;
    int i;

    avctx->pix_fmt = PIX_FMT_PAL8;
    c->avctx = avctx;

    avcodec_get_frame_defaults(&c->pic);
    for (i = 0; i < 3; i++) {
        c->frame[i] = av_mallocz(avctx->height * avctx->width);
        if (!c->frame[i])
            return AVERROR(ENOMEM);
    }

    return 0;
}

static int get_bit(HNM4VideoContext *c)
{
    if (!c->index) {
        c->queue = get_bits_long(&c->gb, 32);
        c->index = 32;
    }
    c->index--;

    return !!(c->queue & (1LL << c->index));
}

static int decode_intra(HNM4VideoContext *c)
{
    uint8_t *dst = c->frame[0];

    skip_bits_long(&c->gb, 32);

    while (1) {
        if (get_bit(c)) {
            *dst++ = get_bits(&c->gb, 8);
        } else {
            int count, offset;

            if (get_bit(c)) {
                count  = get_bits(&c->gb, 3);
                offset = get_bits(&c->gb, 13) - 8192;

                if (!count) {
                    count = get_bits(&c->gb, 8);
                    if (!count)
                        break;
                }
            } else {
                count  = get_bit(c) * 2 + get_bit(c);
                offset = get_bits(&c->gb, 8) - 256;
            }

            count += 2;
            while (count--)
                *dst++ = dst[offset];
        }
    }

    return 0;
}

static int decode_inter(HNM4VideoContext *c)
{
    uint16_t *dst_start = (uint16_t *)c->frame[0];
    uint16_t *dst = (uint16_t *)c->frame[0];
    int count;

    while (get_bits_left(&c->gb) > 0) {
        count = get_bits(&c->gb, 5);
        if (!count) {
            int tag, length, fill;

            tag = get_bits(&c->gb, 3);
            switch (tag) {
            case 0:
                *dst++ = get_bits(&c->gb, 16);
                break;
            case 1:
                dst += get_bits(&c->gb, 8);
                break;
            case 2:
                dst += get_bits(&c->gb, 16);
                break;
            case 3:
                length = get_bits(&c->gb, 8);
                fill   = get_bits(&c->gb, 8);
                fill   = fill << 8 | fill;
                while (length--)
                    *dst++ = fill;
                break;
            default:
                return 0;
            }
        } else {
            int flags, offset, i;
            uint16_t *src;

            flags  = get_bits(&c->gb, 4);
            offset = get_bits(&c->gb, 15);

            if (flags & 1)
                src = (uint16_t *)c->frame[2];
            else
                src = (uint16_t *)c->frame[1];

            src += (dst - dst_start) + offset - 16384;
            if (flags & 2)
                src -= c->avctx->width;
            for (i = 0; i < count; i++) {
                if (flags & 4)
                    dst[i] = flags & 8 ? av_bswap16(src[i]): src[i];
                else
                    dst[i] = flags & 8 ? av_bswap16(src[count - i - 1]): src[count - i - 1];
            }
            dst += count;
        }
    }

    return 0;
}

static int set_palette(HNM4VideoContext *c, GetByteContext *gb)
{
    uint32_t *palette = (uint32_t *)c->pic.data[1];
    int start, count, i;

    while (bytestream2_get_bytes_left(gb) >= 2) {
        start = bytestream2_get_byte(gb);
        count = bytestream2_get_byte(gb);

        if (start == 0xFF && count == 0xFF)
            return 0;

        if (count == 0)
            count = 256;

        if (start + count > AVPALETTE_COUNT)
            return AVERROR_INVALIDDATA;

        for (i = start; i < count + start; i++) {
            unsigned r, g, b;

            r = bytestream2_get_byte(gb);
            r = r << 2 | r >> 4;
            g = bytestream2_get_byte(gb);
            g = g << 2 | r >> 4;
            b = bytestream2_get_byte(gb);
            b = b << 2 | r >> 4;

            palette[i] = 0xFF << 24 | r << 16 | g << 8 | b;
        }
    }
    c->pic.palette_has_changed = 1;

    return 0;
}

static int decode_frame(AVCodecContext *avctx, void *data, int *data_size, AVPacket *pkt)
{
    HNM4VideoContext * const c = avctx->priv_data;
    uint8_t *src, *dst;
    int ret, i, j;

    if (c->pic.data[0])
        avctx->release_buffer(avctx, &c->pic);

    c->pic.reference = 0;
    if ((ret = avctx->get_buffer(avctx, &c->pic)) < 0) {
        av_log(avctx, AV_LOG_ERROR, "get_buffer() failed\n");
        return ret;
    }

    if (pkt->side_data_elems > 0 && pkt->side_data[0].type == AV_PKT_DATA_PALETTE) {
        GetByteContext g;

        bytestream2_init(&g, pkt->side_data[0].data, pkt->side_data[0].size);
        if ((ret = set_palette(c, &g)) < 0)
            return ret;
    }

    init_get_bits(&c->gb, pkt->data, pkt->size * 8);

    if (pkt->flags & AV_PKT_FLAG_KEY) {
        ret = decode_intra(c);
        c->pic.key_frame = 1;
        c->pic.pict_type = AV_PICTURE_TYPE_I;
        memcpy(c->frame[1], c->frame[0], avctx->width * avctx->height);
    } else {
        memcpy(c->frame[2], c->frame[1], avctx->width * avctx->height);
        memcpy(c->frame[1], c->frame[0], avctx->width * avctx->height);
        ret = decode_inter(c);
        c->pic.key_frame = 0;
        c->pic.pict_type = AV_PICTURE_TYPE_P;
    }
    if (ret)
        return AVERROR_INVALIDDATA;

    dst = c->pic.data[0];
    src = c->frame[0];
    for (i = 0; i < avctx->height; i += 2) {
        for (j = 0; j < avctx->width; j += 2) {
            dst[j                         ] = src[2 * j    ];
            dst[j + c->pic.linesize[0]    ] = src[2 * j + 1];
            dst[j + 1                     ] = src[2 * j + 2];
            dst[j + c->pic.linesize[0] + 1] = src[2 * j + 3];
        }
        dst += 2 * c->pic.linesize[0];
        src += 2 * avctx->width;
    }

    *data_size      = sizeof(AVFrame);
    *(AVFrame*)data = c->pic;

    return pkt->size;
}

static av_cold int decode_end(AVCodecContext *avctx)
{
    HNM4VideoContext * const c = avctx->priv_data;
    int i;

    if (c->pic.data[0])
        avctx->release_buffer(avctx, &c->pic);

    for (i = 0; i < 3; i++)
        av_freep(&c->frame[i]);
    return 0;
}

AVCodec ff_hnm4_decoder = {
    .name           = "hnm4",
    .type           = AVMEDIA_TYPE_VIDEO,
    .id             = CODEC_ID_HNM4,
    .priv_data_size = sizeof(HNM4VideoContext),
    .init           = decode_init,
    .close          = decode_end,
    .decode         = decode_frame,
    .capabilities   = CODEC_CAP_DR1,
    .long_name      = NULL_IF_CONFIG_SMALL("Cryo HNM4 video"),
};
