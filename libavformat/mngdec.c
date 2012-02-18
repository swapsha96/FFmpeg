/*
 * MNG demuxer
 * Copyright (c) 2012 Paul B Mahol
 *
 * This file is part of Libav.
 *
 * Libav is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Libav is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Libav; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "libavutil/intreadwrite.h"
#include "libavutil/parseutils.h"
#include "libavutil/opt.h"
#include "avformat.h"
#include "internal.h"

#define MNG_SIGNATURE "\x8aMNG\r\n\x1a\n"
#define PNG_SIGNATURE "\x89PNG\r\n\x1a\n"

#define MNG_IHDR     0x0001
#define MNG_IDAT     0x0002

typedef struct MNGDemuxContext {
    AVClass     *class;
    char        *framerate;
    uint32_t    state;
    uint8_t     *plte;
    int         plte_size;
} MNGDemuxContext;

static int mng_probe(AVProbeData *p)
{
    if (!memcmp(p->buf, MNG_SIGNATURE, 8))
        return AVPROBE_SCORE_MAX;
    return 0;
}

static int mng_copy_chunk(AVFormatContext *s, AVPacket *pkt,
                          uint32_t type, uint32_t length)
{
    AVIOContext *pb = s->pb;
    int temp, ret;

    temp = pkt->size;
    ret = av_grow_packet(pkt, 8 + length + 4);
    if (ret < 0) {
        av_free_packet(pkt);
        return ret;
    }
    AV_WB32(pkt->data + temp, length);
    AV_WL32(pkt->data + temp + 4, type);
    ret = avio_read(pb, pkt->data + temp + 8, length + 4);
    if (ret < 0) {
        av_free_packet(pkt);
        return ret;
    }
    return 0;
}

static int mng_read_header(AVFormatContext *s)
{
    MNGDemuxContext  *m  = s->priv_data;
    AVIOContext      *pb = s->pb;
    AVRational       fps;
    AVStream         *st;
    uint32_t         length, type, ticks;
    int              ret;

    if (m->framerate && (ret = av_parse_video_rate(&fps, m->framerate)) < 0) {
        av_log(s, AV_LOG_ERROR,
               "Could not parse framerate: %s.\n", m->framerate);
        return ret;
    }

    avio_skip(s->pb, 8);   // signature

    length = avio_rb32(pb);
    type   = avio_rl32(pb);

    if ((type != MKTAG('M', 'H', 'D', 'R')) || (length != 28))
        return AVERROR_INVALIDDATA;

    st = avformat_new_stream(s, NULL);
    if (!st)
        return AVERROR(ENOMEM);
    st->codec->codec_type    = AVMEDIA_TYPE_VIDEO;
    st->codec->codec_tag     = 0;
    st->codec->codec_id      = CODEC_ID_PNG;
    st->codec->width         = avio_rb32(pb);
    st->codec->height        = avio_rb32(pb);
    ticks                    = avio_rb32(pb);
    avio_skip(pb, 8);
    st->duration             = avio_rb32(pb);
    st->start_time           = 0;
    avio_skip(pb, 4);
    avio_skip(pb, 4);      // crc
    if (m->framerate)
        avpriv_set_pts_info(st, 64, fps.den, fps.num);
    else
        avpriv_set_pts_info(st, 32, 1, FFMAX(ticks, 1));

    return 0;
}

static int mng_read_packet(AVFormatContext *s, AVPacket *pkt)
{
    MNGDemuxContext  *m  = s->priv_data;
    AVIOContext      *pb = s->pb;
    uint32_t         length, type, olength, delay = 0;
    int              ret;
    int64_t          pos;

    pos = avio_tell(pb);
    while (!pb->eof_reached) {
        length = avio_rb32(pb);
        type   = avio_rl32(pb);
        av_log(s, AV_LOG_INFO, "chunk 0x%0x size %d\n", type, length);
        switch (type) {
        case MKTAG('F', 'R', 'A', 'M'):
            if (length) {
                uint8_t name[1];

                avio_skip(pb, 1); // framing mode
                length -= avio_get_str(pb, length--, name, sizeof(name));
                if (length >= 8) {
                    avio_skip(pb, 4);
                    delay = avio_rb32(pb);
                    avio_skip(pb, length - 8);
                } else
                    avio_skip(pb, length);
            }
            avio_skip(pb, 4); // CRC
            break;
        case MKTAG('p', 'H', 'Y', 's'):
            if ((m->state & MNG_IHDR)) {
                if (m->state & MNG_IDAT) {
                    av_free_packet(pkt);
                    return AVERROR_INVALIDDATA;
                }
                ret = mng_copy_chunk(s, pkt, type, length);
                if (ret)
                    return ret;
                break;
            }
            /* FALLTHROUGH */
        case MKTAG('p', 'H', 'Y', 'g'):
            if (length == 9) {
                uint32_t x, y;

                x = avio_rb32(pb);
                y = avio_rb32(pb);
                avio_skip(pb, 1);
                s->streams[0]->sample_aspect_ratio = (AVRational){x, y};
            } else if (length) {
                return AVERROR_INVALIDDATA;
            }
            avio_skip(pb, 4); // CRC
            break;
        case MKTAG('I', 'H', 'D', 'R'):
            if (length != 13)
                return AVERROR_INVALIDDATA;
            if ((m->state & MNG_IHDR) ||
                (m->state & MNG_IDAT))
                return AVERROR_INVALIDDATA;

            if (av_new_packet(pkt, length + 20) < 0)
                return AVERROR(ENOMEM);
            memcpy(pkt->data, PNG_SIGNATURE, 8);
            AV_WB32(pkt->data + 8, length);
            AV_WL32(pkt->data + 12, type);
            ret = avio_read(pb, pkt->data + 16, length + 4);
            if (ret < 0) {
                av_free_packet(pkt);
                return ret;
            }
            m->state       |= MNG_IHDR;
            break;
        case MKTAG('I', 'D', 'A', 'T'):
            if (!(m->state & MNG_IHDR))
                return AVERROR_INVALIDDATA;
            ret = mng_copy_chunk(s, pkt, type, length);
            if (ret)
                return ret;
            m->state       |= MNG_IDAT;
            break;
        case MKTAG('I', 'E', 'N', 'D'):
            if (!(m->state & MNG_IDAT)) {
                if (m->state & MNG_IHDR)
                    av_free_packet(pkt);
                return AVERROR_INVALIDDATA;
            }
            ret = mng_copy_chunk(s, pkt, type, length);
            if (ret)
                return ret;
            m->state          = 0;
            pkt->flags       |= AV_PKT_FLAG_KEY;
            pkt->pos          = pos;
            pkt->duration     = FFMAX(delay, 1);
            ret = 0;
            goto exit_loop;
        case MKTAG('t', 'E', 'X', 't'):
            if (!(m->state & MNG_IHDR)) {
                uint8_t key[80];
                uint8_t *value;

                value = av_malloc(length + 1);
                if (!value)
                    return AVERROR(ENOMEM);

                olength = avio_get_str(pb, FFMIN(80, length), key, sizeof(key));
                length -= olength;
                olength = avio_get_str(pb, length, value, length + 1);
                av_dict_set(&s->metadata, key, value, AV_DICT_DONT_STRDUP_VAL);
                avio_skip(pb, 4); // CRC
            } else {
                ret = mng_copy_chunk(s, pkt, type, length);
                if (ret)
                    return ret;
            }
            break;
        case MKTAG('t', 'I', 'M', 'E'):
            if (length != 7)
                return AVERROR_INVALIDDATA;
            if (!(m->state & MNG_IHDR)) {
                uint8_t buffer[64], month, day, hour, minute, seconds;
                uint16_t year;

                year    = avio_rb16(pb);
                month   = avio_r8(pb);
                day     = avio_r8(pb);
                hour    = avio_r8(pb);
                minute  = avio_r8(pb);
                seconds = avio_r8(pb);
                snprintf(buffer, sizeof(buffer), "%.4d-%.2d-%.2d %.2d:%2d:%.2d",
                         year, month, day, hour, minute, seconds);
                av_dict_set(&s->metadata, "Last Modification", buffer, 0);
                avio_skip(pb, 4); // CRC
            } else {
                ret = mng_copy_chunk(s, pkt, type, length);
                if (ret)
                    return ret;
            }
            break;
//        case MKTAG('i', 'T', 'X', 't'):
//        case MKTAG('z', 'T', 'X', 't'):
//        case MKTAG('t', 'R', 'N', 'S'):
        case MKTAG('P', 'L', 'T', 'E'):
            if ((length > 256 * 3) || (length & 3)) {
                return AVERROR_INVALIDDATA;
            } else if (!length) {
                if ((m->state & MNG_IHDR) && m->plte) {
                    int temp;

                    temp = pkt->size;
                    ret  = av_grow_packet(pkt, m->plte_size);
                    if (ret < 0) {
                        av_free_packet(pkt);
                        return ret;
                    }
                    memcpy(pkt->data + temp, m->plte, m->plte_size);
                }
                avio_skip(pb, 4); // CRC
            } else if (!(m->state & MNG_IHDR)) {
                if (m->plte)
                    return AVERROR_INVALIDDATA;

                m->plte = av_malloc(8 + length + 4);
                if (!m->plte)
                    return AVERROR(ENOMEM);
                AV_WB32(m->plte, length);
                AV_WL32(m->plte + 4, type);
                ret = avio_read(pb, m->plte + 8, length + 4);
                if (ret < 0)
                    return ret;
                m->plte_size = 8 + length + 4;
            } else {
                ret = mng_copy_chunk(s, pkt, type, length);
                if (ret < 0)
                    return ret;
            }
            break;
        case MKTAG('M', 'E', 'N', 'D'):
            if (length)
                return AVERROR_INVALIDDATA;
            avio_skip(pb, 4); // CRC
            ret = AVERROR_EOF;
            goto exit_loop;
        default:
            av_log(s, AV_LOG_DEBUG, "Unknown or unsupported chunk type 0x%0x\n", type);
            avio_skip(pb, length);
            avio_skip(pb, 4); // CRC
            break;
        }
    }
    return AVERROR_EOF;

exit_loop:

    return ret;
}

#define OFFSET(x) offsetof(MNGDemuxContext, x)
static const AVOption mng_options[] = {
    { "framerate",   "", OFFSET(framerate),   AV_OPT_TYPE_STRING, { .str = NULL },  0, 0,       AV_OPT_FLAG_DECODING_PARAM },
    { NULL },
};

static const AVClass mng_demuxer_class = {
    .class_name = "MNG demuxer",
    .item_name  = av_default_item_name,
    .option     = mng_options,
    .version    = LIBAVUTIL_VERSION_INT,
};

AVInputFormat ff_mng_demuxer = {
    .name           = "mng",
    .long_name      = NULL_IF_CONFIG_SMALL("MNG (Multi-image Network Graphics) format"),
    .priv_data_size = sizeof(MNGDemuxContext),
    .read_probe     = mng_probe,
    .read_header    = mng_read_header,
    .read_packet    = mng_read_packet,
    .extensions     = "mng",
    .flags          = AVFMT_GENERIC_INDEX,
    .priv_class     = &mng_demuxer_class,
};
