/*
 * Copyright (c) 2008 Jaikrishnan Menon <realityman@gmx.net>
 * Copyright (c) 2010 Peter Ross <pross@xvid.org>
 * Copyright (c) 2010 Sebastian Vater <cdgs.basty@googlemail.com>
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

/**
 * @file
 * IFF file demuxer
 * by Jaikrishnan Menon
 * for more information on the .iff file format, visit:
 * http://wiki.multimedia.cx/index.php?title=IFF
 */

#include "libavcodec/bytestream.h"
#include "libavutil/intreadwrite.h"
#include "libavutil/dict.h"
#include "avformat.h"
#include "internal.h"

#define ID_8SVX       MKTAG('8','S','V','X')
#define ID_VHDR       MKTAG('V','H','D','R')
#define ID_ATAK       MKTAG('A','T','A','K')
#define ID_RLSE       MKTAG('R','L','S','E')
#define ID_CHAN       MKTAG('C','H','A','N')
#define ID_PBM        MKTAG('P','B','M',' ')
#define ID_ILBM       MKTAG('I','L','B','M')
#define ID_BMHD       MKTAG('B','M','H','D')
#define ID_DGBL       MKTAG('D','G','B','L')
#define ID_CAMG       MKTAG('C','A','M','G')
#define ID_CMAP       MKTAG('C','M','A','P')
#define ID_ACBM       MKTAG('A','C','B','M')
#define ID_DEEP       MKTAG('D','E','E','P')

#define ID_FORM       MKTAG('F','O','R','M')
#define ID_ANNO       MKTAG('A','N','N','O')
#define ID_AUTH       MKTAG('A','U','T','H')
#define ID_CHRS       MKTAG('C','H','R','S')
#define ID_COPYRIGHT  MKTAG('(','c',')',' ')
#define ID_CSET       MKTAG('C','S','E','T')
#define ID_FVER       MKTAG('F','V','E','R')
#define ID_NAME       MKTAG('N','A','M','E')
#define ID_TEXT       MKTAG('T','E','X','T')
#define ID_ABIT       MKTAG('A','B','I','T')
#define ID_BODY       MKTAG('B','O','D','Y')
#define ID_DBOD       MKTAG('D','B','O','D')
#define ID_DPEL       MKTAG('D','P','E','L')
#define ID_ANIM       MKTAG('A','N','I','M')
#define ID_ANHD       MKTAG('A','N','H','D')
#define ID_DLTA       MKTAG('D','L','T','A')

#define LEFT    2
#define RIGHT   4
#define STEREO  6

/**
 * This number of bytes if added at the beginning of each AVPacket
 * which contain additional information about video properties
 * which has to be shared between demuxer and decoder.
 * This number may change between frames.
 */
#define IFF_EXTRA_VIDEO_SIZE 9

typedef enum {
    COMP_NONE,
    COMP_FIB,
    COMP_EXP
} svx8_compression_type;

typedef enum {
    BITMAP_RAW,
    BITMAP_BYTERUN1
} bitmap_compression_type;

typedef struct {
    int       tag;
    uint32_t  form_size;
    uint8_t   *palette;
    int       palette_size;
} IffDemuxContext;

/* Metadata string read */
static int get_metadata(AVFormatContext *s,
                        const char *const tag,
                        const unsigned data_size)
{
    uint8_t *buf = ((data_size + 1) == 0) ? NULL : av_malloc(data_size + 1);

    if (!buf)
        return AVERROR(ENOMEM);

    if (avio_read(s->pb, buf, data_size) < 0) {
        av_free(buf);
        return AVERROR(EIO);
    }
    buf[data_size] = 0;
    av_dict_set(&s->metadata, tag, buf, AV_DICT_DONT_STRDUP_VAL);
    return 0;
}

static int create_stream(AVFormatContext *s, AVStream **st)
{
    if (*st)
        return AVERROR_INVALIDDATA;
    *st = avformat_new_stream(s, NULL);
    if (!*st)
        return AVERROR(ENOMEM);
    return 0;
}

static int iff_probe(AVProbeData *p)
{
    const uint8_t *d = p->buf;

    if (  AV_RL32(d)   == ID_FORM &&
         (AV_RL32(d+8) == ID_8SVX ||
          AV_RL32(d+8) == ID_PBM  ||
          AV_RL32(d+8) == ID_ACBM ||
          AV_RL32(d+8) == ID_ANIM ||
          AV_RL32(d+8) == ID_DEEP ||
          AV_RL32(d+8) == ID_ILBM) )
        return AVPROBE_SCORE_MAX;
    return 0;
}

static const uint8_t deep_rgb24[] = {0, 0, 0, 3, 0, 1, 0, 8, 0, 2, 0, 8, 0, 3, 0, 8};
static const uint8_t deep_rgba[]  = {0, 0, 0, 4, 0, 1, 0, 8, 0, 2, 0, 8, 0, 3, 0, 8};

static int iff_read_header(AVFormatContext *s)
{
    IffDemuxContext *iff = s->priv_data;
    AVIOContext *pb = s->pb;
    s->ctx_flags |= AVFMTCTX_NOHEADER;

    if (avio_rl32(pb) != ID_FORM)
        return AVERROR_INVALIDDATA;
    iff->form_size = avio_rb32(pb);

    iff->tag = avio_rl32(pb);
    if (iff->tag == ID_ANIM) {
        if (avio_rl32(pb) != ID_FORM)
            return AVERROR_INVALIDDATA;
        iff->form_size = avio_rb32(pb);
        iff->tag = avio_rl32(pb);
        if (iff->tag != ID_ILBM)
            return AVERROR_INVALIDDATA;
    }

    return 0;
}

static int iff_read_packet(AVFormatContext *s, AVPacket *pkt)
{
    IffDemuxContext *iff = s->priv_data;
    AVIOContext *pb = s->pb;
    AVStream *st = NULL;
    bitmap_compression_type bitmap_compression;  ///< delta compression method used
    unsigned  bpp;          ///< bits per plane to decode (differs from bits_per_coded_sample if HAM)
    unsigned  ham;          ///< 0 if non-HAM or number of hold bits (6 for bpp > 6, 4 otherwise)
    unsigned  flags;        ///< 1 for EHB, 0 is no extra half darkening
    unsigned  transparency = 0; ///< transparency color index in palette
    uint8_t *buf;
    uint32_t chunk_id, chunk_size, palette_size = 0;
    uint64_t chunk_pos;
    uint32_t screenmode = 0, duration = 1;
    unsigned masking = 0; // no mask
    uint8_t fmt[16];
    int tag, ret, compr = 0, fmt_size, channels = 1;

    tag = iff->tag;
    if (url_feof(pb))
        return AVERROR_EOF;

    while (!url_feof(pb)) {
        const char *metadata_tag = NULL;

        chunk_id   = avio_rl32(pb);
        chunk_size = avio_rb32(pb);
        chunk_pos  = avio_tell(pb);

        switch(chunk_id) {
        case ID_FORM:
            iff->tag = avio_rl32(pb);
            goto done;
        case ID_VHDR:
            if (chunk_size < 14)
                return AVERROR_INVALIDDATA;
            if ((ret = create_stream(s, &st)) < 0)
                return ret;
            st->codec->codec_type = AVMEDIA_TYPE_AUDIO;
            avio_skip(pb, 12);
            st->codec->sample_rate = avio_rb16(pb);
            if (chunk_size >= 16) {
                avio_skip(pb, 1);
                compr = avio_r8(pb);
            }
            switch (compr) {
            case COMP_NONE:
                st->codec->codec_id = CODEC_ID_PCM_S8_PLANAR;
                break;
            case COMP_FIB:
                st->codec->codec_id = CODEC_ID_8SVX_FIB;
                break;
            case COMP_EXP:
                st->codec->codec_id = CODEC_ID_8SVX_EXP;
                break;
            default:
                av_log(s, AV_LOG_ERROR,
                       "Unknown SVX8 compression method '%d'\n", compr);
                return AVERROR_INVALIDDATA;
            }
            st->codec->bits_per_coded_sample = compr == COMP_NONE ? 8 : 4;
            avpriv_set_pts_info(st, 32, 1, st->codec->sample_rate);
            break;

        case ID_ABIT:
        case ID_BODY:
        case ID_DBOD:
            if (!st)
                return AVERROR_INVALIDDATA;
            if ((ret = av_get_packet(pb, pkt, chunk_size)) < 0)
                return ret;
            pkt->flags |= AV_PKT_FLAG_KEY;
            if (st->codec->codec_type == AVMEDIA_TYPE_VIDEO)
                pkt->duration = FFMAX(duration, 1);
            break;

        case ID_ANHD:
            if (chunk_size < 40 || !s->nb_streams)
                return AVERROR_INVALIDDATA;
            if ((ret = av_get_packet(pb, pkt, 40)) < 0)
                return ret;
            duration = AV_RB32(&pkt->data[14]);
            break;

        case ID_DLTA:
            if (!s->nb_streams)
                return AVERROR_INVALIDDATA;
            if ((ret = av_append_packet(pb, pkt, chunk_size)) < 0)
                return ret;
            pkt->duration = FFMAX(duration, 1);
            break;

        case ID_CHAN:
            if (chunk_size < 4)
                return AVERROR_INVALIDDATA;
            channels = (avio_rb32(pb) < 6) ? 1 : 2;
            break;

        case ID_CAMG:
            if (chunk_size < 4)
                return AVERROR_INVALIDDATA;
            screenmode = avio_rb32(pb);
            break;

        case ID_CMAP:
            if (chunk_size > 768)
                return AVERROR_INVALIDDATA;
            palette_size = chunk_size;
            av_fast_padded_malloc(&iff->palette, &iff->palette_size, palette_size);
            if (!iff->palette)
                return AVERROR(ENOMEM);
            if (avio_read(pb, iff->palette, palette_size) != palette_size)
                return AVERROR(EIO);
            break;

        case ID_BMHD:
            if ((ret = create_stream(s, &st)) < 0)
                return ret;
            bitmap_compression = -1;
            st->start_time = 0;
            st->codec->codec_type            = AVMEDIA_TYPE_VIDEO;
            if (chunk_size <= 8)
                return AVERROR_INVALIDDATA;
            st->codec->width                 = avio_rb16(pb);
            st->codec->height                = avio_rb16(pb);
            avio_skip(pb, 4); // x, y offset
            st->codec->bits_per_coded_sample = avio_r8(pb);
            if (chunk_size >= 10)
                masking                      = avio_r8(pb);
            if (chunk_size >= 11)
                bitmap_compression           = avio_r8(pb);
            if (chunk_size >= 14) {
                avio_skip(pb, 1); // padding
                transparency                 = avio_rb16(pb);
            }
            if (chunk_size >= 16) {
                st->sample_aspect_ratio.num  = avio_r8(pb);
                st->sample_aspect_ratio.den  = avio_r8(pb);
            }
            avpriv_set_pts_info(st, 64, 1, 60);
            break;

        case ID_DPEL:
            if (!st)
                return AVERROR_INVALIDDATA;
            if (chunk_size < 16 || (chunk_size & 3))
                return AVERROR_INVALIDDATA;
            if ((fmt_size = avio_read(pb, fmt, sizeof(fmt))) < 0)
                return fmt_size;
            if (fmt_size == sizeof(deep_rgb24) && !memcmp(fmt, deep_rgb24, sizeof(deep_rgb24))) {
                st->codec->pix_fmt = PIX_FMT_RGB24;
                st->codec->bits_per_coded_sample = 24;
            } else if (fmt_size == sizeof(deep_rgba) && !memcmp(fmt, deep_rgba, sizeof(deep_rgba))) {
                st->codec->pix_fmt = PIX_FMT_RGBA;
                st->codec->bits_per_coded_sample = 32;
            } else {
                av_log_ask_for_sample(s, "unsupported color format\n");
                return AVERROR_PATCHWELCOME;
            }
            break;

        case ID_DGBL:
            if ((ret = create_stream(s, &st)) < 0)
                return ret;
            st->codec->codec_type       = AVMEDIA_TYPE_VIDEO;
            if (chunk_size < 8)
                return AVERROR_INVALIDDATA;
            st->codec->width            = avio_rb16(pb);
            st->codec->height           = avio_rb16(pb);
            bitmap_compression          = avio_rb16(pb);
            if (bitmap_compression) {
                av_log(s, AV_LOG_ERROR,
                       "compression %i not supported\n", bitmap_compression);
                return AVERROR_PATCHWELCOME;
            }
            st->sample_aspect_ratio.num = avio_r8(pb);
            st->sample_aspect_ratio.den = avio_r8(pb);
            break;

        case ID_ANNO:
        case ID_TEXT:      metadata_tag = "comment";   break;
        case ID_AUTH:      metadata_tag = "artist";    break;
        case ID_COPYRIGHT: metadata_tag = "copyright"; break;
        case ID_NAME:      metadata_tag = "title";     break;
        default:
            av_log(s, AV_LOG_DEBUG, "unsupported chunk id: %X\n", chunk_id);
        }

        if (metadata_tag) {
            if ((ret = get_metadata(s, metadata_tag, chunk_size)) < 0) {
                av_log(s, AV_LOG_ERROR, "cannot allocate metadata tag %s!\n", metadata_tag);
                return ret;
            }
        }
        avio_skip(pb, chunk_size - (avio_tell(pb) - chunk_pos) + (chunk_size & 1));
    }

done:
    if (!s->nb_streams || pkt->size <= 0)
        return AVERROR_INVALIDDATA;
    if (st) {
        switch(st->codec->codec_type) {
        case AVMEDIA_TYPE_AUDIO:
            st->codec->channels = channels;
            st->codec->bit_rate = st->codec->channels * st->codec->sample_rate * st->codec->bits_per_coded_sample;
            st->codec->block_align = st->codec->channels * st->codec->bits_per_coded_sample;
            break;

        case AVMEDIA_TYPE_VIDEO:
            bpp     = st->codec->bits_per_coded_sample;
            if ((screenmode & 0x800 /* Hold And Modify */) && bpp <= 8) {
                ham = bpp > 6 ? 6 : 4;
                st->codec->bits_per_coded_sample = 24;
            } else {
                ham = 0;
            }
            flags   = (screenmode & 0x80 /* Extra HalfBrite */) && bpp <= 8;

            if (!st->codec->extradata) {
                st->codec->extradata_size = IFF_EXTRA_VIDEO_SIZE;
                st->codec->extradata      = av_malloc(IFF_EXTRA_VIDEO_SIZE + FF_INPUT_BUFFER_PADDING_SIZE);
                if (!st->codec->extradata)
                    return AVERROR(ENOMEM);
            }
            buf = st->codec->extradata;
            bytestream_put_be16(&buf, IFF_EXTRA_VIDEO_SIZE);
            bytestream_put_byte(&buf, bitmap_compression);
            bytestream_put_byte(&buf, bpp);
            bytestream_put_byte(&buf, ham);
            bytestream_put_byte(&buf, flags);
            bytestream_put_be16(&buf, transparency);
            bytestream_put_byte(&buf, masking);
            st->codec->codec_tag = tag;

            if (palette_size) {
                uint8_t *npal = av_packet_new_side_data(pkt, AV_PKT_DATA_PALETTE, palette_size);
                if (!npal) {
                    return AVERROR(ENOMEM);
                }
                memcpy(npal, iff->palette, palette_size);
            }

            switch (bitmap_compression) {
            case BITMAP_RAW:
                st->codec->codec_id = CODEC_ID_IFF_ILBM;
                break;
            case BITMAP_BYTERUN1:
                st->codec->codec_id = CODEC_ID_IFF_BYTERUN1;
                break;
            default:
                av_log(s, AV_LOG_ERROR,
                    "Unknown bitmap compression method '%d'\n", bitmap_compression);
                return AVERROR_INVALIDDATA;
            }
            break;
        }
    }

    pkt->stream_index = 0;
    return pkt->size;
}

static int iff_read_close(AVFormatContext *s)
{
    IffDemuxContext *iff = s->priv_data;
    av_freep(&iff->palette);
    return 0;
}

AVInputFormat ff_iff_demuxer = {
    .name           = "iff",
    .long_name      = NULL_IF_CONFIG_SMALL("Interchange File Format"),
    .priv_data_size = sizeof(IffDemuxContext),
    .read_probe     = iff_probe,
    .read_header    = iff_read_header,
    .read_packet    = iff_read_packet,
    .read_close     = iff_read_close,
};
