/*
 * Cryo HNM4 demuxer
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
#include "avformat.h"
#include "internal.h"

typedef struct {
    int superchunk_size;
    int superchunk_pos;

    uint8_t *palette;
    int palette_size;
} HNM4DemuxContext;

static int read_probe(AVProbeData *p)
{
    if (AV_RL32(p->buf) == MKTAG('H', 'N', 'M', '4'))
        return AVPROBE_SCORE_MAX;
    return 0;
}

static int read_header(AVFormatContext *s)
{
    AVIOContext *pb = s->pb;
    AVStream *vst;

    vst = avformat_new_stream(s, 0);
    if (!vst)
        return AVERROR(ENOMEM);

    avio_skip(pb, 8);
    vst->codec->codec_type = AVMEDIA_TYPE_VIDEO;
    vst->codec->codec_id   = CODEC_ID_HNM4;
    vst->codec->width      = avio_rl16(pb);
    vst->codec->height     = avio_rl16(pb);
    vst->codec->codec_tag  = 0;
    avio_skip(pb, 4); // file size
    vst->start_time        = 0;
    vst->duration          =
    vst->nb_frames         = avio_rl32(pb);
    avpriv_set_pts_info(vst, 64, 1, 15);
    avio_skip(pb, 44);

    return 0;
}

static int read_packet(AVFormatContext *s, AVPacket *pkt)
{
    HNM4DemuxContext *h = s->priv_data;
    AVIOContext *pb = s->pb;
    int chunk_size, chunk_id, key_frame = 0;

    if (url_feof(pb))
        return AVERROR_EOF;

    if (h->superchunk_pos == h->superchunk_size) {
        h->superchunk_size = avio_rl24(pb);
        if (h->superchunk_size < 8)
            return AVERROR_INVALIDDATA;
        avio_skip(pb, 1);
        h->superchunk_pos  = 4;
    }
    while (h->superchunk_pos < h->superchunk_size) {
        if (url_feof(pb))
            return AVERROR_EOF;

        chunk_size = avio_rl24(pb);
        if (chunk_size < 8 ||
            chunk_size > h->superchunk_size - h->superchunk_pos)
            return AVERROR_INVALIDDATA;
        avio_skip(pb, 1);
        chunk_id = avio_rl16(pb);

        av_log(s, AV_LOG_DEBUG, "%x c:%d sc:%d p:%d\n", chunk_id, chunk_size, h->superchunk_size, h->superchunk_pos);
        switch (chunk_id) {
        case 0x4c50:
            avio_skip(pb, 2);
            if (h->palette) {
                av_log(s, AV_LOG_WARNING, "discarding unused palette\n");
                av_freep(&h->palette);
            }
            h->palette_size = chunk_size - 8;
            h->palette = av_malloc(h->palette_size);
            if (!h->palette)
                return AVERROR(ENOMEM);
            if (avio_read(pb, h->palette, h->palette_size) != h->palette_size) {
                av_freep(&h->palette);
                return AVERROR(EIO);
            }
            break;
        case 0x5a49:
            key_frame = 1;
        case 0x5549:
            avio_skip(pb, 2);
            if (av_get_packet(pb, pkt, chunk_size - 8) < 0)
                return AVERROR(EIO);
            pkt->stream_index = 0;
            pkt->duration = 1;
            if (key_frame)
                pkt->flags |= AV_PKT_FLAG_KEY;
            h->superchunk_pos += chunk_size;

            if (h->palette) {
                uint8_t *data = av_packet_new_side_data(pkt, AV_PKT_DATA_PALETTE,
                                                        h->palette_size);
                memcpy(data, h->palette, h->palette_size);
                av_freep(&h->palette);
            }
            return pkt->size;
            break;
        default:
            avio_skip(pb, chunk_size - 6);
            av_log(s, AV_LOG_WARNING, "unknown chunk\n");
            break;
        }
        h->superchunk_pos += chunk_size;
    }

    return AVERROR_INVALIDDATA;
}

static int read_close(AVFormatContext *s)
{
    HNM4DemuxContext *h = s->priv_data;

    av_freep(&h->palette);

    return 0;
}

AVInputFormat ff_hnm4_demuxer = {
    .name           = "hnm4",
    .long_name      = NULL_IF_CONFIG_SMALL("Cryo HNM4"),
    .priv_data_size = sizeof(HNM4DemuxContext),
    .read_probe     = read_probe,
    .read_header    = read_header,
    .read_packet    = read_packet,
    .read_close     = read_close,
};
