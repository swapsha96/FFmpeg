/*
 * DCX demuxer
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
#include "libavutil/parseutils.h"
#include "libavutil/opt.h"
#include "avformat.h"
#include "internal.h"

typedef struct DCXDemuxConxtex {
    AVClass    *class;
    char       *framerate;
    int32_t    frame_offsets[1024];
    int        current_frame;
} DCXDemuxContext;

static int dcx_probe(AVProbeData *p)
{
    const uint8_t *d = p->buf;
    if (AV_RL32(d) == 0x3ade68b1)
        return AVPROBE_SCORE_MAX / 2;
    return 0;
}

static int dcx_read_header(AVFormatContext *s)
{
    DCXDemuxContext *dc = s->priv_data;
    AVRational fps;
    AVStream *st;
    int i, ret;

    if ((ret = av_parse_video_rate(&fps, dc->framerate)) < 0) {
        av_log(s, AV_LOG_ERROR,
               "Could not parse framerate: %s.\n", dc->framerate);
        return ret;
    }

    st = avformat_new_stream(s, 0);
    if (!st)
        return AVERROR(ENOMEM);

    st->codec->codec_type = AVMEDIA_TYPE_VIDEO;
    st->codec->codec_id   = AV_CODEC_ID_PCX;
    avpriv_set_pts_info(st, 60, fps.den, fps.num);

    avio_skip(s->pb, 4);

    for (i = 0; i < 1024; i++) {
        dc->frame_offsets[i] = avio_rl32(s->pb);
        if (dc->frame_offsets[i] == 0)
            break;
    }

    if (dc->frame_offsets[1023] != 0)
        return AVERROR_INVALIDDATA;

    if (dc->frame_offsets[0] == 0x1004)
        avio_skip(s->pb, 4 * (1023 - i));
    st->nb_frames = i + 1;

    return 0;
}

static int dcx_read_packet(AVFormatContext *s, AVPacket *pkt)
{
    DCXDemuxContext *dc = s->priv_data;
    int size, ret;

    if (dc->current_frame >= s->streams[0]->nb_frames)
        return AVERROR_EOF;
    size = dc->frame_offsets[dc->current_frame + 1] -
           dc->frame_offsets[dc->current_frame];
    if (size < 0)
        size = 9000;

    ret = av_get_packet(s->pb, pkt, size);
    if (ret < 0)
        return ret;
    pkt->stream_index = 0;
    dc->current_frame++;

    return 0;
}

#define OFFSET(x) offsetof(DCXDemuxContext, x)
static const AVOption dcx_options[] = {
    { "framerate", "", OFFSET(framerate), AV_OPT_TYPE_STRING, { .str = "25" }, 0, 0, AV_OPT_FLAG_DECODING_PARAM },
    { NULL },
};

static const AVClass dcx_demuxer_class = {
    .class_name = "DCX demuxer",
    .item_name  = av_default_item_name,
    .option     = dcx_options,
    .version    = LIBAVUTIL_VERSION_INT,
};

AVInputFormat ff_dcx_demuxer = {
    .name           = "dcx",
    .long_name      = NULL_IF_CONFIG_SMALL("DCX"),
    .priv_data_size = sizeof(DCXDemuxContext),
    .read_probe     = dcx_probe,
    .read_header    = dcx_read_header,
    .read_packet    = dcx_read_packet,
    .extensions     = "dcx",
    .priv_class     = &dcx_demuxer_class,
};
