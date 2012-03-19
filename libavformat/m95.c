/*
 * Interplay M95 demuxer
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
    uint16_t index;
    uint8_t  length;
    uint8_t  frames;
} M95BlockRecord;

typedef struct {
    M95BlockRecord block_records[1024];
    int current_block;

    uint32_t frame_offsets[14];
    int current_frame;
} M95DemuxContext;

static int read_probe(AVProbeData *p)
{
    int i, index = 2;
    if (p->buf_size < 16)
        return 0;
    for (i = 0; i < 16; i+= 4) {
        if (AV_RL16(p->buf + i) != index || !p->buf[i + 2] || !p->buf[i + 3])
            return 0;
        index += p->buf[i + 2];
    }
    return AVPROBE_SCORE_MAX;
}

static int read_header(AVFormatContext *s)
{
    M95DemuxContext *m95 = s->priv_data;
    AVIOContext *pb      = s->pb;
    AVStream *video;
    int i, framecount = 0;

    for (i = 0; i < 1024; i++) {
        m95->block_records[i].index  = avio_rl16(pb);
        m95->block_records[i].length = avio_r8(pb);
        m95->block_records[i].frames = avio_r8(pb);
        framecount += m95->block_records[i].frames;
    }

    video = avformat_new_stream(s, NULL);
    if (!video)
        return AVERROR(ENOMEM);

    video->codec->codec_type   = AVMEDIA_TYPE_VIDEO;
    video->codec->codec_id     = CODEC_ID_C93;
    video->codec->width        = 320;
    video->codec->height       = 192;
    video->start_time          = 0;
    video->duration            =
    video->nb_frames           = framecount;
    video->sample_aspect_ratio = (AVRational) { 5, 6 };
    avpriv_set_pts_info(video, 64, 2, 25);

    m95->current_block = 0;
    m95->current_frame = 0;

    return 0;
}

static int read_packet(AVFormatContext *s, AVPacket *pkt)
{
    M95DemuxContext *m95 = s->priv_data;
    AVIOContext *pb      = s->pb;
    M95BlockRecord *br   = &m95->block_records[m95->current_block];
    int i;

    if (m95->current_frame >= br->frames) {
        if (m95->current_block >= 1023 || !br[1].length)
            return AVERROR(EIO);
        br++;
        m95->current_block++;
        m95->current_frame = 0;
    }

    if (m95->current_frame == 0) {
        avio_seek(pb, br->index * 2048, SEEK_SET);
        for (i = 0; i < 14; i++)
            m95->frame_offsets[i] = avio_rl32(pb);
    }

    avio_seek(pb, br->index * 2048 + m95->frame_offsets[m95->current_frame], SEEK_SET);
    return 0;
}

AVInputFormat ff_m95_demuxer = {
    .name           = "m95",
    .long_name      = NULL_IF_CONFIG_SMALL("Interplay M95"),
    .priv_data_size = sizeof(M95DemuxContext),
    .read_probe     = read_probe,
    .read_header    = read_header,
    .read_packet    = read_packet,
};
