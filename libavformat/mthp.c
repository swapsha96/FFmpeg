/*
 * MTHP demuxer
 * Copyright (c) 2009 Michael Montanye
 * based on libavformat/thp.c, Copyright (c) 2007 Marco Gerards
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

static int mthp_probe(AVProbeData *p)
{
    /* check file header */
    if (AV_RL32(p->buf) == MKTAG('M', 'T', 'H', 'P'))
        return AVPROBE_SCORE_MAX;
    else
        return 0;
}

static int mthp_read_header(AVFormatContext *s,
                           AVFormatParameters *ap)
{
    int             *next_framesz = s->priv_data;
    AVStream        *st;
    ByteIOContext   *pb           = s->pb;

    int             width;
    int             height;
    int             fps;
    int             framecount;
    int             next_frame;

    /* Read the file header.  */
                           get_be32(pb); /* Skip Magic.  */
                           get_be32(pb); /* Unknown value */
                           get_be32(pb); /* Unknown value */
                           get_be32(pb); /* Unknown value */

    width                = get_be32(pb);
    height               = get_be32(pb);
    fps                  = get_be32(pb);

    framecount           = get_be32(pb);

    next_frame           = get_be32(pb);
                           get_be32(pb); /* Unknown value */
    *next_framesz        = get_be32(pb);
    s->priv_data         = next_framesz;

    /* Video component.  */
    st = av_new_stream(s, 0);
    if (!st)
        return AVERROR(ENOMEM);

    av_set_pts_info(st, 64, 1, fps);
    st->codec->codec_type   = CODEC_TYPE_VIDEO;
    st->codec->codec_id     = CODEC_ID_THP;
    st->codec->codec_tag    = 0;  /* no fourcc */
    st->codec->width        = width;
    st->codec->height       = height;
    st->codec->sample_rate  = fps;
    st->duration            = framecount;

    url_fseek(pb, next_frame, SEEK_SET);

    return 0;
}

static int mthp_read_packet(AVFormatContext *s, AVPacket *pkt)
{
    int *next_framesz = s->priv_data;
    ByteIOContext *pb = s->pb;
    int size;
    int ret;

    size = *next_framesz-4;

    /* Locate the next frame and read out its size.  */
    av_free(next_framesz);
    *next_framesz  = get_be32(pb);
    s->priv_data = next_framesz;

    ret = av_get_packet(pb, pkt, size);
    if (ret != size) {
        if (ret >= 0) {
            av_free_packet(pkt);
        }
        return AVERROR(EIO);
    }
    return 0;
}

AVInputFormat mthp_demuxer = {
    "mthp",
    NULL_IF_CONFIG_SMALL("MTHP"),
    sizeof(int*),
    mthp_probe,
    mthp_read_header,
    mthp_read_packet
};
