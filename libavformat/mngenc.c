/*
 * MNG muxer
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

#include "avformat.h"
#include "internal.h"

#define MNG_SIGNATURE "\x8aMNG\r\n\x1a\n"

static int mng_write_header(AVFormatContext *s)
{
    return 0;
}

static int mng_write_packet(AVFormatContext *s, AVPacket *pkt)
{
    avio_write(s->pb, pkt->data, pkt->size);
    avio_flush(s->pb);
    return 0;
}

static int mng_write_trailer(AVFormatContext *s)
{
    return 0;
}

AVOutputFormat ff_mng_muxer = {
    .name          = "mng",
    .long_name     = NULL_IF_CONFIG_SMALL("MNG (Multi-image Network Graphics) format"),
    .mime_type     = "video/x-mng",
    .extensions    = "mng",
    .audio_codec   = CODEC_ID_NONE,
    .video_codec   = CODEC_IN_PNG,
    .write_header  = mng_write_header,
    .write_packet  = mng_write_packet,
    .write_trailer = mng_write_trailer,
    .flags         = AVFMT_GLOBALHEADER,
};
