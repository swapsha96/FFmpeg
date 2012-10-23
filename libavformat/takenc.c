/*
 * raw TAK muxer
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

#include "libavutil/crc.h"
#include "avformat.h"
#include "avio_internal.h"
#include "apetag.h"
#define BITSTREAM_WRITER_LE
#include "libavcodec/put_bits.h"
#include "libavcodec/tak.h"

typedef struct TAXMuxContext {
    TAKStreamInfo metadata;
    TAKStreamInfo hdr;
    int64_t offset;
    int size;
} TAKMuxContext;

static unsigned long tak_check_crc(unsigned long checksum, const uint8_t *buf,
                                   unsigned int len)
{
    return av_crc(av_crc_get_table(AV_CRC_24_IEEE), checksum, buf, len);
}

static int write_streaminfo(TAKStreamInfo *s, uint8_t *buf, int buf_size)
{
    PutBitContext pb;

    init_put_bits(&pb, buf, buf_size);
    put_bits(&pb, TAK_ENCODER_CODEC_BITS, s->codec);
    put_bits(&pb, TAK_ENCODER_PROFILE_BITS, s->profile);
    put_bits(&pb, TAK_SIZE_FRAME_DURATION_BITS, s->frame_type);
    put_bits(&pb, 31, s->samples & 0x7fffffff);
    put_bits(&pb,  4, (s->samples >> 31) & 0xf);
    put_bits(&pb, TAK_FORMAT_DATA_TYPE_BITS, s->data_type);
    put_bits(&pb, TAK_FORMAT_SAMPLE_RATE_BITS, s->sample_rate - TAK_SAMPLE_RATE_MIN);
    put_bits(&pb, TAK_FORMAT_BPS_BITS, s->bps - TAK_BPS_MIN);
    put_bits(&pb, TAK_FORMAT_CHANNEL_BITS, s->channels - TAK_CHANNELS_MIN);
    put_bits(&pb, 1, 0);
    flush_put_bits(&pb);

    return put_bits_count(&pb) >> 3;
}

static int tak_write_header(struct AVFormatContext *ctx)
{
    AVCodecContext *codec = ctx->streams[0]->codec;
    TAKMuxContext *s = ctx->priv_data;
    GetBitContext gb;
    uint8_t buffer[TAK_MAX_STREAMINFO_BYTES];
    int size;

    if (ctx->nb_streams != 1 ||
        ctx->streams[0]->codec->codec_id != AV_CODEC_ID_TAK) {
        av_log(ctx, AV_LOG_ERROR, "This muxer only supports a single TAK stream.\n");
        return AVERROR(EINVAL);
    }

    if (!codec->extradata || codec->extradata_size < TAK_MIN_STREAMINFO_BYTES) {
        av_log(codec, AV_LOG_ERROR, "extradata NULL or too small.\n");
        return AVERROR(EINVAL);
    }

    ffio_wfourcc(ctx->pb, "tBaK");

    init_get_bits8(&gb, codec->extradata, codec->extradata_size);
    avpriv_tak_parse_streaminfo(&gb, &s->metadata);
    size = write_streaminfo(&s->metadata, buffer, sizeof(buffer));

    avio_w8(ctx->pb, TAK_METADATA_STREAMINFO);
    avio_wl24(ctx->pb, size + 3);
    ffio_init_checksum(ctx->pb, tak_check_crc, 0xCE04B7U);
    avio_write(ctx->pb, buffer, size);
    avio_wb24(ctx->pb, ffio_get_checksum(ctx->pb));

    if (ctx->pb->seekable) {
        avio_w8(ctx->pb, TAK_METADATA_PADDING);
        avio_wl24(ctx->pb, 11);
        ffio_fill(ctx->pb, 0, 11);
    }

    avio_w8(ctx->pb, TAK_METADATA_END);
    avio_wl24(ctx->pb, 0);
    s->metadata.samples = 0;

    return 0;
}

static int tak_write_packet(struct AVFormatContext *ctx, AVPacket *pkt)
{
    TAKMuxContext *s = ctx->priv_data;
    GetBitContext gb;
    int ret;

    if (pkt->size < TAK_MIN_FRAME_HEADER_BYTES ||
        (ret = init_get_bits8(&gb, pkt->data, pkt->size)) < 0 ||
        (ret = avpriv_tak_decode_frame_header(ctx->streams[0]->codec, &gb, &s->hdr, 0) < 0)) {
        av_log(ctx, AV_LOG_ERROR, "Invalid TAK packet.\n");
        return AVERROR(EINVAL);
    }
    s->metadata.samples += s->hdr.frame_samples;
    s->offset = avio_tell(ctx->pb);
    s->size = pkt->size;
    avio_write(ctx->pb, pkt->data, pkt->size);

    return 0;
}

static int tak_write_trailer(struct AVFormatContext *ctx)
{
    TAKMuxContext *s = ctx->priv_data;

    ff_ape_write_tag(ctx);

    if (ctx->pb->seekable) {
        uint8_t buffer[TAK_MAX_STREAMINFO_BYTES];
        int64_t file_size;
        int size;

        size = write_streaminfo(&s->metadata, buffer, sizeof(buffer));
        file_size = avio_tell(ctx->pb);
        avio_seek(ctx->pb, 8, SEEK_SET);
        ffio_init_checksum(ctx->pb, tak_check_crc, 0xCE04B7U);
        avio_write(ctx->pb, buffer, size);
        avio_wb24(ctx->pb, ffio_get_checksum(ctx->pb));
        avio_w8(ctx->pb, TAK_METADATA_LAST_FRAME);
        avio_wl24(ctx->pb, 11);
        ffio_init_checksum(ctx->pb, tak_check_crc, 0xCE04B7U);
        avio_wl32(ctx->pb, s->offset & 0xFFFFFFFF);
        avio_w8(ctx->pb, (s->offset >> 32) & 0xFF);
        avio_wl24(ctx->pb, s->size);
        avio_wb24(ctx->pb, ffio_get_checksum(ctx->pb));
        avio_seek(ctx->pb, file_size, SEEK_SET);
    }

    return 0;
}

AVOutputFormat ff_tak_muxer = {
    .name           = "tak",
    .long_name      = NULL_IF_CONFIG_SMALL("raw TAK"),
    .priv_data_size = sizeof(TAKMuxContext),
    .audio_codec    = AV_CODEC_ID_TAK,
    .video_codec    = AV_CODEC_ID_NONE,
    .write_header   = tak_write_header,
    .write_packet   = tak_write_packet,
    .write_trailer  = tak_write_trailer,
    .flags          = AVFMT_NOTIMESTAMPS,
    .extensions     = "tak",
};
