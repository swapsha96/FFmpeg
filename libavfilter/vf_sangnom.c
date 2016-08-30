/*
 * Copyright (c) 2013 Victor Efimov
 * Copyright (c) 2016 james1201
 *
 * This file is part of FFmpeg.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

#include "libavutil/avassert.h"
#include "libavutil/cpu.h"
#include "libavutil/common.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "libavutil/imgutils.h"
#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "video.h"

enum SANGNOMMode {
    SANGNOM_MODE_SEND_FRAME           = 0, ///< send 1 frame for each frame
    SANGNOM_MODE_SEND_FIELD           = 1, ///< send 1 frame for each field
};

enum SANGNOMParity {
    SANGNOM_PARITY_TFF  =  0, ///< top field first
    SANGNOM_PARITY_BFF  =  1, ///< bottom field first
    SANGNOM_PARITY_AUTO = -1, ///< auto detection
};

enum SANGNOMDeint {
    SANGNOM_DEINT_ALL        = 0, ///< deinterlace all frames
    SANGNOM_DEINT_INTERLACED = 1, ///< only deinterlace frames marked as interlaced
};

enum Buffers
{
    ADIFF_M3_P3 = 0,
    ADIFF_M2_P2 = 1,
    ADIFF_M1_P1 = 2,
    ADIFF_P0_M0 = 4,
    ADIFF_P1_M1 = 6,
    ADIFF_P2_M2 = 7,
    ADIFF_P3_M3 = 8,

    SG_FORWARD = 3,
    SG_REVERSE = 5,

    TOTAL_BUFFERS = 9,
};

enum BorderMode
{
    LEFT,
    RIGHT,
    NONE
};

typedef struct SangNomContext {
    const AVClass *class;

    int mode;           ///< SANGNOMMode
    int parity;         ///< SANGNOMParity
    int deint;          ///< SANGNOMDeint
    int aa;
    int algo;

    AVFrame *src;
    AVFrame *second;
    AVFrame *dst;

    int nb_planes;
    int linesize[4];      ///< bytes of pixel data per line for each plane
    int planeheight[4];   ///< height of each plane
    int eof;
    int offset;
    float aaf;
    int64_t cur_pts;

    uint8_t *buffer_pool;
    uint8_t *buffers[TOTAL_BUFFERS];
    int16_t *buffer_temp;
    int buffer_stride;
    int buffer_height;
} SangNomContext;

static inline void copy_field(const uint8_t *srcp, const int src_stride,
                              uint8_t *dstp, const int dst_stride,
                              const int w, const int h, SangNomContext *s)
{
    if (s->offset == 0) {
        // keep top field so the bottom line can't be interpolated
        // just copy the data from its correspond top field
        memcpy(dstp + (h - 1) * dst_stride, srcp + (h - 2) * src_stride, w);
    } else {
        // keep bottom field so the top line can't be interpolated
        // just copy the data from its correspond bottom field
        memcpy(dstp, srcp + src_stride, w);
    }
}

static inline uint8_t absdiff(int a, int b)
{
    return av_clip_uint8(FFABS(a - b));
}

static inline int16_t load_pixel(const uint8_t *srcp, int curPos, int offset, int width)
{
    int reqPos = curPos + offset;

    if (reqPos >= 0 && reqPos < width)
        return srcp[reqPos];

    if (reqPos >= 0)
        return srcp[width - 1];

    return srcp[0];
}

static inline int16_t load_pixel_i16(const int16_t *srcp, int curPos, int offset, int width)
{
    int reqPos = curPos + offset;

    if (reqPos >= 0 && reqPos < width)
        return srcp[reqPos];

    if (reqPos >= 0)
        return srcp[width - 1];

    return srcp[0];
}

static inline int16_t calculate_sangnom(const uint8_t p1, const uint8_t p2, const uint8_t p3)
{
    int sum = p1 * 5 + p2 * 4 - p3;

    return av_clip_int16(sum / 8);
}

static inline void prepare_buffers(const uint8_t *srcp, const int src_stride,
                                   const int w, const int h,
                                   const int buffer_stride, uint8_t *buffers[TOTAL_BUFFERS])
{
    const uint8_t *srcpn2 = srcp + src_stride * 2;
    int buffer_offset = buffer_stride;
    int y, x;

    for (y = 0; y < h / 2 - 1; ++y) {
        for (x = 0; x < w; ++x) {
            const int16_t currLineM3 = load_pixel(srcp, x, -3, w);
            const int16_t currLineM2 = load_pixel(srcp, x, -2, w);
            const int16_t currLineM1 = load_pixel(srcp, x, -1, w);
            const int16_t currLine   = srcp[x];
            const int16_t currLineP1 = load_pixel(srcp, x, 1, w);
            const int16_t currLineP2 = load_pixel(srcp, x, 2, w);
            const int16_t currLineP3 = load_pixel(srcp, x, 3, w);

            const int16_t nextLineM3 = load_pixel(srcpn2, x, -3, w);
            const int16_t nextLineM2 = load_pixel(srcpn2, x, -2, w);
            const int16_t nextLineM1 = load_pixel(srcpn2, x, -1, w);
            const int16_t nextLine   = srcpn2[x];
            const int16_t nextLineP1 = load_pixel(srcpn2, x, 1, w);
            const int16_t nextLineP2 = load_pixel(srcpn2, x, 2, w);
            const int16_t nextLineP3 = load_pixel(srcpn2, x, 3, w);

            const int16_t forwardSangNom1 = calculate_sangnom(currLineM1, currLine, currLineP1);
            const int16_t forwardSangNom2 = calculate_sangnom(nextLineP1, nextLine, nextLineM1);
            const int16_t backwardSangNom1 = calculate_sangnom(currLineP1, currLine, currLineM1);
            const int16_t backwardSangNom2 = calculate_sangnom(nextLineM1, nextLine, nextLineP1);

            buffers[ADIFF_M3_P3][buffer_offset + x] = absdiff(currLineM3, nextLineP3);
            buffers[ADIFF_M2_P2][buffer_offset + x] = absdiff(currLineM2, nextLineP2);
            buffers[ADIFF_M1_P1][buffer_offset + x] = absdiff(currLineM1, nextLineP1);
            buffers[ADIFF_P0_M0][buffer_offset + x] = absdiff(currLine,   nextLine);
            buffers[ADIFF_P1_M1][buffer_offset + x] = absdiff(currLineP1, nextLineM1);
            buffers[ADIFF_P2_M2][buffer_offset + x] = absdiff(currLineP2, nextLineM2);
            buffers[ADIFF_P3_M3][buffer_offset + x] = absdiff(currLineP3, nextLineM3);

            buffers[SG_FORWARD][buffer_offset + x] = absdiff(forwardSangNom1, forwardSangNom2);
            buffers[SG_REVERSE][buffer_offset + x] = absdiff(backwardSangNom1, backwardSangNom2);
        }

        srcp += src_stride * 2;
        srcpn2 += src_stride * 2;
        buffer_offset += buffer_stride;
    }
}

static inline void finalize_plane(const uint8_t *srcp, const int src_stride,
                                  uint8_t *dstp, const int dst_stride,
                                  const int w, const int h,
                                  const int buffer_stride,
                                  const uint8_t aaf, uint8_t *buffers[TOTAL_BUFFERS])
{
    const uint8_t *srcpn2 = srcp + src_stride * 2;
    uint8_t *dstpn = dstp + dst_stride;
    int bufferOffset = buffer_stride;
    int y, x, i;

    for (y = 0; y < h / 2 - 1; ++y) {
        for (x = 0; x < w; ++x) {
            const int16_t currLineM3 = load_pixel(srcp, x, -3, w);
            const int16_t currLineM2 = load_pixel(srcp, x, -2, w);
            const int16_t currLineM1 = load_pixel(srcp, x, -1, w);
            const int16_t currLine   = srcp[x];
            const int16_t currLineP1 = load_pixel(srcp, x, 1, w);
            const int16_t currLineP2 = load_pixel(srcp, x, 2, w);
            const int16_t currLineP3 = load_pixel(srcp, x, 3, w);

            const int16_t nextLineM3 = load_pixel(srcpn2, x, -3, w);
            const int16_t nextLineM2 = load_pixel(srcpn2, x, -2, w);
            const int16_t nextLineM1 = load_pixel(srcpn2, x, -1, w);
            const int16_t nextLine   = srcpn2[x];
            const int16_t nextLineP1 = load_pixel(srcpn2, x, 1, w);
            const int16_t nextLineP2 = load_pixel(srcpn2, x, 2, w);
            const int16_t nextLineP3 = load_pixel(srcpn2, x, 3, w);

            const int16_t forwardSangNom1 = calculate_sangnom(currLineM1, currLine, currLineP1);
            const int16_t forwardSangNom2 = calculate_sangnom(nextLineP1, nextLine, nextLineM1);
            const int16_t backwardSangNom1 = calculate_sangnom(currLineP1, currLine, currLineM1);
            const int16_t backwardSangNom2 = calculate_sangnom(nextLineM1, nextLine, nextLineP1);

            int16_t buf[9];
            int16_t minbuf;

            buf[0] = buffers[ADIFF_M3_P3][bufferOffset + x];
            buf[1] = buffers[ADIFF_M2_P2][bufferOffset + x];
            buf[2] = buffers[ADIFF_M1_P1][bufferOffset + x];
            buf[3] = buffers[SG_FORWARD][bufferOffset + x];
            buf[4] = buffers[ADIFF_P0_M0][bufferOffset + x];
            buf[5] = buffers[SG_REVERSE][bufferOffset + x];
            buf[6] = buffers[ADIFF_P1_M1][bufferOffset + x];
            buf[7] = buffers[ADIFF_P2_M2][bufferOffset + x];
            buf[8] = buffers[ADIFF_P3_M3][bufferOffset + x];

            minbuf = buf[0];
            for (i = 1; i < TOTAL_BUFFERS; ++i)
                minbuf = FFMIN(minbuf, buf[i]);

            /* the order of following code is important, don't change it */
            if (buf[4] == minbuf || minbuf >= aaf) {
                dstpn[x] = (currLine + nextLine + 1) >> 1;
            } else if (buf[5] == minbuf) {
                dstpn[x] = (backwardSangNom1 + backwardSangNom2 + 1) >> 1;
            } else if (buf[3] == minbuf) {
                dstpn[x] = (forwardSangNom1 + forwardSangNom2 + 1) >> 1;
            } else if (buf[6] == minbuf) {
                dstpn[x] = (currLineP1 + nextLineM1 + 1) >> 1;
            } else if (buf[2] == minbuf) {
                dstpn[x] = (currLineM1 + nextLineP1 + 1) >> 1;
            } else if (buf[7] == minbuf) {
                dstpn[x] = (currLineP2 + nextLineM2 + 1) >> 1;
            } else if (buf[1] == minbuf) {
                dstpn[x] = (currLineM2 + nextLineP2 + 1) >> 1;
            } else if (buf[8] == minbuf) {
                dstpn[x] = (currLineP3 + nextLineM3 + 1) >> 1;
            } else if (buf[0] == minbuf) {
                dstpn[x] = (currLineM3 + nextLineP3 + 1) >> 1;
            }
        }

        srcp += src_stride * 2;
        srcpn2 += src_stride * 2;
        dstpn += dst_stride * 2;
        bufferOffset += buffer_stride;
    }
}

static inline void process_buffers_org(uint8_t *bufferp, int16_t *buffer_temp, const int buffer_stride,
                                       const int buffer_height)
{
    uint8_t *bufferpc = bufferp + buffer_stride;
    uint8_t *bufferpp1 = bufferpc - buffer_stride;
    uint8_t *bufferpn1 = bufferpc + buffer_stride;
    int16_t *buffer_tempc = buffer_temp + buffer_stride;
    int y, x;

    for (y = 0; y < buffer_height - 1; ++y) {
        for (x = 0; x < buffer_stride; ++x) {
            buffer_tempc[x] = bufferpp1[x] + bufferpc[x] + bufferpn1[x];
        }

        for (x = 0; x < buffer_stride; ++x) {
            const int16_t currLineM3 = load_pixel_i16(buffer_tempc, x, -3, buffer_stride);
            const int16_t currLineM2 = load_pixel_i16(buffer_tempc, x, -2, buffer_stride);
            const int16_t currLineM1 = load_pixel_i16(buffer_tempc, x, -1, buffer_stride);
            const int16_t currLine   = buffer_tempc[x];
            const int16_t currLineP1 = load_pixel_i16(buffer_tempc, x, 1, buffer_stride);
            const int16_t currLineP2 = load_pixel_i16(buffer_tempc, x, 2, buffer_stride);
            const int16_t currLineP3 = load_pixel_i16(buffer_tempc, x, 3, buffer_stride);

            bufferpc[x] = (currLineM3 + currLineM2 + currLineM1 + currLine + currLineP1 + currLineP2 + currLineP3) / 16;
        }

        bufferpc  += buffer_stride;
        bufferpp1 += buffer_stride;
        bufferpn1 += buffer_stride;
    }
}

static inline void process_buffers_new(uint8_t *bufferp, int16_t *buffer_temp,
                                       const int buffer_stride, const int buffer_height)
{
    uint8_t *bufferpc = bufferp + buffer_stride;
    uint8_t *bufferpp1 = bufferpc - buffer_stride;
    uint8_t *bufferpn1 = bufferpc + buffer_stride;
    int16_t *buffer_tempc = buffer_temp + buffer_stride;
    int y, x;

    for (y = 0; y < buffer_height - 1; ++y) {
        for (x = 0; x < buffer_stride; ++x) {
            buffer_tempc[x] = bufferpp1[x] + bufferpc[x] + bufferpn1[x];
        }

        bufferpc += buffer_stride;
        bufferpp1 += buffer_stride;
        bufferpn1 += buffer_stride;
        buffer_tempc += buffer_stride;
    }

    bufferpc = bufferp + buffer_stride;
    buffer_tempc = buffer_temp + buffer_stride;

    for (y = 0; y < buffer_height - 1; ++y) {
        for (x = 0; x < buffer_stride; ++x) {
            const int16_t currLineM3 = load_pixel_i16(buffer_tempc, x, -3, buffer_stride);
            const int16_t currLineM2 = load_pixel_i16(buffer_tempc, x, -2, buffer_stride);
            const int16_t currLineM1 = load_pixel_i16(buffer_tempc, x, -1, buffer_stride);
            const int16_t currLine   = buffer_tempc[x];
            const int16_t currLineP1 = load_pixel_i16(buffer_tempc, x, 1, buffer_stride);
            const int16_t currLineP2 = load_pixel_i16(buffer_tempc, x, 2, buffer_stride);
            const int16_t currLineP3 = load_pixel_i16(buffer_tempc, x, 3, buffer_stride);

            bufferpc[x] = (currLineM3 + currLineM2 + currLineM1 + currLine + currLineP1 + currLineP2 + currLineP3) / 16;
        }

        bufferpc += buffer_stride;
        buffer_tempc += buffer_stride;
    }
}

static void filter(AVFilterContext *ctx, AVFrame *dst)
{
    SangNomContext *s = ctx->priv;
    int p;

    for (p = 0; p < s->nb_planes; p++) {
        int w = s->linesize[p];
        int h = s->planeheight[p];
        uint8_t *srcp = s->src->data[p];
        int src_stride = s->src->linesize[p];
        uint8_t *dstp = dst->data[p];
        int dst_stride = dst->linesize[p];
        int i;

        av_image_copy_plane(dst->data[p] + s->offset * dst->linesize[p],
                            dst->linesize[p] * 2,
                            s->src->data[p] + s->offset * s->src->linesize[p],
                            s->src->linesize[p] * 2,
                            s->linesize[p],
                            s->planeheight[p] / 2);

        copy_field(s->src->data[p], s->src->linesize[p],
                   dst->data[p], dst->linesize[p], w, h, s);

        prepare_buffers(srcp + s->offset * src_stride, src_stride, w, h, s->buffer_stride, s->buffers);

        if (s->algo = 0) {
            for (i = 0; i < TOTAL_BUFFERS; ++i)
                process_buffers_org(s->buffers[i], s->buffer_temp, s->buffer_stride, s->buffer_height);
        } else {
            for (i = 0; i < TOTAL_BUFFERS; ++i)
                process_buffers_new(s->buffers[i], s->buffer_temp, s->buffer_stride, s->buffer_height);
        }

        finalize_plane(srcp + s->offset * src_stride, src_stride,
                       dstp + s->offset * dst_stride, dst_stride,
                       w, h, s->buffer_stride, s->aaf, s->buffers);
    }
}

static int return_frame(AVFilterContext *ctx, int is_second)
{
    SangNomContext *s = ctx->priv;
    AVFilterLink *link = ctx->outputs[0];
    int tff;

    if (s->parity == -1) {
        tff = s->src->interlaced_frame ?
              s->src->top_field_first : 1;
    } else {
        tff = s->parity ^ 1;
    }

    s->dst = ff_get_video_buffer(link, link->w, link->h);
    if (!s->dst)
        return AVERROR(ENOMEM);

    av_frame_copy_props(s->dst, s->src);
    s->dst->interlaced_frame = 0;

    s->offset = tff == 0;
    if (is_second)
        s->offset = !s->offset;

    filter(ctx, s->dst);

    return 0;
}

static int filter_frame(AVFilterLink *link, AVFrame *frame)
{
    AVFilterContext *ctx = link->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    SangNomContext *s = ctx->priv;
    int ret;

    if ((s->mode > 0) && !s->second) {
        goto second;
    } else if (s->mode > 0) {
        AVFrame *dst;

        s->src = s->second;
        ret = return_frame(ctx, 1);
        if (ret < 0) {
            av_frame_free(&s->dst);
            av_frame_free(&s->src);
            av_frame_free(&s->second);
            return ret;
        }
        dst = s->dst;

        if (frame->pts != AV_NOPTS_VALUE &&
            dst->pts != AV_NOPTS_VALUE)
            dst->pts += frame->pts;
        else
            dst->pts = AV_NOPTS_VALUE;

        ret = ff_filter_frame(outlink, dst);
        if (ret < 0)
            return ret;
        if (s->eof)
            return 0;
        s->cur_pts = s->second->pts;
        av_frame_free(&s->second);
second:
        if ((s->deint && frame->interlaced_frame &&
             !ctx->is_disabled) ||
            (!s->deint && !ctx->is_disabled)) {
            s->second = frame;
        }
    }

    if ((s->deint && !s->src->interlaced_frame) ||
        ctx->is_disabled) {
        s->dst = av_frame_clone(s->src);
        if (!s->dst)
            return AVERROR(ENOMEM);

        av_frame_free(&s->src);
        if (s->dst->pts != AV_NOPTS_VALUE)
            s->dst->pts *= 2;
        return ff_filter_frame(outlink, s->dst);
    }

    s->src = frame;
    ret = return_frame(ctx, 0);
    if (ret < 0) {
        av_frame_free(&s->dst);
        av_frame_free(&s->src);
        av_frame_free(&s->second);
        return ret;
    }

    if (frame->pts != AV_NOPTS_VALUE)
        s->dst->pts = frame->pts * 2;
    if (s->mode == 0) {
        av_frame_free(&frame);
        s->src = NULL;
    }

    return ff_filter_frame(outlink, s->dst);
}

static int request_frame(AVFilterLink *link)
{
    AVFilterContext *ctx = link->src;
    SangNomContext *s = ctx->priv;
    int ret;

    if (s->eof)
        return AVERROR_EOF;

    ret  = ff_request_frame(ctx->inputs[0]);

    if (ret == AVERROR_EOF && s->second) {
        AVFrame *next = av_frame_clone(s->second);

        if (!next)
            return AVERROR(ENOMEM);

        next->pts = s->second->pts * 2 - s->cur_pts;
        s->eof = 1;

        filter_frame(ctx->inputs[0], next);
    } else if (ret < 0) {
        return ret;
    }

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    SangNomContext *s = ctx->priv;

    av_freep(&s->buffer_pool);
    av_freep(&s->buffer_temp);
}

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_YUV410P, AV_PIX_FMT_YUV411P,
        AV_PIX_FMT_YUV420P, AV_PIX_FMT_YUV422P,
        AV_PIX_FMT_YUV440P, AV_PIX_FMT_YUV444P,
        AV_PIX_FMT_YUVJ444P, AV_PIX_FMT_YUVJ440P,
        AV_PIX_FMT_YUVJ422P, AV_PIX_FMT_YUVJ420P,
        AV_PIX_FMT_YUVJ411P,
        AV_PIX_FMT_GRAY8,
        AV_PIX_FMT_NONE
    };

    AVFilterFormats *fmts_list = ff_make_format_list(pix_fmts);
    if (!fmts_list)
        return AVERROR(ENOMEM);
    return ff_set_common_formats(ctx, fmts_list);
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    SangNomContext *s = ctx->priv;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);
    int ret, i;

    s->nb_planes = av_pix_fmt_count_planes(inlink->format);
    if ((ret = av_image_fill_linesizes(s->linesize, inlink->format, inlink->w)) < 0)
        return ret;

    s->planeheight[1] = s->planeheight[2] = AV_CEIL_RSHIFT(inlink->h, desc->log2_chroma_h);
    s->planeheight[0] = s->planeheight[3] = inlink->h;
    s->aaf = s->aa * 21.0f / 16.0f;
    s->buffer_stride = FFALIGN(inlink->w, 32);
    s->buffer_height = (inlink->h + 1) >> 1;

    s->buffer_pool = av_malloc_array(sizeof(*s->buffer_pool) * s->buffer_stride, (s->buffer_height + 1) * TOTAL_BUFFERS);
    // separate bufferpool to multiple pieces
    for (i = 0; i < TOTAL_BUFFERS; ++i)
        s->buffers[i] = s->buffer_pool + i * s->buffer_stride * (s->buffer_height + 1);
    s->buffer_temp = av_malloc_array(sizeof(*s->buffer_temp) * s->buffer_stride, (s->buffer_height + 1));
    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    SangNomContext *s = ctx->priv;

    outlink->time_base.num = ctx->inputs[0]->time_base.num;
    outlink->time_base.den = ctx->inputs[0]->time_base.den * 2;
    outlink->w             = ctx->inputs[0]->w;
    outlink->h             = ctx->inputs[0]->h;

    if (s->mode > 0)
        outlink->frame_rate = av_mul_q(ctx->inputs[0]->frame_rate,
                                       (AVRational){2, 1});

    return 0;
}

#define OFFSET(x) offsetof(SangNomContext, x)
#define FLAGS AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_FILTERING_PARAM

#define CONST(name, help, val, unit) { name, help, 0, AV_OPT_TYPE_CONST, {.i64=val}, INT_MIN, INT_MAX, FLAGS, unit }

static const AVOption sangnom_options[] = {
    { "mode",   "specify the interlacing mode", OFFSET(mode), AV_OPT_TYPE_INT, {.i64=SANGNOM_MODE_SEND_FRAME}, 0, 3, FLAGS, "mode"},
    CONST("send_frame",           "send one frame for each frame",                                     SANGNOM_MODE_SEND_FRAME,           "mode"),
    CONST("send_field",           "send one frame for each field",                                     SANGNOM_MODE_SEND_FIELD,           "mode"),

    { "parity", "specify the assumed picture field parity", OFFSET(parity), AV_OPT_TYPE_INT, {.i64=SANGNOM_PARITY_AUTO}, -1, 1, FLAGS, "parity" },
    CONST("tff",  "assume top field first",    SANGNOM_PARITY_TFF,  "parity"),
    CONST("bff",  "assume bottom field first", SANGNOM_PARITY_BFF,  "parity"),
    CONST("auto", "auto detect parity",        SANGNOM_PARITY_AUTO, "parity"),

    { "deint", "specify which frames to deinterlace", OFFSET(deint), AV_OPT_TYPE_INT, {.i64=SANGNOM_DEINT_ALL}, 0, 1, FLAGS, "deint" },
    CONST("all",        "deinterlace all frames",                       SANGNOM_DEINT_ALL,         "deint"),
    CONST("interlaced", "only deinterlace frames marked as interlaced", SANGNOM_DEINT_INTERLACED,  "deint"),

    { "aa", "specify the strength of anti-aliasing", OFFSET(aa), AV_OPT_TYPE_INT, {.i64=48}, 0, 128, FLAGS },
    { "algo", "specify the algorithm", OFFSET(algo), AV_OPT_TYPE_INT, {.i64=0}, 0, 1, FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(sangnom);

static const AVFilterPad avfilter_vf_sangnom_inputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .filter_frame  = filter_frame,
        .config_props  = config_input,
    },
    { NULL }
};

static const AVFilterPad avfilter_vf_sangnom_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .request_frame = request_frame,
        .config_props  = config_output,
    },
    { NULL }
};

AVFilter ff_vf_sangnom = {
    .name          = "sangnom",
    .description   = NULL_IF_CONFIG_SMALL("Apply a single field deinterlacer using edge-directed interpolation."),
    .priv_size     = sizeof(SangNomContext),
    .priv_class    = &sangnom_class,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = avfilter_vf_sangnom_inputs,
    .outputs       = avfilter_vf_sangnom_outputs,
    .flags         = AVFILTER_FLAG_SUPPORT_TIMELINE_INTERNAL | AVFILTER_FLAG_SLICE_THREADS,
};
