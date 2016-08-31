/*
 * Copyright (c) 2012-2013 Oka Motofumi (chikuzen.mo at gmail dot com)
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

#include "libavutil/avstring.h"
#include "libavutil/imgutils.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "video.h"

typedef struct GBlurContext {
    const AVClass *class;

    float sigma;
    int planes;

    int depth;
    int planewidth[4];
    int planeheight[4];
    int radius;
    float *kernel;
    float *buffer;
    uint8_t **temp8;
    uint16_t **temp16;
    int nb_planes;
} GBlurContext;

#define OFFSET(x) offsetof(GBlurContext, x)
#define FLAGS AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_FILTERING_PARAM

static const AVOption gblur_options[] = {
    { "sigma",  "set sigma",            OFFSET(sigma),  AV_OPT_TYPE_FLOAT, {.dbl=0.0}, 0.0, 1024, FLAGS },
    { "planes", "set planes to filter", OFFSET(planes), AV_OPT_TYPE_INT,   {.i64=0xF},   0,  0xF, FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(gblur);

static int set_kernel(GBlurContext *s)
{
    float sum;
    int rad, i, len;

    if (s->sigma == 0.0f)
        return 0;

    rad = (int)(s->sigma * 3.0f + 0.5f);
    if (rad < 1)
        rad = 1;

    s->kernel = av_calloc(rad * 2 + 1, sizeof(*s->kernel));
    if (!s->kernel)
        return AVERROR(ENOMEM);

    sum = 0.0f;

    for (i = -rad; i <= rad; i++) {
        float weight = expf(-(i * i)/(2.0f * s->sigma * s->sigma));
        s->kernel[i + rad] = weight;
        sum += weight;
    }

    for (i = 0, len = rad * 2 + 1; i < len; s->kernel[i++] /= sum);
    s->radius = rad;
    return 0;
}

#define PROC_HORIZONTAL(depth, type)                                                \
static void proc_horizontal##depth(float *srcp, int radius, int length, int width,  \
                                   float *kernel, type *dstp)                       \
{                                                                                   \
    int i, x;                                                                       \
                                                                                    \
    for (i = 1; i <= radius; i++) {                                                 \
        srcp[-i] = srcp[i];                                                         \
        srcp[width - 1 + i] = srcp[width - 1 - i];                                  \
    }                                                                               \
                                                                                    \
    for (x = 0; x < width; x++) {                                                   \
        float sum = 0.0f;                                                           \
                                                                                    \
        for (i = -radius; i <= radius; i++) {                                       \
            sum += srcp[x + i] * kernel[i + radius];                                \
        }                                                                           \
        dstp[x] = sum;                                                              \
    }                                                                               \
}

PROC_HORIZONTAL(8, uint8_t)
PROC_HORIZONTAL(16, uint16_t)

#define BLUR(depth, type)                                                           \
static void gblur##depth(int radius, float *kernel, const type *srcp, float *buff,  \
                         type *dstp, int width, int height,                         \
                         int src_stride, int dst_stride, type **temp)               \
{                                                                                   \
    int length = radius * 2 + 1;                                                    \
    int y, x, i;                                                                    \
                                                                                    \
    dst_stride /= sizeof(type);                                                     \
    src_stride /= sizeof(type);                                                     \
                                                                                    \
    for (i = -radius; i <= radius; i++) {                                           \
        temp[i + radius] = (type *)srcp + FFABS(i) * src_stride;                    \
    }                                                                               \
                                                                                    \
    for (y = 0; y < height; y++) {                                                  \
        for (x = 0; x < width; x++) {                                               \
            float sum = 0.0f;                                                       \
            for (i = 0; i < length; i++) {                                          \
                sum += temp[i][x] * kernel[i];                                      \
            }                                                                       \
            buff[x] = sum;                                                          \
        }                                                                           \
                                                                                    \
        proc_horizontal##depth(buff, radius, length, width, kernel, dstp);          \
                                                                                    \
        for (i = 0; i < length - 1; i++) {                                          \
            temp[i] = temp[i + 1];                                                  \
        }                                                                           \
        temp[length - 1] += (y < height - radius - 1 ? 1 : -1) * src_stride;        \
        dstp += dst_stride;                                                         \
    }                                                                               \
}

BLUR(8, uint8_t)
BLUR(16, uint16_t)

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_YUVA444P, AV_PIX_FMT_YUV444P, AV_PIX_FMT_YUV440P,
        AV_PIX_FMT_YUVJ444P, AV_PIX_FMT_YUVJ440P,
        AV_PIX_FMT_YUVA422P, AV_PIX_FMT_YUV422P, AV_PIX_FMT_YUVA420P, AV_PIX_FMT_YUV420P,
        AV_PIX_FMT_YUVJ422P, AV_PIX_FMT_YUVJ420P,
        AV_PIX_FMT_YUVJ411P, AV_PIX_FMT_YUV411P, AV_PIX_FMT_YUV410P,
        AV_PIX_FMT_YUV420P9, AV_PIX_FMT_YUV422P9, AV_PIX_FMT_YUV444P9,
        AV_PIX_FMT_YUV420P10, AV_PIX_FMT_YUV422P10, AV_PIX_FMT_YUV444P10,
        AV_PIX_FMT_YUV420P12, AV_PIX_FMT_YUV422P12, AV_PIX_FMT_YUV444P12, AV_PIX_FMT_YUV440P12,
        AV_PIX_FMT_YUV420P14, AV_PIX_FMT_YUV422P14, AV_PIX_FMT_YUV444P14,
        AV_PIX_FMT_YUV420P16, AV_PIX_FMT_YUV422P16, AV_PIX_FMT_YUV444P16,
        AV_PIX_FMT_YUVA420P9, AV_PIX_FMT_YUVA422P9, AV_PIX_FMT_YUVA444P9,
        AV_PIX_FMT_YUVA420P10, AV_PIX_FMT_YUVA422P10, AV_PIX_FMT_YUVA444P10,
        AV_PIX_FMT_YUVA420P16, AV_PIX_FMT_YUVA422P16, AV_PIX_FMT_YUVA444P16,
        AV_PIX_FMT_GBRP, AV_PIX_FMT_GBRP9, AV_PIX_FMT_GBRP10,
        AV_PIX_FMT_GBRP12, AV_PIX_FMT_GBRP14, AV_PIX_FMT_GBRP16,
        AV_PIX_FMT_GBRAP, AV_PIX_FMT_GBRAP12, AV_PIX_FMT_GBRAP16,
        AV_PIX_FMT_GRAY8, AV_PIX_FMT_GRAY16,
        AV_PIX_FMT_NONE
    };

    return ff_set_common_formats(ctx, ff_make_format_list(pix_fmts));
}

static int config_input(AVFilterLink *inlink)
{
    GBlurContext *s = inlink->dst->priv;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);

    s->depth = desc->comp[0].depth;
    s->planewidth[1] = s->planewidth[2] = AV_CEIL_RSHIFT(inlink->w, desc->log2_chroma_w);
    s->planewidth[0] = s->planewidth[3] = inlink->w;
    s->planeheight[1] = s->planeheight[2] = AV_CEIL_RSHIFT(inlink->h, desc->log2_chroma_h);
    s->planeheight[0] = s->planeheight[3] = inlink->h;

    s->nb_planes = av_pix_fmt_count_planes(inlink->format);

    s->buffer = av_malloc_array(inlink->w + FFALIGN(s->radius, 32) * 2, sizeof(*s->buffer));
    if (!s->buffer)
        return AVERROR(ENOMEM);
    if (s->depth == 8) {
        s->temp8 = av_calloc(s->radius * 2 + 1, sizeof(*s->temp8));
        if (!s->temp8)
            return AVERROR(ENOMEM);
    } else {
        s->temp16 = av_calloc(s->radius * 2 + 1, sizeof(*s->temp16));
        if (!s->temp16)
            return AVERROR(ENOMEM);
    }
    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    GBlurContext *s = inlink->dst->priv;
    AVFilterLink *outlink = inlink->dst->outputs[0];
    AVFrame *out;
    int plane;

    out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    if (!out) {
        av_frame_free(&in);
        return AVERROR(ENOMEM);
    }
    av_frame_copy_props(out, in);

    for (plane = 0; plane < s->nb_planes; plane++) {
        if (!s->sigma || !(s->planes & (1 << plane))) {
            av_image_copy_plane(out->data[plane], out->linesize[plane],
                                in->data[plane], in->linesize[plane],
                                s->planewidth[plane] * s->depth / 8,
                                s->planeheight[plane]);
            continue;
        }

        if (s->depth == 8) {
            gblur8(s->radius, s->kernel, in->data[plane], s->buffer + FFALIGN(s->radius, 32), out->data[plane],
                   s->planewidth[plane], s->planeheight[plane],
                   in->linesize[plane], out->linesize[plane], s->temp8);
        } else {
            gblur16(s->radius, s->kernel, (const uint16_t *)in->data[plane], s->buffer + FFALIGN(s->radius, 32), (uint16_t *)out->data[plane],
                    s->planewidth[plane], s->planeheight[plane],
                    in->linesize[plane], out->linesize[plane], s->temp16);
        }
    }

    av_frame_free(&in);
    return ff_filter_frame(outlink, out);
}

static av_cold int init(AVFilterContext *ctx)
{
    GBlurContext *s = ctx->priv;

    return set_kernel(s);
}

static av_cold void uninit(AVFilterContext *ctx)
{
    GBlurContext *s = ctx->priv;

    av_freep(&s->buffer);
    av_freep(&s->kernel);
    av_freep(&s->temp8);
    av_freep(&s->temp16);
}

static const AVFilterPad gblur_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = config_input,
        .filter_frame = filter_frame,
    },
    { NULL }
};

static const AVFilterPad gblur_outputs[] = {
    {
        .name = "default",
        .type = AVMEDIA_TYPE_VIDEO,
    },
    { NULL }
};

AVFilter ff_vf_gblur = {
    .name          = "gblur",
    .description   = NULL_IF_CONFIG_SMALL("Apply Gaussian Blur filter."),
    .priv_size     = sizeof(GBlurContext),
    .priv_class    = &gblur_class,
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = gblur_inputs,
    .outputs       = gblur_outputs,
    .flags         = AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC,
};

