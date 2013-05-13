/*
 * Copyright (c) 2013 Paul B Mahol
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

#include "libavutil/imgutils.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "avfilter.h"
#include "drawutils.h"
#include "formats.h"
#include "internal.h"
#include "video.h"

#define R 0
#define G 1
#define B 2
#define A 3

typedef struct {
    double in_min, in_max;
    double out_min, out_max;
} Range;

typedef struct ColorLevelsContext {
    const AVClass *class;
    Range range[4];
    int nb_comp;
    int step;
    uint8_t rgba_map[4];
    int linesize[4];
    int height[4];

    void (*levels)(struct ColorLevelsContext *s, AVFrame *in, AVFrame *out);
} ColorLevelsContext;

#define OFFSET(x) offsetof(ColorLevelsContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM
static const AVOption colorlevels_options[] = {
    { "c0imin", "set input component #0 black point",  OFFSET(range[0].in_min),  AV_OPT_TYPE_DOUBLE, {.dbl=0}, -1, 1, FLAGS },
    { "c1imin", "set input component #1 black point",  OFFSET(range[1].in_min),  AV_OPT_TYPE_DOUBLE, {.dbl=0}, -1, 1, FLAGS },
    { "c2imin", "set input component #2 black point",  OFFSET(range[2].in_min),  AV_OPT_TYPE_DOUBLE, {.dbl=0}, -1, 1, FLAGS },
    { "c3imin", "set input component #3 black point",  OFFSET(range[3].in_min),  AV_OPT_TYPE_DOUBLE, {.dbl=0}, -1, 1, FLAGS },
    { "c0imax", "set input component #0 white point",  OFFSET(range[0].in_max),  AV_OPT_TYPE_DOUBLE, {.dbl=1}, -1, 1, FLAGS },
    { "c1imax", "set input component #1 white point",  OFFSET(range[1].in_max),  AV_OPT_TYPE_DOUBLE, {.dbl=1}, -1, 1, FLAGS },
    { "c2imax", "set input component #2 white point",  OFFSET(range[2].in_max),  AV_OPT_TYPE_DOUBLE, {.dbl=1}, -1, 1, FLAGS },
    { "c3imax", "set input component #3 white point",  OFFSET(range[3].in_max),  AV_OPT_TYPE_DOUBLE, {.dbl=1}, -1, 1, FLAGS },
    { "c0omin", "set output component #0 black point", OFFSET(range[0].out_min), AV_OPT_TYPE_DOUBLE, {.dbl=0},  0, 1, FLAGS },
    { "c1omin", "set output component #1 black point", OFFSET(range[1].out_min), AV_OPT_TYPE_DOUBLE, {.dbl=0},  0, 1, FLAGS },
    { "c2omin", "set output component #2 black point", OFFSET(range[2].out_min), AV_OPT_TYPE_DOUBLE, {.dbl=0},  0, 1, FLAGS },
    { "c3omin", "set output component #3 black point", OFFSET(range[3].out_min), AV_OPT_TYPE_DOUBLE, {.dbl=0},  0, 1, FLAGS },
    { "c0omax", "set output component #0 white point", OFFSET(range[0].out_max), AV_OPT_TYPE_DOUBLE, {.dbl=1},  0, 1, FLAGS },
    { "c1omax", "set output component #1 white point", OFFSET(range[1].out_max), AV_OPT_TYPE_DOUBLE, {.dbl=1},  0, 1, FLAGS },
    { "c2omax", "set output component #2 white point", OFFSET(range[2].out_max), AV_OPT_TYPE_DOUBLE, {.dbl=1},  0, 1, FLAGS },
    { "c3omax", "set output component #3 white point", OFFSET(range[3].out_max), AV_OPT_TYPE_DOUBLE, {.dbl=1},  0, 1, FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(colorlevels);

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_0RGB,  AV_PIX_FMT_0BGR,
        AV_PIX_FMT_ARGB,  AV_PIX_FMT_ABGR,
        AV_PIX_FMT_RGB0,  AV_PIX_FMT_BGR0,
        AV_PIX_FMT_RGB24, AV_PIX_FMT_BGR24,
        AV_PIX_FMT_RGB48, AV_PIX_FMT_BGR48,
        AV_PIX_FMT_RGBA64, AV_PIX_FMT_BGRA64,
        AV_PIX_FMT_RGBA,  AV_PIX_FMT_BGRA,
        AV_PIX_FMT_YUV410P,
        AV_PIX_FMT_YUV411P,
        AV_PIX_FMT_YUV420P,
        AV_PIX_FMT_YUV422P,
        AV_PIX_FMT_YUV440P,
        AV_PIX_FMT_YUV444P,
        AV_PIX_FMT_GRAY8,
        AV_PIX_FMT_NONE
    };

    ff_set_common_formats(ctx, ff_make_format_list(pix_fmts));
    return 0;
}

static void colorlevels_yuv8(ColorLevelsContext *s, AVFrame *in, AVFrame *out)
{
    int i, x, y;

    for (i = 0; i < s->nb_comp; i++) {
        Range *r = &s->range[i];
        const uint8_t *srcrow = in->data[i];
        const int linesize = s->linesize[i];
        const int h = s->height[i];
        uint8_t *dstrow = out->data[i];
        int imin = round(r->in_min  * UINT8_MAX);
        int imax = round(r->in_max  * UINT8_MAX);
        int omin = round(r->out_min * UINT8_MAX);
        int omax = round(r->out_max * UINT8_MAX);
        double coeff;

        if (imin < 0) {
            imin = UINT8_MAX;
            for (y = 0; y < h; y++) {
                const uint8_t *src = srcrow;

                for (x = 0; x < linesize; x++)
                    imin = FFMIN(imin, src[x]);
                srcrow += in->linesize[i];
            }
        }
        if (imax < 0) {
            srcrow = in->data[i];
            imax = 0;
            for (y = 0; y < h; y++) {
                const uint8_t *src = srcrow;

                for (x = 0; x < linesize; x++)
                    imax = FFMAX(imax, src[x]);
                srcrow += in->linesize[i];
            }
        }

        srcrow = in->data[i];
        coeff = (omax - omin) / (double)(imax - imin);
        for (y = 0; y < h; y++) {
            const uint8_t *src = srcrow;
            uint8_t *dst = dstrow;

            for (x = 0; x < linesize; x++)
                dst[x] = av_clip_uint8((src[x] - imin) * coeff + omin);
            dstrow += out->linesize[i];
            srcrow += in->linesize[i];
        }
    }
}

static void colorlevels_rgb8(ColorLevelsContext *s, AVFrame *in, AVFrame *out)
{
    const int step = s->step;
    const int linesize = s->linesize[0];
    const int h = s->height[0];
    int i, x, y;

    for (i = 0; i < s->nb_comp; i++) {
        Range *r = &s->range[i];
        const uint8_t offset = s->rgba_map[i];
        const uint8_t *srcrow = in->data[0];
        uint8_t *dstrow = out->data[0];
        int imin = round(r->in_min  * UINT8_MAX);
        int imax = round(r->in_max  * UINT8_MAX);
        int omin = round(r->out_min * UINT8_MAX);
        int omax = round(r->out_max * UINT8_MAX);
        double coeff;

        if (imin < 0) {
            imin = UINT8_MAX;
            for (y = 0; y < h; y++) {
                const uint8_t *src = srcrow;

                for (x = 0; x < linesize; x += step)
                    imin = FFMIN(imin, src[x + offset]);
                srcrow += in->linesize[0];
            }
        }
        if (imax < 0) {
            srcrow = in->data[0];
            imax = 0;
            for (y = 0; y < h; y++) {
                const uint8_t *src = srcrow;

                for (x = 0; x < linesize; x += step)
                    imax = FFMAX(imax, src[x + offset]);
                srcrow += in->linesize[0];
            }
        }

        srcrow = in->data[0];
        coeff = (omax - omin) / (double)(imax - imin);
        for (y = 0; y < h; y++) {
            const uint8_t *src = srcrow;
            uint8_t *dst = dstrow;

            for (x = 0; x < linesize; x += step)
                dst[x + offset] = av_clip_uint8((src[x + offset] - imin) * coeff + omin);
            dstrow += out->linesize[0];
            srcrow += in->linesize[0];
        }
    }
}

static void colorlevels_rgb16(ColorLevelsContext *s, AVFrame *in, AVFrame *out)
{
    const int step = s->step;
    const int linesize = s->linesize[0];
    const int h = s->height[0];
    int i, x, y;

    for (i = 0; i < s->nb_comp; i++) {
        Range *r = &s->range[i];
        const uint8_t offset = s->rgba_map[i];
        const uint8_t *srcrow = in->data[0];
        uint8_t *dstrow = out->data[0];
        int imin = round(r->in_min  * UINT16_MAX);
        int imax = round(r->in_max  * UINT16_MAX);
        int omin = round(r->out_min * UINT16_MAX);
        int omax = round(r->out_max * UINT16_MAX);
        double coeff;

        if (imin < 0) {
            imin = UINT16_MAX;
            for (y = 0; y < h; y++) {
                const uint16_t *src = (const uint16_t *)srcrow;

                for (x = 0; x < linesize; x += step)
                    imin = FFMIN(imin, src[x + offset]);
                srcrow += in->linesize[0];
            }
        }
        if (imax < 0) {
            srcrow = in->data[0];
            imax = 0;
            for (y = 0; y < h; y++) {
                const uint16_t *src = (const uint16_t *)srcrow;

                for (x = 0; x < linesize; x += step)
                    imax = FFMAX(imax, src[x + offset]);
                srcrow += in->linesize[0];
            }
        }

        srcrow = in->data[0];
        coeff = (omax - omin) / (double)(imax - imin);
        for (y = 0; y < h; y++) {
            const uint16_t *src = (const uint16_t*)srcrow;
            uint16_t *dst = (uint16_t *)dstrow;

            for (x = 0; x < linesize; x += step)
                dst[x + offset] = av_clip_uint16((src[x + offset] - imin) * coeff + omin);
            dstrow += out->linesize[0];
            srcrow += in->linesize[0];
        }
    }
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    ColorLevelsContext *s = ctx->priv;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);
    int bpp, ret;

    s->nb_comp = desc->nb_components;
    bpp = (desc->comp[0].depth_minus1 + 1) >> 3;
    s->step = (av_get_padded_bits_per_pixel(desc) >> 3) / bpp;
    ff_fill_rgba_map(s->rgba_map, inlink->format);
    s->height[1] = s->height[2] = FF_CEIL_RSHIFT(inlink->h, desc->log2_chroma_h);
    s->height[0] = s->height[3] = inlink->h;

    if (desc->flags & AV_PIX_FMT_FLAG_PLANAR) {
        if ((ret = av_image_fill_linesizes(s->linesize, inlink->format, inlink->w)) < 0)
            return ret;

        switch (bpp) {
        case 1: s->levels = colorlevels_yuv8;  break;
        }
    } else {
        s->linesize[0] = inlink->w * s->step;

        switch (bpp) {
        case 1: s->levels = colorlevels_rgb8;  break;
        case 2: s->levels = colorlevels_rgb16; break;
        }
    }

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext *ctx = inlink->dst;
    ColorLevelsContext *s = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    AVFrame *out;

    if (av_frame_is_writable(in)) {
        out = in;
    } else {
        out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
        if (!out) {
            av_frame_free(&in);
            return AVERROR(ENOMEM);
        }
        av_frame_copy_props(out, in);
    }

    s->levels(s, in, out);

    if (in != out)
        av_frame_free(&in);
    return ff_filter_frame(outlink, out);
}

static const AVFilterPad colorlevels_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = filter_frame,
        .config_props = config_input,
    },
    { NULL }
};

static const AVFilterPad colorlevels_outputs[] = {
    {
        .name = "default",
        .type = AVMEDIA_TYPE_VIDEO,
    },
    { NULL }
};

AVFilter avfilter_vf_colorlevels = {
    .name          = "colorlevels",
    .description   = NULL_IF_CONFIG_SMALL("Adjust the color levels."),
    .priv_size     = sizeof(ColorLevelsContext),
    .priv_class    = &colorlevels_class,
    .query_formats = query_formats,
    .inputs        = colorlevels_inputs,
    .outputs       = colorlevels_outputs,
    .flags         = AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC,
};
