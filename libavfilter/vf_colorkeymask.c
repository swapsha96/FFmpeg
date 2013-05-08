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

#include "libavutil/opt.h"
#include "libavutil/parseutils.h"
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
    const AVClass *class;
    int rt, gt, bt;
    int is_rgba;
    uint8_t lut[3][256];
    uint8_t color[4];
    uint8_t rgba_map[4];
} ColorKeyMaskContext;

#define OFFSET(x) offsetof(ColorKeyMaskContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM
static const AVOption colorkeymask_options[] = {
    { "color", "set color",   OFFSET(color), AV_OPT_TYPE_COLOR, {.str="black"}, 0, 0, FLAGS },
    { "r", "set red tolerance",   OFFSET(rt), AV_OPT_TYPE_INT, {.i64=10}, 0, 255, FLAGS },
    { "g", "set green tolerance", OFFSET(gt), AV_OPT_TYPE_INT, {.i64=10}, 0, 255, FLAGS },
    { "b", "set blue tolerance",  OFFSET(bt), AV_OPT_TYPE_INT, {.i64=10}, 0, 255, FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(colorkeymask);

static av_cold int init(AVFilterContext *ctx)
{
    ColorKeyMaskContext *s = ctx->priv;
    uint8_t rmin, gmin, bmin;
    uint8_t rmax, gmax, bmax;
    int i;

    rmin = av_clip_uint8(s->color[R] - s->rt);
    gmin = av_clip_uint8(s->color[G] - s->gt);
    bmin = av_clip_uint8(s->color[B] - s->bt);
    rmax = av_clip_uint8(s->color[R] + s->rt);
    gmax = av_clip_uint8(s->color[G] + s->gt);
    bmax = av_clip_uint8(s->color[B] + s->bt);

    for (i = 0; i < 256; i++) {
        s->lut[R][i] = (i >= rmin && i <= rmax);
        s->lut[G][i] = (i >= gmin && i <= gmax);
        s->lut[B][i] = (i >= bmin && i <= bmax);
    }

    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_RGBA,  AV_PIX_FMT_BGRA,
        AV_PIX_FMT_ABGR,  AV_PIX_FMT_ARGB,
        AV_PIX_FMT_YUVA444P,
        AV_PIX_FMT_NONE
    };

    ff_set_common_formats(ctx, ff_make_format_list(pix_fmts));
    return 0;
}

static int config_input(AVFilterLink *inlink)
{
    ColorKeyMaskContext *s = inlink->dst->priv;
    s->is_rgba = ff_fill_rgba_map(s->rgba_map, inlink->format) >= 0;
    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *frame)
{
    AVFilterContext *ctx = inlink->dst;
    ColorKeyMaskContext *s = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    const uint8_t r = s->rgba_map[R];
    const uint8_t g = s->rgba_map[G];
    const uint8_t b = s->rgba_map[B];
    const uint8_t a = s->rgba_map[A];
    const int bytewidth = inlink->w * 4;
    uint8_t *dst;
    int y, x;

    dst = frame->data[0];

    if (s->is_rgba) {
        for (y = 0; y < inlink->h; y++) {
            for (x = 0; x < bytewidth; x += 4) {
                if (s->lut[R][dst[x + r]] &&
                    s->lut[G][dst[x + g]] &&
                    s->lut[B][dst[x + b]])
                    dst[x + a] = 0;
            }
            dst += frame->linesize[0];
        }
    } else {
        for (y = 0; y < inlink->h; y++) {
            for (x = 0; x < inlink->w; x++) {
                if (s->lut[R][frame->data[0][y * frame->linesize[0] + x]] &&
                    s->lut[G][frame->data[1][y * frame->linesize[1] + x]] &&
                    s->lut[B][frame->data[2][y * frame->linesize[2] + x]])
                    frame->data[3][y * frame->linesize[3] + x] = 0;
            }
        }
    }

    return ff_filter_frame(outlink, frame);
}

static const AVFilterPad colorkeymask_inputs[] = {
    {
        .name           = "default",
        .type           = AVMEDIA_TYPE_VIDEO,
        .filter_frame   = filter_frame,
        .config_props   = config_input,
        .needs_writable = 1,
    },
    { NULL }
};

static const AVFilterPad colorkeymask_outputs[] = {
    {
        .name           = "default",
        .type           = AVMEDIA_TYPE_VIDEO,
    },
    { NULL }
};

AVFilter avfilter_vf_colorkeymask = {
    .name          = "colorkeymask",
    .description   = NULL_IF_CONFIG_SMALL("Clear alpha channel by comparing non-alpha channels."),
    .priv_size     = sizeof(ColorKeyMaskContext),
    .priv_class    = &colorkeymask_class,
    .init          = init,
    .query_formats = query_formats,
    .inputs        = colorkeymask_inputs,
    .outputs       = colorkeymask_outputs,
    .flags         = AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC,
};
