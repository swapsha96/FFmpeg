/*
 * Copyright (c) 2015 Paul B Mahol
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

#include "libavutil/avassert.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "avfilter.h"
#include "formats.h"
#include "video.h"
#include "internal.h"

enum Modes {
    MIN,
    MAX,
    AVG,
    NB_MODES
};

typedef struct SpectrographContext {
    const AVClass *class;
    int mode;
    int layout;
    int size;

    int nb_planes;
    int iplanewidth[4];
    int iplaneheight[4];
    int oplanewidth[4];
    int oplaneheight[4];
    const uint8_t *black;
    AVFrame *out;
} SpectrographContext;

#define OFFSET(x) offsetof(SpectrographContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM

static const AVOption spectrograph_options[] = {
    { "mode", "set mode", OFFSET(mode), AV_OPT_TYPE_INT, {.i64=0}, 0, NB_MODES-1, FLAGS, "mode"},
    { "m",    "set mode", OFFSET(mode), AV_OPT_TYPE_INT, {.i64=0}, 0, NB_MODES-1, FLAGS, "mode"},
        {"min", 0, OFFSET(mode), AV_OPT_TYPE_CONST, {.i64=MIN}, 0, 0, FLAGS, "mode"},
        {"max", 0, OFFSET(mode), AV_OPT_TYPE_CONST, {.i64=MAX}, 0, 0, FLAGS, "mode"},
        {"avg", 0, OFFSET(mode), AV_OPT_TYPE_CONST, {.i64=AVG}, 0, 0, FLAGS, "mode"},
    { "layout", "set layout", OFFSET(layout), AV_OPT_TYPE_INT, {.i64=0}, 0, 1, FLAGS, "layout"},
    { "l",      "set layout", OFFSET(layout), AV_OPT_TYPE_INT, {.i64=0}, 0, 1, FLAGS, "layout"},
        {"row",    0, OFFSET(layout), AV_OPT_TYPE_CONST, {.i64=0}, 0, 0, FLAGS, "layout"},
        {"column", 0, OFFSET(layout), AV_OPT_TYPE_CONST, {.i64=1}, 0, 0, FLAGS, "layout"},
    { "size", "set size", OFFSET(size), AV_OPT_TYPE_INT, {.i64=0}, 0, INT_MAX, FLAGS,},
    { "s",    "set size", OFFSET(size), AV_OPT_TYPE_INT, {.i64=0}, 0, INT_MAX, FLAGS,},
    { NULL }
};

AVFILTER_DEFINE_CLASS(spectrograph);

static int query_formats(AVFilterContext *ctx)
{
    AVFilterFormats *formats = NULL;
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_YUV444P, AV_PIX_FMT_YUVJ444P,
        AV_PIX_FMT_NONE
    };

    formats = ff_make_format_list(pix_fmts);
    if (!formats)
        return AVERROR(ENOMEM);
    return ff_set_common_formats(ctx, formats);
}

static int config_input(AVFilterLink *inlink)
{
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);
    AVFilterContext *ctx = inlink->dst;
    SpectrographContext *s = ctx->priv;

    s->nb_planes = desc->nb_components;

    s->iplaneheight[1] = s->iplaneheight[2] = FF_CEIL_RSHIFT(inlink->h, desc->log2_chroma_h);
    s->iplaneheight[0] = s->iplaneheight[3] = inlink->h;
    s->iplanewidth[1]  = s->iplanewidth[2]  = FF_CEIL_RSHIFT(inlink->w, desc->log2_chroma_w);
    s->iplanewidth[0]  = s->iplanewidth[3]  = inlink->w;

    return 0;
}

static const uint8_t black_yuva_color[4] = { 16, 127, 127, 255 };

static int config_output(AVFilterLink *outlink)
{
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(outlink->format);
    AVFilterContext *ctx = outlink->src;
    SpectrographContext *s = ctx->priv;

    outlink->sample_aspect_ratio = (AVRational){1,1};
    switch (s->layout) {
    case 0:
        outlink->h = ctx->inputs[0]->h;
        outlink->w = s->size ? s->size : ctx->inputs[0]->w;
        break;
    case 1:
        outlink->h = s->size ? s->size : ctx->inputs[0]->h;
        outlink->w = ctx->inputs[0]->w;
        break;
    }

    s->oplaneheight[1] = s->oplaneheight[2] = FF_CEIL_RSHIFT(outlink->h, desc->log2_chroma_h);
    s->oplaneheight[0] = s->oplaneheight[3] = outlink->h;
    s->oplanewidth[1]  = s->oplanewidth[2]  = FF_CEIL_RSHIFT(outlink->w, desc->log2_chroma_w);
    s->oplanewidth[0]  = s->oplanewidth[3]  = outlink->w;

    s->black = black_yuva_color;

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext *ctx = inlink->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    SpectrographContext *s = ctx->priv;
    AVFrame *out;
    int p, x, y;

    if (!s->out || s->out->width  != outlink->w ||
                   s->out->height != outlink->h) {
        av_frame_free(&s->out);
        s->out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
        if (!s->out) {
            av_frame_free(&in);
            return AVERROR(ENOMEM);
        }

        out = s->out;
        for (p = 0; p < s->nb_planes; p++) {
            for (y = 0; y < s->oplaneheight[p]; y++)
                memset(out->data[p] + y * out->linesize[p], s->black[p], s->oplanewidth[p]);
        }
    } else {
        out = s->out;
        if (s->layout) {
            for (p = 0; p < s->nb_planes; p++) {
                for (y = s->oplaneheight[p] - 1; y > 0; y--)
                    memmove(out->data[p] + (y  ) * out->linesize[p],
                            out->data[p] + (y-1) * out->linesize[p], s->oplanewidth[p]);
            }
        } else {
            for (p = 0; p < s->nb_planes; p++) {
                for (y = 0; y < s->oplaneheight[p]; y++)
                    memmove(out->data[p] + y * out->linesize[p] + 1,
                            out->data[p] + y * out->linesize[p], s->oplanewidth[p] - 1);
            }
        }
    }

    for (p = 0; p < s->nb_planes; p++) {
        switch (s->layout) {
        case 0:
            for (y = 0; y < s->iplaneheight[p]; y++) {
                const uint8_t *src = in->data[p] + y * in->linesize[p];
                uint8_t *dst = out->data[p] + y * out->linesize[p];
                unsigned r;

                switch (s->mode) {
                case MIN:
                    r = 255;
                    for (x = 0; x < s->iplanewidth[p]; x++)
                        r = FFMIN(r, src[x]);
                    break;
                case MAX:
                    r = 0;
                    for (x = 0; x < s->iplanewidth[p]; x++)
                        r = FFMAX(r, src[x]);
                    break;
                case AVG:
                    r = 0;
                    for (x = 0; x < s->iplanewidth[p]; x++)
                        r += src[x];
                    r /= s->iplanewidth[p];
                    break;
                }

                dst[0] = r;
            }
            break;
        case 1: {
            const uint8_t *src = in->data[p];
            uint8_t *dst = out->data[p];
            for (x = 0; x < s->iplanewidth[p]; x++) {
                unsigned r;

                switch (s->mode) {
                case MIN:
                    r = 255;
                    for (y = 0; y < s->iplaneheight[p]; y++)
                        r = FFMIN(r, src[y * in->linesize[p] + x]);
                    break;
                case MAX:
                    r = 0;
                    for (y = 0; y < s->iplaneheight[p]; y++)
                        r = FFMAX(r, src[y * in->linesize[p] + x]);
                    break;
                case AVG:
                    r = 0;
                    for (y = 0; y < s->iplaneheight[p]; y++)
                        r += src[y * in->linesize[p] + x];
                    r /= s->iplaneheight[p];
                    break;
                }

                dst[x] = r; }
            }
            break;
        }
    }

    s->out->pts = in->pts;

    av_frame_free(&in);
    return ff_filter_frame(outlink, av_frame_clone(s->out));
}

static av_cold void uninit(AVFilterContext *ctx)
{
    SpectrographContext *s = ctx->priv;

    av_frame_free(&s->out);
}

static const AVFilterPad inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = config_input,
        .filter_frame = filter_frame,
    },
    { NULL }
};

static const AVFilterPad outputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = config_output,
    },
    { NULL }
};

AVFilter ff_vf_spectrograph = {
    .name          = "spectrograph",
    .description   = NULL_IF_CONFIG_SMALL("Spectrograph."),
    .uninit        = uninit,
    .query_formats = query_formats,
    .priv_size     = sizeof(SpectrographContext),
    .inputs        = inputs,
    .outputs       = outputs,
    .priv_class    = &spectrograph_class,
};
