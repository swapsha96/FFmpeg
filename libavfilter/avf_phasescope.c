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

/**
 * @file
 * audio to video multimedia phase scope filter
 */

#include "libavutil/avassert.h"
#include "libavutil/channel_layout.h"
#include "libavutil/opt.h"
#include "libavutil/parseutils.h"
#include "avfilter.h"
#include "formats.h"
#include "audio.h"
#include "video.h"
#include "internal.h"

enum ScopeMode {
    LISSAJOUS,
    LISSAJOUS_XY,
    POLAR,
    CORRELATION,
    BALANCE,
    MODE_NB,
};

typedef struct PhaseScopeContext {
    const AVClass *class;
    AVFrame *outpicref;
    int w, h;
    int hw, hh;
    enum ScopeMode mode;
    int contrast[3];
    int dissolve[3];
    AVRational frame_rate;

    void (*filter)(struct PhaseScopeContext *p, AVFrame *insamples);
} PhaseScopeContext;

#define OFFSET(x) offsetof(PhaseScopeContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM

static const AVOption phasescope_options[] = {
    { "mode", "set mode", OFFSET(mode), AV_OPT_TYPE_INT, {.i64=LISSAJOUS}, 0, MODE_NB-1, FLAGS, "mode" },
    { "m",    "set mode", OFFSET(mode), AV_OPT_TYPE_INT, {.i64=LISSAJOUS}, 0, MODE_NB-1, FLAGS, "mode" },
    { "lissajous",   "", 0, AV_OPT_TYPE_CONST, {.i64=LISSAJOUS},   0, 0, FLAGS, "mode" },
    { "lissajous_xy", "", 0, AV_OPT_TYPE_CONST, {.i64=LISSAJOUS_XY}, 0, 0, FLAGS, "mode" },
    { "polar",       "", 0, AV_OPT_TYPE_CONST, {.i64=POLAR},       0, 0, FLAGS, "mode" },
    { "correlation", "", 0, AV_OPT_TYPE_CONST, {.i64=CORRELATION}, 0, 0, FLAGS, "mode" },
    { "balance",     "", 0, AV_OPT_TYPE_CONST, {.i64=BALANCE},     0, 0, FLAGS, "mode" },
    { "rate", "set video rate", OFFSET(frame_rate), AV_OPT_TYPE_VIDEO_RATE, {.str="25"}, 0, 0, FLAGS },
    { "r",    "set video rate", OFFSET(frame_rate), AV_OPT_TYPE_VIDEO_RATE, {.str="25"}, 0, 0, FLAGS },
    { "size", "set video size", OFFSET(w), AV_OPT_TYPE_IMAGE_SIZE, {.str="320x240"}, 0, 0, FLAGS },
    { "s",    "set video size", OFFSET(w), AV_OPT_TYPE_IMAGE_SIZE, {.str="320x240"}, 0, 0, FLAGS },
    { "contrast",   "set luma contrast", OFFSET(contrast[0]), AV_OPT_TYPE_INT, {.i64=255}, 1, 255, FLAGS },
    { "c",          "set luma contrast", OFFSET(contrast[0]), AV_OPT_TYPE_INT, {.i64=255}, 1, 255, FLAGS },
    { "dissolve",   "set luma dissolve speed", OFFSET(dissolve[0]), AV_OPT_TYPE_INT, {.i64=1}, 0, 255, FLAGS },
    { "d",          "set luma dissolve speed", OFFSET(dissolve[0]), AV_OPT_TYPE_INT, {.i64=1}, 0, 255, FLAGS },
    { "ccontrast",  "set chroma contrast", OFFSET(contrast[1]), AV_OPT_TYPE_INT, {.i64=0}, -128, 128, FLAGS },
    { "cc",         "set chroma contrast", OFFSET(contrast[1]), AV_OPT_TYPE_INT, {.i64=0}, -128, 128, FLAGS },
    { "cdissolve",  "set chroma dissolve speed", OFFSET(dissolve[1]), AV_OPT_TYPE_INT, {.i64=0}, 0, 255, FLAGS },
    { "cd",         "set chroma dissolve speed", OFFSET(dissolve[1]), AV_OPT_TYPE_INT, {.i64=0}, 0, 255, FLAGS },
    { "ccontrast2", "set chroma2 contrast", OFFSET(contrast[2]), AV_OPT_TYPE_INT, {.i64=0}, -128, 128, FLAGS },
    { "cc2",        "set chroma2 contrast", OFFSET(contrast[2]), AV_OPT_TYPE_INT, {.i64=0}, -128, 128, FLAGS },
    { "cdissolve2", "set chroma2 dissolve speed", OFFSET(dissolve[2]), AV_OPT_TYPE_INT, {.i64=0}, 0, 255, FLAGS },
    { "cd2",        "set chroma2 dissolve speed", OFFSET(dissolve[2]), AV_OPT_TYPE_INT, {.i64=0}, 0, 255, FLAGS },
    { NULL },
};

AVFILTER_DEFINE_CLASS(phasescope);

static void draw_dot(PhaseScopeContext *p, unsigned x, unsigned y)
{
    const int linesize = p->outpicref->linesize[0];
    uint8_t *dst;

    y = FFMIN(y, p->h - 1);
    x = FFMIN(x, p->w - 1);

    dst = &p->outpicref->data[0][y * linesize + x];
    *dst = FFMIN(*dst + p->contrast[0], 255);
    dst = &p->outpicref->data[1][y * linesize + x];
    if (p->contrast[1] > 0)
        *dst = FFMAX(*dst - p->contrast[1], 0);
    else
        *dst = FFMIN(*dst - p->contrast[1], 255);
    dst = &p->outpicref->data[2][y * linesize + x];
    if (p->contrast[2] > 0)
        *dst = FFMAX(*dst - p->contrast[2], 0);
    else
        *dst = FFMIN(*dst - p->contrast[2], 255);
}

static void fade(PhaseScopeContext *p)
{
    const int linesize = p->outpicref->linesize[0];
    int v, i, j;

    if (p->dissolve[0] || p->dissolve[1] || p->dissolve[2]) {
        uint8_t *d[3];
        d[0] = p->outpicref->data[0];
        d[1] = p->outpicref->data[1];
        d[2] = p->outpicref->data[2];
        for (i = 0; i < p->h; i++) {
            for (j = 0; j < p->w; j++) {
                v = d[0][j];
                d[0][j] = FFMAX(v - p->dissolve[0], 0);
                v = d[1][j];
                if (v < 128)
                    d[1][j] = FFMIN(v + p->dissolve[1], 128);
                else
                    d[1][j] = FFMAX(v - p->dissolve[1], 128);
                v = d[2][j];
                if (v < 128)
                    d[2][j] = FFMIN(v + p->dissolve[2], 128);
                else
                    d[2][j] = FFMAX(v - p->dissolve[2], 128);
            }
            d[0] += linesize;
            d[1] += linesize;
            d[2] += linesize;
        }
    }

}

static void lissajous(PhaseScopeContext *p, AVFrame *insamples)
{
    const int hw = p->hw;
    const int hh = p->hh;
    unsigned x, y;
    int i;

    fade(p);

    switch (insamples->format) {
    case AV_SAMPLE_FMT_S16:
        for (i = 0; i < insamples->nb_samples; i++) {
            int16_t *src = (int16_t *)insamples->data[0] + i * 2;

            if (p->mode == LISSAJOUS) {
                x = ((src[1] - src[0]) / (float)(UINT16_MAX) + 1) * hw;
                y = (1.0 - (src[0] + src[1]) / (float)UINT16_MAX) * hh;
            } else {
                x = (src[1] / (float)INT16_MAX + 1) * hw;
                y = (src[0] / (float)INT16_MAX + 1) * hh;
            }

            draw_dot(p, x, y);
        }
        break;
    case AV_SAMPLE_FMT_FLT:
        for (i = 0; i < insamples->nb_samples; i++) {
            float *src = (float *)insamples->data[0] + i * 2;

            if (p->mode == LISSAJOUS) {
                x = ((src[1] - src[0]) / 2 + 1) * hw;
                y = (1.0 - (src[0] + src[1]) / 2) * hh;
            } else {
                x = (src[1] + 1) * hw;
                y = (src[0] + 1) * hh;
            }

            draw_dot(p, x, y);
        }
        break;
    }
}

static void polar(PhaseScopeContext *p, AVFrame *insamples)
{
    const int hw = p->hw;
    unsigned x, y;
    int i;

    fade(p);

    switch (insamples->format) {
    case AV_SAMPLE_FMT_S16:
        for (i = 0; i < insamples->nb_samples; i++) {
            int16_t *src = (int16_t *)insamples->data[0] + i * 2;

            x = (1 + cos((FFABS(src[0] - src[1]) / (float)UINT16_MAX + 1) * M_PI / 2)) * hw;
            y = (1.0 - FFABS(src[0] + src[1]) / (float)UINT16_MAX) * p->h;

            draw_dot(p, x, y);
        }
        break;
    case AV_SAMPLE_FMT_FLT:
        for (i = 0; i < insamples->nb_samples; i++) {
            float *src = (float *)insamples->data[0] + i * 2;

            x = (1 + cos(((src[0] - src[1]) / 2 + 1) * M_PI / 2)) * hw;
            y = (1.0 - fabs(src[0] + src[1]) / 2) * p->h;

            draw_dot(p, x, y);
        }
        break;
    }
}

static void correlation(PhaseScopeContext *p, AVFrame *insamples)
{
    const int linesize = p->outpicref->linesize[0];
    unsigned v;
    int i, j;
    long is = 0;
    float aa, a2, a1, n1 = 0, n2 = 0;

    for (i = 0; i < insamples->nb_samples; i++) {
        for (j = i; j < insamples->nb_samples; j++) {
            switch (insamples->format) {
            case AV_SAMPLE_FMT_S16: {
                int16_t *x = (int16_t*)insamples->data[0];
                int16_t *y = (int16_t*)insamples->data[0] + 1;

                a1 = x[i * 2] - x[j * 2];
                a2 = y[i * 2] - y[j * 2];
                break;
            }
            case AV_SAMPLE_FMT_FLT: {
                float *x = (float*)insamples->data[0];
                float *y = (float*)insamples->data[0] + 1;

                a1 = x[i * 2] - x[j * 2];
                a2 = y[i * 2] - y[j * 2];
                break;
            }
            }

            aa = a1 * a2;
            if (aa) {
                ++n1;
                ++n2;
                aa > 0.0 ? ++is : --is;
            } else {
                if (a1) ++n1;
                if (a2) ++n2;
            }
        }
    }


    if (p->dissolve[0]) {
        uint8_t *dst = p->outpicref->data[0];
        for (i = 0; i < p->h; i++) {
            for (j = 0; j < p->w; j++) {
                dst[j] = FFMAX(dst[j] - p->dissolve[0], 0);
            }
            dst += linesize;
        }
    }

    if (p->w > p->h) {
        v = (1 - is / (sqrtf(n1 * n2) + 1.0e-23)) * p->hw;
        av_assert0(v < p->w);
        for (i = 0; i < p->h; i++)
            p->outpicref->data[0][i * linesize + v] = 255;
    } else {
        v = (1 - is / (sqrtf(n1 * n2) + 1.0e-23)) * p->hh;
        av_assert0(v < p->h);
        memset(&p->outpicref->data[0][v * linesize], 225, p->w);
    }
}

static void balance(PhaseScopeContext *p, AVFrame *insamples)
{
    const int linesize = p->outpicref->linesize[0];
    float b = 0;
    unsigned v;
    int i, j;

    switch (insamples->format) {
    case AV_SAMPLE_FMT_S16:
        for (i = 0; i < insamples->nb_samples; i++) {
            int16_t *src = (int16_t *)insamples->data[0] + i * 2;

            b += (FFABS(src[1]) - FFABS(src[0])) / (float)INT16_MAX;
        }
        break;
    case AV_SAMPLE_FMT_FLT:
        for (i = 0; i < insamples->nb_samples; i++) {
            float *src = (float *)insamples->data[0] + i * 2;

            b += av_clipf(fabs(src[1]) - fabs(src[0]), -1, 1);
        }
        break;
    }
    b /= i;

    if (p->dissolve[0]) {
        uint8_t *dst = p->outpicref->data[0];
        for (i = 0; i < p->h; i++) {
            for (j = 0; j < p->w; j++) {
                dst[j] = FFMAX(dst[j] - p->dissolve[0], 0);
            }
            dst += linesize;
        }
    }

    if (p->w > p->h) {
        v = (b + 1) * p->hw;
        av_assert0(v < p->w);
        for (i = 0; i < p->h; i++)
            p->outpicref->data[0][i * linesize + v] = 255;
    } else {
        v = (b + 1) * p->hh;
        av_assert0(v < p->h);
        memset(&p->outpicref->data[0][v * linesize], 225, p->w);
    }
}

static av_cold int init(AVFilterContext *ctx, const char *args)
{
    PhaseScopeContext *p = ctx->priv;

    switch (p->mode) {
    case LISSAJOUS:
    case LISSAJOUS_XY: p->filter = lissajous;   break;
    case POLAR:        p->filter = polar;       break;
    case CORRELATION:  p->filter = correlation; break;
    case BALANCE:      p->filter = balance;     break;
    default: av_assert0(0);
    }

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    PhaseScopeContext *p = ctx->priv;

    av_frame_free(&p->outpicref);
}

static int query_formats(AVFilterContext *ctx)
{
    AVFilterFormats *formats = NULL;
    AVFilterChannelLayouts *layout = NULL;
    AVFilterLink *inlink = ctx->inputs[0];
    AVFilterLink *outlink = ctx->outputs[0];
    static const enum AVSampleFormat sample_fmts[] = { AV_SAMPLE_FMT_S16, AV_SAMPLE_FMT_FLT, AV_SAMPLE_FMT_NONE };
    static const enum AVPixelFormat pix_fmts[] = { AV_PIX_FMT_YUVJ444P, AV_PIX_FMT_NONE };

    formats = ff_make_format_list(sample_fmts);
    if (!formats)
        return AVERROR(ENOMEM);
    ff_formats_ref(formats, &inlink->out_formats);

    ff_add_channel_layout(&layout, AV_CH_LAYOUT_STEREO);
    ff_channel_layouts_ref(layout, &inlink->out_channel_layouts);

    formats = ff_all_samplerates();
    if (!formats)
        return AVERROR(ENOMEM);
    ff_formats_ref(formats, &inlink->out_samplerates);

    formats = ff_make_format_list(pix_fmts);
    if (!formats)
        return AVERROR(ENOMEM);
    ff_formats_ref(formats, &outlink->in_formats);

    return 0;
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    PhaseScopeContext *p = ctx->priv;
    int nb_samples;

    nb_samples = FFMAX(1024, ((double)inlink->sample_rate / av_q2d(p->frame_rate)) + 0.5);
    inlink->partial_buf_size =
    inlink->min_samples =
    inlink->max_samples = nb_samples;

    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    PhaseScopeContext *p = outlink->src->priv;
    int linesize;

    outlink->w = p->w;
    outlink->h = p->h;
    outlink->sample_aspect_ratio = (AVRational){1,1};
    outlink->frame_rate = p->frame_rate;
    p->outpicref = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    if (!p->outpicref)
        return AVERROR(ENOMEM);
    linesize = p->outpicref->linesize[0];
    memset(p->outpicref->data[0],   0, outlink->h * linesize);
    memset(p->outpicref->data[1], 128, outlink->h * linesize);
    memset(p->outpicref->data[2], 128, outlink->h * linesize);

    p->hw = p->w / 2;
    p->hh = p->h / 2;
    switch (p->mode) {
    case BALANCE:
    case CORRELATION:
        if (p->w > p->h) {
            int i;

            for (i = 0; i < p->h; i++) {
                p->outpicref->data[2][i * linesize + p->hw - 1] = 225;
                p->outpicref->data[2][i * linesize + p->hw    ] = 255;
                p->outpicref->data[2][i * linesize + p->hw + 1] = 225;
            }
        } else {
            memset(&p->outpicref->data[2][(p->hh - 1) * linesize], 225, p->w);
            memset(&p->outpicref->data[2][ p->hh      * linesize], 255, p->w);
            memset(&p->outpicref->data[2][(p->hh + 1) * linesize], 225, p->w);
        }
        break;
    }

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *insamples)
{
    AVFilterContext *ctx = inlink->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    PhaseScopeContext *p = ctx->priv;
    int ret;

    p->outpicref->pts = insamples->pts;

    p->filter(p, insamples);

    av_frame_free(&insamples);

    ret = ff_filter_frame(outlink, av_frame_clone(p->outpicref));

    return ret;
}

static const AVFilterPad phasescope_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_AUDIO,
        .config_props = config_input,
        .filter_frame = filter_frame,
    },
    { NULL }
};

static const AVFilterPad phasescope_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = config_output,
    },
    { NULL }
};

static const char *const shorthand[] = { "mode", "contrast", "dissolve", "rate", "size", NULL };

AVFilter avfilter_avf_phasescope = {
    .name           = "phasescope",
    .description    = NULL_IF_CONFIG_SMALL("Audio phase scope."),
    .init           = init,
    .uninit         = uninit,
    .query_formats  = query_formats,
    .priv_size      = sizeof(PhaseScopeContext),
    .inputs         = phasescope_inputs,
    .outputs        = phasescope_outputs,
    .priv_class     = &phasescope_class,
    .shorthand      = shorthand,
};
