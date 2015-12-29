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
#include "libavutil/parseutils.h"
#include "avfilter.h"
#include "formats.h"
#include "audio.h"
#include "video.h"
#include "internal.h"

enum DisplayScale   { LINEAR, SQRT, CBRT, LOG, RLOG, NB_SCALES };
enum AmplitudeScale { ALINEAR, ALOG, NB_ASCALES };
enum SlideMode      { REPLACE, SCROLL, NB_SLIDES };
enum DisplayMode    { SINGLE, SEPARATE, NB_DMODES };
enum HistogramMode  { ACCUMULATE, CURRENT, NB_HMODES };

typedef struct AudioHistogramContext {
    const AVClass *class;
    AVFrame *out;
    int w, h;
    AVRational frame_rate;
    uint64_t *histogram;
    uint64_t *achistogram;
    int ascale;
    int scale;
    float phisto;
    int histogram_h;
    int apos;
    int ypos;
    int slide;
    int hmode;
    int dmode;
    int dchannels;
    float *combine_buffer;
} AudioHistogramContext;

#define OFFSET(x) offsetof(AudioHistogramContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM

static const AVOption ahistogram_options[] = {
    { "dmode", "set method to display channels", OFFSET(dmode), AV_OPT_TYPE_INT, {.i64=SINGLE}, 0, NB_DMODES-1, FLAGS, "dmode" },
        { "single", "all channels use single histogram", 0, AV_OPT_TYPE_CONST, {.i64=SINGLE},   0, 0, FLAGS, "dmode" },
        { "separate", "each channel have own histogram", 0, AV_OPT_TYPE_CONST, {.i64=SEPARATE}, 0, 0, FLAGS, "dmode" },
    { "hmode", "set histogram mode", OFFSET(hmode), AV_OPT_TYPE_INT, {.i64=CURRENT}, 0, NB_HMODES-1, FLAGS, "hmode" },
        { "accumulate", "accumulate all frames",  0, AV_OPT_TYPE_CONST, {.i64=ACCUMULATE},   0, 0, FLAGS, "hmode" },
        { "current",    "use current frame only", 0, AV_OPT_TYPE_CONST, {.i64=CURRENT},      0, 0, FLAGS, "hmode" },
    { "rate", "set video rate", OFFSET(frame_rate), AV_OPT_TYPE_VIDEO_RATE, {.str="25"}, 0, 0, FLAGS },
    { "r",    "set video rate", OFFSET(frame_rate), AV_OPT_TYPE_VIDEO_RATE, {.str="25"}, 0, 0, FLAGS },
    { "size", "set video size", OFFSET(w), AV_OPT_TYPE_IMAGE_SIZE, {.str="hd720"}, 0, 0, FLAGS },
    { "s",    "set video size", OFFSET(w), AV_OPT_TYPE_IMAGE_SIZE, {.str="hd720"}, 0, 0, FLAGS },
    { "scale", "set display scale", OFFSET(scale), AV_OPT_TYPE_INT, {.i64=LOG}, LINEAR, NB_SCALES-1, FLAGS, "scale" },
        { "log",  "logarithmic",         0, AV_OPT_TYPE_CONST, {.i64=LOG},    0, 0, FLAGS, "scale" },
        { "sqrt", "square root",         0, AV_OPT_TYPE_CONST, {.i64=SQRT},   0, 0, FLAGS, "scale" },
        { "cbrt", "cubic root",          0, AV_OPT_TYPE_CONST, {.i64=CBRT},   0, 0, FLAGS, "scale" },
        { "lin",  "linear",              0, AV_OPT_TYPE_CONST, {.i64=LINEAR}, 0, 0, FLAGS, "scale" },
        { "rlog", "reverse logarithmic", 0, AV_OPT_TYPE_CONST, {.i64=RLOG},   0, 0, FLAGS, "scale" },
    { "ascale", "set amplitude scale", OFFSET(ascale), AV_OPT_TYPE_INT, {.i64=ALOG}, LINEAR, NB_ASCALES-1, FLAGS, "ascale" },
        { "log",  "logarithmic", 0, AV_OPT_TYPE_CONST, {.i64=ALOG},    0, 0, FLAGS, "ascale" },
        { "lin",  "linear",      0, AV_OPT_TYPE_CONST, {.i64=ALINEAR}, 0, 0, FLAGS, "ascale" },
    { "pheight", "set histogram percentage of window height", OFFSET(phisto), AV_OPT_TYPE_FLOAT, {.dbl=0.10}, 0, 1, FLAGS },
    { "slide", "set sonogram sliding", OFFSET(slide), AV_OPT_TYPE_INT, {.i64=REPLACE}, 0, NB_SLIDES-1, FLAGS, "slide" },
        { "replace", "replace old rows with new", 0, AV_OPT_TYPE_CONST, {.i64=REPLACE},    0, 0, FLAGS, "slide" },
        { "scroll",  "scroll from top to bottom", 0, AV_OPT_TYPE_CONST, {.i64=SCROLL}, 0, 0, FLAGS, "slide" },
    { NULL }
};

AVFILTER_DEFINE_CLASS(ahistogram);

static int query_formats(AVFilterContext *ctx)
{
    AVFilterFormats *formats = NULL;
    AVFilterChannelLayouts *layouts = NULL;
    AVFilterLink *inlink = ctx->inputs[0];
    AVFilterLink *outlink = ctx->outputs[0];
    static const enum AVSampleFormat sample_fmts[] = { AV_SAMPLE_FMT_FLTP, AV_SAMPLE_FMT_NONE };
    static const enum AVPixelFormat pix_fmts[] = { AV_PIX_FMT_YUVA444P, AV_PIX_FMT_NONE };
    int ret = AVERROR(EINVAL);

    formats = ff_make_format_list(sample_fmts);
    if ((ret = ff_formats_ref         (formats, &inlink->out_formats        )) < 0 ||
        (layouts = ff_all_channel_counts()) == NULL ||
        (ret = ff_channel_layouts_ref (layouts, &inlink->out_channel_layouts)) < 0)
        return ret;

    formats = ff_all_samplerates();
    if ((ret = ff_formats_ref(formats, &inlink->out_samplerates)) < 0)
        return ret;

    formats = ff_make_format_list(pix_fmts);
    if ((ret = ff_formats_ref(formats, &outlink->in_formats)) < 0)
        return ret;

    return 0;
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    AudioHistogramContext *s = ctx->priv;
    int nb_samples;

    nb_samples = FFMAX(1024, ((double)inlink->sample_rate / av_q2d(s->frame_rate)) + 0.5);
    inlink->partial_buf_size =
    inlink->min_samples =
    inlink->max_samples = nb_samples;

    s->dchannels = s->dmode == SINGLE ? 1 : inlink->channels;
    s->histogram = av_malloc_array(s->w, s->dchannels * sizeof(*s->histogram));
    if (!s->histogram)
        return AVERROR(ENOMEM);

    s->achistogram = av_calloc(s->w, s->dchannels * sizeof(*s->achistogram));
    if (!s->achistogram)
        return AVERROR(ENOMEM);

    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    AudioHistogramContext *s = outlink->src->priv;

    outlink->w = s->w;
    outlink->h = s->h;
    outlink->sample_aspect_ratio = (AVRational){1,1};
    outlink->frame_rate = s->frame_rate;

    s->histogram_h = s->h * s->phisto;
    s->ypos = s->h * s->phisto;

    if (s->dmode == SEPARATE) {
        s->combine_buffer = av_malloc_array(outlink->w * 3, sizeof(*s->combine_buffer));
        if (!s->combine_buffer)
            return AVERROR(ENOMEM);
    }

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext *ctx = inlink->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    AudioHistogramContext *s = ctx->priv;
    const int H = s->histogram_h;
    const int w = s->w;
    int c, y, n, p, bin;
    uint64_t max = 0, acmax = 0;

    if (!s->out || s->out->width  != outlink->w ||
                   s->out->height != outlink->h) {
        av_frame_free(&s->out);
        s->out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
        if (!s->out) {
            av_frame_free(&in);
            return AVERROR(ENOMEM);
        }
        for (n = H; n < s->h; n++) {
            memset(s->out->data[0] + n * s->out->linesize[0], 0, w);
            memset(s->out->data[1] + n * s->out->linesize[0], 127, w);
            memset(s->out->data[2] + n * s->out->linesize[0], 127, w);
            memset(s->out->data[3] + n * s->out->linesize[0], 0, w);
        }
    }

    memset(s->histogram, 0, sizeof(*s->histogram) * w * s->dchannels);

    if (s->dmode == SEPARATE) {
        for (y = 0; y < w; y++) {
            s->combine_buffer[3 * y    ] = 0;
            s->combine_buffer[3 * y + 1] = 127.5;
            s->combine_buffer[3 * y + 2] = 127.5;
        }
    }

    for (n = 0; n < H; n++) {
        memset(s->out->data[0] + n * s->out->linesize[0], 0, w);
        memset(s->out->data[1] + n * s->out->linesize[0], 127, w);
        memset(s->out->data[2] + n * s->out->linesize[0], 127, w);
        memset(s->out->data[3] + n * s->out->linesize[0], 0, w);
    }
    s->out->pts = in->pts;

    switch (s->ascale) {
    case ALINEAR:
        for (c = 0; c < inlink->channels; c++) {
            const float *src = (const float *)in->extended_data[c];
            uint64_t *histogram = &s->histogram[(s->dmode == SINGLE ? 0: c) * w];
            uint64_t *achistogram = &s->achistogram[(s->dmode == SINGLE ? 0: c) * w];

            for (n = 0; n < in->nb_samples; n++) {
                bin = lrint(av_clipf(fabsf(src[n]), 0, 1) * (w - 1));

                histogram[bin]++;
                achistogram[bin]++;
            }
        }
        break;
    case ALOG:
        for (c = 0; c < inlink->channels; c++) {
            const float *src = (const float *)in->extended_data[c];
            uint64_t *histogram = &s->histogram[(s->dmode == SINGLE ? 0: c) * w];
            uint64_t *achistogram = &s->achistogram[(s->dmode == SINGLE ? 0: c) * w];

            for (n = 0; n < in->nb_samples; n++) {
                bin = lrint(av_clipf(1 + log10(fabsf(src[n])) / 6, 0, 1) * (w - 1));

                histogram[bin]++;
                achistogram[bin]++;
            }
        }
        break;
    }

    av_frame_free(&in);

    for (n = 0; n < w * s->dchannels; n++) {
        max   = FFMAX(s->histogram[n], max);
        acmax = FFMAX(s->achistogram[n], acmax);
    }

    for (c = 0; c < s->dchannels; c++) {
        uint64_t *histogram   = &s->histogram[c * w];
        uint64_t *achistogram = &s->achistogram[c * w];
        float yf, uf, vf;

        if (s->dmode == SEPARATE) {
            yf = 256.0f / s->dchannels;
            uf = yf * M_PI;
            vf = yf * M_PI;
            uf *= 0.5 * sin((2 * M_PI * c) / s->dchannels);
            vf *= 0.5 * cos((2 * M_PI * c) / s->dchannels);
        }

        for (n = 0; n < w; n++) {
            double a, aa;
            int h;

            switch (s->scale) {
            case LINEAR:
                a  = histogram[n]   / (double)max;
                aa = achistogram[n] / (double)acmax;
                break;
            case SQRT:
                a  = sqrt(histogram[n])   / sqrt(max);
                aa = sqrt(achistogram[n]) / sqrt(acmax);
                break;
            case CBRT:
                a  = cbrt(histogram[n])   / cbrt(max);
                aa = cbrt(achistogram[n]) / cbrt(acmax);
                break;
            case LOG:
                a  = log2(histogram[n]   + 1) / log2(max   + 1);
                aa = log2(achistogram[n] + 1) / log2(acmax + 1);
                break;
            case RLOG:
                a  = 1. - log2(histogram[n]   + 1) / log2(max   + 1);
                aa = 1. - log2(achistogram[n] + 1) / log2(acmax + 1);
                if (a == 1.)
                    a = 0;
                if (aa == 1.)
                    aa = 0;
                break;
            }

            h = (s->hmode == ACCUMULATE ? aa: a) * (H-1);

            if (s->dmode == SINGLE) {

                for (y = H - h; y < H; y++) {
                    s->out->data[0][y * s->out->linesize[0] + n] = 255;
                    s->out->data[3][y * s->out->linesize[0] + n] = 255;
                }

                if (s->h - H > 0) {
                    h = a * 255;

                    s->out->data[0][s->ypos * s->out->linesize[0] + n] = h;
                    s->out->data[1][s->ypos * s->out->linesize[1] + n] = 127;
                    s->out->data[2][s->ypos * s->out->linesize[2] + n] = 127;
                    s->out->data[3][s->ypos * s->out->linesize[3] + n] = 255;
                }
            } else if (s->dmode == SEPARATE) {
                float *out = &s->combine_buffer[3 * n];
                int old;

                old = s->out->data[0][(H - h) * s->out->linesize[0] + n];
                for (y = H - h; y < H; y++) {
                    if (s->out->data[0][y * s->out->linesize[0] + n] != old)
                        break;
                    old = s->out->data[0][y * s->out->linesize[0] + n];
                    s->out->data[0][y * s->out->linesize[0] + n] = yf;
                    s->out->data[1][y * s->out->linesize[1] + n] = 128+uf;
                    s->out->data[2][y * s->out->linesize[2] + n] = 128+vf;
                    s->out->data[3][y * s->out->linesize[3] + n] = 255;
                }

                out[0] += a * yf;
                out[1] += a * uf;
                out[2] += a * vf;
            }
        }
    }

    if (s->h - H > 0) {
        if (s->dmode == SEPARATE) {
            for (n = 0; n < w; n++) {
                float *cb = &s->combine_buffer[3 * n];

                s->out->data[0][s->ypos * s->out->linesize[0] + n] = cb[0];
                s->out->data[1][s->ypos * s->out->linesize[1] + n] = cb[1];
                s->out->data[2][s->ypos * s->out->linesize[2] + n] = cb[2];
                s->out->data[3][s->ypos * s->out->linesize[3] + n] = 255;
            }
        }

        if (s->slide == SCROLL) {
            for (p = 0; p < 4; p++) {
                for (y = s->h; y >= H + 1; y--) {
                    memmove(s->out->data[p] + (y  ) * s->out->linesize[p],
                            s->out->data[p] + (y-1) * s->out->linesize[p], w);
                }
            }
        }

        s->ypos++;
        if (s->slide == SCROLL || s->ypos >= s->h)
            s->ypos = H;
    }

    return ff_filter_frame(outlink, av_frame_clone(s->out));
}

static av_cold void uninit(AVFilterContext *ctx)
{
    AudioHistogramContext *s = ctx->priv;

    av_frame_free(&s->out);
    av_freep(&s->histogram);
    av_freep(&s->achistogram);
    av_freep(&s->combine_buffer);
}

static const AVFilterPad audiovectorscope_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_AUDIO,
        .config_props = config_input,
        .filter_frame = filter_frame,
    },
    { NULL }
};

static const AVFilterPad audiovectorscope_outputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = config_output,
    },
    { NULL }
};

AVFilter ff_avf_ahistogram = {
    .name          = "ahistogram",
    .description   = NULL_IF_CONFIG_SMALL("Convert input audio to histogram video output."),
    .uninit        = uninit,
    .query_formats = query_formats,
    .priv_size     = sizeof(AudioHistogramContext),
    .inputs        = audiovectorscope_inputs,
    .outputs       = audiovectorscope_outputs,
    .priv_class    = &ahistogram_class,
};
