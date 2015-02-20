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

#include "libavutil/channel_layout.h"
#include "libavutil/opt.h"
#include "libavutil/parseutils.h"
#include "avfilter.h"
#include "formats.h"
#include "audio.h"
#include "video.h"
#include "internal.h"

typedef struct ShowVolumeContext {
    const AVClass *class;
    AVFrame *outpicref;
    int w, h;
    int c, f;
    AVRational frame_rate;
} ShowVolumeContext;

#define OFFSET(x) offsetof(ShowVolumeContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM

static const AVOption showvolume_options[] = {
    { "rate", "set video rate",  OFFSET(frame_rate), AV_OPT_TYPE_VIDEO_RATE, {.str="25"}, 0, 0, FLAGS },
    { "r",    "set video rate",  OFFSET(frame_rate), AV_OPT_TYPE_VIDEO_RATE, {.str="25"}, 0, 0, FLAGS },
    { "w", "set channel width",  OFFSET(w), AV_OPT_TYPE_INT, {.i64=400}, 40, 1080, FLAGS },
    { "h", "set channel height", OFFSET(h), AV_OPT_TYPE_INT, {.i64=20}, 1, 100, FLAGS },
    { "c", "set contrast",       OFFSET(c), AV_OPT_TYPE_INT, {.i64=20}, 1, 255, FLAGS },
    { "f", "set fade",           OFFSET(f), AV_OPT_TYPE_INT, {.i64=20}, 1, 255, FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(showvolume);

static int query_formats(AVFilterContext *ctx)
{
    AVFilterFormats *formats = NULL;
    AVFilterChannelLayouts *layouts = NULL;
    AVFilterLink *inlink = ctx->inputs[0];
    AVFilterLink *outlink = ctx->outputs[0];
    static const enum AVSampleFormat sample_fmts[] = { AV_SAMPLE_FMT_FLTP, AV_SAMPLE_FMT_NONE };
    static const enum AVPixelFormat pix_fmts[] = { AV_PIX_FMT_RGBA, AV_PIX_FMT_NONE };

    formats = ff_make_format_list(sample_fmts);
    if (!formats)
        return AVERROR(ENOMEM);
    ff_formats_ref(formats, &inlink->out_formats);

    layouts = ff_all_channel_layouts();
    if (!layouts)
        return AVERROR(ENOMEM);
    ff_channel_layouts_ref(layouts, &inlink->out_channel_layouts);

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
    ShowVolumeContext *s = ctx->priv;
    int nb_samples;

    nb_samples = FFMAX(1024, ((double)inlink->sample_rate / av_q2d(s->frame_rate)) + 0.5);
    inlink->partial_buf_size =
    inlink->min_samples =
    inlink->max_samples = nb_samples;

    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    ShowVolumeContext *s = outlink->src->priv;
    AVFilterLink *inlink = outlink->src->inputs[0];

    outlink->w = s->w;
    outlink->h = s->h * inlink->channels;
    outlink->sample_aspect_ratio = (AVRational){1,1};
    outlink->frame_rate = s->frame_rate;

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *insamples)
{
    AVFilterContext *ctx = inlink->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    ShowVolumeContext *s = ctx->priv;
    int c, i, j, k;

    if (!s->outpicref || s->outpicref->width  != outlink->w ||
                         s->outpicref->height != outlink->h) {
        av_frame_free(&s->outpicref);
        s->outpicref = ff_get_video_buffer(outlink, outlink->w, outlink->h);
        if (!s->outpicref) {
            av_frame_free(&insamples);
            return AVERROR(ENOMEM);
        }

        for (i = 0; i < outlink->h; i++)
            memset(s->outpicref->data[0] + i * s->outpicref->linesize[0], 0, outlink->w * 4);
    }
    s->outpicref->pts = insamples->pts;

    for (j = 0; j < s->h * inlink->channels; j++) {
        uint8_t *dst = s->outpicref->data[0] + j * s->outpicref->linesize[0];
        for (k = 0; k < s->w; k++) {
            dst[k * 4 + 1] = FFMAX(dst[k * 4 + 1] - s->f, 0);
        }
    }

    for (c = 0; c < inlink->channels; c++) {
        float *src = (float *)insamples->extended_data[c];

        for (i = 0; i < insamples->nb_samples; i++) {
            for (j = 0; j < s->h; j++) {
                uint8_t *dst = s->outpicref->data[0] + (c * s->h + j) * s->outpicref->linesize[0];

                for (k = 0; k < s->w * av_clipf(src[i], 0, 1); k++) {
                    dst[k * 4 + 1] = FFMIN(dst[k * 4 + 1] + s->c, 255);
                }
            }
        }
    }

    av_frame_free(&insamples);

    return ff_filter_frame(outlink, av_frame_clone(s->outpicref));
}

static av_cold void uninit(AVFilterContext *ctx)
{
    ShowVolumeContext *s = ctx->priv;

    av_frame_free(&s->outpicref);
}

static const AVFilterPad showvolume_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_AUDIO,
        .config_props = config_input,
        .filter_frame = filter_frame,
    },
    { NULL }
};

static const AVFilterPad showvolume_outputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = config_output,
    },
    { NULL }
};

AVFilter ff_avf_showvolume = {
    .name          = "showvolume",
    .description   = NULL_IF_CONFIG_SMALL("Convert input audio volume to video output."),
    .uninit        = uninit,
    .query_formats = query_formats,
    .priv_size     = sizeof(ShowVolumeContext),
    .inputs        = showvolume_inputs,
    .outputs       = showvolume_outputs,
    .priv_class    = &showvolume_class,
};
