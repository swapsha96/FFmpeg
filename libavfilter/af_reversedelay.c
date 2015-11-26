/*
 * Copyright (c) 2001-2010 Krzysztof Foltman, Markus Schmidt, Thor Harald Johansen and others
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

#include "libavutil/opt.h"
#include "libavutil/samplefmt.h"
#include "avfilter.h"
#include "audio.h"
#include "internal.h"

typedef struct Inertia {
    double old_value;
    double value;
    unsigned count;
    int ramp_len;
    double mul, delta;
} Inertia;

typedef struct OverlapWindow {
    double val;
    double step;
    double acc;
    unsigned active_samples;
    unsigned full_samples;
    unsigned counter;
} OverlapWindow;

typedef struct ReverseDelayContext {
    const AVClass *class;
    double bpm;
    int divide;
    int time_l, time_r;
    double feedback;
    double amount;
    double width;
    double window;

    int counters[2];
    double feedback_buf[2];
    int deltime[2];
    Inertia fb_val, dry, iwidth;
    OverlapWindow ow[2];

    AVFrame *delay_frame;
} ReverseDelayContext;

#define OFFSET(x) offsetof(ReverseDelayContext, x)
#define A AV_OPT_FLAG_AUDIO_PARAM|AV_OPT_FLAG_FILTERING_PARAM

static const AVOption reversedelay_options[] = {
    { "bpm",      "set bpm",      OFFSET(bpm),      AV_OPT_TYPE_DOUBLE, {.dbl=120},  1, 300, A },
    { "divide",   "set divide",   OFFSET(divide),   AV_OPT_TYPE_INT,    {.i64=4},    1,  16, A },
    { "time_l",   "set time_l",   OFFSET(time_l),   AV_OPT_TYPE_INT,    {.i64=5},    1,  16, A },
    { "time_r",   "set time_r",   OFFSET(time_r),   AV_OPT_TYPE_INT,    {.i64=5},    1,  16, A },
    { "feedback", "set feedback", OFFSET(feedback), AV_OPT_TYPE_DOUBLE, {.dbl=.5},   0,   1, A },
    { "amount",   "set dry/wet",  OFFSET(amount),   AV_OPT_TYPE_DOUBLE, {.dbl=0},   -1,   1, A },
    { "width",    "set width",    OFFSET(width),    AV_OPT_TYPE_DOUBLE, {.dbl=0},    0,   1, A },
    { "window",   "set window",   OFFSET(window),   AV_OPT_TYPE_DOUBLE, {.dbl=.5},   0,   1, A },
    { NULL }
};

AVFILTER_DEFINE_CLASS(reversedelay);

static int query_formats(AVFilterContext *ctx)
{
    AVFilterChannelLayouts *layout = NULL;
    AVFilterFormats *formats;
    static const enum AVSampleFormat sample_fmts[] = {
        AV_SAMPLE_FMT_DBL,
        AV_SAMPLE_FMT_NONE
    };
    int ret;

    ret = ff_add_channel_layout(&layout, AV_CH_LAYOUT_STEREO);
    if (ret < 0)
        return ret;
    ret = ff_set_common_channel_layouts(ctx, layout);
    if (ret < 0)
        return ret;

    formats = ff_make_format_list(sample_fmts);
    if (!formats)
        return AVERROR(ENOMEM);
    ret = ff_set_common_formats(ctx, formats);
    if (ret < 0)
        return ret;

    formats = ff_all_samplerates();
    if (!formats)
        return AVERROR(ENOMEM);
    return ff_set_common_samplerates(ctx, formats);
}

static void ow_set_full(OverlapWindow *ow, double min_val, unsigned full_samples, unsigned active_samples)
{
    if (active_samples >= full_samples)
        return;

    ow->acc = min_val;
    ow->val = min_val;
    ow->full_samples = full_samples;
    ow->active_samples = active_samples;
    ow->counter = 0;
    ow->step = (1 - min_val) / (active_samples / 2);
}

static double ow_get(OverlapWindow *ow)
{
    if (ow->counter < ow->active_samples / 2) {
        ow->acc += ow->step;
        ow->counter++;
        return ow->acc;
    } else if (ow->counter >= ow->active_samples / 2 &&
               ow->counter <= ow->full_samples - ow->active_samples / 2) {
        ow->counter++;
        return 1;
    } else if (ow->counter > ow->full_samples - ow->active_samples / 2 &&
               ow->counter < ow->full_samples) {
        ow->acc -= ow->step;
        ow->counter++;
        return ow->acc;
    } else if (ow->counter >= ow->full_samples) {
        double ret_val = ow->acc;

        ow->acc = ow->val;
        ow->counter = 0;
        return ret_val;
    }
    return 1;
}

static void inertia_set_sample_rate(Inertia *in, int sample_rate)
{
    in->ramp_len = sample_rate / 100;
    in->mul = (double)(1.0 / in->ramp_len);
    in->delta = 0.0;
}

static void set_inertia(Inertia *in, double source)
{
    if (source != in->old_value) {
        in->delta = in->mul * (source - in->value);
        in->count = in->ramp_len;
        in->old_value = source;
    }
}

static double get_inertia(Inertia *in)
{
    if (!in->count)
        return in->old_value;
    in->value += in->delta;
    in->count--;
    if (!in->count)
        in->value = in->old_value;
    return in->value;
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    ReverseDelayContext *s = ctx->priv;
    double unit;

    if (inlink->sample_rate < 100)
        return AVERROR(EINVAL);

    inertia_set_sample_rate(&s->fb_val, inlink->sample_rate);
    inertia_set_sample_rate(&s->dry, inlink->sample_rate);
    inertia_set_sample_rate(&s->iwidth, inlink->sample_rate);

    unit = 60.0 * inlink->sample_rate / (s->bpm * s->divide);

    s->deltime[0] = rint(unit * s->time_l);
    s->deltime[1] = rint(unit * s->time_r);

    ow_set_full(&s->ow[0], 0, s->deltime[0] / 2, (s->window + 0.005) * (s->deltime[0] / 2));
    ow_set_full(&s->ow[1], 0, s->deltime[1] / 2, (s->window + 0.005) * (s->deltime[1] / 2));

    set_inertia(&s->iwidth, s->width);
    set_inertia(&s->fb_val, s->feedback);
    set_inertia(&s->dry,    s->amount);

    s->counters[0] = s->counters[1] = 0;

    s->delay_frame = av_frame_alloc();
    if (!s->delay_frame)
        return AVERROR(ENOMEM);

    s->delay_frame->format         = AV_SAMPLE_FMT_DBLP;
    s->delay_frame->nb_samples     = 60 * inlink->sample_rate * 16;
    s->delay_frame->channel_layout = inlink->channel_layout;

    return av_frame_get_buffer(s->delay_frame, 32);
}

static double reverse_delay_line(double in, double *buf,
                                 int *counter, int length)
{
    double out = 0;

    if (*counter < length - 1) {
        unsigned read_counter = length - 1 - (*counter);
        out = buf[read_counter];
    }

    *(buf + (*counter)) = in;
    (*counter)++;
    if ((*counter) > length - 1)
        (*counter) = 0;

    return out;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    const double *src = (const double *)in->data[0];
    AVFilterContext *ctx = inlink->dst;
    ReverseDelayContext *s = ctx->priv;
    double *dst, *buffer[2];
    AVFrame *out;
    int n;

    out = ff_get_audio_buffer(inlink, in->nb_samples);
    if (!out) {
        av_frame_free(&in);
        return AVERROR(ENOMEM);
    }
    av_frame_copy_props(out, in);

    dst    = (double *)out->data[0];
    buffer[0] = (double *)s->delay_frame->extended_data[0];
    buffer[1] = (double *)s->delay_frame->extended_data[1];
    for (n = 0; n < in->nb_samples; n++) {
        const double feedback_val = get_inertia(&s->fb_val);
        const double st_width_val = get_inertia(&s->iwidth);
        double inL = src[0] + st_width_val * src[1];
        double inR = src[1] * (1 - st_width_val);
        double outL, outR;

        inL = inL + s->feedback_buf[0] * feedback_val * (1 - st_width_val) + s->feedback_buf[1] * st_width_val * feedback_val;
        inR = inR + s->feedback_buf[1] * feedback_val * (1 - st_width_val) + s->feedback_buf[0] * st_width_val * feedback_val;
        outL = reverse_delay_line(inL, buffer[0], &s->counters[0], s->deltime[0]);
        outR = reverse_delay_line(inR, buffer[1], &s->counters[1], s->deltime[1]);
        s->feedback_buf[0] = outL;
        s->feedback_buf[1] = outR;

        outL *= ow_get(&s->ow[0]);
        outR *= ow_get(&s->ow[1]);

        dst[0] = outL * (1 + get_inertia(&s->dry)) + inL * (1 - get_inertia(&s->dry));
        dst[1] = outR * (1 + get_inertia(&s->dry)) + inR * (1 - get_inertia(&s->dry));

        src += 2;
        dst += 2;
    }

    av_frame_free(&in);
    return ff_filter_frame(ctx->outputs[0], out);
}

static av_cold void uninit(AVFilterContext *ctx)
{
    ReverseDelayContext *s = ctx->priv;

    av_frame_free(&s->delay_frame);
}

static const AVFilterPad reversedelay_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_AUDIO,
        .config_props = config_input,
        .filter_frame = filter_frame,
    },
    { NULL }
};

static const AVFilterPad reversedelay_outputs[] = {
    {
        .name = "default",
        .type = AVMEDIA_TYPE_AUDIO,
    },
    { NULL }
};

AVFilter ff_af_reversedelay = {
    .name          = "reversedelay",
    .description   = NULL_IF_CONFIG_SMALL("Audio reverse delay."),
    .query_formats = query_formats,
    .priv_size     = sizeof(ReverseDelayContext),
    .priv_class    = &reversedelay_class,
    .uninit        = uninit,
    .inputs        = reversedelay_inputs,
    .outputs       = reversedelay_outputs,
};
