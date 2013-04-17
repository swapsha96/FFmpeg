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
#include "internal.h"

typedef struct {
    const AVClass *class;
    int first_field;
    int64_t frame_count;
    double ts_unit;
    int nb_planes;
    int planeheight[4];
    int linesize[4];

    AVFrame *prev;
} JoinFieldsContext;

#define OFFSET(x) offsetof(JoinFieldsContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM

static const AVOption joinfields_options[] = {
    {"first_field", "set first field", OFFSET(first_field), AV_OPT_TYPE_INT,   {.i64=0}, 0, 1, FLAGS, "field"},
        {"top",    "set top field first",                0, AV_OPT_TYPE_CONST, {.i64=0}, 0, 0, FLAGS, "field"},
        {"t",      "set top field first",                0, AV_OPT_TYPE_CONST, {.i64=0}, 0, 0, FLAGS, "field"},
        {"bottom", "set bottom field first",             0, AV_OPT_TYPE_CONST, {.i64=1}, 0, 0, FLAGS, "field"},
        {"b",      "set bottom field first",             0, AV_OPT_TYPE_CONST, {.i64=1}, 0, 0, FLAGS, "field"},
    {NULL}
};

AVFILTER_DEFINE_CLASS(joinfields);

static int config_props_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    JoinFieldsContext *jf = ctx->priv;
    AVFilterLink *inlink = ctx->inputs[0];
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);
    int ret;

    outlink->time_base.num = inlink->time_base.num * 2;
    outlink->time_base.den = inlink->time_base.den;
    outlink->frame_rate.num = inlink->frame_rate.num;
    outlink->frame_rate.den = inlink->frame_rate.den * 2;
    outlink->w = inlink->w;
    outlink->h = inlink->h * 2;
    outlink->flags |= FF_LINK_FLAG_REQUEST_LOOP;
    jf->ts_unit = av_q2d(av_inv_q(av_mul_q(outlink->frame_rate, outlink->time_base)));

    if ((ret = av_image_fill_linesizes(jf->linesize, inlink->format, inlink->w)) < 0)
        return ret;

    jf->planeheight[1] = jf->planeheight[2] = inlink->h >> desc->log2_chroma_h;
    jf->planeheight[0] = jf->planeheight[3] = inlink->h;

    jf->nb_planes = av_pix_fmt_count_planes(inlink->format);

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *cur)
{
    AVFilterContext *ctx = inlink->dst;
    JoinFieldsContext *jf = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    AVFrame *out;
    int i;

    if (!jf->prev) {
        jf->prev = cur;
        return 0;
    }

    out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    if (!out) {
        av_frame_free(&cur);
        av_frame_free(&jf->prev);
        return AVERROR(ENOMEM);
    }
    av_frame_copy_props(out, cur);

    for (i = 0; i < jf->nb_planes; i++) {
        av_image_copy_plane(out->data[i] + out->linesize[i] * !jf->first_field,
                            out->linesize[i] * 2,
                            cur->data[i], cur->linesize[i],
                            jf->linesize[i], jf->planeheight[i]);
        av_image_copy_plane(out->data[i] + out->linesize[i] * jf->first_field,
                            out->linesize[i] * 2,
                            jf->prev->data[i], jf->prev->linesize[i],
                            jf->linesize[i], jf->planeheight[i]);
    }

    out->pts = jf->frame_count++ * jf->ts_unit;

    av_frame_free(&cur);
    av_frame_free(&jf->prev);
    return ff_filter_frame(outlink, out);
}

static const AVFilterPad joinfields_inputs[] = {
    {
        .name             = "default",
        .type             = AVMEDIA_TYPE_VIDEO,
        .filter_frame     = filter_frame,
    },
    { NULL }
};

static const AVFilterPad joinfields_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = config_props_output,
    },
    { NULL }
};

AVFilter avfilter_vf_joinfields = {
    .name          = "joinfields",
    .description   = NULL_IF_CONFIG_SMALL("Join input video fields into frames."),
    .priv_size     = sizeof(JoinFieldsContext),
    .priv_class    = &joinfields_class,
    .inputs        = joinfields_inputs,
    .outputs       = joinfields_outputs,
};
