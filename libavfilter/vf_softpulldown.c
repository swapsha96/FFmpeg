/*
 * Copyright (c) 2003 Tobias Diedrich
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
#include "avfilter.h"
#include "internal.h"

typedef struct SoftpulldownContext {
    const AVClass *class;
    int state;
    int nb_planes;
    int linesize[4];
    int planeheight[4];
    AVFrame *frame;
} SoftpulldownContext;

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pixel_fmts_eq[] = {
        AV_PIX_FMT_GRAY8,
        AV_PIX_FMT_YUV410P,
        AV_PIX_FMT_YUV411P,
        AV_PIX_FMT_YUV420P,
        AV_PIX_FMT_YUV422P,
        AV_PIX_FMT_YUV444P,
        AV_PIX_FMT_NONE
    };

    ff_set_common_formats(ctx, ff_make_format_list(pixel_fmts_eq));

    return 0;
}

static int config_input(AVFilterLink *inlink)
{
    SoftpulldownContext *s = inlink->dst->priv;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);
    int ret;

    if ((ret = av_image_fill_linesizes(s->linesize, inlink->format, inlink->w)) < 0)
        return ret;

    s->planeheight[1] = s->planeheight[2] = FF_CEIL_RSHIFT(inlink->h, desc->log2_chroma_h);
    s->planeheight[0] = s->planeheight[3] = inlink->h;

    s->nb_planes = av_pix_fmt_count_planes(inlink->format);

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext *ctx = inlink->dst;
    AVFilterLink *outlink = inlink->dst->outputs[0];
    SoftpulldownContext *s = ctx->priv;
    AVFrame *out;
    int ret, i;
    int state = s->state;

    if (!s->frame) {
        s->frame = ff_get_video_buffer(outlink, outlink->w, outlink->h);
        if (!s->frame)
            return AVERROR(ENOMEM);
    }

    out = s->frame;
    av_frame_copy_props(out, in);

    if ((state == 0 && !in->top_field_first) ||
        (state == 1 &&  in->top_field_first)) {
        av_log(ctx, AV_LOG_WARNING, "Unexpected field flags: "
                                    "state=%d top_field_first=%d repeat_first_field=%d\n",
                                    state, in->top_field_first, in->repeat_pict);
        state ^= 1;
    }

    if (state == 0) {
        AVFrame *new;

        new = av_frame_clone(in);
        if (!new)
            return AVERROR(ENOMEM);

        ret = ff_filter_frame(outlink, new);

        if (in->repeat_pict) {
            for (i = 0; i < s->nb_planes; i++) {
                av_image_copy_plane(out->data[i], out->linesize[i] * 2,
                                    in->data[i], in->linesize[i] * 2,
                                    s->linesize[i], s->planeheight[i] / 2);
            }
            state = 1;
        }
    } else {
        AVFrame *new;

        new = av_frame_clone(in);
        if (!new)
            return AVERROR(ENOMEM);

        for (i = 0; i < s->nb_planes; i++) {
            av_image_copy_plane(out->data[i] + out->linesize[i], out->linesize[i] * 2,
                                in->data[i] + in->linesize[i], in->linesize[i] * 2,
                                s->linesize[i], s->planeheight[i] / 2);
        }

        ret = ff_filter_frame(outlink, av_frame_clone(out));

        if (in->repeat_pict) {
            AVFrame *new2;

            new2 = av_frame_clone(in);
            if (!new2)
                return AVERROR(ENOMEM);

            ret = ff_filter_frame(outlink, new2);
            state = 0;
        } else {
            for (i = 0; i < s->nb_planes; i++) {
                av_image_copy_plane(out->data[i], out->linesize[i] * 2,
                                    in->data[i], in->linesize[i] * 2,
                                    s->linesize[i], s->planeheight[i] / 2);
            }
        }
    }

    s->state = state;

    av_frame_free(&in);
    return 0;
}

static const AVFilterPad softpulldown_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = filter_frame,
        .config_props = config_input,
    },
    { NULL }
};

static const AVFilterPad softpulldown_outputs[] = {
    {
        .name = "default",
        .type = AVMEDIA_TYPE_VIDEO,
    },
    { NULL }
};

AVFilter ff_vf_softpulldown = {
    .name          = "softpulldown",
    .description   = NULL_IF_CONFIG_SMALL("Mpeg2 soft 3:2 pulldown"),
    .priv_size     = sizeof(SoftpulldownContext),
    .inputs        = softpulldown_inputs,
    .outputs       = softpulldown_outputs,
    .query_formats = query_formats,
};
