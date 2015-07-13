/*
 *  Copyright (C) 2012 Mark Himsley
 *
 *  get_scene_score() Copyright (c) 2011 Stefano Sabatini
 *  taken from ffmpeg/libavfilter/vf_select.c
 *
 * This filter is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This filter is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * filter for upsampling or downsampling a progressive source
 */

#define DEBUG

#include "libavutil/avassert.h"
#include "libavutil/common.h"
#include "libavutil/imgutils.h"
#include "libavutil/internal.h"
#include "libavutil/opt.h"
#include "libavutil/parseutils.h"
#include "libavutil/pixdesc.h"
#include "libavutil/pixelutils.h"

#include "avfilter.h"
#include "internal.h"
#include "video.h"

#define N_SRCE 3
typedef struct {
    const AVClass *class;
    // parameters
    AVRational dest_frame_rate;         ///< output frames per second
    int flags;                          ///< flags affecting frame rate conversion algorithm
    double scene_score;                 ///< score that denotes a scene change has happened
    int interp_start;                   ///< start of range to apply linear interpolation
    int interp_end;                     ///< end of range to apply linear interpolation

    int line_size[4];                   ///< bytes of pixel data per line for each plane
    int vsub;

    int frst, next, prev, crnt, last;
    int pending_srce_frames;            ///< how many input frames are still waiting to be processed
    int flush;                          ///< are we flushing final frames
    int pending_end_frame;              ///< flag indicating we are waiting to call end_frame()

    AVRational srce_time_base;          ///< timebase of source

    AVRational dest_time_base;          ///< timebase of destination
    int32_t dest_frame_num;
    int64_t last_dest_frame_pts;        ///< pts of the last frame output
    int average_srce_pts_dest_delta;    ///< average input pts delta converted from input rate to output rate
    int average_dest_pts_delta;         ///< calculated average output pts delta

    av_pixelutils_sad_fn sad;           ///< Sum of the absolute difference function (scene detect only)
    double prev_mafd;                   ///< previous MAFD                             (scene detect only)

    AVFrame *srce[N_SRCE];    ///< buffered source frames
    int64_t srce_pts_dest[N_SRCE];      ///< pts for source frames scaled to output timebase
    AVFrame *work;            ///< frame we are working on
} FrameRateContext;

#define OFFSET(x) offsetof(FrameRateContext, x)
#define V AV_OPT_FLAG_VIDEO_PARAM
#define F AV_OPT_FLAG_FILTERING_PARAM
#define FRAMERATE_FLAG_SCD 01

static const AVOption framerate_options[] = {
    {"fps",                 "required output frames per second rate", OFFSET(dest_frame_rate), AV_OPT_TYPE_VIDEO_RATE, {.str="50"},             0,       INT_MAX, V|F },

    {"interp_start",        "point to start linear interpolation",    OFFSET(interp_start),    AV_OPT_TYPE_INT,      {.i64=15},                 0,       255,     V|F },
    {"interp_end",          "point to end linear interpolation",      OFFSET(interp_end),      AV_OPT_TYPE_INT,      {.i64=240},                0,       255,     V|F },
    {"scene",               "scene change level",                     OFFSET(scene_score),     AV_OPT_TYPE_DOUBLE,   {.dbl=7.0},                0,       INT_MAX, V|F },

    {"flags",               "set flags",                              OFFSET(flags),           AV_OPT_TYPE_FLAGS,    {.i64=1},                  0,       INT_MAX, V|F, "flags" },
    {"scene_change_detect", "enable scene change detection",          0,                       AV_OPT_TYPE_CONST,    {.i64=FRAMERATE_FLAG_SCD}, INT_MIN, INT_MAX, V|F, "flags" },
    {"scd",                 "enable scene change detection",          0,                       AV_OPT_TYPE_CONST,    {.i64=FRAMERATE_FLAG_SCD}, INT_MIN, INT_MAX, V|F, "flags" },

    {NULL}
};

AVFILTER_DEFINE_CLASS(framerate);

static void next_source(AVFilterContext *ctx)
{
    FrameRateContext *s = ctx->priv;

    av_dlog(ctx,  "next_source()\n");

    if (s->srce[s->last] && s->srce[s->last] != s->srce[s->last-1]) {
        av_dlog(ctx, "next_source() unlink %d\n", s->last);
        av_frame_free(&s->srce[s->last]);
    }
    for (int i = s->last; i > s->frst; i--) {
        av_dlog(ctx, "next_source() copy %d to %d\n", i - 1, i);
        s->srce[i] = s->srce[i - 1];
    }
    av_dlog(ctx, "next_source() make %d null\n", s->frst);
    s->srce[s->frst] = NULL;
}

static double get_scene_score(AVFilterContext *ctx, AVFrame *crnt, AVFrame *next)
{
    FrameRateContext *s = ctx->priv;

    double ret = 0;

    av_dlog(ctx, "get_scene_score()\n");

    if (crnt &&
            crnt->height == next->height &&
            crnt->width  == next->width) {
        int x, y;
        int64_t sad;
        double mafd, diff;
        uint8_t *p1 = crnt->data[0];
        uint8_t *p2 = next->data[0];
        const int p1_linesize = crnt->linesize[0];
        const int p2_linesize = next->linesize[0];

        av_dlog(ctx, "get_scene_score() process\n");

        for (sad = y = 0; y < crnt->height; y += 8) {
            for (x = 0; x < p1_linesize; x += 8) {
                sad += s->sad(p1 + y * p1_linesize + x,
                              p1_linesize,
                              p2 + y * p2_linesize + x,
                              p2_linesize);
            }
        }
        emms_c();
        mafd = sad / (crnt->height * crnt->width * 3);
        diff = fabs(mafd - s->prev_mafd);
        ret  = av_clipf(FFMIN(mafd, diff), 0, 100.0);
        s->prev_mafd = mafd;
    }
        av_dlog(ctx, "get_scene_score() result is:%f\n", ret);
    return ret;
}

static int process_work_frame(AVFilterContext *ctx)
{
    FrameRateContext *s = ctx->priv;
    int64_t work_next_pts;
    AVFrame *copy_src1, *copy_src2;
    int interpolate;

    av_dlog(ctx, "process_work_frame()\n");

    av_dlog(ctx, "process_work_frame() pending_input_frames %d\n", s->pending_srce_frames);

    if (s->srce[s->prev]) av_dlog(ctx, "process_work_frame() srce prev pts:%"PRId64"\n", s->srce[s->prev]->pts);
    if (s->srce[s->crnt]) av_dlog(ctx, "process_work_frame() srce crnt pts:%"PRId64"\n", s->srce[s->crnt]->pts);
    if (s->srce[s->next]) av_dlog(ctx, "process_work_frame() srce next pts:%"PRId64"\n", s->srce[s->next]->pts);

    if (!s->srce[s->crnt]) {
        // the filter cannot do anything
        av_dlog(ctx, "process_work_frame() no current frame cached: move on to next frame, do not output a frame\n");
        next_source(ctx);
        return 0;
    }

    av_assert0(s->work);

    work_next_pts = s->work->pts + s->average_dest_pts_delta;

    av_dlog(ctx, "process_work_frame() work crnt pts:%"PRId64"\n", s->work->pts);
    av_dlog(ctx, "process_work_frame() work next pts:%"PRId64"\n", work_next_pts);
    if (s->srce[s->prev])
        av_dlog(ctx, "process_work_frame() srce prev pts:%"PRId64" at dest time base:%u/%u\n",
            s->srce_pts_dest[s->prev], s->dest_time_base.num, s->dest_time_base.den);
    if (s->srce[s->crnt])
        av_dlog(ctx, "process_work_frame() srce crnt pts:%"PRId64" at dest time base:%u/%u\n",
            s->srce_pts_dest[s->crnt], s->dest_time_base.num, s->dest_time_base.den);
    if (s->srce[s->next])
        av_dlog(ctx, "process_work_frame() srce next pts:%"PRId64" at dest time base:%u/%u\n",
            s->srce_pts_dest[s->next], s->dest_time_base.num, s->dest_time_base.den);

    av_assert0(s->srce[s->next]);

    // should filter be skipping input frame (output frame rate is lower than input frame rate)
    if (!s->flush && s->work->pts >= s->srce_pts_dest[s->next]) {
        av_dlog(ctx, "process_work_frame() work crnt pts >= srce next pts: SKIP FRAME, move on to next frame, do not output a frame\n");
        next_source(ctx);
        s->pending_srce_frames--;
        return 0;
    }

    // calculate interpolation
    interpolate = (int) ((s->work->pts - s->srce_pts_dest[s->crnt]) * 256 / s->average_srce_pts_dest_delta);
    av_dlog(ctx, "process_work_frame() interpolate:%d/256\n", interpolate);
    copy_src1 = s->srce[s->crnt];
    if (interpolate > s->interp_end) {
        av_dlog(ctx, "process_work_frame() source is:NEXT\n");
        copy_src1 = s->srce[s->next];
    }
    if (s->srce[s->prev] && interpolate < -s->interp_end) {
        av_dlog(ctx, "process_work_frame() source is:PREV\n");
        copy_src1 = s->srce[s->prev];
    }

    // decide whether to blend two frames
    if ((interpolate >= s->interp_start && interpolate <= s->interp_end) || (interpolate <= -s->interp_start && interpolate >= -s->interp_end)) {
        double interpolate_scene_score = 0;

        if (interpolate > 0) {
            av_dlog(ctx, "process_work_frame() interpolate source is:NEXT\n");
            copy_src2 = s->srce[s->next];
        } else {
            av_dlog(ctx, "process_work_frame() interpolate source is:PREV\n");
            copy_src2 = s->srce[s->prev];
        }
        if ((s->flags & FRAMERATE_FLAG_SCD) && copy_src2) {
            interpolate_scene_score = get_scene_score(ctx, copy_src1, copy_src2);
            av_dlog(ctx, "process_work_frame() interpolate scene score:%f\n", interpolate_scene_score);
        }
        // decide if the shot-change detection allows us to blend two frames
        if (interpolate_scene_score < s->scene_score && copy_src2) {
            uint16_t src2_factor = abs(interpolate);
            uint16_t src1_factor = 256 - src2_factor;
            av_dlog(ctx, "process_work_frame() INTERPOLATE to create work frame\n");
            for (int plane = 0; plane < 4 && copy_src1->data[plane] && copy_src2->data[plane]; plane++) {
                int cpy_line_width = s->line_size[plane];
                uint8_t *cpy_src1_data = copy_src1->data[plane];
                int cpy_src1_line_size = copy_src1->linesize[plane];
                uint8_t *cpy_src2_data = copy_src2->data[plane];
                int cpy_src2_line_size = copy_src2->linesize[plane];
                int cpy_src_h = (plane > 0 && plane < 3) ? (copy_src1->height >> s->vsub) : (copy_src1->height);
                uint8_t *cpy_dst_data = s->work->data[plane];
                int cpy_dst_line_size = s->work->linesize[plane];
                if (plane <1 || plane >2) {
                    // luma or alpha
                    for (int line = 0; line < cpy_src_h; line++) {
                        for (int pixel = 0; pixel < cpy_line_width; pixel++) {
                            // integer version of (src1 * src1_factor) + (src2 + src2_factor) + 0.5
                            // 0.5 is for rounding
                            // 128 is the integer representation of 0.5 << 8
                            cpy_dst_data[pixel] = ((cpy_src1_data[pixel] * src1_factor) + (cpy_src2_data[pixel] * src2_factor) + 128) >> 8;
                        }
                        cpy_src1_data += cpy_src1_line_size;
                        cpy_src2_data += cpy_src2_line_size;
                        cpy_dst_data += cpy_dst_line_size;
                    }
                } else {
                    // chroma
                    for (int line = 0; line < cpy_src_h; line++) {
                        for (int pixel = 0; pixel < cpy_line_width; pixel++) {
                            // as above
                            // because U and V are based around 128 we have to subtract 128 from the components.
                            // 32896 is the integer representation of 128.5 << 8
                            cpy_dst_data[pixel] = (((cpy_src1_data[pixel] - 128) * src1_factor) + ((cpy_src2_data[pixel] - 128) * src2_factor) + 32896) >> 8;
                        }
                        cpy_src1_data += cpy_src1_line_size;
                        cpy_src2_data += cpy_src2_line_size;
                        cpy_dst_data += cpy_dst_line_size;
                    }
                }
            }
            goto copy_done;
        }
        else {
            av_dlog(ctx, "process_work_frame() CUT - DON'T INTERPOLATE\n");
        }
    }

    av_dlog(ctx, "process_work_frame() COPY to the work frame\n");
    // copy the frame we decided is our base source
    for (int plane = 0; plane < 4 && copy_src1->data[plane]; plane++) {
        int cpy_line_width = s->line_size[plane];
        uint8_t *cpy_src1_data = copy_src1->data[plane];
        int cpy_src1_line_size = copy_src1->linesize[plane];
        int cpy_src_h = (plane > 0 && plane < 3) ? (copy_src1->height >> s->vsub) : (copy_src1->height);
        uint8_t *cpy_dst_data = s->work->data[plane];
        int cpy_dst_line_size = s->work->linesize[plane];
        for (int line = 0; line < cpy_src_h; line++) {
            memcpy(cpy_dst_data, cpy_src1_data, cpy_line_width);
            cpy_src1_data += cpy_src1_line_size;
            cpy_dst_data += cpy_dst_line_size;
        }
    }

copy_done:
    // should filter be re-using input frame (output frame rate is higher than input frame rate)
    if (!s->flush && (work_next_pts + s->average_dest_pts_delta) < (s->srce_pts_dest[s->crnt] + s->average_srce_pts_dest_delta)) {
        av_dlog(ctx, "process_work_frame() REPEAT FRAME\n");
    } else {
        av_dlog(ctx, "process_work_frame() CONSUME FRAME, move to next frame\n");
        s->pending_srce_frames--;
        next_source(ctx);
    }
    av_dlog(ctx, "process_work_frame() output a frame\n");
    return 1;
}

static void set_srce_frame_dest_pts(AVFilterContext *ctx)
{
    FrameRateContext *s = ctx->priv;

    av_dlog(ctx, "set_srce_frame_output_pts()\n");

    // scale the input pts from the timebase difference between input and output
    if (s->srce[s->prev])
        s->srce_pts_dest[s->prev] = av_rescale_q(s->srce[s->prev]->pts, s->srce_time_base, s->dest_time_base);
    if (s->srce[s->crnt])
        s->srce_pts_dest[s->crnt] = av_rescale_q(s->srce[s->crnt]->pts, s->srce_time_base, s->dest_time_base);
    if (s->srce[s->next])
        s->srce_pts_dest[s->next] = av_rescale_q(s->srce[s->next]->pts, s->srce_time_base, s->dest_time_base);
}

static void set_work_frame_pts(AVFilterContext *ctx)
{
    FrameRateContext *s = ctx->priv;
    int pts, average_srce_pts_delta = 0;

    av_dlog(ctx, "set_work_frame_pts()\n");


    av_assert0(s->srce[s->next]);
    av_assert0(s->srce[s->crnt]);

    av_dlog(ctx, "set_work_frame_pts() srce crnt pts:%"PRId64"\n", s->srce[s->crnt]->pts);
    av_dlog(ctx, "set_work_frame_pts() srce next pts:%"PRId64"\n", s->srce[s->next]->pts);
    if (s->srce[s->prev])
        av_dlog(ctx, "set_work_frame_pts() srce prev pts:%"PRId64"\n", s->srce[s->prev]->pts);

    average_srce_pts_delta = s->average_srce_pts_dest_delta;
    av_dlog(ctx, "set_work_frame_pts() initial average srce pts:%d\n", average_srce_pts_delta);

    // calculate the PTS delta
    if ((pts = (s->srce[s->next]->pts - s->srce[s->crnt]->pts))) {
        average_srce_pts_delta = average_srce_pts_delta?((average_srce_pts_delta+pts)>>1):pts;
    } else if (s->srce[s->prev] && (pts = (s->srce[s->crnt]->pts - s->srce[s->prev]->pts))) {
        average_srce_pts_delta = average_srce_pts_delta?((average_srce_pts_delta+pts)>>1):pts;
    }

    s->average_srce_pts_dest_delta = av_rescale_q(average_srce_pts_delta, s->srce_time_base, s->dest_time_base);
    av_dlog(ctx, "set_work_frame_pts() average srce pts:%d\n", average_srce_pts_delta);
    av_dlog(ctx, "set_work_frame_pts() average srce pts:%d at dest time base:%u/%u\n",
            s->average_srce_pts_dest_delta, s->dest_time_base.num, s->dest_time_base.den);

    set_srce_frame_dest_pts(ctx);

    if (ctx->inputs[0] && !s->average_dest_pts_delta) {
        // TODO does this overflow / underflow / lose precision
        // TODO why would this ever change?

//            s->average_dest_pts_delta =
//                ctx->inputs[0]->time_base.den / ctx->inputs[0]->time_base.num *
//                s->dest_frame_rate.den / s->dest_frame_rate.num;

        s->average_dest_pts_delta = av_rescale_q(
                ctx->inputs[0]->time_base.den / ctx->inputs[0]->time_base.num *
                s->dest_frame_rate.den / s->dest_frame_rate.num,
                s->srce_time_base, s->dest_time_base);

        av_dlog(ctx, "set_frame_pts() average output pts from input timebase\n");
        av_dlog(ctx, "set_work_frame_pts() average dest pts:%d\n", s->average_dest_pts_delta);
    }
//    else {
//        s->average_output_pts_delta = s->average_input_pts_delta
//                * s->input_frame_rate.num / s->input_frame_rate.den
//                * s->output_frame_rate.den / s->output_frame_rate.num;
//        av_dlog(ctx, "set_frame_pts() average output pts from source pts\n");
//    }


    if (!s->dest_frame_num) {
        s->work->pts = s->last_dest_frame_pts = s->srce_pts_dest[s->crnt];
    } else {
        s->work->pts = s->last_dest_frame_pts + s->average_dest_pts_delta;
    }

    av_dlog(ctx, "set_work_frame_pts() calculated pts:%"PRId64" at dest time base:%u/%u\n",
            s->work->pts, s->dest_time_base.num, s->dest_time_base.den);
}

static av_cold int init(AVFilterContext *ctx)
{
    FrameRateContext *s = ctx->priv;

    s->dest_frame_num = 0;

    s->crnt = (N_SRCE)>>1;
    s->last = N_SRCE - 1;

    s->next = s->crnt - 1;
    s->prev = s->crnt + 1;

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    FrameRateContext *s = ctx->priv;
    int i;

    for (i = s->frst + 1; i > s->last; i++) {
        if (s->srce[i] && (s->srce[i] != s->srce[i + 1]))
            av_frame_free(&s->srce[i]);
    }
    av_frame_free(&s->srce[s->last]);
}

static int query_formats(AVFilterContext *ctx)
{
    static const enum PixelFormat pix_fmts[] = {
        PIX_FMT_YUV410P,
        PIX_FMT_YUV411P,
        PIX_FMT_YUV420P,
        PIX_FMT_YUV422P,
        PIX_FMT_YUV440P,
        PIX_FMT_YUV444P,
        PIX_FMT_NONE
    };

    AVFilterFormats *fmts_list = ff_make_format_list(pix_fmts);
    if (!fmts_list)
        return AVERROR(ENOMEM);
    return ff_set_common_formats(ctx, fmts_list);
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    FrameRateContext *s = ctx->priv;
    const AVPixFmtDescriptor *pix_desc = av_pix_fmt_desc_get(inlink->format);
    int plane;

    /** full an array with the number of bytes that the video
     *  data occupies per line for each plane of the input video */
    for (plane = 0; plane < 4; plane++) {
        s->line_size[plane] = av_image_get_linesize(
                inlink->format,
                inlink->w,
                plane);
    }

    s->vsub = pix_desc->log2_chroma_h;

    s->sad = av_pixelutils_get_sad_fn(3, 3, 2, s); // 8x8 both sources aligned
    if (!s->sad)
        return AVERROR(EINVAL);

    s->srce_time_base = inlink->time_base;

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *inpicref)
{
    AVFilterContext *ctx = inlink->dst;
    FrameRateContext *s = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];

    // we have one new frame
    s->pending_srce_frames++;

    if (inpicref->interlaced_frame)
        av_log(ctx, AV_LOG_WARNING, "Interlaced frame found - the output will not be correct\n");

    // store the pointer to the new frame
    av_frame_free(&s->srce[s->frst]);
    s->srce[s->frst] = inpicref;

    if (!s->pending_end_frame && s->srce[s->crnt]) {
        s->work = ff_get_video_buffer(outlink, outlink->w, outlink->h);
        av_frame_copy_props(s->work, s->srce[s->crnt]);
        set_work_frame_pts(ctx);

        s->pending_end_frame = 1;
    } else {
        set_srce_frame_dest_pts(ctx);
    }

//    if (!s->srce[s->crnt]) {
//        av_dlog(ctx, "end_frame() no current frame\n");
//        return;
//    }

    if (process_work_frame(ctx)) {
        ff_filter_frame(outlink, s->work);
        s->dest_frame_num++;
        s->last_dest_frame_pts = s->work->pts;

        s->pending_end_frame = 0;
    }

    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    FrameRateContext *s = ctx->priv;
    int exact;

    av_dlog(ctx, "config_output()\n");

    av_dlog(ctx,
           "config_output() input time base:%u/%u (%f)\n",
           ctx->inputs[0]->time_base.num,ctx->inputs[0]->time_base.den,
           av_q2d(ctx->inputs[0]->time_base));

    // make sure timebase is small enough to hold the framerate

    exact = av_reduce(&s->dest_time_base.num, &s->dest_time_base.den,
                      av_gcd((int64_t)s->srce_time_base.num * s->dest_frame_rate.num,
                             (int64_t)s->srce_time_base.den * s->dest_frame_rate.den ),
                      (int64_t)s->srce_time_base.den * s->dest_frame_rate.num, INT_MAX);

    av_log(ctx, AV_LOG_INFO,
           "time base:%u/%u -> %u/%u exact:%d\n",
           s->srce_time_base.num, s->srce_time_base.den,
           s->dest_time_base.num, s->dest_time_base.den, exact);
    if (!exact) {
        av_log(ctx, AV_LOG_WARNING, "Timebase conversion is not exact\n");
    }

    outlink->frame_rate = s->dest_frame_rate;
    outlink->time_base = s->dest_time_base;
    outlink->flags |= FF_LINK_FLAG_REQUEST_LOOP;

    av_dlog(ctx,
           "config_output() output time base:%u/%u (%f) w:%d h:%d\n",
           outlink->time_base.num, outlink->time_base.den,
           av_q2d(outlink->time_base),
           outlink->w, outlink->h);


    av_log(ctx, AV_LOG_INFO, "fps -> fps:%u/%u scene score:%f interpolate start:%d end:%d\n",
            s->dest_frame_rate.num, s->dest_frame_rate.den,
            s->scene_score, s->interp_start, s->interp_end);

    return 0;
}

static int request_frame(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    FrameRateContext *s = ctx->priv;
    int val, i, ret = 0;

    av_dlog(ctx, "request_frame()\n");

    // if there is no "next" frame AND we are not in flush then get one from our input filter
    if (!s->srce[s->frst] && !s->flush) {
        av_dlog(ctx, "request_frame() call source's request_frame()\n");
        if ((val = ff_request_frame(outlink->src->inputs[0])) < 0) {
            av_dlog(ctx, "request_frame() source's request_frame() returned error:%d\n", val);
            return val;
        }
        av_dlog(ctx, "request_frame() source's request_frame() returned:%d\n", val);
        return 0;
    }

    av_dlog(ctx, "request_frame() REPEAT or FLUSH\n");

    if (s->pending_srce_frames <= 0) {
        av_dlog(ctx, "request_frame() nothing else to do, return:EOF\n");
        return AVERROR(AVERROR_EOF);
    }

    // otherwise, make brand-new frame and pass to our output filter
    av_dlog(ctx, "request_frame() FLUSH\n");

    // back fill at end of file when source has no more frames
    for (i = s->last; i > s->frst; i--) {
        if (!s->srce[i - 1] && s->srce[i]) {
            av_dlog(ctx, "request_frame() copy:%d to:%d\n", i, i - 1);
            s->srce[i - 1] = s->srce[i];
        }
    }

    // get work-space for output frame
    s->work = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    av_frame_copy_props(s->work, s->srce[s->crnt]);
    set_work_frame_pts(ctx);

    if (process_work_frame(ctx)) {
        av_dlog(ctx, "request_frame() we are outputing a frame\n");
        ret = ff_filter_frame(outlink, s->work);
        s->dest_frame_num++;
        s->last_dest_frame_pts = s->work->pts;

    }
    return ret;
}

static const AVFilterPad framerate_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = config_input,
        .filter_frame = filter_frame,
    },
    { NULL }
};

static const AVFilterPad framerate_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .request_frame = request_frame,
        .config_props  = config_output,
    },
    { NULL }
};

AVFilter ff_vf_framerate = {
    .name          = "framerate",
    .description   = NULL_IF_CONFIG_SMALL("Upsamples or downsamples progressive source between specified frame rates"),
    .priv_size     = sizeof(FrameRateContext),
    .priv_class    = &framerate_class,
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = framerate_inputs,
    .outputs       = framerate_outputs,
};
