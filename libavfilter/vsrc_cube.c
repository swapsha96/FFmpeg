/*
 * Copyright (c) 2016 Paul B Mahol
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

#include <float.h>

#include "libavutil/internal.h"
#include "libavutil/opt.h"
#include "libavutil/parseutils.h"
#include "avfilter.h"
#include "internal.h"
#include "formats.h"
#include "video.h"

typedef struct CubeContext {
    const AVClass *class;

    int w, h;
    AVRational frame_rate;
    float angle[3];
    float pos[3];
    float x, y, z;

    int64_t pts;
} CubeContext;

#define OFFSET(x) offsetof(CubeContext, x)
#define FLAGS AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_FILTERING_PARAM

static const AVOption cube_options[] = {
    { "size",     "set video size",   OFFSET(w),        AV_OPT_TYPE_IMAGE_SIZE, {.str = "vga"}, 0, 0, FLAGS },
    { "s",        "set video size",   OFFSET(w),        AV_OPT_TYPE_IMAGE_SIZE, {.str = "vga"}, 0, 0, FLAGS },
    { "rate",     "set video rate",   OFFSET(frame_rate), AV_OPT_TYPE_VIDEO_RATE, {.str = "25"}, 0, INT_MAX, FLAGS },
    { "r",        "set video rate",   OFFSET(frame_rate), AV_OPT_TYPE_VIDEO_RATE, {.str = "25"}, 0, INT_MAX, FLAGS },
    { "ax",       "set rotation angle around x axis",   OFFSET(angle[0]), AV_OPT_TYPE_FLOAT, {.dbl = M_PI/180}, 0, M_PI, FLAGS },
    { "ay",       "set rotation angle around y axis",   OFFSET(angle[1]), AV_OPT_TYPE_FLOAT, {.dbl = M_PI/240}, 0, M_PI, FLAGS },
    { "az",       "set rotation angle around z axis",   OFFSET(angle[2]), AV_OPT_TYPE_FLOAT, {.dbl = M_PI/360}, 0, M_PI, FLAGS },
    { "x",        "set camera position on x axis",      OFFSET(pos[0]),   AV_OPT_TYPE_FLOAT, {.dbl = 7}, 0, 10, FLAGS },
    { "y",        "set camera position on y axis",      OFFSET(pos[1]),   AV_OPT_TYPE_FLOAT, {.dbl = 7}, 0, 10, FLAGS },
    { "z",        "set camera position on z axis",      OFFSET(pos[2]),   AV_OPT_TYPE_FLOAT, {.dbl = 7}, 0, 10, FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(cube);

static int config_props(AVFilterLink *outlink)
{
    CubeContext *s = outlink->src->priv;

    outlink->w = s->w;
    outlink->h = s->h;
    outlink->frame_rate = s->frame_rate;
    outlink->time_base = av_inv_q(s->frame_rate);
    outlink->sample_aspect_ratio = (AVRational) {1, 1};

    return 0;
}

static void vsub(float *v1, float *v2, float *s)
{
    s[0] = v1[0] - v2[0];
    s[1] = v1[1] - v2[1];
    s[2] = v1[2] - v2[2];
}

static float normalize(float * v)
{
    float len = sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);

    v[0] /= len; v[1] /= len; v[2] /= len;

    return len;
}

static inline float dot(float *x, float *y)
{
    return x[0]*y[0] + x[1]*y[1] + x[2]*y[2];
}

static float *cross(float x[3], float y[3], float s[3])
{
    s[0] = x[1] * y[2] - x[2] * y[1];
    s[1] = x[2] * y[0] - x[0] * y[2];
    s[2] = x[0] * y[1] - x[1] * y[0];
    return s;
}

static float *madd(float *x, float *y, float d, float *r)
{
    r[0] = x[0] + y[0] * d;
    r[1] = x[1] + y[1] * d;
    r[2] = x[2] + y[2] * d;
    return r;
}

static int inline GetIntersection(float fDst1, float fDst2, float *P1, float *P2, float *Hit)
{
    float x[3];

    if ((fDst1 * fDst2) >= 0.0f)
        return 0;
    if (fDst1 == fDst2)
        return 0;

    vsub(P2, P1, x);
    madd(P1, x, -fDst1 / (fDst2 - fDst1), Hit);
    return 1;
}

static int inline InBox(float *Hit, const int Axis)
{
    if (Axis == 1 && Hit[2] >= -1 && Hit[2] <= 1 && Hit[1] >= -1 && Hit[1] <= 1) return 1;
    if (Axis == 2 && Hit[2] >= -1 && Hit[2] <= 1 && Hit[0] >= -1 && Hit[0] <= 1) return 1;
    if (Axis == 3 && Hit[0] >= -1 && Hit[0] <= 1 && Hit[1] >= -1 && Hit[1] <= 1) return 1;

    return 0;
}

static int CheckLineBox(float *L1, float *L2, float *Hit)
{
    int ret = 0;
    float hit[3];
    float temp2[3];
    float distance;
    float min_distance = DBL_MAX;

    if (L2[0] < -1 && L1[0] < -1)
        return 0;
    if (L2[0] > 1 && L1[0] > 1)
        return 0;
    if (L2[1] < -1 && L1[1] < -1)
        return 0;
    if (L2[1] > 1 && L1[1] > 1)
        return 0;
    if (L2[2] < -1 && L1[2] < -1)
        return 0;
    if (L2[2] > 1 && L1[2] > 1)
        return 0;

    if (L1[0] > -1 && L1[0] < 1 &&
        L1[1] > -1 && L1[1] < 1 &&
        L1[2] > -1 && L1[2] < 1) {
        Hit = L1;
        return 1;
    }
    if ((GetIntersection(L1[0] + 1, L2[0] + 1, L1, L2, hit) && InBox(hit, 1))) {
        vsub(L1, hit, temp2);
        min_distance = dot(temp2, temp2);
        Hit[0] = hit[0];
        Hit[1] = hit[1];
        Hit[2] = hit[2];
        ret++;
    }
    if ((GetIntersection(L1[1] + 1, L2[1] + 1, L1, L2, hit) && InBox(hit, 2))) {
        vsub(L1, hit, temp2);
        distance = dot(temp2, temp2);
        if (distance < min_distance) {
            Hit[0] = hit[0];
            Hit[1] = hit[1];
            Hit[2] = hit[2];
            min_distance = distance;
        }
        ret++;
        if (ret > 1)
            return 1;
    }
    if ((GetIntersection(L1[2] + 1, L2[2] + 1, L1, L2, hit) && InBox(hit, 3))) {
        vsub(L1, hit, temp2);
        distance = dot(temp2, temp2);
        if (distance < min_distance) {
            Hit[0] = hit[0];
            Hit[1] = hit[1];
            Hit[2] = hit[2];
            min_distance = distance;
        }
        ret++;
        if (ret > 1)
            return 1;
    }
    if ((GetIntersection(L1[0] - 1, L2[0] - 1, L1, L2, hit) && InBox(hit, 1))) {
        vsub(L1, hit, temp2);
        distance = dot(temp2, temp2);
        if (distance < min_distance) {
            Hit[0] = hit[0];
            Hit[1] = hit[1];
            Hit[2] = hit[2];
            min_distance = distance;
        }
        ret++;
        if (ret > 1)
            return 1;
    }
    if ((GetIntersection(L1[1] - 1, L2[1] - 1, L1, L2, hit) && InBox(hit, 2))) {
        vsub(L1, hit, temp2);
        distance = dot(temp2, temp2);
        if (distance < min_distance) {
            Hit[0] = hit[0];
            Hit[1] = hit[1];
            Hit[2] = hit[2];
            min_distance = distance;
        }
        ret++;
        if (ret > 1)
            return 1;
    }
    if ((GetIntersection(L1[2] - 1, L2[2] - 1, L1, L2, hit) && InBox(hit, 3))) {
        vsub(L1, hit, temp2);
        distance = dot(temp2, temp2);
        if (distance < min_distance) {
            Hit[0] = hit[0];
            Hit[1] = hit[1];
            Hit[2] = hit[2];
            min_distance = distance;
        }
        ret++;
        if (ret > 1)
            return 1;
    }
    return !!ret;
}

static void rotate(float *v, float angle_x, float angle_y, float angle_z)
{
    float sinX = sin(angle_x);
    float cosX = cos(angle_x);
    float sinY = sin(angle_y);
    float cosY = cos(angle_y);
    float sinZ = sin(angle_z);
    float cosZ = cos(angle_z);
    float x = v[0];
    float y = v[1];
    float z = v[2];

    v[1] = x * cosX + y * sinX;
    v[2] =-x * sinX + y * cosX;

    y = v[1];
    z = v[2];

    v[0] = x * cosY - z * sinY;
    v[2] = x * sinY + z * cosY;

    z = v[2];

    v[0] = y * cosZ + z * sinZ;
    v[1] =-y * sinZ + z * cosZ;
}

static void rotate_cube(CubeContext *s, AVFrame *out, float angle_x, float angle_y, float angle_z)
{
    const float zoom = FFMIN(out->width, out->height) / 10;
    float eye[3];
    float hit[3], dx[3] = { 0, 0, 0}, dy[3] = {1, 0, 0};
    int hw = out->width / 2;
    int hh = out->height / 2;
    uint8_t *dst = out->data[0];
    int i, j;

    eye[0] = s->pos[0];
    eye[1] = s->pos[1];
    eye[2] = s->pos[2];
    rotate(eye, angle_x, angle_y, angle_z);

    normalize(cross(eye, dy, dx));
    normalize(cross(eye, dx, dy));

    for (i = 0; i < out->height; i++) {
        for (j = 0; j < out->width; j++) {
            float proj[3] = {0, 0, 0};
            float dir[3];

            madd(proj, dx, (j - hw) / zoom, proj);
            madd(proj, dy, (i - hh) / zoom, proj);
            vsub(proj, eye, dir);

            if (!CheckLineBox(eye, dir, hit)) {
                dst[j * 4 + 0] = 0;
                dst[j * 4 + 1] = 0;
                dst[j * 4 + 2] = 0;
                dst[j * 4 + 3] = 0;
                continue;
            }

            dst[j * 4 + 0] = (hit[0] + 1) * 127.5;
            dst[j * 4 + 1] = (hit[1] + 1) * 127.5;
            dst[j * 4 + 2] = (hit[2] + 1) * 127.5;
            dst[j * 4 + 3] = 255;
        }
        dst += out->linesize[0];
    }
}

static int request_frame(AVFilterLink *outlink)
{
    CubeContext *s = outlink->src->priv;
    AVFrame *out = ff_get_video_buffer(outlink, s->w, s->h);
    if (!out)
        return AVERROR(ENOMEM);
    out->sample_aspect_ratio = (AVRational) {1, 1};

    rotate_cube(s, out, s->x, s->y, s->z);
    s->x += s->angle[0];
    s->y += s->angle[1];
    s->z += s->angle[2];
    out->pts = s->pts++;
    return ff_filter_frame(outlink, out);
}

static int query_formats(AVFilterContext *ctx)
{
    enum AVPixelFormat pix_fmts[] = { AV_PIX_FMT_RGBA, AV_PIX_FMT_NONE };
    AVFilterFormats *fmts_list;
    fmts_list = ff_make_format_list(pix_fmts);
    return ff_set_common_formats(ctx, fmts_list);
}

static const AVFilterPad cube_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .request_frame = request_frame,
        .config_props  = config_props,
    },
    { NULL}
};

AVFilter ff_vsrc_cube = {
    .name          = "cube",
    .description   = NULL_IF_CONFIG_SMALL("Draw 3D rotating cube."),
    .priv_size     = sizeof(CubeContext),
    .priv_class    = &cube_class,
    .query_formats = query_formats,
    .inputs        = NULL,
    .outputs       = cube_outputs,
};
