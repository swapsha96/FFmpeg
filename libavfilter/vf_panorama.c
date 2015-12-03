/*
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
#include "libavutil/eval.h"
#include "libavutil/imgutils.h"
#include "libavutil/pixdesc.h"
#include "libavutil/opt.h"
#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "video.h"

enum Projections {
    EQUIRECTANGULAR,
    CUBEMAP,
    NB_PROJECTIONS,
};

enum Faces {
    LEFT,
    FRONT,
    RIGHT,
    TOP,
    BACK,
    DOWN,
};

struct XYRemap {
    int vi, ui;
    int v2, u2;
    double a, b, c, d;
};

typedef struct PanoramaContext {
    const AVClass *class;
    int in, out;

    int planewidth[4], planeheight[4];
    int inplanewidth[4], inplaneheight[4];
    int nb_planes;

    struct XYRemap *remap[4];

    int (*panorama)(struct PanoramaContext *s,
                    const uint8_t *src, uint8_t *dst,
                    int width, int height,
                    int in_linesize, int out_linesize,
                    const struct XYRemap *remap);
} PanoramaContext;

#define OFFSET(x) offsetof(PanoramaContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM

static const AVOption panorama_options[] = {
    { "input",  "set input projection",   OFFSET(in), AV_OPT_TYPE_INT,   {.i64=EQUIRECTANGULAR}, 0, NB_PROJECTIONS-1, FLAGS, "in" },
    {     "e", "equirectangular",                  0, AV_OPT_TYPE_CONST, {.i64=EQUIRECTANGULAR}, 0,                0, FLAGS, "in" },
    {     "c", "cubemap",                          0, AV_OPT_TYPE_CONST, {.i64=CUBEMAP},         0,                0, FLAGS, "in" },
    { "output", "set output projection", OFFSET(out), AV_OPT_TYPE_INT,   {.i64=CUBEMAP},         0, NB_PROJECTIONS-1, FLAGS, "out" },
    {     "e", "equirectangular",                  0, AV_OPT_TYPE_CONST, {.i64=EQUIRECTANGULAR}, 0,                0, FLAGS, "out" },
    {     "c", "cubemap",                          0, AV_OPT_TYPE_CONST, {.i64=CUBEMAP},         0,                0, FLAGS, "out" },
    { NULL }
};

AVFILTER_DEFINE_CLASS(panorama);

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_YUVA444P, AV_PIX_FMT_YUVA422P, AV_PIX_FMT_YUVA420P,
        AV_PIX_FMT_YUVJ444P, AV_PIX_FMT_YUVJ440P, AV_PIX_FMT_YUVJ422P,AV_PIX_FMT_YUVJ420P, AV_PIX_FMT_YUVJ411P,
        AV_PIX_FMT_YUV444P, AV_PIX_FMT_YUV440P, AV_PIX_FMT_YUV422P, AV_PIX_FMT_YUV420P, AV_PIX_FMT_YUV411P, AV_PIX_FMT_YUV410P,
        AV_PIX_FMT_GBRP, AV_PIX_FMT_GBRAP, AV_PIX_FMT_GRAY8, AV_PIX_FMT_NONE
    };

    AVFilterFormats *fmts_list = ff_make_format_list(pix_fmts);
    if (!fmts_list)
        return AVERROR(ENOMEM);
    return ff_set_common_formats(ctx, fmts_list);
}

static int bilinear(PanoramaContext *s,
                    const uint8_t *src, uint8_t *dst,
                    int width, int height,
                    int in_linesize, int out_linesize,
                    const struct XYRemap *remap)
{
    double A, B, C, D;
    int x, y;

    for (y = 0; y < height; y++) {
        uint8_t *d = dst + y * out_linesize;
        for (x = 0; x < width; x++) {
            const struct XYRemap *r = &remap[y * width + x];

            A = src[r->vi * in_linesize + r->ui];
            B = src[r->vi * in_linesize + r->u2];
            C = src[r->v2 * in_linesize + r->ui];
            D = src[r->v2 * in_linesize + r->u2];
            *d++ = round(A * r->a + B * r->b + C * r->c + D * r->d);
        }
    }

    return 0;
}

static int nearest(PanoramaContext *s,
                   const uint8_t *src, uint8_t *dst,
                   int width, int height,
                   int in_linesize, int out_linesize,
                   const struct XYRemap *remap)
{
    double A;
    int x, y;

    for (y = 0; y < height; y++) {
        uint8_t *d = dst + y * out_linesize;
        for (x = 0; x < width; x++) {
            const struct XYRemap *r = &remap[y * width + x];

            A = src[r->vi * in_linesize + r->ui];
            *d++ = A;
        }
    }

    return 0;
}

static void to_cube_xyz(int i, int j, int face, double ew, double eh,
                        double *x, double *y, double *z)
{
    double a = 2 * i / ew;
    double b = 2 * j / eh;

    if (face == BACK) {
        *x = -1     ;
        *y =  3. - a;
        *z =  3. - b;
    } else if (face == LEFT) {
        *x =  a  - 1;
        *y = -1     ;
        *z =  1. - b;
    } else if (face == FRONT) {
        *x =  1     ;
        *y =  a  - 3;
        *z =  1. - b;
    } else if (face == RIGHT) {
        *x =  5. - a;
        *y =  1     ;
        *z =  1. - b;
    } else if (face == TOP) {
        *x =  b  - 3;
        *y =  a  - 1;
        *z =  1     ;
    } else if (face == DOWN) {
        *x = -b  + 3;
        *y =  a  - 5;
        *z = -1     ;
    }
}

static void to_sphere_xyz(double theta, double phi, double *x, double *y, double *z)
{
    *x = cos(phi) * cos(theta);
    *y = sin(phi);
    *z = cos(phi) * sin(theta);
}

static void locate(double axis, double x, double y, double rad,
                   double rw, double rh, int *ox, int *oy)
{
    *ox = rw / axis * (x * cos(rad) - y * sin(rad));
    *oy = rh / axis * (x * sin(rad) + y * cos(rad));
    *ox += rw;
    *oy += rh;
}

static inline int equal(double a, double b, double epsilon)
{
    return fabs(a - b) < epsilon;
}

static inline int smaller(double a, double b, double epsilon)
{
    return ((a - b) < 0.0) && (!equal(a, b, epsilon));
}

static inline int in_range(double rd, double small, double large, double res)
{
   return    !smaller(rd, small, res)
          &&  smaller(rd, large, res);
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    AVFilterLink *inlink = ctx->inputs[0];
    PanoramaContext *s = ctx->priv;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);
    int p, h, w;

    if (s->in == EQUIRECTANGULAR && s->out == CUBEMAP) {
        w = inlink->w / 4 * 3;
    } else if (s->in == CUBEMAP && s->out == EQUIRECTANGULAR) {
        w = inlink->w / 3 * 4;
    } else if (s->in == s->out) {
        w = inlink->w;
    } else {
        av_assert0(0);
    }
    h = inlink->h;

    s->planeheight[1] = s->planeheight[2] = FF_CEIL_RSHIFT(h, desc->log2_chroma_h);
    s->planeheight[0] = s->planeheight[3] = h;
    s->planewidth[1] = s->planewidth[2] = FF_CEIL_RSHIFT(w, desc->log2_chroma_w);
    s->planewidth[0] = s->planewidth[3] = w;

    outlink->h = h;
    outlink->w = w;

    s->inplaneheight[1] = s->inplaneheight[2] = FF_CEIL_RSHIFT(inlink->h, desc->log2_chroma_h);
    s->inplaneheight[0] = s->inplaneheight[3] = inlink->h;
    s->inplanewidth[1]  = s->inplanewidth[2]  = FF_CEIL_RSHIFT(inlink->w, desc->log2_chroma_w);
    s->inplanewidth[0]  = s->inplanewidth[3]  = inlink->w;
    s->nb_planes = av_pix_fmt_count_planes(inlink->format);

    for (p = 0; p < s->nb_planes; p++) {
        s->remap[p] = av_calloc(s->planewidth[p] * s->planeheight[p], sizeof(struct XYRemap));
        if (!s->remap[p])
            return AVERROR(ENOMEM);
    }

    if (s->in == EQUIRECTANGULAR && s->out == CUBEMAP) {
        for (p = 0; p < s->nb_planes; p++) {
            int face, ui, vi, u2, v2;
            double theta, R, phi, uf, vf, mu, nu, x, y, z;
            int ew = s->planewidth[p] / 3;
            int eh = s->planeheight[p] / 2;
            int width = s->planewidth[p];
            int height = s->planeheight[p];
            int in_width = s->inplanewidth[p];
            int in_height = s->inplaneheight[p];
            int i, j;

            for (i = 0; i < width; i++) {
                for (j = 0; j < height; j++) {
                    struct XYRemap *r = &s->remap[p][j * width + i];
                    face = (i / ew) + 3 * (j / (height / 2));

                    to_cube_xyz(i, j, face, ew, eh, &x, &y, &z);
                    theta = atan2(y, x);
                    R = hypot(x, y);
                    phi = atan2(z, R);
                    uf = (2.0 * ew * (theta + M_PI) / M_PI);
                    vf = (2.0 * eh * (M_PI_2 - phi) / M_PI);

                    ui = floor(uf);
                    vi = floor(vf);
                    u2 = ui + 1;
                    v2 = vi + 1;
                    mu = uf - ui;
                    nu = vf - vi;
                    r->vi = av_clip(vi, 0, in_height - 1);
                    r->ui = ui % in_width;
                    r->v2 = av_clip(v2, 0, in_height - 1);
                    r->u2 = u2 % in_width;
                    r->a = (1 - mu) * (1 - nu);
                    r->b =  mu * (1 - nu);
                    r->c = (1 - mu) * nu;
                    r->d = mu * nu;
                }
            }
        }
        s->panorama = bilinear;
    } else if (s->in == CUBEMAP && s->out == EQUIRECTANGULAR) {
        for (p = 0; p < s->nb_planes; p++) {
            double theta, theta_norm, phi, phi_threshold, x, y, z;
            int height = s->planeheight[p];
            int width = s->planewidth[p];
            int in_width = s->inplanewidth[p];
            int in_height = s->inplaneheight[p];
            double res = M_PI_4 / (width / 3) / 10.0;
            int rh = in_height / 4;
            int rw = in_width / 6;
            int face, i, j;
            int ox, oy;

            for (i = 0; i < width; i++) {
                for (j = 0; j < height; j++) {
                    struct XYRemap *r = &s->remap[p][j * width + i];

                    x = (2. * i) / width - 1.;
                    y = (2. * j) / height - 1.;
                    theta = x * M_PI;
                    phi   = y * M_PI_2;
                    to_sphere_xyz(theta, phi, &x, &y, &z);

                    if (in_range(theta, -M_PI_4, M_PI_4, res)) {
                        face = FRONT;
                        theta_norm = theta;
                    } else if (in_range(theta, -(M_PI_2 + M_PI_4), -M_PI_4, res)) {
                        face = LEFT;
                        theta_norm = theta + M_PI_2;
                    } else if (in_range(theta, M_PI_4, M_PI_2 + M_PI_4, res)) {
                        face = RIGHT;
                        theta_norm = theta - M_PI_2;
                    } else {
                        face = BACK;
                        theta_norm = theta + ((theta > 0) ? -M_PI : M_PI);
                    }

                    phi_threshold = atan2(1., 1. / cos(theta_norm));
                    if (phi > phi_threshold) {
                        face = DOWN;
                    } else if (phi < -phi_threshold) {
                        face = TOP;
                    } else {
                        ;
                    }

                    switch (face) {
                    case LEFT:
                        locate(z, x, y, M_PI,   rw, rh, &ox, &oy);
                        break;
                    case FRONT:
                        locate(x, z, y, 0.,     rw, rh, &ox, &oy);
                        break;
                    case RIGHT:
                        locate(z, y, x, M_PI_2, rw, rh, &ox, &oy);
                        break;
                    case TOP:
                        locate(y, z, x, M_PI,   rw, rh, &ox, &oy);
                        oy += height/2;
                        break;
                    case BACK:
                        locate(x, y, z,-M_PI_2, rw, rh, &ox, &oy);
                        oy += height/2;
                        break;
                    case DOWN:
                        locate(y, x, z,-M_PI_2, rw, rh, &ox, &oy);
                        oy += height/2;
                        break;
                    }

                    ox += (in_width / 3) * (face % 3);
                    r->vi = oy;
                    r->ui = ox;
                }
            }
        }
        s->panorama = nearest;
    }

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext *ctx = inlink->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    PanoramaContext *s = ctx->priv;
    AVFrame *out;
    int plane;

    if (s->in == s->out)
        return ff_filter_frame(outlink, in);

    out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    if (!out) {
        av_frame_free(&in);
        return AVERROR(ENOMEM);
    }
    av_frame_copy_props(out, in);

    for (plane = 0; plane < s->nb_planes; plane++) {
        s->panorama(s, in->data[plane], out->data[plane],
                    s->planewidth[plane], s->planeheight[plane],
                    in->linesize[plane], out->linesize[plane],
                    s->remap[plane]);
    }

    av_frame_free(&in);
    return ff_filter_frame(outlink, out);
}

static av_cold void uninit(AVFilterContext *ctx)
{
    PanoramaContext *s = ctx->priv;
    int p;

    for (p = 0; p < s->nb_planes; p++)
        av_freep(&s->remap[p]);
}

static const AVFilterPad inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
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

AVFilter ff_vf_panorama = {
    .name          = "panorama",
    .description   = NULL_IF_CONFIG_SMALL("Convert panorama projection of video."),
    .priv_size     = sizeof(PanoramaContext),
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = inputs,
    .outputs       = outputs,
    .priv_class    = &panorama_class,
    .flags         = AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC,
};
