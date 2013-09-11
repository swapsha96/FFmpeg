/*
 * Copyright (c) 2010 Stefano Sabatini
 * Copyright (c) 2008 Vitor Sessak
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

#ifndef AVFILTER_VF_TRANSPOSE_H
#define AVFILTER_VF_TRANSPOSE_H

#include <stddef.h>
#include <stdint.h>

#include "libavutil/opt.h"

typedef enum {
    TRANSPOSE_PT_TYPE_NONE,
    TRANSPOSE_PT_TYPE_LANDSCAPE,
    TRANSPOSE_PT_TYPE_PORTRAIT,
} PassthroughType;

enum TransposeDir {
    TRANSPOSE_CCLOCK_FLIP,
    TRANSPOSE_CLOCK,
    TRANSPOSE_CCLOCK,
    TRANSPOSE_CLOCK_FLIP,
};

typedef struct TransContext {
    const AVClass *class;
    int hsub, vsub;
    int pixsteps[4];

    PassthroughType passthrough; ///< landscape passthrough mode enabled
    enum TransposeDir dir;

    void (*transpose_block)(void *src, int src_linesize,
                            void *dst, int dst_linesize);
} TransContext;

void ff_transpose_init_x86(TransContext *trans);

#endif /* AVFILTER_VF_TRANSPOSE */
