;******************************************************************************
;* x86-optimized functions for transpose filter
;*
;* This file is part of FFmpeg.
;*
;* FFmpeg is free software; you can redistribute it and/or
;* modify it under the terms of the GNU Lesser General Public
;* License as published by the Free Software Foundation; either
;* version 2.1 of the License, or (at your option) any later version.
;*
;* FFmpeg is distributed in the hope that it will be useful,
;* but WITHOUT ANY WARRANTY; without even the implied warranty of
;* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;* Lesser General Public License for more details.
;*
;* You should have received a copy of the GNU Lesser General Public
;* License along with FFmpeg; if not, write to the Free Software
;* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;******************************************************************************

%include "libavutil/x86/x86util.asm"

SECTION .text

INIT_XMM sse2
cglobal transpose_filter_byte, 4, 5, 8, src, srcstride, dst, dststride, tmp1
    lea tmp1q, [srcq + srcstrideq]
    movh m0, [srcq]
    movh m1, [tmp1q]
    movh m2, [srcq + 2*srcstrideq]
    movh m3, [tmp1q + 2*srcstrideq]
    lea srcq, [srcq + 4*srcstrideq]
    lea tmp1q, [tmp1q + 4*srcstrideq]
    punpcklbw m0, m1
    punpcklbw m2, m3
    movh m4, [srcq]
    movh m5, [tmp1q]
    movh m6, [srcq + 2*srcstrideq]
    movh m7, [tmp1q + 2*srcstrideq]
    punpcklbw m4, m5
    punpcklbw m6, m7
    SBUTTERFLY wd, 0, 2, 1
    SBUTTERFLY wd, 4, 6, 5
    SBUTTERFLY dq, 0, 4, 2
    SBUTTERFLY dq, 1, 5, 3
    lea tmp1q, [dstq + dststrideq]
    movh [dstq], m0
    movhps [tmp1q], m0
    movh [dstq + 2*dststrideq], m2
    movhps [tmp1q + 2*dststrideq], m2
    lea dstq, [dstq + 4*dststrideq]
    lea tmp1q, [tmp1q + 4*dststrideq]
    movh [dstq], m1
    movhps [tmp1q], m1
    movh [dstq + 2*dststrideq], m3
    movhps [tmp1q + 2*dststrideq], m3
    RET
