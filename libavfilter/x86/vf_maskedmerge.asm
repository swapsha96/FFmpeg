;*****************************************************************************
;* x86-optimized functions for maskedmerge filter
;*
;* Copyright (C) 2015 Paul B Mahol
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
;*****************************************************************************

%include "libavutil/x86/x86util.asm"

SECTION .text

INIT_XMM sse2
cglobal maskedmerge8, 10, 14, 3, 0, bsrc, blinesize, osrc, olinesize, msrc, mlinesize, dst, dlinesize, w, h
.nextrow:

    lea bsrcq, [bsrcq+blinesizeq]
    lea osrcq, [osrcq+olinesizeq]
    lea msrcq, [msrcq+mlinesizeq]
    lea dstq, [dstq+dlinesizeq]
    sub hd, 1
    jg .nextrow
RET
