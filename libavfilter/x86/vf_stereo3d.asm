;*****************************************************************************
;* x86-optimized functions for stereo3d filter
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

%if ARCH_X86_64

SECTION_RODATA

; rgbrgbrgbrgb
; rrrrggggbbbb

shuf: db 0,3,6,9,1,4,7,10,2,5,8,11,-1,-1,-1,-1
;rshuf: db 0,4,8,1,5,9,2,6,10,3,7,11,-1,-1,-1,-1
rshuf: db 0,-1,-1,1,-1,-1,2,-1,-1,3,-1,-1,-1,-1,-1,-1
ex_r: db 0,-1,-1,-1,3,-1,-1,-1,6,-1,-1,-1,9,-1,-1,-1
ex_g: db 1,-1,-1,-1,4,-1,-1,-1,7,-1,-1,-1,10,-1,-1,-1
ex_b: db 2,-1,-1,-1,5,-1,-1,-1,8,-1,-1,-1,11,-1,-1,-1

SECTION .text

INIT_XMM sse4
cglobal anaglyph, 11, 13, 20, 0, dst, lsrc, rsrc, dst_linesize, l_linesize, r_linesize, width, height, ana_matrix_r, ana_matrix_g, ana_matrix_b
.nextrow:
    mov       r11q, widthq
    mov       r12q, 0
    movd        m8, [ana_matrix_rd+0]
    movd        m9, [ana_matrix_rd+1]
    movd       m10, [ana_matrix_rd+2]
    %define      o  r12q

    .loop:
        movu                 m0, [lsrcq+o+0]
        pshufb               m1, m0, [ex_r]
        pshufb               m2, m0, [ex_g]
        pshufb               m3, m0, [ex_b]
        pmulld               m1, m8
        pmulld               m2, m9
        pmulld               m3, m10
        paddd                m1, m2
        paddd                m3, m1
        psrld                m3, 16
        packusdw             m3, m3
        packuswb             m3, m3
        pshufb               m3, m3, [rshuf]
        movu         [dstq+o+0], m3
        add                r12d, 12
        sub                r11d, 4
    jg .loop

    add          dstq, dst_linesizeq
    add         lsrcq, l_linesizeq
    add         rsrcq, r_linesizeq
    sub       heightd, 1
    jg .nextrow
REP_RET
%endif
