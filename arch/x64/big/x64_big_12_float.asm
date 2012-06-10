;;;;
;;; evcl - big - 12 Number - Float
;;; arch/x64/big/x64_big_12_float.asm
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x64/kernel/x64_ke.asm#6 $
;;;
;;; Description:
;;;  This file contains implementation of
;;;     ceil        ceilf
;;;     floor       floorf
;;;     rint        rintf
;;;     truncate    truncatef
;;;  by using x87 instructions.
;
_text segment

macro_div MACRO mp_mov, mp_size, mp_cw
    mp_mov  mp_size ptr [esp-16], xmm0
    fld     mp_size ptr [esp-16]
    fnstcw  [esp-2]
    mov     ax, [esp-2]
    or      ax, mp_cw
    mov     [esp-4], ax
    fldcw   [esp-4]
    frndint
    fldcw   [esp-2]
    fstp    mp_size ptr [esp-16]
    mp_mov  xmm0, mp_size ptr [esp-16]
    ret
ENDM

PUBLIC ceilf
ceilf PROC
    macro_div movss, dword, 0800h
ceilf ENDP

PUBLIC ceil
ceil PROC
    macro_div movsd, qword, 0800h
ceil ENDP

PUBLIC floorf
floorf PROC
    macro_div movss, dword, 0800h
floorf ENDP

PUBLIC floor
floor PROC
    macro_div movsd, qword, 0800h
floor ENDP

PUBLIC rintf
rintf PROC
    movss   dword ptr [esp-16], xmm0
    fld     dword ptr [esp-16]
    frndint
    fstp    dword ptr [esp-16]
    movss   xmm0, dword ptr [esp-16]
    ret
rintf ENDP

PUBLIC rint
rint PROC
    movsd   qword ptr [esp-16], xmm0
    fld     qword ptr [esp-16]
    frndint
    fstp    qword ptr [esp-16]
    movsd   xmm0, qword ptr [esp-16]
    ret
rint ENDP

PUBLIC truncatef
truncatef PROC
    macro_div movss, dword, 0C00h
truncatef ENDP

PUBLIC truncate
truncate PROC
    macro_div movsd, qword, 0C00h
truncate ENDP

_text ends
end
