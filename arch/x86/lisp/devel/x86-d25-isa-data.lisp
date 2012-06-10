;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x86 - devel - Disassembler
;;; arch/x86/lisp/devel/asm/x86-d25-asm.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86/lisp/devel/x86-d25-isa-data.lisp#1 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :x86)

(define-isa :x86 (:size 32)
    :registers (
        (:gpr   8 #(al  cl  dl  bl  ah  ch  dh  bh))
        (:gpr  16 #(ax  cx  dx  bx  sp  bp  si  di))
        (:gpr  32 #(eax ecx edx ebx esp ebp esi edi))
        (:fpr  80 #(st0 st1 st2 st3 st4 st5 st6 st7))
        (:mmx  64 #(mm0 mm1 mm2 mm3 mm4 mm5 mm6 mm7))
        (:xmm 128 #(xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7))
        (:idx  16 #(ax*1 cx*1 dx*1 bx*1 nil nil si*1 di*1
                    ax*2 cx*2 dx*2 bx*2 nil nil si*2 di*2
                    ax*4 cx*4 dx*4 bx*4 nil nil si*4 di*4
                    ax*8 cx*8 dx*8 bx*8 nil nil si*8 di*8 ))
        (:idx  32 #(eax*1 ecx*1 edx*1 bx*1 nil nil esi*1 edi*1
                    eax*2 ecx*2 edx*2 bx*2 nil nil esi*2 edi*2
                    eax*4 ecx*4 edx*4 bx*4 nil nil esi*4 edi*4
                    eax*8 ecx*8 edx*8 bx*8 nil nil esi*8 edi*8 ))
      )

    ;; For assembler
    :callee-saves ()
    :caller-saves (
        eax ecx edx ebx esi edi
        xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7
      )

    :aliases (
        ($r0 eax) ($r1 edx) ($r2 ebx) ($r3 esi) ($r4 edi)
        ($rn ecx)
        ($rtcb ebp)
        ($rsp esp)
      )

    ;; For disassembler
    :tlvoffset #x45C
    :thfields (
        (  0 4 "m_classd")
        (  4 4 "m_name")
        (  8 4 "m_waiting")
        ( 12 4 "m_next_waiter")
        ( 16 4 "m_pConsArea")
        ( 20 4 "m_pRecordArea")
        ( 24 4 "m_pBinObjArea")
        ( 28 4 "m_pFunObjArea")
        ( 32 4 "m_pObStackArea")
        ( 36 4 "m_pNextThread")
        ( 40 4 "m_pPrevThread")
        ( 44 4 "m_fp")
        ( 48 4 "m_fn")
        ( 52 4 "m_n")
        ( 56 512 "mv_value")

        (-124 4 "SVC_stack_alloc")
        (-120 4 "SVC_alloc_bino_area")
        (-116 4 "SVC_alloc_code_area")
        (-112 4 "SVC_alloc_cons_area")
        (-108 4 "SVC_alloc_reco_area")
        (-104 4 "SVC_alloc_symb_area")
        (-100 4 "SVC_arity_error")
        (-96  4 "SVC_type_error")

        (-68  4 "SVC_not_function")
        (-64  4 "SVC_unbound_variable")
        (-60  4 "SVC_undefined_function")
        (-56  4 "SVC_error")

        (-4   4 "SVC_fixnum_one")
    ) )
