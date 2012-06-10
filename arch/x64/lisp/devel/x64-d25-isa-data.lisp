;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x64 - devel - Disassembler
;;; arch/x64/lisp/devel/asm/x64-d25-asm.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x64/lisp/devel/x64-d25-isa-data.lisp#1 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :x64)

(define-isa :x64 (:size 64)
  :registers (
    (:gpr 8 #(al  cl  dl   bl   ah   ch   dh   bh
             ;; When REX prefix specified, x64 use another encoding of
             ;; 8bit register set.
             al  cl  dl   bl   spl  bpl  sil  dil
             r8l r9l r10l r11l r12l r13l r14l r15l))
    (:gpr 16 #(ax  cx  dx   bx   sp   bp   si   di
             r8w r9w r10w r11w r12w r13w r14w r15w ))
    (:gpr 32 #(eax ecx edx  ebx  esp  ebp  esi edi
              r8d r9d r10d r11d r12d r13d r14d r15d ))
    (:gpr 64 #(rax rcx rdx rbx rsp rbp rsi rdi
              r8  r9  r10 r11 r12 r13 r14 r15 ))
    (:fpr 80 #(st0 st1 st2 st3 st4 st5 st6 st7))
    (:mmx 64 #(mm0 mm1 mm2 mm3 mm4 mm5 mm6 mm7))
    (:xmm 128 #(xmm0 xmm1 xmm2  xmm3  xmm4  xmm5  xmm6  xmm7
               xmm8 xmm9 xmm10 xmm11 xmm12 xmm13 xmm14 xmm15 ))
    (:idx  16 #(ax*1 cx*1 dx*1 bx*1 nil nil si*1 di*1
                ax*2 cx*2 dx*2 bx*2 nil nil si*2 di*2
                ax*4 cx*4 dx*4 bx*4 nil nil si*4 di*4
                ax*8 cx*8 dx*8 bx*8 nil nil si*8 di*8 ))
    (:idx  32 #(eax*1 ecx*1 edx*1 bx*1 nil nil esi*1 edi*1
                eax*2 ecx*2 edx*2 bx*2 nil nil esi*2 edi*2
                eax*4 ecx*4 edx*4 bx*4 nil nil esi*4 edi*4
                eax*8 ecx*8 edx*8 bx*8 nil nil esi*8 edi*8 ))
    (:idx  64 #(rax*1 rcx*1 rdx*1 bx*1  nil   nil   rsi*1 rdi*1
                r8*1  r9*1  r10*1 r11*1 r12*1 r13*1 r14*1 r15*1
                rax*2 rcx*2 rdx*2 bx*2  nil   nil   rsi*2 rdi*2
                r8*2  r9*2  r10*2 r11*2 r12*2 r13*2 r14*2 r15*2
                rax*4 rcx*4 rdx*4 bx*4  nil   nil   rsi*4 rdi*4
                r8*4  r9*4  r10*4 r11*4 r12*4 r13*4 r14*4 r15*4
                rax*8 rcx*8 rdx*8 bx*8  nil   nil   rsi*8 rdi*8
                r8*8  r9*8  r10*8 r11*8 r12*8 r13*8 r14*8 r15*8 ))
    )

    ;; For assembler
    :callee-saves ()
    :caller-saves (
        rax rcx rdx rbx  rsi rdi
        r8  r9  r10 r11  r14 r15
        xmm0 xmm1 xmm2  xmm3  xmm4  xmm5  xmm6  xmm7
        xmm8 xmm9 xmm10 xmm11 xmm12 xmm13 xmm14 xmm15
      )

    :aliases (
        ($r0 rax) ($r1 rdx) ($r2 rbx) ($r3 rsi) ($r4 rdi)
        ($rn rcx)
        ($rfn  r12)
        ($rnil r13)
        ($rtcb rbp)
        ($rsp  rsp)
      )

    ;; For disassembler
    :tlvoffset #x8B8
    :thfields (
        (  0 8 "m_classd")
        (  8 8 "m_name")
        ( 16 8 "m_waiting")
        ( 24 8 "m_next_waiter")
        ( 32 8 "m_pConsArea")
        ( 40 8 "m_pRecordArea")
        ( 48 8 "m_pBinObjArea")
        ( 56 8 "m_pFunObjArea")
        ( 64 8 "m_pObStackArea")
        ( 72 8 "m_pNextThread")
        ( 80 8 "m_pPrevThread")
        ( 88 8 "m_fp")
        ( 96 8 "m_fn")
        (104 8 "m_n")
        (112 512 "mv_value")

        (#.(* -13 8) 8  "SVC_stack_alloc")
        (#.(* -12 8) 8  "SVC_alloc_bino_area")
        (#.(* -11 8) 8  "SVC_alloc_code_area")
        (#.(* -10 8) 8  "SVC_alloc_cons_area")
        (#.(* -9  8) 8  "SVC_alloc_reco_area")
        (#.(* -8  8) 8  "SVC_alloc_symb_area")

        (#.(*  -7 8) 8  "SVC_arity_error")
        (#.(*  -6 8) 8  "SVC_type_error")
        (#.(*  -5 8) 8  "SVC_not_function")
        (#.(*  -4 8) 8  "SVC_unbound_variable")
        (#.(*  -3 8) 8  "SVC_undefined_function")
        (#.(*  -2 8) 8  "SVC_error")
        (#.(*  -1 8) 8  "SVC_fixnum_one")
  ) )
