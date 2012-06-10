;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: X86x64; Base: 10 -*-
;;;;
;;;; evcl - X86/X64 Assembler - Declarations
;;; arch/x86x64/lisp/devel/asm/x86x64-asm-pseudo.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/asm/x86x64-asm-pseudo.lisp#1 $
;;;
;;; Description:
;;;  This fils contains pseudo instructions of x86-x64 assembler.
;;;
;;; Note:We don't support 16-bit addressing mode.
;
(in-package :x86x64)

;;;; align k
(define-pseudo-instruction align (k)
  (let ((bb  (slot-value ctx 'bblock))
        (adr (slot-value ctx 'address)) )
    (if (eql (slot-value bb 'address) adr)
        (setf (slot-value bb 'kind) :align
              (slot-value bb 'align) k )
      (progn
        (setq bb (make-instance 'asm-bblock
                      :address    adr
                      :align      k
                      :kind       :align
                      :offset     (slot-value ctx 'offset) ))
        (push bb (slot-value ctx 'bblocks))
        (setf (slot-value ctx 'bblock) bb) ))
    (let ((rem (rem adr k)))
      (unless (eql rem 0)
        (incf adr (- k rem))
        (setf (slot-value bb  'address) adr)
        (setf (slot-value ctx 'address) adr) ) ) ) )


;;;; class
(define-pseudo-instruction class (class-name)
  (let ((class (find-class class-name nil)))
    (cond
      ((null class)
        (asm-error ctx "No such class ~S." class-name) )
      ((not (subtypep class 'function))
        (asm-error ctx "Class ~S isn't subclass of function." class-name) ))
    (setf (slot-value ctx 'class) class-name) ) )


;;;; db
(define-pseudo-instruction db (x &rest x*)
  (labels (
    (emit (x)
      (cond
        ((not (integerp x))
          (asm-error ctx "Expect integer ~S" x) )
        ((<= (integer-length x) 8)
          (asm-emit-u8 ctx (ldb (byte 8 0) x)) )
        (t
          (asm-error ctx "Expect 8bit value ~S" x) )) )
    )
  (emit x)
  (dolist (x x*) (emit x)) ) )


;;;; dw
(define-pseudo-instruction dw (x &rest x*)
  (labels (
    (emit (x)
      (cond
        ((characterp x)
          (emit-u16 (char-code x)) )
        ((not (integerp x))
          (asm-error ctx "Expect integer ~S" x) )
        ((<= (integer-length x) 16)
          (emit-u16 (ldb (byte 16 0) x)) )
        (t
          (asm-error ctx "Expect 16bit value ~S" x) )) )

    (emit-u16 (x)
      (asm-emit-u8 ctx (ldb (byte 8 0) x))
      (asm-emit-u8 ctx (ldb (byte 8 8) x)) )
    )
  (emit x)
  (dolist (x x*) (emit x)) ) )


;;;; dd
(define-pseudo-instruction dd (x &rest x*)
  (labels (
    (emit (x)
      (cond
        ((typep x 'single-float)
          (emit (si::decode-float32 x)) )
        ((not (integerp x))
          (asm-error ctx "Expect integer ~S" x) )
        ((<= (integer-length x) 16)
          (asm-emit-u8 ctx (ldb (byte 8  0) x))
          (asm-emit-u8 ctx (ldb (byte 8  8) x))
          (asm-emit-u8 ctx (ldb (byte 8 16) x))
          (asm-emit-u8 ctx (ldb (byte 8 24) x)) )
        (t
          (asm-error ctx "Expect 32bit value ~S" x) )) )
    )
  (emit x)
  (dolist (x x*) (emit x)) ) )


;;;; dq
(define-pseudo-instruction dq (x &rest x*)
  (labels (
    (emit (x)
      (cond
        ((typep x 'double-float)
          (multiple-value-bind (hx lx) (si::decode-float64 x)
            (emit-u32 lx)
            (emit-u32 hx) ) )
        ((not (integerp x))
          (asm-error ctx "Expect integer ~S" x) )
        ((<= (integer-length x) 64)
          (emit-u32 (ldb (byte 32  0) x))
          (emit-u32 (ldb (byte 32 32) x)) )
        (t
          (asm-error ctx "Expect 32bit value ~S" x) )) )

    (emit-u32 (x)
      (asm-emit-u8 ctx (ldb (byte 8  0) x))
      (asm-emit-u8 ctx (ldb (byte 8  8) x))
      (asm-emit-u8 ctx (ldb (byte 8 16) x))
      (asm-emit-u8 ctx (ldb (byte 8 24) x)) )
    )
  (emit x)
  (dolist (x x*) (emit x)) ) )


;;;; frame size kind
(define-pseudo-instruction frame (size &optional (kind :fixed))
  (setf (slot-value ctx 'frame-size) size)
  (setf (slot-value ctx 'frame-type) kind) )
