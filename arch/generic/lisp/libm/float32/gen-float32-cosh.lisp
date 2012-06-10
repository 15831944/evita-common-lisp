;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 cosh
;;; arch/generic/lisp/macth/gen-math-f32-cosh.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-cosh.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-cosh
;
(in-package :si)

#|
 From fdlibm (http://www.netlib.org/fdlibm/)

/* ef_cosh.c -- float version of e_cosh.c.
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 */

/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 */
|#
(defun float32-cosh (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (one 1f0)
    (half 0.5f0)
    (huge 1.0f30)
    ;;
    (jx (decode-float32 x))
    (ix (logand jx #x7fffffff))
    )

    ;; x is INF or NaN
    (when (>= ix #x7FC00000)
      (return (* x x)) )

    ;; |x| in [0,0.5*ln2], return 1+expm1(|x|)^2/(2*exp(|x|))
    (when (< ix #x3eb17218)
      (let* ((tt (float32-expm1 (float32-abs x)))
             (w  (+ one tt)) )
        (return (if (< ix #x24000000)
                w ;; cosh(tiny) = 1
            (+ one (/ (* tt tt) (+ w w))) )) ))

    ;; |x| in [0.5*ln2,22], return (exp(|x|)+1)/exp(|x|)/2;
    (when (< ix #x41b00000)
      (let ((tt (float32-exp (float32-abs x))))
        (return (+ (* half tt) (/ half tt))) ))

    ;; |x| in [22, log(maxdouble)] return half*exp(|x|)
    ;; FLT_UWORD_LOG_MAX 0x42b17217
    (when (<= ix #x42b17217)
      (return (* half (float32-exp (float32-abs x)))) )

    ;; |x| in [log(maxdouble), overflowthresold]
    ;; FLT_UWORD_LOG_2MAX 0x42b2d4fc
    (when (<= ix #x42b2d4fc)
      (let* ((w  (float32-exp (* half (float32-abs x))))
             (tt (* half w)) )
        (return (* tt w)) ))

    ;; |x| > overflowthresold, cosh(x) overflow
    (return (* huge huge)) ) )
