;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 sinh
;;; arch/generic/lisp/macth/gen-math-f32-sinh.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-sinh.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-sinh
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
/* ef_sinh.c -- float version of e_sinh.c.
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
(defun float32-sinh (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (one 1f0)
    (shuge 1.0f37)
    ;;
    (jx (decode-float32 x))
    (ix (logand jx #x7fffffff))
    )
    ;;
    ;; x is INF or NaN
    (when (>= ix #x7FC00000)
      (return (+ x x)) )

    (let ((h (if (< jx 0) -0.5f0 0.5f0)))

        ;; |x| in [0,22], return sign(x)*0.5*(E+E/(E+1))
        (when (< ix #x41b00000)
          ;; |x|<22
          (when (< ix #x31800000)
            ;; |x|<2**-28
            (when (> (+ shuge x) one)
               ;; sinh(tiny) = tiny with inexact
               (return x) ))
          (let ((tt (float32-expm1 (float32-abs x))))
            (if (< ix #x3f800000)
                (return (* h (- (* 2 tt) (/ (* tt tt) (+ tt one)))))
              (return (* h (+ tt (/ tt (+ tt one))))) ) ))

        ;; |x| in [22, log(maxdouble)] return 0.5*exp(|x|)
        ;; FLT_UWORD_LOG_MAX 0x42b17217
        (when (<= ix #x42b17217)
          (return (* h (float32-exp (float32-abs x)))) )

        ;; |x| in [log(maxdouble), overflowthresold]
        ;; FLT_UWORD_LOG_2MAX 0x42b2d4fc
        (when (<= ix #x42b2d4fc)
          (let* ((w  (float32-exp (* 0.5f0 (float32-abs x))))
                 (tt (* h w)) )
            (return (* tt w)) ))

        ;; |x| > overflowthresold, sinh(x) overflow
        (return (* x shuge)) ) ) )
