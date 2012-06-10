;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - cos
;;; arch/generic/lisp/float/gen-float-f64-cos.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-cos.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-cos
;
(in-package :si)

#|
 *  From fdlibm (http://www.netlib.org/fdlibm/)
/* @(#)s_cos.c 5.1 93/09/24 */
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

/* cos(x)
 * Return cosine function of x.
 *
 * kernel function:
 *      __kernel_sin                ... sine function on [-pi/4,pi/4]
 *      __kernel_cos                ... cosine function on [-pi/4,pi/4]
 *      __ieee754_rem_pio2        ... argument reduction routine
 *
 * Method.
 *      Let S,C and T denote the sin, cos and tan respectively on 
 *      [-PI/4, +PI/4]. Reduce the argument x to y1+y2 = x-k*pi/2 
 *      in [-pi/4 , +pi/4], and let n = k mod 4.
 *      We have
 *
 *          n        sin(x)      cos(x)        tan(x)
 *     ----------------------------------------------------------
 *          0               S           C                 T
 *          1               C          -S                -1/T
 *          2              -S          -C                 T
 *          3              -C           S                -1/T
 *     ----------------------------------------------------------
 *
 * Special cases:
 *      Let trig be any of sin, cos, or tan.
 *      trig(+-INF)  is NaN, with signals;
 *      trig(NaN)    is that NaN;
 *
 * Accuracy:
 *      TRIG(x) returns trig(x) nearly rounded 
 */
|#
(defun float64-cos (x)
    (declare (values double-float))
    (declare (type double-float x))
  (let ((hx (logand (decode-float64 x) #x7fffffff)))
    (cond
      ;; |x| ~< pi/4
      ((<= hx #x3fe921fb)
        (float64-kernel-cos x 0d0) )

      ;; cos(Inf or NaN) is NaN
      ((>= hx #x7ff00000)
        (- x x) )

      ;; argument reduction needed
      (t
        (multiple-value-bind (n y0 y1) (float64-rem-pio2 x)
          (ecase (logand n 3)
            (0 (float64-kernel-cos y0 y1))
            (1 (- (float64-kernel-sin y0 y1 1)))
            (2 (- (float64-kernel-cos y0 y1)))
            (3 (float64-kernel-sin y0 y1 1)) ) ) )) ) )
