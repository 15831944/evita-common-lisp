;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - sin
;;; arch/generic/lisp/macth/gen-math-64-sin.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-sin.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-sin
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* @(#)s_sin.c 5.1 93/09/24 */
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

/*
FUNCTION
        <<sin>>, <<sinf>>, <<cos>>, <<cosf>>---sine or cosine
INDEX
sin
INDEX
sinf
INDEX
cos
INDEX
cosf
ANSI_SYNOPSIS
        #include <math.h>
        double sin(double <[x]>);
        float  sinf(float <[x]>);
        double cos(double <[x]>);
        float cosf(float <[x]>);

TRAD_SYNOPSIS
        #include <math.h>
        double sin(<[x]>)
        double <[x]>;
        float  sinf(<[x]>)
        float <[x]>;

        double cos(<[x]>)
        double <[x]>;
        float cosf(<[x]>)
        float <[x]>;

DESCRIPTION
        <<sin>> and <<cos>> compute (respectively) the sine and cosine
        of the argument <[x]>.  Angles are specified in radians. 

        <<sinf>> and <<cosf>> are identical, save that they take and
        return <<float>> values. 


RETURNS
        The sine or cosine of <[x]> is returned.

PORTABILITY
        <<sin>> and <<cos>> are ANSI C. 
        <<sinf>> and <<cosf>> are extensions.

QUICKREF
        sin ansi pure
        sinf - pure
*/

/* sin(x)
 * Return sine function of x.
 *
 * kernel function:
 *      __kernel_sin                ... sine function on [-pi/4,pi/4]
 *      __kernel_cos                ... cose function on [-pi/4,pi/4]
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
(defun float64-sin (x)
    (declare (values double-float))
    (declare (type double-float x))
  (let* ((hx (decode-float64 x))
         (ix (logand hx #x7fffffff)) )
    (cond
      ;; |x| ~< pi/4 
      ((<= ix #x3fe921fb)
        (float64-kernel-sin x 0d0 0) )

      ;; sin(Inf or NaN) is NaN
      ((>= ix #x7ff00000)
        (- x x) )

      ;; argument reduction needed
      (t
        (multiple-value-bind (n y0 y1) (float64-rem-pio2 x)
          (ecase (logand n 3)
            (0 (float64-kernel-sin y0 y1 1))
            (1 (float64-kernel-cos y0 y1))
            (2 (- (float64-kernel-sin y0 y1 1)))
            (3 (- (float64-kernel-cos y0 y1))) ) ) )) ) )
