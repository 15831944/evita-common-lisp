;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64 - tan
;;; arch/generic/lisp/float/float64/gen-float64-tan.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-tan.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-tan
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* @(#)s_tan.c 5.1 93/09/24 */
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
        <<tan>>, <<tanf>>---tangent

INDEX
tan
INDEX
tanf

ANSI_SYNOPSIS
        #include <math.h>
        double tan(double <[x]>);
        float tanf(float <[x]>);

TRAD_SYNOPSIS
        #include <math.h>
        double tan(<[x]>)
        double <[x]>;

        float tanf(<[x]>)
        float <[x]>;


DESCRIPTION
<<tan>> computes the tangent of the argument <[x]>.  
Angles are specified in radians.  

<<tanf>> is identical, save that it takes and returns <<float>> values.

RETURNS
The tangent of <[x]> is returned. 

PORTABILITY
<<tan>> is ANSI. <<tanf>> is an extension.
*/

/* tan(x)
 * Return tangent function of x.
 *
 * kernel function:
 *      __kernel_tan                ... tangent function on [-pi/4,pi/4]
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
(defun float64-tan (x)
    (declare (values double-float))
    (declare (type double-float x))
  (let* ((hx (decode-float64 x))
         (ix (logand hx #x7fffffff)) )
    (cond
      ;; |x| ~< pi/4
      ((<= ix #x3fe921fb)
        (float64-kernel-tan x 0d0 1) )

      ;; tan(Inf or NaN) is NaN 
      ((>= ix #x7ff00000)
        (- x x) )

      ;; argument reduction needed
      (t
        (multiple-value-bind (n y0 y1) (float64-rem-pio2 x)
            ;; 1 .. n is even, -1 ... n is odd
          (float64-kernel-tan y0 y1 (- 1 (ash (logand n 1) 1))) ) )) ) )
