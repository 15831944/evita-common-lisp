;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - tanh
;;; arch/generic/lisp/float/float64/gen-float64-tanh.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-tanh.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-tanh
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* @(#)s_tanh.c 5.1 93/09/24 */
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
        <<tanh>>, <<tanhf>>---hyperbolic tangent

INDEX
tanh
INDEX
tanhf

ANSI_SYNOPSIS
        #include <math.h>
        double tanh(double <[x]>);
        float tanhf(float <[x]>);

TRAD_SYNOPSIS
        #include <math.h>
        double tanh(<[x]>)
        double <[x]>;

        float tanhf(<[x]>)
        float <[x]>;


DESCRIPTION

<<tanh>> computes the hyperbolic tangent of
the argument <[x]>.  Angles are specified in radians.  

<<tanh(<[x]>)>> is defined as 
. sinh(<[x]>)/cosh(<[x]>)
        
<<tanhf>> is identical, save that it takes and returns <<float>> values.

RETURNS
The hyperbolic tangent of <[x]> is returned.

PORTABILITY
<<tanh>> is ANSI C.  <<tanhf>> is an extension.

*/

/* Tanh(x)
 * Return the Hyperbolic Tangent of x
 *
 * Method :
 *                                     x    -x
 *                                    e  - e
 *      0. tanh(x) is defined to be -----------
 *                                     x    -x
 *                                    e  + e
 *      1. reduce x to non-negative by tanh(-x) = -tanh(x).
 *      2.  0      <= x <= 2**-55 : tanh(x) := x*(one+x)
 *                                              -t
 *          2**-55 <  x <=  1     : tanh(x) := -----; t = expm1(-2x)
 *                                             t + 2
 *                                                   2
 *          1      <= x <=  22.0  : tanh(x) := 1-  ----- ; t=expm1(2x)
 *                                                 t + 2
 *          22.0   <  x <= INF    : tanh(x) := 1.
 *
 * Special cases:
 *      tanh(NaN) is NaN;
 *      only tanh(0)=0 is exact for finite argument.
 */
|#
(defun float64-tanh (x)
    (declare (values double-float))
    (declare (type double-float x))
  (let* (
    (one   1.0d0)
    (two   2.0d0)
    (tiny  1.0d-300)
    ;;
    (hx (decode-float64 x))
    (ix (logand hx #x7fffffff))
    )

    (cond
      ;; x is INF or NaN
      ((>= ix #x7ff00000) 
        (if (>= hx 0)
            (/ one (+ x one))   ; tanh(+-inf)=+-1
          (/ one (- x one)) ) ) ; tanh(NaN) = NaN

      ;; |x| < 22
      ((< ix #x40360000)        ; |x|<22
        (cond
          ((< ix #x3c800000)    ; |x|<2**-55
            (* x (+ one x)) )   ; tanh(small) = small
          ((>= ix #x3ff00000)   ; |x|>=1
            (let* ((tt (float64-expm1 (* two (float64-abs x))))
                   (z  (- one (/ two (+ tt two)))) )
              (if (>= hx 0) z (- z)) ) )
          (t
            (let* ((tt (float64-expm1 (* (- two) (float64-abs x))))
                   (z  (/ (- tt) (+ tt two))) )
              (if (>= hx 0) z (- z)) ) )) )
      ;; |x| > 22, return +-1
      (t
         (let ((z (- one tiny)))    ; raised inexact flag
           (if (>= hx 0) z (- z)) ) )) ) )
