;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - log
;;; arch/generic/lisp/macth/gen-math-64-log.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-sinh.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-log
;
(in-package :si)

#|
 * @(#)e_sinh.c 5.1 93/09/24
 *
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 *

 * __ieee754_sinh(x)
 * Method : 
 * mathematically sinh(x) if defined to be (exp(x)-exp(-x))/2
 *      1. Replace x by |x| (sinh(-x) = -sinh(x)). 
 *      2. 
 *                                                  E + E/(E+1)
 *          0        <= x <= 22     :  sinh(x) := --------------, E=expm1(x)
 *                                                             2
 *
 *          22       <= x <= lnovft :  sinh(x) := exp(x)/2 
 *          lnovft   <= x <= ln2ovft:  sinh(x) := exp(x/2)/2 * exp(x/2)
 *          ln2ovft  <  x            :  sinh(x) := x*shuge (overflow)
 *
 * Special cases:
 *      sinh(x) is |x| if x is +INF, -INF, or NaN.
 *      only sinh(0)=0 is exact for finite x.
 *
|#
(defun float64-sinh (x)
    (declare (values double-float))
    (declare (type double-float x))
  (labels ((uint32 (x) (if (>= x 0) x (+ x #.(ash 1 32)))))
  (prog
    ((one    1.0d0)
     (shuge  1.0d307) )
  (multiple-value-bind (jx lx) (decode-float64 x)
  (let ((ix (logand jx #x7fffffff))) ; High word of |x|.
    ;; x is INF or NaN
    (when (>= ix #x7ff00000) (return (+ x x)))
    (let ((h (if (minusp jx) -0.5d0 0.5d0)))
      ;; |x| in [0,22], return sign(x)*0.5*(E+E/(E+1))
      (when (< ix #x40360000)   ; |x|<22
        (when (< ix #x3e300000) ; |x|<2**-28
          ;; sinh(tiny) = tiny with inexact
          (when (> (+ shuge x) one) (return x))
          (let ((s (float64-expm1 (float64-abs x))))
            (return (if (< ix #x3ff00000)
                (* h (- (* 2.0d0 s) (/ (* s s) (+ s one))))
                (* h (+ s (/ s (+ s one)))) )) )))

      ;; |x| in [22, log(maxdouble)] return 0.5*exp(|x|)
      (when (< ix #x40862E42)
        (return (* h (float64-exp (float64-abs x)))) )

     ;; |x| in [log(maxdouble), overflowthresold]
     (when (or (< ix #x408633CE)
               (and (= ix #x408633ce)
                    (<= (uint32 lx) #x8fb9f87d) ))
       (let ((w (float64-exp (* 0.5 (float64-abs x)))))
         (return (* h w w)) ))
     ;; |x| > overflowthresold, sinh(x) overflow
     (return (* x shuge)) ) ) ) ) ) )
