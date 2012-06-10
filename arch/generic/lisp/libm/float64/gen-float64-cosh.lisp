;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - cosh
;;; arch/generic/lisp/macth/gen-math-64-cosh.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-cosh.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     fcosh64
;
(in-package :si)

#|
 * @(#)e_cosh.c 5.1 93/09/24
 *
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================


 * __ieee754_cosh(x)
 * Method :
 * mathematically cosh(x) if defined to be (exp(x)+exp(-x))/2
 *      1. Replace x by |x| (cosh(x) = cosh(-x)).
 *      2.
 *                                                      [ exp(x) - 1 ]^2
 *          0        <= x <= ln2/2  :  cosh(x) := 1 + -------------------
 *                                                                2*exp(x)
 *
 *                                                exp(x) +  1/exp(x)
 *          ln2/2    <= x <= 22     :  cosh(x) := -------------------
 *                                                               2
 *          22       <= x <= lnovft :  cosh(x) := exp(x)/2
 *          lnovft   <= x <= ln2ovft:  cosh(x) := exp(x/2)/2 * exp(x/2)
 *          ln2ovft  <  x            :  cosh(x) := huge*huge (overflow)
 *
 * Special cases:
 *      cosh(x) is |x| if x is +INF, -INF, or NaN.
 *      only cosh(0)=1 is exact for finite x.
|#
(defun float64-cosh (x)
    (declare (values double-float))
    (declare (type double-float x))
  (let ((one 1d0)
        (half 0.5d0)
        (huge 1.0d300) )
    (labels ((uint (i) (if (minusp i) (+ (ash 1 32) i) i)))
    (multiple-value-bind (hx lx) (decode-float64 x)
      (let ((ix (logand hx #x7fffffff)))
        (cond
          ((>= ix #x7ff00000)
            ;; x is INF or NaN
            (* x x) )

          ((< ix #x3fd62e43)
           ;; |x| in [0,0.5*ln2], return 1+expm1(|x|)^2/(2*exp(|x|))
           (let* ((g (float64-expm1 (float64-abs x)))
                  (w (+ one g)) )
             (if (< ix #x3c800000)
                 w        ; cosh(tiny) = 1
               (+ one (/ (* g g) (+ w w))) ) ) )

          ((< ix #x40360000)
            ;; |x| in [0.5*ln2,22], return (exp(|x|)+1)/exp(|x|)/2
            (let ((g (float64-exp (float64-abs x))))
              (+ (* half g) (/ half g)) ) )
          ((< ix #x40862E42)
            ;; |x| in [22, log(maxdouble)] return half*exp(|x|)
            (* half (float64-exp (float64-abs x))) )
          ((or (< ix #x408633CE)
               (and (eql ix #x408633ce)
                    (<= (uint lx) #x8fb9f87d) ))
            ;; |x| in [log(maxdouble), overflowthresold]
            (let* ((w  (float64-exp (* half (float64-abs x))))
                   (g  (* half w)) )
              (* g w) ) )
          (t
            ;; |x| > overflowthresold, cosh(x) overflow
            (* huge huge) )) ) ) ) ) )
