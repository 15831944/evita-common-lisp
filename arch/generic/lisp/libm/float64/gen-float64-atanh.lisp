;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64 - atanh
;;; arch/generic/lisp/float/float64/gen-float64-atanh.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-atanh.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-atanh
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
 * @(#)e_atanh.c 5.1 93/09/24
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

 *
 * __ieee754_atanh(x)
 * Method :
 *    1.Reduced x to positive by atanh(-x) = -atanh(x)
 *    2.For x>=0.5
 *                  1              2x                          x
 *      atanh(x) = --- * log(1 + -------) = 0.5 * log1p(2 * --------)
 *                  2             1 - x                      1 - x
 *
 *      For x<0.5
 *      atanh(x) = 0.5*log1p(2x+2x*x/(1-x))
 *
 * Special cases:
 *      atanh(x) is NaN if |x| > 1 with signal
 *      atanh(NaN) is that NaN with no signal
 *      atanh(+-1) is +-INF with signal.
 *
|#
(defun float64-atanh (x)
    (declare (values double-float))
    (declare (type double-float x))
  (prog* (
    (one 1d0)
    (huge 1d300)
    (zero 0d0)
    )
    (multiple-value-bind (hx lx) (decode-float64 x)
      (let ((ix (logand hx #x7fffffff)))
        (when (> (float64-abs x) 1d0)  ; |x|>1
          (return (/ (- x x) (- x x))) )

        (when (eql ix #x3ff00000)
          (return (/ x zero)) )

        (when (and (< ix #x3e300000 (> (+ huge x) zero)))
          (return x) )    ; x<2**-28

        (let* ((x (encode-float64 ix lx))
               (g
                  (if (< ix #x3fe00000) ; x < 0.5
                      (let ((g (+ x x)))
                        (* 0.5 (float64-log1p (+ g (/ (* g x) (- one x))))) )
                    (* 0.5 (float64-log1p (/ (+ x x) (- one x)))) ) ))
          (return (if (>= hx 0) g (- g))) ) ) ) ) )
