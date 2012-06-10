;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64 - rem
;;; arch/generic/lisp/macth/gen-float64-rem.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-rem.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-rem
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* @(#)e_remainder.c 5.1 93/09/24 */
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

/* __ieee754_remainder(x,p)
 * Return :                  
 *      returns  x REM p  =  x - [x/p]*p as if in infinite 
 *      precise arithmetic, where [x/p] is the (infinite bit) 
 *      integer nearest x/p (in half way case choose the even one).
 * Method : 
 *      Based on fmod() return x-[x/p]chopped*p exactlp.
 */
|#
(defun float64-rem (x y)
    (declare (values double-float))
    (declare (type double-float x y))
  (multiple-value-bind (hx lx) (decode-float64 x)
  (multiple-value-bind (hy ly) (decode-float64 y)
  (prog* (
    (sx (logand hx #x80000000))
    (hx (logand hx #x7fffffff))
    (hy (logand hy #x7fffffff))
    )

    ;; purge off exception values
    (cond
      ;; y = 0
      ((eql (logior hy ly) 0)
        (return (/ (* x y) (* x y))) )
      ((or (>= hx #x7ff00000)   ; x is not finite
           (>= hy #x7ff00001) ) ; y is NaN
        (return (/ (* x y) (* x y))) ))

    ;; both x and y are finite.
    (when (<= hy #x7fdfffff)
      ;; now x < 2y
      (setq x (float64-mod x (+ y y))) )

    (when (eql (logior (- hx hy) (- lx ly)) 0)
      (return (encode-float64 sx 0)) )

    (let ((x (float64-abs x))
          (y (float64-abs y)) )
      (if (< hy #x00200000)
          (when (> (+ x x) y)
            (decf x y)
            (when (>= (+ x x) y) (decf x y)) )
        (let ((y_half (* 0.5d0 y)))
          (when (> x y_half)
            (decf x y)
            (when (>= x y_half) (decf x y)) ) ))
      (multiple-value-bind (hx lx) (decode-float64 x)
        (return (encode-float64 (logxor hx sx) lx)) ) ) ) ) ) )
