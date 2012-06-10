;;;; -*- Hypote: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64 - hypot
;;; arch/generic/lisp/macth/gen-float64-hypot.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-hypot.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-hypot
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* @(#)e_hypot.c 5.1 93/09/24 */
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

/* __ieee754_hypot(x,y)
 *
 * Method :
 *      If (assume round-to-nearest) z=x*x+y*y
 *      has error less than sqrt(2)/2 ulp, than
 *      sqrt(z) has error less than 1 ulp (exercise).
 *
 *      So, compute sqrt(x*x+y*y) with some care as
 *      follows to get the error below 1 ulp:
 *
 *      Assume x>y>0;
 *      (if possible, set rounding to round-to-nearest)
 *      1. if x > 2y  use
 *              x1*x1+(y*y+(x2*(x+x1))) for x*x+y*y
 *      where x1 = x with lower 32 bits cleared, x2 = x-x1; else
 *      2. if x <= 2y use
 *              t1*y1+((x-y)*(x-y)+(t1*y2+t2*y))
 *      where t1 = 2x with lower 32 bits cleared, t2 = 2x-t1,
 *      y1= y with lower 32 bits chopped, y2 = y-y1.
 *
 *      NOTE: scaling may be necessary if some argument is too
 *            large or too tiny
 *
 * Special cases:
 *      hypot(x,y) is INF if x or y is +INF or -INF; else
 *      hypot(x,y) is NAN if x or y is NAN.
 *
 * Accuracy:
 *      hypot(x,y) returns sqrt(x^2+y^2) with error less
 *      than 1 ulps (units in the last place)
 */
|#
(defun float64-hypot (x y)
    (declare (values double-float))
    (declare (type double-float x y))
  (multiple-value-bind (hx la) (decode-float64 x)
  (multiple-value-bind (hy lb) (decode-float64 y)
  (prog* (
    (ha (logand hx #x7fffffff))
    (hb (logand hy #x7fffffff))
    (a x)
    (b y)
    (k 0)
    )
    (when (> hb ha)
      (rotatef a b)
      (rotatef ha hb)
      (rotatef la lb) )
    (setq a (encode-float64 ha la))
    (setq b (encode-float64 hb lb))

    ;; x/y > 2**60
    (when (> (- ha hb) #x3c00000)
      (return (+ a b)) )

    ;; a>2**500
    (when (> ha #x5f300000)
      ;; Inf or NaN
      (when (>= ha #x7ff00000)
        (let ((w (+ a b)))  ; for sNaN
          (when (eql (logior (logand ha #x000fffff) la) 0) (setq w a))  ; inf?
          (when (eql (logior (logxor hb #x7ff00000) lb) 0) (setq w b))  ; inf?
          (return w) ))

      ;; scale a and b by 2**-600
      (decf ha #x25800000)
      (decf hb #x25800000)
      (incf k 600)
      (setq a (encode-float64 ha la))
      (setq b (encode-float64 hb lb)) )

    ;; b < 2**-500
    (when (< hb #x20b00000)
      (if (<= hb #x000fffff)
        ;; subnormal b or 0
        (progn
          (when (eql (logior hb lb) 0) (return a))
          (let ((t1 (encode-float64 #x7fd00000 0))) ; t1=2^1022
            (setq b (* b t1))
            (setq a (* a t1))
            (decf k 1022) ))
        ;; scale a and b by 2^600
        (progn
          (incf ha #x25800000)  ; a *= 2^600
          (incf hb #x25800000)  ; b *= 2^600
          (decf k 600)
          (setq a (encode-float64 ha la))
          (setq b (encode-float64 hb lb)) )))

    ;; medium size a and b
    (let ((w (- a b)))
      (if (> w b)
        (let* ((t1 (encode-float64 ha 0))
               (t2 (- a t1)) )
          (setq w (float64-sqrt
                    (- (* t1 t1) (- (* b (- b)) (* t2 (+ a t1)))) )) )
        (let* ((a  (+ a a))
               (y1 (encode-float64 hb 0))
               (y2 (- b y1))
               (t1 (encode-float64 (+ ha #x00100000) 0))
               (t2 (- a t1)) )
          (setq w (float64-sqrt
                    (- (* t1 y1) (- (* w (- w)) (+ (* t1 y2) (* t2 b)))) )) ))

      (when (eql k 0)
        (return w) )

      (let ((t1 (encode-float64 (+ #x3FF00000 (ash k 20)) 0)))
        (return (* t1 w)) ) ) ) ) ) )
