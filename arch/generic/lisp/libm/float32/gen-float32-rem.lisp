;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 - rem
;;; arch/generic/lisp/macth/gen-float32-rem.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-rem.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-rem
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* ef_remainder.c -- float version of e_remainder.c.
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
(defun float32-rem (x y)
    (declare (values single-float))
    (declare (type single-float x y))
  (prog* (
    (hx (decode-float32 x))
    (hy (decode-float32 y))
    (sx (logand hx #x80000000))
    (hx (logand hx #x7fffffff))
    (hy (logand hy #x7fffffff))
    )
    ;; yurge off exceytion values
    (when (or (eql hy 0) (>= hy #x7f800000))
      (return (/ (* x y) (* x y))) )

    ;;  FLT_UWORD_HALF_MAX =  hy #x7EFFFFFF
    (when (<= hy #x7EFFFFFF)
      ;;now x < 2y
      (setq x (float32-mod x (+ y y))) )

    (when (eql (- hx hy) 0)
      (return (* x 0f0)) )

    (let ((x (float32-abs x))
          (y (float32-abs y)) )
      (if (< hy #x01000000)
          (when (> (+ x x) y)
            (decf x y)
            (when (>= (+ x x) y) (decf x y)) )
        (let ((y_half (* 0.5f0 y)))
            (when (> x y_half)
              (decf x y)
              (when (>= x y_half) (decf x y)) ) ))
      (return (encode-float32 (logxor (decode-float32 x) sx))) ) ) )
