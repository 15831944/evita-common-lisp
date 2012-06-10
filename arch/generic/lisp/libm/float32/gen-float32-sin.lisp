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
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-sin.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-sin
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* sf_sin.c -- float version of s_sin.c.
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
(defun float32-sin (x)
    (declare (values single-float))
    (declare (type single-float x))
  (let* ((hx (decode-float32 x))
         (ix (logand hx #x7fffffff)) )
    (cond
      ;; |x| ~< pi/4
      ((<= ix #x3f490fd8)
        (float32-kernel-sin x 0f0 0) )
      ;; sin(Inf or NaN) is NaN
      ((>= ix #x7f800000)
        (- x x) )
      ;; argument reduction needed
      (t
        (multiple-value-bind (n y0 y1) (float32-rem-pio2 x)
          (ecase (logand n 3)
            (0 (float32-kernel-sin y0 y1 1))
            (1 (float32-kernel-cos y0 y1))
            (2 (- (float32-kernel-sin y0 y1 1)))
            (3 (- (float32-kernel-cos y0 y1))) ) ) )) ) )
