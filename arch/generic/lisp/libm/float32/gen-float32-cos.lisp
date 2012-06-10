;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - cos
;;; arch/generic/lisp/float/gen-float-f32-cos.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-cos.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-cos
;
(in-package :si)

#|
 *  From fdlibm (http://www.netlib.org/fdlibm/)
/* sf_cos.c -- float version of s_cos.c.
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
(defun float32-cos (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (zero (encode-float32 0))
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    )
    ;;
    (cond
      ;; |x| ~< pi/4
      ((<= ix #x3f490fd8)
        (return (float32-kernel-cos x zero)) )

      ;; cos(Inf or NaN) is NaN
      ((>= ix #x7F800000)
        (return (- x x)) )

      ;; argument reduction needed
      (t
        (multiple-value-bind (n y0 y1) (float32-rem-pio2 x)
          (ecase (logand n 3)
            (0 (return (float32-kernel-cos y0 y1)))
            (1 (return (- (float32-kernel-sin y0 y1 1))))
            (2 (return (- (float32-kernel-cos y0 y1))))
            (3 (return (float32-kernel-sin y0 y1 1))) ) ) )) ) )
