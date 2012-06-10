;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 - tan
;;; arch/generic/lisp/float/float32/gen-float32-tan.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-tan.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-tan
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* sf_tan.c -- float version of s_tan.c.
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
(defun float32-tan (x)
    (declare (values single-float))
    (declare (type single-float x))
  (let* ((hx (decode-float32 x))
         (ix (logand hx #x7fffffff)) )
    (cond
      ;; |x| ~< pi/4
      ((<= ix #x3f490fda)
        (float32-kernel-tan x 0f0 1) )
      ;; tan(Inf or NaN) is NaN
      ((>= ix #x7f800000)
        (- x x) )
      ;; argument reduction needed
      (t
        (multiple-value-bind (n y0 y1) (float32-rem-pio2 x)
          ;; 1..n even, -1..n odd
          (float32-kernel-tan y0 y1 (- 1 (ash (logand n 1) 1))) ) )) ) )
