;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32-scale
;;; arch/generic/lisp/math/gen-math-f32-scale.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-sign.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of float32-scale.
;
(in-package :si)

#|
/* sf_copysign.c -- float version of s_copysign.c.
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

/*
 * copysignf(float x, float y)
 * copysignf(x,y) returns a value with the magnitude of x and
 * with the sign bit of y.
 */
|#
(defun float32-sign (x y)
    (declare (values single-float))
    (declare (type single-float single-float))
  (let ((ix (decode-float32 x))
        (iy (decode-float32 y)) )
   (encode-float32 (logior (logand ix #x7fffffff)
                            (logand iy #x80000000) )) ) )
