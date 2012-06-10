;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 - ceiling
;;; arch/generic/lisp/macth/gen-float32-ceiling.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-fceiling.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-ceiling
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* sf_ceil.c -- float version of s_ceil.c.
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
(defun float32-fceiling (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (huge 1.0f30)
    ;;
    (i0 (decode-float32 x))
    (ix (logand i0 #x7fffffff))
    (j0 (- (ash ix -23) #x7f))
    )
    (cond
      ((< j0 0)
        ;; raise inexact if x != 0
        (when (> (+ huge x) 0f0)
          ;; return 0*sign(x) if |x|<1
          (cond
            ;; FIXME 2007-03-24: #x80000000 isn't (signed-byte 32).
            ((< i0 0) (setq i0 #x80000000))
            ((not (eql ix 0)) (setq i0 #x3f800000)) )) )
      ((< j0 23)
        (let ((i (ash #x007fffff (- j0))))
          ;; x is integral
          (when (eql (logand i0 i) 0)
            (return x) ) 
          ;; raise inexact flag
          (when (> (+ huge x) 0f0)
            (when (> i0 0) (incf i0 (ash #x00800000 (- j0))))
            (setq i0 (logand i0 (lognot i))) ) ) )
      ((> ix #x7f800000)
        ;; inf or NaN
        (return (+ x x)) )
      (t
        ;; x is integral
        (return x) ))
    (return (encode-float32 i0)) ) )
