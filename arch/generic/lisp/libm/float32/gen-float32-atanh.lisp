;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 - atanh
;;; arch/generic/lisp/float/float32/gen-float32-atanh.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-atanh.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-atanh
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
/* ef_atanh.c -- float version of e_atanh.c.
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
 *
 */
|#
(defun float32-atanh (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (one    1f0)
    (huge   1f30)
    (zero   0f0)
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    )
    ;;
    (cond
      ((> ix #x3f800000)
        ;;* |x|>1
        (return (/ (- x x) (- x x))) )
      ((eql ix #x3f800000)
        (return (/ x zero)) )
      ((and (< ix #x31800000) (> (+ huge x) zero))
        ;; x<2**-28
        (return x) )
      (t
        (let* ((x (encode-float32 ix))
               (tt
                 (if (< ix #x3f000000) ; x < 0.5
                      (let ((tt (+ x x)))
                         (* 0.5f0
                            (float32-log1p (+ tt (/ (* tt x) (- one x)))) ) )
                      (* 0.5f0 (float32-log1p (/ (+ x x) (- one x)))) ) ))
          (return (if (>= hx 0) tt (- tt))) ) )) ) )
