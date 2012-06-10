;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 acosh2
;;; arch/generic/lisp/macth/gen-math-f32-acosh2.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-acosh.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-acosh
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
/* ef_acosh.c -- float version of e_acosh.c.
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
(defun float32-acosh (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (one 1f0)
    (ln2 #+nil 6.9314718246f-01
         #.(encode-float32 #x3f317218) )
    ;;
    (hx (decode-float32 x))
    )
    ;;
    (cond
      ((< hx #x3f800000)    ; x < 1
        (return (/ (- x x) (- x x))) )   ; NaN
      ((>= hx #x4d800000)    ;x > 2**28
        (if (>= hx #x7FC00000) ; x is inf or NaN
            (return (+ x x))
          ;; acosh(huge)=log(2x)
          (return (+ (float32-log x) ln2)) ) )
      ((eql hx #x3f800000)
        (return 0f0) )  ; acosh(1) = 0
      ((> hx #x40000000)    ;  2**28 > x > 2
        (let ((tt (* x x)))
          (return (float32-log
                (- (* 2 x) (/ one (+ x (float32-sqrt (- tt one))))) )) ) )
      (t ; 1<x<2
        (let ((tt (- x one)))
          (return (float32-log1p
            (+ tt (float32-sqrt (+ (* 2 tt) (* tt tt)))) )) ) )) ) )

