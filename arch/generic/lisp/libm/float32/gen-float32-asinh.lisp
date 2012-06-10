;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 - asinh
;;; arch/generic/lisp/float/float32/gen-float32-asinh.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-asinh.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-asinh
;
(in-package :si)

#|
 *  From fdlibm (http://www.netlib.org/fdlibm/)
/* sf_asinh.c -- float version of s_asinh.c.
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
(defun float32-asinh (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (one    #+nil 1.0000000000e+00 #.(encode-float32 #x3F800000))
    (ln2    #+nil 6.9314718246e-01 #.(encode-float32 #x3f317218))
    (huge   #+nil 1.0000000000e+30 #.(encode-float32 #x7149F2C9))
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    )
    ;;
    (when (>= ix #x7f800000)    ; x is inf or NaN
      (return (+ x x)) )


    ;; |x|<2**-28
    (when (< ix #x31800000)
        ;; return x inexact except 0
        (when (> (+ huge x) one) (return x)) )

    (let ((w
            (cond
              ((> ix #x4d800000)
                ;; |x| > 2**28
                (float32-log (+ (float32-abs x) ln2)) )
              ((> ix #x40000000)
                ;; 2**28 > |x| > 2.0
                (let ((tt (float32-abs x)))
                  (float32-log
                    (+ (* 2 tt)
                       (/ one (+ (float32-sqrt (+ (* x x) one)) tt)) )) ) )
              (t
                ;; 2.0 > |x| > 2**-28
                (let ((tt (* x x)))
                  (float32-log1p
                    (+ (float32-abs x)
                       (/ tt (+ one (float32-sqrt (+ one tt)))) )) ) )) ))
      (return (if (> hx 0) w (- w))) ) ) )

