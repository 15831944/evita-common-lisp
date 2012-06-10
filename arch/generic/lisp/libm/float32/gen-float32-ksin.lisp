;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - Kernel cos
;;; arch/generic/lisp/math/gen-math-f64-kcos.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-ksin.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-kernel-cos
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* kf_sin.c -- float version of k_sin.c
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
(defun float32-kernel-sin (x y iy)
    (declare (values single-float))
    (declare (type single-float x y))
    (declare (type fixnum iy))  ; iy=0 if y is zero
  (prog* (
    (half   #+nil 5.0000000000e-01  #.(encode-float32 #x3f000000))
    (S1     #+nil -1.6666667163e-01 #.(encode-float32 #xbe2aaaab))
    (S2     #+nil 8.3333337680e-03  #.(encode-float32 #x3c088889))
    (S3     #+nil -1.9841270114e-04 #.(encode-float32 #xb9500d01))
    (S4     #+nil 2.7557314297e-06  #.(encode-float32 #x3638ef1b))
    (S5     #+nil -2.5050759689e-08 #.(encode-float32 #xb2d72f34))
    (S6     #+nil 1.5896910177e-10  #.(encode-float32 #x2f2ec9d3))
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    )
    (when (< ix #x32000000) ; |x| < 2**-27
      ;; generate inexact
      (when (eql (truncate x) 0) (return x)) )
    (let* ((z (* x x))
           (v (* z x))
           (r (+ s2 (* z (+ s3 (* z (+ s4 (* z (+ s5 (* z s6))))))))) )
      (if (eql iy 0)
          (return (+ x (* v (+ s1 (* z r)))))
        (return (- x (- (- (* z (- (* half y) (* v r))) y) (* v s1)))) ) ) ) )
