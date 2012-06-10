;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - tanh
;;; arch/generic/lisp/float/float32/gen-float32-tanh.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-tanh.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-tanh
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* sf_tanh.c -- float version of s_tanh.c.
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
(defun float32-tanh (x)
    (declare (values single-float))
    (declare (type single-float x))
  (let* (
    (one   1.0f0)
    (two   2.0f0)
    (tiny  1.0f-300)
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    )

    (cond
      ;; x is INF or NaN
      ((>= ix #x7f800000)
        ;; tanh(+-inf)=+-1, tanh(NaN) = NaN
        (if (>= hx 0) (/ one (+ x one)) (/ one (- x one))) )
      ;; |x| < 22
      ((< ix #x41b00000)        ; |x|<22
        (cond
          ((< ix #x24000000)    ; |x|<2**-55
            (* x (+ one x)) )   ; tanh(small) = small
          ((>= ix #x3f800000)   ; |x|>=1
            (let* ((tt (float32-expm1 (* two (encode-float32 ix))))
                   (z  (- one (/ two (+ tt two)))) )
              (if (>= hx 0) z (- z)) ) )
          (t
            (let* ((tt (float32-expm1 (* (- two) (encode-float32 ix))))
                   (z  (/ (- tt) (+ tt two))) )
              (if (>= hx 0) z (- z)) ) )) )
      ;; |x| > 22, return +-1
      (t
        (let ((z (- one tiny))) ; raised inexact flag
          (if (>= hx 0) z (- z)) ) )) ) )
