;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - acosh
;;; arch/generic/lisp/macth/gen-r12-acosh64.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-acosh.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     facosh64
;
(in-package :si)

#|
 * 
 * From fdlibm (http://www.netlib.org/fdlibm/)
 * @(#)e_acosh.c 5.1 93/09/24
 *
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 *
 *
 * __ieee754_acosh(x)
 * Method :
 *      Based on 
 *              acosh(x) = log [ x + sqrt(x*x-1) ]
 *      we have
 *              acosh(x) := log(x)+ln2,        if x is large; else
 *              acosh(x) := log(2x-1/(sqrt(x*x-1)+x)) if x>2; else
 *              acosh(x) := log1p(t+sqrt(2.0*t+t*t)); where t=x-1.
 *
 * Special cases:
 *      acosh(x) is NaN with signal if x<1.
 *      acosh(NaN) is NaN without signal.
|#
(defun float64-acosh (x)
    (declare (values double-float))
    (declare (type double-float x))
  (let ((one 1.0d0)
        (ln2 #+nil 6.93147180559945286227e-01;
              #.(encode-float64 #x3FE62E42 #xFEFA39EF) ))
    (multiple-value-bind (hx lx) (decode-float64 x)
      (cond
        ((< hx #x3ff00000)              ; x < 1
          (/ (- x x) (- x x)) )         ; NaN
        ((> hx #x41b00000)              ; x > 2**28
          (if (>= hx #x7ff000000)       ; x is inf of NaN
              (+ x x)
            (+ (float64-log x) ln2) ) ) ; acosh(huge)=log(2x)
        ((zerop (logior (- hx #x3ff00000) lx))
          0d0 )                         ; acos(1) = 0
        ((> hx #x40000000)              ; 2**28 > x > 2
          (let ((x2 (* x x)))
            (float64-log  (- (* 2.0d0 x)
                        (/ one (+ x (float64-sqrt (- x2 one)))))) ) )
        (t  ; 1 < x < 2
          (let ((tt (- x one)))
            (float64-log1p
                (+ tt (float64-sqrt (+ (* 2.0d0 tt) (* tt tt))))) ) )) ) ) )
