;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - Kernel sin
;;; arch/generic/lisp/math/gen-math-f64-ksin.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-ksin.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-kernel-sin
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* @(#)k_sin.c 5.1 93/09/24 */
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

/* __kernel_sin( x, y, iy)
 * kernel sin function on [-pi/4, pi/4], pi/4 ~ 0.7854
 * Input x is assumed to be bounded by ~pi/4 in magnitude.
 * Input y is the tail of x.
 * Input iy indicates whether y is 0. (if iy=0, y assume to be 0). 
 *
 * Algorithm
 *      1. Since sin(-x) = -sin(x), we need only to consider positive x. 
 *      2. if x < 2^-27 (hx<#x3e400000 0), return x with inexact if x!=0.
 *      3. sin(x) is approximated by a polynomial of degree 13 on
 *         [0,pi/4]
 *                                 3            13
 *                 sin(x) ~ x + S1*x + ... + S6*x
 *         where
 *      
 *      |sin(x)         2     4     6     8     10     12  |     -58
 *      |----- - (1+S1*x +S2*x +S3*x +S4*x +S5*x  +S6*x   )| <= 2
 *      |  x                                                    | 
 * 
 *      4. sin(x+y) = sin(x) + sin'(x')*y
 *                  ~ sin(x) + (1-x*x/2)*y
 *         For better accuracy, let 
 *                   3      2      2      2      2
 *              r = x *(S2+x *(S3+x *(S4+x *(S5+x *S6))))
 *         then                   3    2
 *              sin(x) = x + (S1*x + (x *(r-y/2)+y))
 */
|#
(defun float64-kernel-sin (x y iy)
    (declare (values double-float))
    (declare (type double-float x y))
    (declare (type fixnum iy))
    ;; iy=0 if y is zero
  (prog* (
    (half   #+nil 5.00000000000000000000e-01
            #.(encode-float64 #x3FE00000 #x00000000) )
    (S1     #+nil -1.66666666666666324348e-01
            #.(encode-float64 #xBFC55555 #x55555549) )
    (S2     #+nil 8.33333333332248946124e-03
            #.(encode-float64 #x3F811111 #x1110F8A6) )
    (S3     #+nil -1.98412698298579493134e-04
            #.(encode-float64 #xBF2A01A0 #x19C161D5) )
    (S4     #+nil 2.75573137070700676789e-06
            #.(encode-float64 #x3EC71DE3 #x57B1FE7D) )
    (S5     #+nil -2.50507602534068634195e-08
            #.(encode-float64 #xBE5AE5E6 #x8A2B9CEB) )
    (S6     #+nil 1.58969099521155010221e-10
            #.(encode-float64 #x3DE5D93A #x5ACFD57C) )
    ;;
    (hx (decode-float64 x)) ;  high word of x
    (ix (logand hx #x7fffffff))
    )

    ;; |x| < 2**-27 
    (when (< ix #x3e400000)
      ;; generate inexact
      (when (eql (truncate x) 0) (return x)) )
    (let* ((z (* x x))
           (v (* z x))
           (r (+ s2 (* z (+ s3 (* z (+ s4 (* z (+ s5 (* z s6))))))))) )
      (if (eql iy 0)
          (return (+ x (* v (+ s1 (* z r)))))
        (return (- x (- (- (* z (- (* half y) (* v r))) y) (* v s1)))) ) ) ) )

