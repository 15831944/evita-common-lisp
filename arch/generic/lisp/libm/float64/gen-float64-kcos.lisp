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
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-kcos.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-kernel-cos
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* @(#)k_cos.c 5.1 93/09/24 */
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
 * __kernel_cos( x,  y )
 * kernel cos function on [-pi/4, pi/4], pi/4 ~ 0.785398164
 * Input x is assumed to be bounded by ~pi/4 in magnitude.
 * Input y is the tail of x.
 *
 * Algorithm
 *      1. Since cos(-x) = cos(x), we need only to consider positive x.
 *      2. if x < 2^-27 (hx<#x3e400000 0), return 1 with inexact if x!=0.
 *      3. cos(x) is approximated by a polynomial of degree 14 on
 *         [0,pi/4]
 *                                         4            14
 *                 cos(x) ~ 1 - x*x/2 + C1*x + ... + C6*x
 *         where the remez error is
 *
 *      |              2     4     6     8     10    12     14 |     -58
 *      |cos(x)-(1-.5*x +C1*x +C2*x +C3*x +C4*x +C5*x  +C6*x  )| <= 2
 *      |                                                           |
 *
 *                     4     6     8     10    12     14
 *      4. let r = C1*x +C2*x +C3*x +C4*x +C5*x  +C6*x  , then
 *             cos(x) = 1 - x*x/2 + r
 *         since cos(x+y) ~ cos(x) - sin(x)*y
 *                        ~ cos(x) - x*y,
 *         a correction term is necessary in cos(x) and hence
 *              cos(x+y) = 1 - (x*x/2 - (r - x*y))
 *         For better accuracy when x > 0.3, let qx = |x|/4 with
 *         the last 32 bits mask off, and if x > 0.78125, let qx = 0.28125.
 *         Then
 *              cos(x+y) = (1-qx) - ((x*x/2-qx) - (r-x*y)).
 *         Note that 1-qx and (x*x/2-qx) is EXACT here, and the
 *         magnitude of the latter is at least a quarter of x*x/2,
 *         thus, reducing the rounding error in the subtraction.
 */
|#
(defun float64-kernel-cos (x y)
    (declare (values double-float))
    (declare (type double-float x y))
  (prog* (
    (one    #+nil 1.00000000000000000000e+00
            #.(encode-float64 #x3FF00000 #x00000000) )
    (C1     #+nil 4.16666666666666019037e-02
            #.(encode-float64 #x3FA55555 #x5555554C) )
    (C2     #+nil -1.38888888888741095749e-03
            #.(encode-float64 #xBF56C16C #x16C15177) )
    (C3     #+nil 2.48015872894767294178e-05
            #.(encode-float64 #x3EFA01A0 #x19CB1590) )
    (C4     #+nil -2.75573143513906633035e-07
            #.(encode-float64 #xBE927E4F #x809C52AD) )
    (C5     #+nil 2.08757232129817482790e-09
            #.(encode-float64 #x3E21EE9E #xBDB4B1C4) )
    (C6     #+nil -1.13596475577881948265e-11
            #.(encode-float64 #xBDA8FAE9 #xBE8838D4) )
    ;;
    (hx (decode-float64 x))
    (ix (logand hx #x7fffffff))
    )
    ;; if x < 2**27
    (when (< ix #x3e400000)
      ;; generate inexact
      (when (eql (truncate x) 0) (return one)) )

    (let* ((z  (* x x))
           (r  (* z (+ c1
               (* z (+ c2
               (* z (+ c3
               (* z (+ c4
               (* z (+ c5
               (* z c6))))))))))) ))
       ;; if |x| < 0.3 */
       (when (< ix #x3FD33333)
         (return (- one (- (* 0.5d0 z) (- (* z r) (* x y))))) )


        (let* ((qx
                (if (> ix #x3fe90000) ; x > 0.78125
                    0.28125d0
                  (encode-float64 (- ix #x00200000) 0) ) ) ; x/4
               (hz (- (* 0.5d0 z) qx))
               (a  (- one qx)) )
          (return (- a (- hz (- (* z r) (* x y))))) ) ) ) )
