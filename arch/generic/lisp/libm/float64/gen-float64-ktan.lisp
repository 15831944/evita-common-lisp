;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - Kernel tan
;;; arch/generic/lisp/math/gen-math-f64-ktan.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-ktan.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-kernel-tan
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* @(#)k_tan.c 5.1 93/09/24 */
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

/* __kernel_tan( x, y, k )
 * kernel tan function on [-pi/4, pi/4], pi/4 ~ 0.7854
 * Input x is assumed to be bounded by ~pi/4 in magnitude.
 * Input y is the tail of x.
 * Input k indicates whether tan (if k=1) or 
 * -1/tan (if k= -1) is returned.
 *
 * Algorithm
 *      1. Since tan(-x) = -tan(x), we need only to consider positive x. 
 *      2. if x < 2^-28 (hx<#x3e300000 0), return x with inexact if x!=0.
 *      3. tan(x) is approximated by a odd polynomial of degree 27 on
 *         [0,0.67434]
 *                                 3             27
 *                 tan(x) ~ x + T1*x + ... + T13*x
 *         where
 *      
 *              |tan(x)         2     4            26   |     -59.2
 *              |----- - (1+T1*x +T2*x +.... +T13*x    )| <= 2
 *              |  x                                         | 
 * 
 *         Note: tan(x+y) = tan(x) + tan'(x)*y
 *                        ~ tan(x) + (1+x*x)*y
 *         Therefore, for better accuracy in computing tan(x+y), let 
 *                   3      2      2       2       2
 *              r = x *(T2+x *(T3+x *(...+x *(T12+x *T13))))
 *         then
 *                                   3    2
 *              tan(x+y) = x + (T1*x + (x *(r+y)+y))
 *
 *      4. For x in [0.67434,pi/4],  let y = pi/4 - x, then
 *              tan(x) = tan(pi/4-y) = (1-tan(y))/(1+tan(y))
 *                     = 1 - 2*(tan(y) - (tan(y)^2)/(1+tan(y)))
 */
|#
(defun float64-kernel-tan (x y iy)
    (declare (values double-float))
    (declare (type double-float x y))
    (declare (type fixnum iy))
  (prog* (
    (one    #+nil 1.00000000000000000000e+00
            #.(encode-float64 #x3FF00000 #x00000000) )
    (pio4   #+nil 7.85398163397448278999e-01
            #.(encode-float64 #x3FE921FB #x54442D18) )
    (pio4lo #+nil 3.06161699786838301793e-17
            #.(encode-float64 #x3C81A626 #x33145C07) )
    (T0     #+nil 3.33333333333334091986e-01
            #.(encode-float64 #x3FD55555 #x55555563) )
    (T1     #+nil 1.33333333333201242699e-01
            #.(encode-float64 #x3FC11111 #x1110FE7A ) )
    (T2     #+nil 5.39682539762260521377e-02
            #.(encode-float64 #x3FABA1BA #x1BB341FE ) )
    (T3     #+nil 2.18694882948595424599e-02
            #.(encode-float64 #x3F9664F4 #x8406D637 ) )
    (T4     #+nil 8.86323982359930005737e-03
            #.(encode-float64 #x3F8226E3 #xE96E8493 ) )
    (T5     #+nil 3.59207910759131235356e-03
            #.(encode-float64 #x3F6D6D22 #xC9560328 ) )
    (T6     #+nil 1.45620945432529025516e-03
            #.(encode-float64 #x3F57DBC8 #xFEE08315 ) )
    (T7     #+nil 5.88041240820264096874e-04
            #.(encode-float64 #x3F4344D8 #xF2F26501 ) )
    (T8     #+nil 2.46463134818469906812e-04
            #.(encode-float64 #x3F3026F7 #x1A8D1068 ) )
    (T9     #+nil 7.81794442939557092300e-05
            #.(encode-float64 #x3F147E88 #xA03792A6 ) )
    (T10    #+nil 7.14072491382608190305e-05
            #.(encode-float64 #x3F12B80F #x32F0A7E9 ) )
    (T11    #+nil -1.85586374855275456654e-05
            #.(encode-float64 #xBEF375CB #xDB605373 ) )
    (T12    #+nil 2.59073051863633712884e-05
            #.(encode-float64 #x3EFB2A70 #x74BF7AD4 ) )
    )
    (multiple-value-bind (hx lx) (decode-float64 x)
    (let ((ix (logand hx #x7fffffff))) ; high word of |x|

    ;; x < 2**-28
    (when (< ix #x3e300000)
      (when (eql (truncate x) 0)    ; generate inexact
        (cond
          ((eql (logior ix lx (1+ iy)) 0)
            (return (/ one (float64-abs x))) )
          ((eql iy 1)
            (return x) )
          (t
            (return (/ (- one) x)) ))))

    ;; |x|>=0.6744
    (when (>= ix #x3FE59428)
      (when (< hx 0) (setq x (- x) y (- y)))
      (let ((z (- pio4 x))
            (w (- pio4lo y)) )
        (setq x (+ z w))
        (setq y 0.0d0) ))

    (let* (
      (z (* x x))
      (w (* z z))
      ;; Break x^5*(T[1]+x^2*T[2]+...) into
      ;; x^5(T[1]+x^4*T[3]+...+x^20*T[11]) +
      ;; x^5(x^2*(T[2]+x^4*T[4]+...+x^22*[T12]))
      (r (+ t1 (* w
         (+ t3 (* w
         (+ t5 (* w
         (+ t7 (* w
         (+ t9 (* w t11)))))))))) )
      (v (* z (+ t2
         (* w (+ t4
         (* w (+ t6
         (* w (+ t8
         (* w (+ t10
         (* w t12))))))))))) )
      (s (* z x))
      )
      (setq r (+ y (* z (+ (* s (+ r v)) y))))
      (incf r (* T0 s))
      (setq w (+ x r))

      (when (>= ix #x3FE59428)
        (let ((v (float iy 0d0)))
          (return (float (* (- 1 (logand (ash hx -30) 2))
                            (- v (* 2.0d0 (- x (- (/ (* w w) (+ w v)) r)))))
                         0d0 )) ))
      (when (eql iy 1) (return w))

      ;; if allow error up to 2 ulp, simply return -1.0/(x+r) here

      ;; compute -1.0/(x+r) accurately
      (let* (
        (z (encode-float64 (decode-float64 w) 0))
        (v (- r (- z x)))    ; z+v = r+x
        (a (/ -1.0d0 w))     ; a= -1.0/w
        (tt (encode-float64 (decode-float64 a) 0))
        (s  (+ 1d0 (* tt z)))
        )
        (return (+ tt (* a (+ s (* tt v))))) ) ) ) ) ) )
