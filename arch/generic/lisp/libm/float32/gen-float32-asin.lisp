;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 asin
;;; arch/generic/lisp/math/gen-math-f32-asin.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-asin.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of single-float asin.
;
(in-package :si)

#|
/* ef_asin.c -- float version of e_asin.c.
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
(defun float32-asin (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (one        #+nil 1.0000000000e+00  #.(encode-float32 #x3F800000))
    (huge       #+nil 1.000f+30         #.(encode-float32 #x7149F2C9))
    (pio2_hi    #+nil 1.57079637050628662109375f
                    #.(encode-float32 #x3FC90FDB) )
    (pio2_lo    #+nil -4.37113900018624283e-8f  #.(encode-float32 #xB33BBD2E))
    (pio4_hi    #+nil 0.785398185253143310546875f
                    #.(encode-float32 #x3F490FDB) )
    ;; coefficient for R(x^2)
    (pS0        #+nil  1.6666667163e-01 #.(encode-float32 #x3e2aaaab))
    (pS1        #+nil -3.2556581497e-01 #.(encode-float32 #xbea6b090))
    (pS2        #+nil  2.0121252537e-01 #.(encode-float32 #x3e4e0aa8))
    (pS3        #+nil -4.0055535734e-02 #.(encode-float32 #xbd241146))
    (pS4        #+nil  7.9153501429e-04 #.(encode-float32 #x3a4f7f04))
    (pS5        #+nil  3.4793309169e-05 #.(encode-float32 #x3811ef08))
    (qS1        #+nil -2.4033949375e+00 #.(encode-float32 #xc019d139))
    (qS2        #+nil  2.0209457874e+00 #.(encode-float32 #x4001572d))
    (qS3        #+nil -6.8828397989e-01 #.(encode-float32 #xbf303361))
    (qS4        #+nil  7.7038154006e-02 #.(encode-float32 #x3d9dc62e))
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    )
    ;;
    (cond
      ((eql ix #x3f800000)
        ;; asin(1)=+-pi/2 with inexact
        (return (+ (* x pio2_hi) (* x pio2_lo))) )
      ((> ix #x3f800000)
        ;; |x|>= 1
        (return (/ (- x x) (- x x))) )  ; asin(|x|>1) is NaN
      ((< ix #x3f000000)
        ;; |x|<0.5
        (cond
          ((< ix #x32000000)
            ;; if |x| < 2**-27
            (when (> (+ huge x) one)
              (return x) ) )    ; return x with inexact if x!=0
          (t
            (let* ((tt (* x x))
                   (p  (* tt (+ ps0
                       (* tt (+ ps1
                       (* tt (+ ps2
                       (* tt (+ ps3
                       (* tt (+ ps4
                       (* tt ps5))))))))))) )
                   (q  (+ one (* tt
                       (+ qs1 (* tt
                       (+ qs2 (* tt
                       (+ qs3 (* tt qs4)))))))) )
                   (w  (/ p q)) )
              (return (+ x (* x w))) ) )) ))
    ;; 1> |x|>= 0.5
    (let* ((w  (- one (float32-abs x)))
           (tt (* w 0.5f0))
           (p  (* tt (+ ps0
               (* tt (+ ps1
               (* tt (+ ps2
               (* tt (+ ps3
               (* tt (+ ps4
               (* tt ps5))))))))))) )
           (q  (+ one (* tt
               (+ qs1 (* tt
               (+ qs2 (* tt
               (+ qs3 (* tt qs4)))))))) )
           (s  (float32-sqrt tt)) )
        (if (>= ix #x3F79999A)  ; if |x| > 0.975
            (setq w (/ p q)
                  tt (- pio2_hi (- (* 2 (+ s (* s w))) pio2_lo)) )
          (let* ((iw (decode-float32 w))
                 (w  (encode-float32 (logand iw #xfffff000)))
                 (c  (/ (- tt (* w w)) (+ s w)))
                 (r  (/ p q))
                 (p  (- (* (* 2 s) r) (- pio2_lo (* 2 c))))
                 (q  (- pio4_hi (* 2 w))) )
            (setq tt  (- pio4_hi (- p q))) ))
        (return (if (> hx 0) tt (- tt))) ) ) )
