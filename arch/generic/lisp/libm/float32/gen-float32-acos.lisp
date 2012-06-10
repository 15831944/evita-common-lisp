;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 acos
;;; arch/generic/lisp/math/gen-math-f32-acos.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-acos.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of single-float acos.
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
/* ef_acos.c -- float version of e_acos.c.
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
(defun float32-acos (x)
    (declare (values single-float))
    (declare (type single-float x))
  (let* (
    (one        #+nil  1.0000000000e+00
                    #.(encode-float32 #x3F800000) )
    (fpi        #+nil  3.1415925026e+00
                    #.(encode-float32 #x40490fda) )
    (pio2_hi    #+nil  1.5707962513e+00
                    #.(encode-float32 #x3fc90fda) )
    (pio2_lo    #+nil  7.5497894159e-08
                    #.(encode-float32 #x33a22168) )
    (pS0        #+nil  1.6666667163e-01
                    #.(encode-float32 #x3e2aaaab) )
    (pS1        #+nil -3.2556581497e-01
                    #.(encode-float32 #xbea6b090) )
    (pS2        #+nil  2.0121252537e-01
                    #.(encode-float32 #x3e4e0aa8) )
    (pS3        #+nil -4.0055535734e-02
                    #.(encode-float32 #xbd241146) )
    (pS4        #+nil  7.9153501429e-04
                    #.(encode-float32 #x3a4f7f04) )
    (pS5        #+nil  3.4793309169e-05
                    #.(encode-float32 #x3811ef08) )
    (qS1        #+nil -2.4033949375e+00
                    #.(encode-float32 #xc019d139) )
    (qS2        #+nil  2.0209457874e+00
                    #.(encode-float32 #x4001572d) )
    (qS3        #+nil -6.8828397989e-01
                    #.(encode-float32 #xbf303361) )
    (qS4        #+nil  7.7038154006e-02
                    #.(encode-float32 #x3d9dc62e) )
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    )
    ;;
    (cond
      ((eql ix #x3f800000)
        ;;; |x|==1
        (if (> hx 0)
            0f0                     ; acos(1) = 0
          (+ fpi (* 2 pio2_lo)) ) ) ; acos(-1)= pi
      ((> ix #x3f800000)            ; |x| >= 1
        ;; acos(|x|>1) is NaN
        (/ (- x x) (- x x)) )
      ((< ix #x3f000000)            ; |x| < 0.5 
        (if (<= ix #x23000000)
            (+ pio2_hi pio2_lo)     ;/*if|x|<2**-57
          (let* (
            (z (* x x))
            (p (* z (+ ps0
               (* z (+ ps1
               (* z (+ ps2
               (* z (+ ps3
               (* z (+ ps4
               (* z ps5))))))))))) )
            (q (+ one (* z
               (+ qs1 (* z
               (+ qs2 (* z
               (+ qs3 (* z qs4)))))))) )
            (r (/ p q))
            )
            (- pio2_hi (- x (- pio2_lo (* x r)))) )) )
      ((< hx 0) ; x < -0.5
        (let* (
          (z (* (+ one x) 0.5f0))
          (p (* z (+ ps0
             (* z (+ ps1
             (* z (+ ps2
             (* z (+ ps3
             (* z (+ ps4
             (* z ps5))))))))))) )
          (q (+ one (* z
             (+ qs1 (* z
             (+ qs2 (* z
             (+ qs3 (* z qs4)))))))) )
          (s (float32-sqrt z))
          (r (/ p q))
          (w (- (* r s) pio2_lo))
          )
          (- fpi (* 2f0 (+ s w))) ) )
      (t ; x > 0.5
        (let* (
          (z  (* (- one x) 0.5f0))
          (s  (float32-sqrt z))
          (df (encode-float32 (logand (decode-float32 s) #xfffff000)))
          (c  (/ (- z (* df df)) (+ s df)))
          (p  (* z (+ ps0
              (* z (+ ps1
              (* z (+ ps2
              (* z (+ ps3
              (* z (+ ps4
              (* z ps5))))))))))) )
          (q  (+ one (* z
              (+ qs1 (* z
              (+ qs2 (* z
              (+ qs3 (* z qs4)))))))) )
          (r  (/ p q))
          (w  (+ (* r s) c))
          )
          (* 2f0 (+ df w)) ) )) ) )
