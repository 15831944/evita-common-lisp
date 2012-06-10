;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - Kernel tan
;;; arch/generic/lisp/math/gen-math-f32-ktan.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-ktan.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-kernel-tan
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* kf_tan.c -- float version of k_tan.c
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
(defun float32-kernel-tan (x y iy)
    (declare (values single-float))
    (declare (type single-float x y))
    (declare (type fixnum iy))
  (prog* (
    (one    #+nil 1.0000000000e+00 #.(encode-float32 #x3f800000))
    (pio4   #+nil 7.8539812565e-01 #.(encode-float32 #x3f490fda))
    (pio4lo #+nil 3.7748947079e-08 #.(encode-float32 #x33222168))
    ;;
    (T0     #+nil 3.3333334327e-01 #.(encode-float32 #x3eaaaaab))
    (T1     #+nil 1.3333334029e-01 #.(encode-float32 #x3e088889))
    (T2     #+nil 5.3968254477e-02 #.(encode-float32 #x3d5d0dd1))
    (T3     #+nil 2.1869488060e-02 #.(encode-float32 #x3cb327a4))
    (T4     #+nil 8.8632395491e-03 #.(encode-float32 #x3c11371f))
    (T5     #+nil 3.5920790397e-03 #.(encode-float32 #x3b6b6916))
    (T6     #+nil 1.4562094584e-03 #.(encode-float32 #x3abede48))
    (T7     #+nil 5.8804126456e-04 #.(encode-float32 #x3a1a26c8))
    (T8     #+nil 2.4646313977e-04 #.(encode-float32 #x398137b9))
    (T9     #+nil 7.8179444245e-05 #.(encode-float32 #x38a3f445))
    (T10    #+nil 7.1407252108e-05 #.(encode-float32 #x3895c07a))
    (T11    #+nil -1.8558637748e-05 #.(encode-float32 #xb79bae5f))
    (T12    #+nil 2.5907305826e-05 #.(encode-float32 #x37d95384))
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    )
    ;; x < 2**-28
    (when (< ix #x31800000)
      ;; generate inexact
      (when (eql (truncate x) 0)
        (cond
          ((eql (logior ix (1+ iy)) 0)
            (return (/ one (encode-float32 ix))) )
          ((eql iy 1)
            (return x) )
          (t
            (return (/ (- one) x)) ))))

    ;; |x|>=0.6744
    (when (>= ix #x3f2ca140)
      (when (< hx 0) (setq x (- x) y (- y)))
      (let ((z (- pio4 x))
            (w (- pio4lo y)) )
        (setq x (+ z w))
        (setq y 0f0) ))

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

      (when (>= ix #x3f2ca140)
        (let ((v (float iy)))
          (return (float (* (- 1 (logand (ash hx -30) 2))
                            (- v (* 2f0 (- x (- (/ (* w w) (+ w v)) r))))))) ))

      (when (eql iy 1) (return w))

      ;; if allow error up to 2 ulp, simply return -1.0/(x+r) here

      ;; compute -1.0/(x+r) accurately
      (let* (
        (z  w)
        (i  (decode-float32 z))
        (z  (encode-float32 (logand i #xfffff000)))
        (v  (- r (- z z)))   ; z+v = r+x
        (a  (/ -1f0 w))      ; a = -1.0/w
        (i  (decode-float32 a))
        (tt (encode-float32 (logand i #xfffff000)))
        (s  (+ 1f0 (* tt z)))
        )
        (return (+ tt (* a (+ s (* tt v))))) ) ) ) )
