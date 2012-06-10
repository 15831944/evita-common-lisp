;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - exp
;;; arch/generic/lisp/float32/gen-float32-expm1.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-expm1.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-expm1
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* sf_expm1.c -- float version of s_expm1.c.
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
(defun float32-expm1 (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (one        #+nil 1.0e0             #.(encode-float32 #x3F800000))
    (huge       #+nil 1.0e+30           #.(encode-float32 #x7149F2C9))
    (tiny       #+nil 1.0e-30           #.(encode-float32 #x0DA24261))
    (ln2_hi     #+nil 6.9313812256e-01  #.(encode-float32 #x3f317180))
    (ln2_lo     #+nil 9.0580006145e-06  #.(encode-float32 #x3717f7d1))
    (invln2     #+nil 1.4426950216e+00  #.(encode-float32 #x3fb8aa3b))
     ;; scaled coefficients related to expm1
    (Q1         #+nil -3.3333335072e-02 #.(encode-float32 #xbd088889))
    (Q2         #+nil 1.5873016091e-03  #.(encode-float32 #x3ad00d01))
    (Q3         #+nil -7.9365076090e-05 #.(encode-float32 #xb8a670cd))
    (Q4         #+nil 4.0082177293e-06  #.(encode-float32 #x36867e54))
    (Q5         #+nil -2.0109921195e-07 #.(encode-float32 #xb457edbb))
    ;;
    (sx (decode-float32 x))
    (hx (logand sx #x7fffffff))
    (k 0)
    (c 0f0)
    )
    (declare (type (signed-byte 32) sx))
    (declare (type (unsigned-byte 31) hx))
    (declare (type single-float c y))
    (declare (type fixnum k))
    ;;

    ;; filter out huge and non-finite argument

    ;; if |x|>=27*ln2
    (when (>= hx #x4195b844)
      (cond
        ;; exp(+-inf) -> {inf,-1}
        ((eql hx #x7F800000)
          (return (if (> sx 0) x -1f0)) )

        ;; exp(NaN) -> NaN
        ((> hx #x7F800000)
          (return (+ x x)) )

        ;; if x>=o_threshold -> overflow
        ;; o_threshold=FLT_UWORD_LOG_MAX
        ((>= sx #x42b17217)
          (return (* huge huge)) )

        ((< sx 0)
          ;; x < -27*ln2, return -1.0 with inexact
          (when (< (+ x tiny) 0f0)        ;; raise inexact
            ;; return -1
            (return (- tiny one)) ) )))

    ;; argument reduction
    (cond
      ;; if  |x| > 0.5 ln2 
      ((> hx #x3eb17218)
        (multiple-value-bind (hi lo kk)
            (cond
              ;; and |x| >= 1.5 ln2
              ((>= hx #x3F851592)
                (let* ((halF (if (> sx 0) +0.5f0 -0.5f0))
                       (kk (truncate (+ (* invln2 x) halF)))
                       (tt (float kk)) )
                  ;; tt*ln2_hi is exact here
                  (values (- x (* tt ln2_hi)) (* tt ln2_lo) kk) ) )
              ((< sx 0)
                (values (- x ln2_hi) ln2_lo 1) )
              (t
                (values (+ x ln2_hi) (- ln2_lo) -1) ))
            (declare (type single-float hi lo))
            (declare (type fixnum kk))
          (setq k kk)
          (setq x  (- hi lo))
          (setq c  (- (- hi x) lo)) ) )

     ;; when |x|<2**-25, return x
     ((< hx  #x33000000)
       (let ((tt (+ huge x)))
         (return (- x (- tt (+ huge x)))) ) ))

    ;; x is now in primary range
    (let* ((hfx (* 0.5f0 x))
           (hxs (* x hfx))
           (r1  (+ one (* hxs
                (+ q1 (* hxs
                (+ q2 (* hxs
                (+ q3 (* hxs
                (+ q4 (* hxs q5)))))))))) )
           (tt  (- 3.0f0 (* r1 hfx)))
           (e    (* hxs (/ (- r1 tt) (- 6.0f0 (* x tt))))) )

        (when (eql k 0)
          ;; c is 0
          (return (- x (- (* x e) hxs))) )

        (setq e  (- (* x (- e c)) c))
        (decf e hxs)

        (cond
          ((eql k -1)
            (return (- (* 0.5f0 (- x e)) 0.5f0)) )

          ((eql k 1)
            (if (< x -0.25f0)
                (return (* -2.0f0 (- e (+ x 0.5f0))))
                (return (+ one (* 2.0f0 (- x e)))) ) )

          ((or (<= k -2) (> k 56))
            ;; suffice to return exp(x)-1
            (let* ((iy (decode-float32 (- one (- e x))))
                   ;; add k to y's exponent
                   (y (encode-float32 (+ iy (ash k 23)))) )
              (return (- y one)) ) )

          ((< k 23)
            ;; tt=1-2^-k
            (let* ((kk k)
                   (tt (encode-float32 (- #x3f800000 (ash #x1000000 (- kk)))))
                   (yy (- tt (- e x)))
                   (iy (decode-float32 yy)) )
                (declare (type (integer 2 22) kk))
              ;; add k to y's exponent
              (return (encode-float32 (+ iy (ash k 23)))) ) )
          (t
            (let* ((kk k)
                   (tt (encode-float32 (ash (- #x7f kk) 23)))
                   (yy (- x (+ e tt)))
                   (y2 (+ yy 1f0))
                   (iy (decode-float32 y2)) )
                (declare (type (integer 23 56) kk))
              ;; add k to y's exponent
              (return (encode-float32 (+ iy (ash kk 23)))) ) )) ) ) )
