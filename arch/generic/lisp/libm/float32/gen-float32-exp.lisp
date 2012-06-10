;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32 exp
;;; arch/generic/lisp/macth/gen-math-f32-exp.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-exp.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-exp
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* ef_exp.c -- float version of e_exp.c.
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
(defun float32-exp (x)
    (declare (values single-float))
    (declare (type single-float x))
 (prog* (
    (zero    0f0)
    (one     1f0)
    (halF_0  0.5f0)
    (halF_1  -0.5f0)
    (huge    1.0f+30)
    (twom100 #+nil  7.8886090522e-31 #.(encode-float32 #x0d800000)) ; 2**-100
    (ln2HI_0 #+nil  6.9313812256e-01 #.(encode-float32 #x3f317180))
    (ln2HI_1 #+nil -6.9313812256e-01 #.(encode-float32 #xbf317180))
    (ln2LO_0 #+nil  9.0580006145e-06 #.(encode-float32 #x3717f7d1))
    (ln2LO_1 #+nil -9.0580006145e-06 #.(encode-float32 #xb717f7d1))
    (invln2  #+nil  1.4426950216e+00 #.(encode-float32 #x3fb8aa3b))
    (P1      #+nil  1.6666667163e-01 #.(encode-float32 #x3e2aaaab))
    (P2      #+nil -2.7777778450e-03 #.(encode-float32 #xbb360b61))
    (P3      #+nil  6.6137559770e-05 #.(encode-float32 #x388ab355))
    (P4      #+nil -1.6533901999e-06 #.(encode-float32 #xb5ddea0e))
    (P5      #+nil  4.1381369442e-08 #.(encode-float32 #x3331bb4c))
    ;;
    (sx (decode-float32 x))
    (hx (logand sx #x7fffffff))  ; high word of |x|
    )
    ;; filter out non-finite argument
    (cond
      ;; +-inf -> {inf,0}
      ((eql hx #x7f800000)
        (return (if (> x 0) x zero)) )

      ;; NaN -> NaN
      ((> hx #x7f800000)
        (return (+ x x)) )

      ;; (exp 88.72283) -> overflow
      ((> sx #x42b17217)
        (return (* huge huge)) )

      ;; (log REAL_FLT_MIN) -> underflow
      ((and (< sx 0) (> hx #x42aeac50))
        (return (* twom100 twom100)) ))

    ;; argument reduction
    (let ((k 0)
          (hi zero)
          (lo zero) )
        (declare (type single-float hi lo))
      (cond
        ;; if  |x| > 0.5 ln2
        ((> hx #x3eb17218)
          (if (< hx #x3F851592)   ; and |x| < 1.5 ln2
              (if (> sx 0)
                (setq hi (- x ln2HI_0) lo ln2LO_0 k +1)
                (setq hi (- x ln2HI_1) lo ln2LO_1 k -1) )
              (let* ((halF (if (> sx 0) halF_0 halF_1))
                     (kk  (truncate (+ (* invln2 x) halF)))
                     (tt  (float kk)) )
                ;; tt*ln2HI is exact here
                (setq hi (- x (* tt ln2HI_0)) lo (* tt ln2LO_0) k kk) ))
            (setq x (- hi lo)) )

        ;; when |x|<2**-28
        ((< hx #x31800000)
          ;; trigger inexact
          (when (> (+ huge x) one)
            (return (+ one x)) ) ))

      ;; x is now in primary range
      (let* ((tt  (* x x))
             (c   (- x (* tt (+ p1
                       (* tt (+ p2
                       (* tt (+ p3
                       (* tt (+ p4
                       (* tt p5) ))))))))) ))
        (declare (type single-float tt c))
        (when (eql k 0)
          (return (- one (- (/ (* x c) (- c 2f0)) x))) )

        (let* ((y (- one (- (- lo (/ (* x c) (- 2f0 c))) hi)))
               (hy (decode-float32 y)) )
          ;; add k to y's exponent
          (if (>= k -125)
              (return (encode-float32 (+ hy (ash k 23))))
              (return (* (encode-float32 (+ hy (ash (+ k 100) 23)))
                         twom100 ))) ) ) ) ) )
