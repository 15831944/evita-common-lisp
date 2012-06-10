;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - log1p
;;; arch/generic/lisp/float/float32/gen-float32-log1p.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-log1p.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-log1p
;;; for acosh, asinh.
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
/* sf_log1p.c -- float version of s_log1p.c.
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
(defun float32-log1p (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (ln2_hi     #+nil 6.9313812256e-01 #.(encode-float32 #x3f317180))
    (ln2_lo     #+nil 9.0580006145e-06 #.(encode-float32 #x3717f7d1))
    (two25      #+nil  3.355443200e+07 #.(encode-float32 #x4c000000))
    (Lp1        #+nil 6.6666668653e-01 #.(encode-float32 #x3F2AAAAB))
    (Lp2        #+nil 4.0000000596e-01 #.(encode-float32 #x3ECCCCCD))
    (Lp3        #+nil 2.8571429849e-01 #.(encode-float32 #x3E924925))
    (Lp4        #+nil 2.2222198546e-01 #.(encode-float32 #x3E638E29))
    (Lp5        #+nil 1.8183572590e-01 #.(encode-float32 #x3E3A3325))
    (Lp6        #+nil 1.5313838422e-01 #.(encode-float32 #x3E1CD04F))
    (Lp7        #+nil 1.4798198640e-01 #.(encode-float32 #x3E178897))
    ;;
    (zero 0f0)
    ;;
    (hx (decode-float32 x))
    (ax (logand hx #x7fffffff))
    (k 1)
    (f x)
    (hu 1)
    (c zero)
    )
    ;; Inf or Nan?
    (when (>= ax #x7F800000)
      (return (+ x x)) )

    ;; x < 0.41422
    (when (< hx #x3ed413d7)
      (cond
        ((>= ax #x3f800000)  ;; x <= -1.0
          (return (if (eql x 1f0)
                       ;; log1p(-1)=+inf
                       (/ (- two25 zero))
                       ;; log1p(x<-1)=NaN
                       (/ (- x x) (- x x)) )) )

        ((< ax #x31000000)  ;; |x| < 2**-29
          (when (> (+ two25 x) zero)    ;; raise inexact
            (when (< ax #x24800000)     ;; |x| < 2**-54
               (return x) )
            (return (- x  (* x x 0.5f0))) ) )
        ((or (> hx 0) (<= hx #.(- #xbe95f61f (ash 1 32))))
          ;; -0.2929<x<0.41422
          (setq k 0) )))

    (unless (eql k 0)
      (multiple-value-bind (u hu)
        (if (< hx #x5a000000)
            (let* ((u  (+ x 1))
                   (hu (decode-float32 u))
                   (k  (- (ash hu -23) 127)) )
              ;; correction term
              (setq c (/ (if (> k 0) (- 1 (- u x)) (- x (- u 1))) u))
              (values u hu) )
            (values x hx) )

        (setq k (- (ash hu -23) 127))
        (setq hu (logand hu #x007fffff))

        (if (< hu #x3504f7)
            ;;; normalize u
            (setq u (encode-float32 (logior hu #x3f800000)))
            (progn
              (incf k)
              ;; normalize u/2
              (setq u (encode-float32 (logior hu hu #x3f000000)))
              (setq hu (ash (- #x00800000 hu) -2)) ))
        (setq f (- u 1)) ))

    (let* ((hfsq (* 0.5f0 f f)))
      (cond
        ;; |f| >= 2**-20
        ((not (eql hu 0))
          (let* ((s (/ f (+ f 2)))
                 (z (* s s))
                 (R (* z (+ lp1
                    (* z (+ lp2
                    (* z (+ lp3
                    (* z (+ lp4
                    (* z (+ lp5
                    (* z (+ lp6
                    (* z lp7))))))))))))) ))
            (if (eql k 0)
                (return (- f (- hfsq (* s (+ hfsq r)))))
                (return (- (* k ln2_hi)
                           (- (- hfsq (+ (* s (+ hfsq r))
                              (+ (* k ln2_lo) c))) f)) )) ) )
        ((not (eql f zero))
          (let ((R  (* hfsq (- 1 (* 0.66666666666666666 f)))))
            (if (eql k 0)
                (return (- f R))
                (return (- (* k ln2_hi) (- (- r (+ (* k ln2_lo) c)) f))) ) ) )
        ((eql k 0)
          (return zero) )
        (t
          (return (+ (* k ln2_hi) (+ (* k ln2_lo)) c)) )) ) ) )
