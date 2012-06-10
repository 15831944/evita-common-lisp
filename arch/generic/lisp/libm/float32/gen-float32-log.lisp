;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - log
;;; arch/generic/lisp/macth/gen-math-32-log.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-log.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-log
;
(in-package :si)

#|
 *  From fdlibm (http://www.netlib.org/fdlibm/)
/* ef_log.c -- float version of e_log.c.
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
(defun float32-log (x)
    (declare (values single-float))
    (declare (type single-float x))
  (prog* (
    (ln2_hi #+nil   6.9313812256e-01    #.(encode-float32 #x3f317180))
    (ln2_lo #+nil   9.0580006145e-06    #.(encode-float32 #x3717f7d1))
    (two25  #+nil    3.355443200e+07    #.(encode-float32 #x4c000000))
    (Lg1    #+nil 6.6666668653e-01      #.(encode-float32 #x3F2AAAAB))
    (Lg2    #+nil 4.0000000596e-01      #.(encode-float32 #x3ECCCCCD))
    (Lg3    #+nil 2.8571429849e-01      #.(encode-float32 #x3E924925))
    (Lg4    #+nil 2.2222198546e-01      #.(encode-float32 #x3E638E29))
    (Lg5    #+nil 1.8183572590e-01      #.(encode-float32 #x3E3A3325))
    (Lg6    #+nil 1.5313838422e-01      #.(encode-float32 #x3E1CD04F))
    (Lg7    #+nil 1.4798198640e-01      #.(encode-float32 #x3E178897))
    ;;
    (zero 0f0)
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    (k 0)
    )
    ;;
    (when (eql ix 0)
      ;; log(+-0)=-inf
      (return (/ (- two25) zero)) )

    (when (< hx 0)
      ;; log(-#) = NaN
      (return (/ (- x x) zero)) )

    (when (< ix #x007fffff)
      ;; subnormal number, scale up x
      (decf k 25)
      (setq x (* x two25))
      (setq hx (decode-float32 x))
      (setq ix (logand hx #x7fffffff)) )

    (incf k (- (ash ix -23) 127))
    (setq ix (logand ix #x007fffff))

    ;; normalize x or x/2
    (let ((i (logand (+ ix (ash #x95f64 3)) #x800000)))
      (setq x (encode-float32 (logior ix (logxor i #x3f800000))))
      (incf k (ash i -23)) )

    (let ((f (- x 1)))
      (when (< (logand #x007fffff (+ 15 ix)) 16)
        ;; |f| < 2**-20
        (when (eql f zero)
          (if (eql k 0)
              (return zero)
              (let ((dk (float k)))
                (return (+ (* dk ln2_hi) (* dk ln2_lo))) )) )

        (let ((R (* f f (- 0.5 (* 0.33333333333333333 f)))))
          (if (eql k 0)
              (return (- f R))
            (let ((dk (float k)))
              (return (- (* dk ln2_hi) (- (- r (* dk ln2_lo)) f))) )) ))

      (let* (
        (s  (/ f (+ 2 f)))
        (dk (float k))
        (z  (* s s))
        (i0 (- ix (ash #x6147a 3)))
        (w  (* z z))
        (j  (- (ash #x6b851 3) ix))
        (t1 (* w (+ lg2 (* w (+ lg4 (* w lg6))))))
        (t2 (* z (+ lg1 (* w (+ lg3 (* w (+ lg5 (* w lg7))))))))
        (i  (logior i0 j))
        (R  (+ t2 t1))
        )
        (if (> i 0)
            (let ((hfsq (* 0.5f0 f f)))
              (if (eql k 0)
                  (return (- f (- hfsq (* s (+ hfsq r)))))
                  (return (- (* dk ln2_hi)
                             (- (- hfsq (+ (* s (+ hfsq r))
                                (* dk ln2_lo))) f )))) )
            (if (eql k 0)
                (return (- f (* s (- f r))))
                (return (- (* dk ln2_hi)
                           (- (- (* s (- f r))
                              (* dk ln2_lo))
                              f ))))) ) ) ) )
