;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64- rem pi/2
;;; arch/generic/lisp/float64/gen-floa64-rem-pio2.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-rem-pio2.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-rem-pio2
;
(in-package :si)

#|
 *
 From fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* ef_rem_pio2.c -- float version of e_rem_pio2.c
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
 *
 */

/* __ieee754_rem_pio2f(x,y)
 *
 * return the remainder of x rem pi/2 in y[0]+y[1]
 * use __kernel_rem_pio2f()
 */

#include "fdlibm.h"

/*
 * Table of constants for 2/pi, 396 Hex digits (476 decimal) of 2/pi
 */
#ifdef __STDC__
static const __int32_t two_over_pi[] = {
#else
static __int32_t two_over_pi[] = {
#endif
#xA2, #xF9, #x83, #x6E, #x4E, #x44, #x15, #x29, #xFC,
#x27, #x57, #xD1, #xF5, #x34, #xDD, #xC0, #xDB, #x62,
#x95, #x99, #x3C, #x43, #x90, #x41, #xFE, #x51, #x63,
#xAB, #xDE, #xBB, #xC5, #x61, #xB7, #x24, #x6E, #x3A,
#x42, #x4D, #xD2, #xE0, #x06, #x49, #x2E, #xEA, #x09,
#xD1, #x92, #x1C, #xFE, #x1D, #xEB, #x1C, #xB1, #x29,
#xA7, #x3E, #xE8, #x82, #x35, #xF5, #x2E, #xBB, #x44,
#x84, #xE9, #x9C, #x70, #x26, #xB4, #x5F, #x7E, #x41,
#x39, #x91, #xD6, #x39, #x83, #x53, #x39, #xF4, #x9C,
#x84, #x5F, #x8B, #xBD, #xF9, #x28, #x3B, #x1F, #xF8,
#x97, #xFF, #xDE, #x05, #x98, #x0F, #xEF, #x2F, #x11,
#x8B, #x5A, #x0A, #x6D, #x1F, #x6D, #x36, #x7E, #xCF,
#x27, #xCB, #x09, #xB7, #x4F, #x46, #x3F, #x66, #x9E,
#x5F, #xEA, #x2D, #x75, #x27, #xBA, #xC7, #xEB, #xE5,
#xF1, #x7B, #x3D, #x07, #x39, #xF7, #x8A, #x52, #x92,
#xEA, #x6B, #xFB, #x5F, #xB1, #x1F, #x8D, #x5D, #x08,
#x56, #x03, #x30, #x46, #xFC, #x7B, #x6B, #xAB, #xF0,
#xCF, #xBC, #x20, #x9A, #xF4, #x36, #x1D, #xA9, #xE3,
#x91, #x61, #x5E, #xE6, #x1B, #x08, #x65, #x99, #x85,
#x5F, #x14, #xA0, #x68, #x40, #x8D, #xFF, #xD8, #x80,
#x4D, #x73, #x27, #x31, #x06, #x06, #x15, #x56, #xCA,
#x73, #xA8, #xC9, #x60, #xE2, #x7B, #xC0, #x8C, #x6B,
};

/* This array is like the one in e_rem_pio2.c, but the numbers are
   single precision and the last 8 bits are forced to 0.  */
#ifdef __STDC__
static const __int32_t npio2_hw[] = {
#else
static __int32_t npio2_hw[] = {
#endif
#x3fc90f00, #x40490f00, #x4096cb00, #x40c90f00, #x40fb5300, #x4116cb00,
#x412fed00, #x41490f00, #x41623100, #x417b5300, #x418a3a00, #x4196cb00,
#x41a35c00, #x41afed00, #x41bc7e00, #x41c90f00, #x41d5a000, #x41e23100,
#x41eec200, #x41fb5300, #x4203f200, #x420a3a00, #x42108300, #x4216cb00,
#x421d1400, #x42235c00, #x4229a500, #x422fed00, #x42363600, #x423c7e00,
#x4242c700, #x42490f00
};

/*
 * invpio2:  24 bits of 2/pi
 * pio2_1:   first  17 bit of pi/2
 * pio2_1t:  pi/2 - pio2_1
 * pio2_2:   second 17 bit of pi/2
 * pio2_2t:  pi/2 - (pio2_1+pio2_2)
 * pio2_3:   third  17 bit of pi/2
 * pio2_3t:  pi/2 - (pio2_1+pio2_2+pio2_3)
 */
|#
(defun float32-rem-pio2 (x)
    (declare (values fixnum single-float single-float))
    (declare (type single-float x))
  (prog* (
    (half    #+nil  5.0000000000e-01 #.(encode-float32 #x3f000000))
    (two8    #+nil  2.5600000000e+02 #.(encode-float32 #x43800000))
    (invpio2 #+nil  6.3661980629e-01 #.(encode-float32 #x3f22f984))
    (pio2_1  #+nil  1.5707855225e+00 #.(encode-float32 #x3fc90f80))
    (pio2_1t #+nil  1.0804334124e-05 #.(encode-float32 #x37354443))
    (pio2_2  #+nil  1.0804273188e-05 #.(encode-float32 #x37354400))
    (pio2_2t #+nil  6.0770999344e-11 #.(encode-float32 #x2e85a308))
    (pio2_3  #+nil  6.0770943833e-11 #.(encode-float32 #x2e85a300))
    (pio2_3t #+nil  6.1232342629e-17 #.(encode-float32 #x248d3132))
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    )
    (cond
      ;; |x| ~<= pi/4 , no need for reduction
      ((<= ix #x3f490fd8)
        (return (values 0 x 0f0)) )

      ;; |x| < 3pi/4, special case with n=+-1
      ((< ix #x4016cbe4)
        (if (> hx 0)
          (let ((z (- x pio2_1)))
            (if (not (eql (logand ix #xfffffff0) #x3fc90fd0))
               ;; 24+24 bit pi OK
               (let* ((y0 (- z  pio2_1t))
                      (y1 (- (- z y0) pio2_1t)) )
                 (return (values 1 y0 y1)) )
               ;; near pi/2, use 33+33+53 bit pi *
               (let* ((z (- z pio2_2))
                      (y0 (- z pio2_2t))
                      (y1 (- (- z y0) pio2_2t)) )
                 (return (values 1 y0 y1)) )) )
          ;; negative x
          (let ((z (+ x pio2_1)))
            (if (not (eql (logand ix #xfffffff0)#x3fc90fd0))
                ;; 24+24 bit pi OK
                (let* ((y0 (+ z  pio2_1t))
                       (y1 (+ (- z y0) pio2_1t)) )
                  (return (values -1 y0 y1)) )
                ;; near pi/2, use 24+24+24 bit pi
                (let* ((z  (+ z pio2_2))
                       (y0 (+ z  pio2_2t))
                       (y1 (+ (- z y0) pio2_2t)) )
                  (return (values -1 y0 y1)) )) )) )

      ;; |x| ~<= 2^7*(pi/2), medium size
      ((<= ix #x43490f80)
        (let* (
          (tt  (encode-float32 ix))
          (n   (truncate (+ (* tt invpio2) half)))
          (fn  (float n))
          (r   (- tt (* fn pio2_1)))
          (w   (* fn pio2_1t))   ; 1st round good to 40 bit
          (y0 (- r w))
          )
        ;; quick check no cancellation
        (unless (and (< n 32)
                     (not (eql (logand ix #xffffff00)
                               (elt +npio2_hw+ (- n 1)) )))
          (let* ((j    (ash ix -23))
                 (high (decode-float32 y0))
                 (i    (- j (logand (ash high -23) #xff))) )
            ;; 2nd iteration needed, good to 57
            (when (> i 8)
              (let* (
                (tt  r)
                (w   (* fn pio2_2))
                (r   (- tt w))
                (w   (- (* fn pio2_2t) (- tt r w)))
                )
                (setq y0 (- r w))
                (let* (
                  (high (decode-float32 y0))
                  (i    (- j (logand (ash high -23) #xff)))
                  )
                  ;; 3rd iteration need, 74 bits acc
                  (when (> i 25)
                    ;; will cover all possible cases
                    (let* (
                      (tt  r)
                      (w  (* fn pio2_3))
                      (r  (- tt w))
                      (w  (- (* fn pio2_3t) (- tt r w)))
                      )
                      (setq y0 (- r w)) ) ) ) )) ))
        (let ((y1 (- r y0 w)))
          (if (< hx 0)
              (return (values (- n) (- y0) (- y1)))
              (return (values n y0 y1)) ) ) ) )

    ;; Inf or NaN?
    ((>= ix #x7f800000)
      (let ((nan (- x x)))
        (return (values 0 nan nan)) ) )

    ;; all other (large) arguments
    (t
      ;; set z = scalbn(|x|,ilogb(x)-7)
      (let* (
        (e0  (- (ash ix -23) -134)) ; e0 = ilogb(z)-7
        (z   (encode-float32 (- ix (ash e0 23))))
        (tx0 (ftruncate z))
        (z   (* (- z tx0) two8))
        (tx1 (ftruncate z))
        (z   (* (- z tx0) two8))
        (tx2 z)
        )
        (multiple-value-bind (n y0 y1)
            (float32-krem-pio2 tx0 tx1 tx2 e0)
          (if (< hx 0)
              (return (values (- n) (- y0) (- y1)))
              (return (values n y0 y1)) ) ) ) )) ) )
