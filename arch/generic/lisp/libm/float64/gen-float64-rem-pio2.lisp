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
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-rem-pio2.lisp#1 $
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

 /* @(#)e_rem_pio2.c 5.1 93/09/24 *
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
 *

 * __ieee754_rem_pio2(x,y)
 *
 * return the remainder of x rem pi/2 in y[0]+y[1]
 * use __kernel_rem_pio2()
 *
|#


;;; Table of constants for 2/pi, 396 Hex digits (476 decimal) of 2/pi
(defconstant +two-over-pi+ (make-array 66 :element-type '(unsigned-byte 32)
    :initial-contents #(
    #xA2F983 #x6E4E44 #x1529FC #x2757D1 #xF534DD #xC0DB62
    #x95993C #x439041 #xFE5163 #xABDEBB #xC561B7 #x246E3A
    #x424DD2 #xE00649 #x2EEA09 #xD1921C #xFE1DEB #x1CB129
    #xA73EE8 #x8235F5 #x2EBB44 #x84E99C #x7026B4 #x5F7E41
    #x3991D6 #x398353 #x39F49C #x845F8B #xBDF928 #x3B1FF8
    #x97FFDE #x05980F #xEF2F11 #x8B5A0A #x6D1F6D #x367ECF
    #x27CB09 #xB74F46 #x3F669E #x5FEA2D #x7527BA #xC7EBE5
    #xF17B3D #x0739F7 #x8A5292 #xEA6BFB #x5FB11F #x8D5D08
    #x560330 #x46FC7B #x6BABF0 #xCFBC20 #x9AF436 #x1DA9E3
    #x91615E #xE61B08 #x659985 #x5F14A0 #x68408D #xFFD880
    #x4D7327 #x310606 #x1556CA #x73A8C9 #x60E27B #xC08C6B
 ) ) )

(defconstant +npio2-hw+ (make-array 32 :element-type '(unsigned-byte 32)
    :initial-contents #(
    #x3FF921FB #x400921FB #x4012D97C #x401921FB #x401F6A7A #x4022D97C
    #x4025FDBB #x402921FB #x402C463A #x402F6A7A #x4031475C #x4032D97C
    #x40346B9C #x4035FDBB #x40378FDB #x403921FB #x403AB41B #x403C463A
    #x403DD85A #x403F6A7A #x40407E4C #x4041475C #x4042106C #x4042D97C
    #x4043A28C #x40446B9C #x404534AC #x4045FDBB #x4046C6CB #x40478FDB
    #x404858EB #x404921FB
 ) ) )

#|
 *
 * invpio2:  53 bits of 2/pi
 * pio2_1:   first  33 bit of pi/2
 * pio2_1t:  pi/2 - pio2_1
 * pio2_2:   second 33 bit of pi/2
 * pio2_2t:  pi/2 - (pio2_1+pio2_2)
 * pio2_3:   third  33 bit of pi/2
 * pio2_3t:  pi/2 - (pio2_1+pio2_2+pio2_3)
 *
|#
(defun float64-rem-pio2 (x)
    (declare (values fixnum double-float double-float))
    (declare (type double-float x))
  (prog (
    (half       #+nil 5.00000000000000000000e-01
                #.(encode-float64 #x3FE00000 #x00000000) )
    (two24      #+nil 1.67772160000000000000e+07
                #.(encode-float64 #x41700000 #x00000000) )
    (invpio2    #+nil 6.36619772367581382433e-01
                #.(encode-float64 #x3FE45F30 #x6DC9C883) )
    (pio2_1     #+nil 1.57079632673412561417e+00
                #.(encode-float64 #x3FF921FB #x54400000) )
    (pio2_1t    #+nil 6.07710050650619224932e-11
                #.(encode-float64 #x3DD0B461 #x1A626331) )
    (pio2_2     #+nil 6.07710050630396597660e-11
                #.(encode-float64 #x3DD0B461 #x1A600000) )
    (pio2_2t    #+nil 2.02226624879595063154e-21
                #.(encode-float64 #x3BA3198A #x2E037073) )
    (pio2_3     #+nil 2.02226624871116645580e-21
                #.(encode-float64 #x3BA3198A #x2E000000) )
    (pio2_3t    #+nil 8.47842766036889956997e-32
                #.(encode-float64 #x397B839A #x252049C1) )
    )
    (multiple-value-bind (hx lx) (decode-float64 x)
    (let ((ix (logand hx #x7fffffff)))

      ;; |x| ~<= pi/4 , no need for reduction
      (when (<= ix #x3fe921fb)
        (return (values 0 x 0d0)) )

      ;; |x| < 3pi/4, special case with n=+-1
      (when (< ix #x4002d97c)
        (if (> hx 0)
            (let ((z (- x pio2_1)))
              (if (not (eql ix #x3ff921fb))
                  ;; 33+53 bit pi is good enough
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
              (if (not (eql ix #x3ff921fb))
                  ;; 33+53 bit pi is good enough
                  (let* ((y0 (+ z  pio2_1t))
                         (y1 (+ (- z y0) pio2_1t)) )
                    (return (values -1 y0 y1)) )
                  ;; near pi/2, use 33+33+53 bit pi
                  (let* ((z  (+ z pio2_2))
                         (y0 (+ z  pio2_2t))
                         (y1 (+ (- z y0) pio2_2t)) )
                    (return (values -1 y0 y1)) )) )))

      ;; |x| ~<= 2^19*(pi/2), medium size
      (when (<= ix #x413921fb)
        (let* ((tt (float64-abs x))
               (n  (truncate (+ (* tt invpio2) half)))
               (fn (float n 0d0))
               (r  (- tt (* fn pio2_1)))
               (w  (* fn pio2_1t))   ; 1st round good to 85 bit *
               (y0 (- r w)) )
          ;; quick check no cancellation
          (unless (and (< n 32) (not (eql ix (elt +npio2-hw+ (- n 1)))))
            (let* ((j (ash ix -20))
                   (high (decode-float64 y0))
                   (i (- j (logand (ash high -20) #x7ff))) )
              ;; 2nd iteration needed, good to 118 *
              (when (> i 16)
                (setq tt  r)
                (setq w  (* fn pio2_2))
                (setq r  (- tt w))
                (setq w  (- (* fn pio2_2t) (- (- tt r) w)))
                (setq y0 (- r w))
                (setq high (decode-float64 y0))
                (setq i (- j (logand (ash high -20) #x7ff)))

                ;; 3rd iteration need, 151 bits acc *
                (when (> i 49)
                  (setq tt  r)    ; will cover all possible cases *
                  (setq w  (* fn pio2_3))
                  (setq r  (- tt w))
                  (setq w  (- (* fn pio2_3t) (- (- tt r) w)))
                  (setq y0 (- r w)) )) ))
          (let ((y1 (- (- r y0) w)))
            (if (< hx 0)
                (return (values (- n) (- y0) (- y1)))
              (return (values n y0 y1)) ) ) ))

     ;; all other (large) arguments
     (when (>= ix #x7ff00000)  ; x is inf or NaN *
       (let ((nan (- x x)))
         (return (values 0 nan nan)) ))

     ;; set z = scalbn(|x|,ilogb(x)-23)
     (let* ((e0  (- (ash ix -20) 1046)) ;; e0 = ilogb(z)-23
            (z0  (encode-float64 (- ix (ash e0 20)) lx))
            (tx0 (float (truncate z0) 0d0))
            (z1  (* (- z0 tx0) two24))
            (tx1 (float (truncate z1) 0d0))
            (z2  (* (- z1 tx1) two24))
            (tx2  z2) )
        (multiple-value-bind (n y0 y1)
            (let ((nx
                    ;; skip zero term
                    (cond
                      ((not (zerop tx2)) 3)
                      ((not (zerop tx1)) 2)
                      ((not (zerop tx0)) 1)
                      (t 0) ) ))
              (let ((tx (make-array 3 :element-type 'double-float)))
                (setf (elt tx 0) tx0)
                (setf (elt tx 1) tx1)
                (setf (elt tx 2) tx2)
                (float64-kernel-rem-pio2 tx e0 nx 2 +two-over-pi+) ) )
          (if (< hx 0)
              (return (values (- n) (- y0) (- y1)))
            (return (values n y0 y1)) ) ) ) ) ) ) )
