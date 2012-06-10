;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - acos
;;; arch/generic/lisp/macth/gen-r12-acos64.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-acos.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     facos64
;
(in-package :si)

#|
 * 
 *  From fdlibm (http://www.netlib.org/fdlibm/)
 *  @(#)e_acos.c 1.3 95/01/18
 * 
 *  Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *  Developed at SunSoft, a Sun Microsystems, Inc. business.
 *  Permission to use, copy, modify, and distribute this
 *  software is freely granted, provided that this notice
 *  is preserved.
 *
  __ieee754_acos(x)
 * Method :
 *    acos(x)  = pi/2 - asin(x)
 *    acos(-x) = pi/2 + asin(x)
 * For |x|<=0.5
 *    acos(x) = pi/2 - (x + x*x^2*R(x^2))    (see asin.c)
 * For x>0.5
 *     acos(x) = pi/2 - (pi/2 - 2asin(sqrt((1-x)/2)))
 *        = 2asin(sqrt((1-x)/2))
 *        = 2s + 2s*z*R(z)     ...z=(1-x)/2, s=sqrt(z)
 *        = 2f + (2c + 2s*z*R(z))
 *     where f=hi part of s, and c = (z-f*f)/(s+f) is the correction term
 *     for f so that f+c ~ sqrt(z).
 * For x<-0.5
 *    acos(x) = pi - 2asin(sqrt((1-|x|)/2))
 *        = pi - 0.5*(s+s*z*R(z)), where z=(1-|x|)/2,s=sqrt(z)
 *
 * Special cases:
 *    if x is NaN, return x itself;
 *    if |x|>1, return NaN with invalid signal.
 *
 * Function needed: sqrt
|#
(defun float64-acos (x)
      (declare (values double-float))
      (declare (type double-float x))
  (let (
    (one  #+nil 1.00000000000000000000e+00
          #.(encode-float64 #x3FF00000 #x00000000) )
    #+nil (pi   #+nil 3.14159265358979311600e+00
        #.(encode-float64 #x400921FB
                           #x54442D18) )
    (pio2_hi #+nil  1.57079632679489655800e+00
        #.(encode-float64 #x3FF921FB
                           #x54442D18) )
    (pio2_lo #+nil  6.12323399573676603587e-17
        #.(encode-float64 #x3C91A626
                           #x33145C07 ) )
    (pS0 #+nil  1.66666666666666657415e-01
        #.(encode-float64 #x3FC55555 #x55555555 ) )
    (pS1 #+nil -3.25565818622400915405e-01
        #.(encode-float64 #xBFD4D612 #x03EB6F7D) )
    (pS2 #+nil  2.01212532134862925881e-01
        #.(encode-float64 #x3FC9C155
                           #x0E884455 ) )
    (pS3 #+nil -4.00555345006794114027e-02
        #.(encode-float64 #xBFA48228 #xB5688F3B) )
    (pS4 #+nil  7.91534994289814532176e-04
        #.(encode-float64 #x3F49EFE0 #x7501B288 ) )
    (pS5 #+nil  3.47933107596021167570e-05
        #.(encode-float64 #x3F023DE1 #x0DFDF709 ) )
    (qS1 #+nil -2.40339491173441421878e+00
        #.(encode-float64 #xC0033A27 #x1C8A2D4B ) )
    (qS2 #+nil  2.02094576023350569471e+00
        #.(encode-float64 #x40002AE5 #x9C598AC8) )
    (qS3 #+nil -6.88283971605453293030e-01
        #.(encode-float64 #xBFE6066C #x1B8D0159) )
    (qS4 #+nil  7.70381505559019352791e-02
        #.(encode-float64 #x3FB3B8C5 #xB12E9282) )
    )
    (multiple-value-bind (hx lx) (decode-float64 x)
        (declare (type (signed-byte 32) hx lx))
      (let ((ix (logand hx #x7fffffff)))
          (declare (type (signed-byte 32) ix))
        (cond
          ((>= ix #x3ff00000)                           ; |x| >= 1
            (if (eql (logior (- ix #x3ff00000) lx) 0)   ; |x|==1
                (if (> hx 0)
                     0.0d0                              ; acos(1) = 0
                  (+ pi (* 2.0d0 pio2_lo)) )            ; acos(-1) = pi
              (/ (- x x) (- x x) )) )                   ; acos(|x|>1) is NaN
          ((<= ix #x3fe00000)                           ; |x| <= 0.5
            (if (<= ix #x3c600000)
                (+ pio2_hi pio2_lo)                     ; if|x|<2**-57
               (let* ((z  (* x x))
                      (p  (* z (+ pS0
                          (* z (+ pS1
                          (* z (+ pS2
                          (* z (+ pS3
                          (* z (+ pS4
                          (* z pS5) )))))))))) )
                      (q  (* (+ one (* z
                             (+ qS1 (* z
                             (+ qS2 (* z
                             (+ qS3 (* z qS4)) ))))))) )
                      (r  (/ p q)) )
                    (declare (type double-float z p q r))
                  (- pio2_hi (- x (- pio2_lo (* x r)))) )) )
          ((< hx 0)                                     ; x < -0.5
            (let* ((z (* (+ one x) 0.5d0))
                   (p (* z (+ pS0
                      (* z (+ pS1
                      (* z (+ pS2
                      (* z (+ pS3
                      (* z (+ pS4
                      (* z pS5) )))))))))) )
                   (q (+ one (* z (+ qS1
                             (* z (+ qS2
                             (* z (+ qS3
                             (* z qS4) ))))))) )
                   (s (float64-sqrt z))
                   (r (/ p q))
                   (w (- (* r s) pio2_lo)) )
                (declare (type double-float z p q r w))
              (- pi  (* 2.0d0 (+ s w))) ) )
          (t                                            ; x > 0.5
            (let* ((z  (* (- one x) 0.5d0))
                   (s  (float64-sqrt z))
                   (df (encode-float64 (decode-float64 s) 0))
                   (c  (/ (- z (* df df)) (+ s df)))
                   (p  (* z (+ pS0
                       (* z (+ pS1
                       (* z (+ pS2
                       (* z (+ pS3
                       (* z (+ pS4
                       (* z pS5))))) )))))) )
                   (q  (+ one (* z (+ qS1
                              (* z (+ qS2
                              (* z (+ qS3
                              (* z qS4) ))))))) )
                   (r  (/ p q))
                   (w  (+ (* r s) c)) )
                (declare (type double-float z s df c p q r w))
              (* 2.0d0 (+ df w)) ) )) ) ) ) )
