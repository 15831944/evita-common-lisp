;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - asin
;;; arch/generic/lisp/macth/gen-math-f64-asin.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-asin.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-asin
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
 *
 * @(#)e_asin.c 5.1 93/09/24
 *
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================

 * __ieee754_asin(x)
 * Method :
 *      Since  asin(x) = x + x^3/6 + x^5*3/40 + x^7*15/336 + ...
 *      we approximate asin(x) on [0,0.5] by
 *              asin(x) = x + x*x^2*R(x^2)
 *      where
 *              R(x^2) is a rational approximation of (asin(x)-x)/x^3
 *      and its remez error is bounded by
 *              |(asin(x)-x)/x^3 - R(x^2)| < 2^(-58.75)
 *
 *      For x in [0.5,1]
 *              asin(x) = pi/2-2*asin(sqrt((1-x)/2))
 *      Let y = (1-x), z = y/2, s := sqrt(z), and pio2_hi+pio2_lo=pi/2;
 *      then for x>0.98
 *              asin(x) = pi/2 - 2*(s+s*z*R(z))
 *                      = pio2_hi - (2*(s+s*z*R(z)) - pio2_lo)
 *      For x<=0.98, let pio4_hi = pio2_hi/2, then
 *              f = hi part of s;
 *              c = sqrt(z) - f = (z-f*f)/(s+f)         ...f+c=sqrt(z)
 *      and
 *              asin(x) = pi/2 - 2*(s+s*z*R(z))
 *                      = pio4_hi+(pio4-2s)-(2s*z*R(z)-pio2_lo)
 *                      = pio4_hi+(pio4-2f)-(2s*z*R(z)-(pio2_lo+2c))
 *
 * Special cases:
 *      if x is NaN, return x itself
 *      if |x|>1, return NaN with invalid signal.
 *
|#
(defun float64-asin (x)
    (declare (values double-float))
    (declare (type double-float x))
 (prog (
    (one     #+nil  1.00000000000000000000e+00
                #.(encode-float64 #x3FF00000 #x00000000) )
    (huge    #+nil 1.000e+300
                #.(encode-float64 #x7E37E43C #x8800759C) )
    (pio2_hi #+nil  1.57079632679489655800e+00
                #.(encode-float64 #x3FF921FB #x54442D18) )
    (pio2_lo #+nil  6.12323399573676603587e-17
                #.(encode-float64 #x3C91A626 #x33145C07) )
    (pio4_hi #+nil  7.85398163397448278999e-01
                #.(encode-float64 #x3FE921FB #x54442D18) )
    ;; coefficient for R(x^2)
    (pS0     #+nil  1.66666666666666657415e-01
                #.(encode-float64 #x3FC55555 #x55555555) )
    (pS1     #+nil -3.25565818622400915405e-01
                #.(encode-float64 #xBFD4D612 #x03EB6F7D) )
    (pS2     #+nil  2.01212532134862925881e-01
                #.(encode-float64 #x3FC9C155 #x0E884455) )
    (pS3     #+nil -4.00555345006794114027e-02
                #.(encode-float64 #xBFA48228 #xB5688F3B) )
    (pS4     #+nil  7.91534994289814532176e-04
                #.(encode-float64 #x3F49EFE0 #x7501B288) )
    (pS5     #+nil  3.47933107596021167570e-05
                #.(encode-float64 #x3F023DE1 #x0DFDF709) )
    (qS1     #+nil -2.40339491173441421878e+00
                #.(encode-float64 #xC0033A27 #x1C8A2D4B) )
    (qS2     #+nil  2.02094576023350569471e+00
                #.(encode-float64 #x40002AE5 #x9C598AC8) )
    (qS3     #+nil -6.88283971605453293030e-01
                #.(encode-float64 #xBFE6066C #x1B8D0159) )
    (qS4     #+nil  7.70381505559019352791e-02
                #.(encode-float64 #x3FB3B8C5 #xB12E9282) ))
  (multiple-value-bind (hx lx) (decode-float64 x)
    (let ((ix (logand hx #x7fffffff)))
      (cond
        ((>= ix #x3ff00000)   ; |x|>= 1
          (if (zerop (logior (- ix #x3ff00000) lx))
              ; asin(1)=+-pi/2 with inexact
              (return (+ (* x pio2_hi) (* x pio2_lo)))
            (return (/ (- x x) (- x x))) ) )  ; asin(|x|>1) is NaN
        ((< ix #x3fe00000)    ; |x|<0.5
          (if (< ix #x3e400000)   ; if |x| < 2**-27
              (when (> (+ huge x) one)
                (return x) )  ; return x with inexact if x!=0
            (let* ((g (* x x))
                   (p (* g (+ ps0
                      (* g (+ ps1
                      (* g (+ ps2
                      (* g (+ ps3
                      (* g (+ ps4
                      (* g ps5) )))))))))) )
                   (q (+ one
                        (* g (+ qs1
                        (* g (+ qs2
                        (* g (+ qs3
                        (* g qs4) ))))))) )
                   (w (/ p q)) )
              (return (+ x (* x w))) )) ))
      ;; 1 > |x| >= 0.5
      (let* ((w (- one (float64-abs x)))
             (g (* w 0.5))
             (p (* g (+ ps0
                (* g (+ ps1
                (* g (+ ps2
                (* g (+ ps3
                (* g (+ ps4
                (* g ps5) )))))))))) )
             (q (+ one
                  (* g (+ qs1
                  (* g (+ qs2
                  (* g (+ qs3
                  (* g qs4) ))))))) )
             (s (float64-sqrt g))
             (a
                (if (>= ix #x3FEF3333)  ; if |x| > 0.975
                    (let ((w (/ p q)))
                      (- pio2_hi (- (* 2.0d0 (+ s (* s w))) pio2_lo)) )
                  (let* ((w  (encode-float64 (decode-float64 s) 0))
                         (c  (/ (- g (* w w)) (+ s w)))
                         (r  (/ p q))
                         (p  (- (* (* 2.0d0 s) r) (- pio2_lo (* 2.0d0 c))))
                         (q  (- pio4_hi (* 2.0d0 w))) )
                    (- pio4_hi (- p q)) )) ))
          (return (if (> hx 0) a (- a))) ) ) ) ) )
