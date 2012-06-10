;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - log
;;; arch/generic/lisp/macth/gen-math-64-log.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-log.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-log
;
(in-package :si)

#|
 *  From fdlibm (http://www.netlib.org/fdlibm/)
 * @(#)e_log.c 5.1 93/09/24
 *
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 *

 * __ieee754_log(x)
 * Return the logrithm of x
 *
 * Method :                  
 *   1. Argument Reduction: find k and f such that 
 *                      x = 2^k * (1+f), 
 *         where  sqrt(2)/2 < 1+f < sqrt(2) .
 *
 *   2. Approximation of log(1+f).
 *      Let s = f/(2+f) ; based on log(1+f) = log(1+s) - log(1-s)
 *               = 2s + 2/3 s**3 + 2/5 s**5 + .....,
 *                    = 2s + s*R
 *      We use a special Reme algorithm on [0,0.1716] to generate 
 *      a polynomial of degree 14 to approximate R The maximum error 
 *      of this polynomial approximation is bounded by 2**-58.45. In
 *      other words,
 *                      2      4      6      8      10      12      14
 *          R(z) ~ Lg1*s +Lg2*s +Lg3*s +Lg4*s +Lg5*s  +Lg6*s  +Lg7*s
 *      (the values of Lg1 to Lg7 are listed in the program)
 *      and
 *          |      2          14          |     -58.45
 *          | Lg1*s +...+Lg7*s    -  R(z) | <= 2 
 *          |                             |
 *      Note that 2s = f - s*f = f - hfsq + s*hfsq, where hfsq = f*f/2.
 *      In order to guarantee error in log below 1ulp, we compute log
 *      by
 *              log(1+f) = f - s*(f - R)        (if f is not too large)
 *              log(1+f) = f - (hfsq - s*(hfsq+R)).        (better accuracy)
 *      
 *      3. Finally,  log(x) = k*ln2 + log(1+f).  
 *                          = k*ln2_hi+(f-(hfsq-(s*(hfsq+R)+k*ln2_lo)))
 *         Here ln2 is split into two floating point number: 
 *                      ln2_hi + ln2_lo,
 *         where n*ln2_hi is always exact for |n| < 2000.
 *
 * Special cases:
 *      log(x) is NaN with signal if x < 0 (including -INF) ; 
 *      log(+INF) is +INF; log(0) is -INF with signal;
 *      log(NaN) is that NaN with no signal.
 *
 * Accuracy:
 *      according to an error analysis, the error is always less than
 *      1 ulp (unit in the last place).
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following 
 * constants. The decimal values may be used, provided that the 
 * compiler will convert from decimal to binary accurately enough 
 * to produce the hexadecimal values shown.
|#
(defun float64-log (x)
    (declare (values double-float))
    (declare (type double-float x))
  (prog* (
    (ln2_hi #+nil  6.93147180369123816490e-01
                #.(encode-float64 #x3fe62e42 #xfee00000) )
    (ln2_lo #+nil  1.90821492927058770002e-10
                #.(encode-float64 #x3dea39ef #x35793c76) )
    (two54  #+nil  1.80143985094819840000e+16
                #.(encode-float64 #x43500000 #x00000000) )
    (Lg1    #+nil 6.666666666666735130e-01
                #.(encode-float64 #x3FE55555 #x55555593) )
    (Lg2    #+nil 3.999999999940941908e-01
                #.(encode-float64 #x3FD99999 #x9997FA04) )
    (Lg3    #+nil 2.857142874366239149e-01
                #.(encode-float64 #x3FD24924 #x94229359) )
    (Lg4    #+nil 2.222219843214978396e-01
                #.(encode-float64 #x3FCC71C5 #x1D8E78AF) )
    (Lg5    #+nil 1.818357216161805012e-01
                #.(encode-float64 #x3FC74664 #x96CB03DE) )
    (Lg6    #+nil 1.531383769920937332e-01
                #.(encode-float64 #x3FC39A09 #xD078C69F) )
    (Lg7    #+nil 1.479819860511658591e-01
                #.(encode-float64 #x3FC2F112 #xDF3E5244) )
    (zero   0d0) )
    (multiple-value-bind (hx lx) (decode-float64 x)
        (declare (type (signed-byte 32) hx lx))
      (let ((k 0))
          (declare (type (signed-byte 32) k))
        (when (< hx #x00100000) ; x < 2**-1022
          (when (zerop (logior (logand hx #x7fffffff) lx))
            (return (/ (- two54) zero)) )   ; log(+-0)=-inf

          (when (minusp x)
            (return (/ (- x x) zero)) )     ; log(-#)=NaN

          (decf k 54)
          (setq x (* x two54))  ; subnormal number, scale up x
          (multiple-value-setq (hx lx) (decode-float64 x)) )

        (when (>= x #x7ff00000) ; x is inf
          (return (+ x x)) )

        (incf k (- (ash hx -20) 1023))
        (setq hx (logand hx #x000fffff))

        ;; normalize x or x/2
        (let ((i (logand (+ hx #x95f64) #x100000)))
          (setq x (encode-float64 (logior hx (logxor i #x3ff00000)) lx))
          (incf k (ash i -20)) )

        (let ((f (- x 1)))
            (declare (type double-float f))

          ;; |f| < 2**-20
          (when (< (logand (+ hx 2) #x000fffff) 3)
            (return
              (if (eql f zero)
                  (if (eql k 0)
                      zero
                    (let ((dk (float k zero)))
                       (+ (* dk ln2_hi) (* dk ln2_lo)) ))
                (let ((R (* (* f f) (- 0.5d0 (* 0.3333333333333333d0 f)))))
                  (if (eql k 0)
                      (- f R)
                    (let ((dk (float k zero)))
                      (- (* dk ln2_hi) (- (- r (* dk ln2_lo)) f)) )) ))) )

          (let* ((s  (/ f (+ 2 f)))
                 (z  (* s s))
                 (i  (- hx #x6147a))
                 (w  (* z z))
                 (j  (- #x6b851 hx))
                 (t1 (* w (+ Lg2 (* w (+ Lg4 (* w Lg6))))))
                 (t2 (* z (+ Lg1 (* w (+ Lg3 (* w (+ Lg5 (* w Lg7))))))))
                 (R  (+ t1 t2)) )
            (setq i (logior i j))
            (if (> i 0)
                (let ((hfsq (* 0.5 f f)) (dk (float k zero)))
                  (if (eql k 0)
                      (return (- f (- hfsq (* s (+ hfsq r)))))
                    (return (- (* dk ln2_hi)
                       (- (- hfsq (+ (* s (+ hfsq R)) (* dk ln2_lo))) f) ))) )
              (let ((dk (float k zero)))
                (if (eql k 0)
                    (return (- f (* s (- f R))))
                  (return (- (* dk ln2_hi)
                             (- (- (* s (- f R)) (* dk ln2_lo))
                             f)) ) )) ) ) ) ) ) ) )
