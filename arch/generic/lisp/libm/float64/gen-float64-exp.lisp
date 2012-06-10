;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - exp
;;; arch/generic/lisp/macth/gen-math-64-exp.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-exp.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-exp
;
(in-package :si)

#|
 * @(#)e_exp.c 5.1 93/09/24
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
 *
 * __ieee754_exp(x)
 * Returns the exponential of x.
 *
 * Method
 *   1. Argument reduction:
 *      Reduce x to an r so that |r| <= 0.5*ln2 ~ 0.34658.
 *      Given x, find r and integer k such that
 *
 *               x = k*ln2 + r,  |r| <= 0.5*ln2.
 *
 *      Here r will be represented as r = hi-lo for better
 *      accuracy.
 *
 *   2. Approximation of exp(r) by a special rational function on
 *      the interval [0,0.34658]:
 *      Write
 *          R(r**2) = r*(exp(r)+1)/(exp(r)-1) = 2 + r*r/6 - r**4/360 + ...
 *      We use a special Reme algorithm on [0,0.34658] to generate
 *      a polynomial of degree 5 to approximate R. The maximum error
 *      of this polynomial approximation is bounded by 2**-59. In
 *      other words,
 *          R(z) ~ 2.0 + P1*z + P2*z**2 + P3*z**3 + P4*z**4 + P5*z**5
 *      (where z=r*r, and the values of P1 to P5 are listed below)
 *      and
 *          |                  5          |     -59
 *          | 2.0+P1*z+...+P5*z   -  R(z) | <= 2
 *          |                             |
 *      The computation of exp(r) thus becomes
 *                             2*r
 *              exp(r) = 1 + -------
 *                            R - r
 *                                 r*R1(r)
 *                     = 1 + r + ----------- (for better accuracy)
 *                                2 - R1(r)
 *      where
 *                               2       4             10
 *              R1(r) = r - (P1*r  + P2*r  + ... + P5*r   ).
 *
 *   3. Scale back to obtain exp(x):
 *      From step 1, we have
 *         exp(x) = 2^k * exp(r)
 *
 * Special cases:
 *      exp(INF) is INF, exp(NaN) is NaN;
 *      exp(-INF) is 0, and
 *      for finite argument, only exp(0)=1 is exact.
 *
 * Accuracy:
 *      according to an error analysis, the error is always less than
 *      1 ulp (unit in the last place).
 *
 * Misc. info.
 *      For IEEE double
 *          if x >  7.09782712893383973096e+02 then exp(x) overflow
 *          if x < -7.45133219101941108420e+02 then exp(x) underflow
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following
 * constants. The decimal values may be used, provided that the
 * compiler will convert from decimal to binary accurately enough
 * to produce the hexadecimal values shown.
 *
|#
(defun float64-exp (x)
    (declare (values double-float))
    (declare (type double-float x))
  (prog (
    (zero   0d0)
    (one   1.0d0)
    (halF_0 +0.5d0)
    (halF_1 -0.5d0)
    (huge  1.0d+300)
    (twom1000   #+nil 9.33263618503218878990e-302   ; 2**-1000
                #.(encode-float64 #x017000000 0) )
    (o_threshold  #+nil 7.09782712893383973096e+02
                #.(encode-float64 #x40862E42 #xFEFA39EF) )
    (u_threshold #+nil -7.45133219101941108420e+02
                #.(encode-float64 #xc0874910 #xD52D3051) )
    (P1         #+nil 1.66666666666666019037e-01
                #.(encode-float64 #x3FC55555 #x5555553E) )
    (P2         #+nil -2.77777777770155933842e-03
                #.(encode-float64 #xBF66C16C #x16BEBD93) )
    (P3         #+nil 6.61375632143793436117e-05
                #.(encode-float64 #x3F11566A #xAF25DE2C) )
    (P4         #+nil -1.65339022054652515390e-06
                #.(encode-float64 #xBEBBBD41 #xC5D26BF1) )
    (P5         #+nil 4.13813679705723846039e-08
                #.(encode-float64 #x3E663769 #x72BEA4D0) )
    (ln2HI_0    #+nil 6.93147180369123816490e-01
                #.(encode-float64 #x3fe62e42 #xfee00000) )
    (ln2HI_1    #+nil -6.93147180369123816490e-01
                #.(encode-float64 #xbfe62e42 #xfee00000) )
    (ln2LO_0    #+nil 1.90821492927058770002e-10
                #.(encode-float64 #x3dea39ef #x35793c76) )
    (ln2LO_1    #+nil -1.90821492927058770002e-10
                #.(encode-float64 #xbdea39ef #x35793c76) )
    (invln2     #+nil 1.44269504088896338700e+00
                #.(encode-float64 #x3ff71547 #x652b82fe) )
    )
  (multiple-value-bind (hx lx) (decode-float64 x)
    (let ((minus? (minusp hx))               ; sign bit of x
          (ix     (logand hx #x7fffffff)) )  ; high word of |x|
      ;; filter out non-finite argument
      (when (>= ix #x40862E42)              ; if |x|>=709.78...
        (cond
          ((>= ix #x7ff00000)
            (return
              (if (eql (logior (logand ix #xfffff) lx) 0)
                  (if minus? x 0d0)            ; exp(+-inf)={inf,0}
                (+ x x) )) )                        ; NaN
          ((> x o_threshold)
            ;; overflow
            (return (* huge huge)) )
          ((< x u_threshold)
            ;; underflow
            (return (* twom1000 twom1000)) )))

      ;; argument reduction
      (let ((k 0)
            (hi zero)
            (lo zero) )
          (declare (type double-float hi lo))
        (cond
          ((> ix #x3fd62e42)        ; if  |x| > 0.5 ln2
            (if (< ix  #x3FF0A2B2)  ; and |x| < 1.5 ln2
                (if minus?
                    (setq hi (- x ln2HI_1) lo ln2LO_1 k -1)
                    (setq hi (- x ln2HI_0) lo ln2LO_0 k +1) )
              (let ((halF (if minus? halF_1 halF_0)))
                (setq k (truncate (+ (* invln2 x) halF)))
                (setq hi (- x (* k ln2HI_0)))    ; t*ln2HI is exact here
                (setq lo (* k ln2LO_0)) ))
            (setq x  (- hi lo)) )

          ;; when |x|<2**-28
          ((< ix  #x3e300000)
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

          (when (eql k 0)
            (return (- one (- (/ (* x c) (- c 2)) x))) )

          (multiple-value-bind (hy ly)
              (decode-float64 (- one (- (- lo (/ (* x c) (- 2 c))) hi)))
            (if (>= k -1021)
                ;; add k to y's exponent
                (return (encode-float64 (+ hy (ash k 20)) ly))
              ;; add k to y's exponent
              (return (* (encode-float64 (+ hy (ash (+ k 1000) 20)) ly)
                 twom1000 ))) ) ) ) ) ) ) )
