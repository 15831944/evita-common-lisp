;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - expm1
;;; arch/generic/lisp/float64/gen-float64-expm1.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-expm1.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-expm1
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* @(#)s_expm1.c 5.1 93/09/24 */
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

/*
FUNCTION
        <<expm1>>, <<expm1f>>---exponential minus 1
INDEX
        expm1
INDEX
        expm1f

ANSI_SYNOPSIS
        #include <math.h>
        double expm1(double <[x]>);
        float expm1f(float <[x]>);

TRAD_SYNOPSIS
        #include <math.h>
        double expm1(<[x]>);
        double <[x]>;

        float expm1f(<[x]>);
        float <[x]>;

DESCRIPTION
        <<expm1>> and <<expm1f>> calculate the exponential of <[x]>
        and subtract 1, that is,
        @ifnottex
        e raised to the power <[x]> minus 1 where e
        @end ifnottex
        @tex
        $e^x - 1$ (where $e$
        @end tex
        is the base of the natural system of logarithms, approximately
        2.71828).  The result is accurate even for small values of
        <[x]>, where using <<exp(<[x]>)-1>> would lose many
        significant digits.

RETURNS
        e raised to the power <[x]>, minus 1.

PORTABILITY
        Neither <<expm1>> nor <<expm1f>> is required by ANSI C or by
        the System V Interface Definition (Issue 2).
*/

/* expm1(x)
 * Returns exp(x)-1, the exponential of x minus 1.
 *
 * Method
 *   1. Argument reduction:
 *      Given x, find r and integer k such that
 *
 *               x = k*ln2 + r,  |r| <= 0.5*ln2 ~ 0.34658
 *
 *      Here a correction term c will be computed to compensate
 *      the error in r when rounded to a floating-point number.
 *
 *   2. Approximating expm1(r) by a special rational function on
 *      the interval [0,0.34658]:
 *      Since
 *          r*(exp(r)+1)/(exp(r)-1) = 2+ r^2/6 - r^4/360 + ...
 *      we define R1(r*r) by
 *          r*(exp(r)+1)/(exp(r)-1) = 2+ r^2/6 * R1(r*r)
 *      That is,
 *          R1(r**2) = 6/r *((exp(r)+1)/(exp(r)-1) - 2/r)
 *                   = 6/r * ( 1 + 2.0*(1/(exp(r)-1) - 1/r))
 *                   = 1 - r^2/60 + r^4/2520 - r^6/100800 + ...
 *      We use a special Reme algorithm on [0,0.347] to generate
 *      a polynomial of degree 5 in r*r to approximate R1. The
 *      maximum error of this polynomial approximation is bounded
 *      by 2**-61. In other words,
 *          R1(z) ~ 1.0 + Q1*z + Q2*z**2 + Q3*z**3 + Q4*z**4 + Q5*z**5
 *      where         Q1  =  -1.6666666666666567384E-2,
 *              Q2  =   3.9682539681370365873E-4,
 *              Q3  =  -9.9206344733435987357E-6,
 *              Q4  =   2.5051361420808517002E-7,
 *              Q5  =  -6.2843505682382617102E-9;
 *      (where z=r*r, and the values of Q1 to Q5 are listed below)
 *      with error bounded by
 *          |                  5           |     -61
 *          | 1.0+Q1*z+...+Q5*z   -  R1(z) | <= 2
 *          |                              |
 *
 *      expm1(r) = exp(r)-1 is then computed by the following
 *      specific way which minimize the accumulation rounding error:
 *                             2     3
 *                            r     r    [ 3 - (R1 + R1*r/2)  ]
 *            expm1(r) = r + --- + --- * [--------------------]
 *                            2     2    [ 6 - r*(3 - R1*r/2) ]
 *
 *      To compensate the error in the argument reduction, we use
 *              expm1(r+c) = expm1(r) + c + expm1(r)*c
 *                         ~ expm1(r) + c + r*c
 *      Thus c+r*c will be added in as the correction terms for
 *      expm1(r+c). Now rearrange the term to avoid optimization
 *      screw up:
 *                      (      2                                    2 )
 *                      ({  ( r    [ R1 -  (3 - R1*r/2) ]  )  }    r  )
 *       expm1(r+c)~r - ({r*(--- * [--------------------]-c)-c} - --- )
 *                      ({  ( 2    [ 6 - r*(3 - R1*r/2) ]  )  }    2  )
 *                      (                                             )
 *
 *                 = r - E
 *   3. Scale back to obtain expm1(x):
 *      From step 1, we have
 *         expm1(x) = either 2^k*[expm1(r)+1] - 1
 *                  = or     2^k*[expm1(r) + (1-2^-k)]
 *   4. Implementation notes:
 *      (A). To save one multiplication, we scale the coefficient Qi
 *           to Qi*2^i, and replace z by (x^2)/2.
 *      (B). To achieve maximum accuracy, we compute expm1(x) by
 *        (i)   if x < -56*ln2, return -1.0, (raise inexact if x!=inf)
 *        (ii)  if k=0, return r-E
 *        (iii) if k=-1, return 0.5*(r-E)-0.5
 *        (iv)  if k=1 if r < -0.25, return 2*((r+0.5)- E)
 *                            else             return  1.0+2.0*(r-E);
 *        (v)   if (k<-2||k>56) return 2^k(1-(E-r)) - 1 (or exp(x)-1)
 *        (vi)  if k <= 20, return 2^k((1-2^-k)-(E-r)), else
 *        (vii) return 2^k(1-((E+2^-k)-r))
 *
 * Special cases:
 *      expm1(INF) is INF, expm1(NaN) is NaN;
 *      expm1(-INF) is -1, and
 *      for finite argument, only expm1(0)=0 is exact.
 *
 * Accuracy:
 *      according to an error analysis, the error is always less than
 *      1 ulp (unit in the last place).
 *
 * Misc. info.
 *      For IEEE double
 *          if x >  7.09782712893383973096e+02 then expm1(x) overflow
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following
 * constants. The decimal values may be used, provided that the
 * compiler will convert from decimal to binary accurately enough
 * to produce the hexadecimal values shown.
 */
|#
(defun float64-expm1 (x)
    (declare (values double-float))
    (declare (type double-float x))
  (prog (
    (one            1.0d0)
    (huge           1.0d+300)
    (tiny           1.0d-300)
    (o_threshold    #+nil 7.09782712893383973096e+02
                    #.(encode-float64 #x40862E42 #xFEFA39EF) )
    (ln2_hi         #+nil 6.93147180369123816490e-01
                    #.(encode-float64 #x3fe62e42 #xfee00000) )
    (ln2_lo         #+nil 1.90821492927058770002e-10
                    #.(encode-float64 #x3dea39ef #x35793c76) )
    (invln2         #+nil 1.44269504088896338700e+00
                    #.(encode-float64 #x3ff71547 #x652b82fe) )
    ;; scaled coefficients related to expm1
    (Q1             #+nil -3.33333333333331316428e-02
                    #.(encode-float64 #xBFA11111 #x111110F4) )
    (Q2             #+nil 1.58730158725481460165e-03
                    #.(encode-float64 #x3F5A01A0 #x19FE5585) )
    (Q3             #+nil -7.93650757867487942473e-05
                    #.(encode-float64 #xBF14CE19 #x9EAADBB7) )
    (Q4             #+nil 4.00821782732936239552e-06
                    #.(encode-float64 #x3ED0CFCA #x86E65239) )
    (Q5             #+nil -2.01099218183624371326e-07
                    #.(encode-float64 #xBE8AFDB7 #x6E09C32D) )
    )
    (multiple-value-bind (hx lx) (decode-float64 x)
    (let* ((xsb (logand hx #x80000000))     ; sign bit of x
           (hx  (logand hx #x7fffffff)) )   ; high word of |x|

      ;; filter out huge and non-finite argument
      (when (>= hx #x4043687A)      ; if |x|>=56*ln2
        (when (>= hx #x40862E42)    ; if |x|>=709.78...
          (cond
            ((>= hx #x7ff00000)
              (cond
                ((not (eql (logior (logand hx #xfffff) lx) 0))
                  (return (+ x x)) ) ; NaN
                ((eql xsb 0)
                  ;; exp(+inf)=+inf
                  (return x) )
                (t
                  ;; exp(-inf)=-1
                  (return -1.0d0) )) )
            ((> x  o_threshold)
              ;; overflow
              (return (* huge huge)) )))
        (unless (eql xsb 0)             ; x < -56*ln2, return -1.0 with inexact
          (when (minusp (+ x tiny))     ; raise inexact
            (return (- tiny one)) )))   ; return -1

      ;; argument reduction
      (let ((hi 0d0) (lo 0d0) (c 0d0) (k 0) (s 0d0))
      (cond
        ((> hx #x3fd62e42)      ; if  |x| > 0.5 ln2
          (if (< hx #x3FF0A2B2) ; and |x| < 1.5 ln2
              (if (eql xsb 0)
                   (setq hi (- x ln2_hi) lo ln2_lo k 1)
                   (setq hi (+ x ln2_hi) lo (- ln2_lo) k -1) )
              (setq
                k  (truncate (+ (* invln2 x) (if (eql xsb 0) 0.5d0 -0.5d0)))
                s k
                hi (- x (* s ln2_hi))   ; s*ln2_hi is exact here
                lo (* s ln2_lo)))
          (setq x  (- hi lo))
          (setq c  (- (- hi x) lo)) )
        ((< hx #x3c900000)      ; when |x|<2**-54, return x
          (setq s (+ huge x))   ; return x with inexact flags when x!=0
          (return (- x (- s (+ huge x)))) ))

      ;; x is now in primary range
      (let* (
        (hfx (* 0.5d0 x))
        (hxs (* x hfx))
        (r1  (+ one (* hxs (+ Q1
                    (* hxs (+ Q2
                    (* hxs (+ Q3
                    (* hxs (+ Q4
                    (* hxs Q5)))))))))) )
        (u (- 3.0d0 (* r1 hfx)))
        (e   (* hxs (/ (- r1 u) (- 6.0d0 (* x u)))))
        )
        (when (eql k 0)
          ;; c is 0
          (return (- x (- (* x e) hxs))) )

        (setq e (- (* x (- e c)) c))
        (decf e hxs)

        (when (eql k -1)
          (return (- (* 0.5d0 (- x e)) 0.5d0)) )

        (when (eql k 1)
          (when (< x -0.25d0) (return (* -2.0d0 (- e (+ x 0.5d0)))))
          (return (+ one (* 2.0d0 (- x e)))) )

        (when (or (<= k -2) (> k 56))   ; suffice to return exp(x)-1
          (multiple-value-bind (hy ly) (decode-float64 (- one (- e x)))
            ;; add k to y's exponent
            (return (- (encode-float64 (+ hy (ash k 20)) ly) one) ) ))

        (if (< k 20)
            (let* ((u (encode-float64     ; u=1-2^-k
                        (- #x3ff00000 (ash #x200000 (- k))) 0  ) )
                   (y (- u (- e x))) )
              (multiple-value-bind (hy ly) (decode-float64 y)
                ;; add k to y's exponent
                (return (encode-float64 (+ hy (ash k 20)) ly) ) ) )
            (let* ((u (encode-float64  ; 2^-k
                        (ash (- #x3ff k) 20) 0 ) )
                   (y (+ (- x (+ e u)) one)) )
              (multiple-value-bind (hy ly) (decode-float64 y)
                ;; add k to y's exponent
                (return (encode-float64
                    (+ hy (ash k 20)) ly) ) ) )) ) ) ) ) ) )
