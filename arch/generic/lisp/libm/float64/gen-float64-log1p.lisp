;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - log1p
;;; arch/generic/lisp/float/float64/gen-float64-log1p.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-log1p.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-log1p
;;; for acosh, asinh.
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
 * @(#)s_log1p.c 5.1 93/09/24
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

 * double log1p(double x) = log of 1 + x
 *
 * Method :
 *   1. Argument Reduction: find k and f such that
 *                      1+x = 2^k * (1+f),
 *         where  sqrt(2)/2 < 1+f < sqrt(2) .
 *
 *      Note. If k=0, then f=x is exact. However, if k!=0, then f
 *      may not be representable exactly. In that case, a correction
 *      term is need. Let u=1+x rounded. Let c = (1+x)-u, then
 *      log(1+x) - log(u) ~ c/u. Thus, we proceed to compute log(u),
 *      and add back the correction term c/u.
 *      (Note: when x > 2**53, one can simply return log(x))
 *
 *   2. Approximation of log1p(f).
 *      Let s = f/(2+f) ; based on log(1+f) = log(1+s) - log(1-s)
 *               = 2s + 2/3 s**3 + 2/5 s**5 + .....,
 *                    = 2s + s*R
 *      We use a special Reme algorithm on [0,0.1716] to generate
 *      a polynomial of degree 14 to approximate R The maximum error
 *      of this polynomial approximation is bounded by 2**-58.45. In
 *      other words,
 *                      2      4      6      8      10      12      14
 *          R(z) ~ Lp1*s +Lp2*s +Lp3*s +Lp4*s +Lp5*s  +Lp6*s  +Lp7*s
 *      (the values of Lp1 to Lp7 are listed in the program)
 *      and
 *          |      2          14          |     -58.45
 *          | Lp1*s +...+Lp7*s    -  R(z) | <= 2
 *          |                             |
 *      Note that 2s = f - s*f = f - hfsq + s*hfsq, where hfsq = f*f/2.
 *      In order to guarantee error in log below 1ulp, we compute log
 *      by
 *              log1p(f) = f - (hfsq - s*(hfsq+R)).
 *
 *      3. Finally, log1p(x) = k*ln2 + log1p(f).
 *                            = k*ln2_hi+(f-(hfsq-(s*(hfsq+R)+k*ln2_lo)))
 *         Here ln2 is split into two floating point number:
 *                      ln2_hi + ln2_lo,
 *         where n*ln2_hi is always exact for |n| < 2000.
 *
 * Special cases:
 *      log1p(x) is NaN with signal if x < -1 (including -INF) ;
 *      log1p(+INF) is +INF; log1p(-1) is -INF with signal;
 *      log1p(NaN) is that NaN with no signal.
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
 *
 * Note: Assuming log() return accurate answer, the following
 *       algorithm can be used to compute log1p(x) to within a few ULP:
 *
 *              u = 1+x;
 *              if(u==1.0) return x ; else
 *                         return log(u)*(x/(u-1.0));
 *
 *       See HP-15C Advanced Functions Handbook, p.193.
|#
(defun float64-log1p (x)
    (declare (values double-float))
    (declare (type double-float x))
  (prog* (
    (ln2_hi #+nil  6.93147180369123816490e-01
                #.(encode-float64 #x3fe62e42 #xfee00000 ))
    (ln2_lo #+nil  1.90821492927058770002e-10
                #.(encode-float64 #x3dea39ef #x35793c76 ))
    (two54  #+nil  1.80143985094819840000e+16
                #.(encode-float64 #x43500000 #x00000000 ))
    (Lp1    #+nil 6.666666666666735130e-01
                #.(encode-float64 #x3FE55555 #x55555593 ))
    (Lp2    #+nil 3.999999999940941908e-01
                #.(encode-float64 #x3FD99999 #x9997FA04 ))
    (Lp3    #+nil 2.857142874366239149e-01
                #.(encode-float64 #x3FD24924 #x94229359 ))
    (Lp4    #+nil 2.222219843214978396e-01
                #.(encode-float64 #x3FCC71C5 #x1D8E78AF ))
    (Lp5    #+nil 1.818357216161805012e-01
                #.(encode-float64 #x3FC74664 #x96CB03DE ))
    (Lp6    #+nil 1.531383769920937332e-01
                #.(encode-float64 #x3FC39A09 #xD078C69F ))
    (Lp7    #+nil 1.479819860511658591e-01
                #.(encode-float64 #x3FC2F112 #xDF3E5244 ))
    (zero 0d0)
    ;;
    (hx (decode-float64 x))
    (k  1)
    (hu 0)
    (f  x)
    )
    (declare (type (signed-byte 32) hx k hu))
    (declare (type double-float f))

    (when (< hx #x3FDA827A)               ; x < 0.41422
      (let ((ax (logand hx #x7fffffff)))
        ;; x <= -1.0
        (when (>= ax #x3ff00000)
          (return
                (if (eql x -1.0d0)
                    (/ (- two54) zero)          ; log1p(-1)=+inf
                  (/ (- x x) (- x x)) )))       ; log1p(x<-1)=NaN

        ;; |x| < 2**-29
        (when (< ax #x3e200000)
          (return
                (if (and (> (+ two54 x) zero)   ; raise inexact
                         (< ax #x3c900000) )    ; |x| < 2**-54
                    x
                  (- x (* (* x x) 0.5d0)) )))

        ;; -0.2929<x<0.41422
        (when (or (> hx 0) (<= hx #.(- #xbfd2bec3 (ash 1 32))))
          (setq k 0)
          (setq hu 1) ) ))

    (when (>= hx #x7ff00000)
      (return (+ x x)) )

    (let ((c zero))
      (declare (type double-float c))
      (declare (type (signed-byte 32) hu))

      (unless (eql k 0)
        (let ((u zero) (lu 0))
          (if (< hx #x43400000)
              (progn
                (setq u (+ 1 x))
                (multiple-value-setq (hu lu) (decode-float64 u))
                (setq k (- (ash hu -20) 1023))
                ;; correction term
                (setq c (if (plusp k) (- 1.0 (- u x)) (- x (- u 1.0))))
                (setq c (/ c u)) )
            (progn
              (setq u x)
              (multiple-value-setq (hu lu) (decode-float64 u))
              (setq k (- (ash hu -20) 1023))
              (setq c zero) ))
          (setq hu (logand hu #x000fffff))
          (if (< hu #x6a09e)
              ;; normalize u
              (setq u (encode-float64 (logior hu #x3ff00000) lu))
            (progn
              (incf k)
              ;; normalize u/2
              (setq u (encode-float64 (logior hu #x3fe00000) lu))
              (setq hu (ash (- #x00100000 hu) -2)) ))
          (setq f (- u 1)) ))

      (let ((hfsq (* 0.5 f f)))
        (if (eql hu 0)    ; |f| < 2**-20
            (if (eql f zero)
                (if (eql k 0)
                     (return zero)
                  (progn
                    (incf c (* k ln2_lo))
                    (return (+ (* k ln2_hi) c)) ))
              (let ((R (* hfsq (- 1.0 (* 0.66666666666666666d0 f)))))
                (return
                  (if (eql k 0)
                      (- f R)
                    (- (* k ln2_hi) (- (- r (+ (* k ln2_lo) c)) f)) )) ))
          (let* ((s (/ f (+ 2 f)))
                 (z (* s s))
                 (R (* z (+ lp1 (* z
                         (+ lp2 (* z
                         (+ lp3 (* z
                         (+ lp4 (* z
                         (+ lp5 (* z
                         (+ lp6 (* z lp7))))))))))))) ))
            (return
                (if (eql k 0)
                    (- f (- hfsq (* s (+ hfsq R))))
                  (- (* k ln2_hi)
                     (- (- hfsq (+ (* s (+ hfsq r))
                        (+ (* k ln2_lo) c))) f) ))) )) ) ) ) )

