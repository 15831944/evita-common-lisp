;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - atan
;;; arch/generic/lisp/macth/gen-math-f64-atan.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-atan.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-atan
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
 *
/* @(#)s_atan.c 5.1 93/09/24 */
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

/*
FUNCTION
        <<atan>>, <<atanf>>---arc tangent

INDEX
   atan
INDEX
   atanf

ANSI_SYNOPSIS
        #include <math.h>
        double atan(double <[x]>);
        float atanf(float <[x]>);

TRAD_SYNOPSIS
        #include <math.h>
        double atan(<[x]>);
        double <[x]>;

        float atanf(<[x]>);
        float <[x]>;

DESCRIPTION

<<atan>> computes the inverse tangent (arc tangent) of the input value.

<<atanf>> is identical to <<atan>>, save that it operates on <<floats>>.

RETURNS
@ifnottex
<<atan>> returns a value in radians, in the range of -pi/2 to pi/2.
@end ifnottex
@tex
<<atan>> returns a value in radians, in the range of $-\pi/2$ to $\pi/2$.
@end tex

PORTABILITY
<<atan>> is ANSI C.  <<atanf>> is an extension.

*/

/* atan(x)
 * Method
 *   1. Reduce x to positive by atan(x) = -atan(-x).
 *   2. According to the integer k=4t+0.25 chopped, t=x, the argument
 *      is further reduced to one of the following intervals and the
 *      arctangent of t is evaluated by the corresponding formula:
 *
 *      [0,7/16]      atan(x) = t-t^3*(a1+t^2*(a2+...(a10+t^2*a11)...)
 *      [7/16,11/16]  atan(x) = atan(1/2) + atan( (t-0.5)/(1+t/2) )
 *      [11/16.19/16] atan(x) = atan( 1 ) + atan( (t-1)/(1+t) )
 *      [19/16,39/16] atan(x) = atan(3/2) + atan( (t-1.5)/(1+1.5t) )
 *      [39/16,INF]   atan(x) = atan(INF) + atan( -1/t )
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following 
 * constants. The decimal values may be used, provided that the 
 * compiler will convert from decimal to binary accurately enough 
 * to produce the hexadecimal values shown.
 */
|#
(defun float64-atan/1 (x)
    (declare (values double-float))
    (declare (type double-float x))
  (prog* (
    (atanhi0    #+nil 4.63647609000806093515e-01    ; atan(0.5)hi 
                    #.(encode-float64 #x3FDDAC67 #x0561BB4F) )
    (atanhi1    #+nil 7.85398163397448278999e-01    ; atan(1.0)hi
                    #.(encode-float64 #x3FE921FB #x54442D18) )
    (atanhi2    #+nil 9.82793723247329054082e-01    ;  atan(1.5)hi
                    #.(encode-float64 #x3FEF730B #xD281F69B) )
    (atanhi3    #+nil 1.57079632679489655800e+00    ;  atan(inf)hi
                    #.(encode-float64 #x3FF921FB #x54442D18) )
    ;;
    (atanlo0    #+nil 2.26987774529616870924e-17    ;  atan(0.5)lo
                    #.(encode-float64 #x3C7A2B7F #x222F65E2) )
    (atanlo1    #+nil 3.06161699786838301793e-17    ;  atan(1.0)lo
                    #.(encode-float64 #x3C81A626 #x33145C07) )
    (atanlo2    #+nil 1.39033110312309984516e-17    ;  atan(1.5)lo
                    #.(encode-float64 #x3C700788 #x7AF0CBBD) )
    (atanlo3    #+nil 6.12323399573676603587e-17    ;  atan(inf)lo
                    #.(encode-float64 #x3C91A626 #x33145C07) )
    ;;
    (aT0        #+nil 3.33333333333329318027e-01
                    #.(encode-float64 #x3FD55555 #x5555550D) )
    (aT1        #+nil -1.99999999998764832476e-01
                    #.(encode-float64 #xBFC99999 #x9998EBC4) )
    (aT2        #+nil 1.42857142725034663711e-01
                    #.(encode-float64 #x3FC24924 #x920083FF) )
    (aT3        #+nil -1.11111104054623557880e-01
                    #.(encode-float64 #xBFBC71C6 #xFE231671) )
    (aT4        #+nil 9.09088713343650656196e-02
                    #.(encode-float64 #x3FB745CD #xC54C206E) )
    (aT5        #+nil -7.69187620504482999495e-02
                    #.(encode-float64 #xBFB3B0F2 #xAF749A6D) )
    (aT6        #+nil 6.66107313738753120669e-02
                    #.(encode-float64 #x3FB10D66 #xA0D03D51) )
    (aT7        #+nil -5.83357013379057348645e-02
                    #.(encode-float64 #xBFADDE2D #x52DEFD9A) )
    (aT8        #+nil 4.97687799461593236017e-02
                    #.(encode-float64 #x3FA97B4B #x24760DEB) )
    (aT9        #+nil -3.65315727442169155270e-02
                    #.(encode-float64 #xBFA2B444 #x2C6A6C2F) )
    (aT10       #+nil 1.62858201153657823623e-02
                    #.(encode-float64 #x3F90AD3A #xE322DA11) )
    ;;
    (zero       #+nil 0.0
                    #.(encode-float64 0 0) )
    (one        #+nil 1.00000000000000000000e+00
                    #.(encode-float64 #x3FF00000 #x00000000) )
    (huge       #+nil 1.000e+300
                    #.(encode-float64 #x7E37E43C #x8800759C) )
    )
    ;;
    (multiple-value-bind (hx lx) (decode-float64 x)
    (let ((ix (logand hx #x7fffffff))
          (atanhi zero)
          (atanlo zero) )
      ;; if |x| >= 2^66
      (when (>= ix #x44100000) 
        (cond
          ((or (> ix #x7ff00000) (and (eql ix #x7ff00000) (not (eql lx 0))))
            (return (+ x x)) )  ; NaN
          ((> hx 0)
            (return (+ atanhi3 atanlo3)) )
          (t
            (return (- (+ atanhi3 atanlo3))) )))

      ;; |x| < 0.4375
      (if (< ix  #x3fdc0000)
          ;; |x| < 2^-29
          (when (< ix #x3e200000)
            ;; raise inexact
            (when (> (+ huge x) one) (return x)) )
        (progn
          (setq x (float64-abs x))
            ;; |x| < 1.1875
            (if (< ix #x3ff30000)
                (if (< ix #x3fe60000)
                    ;; 7/16 <=|x|<11/16
                    (setq atanhi atanhi0
                          atanlo atanlo0
                          x  (/ (- (* 2 x) one) (+ 2 x)) )
                    ;; 11/16<=|x|< 19/16
                    (setq atanhi atanhi1
                          atanlo atanlo1
                          x  (/ (- x one) (+ x one)) ))
                ;; |x| < 2.4375
                (if (< ix #x40038000)
                    (setq atanhi atanhi2
                          atanlo atanlo2
                          x (/ (- x 1.5) (+ one (* 1.5 x))) )
                    ;; 2.4375 <= |x| < 2^66
                    (setq atanhi atanhi3
                          atanlo atanlo3
                          x (/ -1.0 x) )))))

      ;; end of argument reduction
      (let* ((z  (* x x))
             (w  (* z z))
             ;; break sum from i=0 to 10 aT[i]z**(i+1) into odd and even poly
             (s1 (* z (+ at0
                 (* w (+ at2
                 (* w (+ at4
                 (* w (+ at6
                 (* w (+ at8
                 (* w at10))))))))))) )
             (s2 (* w (+ at1
                 (* w (+ at3
                 (* w (+ at5
                 (* w (+ at7
                 (* w at9))))))))) ))
        (when (eql atanhi zero) (return (- x (* x (+ s1 s2)))))
        (let ((z (- atanhi (- (- (* x (+ s1 s2)) atanlo) x))))
          (return (if (< hx 0) (- z) z)) ) ) ) ) ) )
