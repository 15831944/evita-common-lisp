;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64 - asinh
;;; arch/generic/lisp/float/float64/gen-float64-asinh.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-asinh.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-asinh
;
(in-package :si)

#|
 *  From fdlibm (http://www.netlib.org/fdlibm/)
/* @(#)s_asinh.c 5.1 93/09/24 */
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
        <<asinh>>, <<asinhf>>---inverse hyperbolic sine 

INDEX
        asinh
INDEX
        asinhf

ANSI_SYNOPSIS
        #include <math.h>
        double asinh(double <[x]>);
        float asinhf(float <[x]>);

TRAD_SYNOPSIS
        #include <math.h>
        double asinh(<[x]>)
        double <[x]>;

        float asinhf(<[x]>)
        float <[x]>;

DESCRIPTION
<<asinh>> calculates the inverse hyperbolic sine of <[x]>.
<<asinh>> is defined as 
@ifnottex
. sgn(<[x]>) * log(abs(<[x]>) + sqrt(1+<[x]>*<[x]>))
@end ifnottex
@tex
$$sign(x) \times ln\Bigl(|x| + \sqrt{1+x^2}\Bigr)$$
@end tex

<<asinhf>> is identical, other than taking and returning floats.

RETURNS
<<asinh>> and <<asinhf>> return the calculated value.

PORTABILITY
Neither <<asinh>> nor <<asinhf>> are ANSI C.

*/

/* asinh(x)
 * Method :
 *      Based on 
 *              asinh(x) = sign(x) * log [ |x| + sqrt(x*x+1) ]
 *      we have
 *      asinh(x) := x  if  1+x*x=1,
 *               := sign(x)*(log(x)+ln2)) for large |x|, else
 *               := sign(x)*log(2|x|+1/(|x|+sqrt(x*x+1))) if|x|>2, else
 *               := sign(x)*log1p(|x| + x^2/(1 + sqrt(1+x^2)))  
 */
|#
(defun float64-asinh (x)
    (declare (values double-float))
    (declare (type double-float x))
  (prog* (
    (one    #+nil 1.00000000000000000000e+00
            #.(encode-float64 #x3FF00000 #x00000000) )
    (ln2    #+nil 6.93147180559945286227e-01
            #.(encode-float64 #x3FE62E42 #xFEFA39EF) ) 
    (huge   #+nil 1.000e+300
            #.(encode-float64 #x7E37E43C #x8800759C) )
    ;;
    (hx (decode-float64 x))
    (ix (logand hx #x7fffffff))
    )
    ;;
    (cond
      ((>= ix #x7ff00000)
        (return (+ x x)) )  ; x is inf or NaN
      ((< ix #x3e300000)   ; |x|<2**-28
        ; return x inexact except 0
        (when (> (+ huge x) one) (return x)) ))

    (let ((w
            (cond
              ((> ix #x41b00000) ; |x| > 2**28
                (+ (float64-log (float64-abs x)) ln2) )
              ((> ix #x40000000) ; 2**28 > |x| > 2.0
                (let ((tt (float64-abs x)))
                  (float64-log (+ (* 2 tt)
                                  (/ one (+ (sqrt (+ (* x x) one)) tt)) )) ) )
              (t ; 2.0 > |x| > 2**-28
                (let ((tt (* x x)))
                  (float64-log1p
                    (+ (float64-abs x)
                       (/ tt (+ one (float64-sqrt (+ one tt)))) ) ) ) )) ))
      (return (if (> hx 0) w (- w))) ) ) )
