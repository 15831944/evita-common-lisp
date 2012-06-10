;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64-sign
;;; arch/generic/lisp/math/gen-math-f32-sign.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-sign.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of float64-sign.
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
/* @(#)s_copysign.c 5.1 93/09/24 */
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
<<copysign>>, <<copysignf>>---sign of <[y]>, magnitude of <[x]>

INDEX
        copysign
INDEX
        copysignf

ANSI_SYNOPSIS
        #include <math.h>
        double copysign (double <[x]>, double <[y]>);
        float copysignf (float <[x]>, float <[y]>);

TRAD_SYNOPSIS
        #include <math.h>
        double copysign (<[x]>, <[y]>)
        double <[x]>;
        double <[y]>;

        float copysignf (<[x]>, <[y]>)
        float <[x]>;
        float <[y]>;

DESCRIPTION
<<copysign>> constructs a number with the magnitude (absolute value)
of its first argument, <[x]>, and the sign of its second argument,
<[y]>.

<<copysignf>> does the same thing; the two functions differ only in
the type of their arguments and result.

RETURNS
<<copysign>> returns a <<double>> with the magnitude of
<[x]> and the sign of <[y]>.
<<copysignf>> returns a <<float>> with the magnitude of
<[x]> and the sign of <[y]>.

PORTABILITY
<<copysign>> is not required by either ANSI C or the System V Interface
Definition (Issue 2).

*/

/*
 * copysign(double x, double y)
 * copysign(x,y) returns a value with the magnitude of x and
 * with the sign bit of y.
 */
|#
(defun float64-sign (x y)
    (declare (values double-float))
    (declare (type double-float x y))
  (multiple-value-bind (hx lx) (decode-float64 x)
  (let ((hy (decode-float64 y)))
    (encode-float64
        (logior (logand hx #x7fffffff) (logand hy #x80000000))
        lx ) ) ) )
