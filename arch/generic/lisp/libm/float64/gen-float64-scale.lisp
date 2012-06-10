;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64-scale
;;; arch/generic/lisp/math/gen-math-f32-scale.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-scale.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of float64-scale.
;
(in-package :si)

#|
 * From fdlibm (http://www.netlib.org/fdlibm/)
/* @(#)s_scalbn.c 5.1 93/09/24 */
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
<<scalbn>>, <<scalbnf>>---scale by power of two
INDEX
        scalbn
INDEX
        scalbnf

ANSI_SYNOPSIS
        #include <math.h>
        double scalbn(double <[x]>, int <[y]>);
        float scalbnf(float <[x]>, int <[y]>);

TRAD_SYNOPSIS
        #include <math.h>
        double scalbn(<[x]>,<[y]>)
        double <[x]>;
        int <[y]>;
        float scalbnf(<[x]>,<[y]>)
        float <[x]>;
        int <[y]>;

DESCRIPTION
<<scalbn>> and <<scalbnf>> scale <[x]> by <[n]>, returning <[x]> times
2 to the power <[n]>.  The result is computed by manipulating the
exponent, rather than by actually performing an exponentiation or
multiplication.

RETURNS
<[x]> times 2 to the power <[n]>.

PORTABILITY
Neither <<scalbn>> nor <<scalbnf>> is required by ANSI C or by the System V
Interface Definition (Issue 2).

*/

/* 
 * scalbn (double x, int n)
 * scalbn(x,n) returns x* 2**n  computed by  exponent  
 * manipulation rather than by actually performing an 
 * exponentiation or a multiplication.
 */
|#
(defun scale-float64 (x n)
    (declare (values double-float))
    (declare (type double-float x))
    (declare (type fixnum n))
  (prog* (
    (two54  #+nil 1.80143985094819840000e+16
            #.(encode-float64 #x43500000 #x00000000) )
    (twom54 #+nil 5.55111512312578270212e-17
            #.(encode-float64 #x3C900000 #x00000000) )
    (huge   #+nil 1.0e+300
            #.(encode-float64 #x7E37E43C #x8800759B) )
    (tiny   #+nil 1.0e-300
            #.(encode-float64 #x01A56E1F #xC2F8F359) )
    )
    ;;
    (multiple-value-bind (hx lx) (decode-float64 x)
    (let ((k (ash (logand hx #x7ff00000) -20))) ; extract exponent
      (when (eql k 0)   ; 0 or subnormal x
        (when (eql (logior lx (logand hx #x7fffffff)) 0)
          (return x) )  ; +-0
        (setq x (* x two54))
        (multiple-value-setq (hx lx) (decode-float64 x))
        (setq k (- (ash (logand hx #x7ff00000) -20) 54))
        ;; underflow
        (when (< n -50000) (return (* tiny x))) )

      (when (eql k #x7ff) (return (+ x x))) ; NaN or Inf 

      (incf k n)

      (when (> k  #x7fe)
        ;; overflow
        (return (* huge (float64-sign huge x))) )

      (when (> k  0)  ; normal result
        (return (encode-float64
            (logior (logand hx #x800fffff) (ash k 20)) lx )))

      (when (<= k -54)
        (return  (if (> n  50000)    ; in case integer overflow in n+k
            (* huge (float64-sign huge x))     ; *overflow*
            (* tiny (float64-sign tiny x)) ))) ; *underflow*
      (incf k 54)   ; subnormal result
      (let ((x (encode-float64 
                  (logior (logand hx #x800fffff) (ash k 20)) lx) ))
        (return (* x twom54)) ) ) ) ) )
