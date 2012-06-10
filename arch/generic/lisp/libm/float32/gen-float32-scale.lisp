;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32-scale
;;; arch/generic/lisp/math/gen-math-f32-scale.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-scale.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of single-float scalbn.
;
(in-package :si)

#|
/* s_scalbnf.c -- float version of s_scalbn.c.
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 */

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
|#
(defun scale-float32 (x n)
    (declare (values single-float))
    (declare (type single-float x))
    (declare (type fixnum n))
  (prog* (
    (two25  #+nil 3.355443200e+07
            #.(encode-float32 #x4c000000) )
    (twom25 #+nil 2.9802322388e-08
            #.(encode-float32 #x33000000) )
    (huge   #+nil 1.0e+30
            #.(encode-float32 #x7149F2C9) )
    (tiny   #+nil 1.0e-30
            #.(encode-float32 #x0DA24261) )
    ;;
    (ix (decode-float32 x))
    (k  (ash (logand ix #x7f800000) -23))   ;  extract exponent
    )
    ;;
    (when (eql k 0) ; 0 or subnormal x
      (when (eql (logand ix #x7fffffff) 0) (return x))  ; +-0
      (setq x (* x two25))
      (setq ix (decode-float32 x))
      (setq k  (- (ash (logand ix #x7f800000) -23) 25)) )

    (when (eql k #xff) (return (+ x x)))    ; * NaN or Inf
    (incf k n)

    (when (or (> n 50000) (> k  #xfe))
      (return (* huge (float32-sign huge x))) )    ; overflow

    (when (< n -50000)
      (return (* tiny (float32-sign tiny x))) )    ; underflow

    (when (> k 0)   ; normal result
      (return (encode-float32 (logior (logand ix #x807fffff) (ash k 23)))) )

    (when (<= k -25)
      (return (* tiny (float32-sign tiny x))) )    ; underflow

    (incf k 25) ; subnormal result

    (let ((x (encode-float32 (logior (logand ix #x807fffff) (ash k 23)))))
      (return (* x twom25)) ) ) )

