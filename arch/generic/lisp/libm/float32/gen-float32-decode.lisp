;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32-decode
;;; arch/generic/lisp/math/gen-math-f32-decode.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-decode.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of float32-decode.
;
(in-package :si)

;;;; integer-decode-float32
;;;     (byte 1 31) sign        0=positive, 1=negative
;;;     (byte 8 23) exponent    bias=127, 255=inf or NaN
;;;     (byte 23 0) significand
(defun integer-decode-float32 (x)
    (declare (values (unsigned-byte 24) (integer -149 255) (member 1 -1)))
    (declare (type single-float x))
  (let* ((ix (decode-float32 x))
         (sign         (if (< ix 0) -1 1))
         (exponent*    (ldb (byte 8 23) ix))
         (significand* (ldb (byte 23 0) ix)) )
    (case exponent*
      ((0)      ; subnormal
        (if (zerop significand*)
            (values 0 0 sign)
          (values significand* -149 sign) ) )
      ((255)
        (values significand* exponent* sign) )
      (otherwise
        (values (logior significand* (ash 1 23))
                (- exponent* 127 23)
                sign ) )) ) )
