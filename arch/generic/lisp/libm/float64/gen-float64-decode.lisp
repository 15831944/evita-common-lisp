;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64-decode
;;; arch/generic/lisp/math/gen-math-f64-decode.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-decode.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of float64-decode.
;
(in-package :si)

;;;; integer-decode-float64
;;;     (byte  1 63)    sign        0=positive, 1=negative
;;;     (byte 11 52)    exponent    bias=1023 2047=inf or NaN
;;;     (byte 52  0)    signficand
(defun integer-decode-float64 (x)
    (declare (values (unsigned-byte 53) (integer -1074 2047) (member 1 -1)))
    (declare (type double-float fval))
  (multiple-value-bind (hx lx) (decode-float64 x)
    (let ((sign (if (< hx 0) -1 1))
          (exponent* (ldb (byte 11 20) hx))
          (significand* (logior (ash (ldb (byte 20 0) hx) 32)
                                (logand lx (1- (ash 1 32))) )) )
      (case exponent*
        ((0)        ; subnormal
          (if (zerop significand*)
              (values 0 0 sign)
            (values significand* -1074 sign) ) )
        ((2047)     ; inf or NaN
          (values significand* exponent* sign) )
        (otherwise
          (values (logior significand* (ash 1 52))
                  (- exponent* 1023 52)
                  sign ) )) ) ) )
