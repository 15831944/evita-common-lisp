;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64-float
;;; arch/generic/lisp/math/gen-math-f64-float.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-float.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of float64-rationalize.
;
(in-package :si)

(defun float64-precision (x)
    (declare (values (integer 0 53)))
    (declare (type double-float x))
  (multiple-value-bind (hx lx) (decode-float64 x)
  (let ((ix (logand hx #x7fffffff)))
    (cond
      ((<= ix #x000ffff)   ; subnormal or zero
        (integer-length (logior (ash ix 32) (logand lx #xffffffff))) )
      ((>= ix #x7f800000)   ; inf or NaN
        1 )
      (t 53) ) ) ) )
