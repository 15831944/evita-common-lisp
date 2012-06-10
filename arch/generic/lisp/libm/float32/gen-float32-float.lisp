;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32-float
;;; arch/generic/lisp/math/gen-math-f32-float.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-float.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of float32-rationalize.
;
(in-package :si)

(defun float32-precision (x)
    (declare (values (integer 0 24)))
    (declare (type single-float x))
  (let* ((hx (decode-float32 x))
         (ix (logand hx #x7fffffff)) )
    (cond
      ((<= ix #x007fffff)   ; subnormal or zero
        (integer-length ix) )
      ((>= ix #x7f800000)   ; inf or NaN
        1 )
      (t 24) ) ) )
