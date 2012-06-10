;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32-rational
;;; arch/generic/lisp/math/gen-math-f32-rational.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-rational.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of float32-rational.
;
(in-package :si)

(defun float32-rational (x)
    (declare (values rational))
    (declare (type single-float x))
  (let* ((hx (decode-float32 x))
         (ix (logand hx #x7FFFFFFF)) )
    (cond
      ((>= ix #x7F800000)   ; inf or NaN
        (error 'arithmetic-error
               :operation 'rational
               :operands (list x) ) )
      ((<= ix #x007FFFFF)    ; subnormal
        (let ((r (/ ix (ash 1 (+ 127 23 -1)))))
          (if (< ix 0) (- r) r) ) )
      (t    ; normal
        (let* ((sign (if (< hx 0) -1 1))
               (num (* (logior (ash 1 23) (logand ix #x007FFFFF)) sign))
               (k   (- (logand (ash ix -23) #xff) 127 23)) )
          (cond
            ((zerop k) num)
            ((plusp k) (ash num k))
            (t (/ num (ash 1 (- k)))) ) ) )) ) )
