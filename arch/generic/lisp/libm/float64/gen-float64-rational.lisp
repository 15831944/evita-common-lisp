;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64-rational
;;; arch/generic/lisp/math/gen-math-f64-rational.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-rational.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of float64-rational.
;
(in-package :si)

(defun float64-rational (x)
    (declare (values rational))
    (declare (type double-float x))
  (multiple-value-bind (hx lx) (decode-float64 x)
  (let ((ix (logand hx #x7FFFFFFF)))
    (cond
      ((>= ix #x7FF00000)   ; inf or NaN
        (error 'arithmetic-error
               :operation 'rational
               :operands (list x) ) )
      ((<= ix #x000FFFFF)    ; subnormal
        (let ((r (/ ix (ash 1 (+ 1023 52 -1)))))
          (if (< ix 0) (- r) r) ) )
      (t    ; normal
        (let* ((sign (if (< hx 0) -1 1))
               (num (* (logior (ash 1 52)
                               (ash (logand ix #x000FFFFF) 32)
                               (logand lx #xFFFFFFFF) )
                        sign ) )
               (k   (- (logand (ash ix -20) #x7FF) 1023 52)) )
          (cond
            ((zerop k) num)
            ((plusp k) (ash num k))
            (t (/ num (ash 1 (- k)))) ) ) )) ) ) )
