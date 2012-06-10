;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 12 Numbers - Float
;;; arch/generic/lisp/runtime/gen-r12-float.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r12-float.lisp#13 $
;
(in-package :si)

;;;;; 12.2.73 decode-float
(defun cl:decode-float (x)
  (typecase x
    (single-float
      (let* ((ix (decode-float32 x))
             (e  (- (ash (logand ix #x7f800000) -23) 127)) )
        (values (encode-float32
                    (logior (logand ix #x007fffff) (ash 126 23)) )
                (1+ e)
                (if (>= ix 0) 1f0 -1f0) ) ) )
    (double-float
      (multiple-value-bind (hx lx) (decode-float64 x)
        (let ((e (- (ash (logand #x7ff00000 hx) -20) 1023)))
          (values (encode-float64
                      (logior (logand hx #x000fffff) (ash 1022 20))
                      lx )
                  (1+ e)
                  (if (>= hx 0) 1d0 -1d0) ) ) ) )
    (otherwise (error 'type-error :datum x :expected-type 'float)) ) )


;;;; 12.2.74 float
;;; o 2006-09-29: We must check conversion of infinity and NaN.
;;; o Compiler transforms all call sites of coerce in below.
;;;
;;; Note: x86 and x64 returns infinity for large bignum.
;;; Note: x86 and x64 can convert infinity and NaN between single-float and
;;; double-float.
;;;
;;;  (float (1+ (ash #xFFFFFF 104)))    => #<Single-Float infinity>
;;;  (float (ash #xFFFFFF 104))         => most-positive-single-float
;;;
(defun cl:float (x &optional (proto 0f0))
  (typecase proto
    (double-float
      (typecase x
        (fixnum (coerce x 'double-float))
        (bignum (coerce/bignum/float64 x))
        ;; FIXME 2007-03-24: Is it better not to use intrinsic function FLOAT?
        ;; Note: (funcall float x) means intrinsic function FLOAT instead of
        ;; function FLOAT being defined.
        (ratio  (/ (funcall 'float (numerator x)   0d0)
                   (funcall 'float (denominator x) 0d0)) )
        (double-float x)
        (single-float (coerce x 'double-float))
        (otherwise (error 'type-error :datum x :expected-type 'real)) ) )
    (single-float
      (typecase x
        (fixnum (coerce x 'single-float))
        (bignum (coerce/bignum/float32 x))
        ;; Note: (funcall float x) means intrinsic function FLOAT instead of
        ;; function FLOAT being defined.
        (ratio  (/ (funcall 'float (numerator x))
                   (funcall 'float (denominator x)) ) )
        (double-float (coerce x 'single-float))
        (single-float x)
        (otherwise (error 'type-error :datum x :expected-type 'real)) ) )
    (otherwise
      (error 'type-error :datum proto :expected-type 'float) )) )


;;;; 12.2.73 float-digits
(defun cl:float-digits (x)
  (typecase x
    (single-float 24)
    (double-float 53)
    (otherwise (error 'type-error :datum x :expected-type 'float)) ) )


;;;; 12.2.73 float-precision
(defun cl:float-precision (x)
  (typecase x
    (single-float (float32-precision x))
    (double-float (float64-precision x))
    (otherwise (error 'type-error :datum x :expected-type 'float)) ) )


;;;; 12.2.73 float-radix
(defun cl:float-radix (x)
  (typecase x
    (single-float 2)
    (double-float 2)
    (otherwise (error 'type-error :datum x :expected-type 'float)) ) )


;;;; 12.2.73 float-sign
(defun cl:float-sign (x &optional (y x))
  (typecase x
    (single-float
      (typecase y
        (single-float (float32-sign x y))
        (double-float (float64-sign (float x 0d0) y))
        (otherwise (error 'type-error :datum y :expected-type 'float)) ) )
    (double-float
      (typecase y
        (single-float (float32-sign (float x) y))
        (double-float (float64-sign x y))
        (otherwise (error 'type-error :datum y :expected-type 'float)) ) )
    (otherwise (error 'type-error :datum x :expected-type 'float)) ) )


;;;;; 12.2.73 integer-decode-float
(defun cl:integer-decode-float (x)
  (typecase x
    (single-float (integer-decode-float32 x))
    (double-float (integer-decode-float64 x))
    (otherwise (error 'type-error :datum x :expected-type 'float)) ) )


;;;; 12.2.46 rational
(defun cl:rational (x)
    (declare (values rational))
    (declare (type real x))
  (typecase x
    (fixnum x)
    (bignum x)
    (ratio  x)
    (single-float (float32-rational x))
    (double-float (float64-rational x))
    (otherwise (error 'type-error :datum x :expected-type 'real)) ) )


;;;; 12.2.46 rationalize
(defun cl:rationalize (x)
    (declare (values rational))
    (declare (type real x))
  (typecase x
    (fixnum x)
    (bignum x)
    (ratio  x)
    (single-float (float32-rationalize x))
    (double-float (float64-rationalize x))
    (otherwise (error 'type-error :datum x :expected-type 'real)) ) )


;;;; 12.2.73 cl:scale-float
(defun cl:scale-float (x n)
  (typecase x
    (single-float (scale-float32 x n))
    (double-float (scale-float64 x n))
    (otherwise (error 'type-error :datum x :expected-type 'float)) ) )
