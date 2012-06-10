;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 Numbers - Math
;;; lisp/runtime/r12-math.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r12-math.lisp#3 $
;;;
;
(in-package :si)

;;;; 12.2.21 acos
;;;
;;; Note:
;;;   acos z = -i log (z + i sqrt{1 - z^2})
;
(defun cl:acos (number)
  (etypecase number
    (real
      (if (> (abs number) 1)
          (setq number (complex 0 number))
        (.acos number) ) )
    (complex
      (let* ((z  number)
             (z2  (sqrt (- 1 (* z z))))
             (iz2 (complex (- (imagpart z2)) (realpart z2)))
             (z3  (log (+ iz2 z))) )
        (complex (imagpart z3) (- (realpart z3))) ) )) )


;;;; 12.2.23 acosh
;;;
;;; Note:
;;;  acosh z = 2 log(sqrt((z+1)/2) + sqrt((z-1)/2))
;
(defun cl:acosh (z)
  (* 2 (log (+ (sqrt (/ (+ z 1) 2)) (sqrt (/ (- z 1) 2))))) )


;;;; 12.2.21 asin
;;;
;;; Note:
;;;   asin z = -i log (iz + sqrt{1 - z^2})
;
(defun cl:asin (number)
  (etypecase number
    (real
      (if (> (abs number) 1)
          (setq number (complex 0 number))
        (.asin number) ) )
    (complex
      (let* ((z  number)
             (iz (complex (- (imagpart z)) (realpart z)))
             (z2 (sqrt (- 1 (* z z))))
             (z3 (log (+ iz z2))) )
        (complex (imagpart z3) (- (realpart z3))) ) )) )


;;;; 12.2.23 asinh
;;;
;;; Note:
;;;  asinh z = log(z + sqrt((1+x^2)))
;
(defun cl:asinh (z)
  (log (+ z (sqrt (+ 1 (* z z))))) )


;;;; 12.2.21 atan
;;;
;;; Syntax:
;;;     atan number1 &optional number2 => radians
;;;
;;; Arguments and Values:
;;;     number1 - a real if number2 is supplied.
;;;     number2 - a real.
;;;     radians - a number of radians.
;;;
;;; Note:
;;;  atan z = -i log((1 + iz) sqrt{1/(1+z^2)})
;
(defun cl:atan (number1 &optional (number2 nil number2-p))
  (if number2-p
      (.atan2 number1 number2)
    (etypecase number1
     (real (.atan number1))
     (complex
       (let* ((z  number1)
              (z2 (complex (1+ (- (imagpart z))) (realpart z)))
              (z3 (* z2 z2))
              (z4 (1+ (* z z)))
              (z5 (/ z3 z4))
              (z6 (/ (log z5) 2)) )
         (complex (imagpart z6) (- (realpart z6))) ) ))) )


;;;; 12.2.23 atanh
;;;
;;; Note:
;;;  atanh z = (log(1+x) - log(1-x))/2
;
(defun cl:atanh (z)
  (/ (- (log (+ 1 z) (log (- 1 z)))) 2) )


;;;; 12.2.20 cos
;;;
;;; Syntax:
;;;     cos radians => number
;;;
;;; Arguments and Values:
;;;     radians - a number given in radians.
;;;     number   - a number.
;;;
;;; Note:
;;;     cos z = (e^{iz} + e^{-iz}) / 2
;;;
(defun cl:cos (radians)
  (etypecase radians
    (real (.cos radians))
    (complex
      (let* ((e^iz (exp (complex (imagpart radians) (realpart radians))))
             (e^-iz (/ 1 e^iz)) )
        (/ (+ e^iz e^-iz) 2) )) ) )


;;;; 12.2.23 cosh
;;;
;;; Syntax:
;;;     cosh number => result
;
(defun cl:cosh (z)
  (/ (+ (exp z) (exp (- z))) 2) )


;;;; 12.2.31 expt
;;;
;;; Syntax:
;;;     expt base-number power-number => result
;;;
;;; Arguments and Values:
;;;     base-number - a number.
;;;     power-number - a number.
;;;
;;; Note:
;;;  If power-number is an integer and base-number is (or rational
;;;  (complex rational)), result is also (or rational (complex rational)))
;
(defun expt/integer (base-number power-number)
    (declare (type number base-number))
    (declare (type (integer 1 *) power-number))
  (let ((result 1))
    (loop
      (cond
        ((= 0 power-number)
          (return) )
        ((logbitp 0 power-number)
          (setq result (* result base-number))
          (decf power-number) )
        (t
          (setq base-number (* base-number base-number))
          (setq power-number (ash power-number -1)) )) )
     result ) )


;;; (expt 2 n)   => (ash 1 n)
;;; (expt b n/2) => (sqrt (expt b n))
;;; (expt b p)   => (exp (* p (log b)))
;
(defun cl:expt (base-number power-number)
    (declare (type number base-number))
    (declare (type number power-number))
    (declare (values number))
  (cond
    ((zerop power-number)
      (float-contagion-2 1 base-number power-number) )

    ((typep power-number 'fixnum)
      (cond
        ((eql 2 base-number)
            (ash 1 power-number) )
        ((plusp power-number)
          (expt/integer base-number power-number) )
        (t
          (/ 1 (expt/integer base-number (- power-number))) )) )

    ;; b ** n/m
    ((and (typep power-number 'ratio)
          (= (denominator power-number) 2) )
      (sqrt (expt base-number (numerator power-number))) )

    (t
      (exp (* power-number (log base-number))) )) )


;;;; 12.2.20 sin
;;;
;;; Syntax:
;;;     sin radian => number
;;; Note:
;;;  sin z = (e^{iz} - e^{-iz}) / 2i
;
(defun cl:sin (radians)
  (etypecase radians
    (real (.sin radians))
    (complex
      (let* ((e^iz (exp (complex (imagpart radians) (realpart radians))))
             (e^-iz (/ 1 e^iz)) )
        (/ (- e^iz e^-iz) 2) ) )) )


;;;; 12.2.23 sinh
;;;
;;; Syntax:
;;;     sinh number => result
;
(defun cl:sinh (z)
  (/ (- (exp z) (exp (- z))) 2) )


;;;; 12.2.20 tan
;;;
;;; Syntax:
;;;     tan radians => number
;;;
;;; Arguments and Values:
;;;     radians - a number given in radians.
;;;     number   - a number.
;;;
;;; Note:
;;;  tan z = (e^{iz} - e^{-iz}) / i (e^{iz} + e^{-iz})
;;;
(defun cl:tan (radians)
  (etypecase radians
    (real (.tan radians))
    (complex
      (let* ((e^iz  (exp (complex (imagpart radians) (realpart radians))))
             (e^-iz (/ 1 e^iz))
             (w  (/ (- e^iz e^-iz) (+ e^iz e^-iz))) )
        (complex (imagpart w) (realpart w)) ) )) )


;;;; 12.2.23 tanh
;;;
;;; Syntax:
;;;     tanh number => result
;
(defun cl:tanh (z)
  (let ((e^z  (exp z))
        (e^-z (exp (- z))) )
    (/ (- e^z e^-z) (+ e^z e^-z)) ) )
