;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 Numbers
;;; lisp/runtime/r12-number.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r12-number.lisp#5 $
;;;
;;; See Also:
;;;  macro/m12-number.lisp
;;;  runtime/r12-byte.lisp
;;;  runtime/r12-integer.lisp
;;;  runtime/r12-random.lisp
;;;
;;; Description:
;;;  This file contains following functions:
;;;     *                           12.2.24
;;;     +                           12.2.25
;;;     -                           12.2.26
;;;     /                           12.2.27
;;;;    1+                          12.2.28
;;;     1-                          12.2.28
;;;     =                           12.2.15
;;;     /=                          12.2.15
;;;     <                           12.2.15
;;;     <=                          12.2.15
;;;     >                           12.2.15
;;;     >=                          12.2.15
;;;     abs                         12.2.29     intrinsic
;;;     acos                        12.2.21
;;;     acosh                       12.2.23
;;;     asin                        12.2.21
;;;     asinh                       12.2.23
;;;     atan                        12.2.21
;;;     atanh                       12.2.23
;;;     ash                         12.2.56     intrinsic
;;;     boole                       12.2.60
;;;     complex                     12.2.46
;;;     complexp                    12.2.47     boot
;;;     conjugate                   12.2.48
;;;     cis                         12.2.45     intrinsic
;;;     cos                         12.2.20
;;;     cosh                        12.2.23
;;;     decode-float                12.2.73
;;;     denominator                 12.2.53
;;;     exp                         12.2.31     intrinsic
;;;     expt                        12.2.31
;;;     float                       12.2.74     intrinsic
;;;     float-digits                12.2.73
;;;     floatp                      12.2.75     boot
;;;     float-precision             12.2.73
;;;     float-radix                 12.2.73
;;;     float-sign                  12.2.73
;;;     imagpart                    12.2.50
;;;     integer-decode-float        12.2.73     intrinsic
;;;     log                         12.2.35     intrinsic
;;;     max                         12.2.16
;;;     min                         12.2.16
;;;     minusp                      12.2.17     primitive
;;;     numberp                     12.2.44
;;;     numerator                   12.2.53
;;;     plusp                       12.2.17     primitive
;;;     rationalp                   12.2.55
;;;     rational                    12.2.54     intrinsic
;;;     rationalize                 12.2.54
;;;     realp                       12.2.52
;;;     realpart                    12.2.50
;;;     scale-float                 12.2.73     intrinsic
;;;     signum                      12.2.37
;;;;    sin                         12.2.20
;;;     sinh                        12.2.23
;;;     sqrt                        12.2.38     intrinsic
;;;     tan                         12.2.20
;;;     tanh                        12.2.23
;;;     upgraded-complex-part-type  12.2.51 mostly stab
;;;     zerop                       12.2.18     primitive
;
(in-package :si)

;;;; 12.2.24 *
(defun cl:* (&rest numbers)
    (declare (dynamic-extent numbers))
  (let ((product 1))
    (dolist (number numbers product)
      (setq product (*_2op product number)) ) ) )


;;;; 12.2.25 +
(defun cl:+ (&rest numbers)
    (declare (dynamic-extent numbers))
  (let ((sum 0))
    (dolist (number numbers sum)
      (setq sum (+_2op sum number)) ) ) )


;;;; 12.2.25 -
(defun cl:- (number &rest numbers)
    (declare (dynamic-extent numbers))
  (if (null numbers)
      (-_1op number)
   (dolist (number2 numbers number)
      (setq number (-_2op number number2)) )) )


;;;; 12.2.25 /
;;;
;;; Syntax:
;;;     / number &rest numbers => sum
;
(defun cl:/ (number &rest numbers)
    (declare (dynamic-extent numbers))
  (if (null numbers)
      (/_2op 1 number)
   (/_2op number (apply #'* numbers)) ) )


;;;; 12.2.28 1+
;;;
;;; Syntax:
;;;     1+ number => successor
;
#|
(defun cl:1+ (number)
  (+_2op number 1) )
|#

;;;; 12.2.28 1-
;;;
;;; Syntax:
;;;     1- number => predecessor
;
#|
(defun cl:1- (number)
  (-_2op number 1) )
|#

;;;; 12.2.15 =
(defun cl:= (number1 &rest numbers)
    (declare (dynamic-extent numbers))
  (dolist (number2 numbers t)
    (unless (=_2op number1 number2)
      (return nil) ) ) )


;;;; 12.2.15 /=
(defun cl:/= (number1 &rest numbers)
    (declare (dynamic-extent numbers))
  (loop
    (when (null numbers) (return t))
    (dolist (number2 numbers)
      (unless (/=_2op number1 number2)
        (return-from /= nil) ) )
    (pop numbers) ) )


;;;; 12.2.15 <
(defun cl:< (real1 &rest reals)
    (declare (dynamic-extent reals))
  (if (null reals)
      (progn (check-type real1 real) t)
    (dolist (real2 reals t)
      (unless (<_2op real1 real2)
        (return nil) )
      (setq real1 real2) )) )


;;;; 12.2.15 <=
(defun cl:<= (real1 &rest reals)
    (declare (dynamic-extent reals))
  (if (null reals)
      (progn (check-type real1 real) t)
    (dolist (real2 reals t)
      (unless (<=_2op real1 real2)
        (return nil) )
      (setq real1 real2) )) )


;;;; 12.2.15 >
;;;
;;; Syntax:
;;;     >  &rest numbers+ => decreased-p
;
(defun cl:> (real1 &rest reals)
    (declare (dynamic-extent reals))
  (if (null reals)
      (progn (check-type real1 real) t)
    (dolist (real2 reals t)
      (unless (>_2op real1 real2)
        (return nil) )
      (setq real1 real2) )) )


;;;; 12.2.15 >=
;;;
;;; Syntax:
;;;     >=  &rest numbers+ => not-increased-p
;
(defun cl:>= (real1 &rest reals)
    (declare (dynamic-extent reals))
  (if (null reals)
      (progn (check-type real1 real) t)
    (dolist (real2 reals t)
      (unless (>=_2op real1 real2)
        (return nil) )
      (setq real1 real2) )) )


;;;; 12.2.48 conjugate
(defun cl:conjugate (number)
  (etypecase number
    (real number)
    (complex (complex (realpart number) (- (imagpart number)))) ) )


;;;; 12.2.16 max
;
(defun max (real1 &rest reals)
  (dolist (real2 reals real1)
    (when (< real1 real2) (setq real1 real2)) ) )


;;;; 12.2.16 min
;
(defun min (real1 &rest reals)
  (dolist (real2 reals real1)
    (when (> real1 real2) (setq real1 real2)) ) )


;;;; 12.2.17 minusp
;;;
;;; Syntax:
;;;     minusp real => boolean
;
#|
(defun cl:minusp (real)
  (< real 0) )
|#


;;;; 12.2.44 numberp
;;;
;;; Syntax:
;;;     numberp object => number-p
;;;
;;; Arguments and Values:
;;;     object   - an object.
;;;     number-p - a generalized boolean.
;;;
;;; BUGBUG: Should use type code base predicate.
;;;
#|
(defun cl:numberp (object)
  (or (realp object) (complexp object)) )
|#


;;;; 12.2.17 plusp
;;;
;;; Syntax:
;;;     plusp real => boolean
;
#|
(defun cl:plusp (real)
  (> real 0) )
|#


;;;; 12.2.55 rationalp
;;;
;;; Syntax:
;;;     rationalp object => rational-p
;;;
;;; Arguments and Values:
;;;     object   - an object.
;;;     rational-p - a generalized boolean.
;;;
;;; BUGBUG: Should use type code base predicate.
;;;
;
#+nil
(defun cl:rationalp (object)
  (or (integerp object) (typep object 'ratio)) )


;;;; 12.2.46 rationalize
;;;
;;; Copy from CMUCL
;;; Thanks to Kim Fateman, who stole this function rationalize-float
;;; from macsyma's rational. Macsyma'a rationalize was written
;;; by the legendary Gosper (rwg). Gosper is now working for Symbolics.
;;; Guy Steele said about Gosper, "He has been called the
;;; only living 17th century mathematician and is also the best
;;; pdp-10 hacker I know." So, if you can understand or debug this
;;; code you win big.
(defun cl:rationalize (x)
  (declare (type real x))

  (when (or (rationalp x) (zerop x))
    (return-from rationalize x) )

  (when (minusp x)
    (return-from rationalize (- (rationalize (- x)))) )

  (let ((eps (etypecase x
                    (single-float       single-float-epsilon)
                    (double-float       double-float-epsilon)
                    (short-float        short-float-epsilon)
                    (long-float         long-float-epsilon) ))
             (a (truncate x))
             y )
  (do ((xx x (setq y (/ (float 1.0 x) (- xx (float a x)))))
       (num a (+ (* (setq a (truncate y)) num) onum))
       (den 1 (+ (* a den) oden))
       (onum 1 num)
       (oden 0 den))
      ((and (not (zerop den))
            (not (> (abs (/ (- x (/ (float num x) (float den x))) x)) eps)))
       (/ num den) ) ) ) )


#|
;;;; 12.2.52 realp
;;;
;;; Syntax:
;;;     realp object => real-p
;;;
;;; Arguments and Values:
;;;     object   - an object.
;;;     real-p - a generalized boolean.
;;;
;;; BUGBUG: Should use type code base predicate.
;;;
(defun cl:realp (object)
  (or (rationalp object) (floatp object)) )
|#


;;;; 12.2.37 signum
;
(defun cl:signum (x)
  (cond
    ((zerop     x) x)
    ((rationalp x) (if (plusp x) 1 -1))
    ((floatp    x) (if (plusp x) (float 1 x) (float -1 x)))
    (t             (/ x (abs x))) ) )


;;;; 12.2.51 upgraded-complex-part-type
;;;
;;; Note: evcl doesn't support specialized complex.
;
(defun cl:upgraded-complex-part-type (typespec &optional env)
    (declare (ignore typespec env))
  'real )
