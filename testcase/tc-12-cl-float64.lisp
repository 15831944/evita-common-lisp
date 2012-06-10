;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-12-cl-float64.lisp#1 $
(in-package :tc-user)

;;; 12.2.54 rational
(deftest cl-12-54-401 () (rational +0.1d0)
    +3602879701896397/36028797018963968 )

(deftest cl-12-54-402 () (rational -0.1d0)
    -3602879701896397/36028797018963968 )

(deftest cl-12-54-403 () (rational +0.5d0)     +1/2)
(deftest cl-12-54-404 () (rational -0.5d0)     -1/2)
(deftest cl-12-54-405 () (rational +0.25d0)    +1/4)
(deftest cl-12-54-406 () (rational -0.25d0)    -1/4)
(deftest cl-12-54-407 () (rational +0.125d0)   +1/8)
(deftest cl-12-54-408 () (rational -0.125d0)   -1/8)
(deftest cl-12-54-409 () (rational +2.0d0)    +2)
(deftest cl-12-54-410 () (rational -2.0d0)    -2)


;;; 12.2.54 rationalize
(deftest cl-12-54-450 () (rationalize +0.1d0) +1/10)
(deftest cl-12-54-451 () (rationalize -0.1d0) -1/10)
(deftest cl-12-54-452 () (rationalize +0.2d0) +1/5)
(deftest cl-12-54-453 () (rationalize -0.2d0) -1/5)
(deftest cl-12-54-454 () (rationalize +0.3d0) +3/10)
(deftest cl-12-54-455 () (rationalize -0.3d0) -3/10)
(deftest cl-12-54-456 () (rationalize +0.0d0) 0)
(deftest cl-12-54-457 () (rationalize -0.0d0) 0)
(deftest cl-12-54-458 () (rationalize pi) 245850922/78256779)


;;; 12.2.74 float
(deftest cl-12-74-401 ()
  (bool (eql (float 1.0d0 1.0f0) 1f0))
  t )

;;; 12.2.73 integer-decode-float

;;; 12.2.76 most-positive-double-float
;;; 12.2.76 least-positive-double-float
;;; 12.2.76 most-positive-double-float
;;; 12.2.76 most-positive-normalized-double-float
(deftest cl-12-76-401 ()
  (integer-decode-float most-positive-double-float)
  (values 9007199254740991 971 1) )

(deftest cl-12-76-402 ()
  (integer-decode-float least-positive-double-float)
  (values 1 -1074 1) )

(deftest cl-12-76-403 ()
  (integer-decode-float least-positive-normalized-double-float)
  (values 4503599627370496 -1074 1) )

;;; 12.2.76 most-negative-double-float
;;; 12.2.76 least-negative-double-float
;;; 12.2.76 most-negative-double-float
;;; 12.2.76 most-negative-normalized-double-float
(deftest cl-12-76-404 ()
  (integer-decode-float most-negative-double-float)
  (values 9007199254740991 971 -1) )

(deftest cl-12-76-405 ()
  (integer-decode-float least-negative-double-float)
  (values 1 -1074 -1) )

(deftest cl-12-76-406 ()
  (integer-decode-float least-negative-normalized-double-float)
  (values 4503599627370496 -1074 -1) )


;;; 12.2.77 double-float-epsilon            1.1102230246251568D-16
;;; 12.2.77 double-float-negative-epsilon   5.551115123125784D-17
(deftest cl-12-77-401 ()
  (integer-decode-float double-float-epsilon)
  (values 4503599627370497 -105 1) )  ; #x10000000000001

(deftest cl-12-77-402 ()
  (integer-decode-float double-float-negative-epsilon)
  (values 4503599627370497 -106 1) )  ; #x10000000000001

(deftest cl-12-77-403 ()
  (not (= (float 1) (+ (float 1) double-float-epsilon)))
  t )

(deftest cl-12-77-404 ()
  (not (= (float 1) (- (float 1) double-float-negative-epsilon)))
  t )
