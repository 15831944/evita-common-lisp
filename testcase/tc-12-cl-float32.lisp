;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-12-cl-float32.lisp#1 $
(in-package :tc-user)

;;; 12.2.54 rational
(deftest cl-12-54-301 () (rational +0.1f0)     +13421773/134217728)
(deftest cl-12-54-302 () (rational -0.1f0)     -13421773/134217728)
(deftest cl-12-54-303 () (rational +0.5f0)     +1/2)
(deftest cl-12-54-304 () (rational -0.5f0)     -1/2)
(deftest cl-12-54-305 () (rational +0.25f0)    +1/4)
(deftest cl-12-54-306 () (rational -0.25f0)    -1/4)
(deftest cl-12-54-307 () (rational +0.125f0)   +1/8)
(deftest cl-12-54-308 () (rational -0.125f0)   -1/8)
(deftest cl-12-54-309 () (rational +2.0f0)    +2)
(deftest cl-12-54-310 () (rational -2.0f0)    -2)
(deftest cl-12-54-311 () (rational +0.0)      0)
(deftest cl-12-54-312 () (rational -0.0)      0)

(deftest cl-12-54-313 () (rational least-positive-single-float)
    +1/713623846352979940529142984724747568191373312 )

(deftest cl-12-54-314 () (rational least-negative-single-float)
    -1/713623846352979940529142984724747568191373312 )

(deftest cl-12-54-313 () (rational most-positive-single-float)
    +340282346638528859811704183484516925440 )

(deftest cl-12-54-314 () (rational most-negative-single-float)
    -340282346638528859811704183484516925440 )

(deftest cl-12-54-315 () (rational least-positive-normalized-single-float)
    +1/85070591730234615865843651857942052864 )

(deftest cl-12-54-316 () (rational least-negative-normalized-single-float)
    -1/85070591730234615865843651857942052864 )

(deftest cl-12-54-317 () (rational single-float-epsilon)
    8388609/140737488355328 )
(deftest cl-12-54-318 () (rational single-float-negative-epsilon)
    8388609/281474976710656 )

;;; 12.2.54 rationalize
(deftest cl-12-54-350 () (rationalize +0.1f0) +1/10)
(deftest cl-12-54-351 () (rationalize -0.1f0) -1/10)
(deftest cl-12-54-352 () (rationalize +0.2f0) +1/5)
(deftest cl-12-54-353 () (rationalize -0.2f0) -1/5)
(deftest cl-12-54-354 () (rationalize +0.3f0) +3/10)
(deftest cl-12-54-355 () (rationalize -0.3f0) -3/10)
(deftest cl-12-54-356 () (rationalize +0.0f0) 0)
(deftest cl-12-54-357 () (rationalize -0.0f0) 0)


;;; 12.2.74 float
(deftest cl-12-74-301 ()
  (bool (eql (float 1.0 1.0d0) 1d0))
  t )

;;; 12.2.73 integer-decode-float

;;; 12.2.76 most-positive-single-float
;;; 12.2.76 least-positive-single-float
;;; 12.2.76 most-positive-single-float
;;; 12.2.76 most-positive-normalized-single-float
(deftest cl-12-76-301 ()
  (integer-decode-float most-positive-single-float)
  (values 16777215 104 1) )

(deftest cl-12-76-302 ()
  (integer-decode-float least-positive-single-float)
  (values 1 -149 1) )

(deftest cl-12-76-303 ()
  (integer-decode-float least-positive-normalized-single-float)
  (values 8388608 -149 1) )

;;; 12.2.76 most-negative-single-float
;;; 12.2.76 least-negative-single-float
;;; 12.2.76 most-negative-single-float
;;; 12.2.76 most-negative-normalized-single-float
(deftest cl-12-76-304 ()
  (integer-decode-float most-negative-single-float)
  (values 16777215 104 -1) )

(deftest cl-12-76-305 ()
  (integer-decode-float least-negative-single-float)
  (values 1 -149 -1) )

(deftest cl-12-76-306 ()
  (integer-decode-float least-negative-normalized-single-float)
  (values 8388608 -149 -1) )


;;; 12.2.77 single-float-epsilon                5.960465E-8
;;; 12.2.77 single-float-negative-epsilon       2.9802326E-8
(deftest cl-12-77-301 ()
  (integer-decode-float single-float-epsilon)
  (values 8388609 -47 1) )  ; #x800001

(deftest cl-12-77-302 ()
  (integer-decode-float single-float-negative-epsilon)
  (values 8388609 -48 1) )  ; #x800001

(deftest cl-12-77-303 ()
  (not (= (float 1) (+ (float 1) single-float-epsilon)))
  t )

(deftest cl-12-77-304 ()
  (not (= (float 1) (- (float 1) single-float-negative-epsilon)))
  t )
