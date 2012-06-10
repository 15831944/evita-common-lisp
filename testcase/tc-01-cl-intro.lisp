;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-01-cl-intro.lisp#1 $
(in-package #:tc-user)

;;; symbol
(deftest cl-01-10-001 ()
    (type-of (read-from-string "foo")) symbol )

(deftest cl-01-10-002 ()
    (type-of (read-from-string "nil")) null )

(deftest cl-01-10-003 ()
    (type-of (read-from-string "nil")) null )

(deftest cl-01-10-004 ()
    (type-of (read-from-string "12x")) symbol)

(deftest cl-01-10-005 ()
    (type-of (read-from-string "1.2x")) symbol)

(deftest cl-01-10-006 ()
    (type-of (read-from-string "+12x")) symbol)

(deftest cl-01-10-007 ()
    (type-of (read-from-string "+1.2x")) symbol)

(deftest cl-01-10-008 ()
    (type-of (read-from-string "1.2e0x")) symbol)

(deftest cl-01-10-009 ()
    (type-of (read-from-string "+12e0x")) symbol)

(deftest cl-01-10-010 ()
    (type-of (read-from-string "+1.2e0x")) symbol)

;;; fixnum
(deftest cl-01-12-001 ()
    (type-of (read-from-string "1")) fixnum )

(deftest cl-01-12-002 ()
    (type-of (read-from-string "-1")) fixnum )

(deftest cl-01-12-003 ()
    (type-of (read-from-string "+1")) fixnum )

(deftest cl-01-12-004 ()
    (type-of (read-from-string "0")) fixnum )

(deftest cl-01-12-005 ()
    (type-of (read-from-string "-0")) fixnum )

(deftest cl-01-12-006 ()
    (type-of (read-from-string "+0")) fixnum )

(deftest cl-01-12-004 ()
    (type-of (read-from-string "1234567")) fixnum )

(deftest cl-01-12-005 ()
    (type-of (read-from-string "-1234567")) fixnum )

(deftest cl-01-12-101 ()
    (type-of (read-from-string "+1234567")) fixnum )

(deftest cl-01-12-102 ()
    (type-of (read-from-string "123456789012345678901234567890")) bignum )

(deftest cl-01-12-103 ()
    (type-of (read-from-string "-123456789012345678901234567890")) bignum )

(deftest cl-01-12-104 ()
    (type-of (read-from-string "+123456789012345678901234567890")) bignum )

;;; single-float
(deftest cl-01-12-201 ()
    (type-of (read-from-string "1.0")) single-float )

(deftest cl-01-12-202 ()
    (type-of (read-from-string "-1.0")) single-float )

(deftest cl-01-12-203 ()
    (type-of (read-from-string "+1.0")) single-float )

(deftest cl-01-12-204 ()
    (type-of (read-from-string "0.0")) single-float )

(deftest cl-01-12-205 ()
    (type-of (read-from-string "-0.0")) single-float )

(deftest cl-01-12-206 ()
    (type-of (read-from-string "+0.0")) single-float )

;;; double-float
(deftest cl-01-12-301 ()
    (type-of (read-from-string "1.0d0")) double-float )

(deftest cl-01-12-302 ()
    (type-of (read-from-string "-1.0d0")) double-float )

(deftest cl-01-12-303 ()
    (type-of (read-from-string "+1.0d0")) double-float )

(deftest cl-01-12-304 ()
    (type-of (read-from-string "0.0d0")) double-float )

(deftest cl-01-12-305 ()
    (type-of (read-from-string "-0.0d0")) double-float )

(deftest cl-01-12-306 ()
    (type-of (read-from-string "+0.0d0")) double-float )

;;; ratio
(deftest cl-01-12-401 ()
    (type-of (read-from-string "3/5")) ratio )

(deftest cl-01-12-402 ()
    (type-of (read-from-string "-3/5")) ratio )

(deftest cl-01-12-403 ()
    (type-of (read-from-string "+3/5")) ratio )

;;; cons
(deftest cl-01-14-001 ()
    (type-of (read-from-string "()")) null )

(deftest cl-01-14-002 ()
    (type-of (read-from-string "(a b)")) cons )

(deftest cl-01-14-003 ()
    (type-of (read-from-string "(a . b)")) cons )

(deftest cl-01-14-004 ()
    (type-of (read-from-string "(a  b . c)")) cons )
