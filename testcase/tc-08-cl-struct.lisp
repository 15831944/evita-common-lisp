;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; evcl - test - 8 Structures.
;;; lisp/test/t08-struct
;;;
;;; This file is NOT part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2004 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-08-cl-struct.lisp#1 $
;;;
;;; Description:
;;;  This file contains test cases of structure.
;
(in-package :tc-user)

;;;
;;; BOA construtors
;;;

(deftest cl-08-01-001 ()
  "initform for slot x is nerver called."
    (defstruct (struct-boa-01 (:constructor make-struct-boa-01 (&aux x)))
      (x (error "nerver called")) )
  struct-boa-01 )

(deftest cl-08-01-002 () (progn (make-struct-boa-01) 'ok) ok)

(deftest cl-08-01-003 ()
    (defstruct (struct-boa-02 (:constructor make-struct-boa-02 (&optional x)))
      (x 123) )
  struct-boa-02 )

(deftest cl-08-01-004 ()
    (list (struct-boa-02-x (make-struct-boa-02))
          (struct-boa-02-x (make-struct-boa-02 456)) )
  (123 456) )

(deftest cl-08-01-005 ()
    (defstruct (struct-boa-03
                  (:constructor make-struct-boa-03 (&optional (x 'abc))) )
      (x 123) )
  struct-boa-03 )

(deftest cl-08-01-006 ()
    (list (struct-boa-03-x (make-struct-boa-03))
          (struct-boa-03-x (make-struct-boa-03 456)) )
  (abc 456) )


(deftest cl-08-01-007 ()
    (defstruct (struct-boa-04
                  (:constructor make-struct-boa-04 (&optional (x 'abc y))) )
      (x 123) (y 456) )
  struct-boa-04 )

(deftest cl-08-01-008 ()
    (let ((a (make-struct-boa-04))
          (b (make-struct-boa-04 'xyz)) )
      (list (struct-boa-04-x a) (bool (struct-boa-04-y a))
            (struct-boa-04-x b) (bool (struct-boa-04-y b)) ) )
  (abc nil xyz t) )


(deftest cl-08-01-009 ()
    (defstruct (struct-boa-05 (:constructor make-struct-boa-05 (&key x)))
      (x 123) )
  struct-boa-05 )

(deftest cl-08-01-010 ()
    (list (struct-boa-05-x (make-struct-boa-05))
          (struct-boa-05-x (make-struct-boa-05 :x 456)) )
  (123 456) )


(deftest cl-08-01-011 ()
    (defstruct (struct-boa-06
                  (:constructor make-struct-boa-06 (&key (x 'abc))) )
      (x 123) )
  struct-boa-06 )

(deftest cl-08-01-012 ()
    (list (struct-boa-06-x (make-struct-boa-06))
          (struct-boa-06-x (make-struct-boa-06 :x 456)) )
  (abc 456) )


(deftest cl-08-01-013 ()
    (defstruct (struct-boa-07
                  (:constructor make-struct-boa-07 (&key (x 'abc y))) )
      (x 123) (y 456) )
  struct-boa-07 )

(deftest cl-08-01-014 ()
    (let ((a (make-struct-boa-07))
          (b (make-struct-boa-07 :x 'xyz)) )
      (list (struct-boa-07-x a) (struct-boa-07-y a)
            (struct-boa-07-x b) (bool (struct-boa-07-y b)) ) )
  (abc nil xyz t) )


(deftest cl-08-01-015 ()
    (defstruct (struct-boa-08
                (:constructor make-struct-boa-08 (&rest x)) )
      x )
  struct-boa-08 )

(deftest cl-08-01-016 ()
    (let ((a (make-struct-boa-08 1 2 3)))
      (struct-boa-08-x a) )
  (1 2 3) )

(deftest cl-08-01-017 ()
    (defstruct (struct-boa-09
                (:constructor make-struct-boa-09 (x &rest y)) )
      x y )
  struct-boa-09 )

(deftest cl-08-01-018 ()
    (let ((a (make-struct-boa-09 1 2 3)))
      (values (struct-boa-09-x a) (struct-boa-09-y a)) )
  (values 1 (2 3)) )


(deftest cl-08-01-019 ()
    (defstruct (struct-boa-10
                (:constructor make-struct-boa-10 (x &optional y &rest z)) )
      x y z)
  struct-boa-10 )

(deftest cl-08-01-020 ()
    (let ((a (make-struct-boa-10 1 2 3 4 5)))
      (values (struct-boa-10-x a)
              (struct-boa-10-y a)
              (struct-boa-10-z a) ) )
  (values 1 2 (3 4 5)) )

(deftest cl-08-01-021 ()
    (defstruct (struct-boa-11
                (:constructor make-struct-boa-11
                    (x &optional y &rest z &aux (w 0)) ))
      x y z w )
  struct-boa-11 )

(deftest cl-08-01-022 ()
    (let ((a (make-struct-boa-11 1 2 3 4 5)))
      (values (struct-boa-11-x a)
              (struct-boa-11-y a)
              (struct-boa-11-z a)
              (struct-boa-11-w a) ) )
  (values 1 2 (3 4 5) 0) )


;;; Test slot inheritance
(deftest cl-08-02-001 ()
    (progn
      (setf (find-class 'a) nil)
      (setf (find-class 'b) nil)
      (setf (find-class 'c) nil)

      (defstruct a (x 1))
      (defstruct (b (:include a (x 2))))
      (defstruct (c (:include b (x 3))))
      (defstruct (d (:include b)))

      (list (slot-value (make-a) 'x)
            (slot-value (make-b) 'x)
            (slot-value (make-c) 'x)
            (slot-value (make-d) 'x) ) )
  (1 2 3 2) )


;;; Set initform for inherited slot.
(deftest cl-08-03-001 ()
  (progn
    (setf (find-class 'a) nil)
    (setf (find-class 'b) nil)
    (setf (find-class 'c) nil)
    (defstruct a (x 1)) )
  a )

(deftest cl-08-03-002 (:depend-to (cl-08-03-001))
    (defstruct (b (:include a (x 2)))) b )

(deftest cl-08-03-003 (:depend-to (cl-08-03-002))
    (defstruct (c (:include b))) c )

(deftest cl-08-03-004 (:depend-to (cl-08-03-003))
  (defstruct (d (:include a))) d )

(deftest cl-08-03-005 (:depend-to (cl-08-03-004))
    (let ((a (make-a)) (b (make-b)) (c (make-c)) (d (make-d)))
      (values (a-x a) (b-x b) (c-x c) (d-x d)) )
  (values 1 2 2 1) )
