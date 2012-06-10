;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: TC-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-07-cl-object.lisp#1 $
(in-package #:tc-user)

;;;; 7.7.7 update-instance-for-redefined-class
(deftest cl-07-07-001 ()
    (locally
        (declare (notinline make-instance))
      (setf (find-class 'foo) nil)
      (defclass foo () ((x :initform 10) (y :initform 20)))
      (let ((foo (make-instance 'foo)))
        (defclass foo ()
            ((a :initform 31) (x :initform 41)
             (z :initform 51) (y :initform 61) ) )
        (values (slot-value foo 'a)
                (slot-value foo 'x)
                (slot-value foo 'z)
                (slot-value foo 'y) ) ))
  (values 31 10 51 20) )


;;;; 7.7.8 change-class
(deftest cl-07-08-001 ()
    (progn
      (setf (find-class 'foo) nil)
      (setf (find-class 'bar) nil)
      (defclass foo ()
        ((x :initform 'foo-x) (y :initform 'foo-y) (z :initform 'foo-z)) )
      (defclass bar ()
        ((a :initform 'bar-a) (x :initform 'bar-x) (c :initform 'bar-c)) )
      (let* ((foo (make-instance 'foo))
             (foo-slots
                (list (slot-value foo 'x)
                      (slot-value foo 'y)
                      (slot-value foo 'z) ) )
             (bar (change-class foo 'bar))
             (bar-slots
                (list (slot-value bar 'a)
                      (slot-value bar 'x)
                      (slot-value bar 'c) )) )
        (values (bool (eq foo bar)) foo-slots bar-slots) ))
  (values t (foo-x foo-y foo-z) (bar-a foo-x bar-c)) )


;;;; 7.7.25 defclass
(deftest cl-07-25-001 ()
    (progn
      (fmakunbound 'foo-x)
      (fmakunbound '(setf foo-x))
      (fmakunbound 'foo-x2)
      (setf (find-class 'foo) nil)
      (defclass foo () ((x :accessor foo-x :reader foo-x2)))
      (labels (
        (check ()
          (list (length (clos:generic-function-methods #'foo-x))
                (length (clos:generic-function-methods #'(setf foo-x)))
                (length (clos:generic-function-methods #'foo-x2)) ) )
        )
        (let (before after)
          (setq before (check))
          (defclass foo () ())
          (setq after (check))
          (values before after) ) ))
  (values (1 1 1) (0 0 0)) )


;;;; 7.7.26 defgeneric

;;;; argument-precedence-order
(deftest cl-07-26-001 ()
    (progn
      (fmakunbound 'apo-test)
      (defgeneric apo-test (x y z)
        (:argument-precedence-order z y x)
        (:method (x y z)          (declare (ignore x y z)) '-)
        (:method ((x fixnum) y z) (declare (ignore   y z)) 'x)
        (:method (x (y fixnum) z) (declare (ignore x   z)) 'y)
        (:method (x y (z fixnum)) (declare (ignore x y  )) 'z) )
      (values
        (apo-test 1 '- '-)
        (apo-test '- 2 '-)
        (apo-test '- '- 3)
        (apo-test 1 2 3)
        (apo-test '- '- '-) ))
  (values x y z z -) )


;;;; 7.7.38 (setf class-name)
(deftest cl-07-38-001 ()
    "Renaming class doesn't affect find-class. When (eq (find-class (class-name C)) C), (class-name C) is proper name of class C."
;;; When
    (locally
        (declare (notinline find-class))
      (setf (find-class 'foo) nil)
      (setf (find-class 'bar) nil)
      (defclass foo () ())
      (let* ((foo (find-class (intern "FOO")))
             (bar
               (progn
                 (setf (class-name foo) 'bar)
                 (find-class (intern "BAR") nil) ) ))
        (values (null bar) (class-name foo)) ) )
  (values t bar) )
