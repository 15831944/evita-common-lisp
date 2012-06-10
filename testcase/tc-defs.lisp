;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;; testcase/t00-defs
;;;
;;; This file is part of Common Lisp Test Cases.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-defs.lisp#1 $
;;;
;
(in-package :cl-user)

(defpackage #:test-case
    (:nicknames #:tc)
    (:use #:cl #:ext)
    (:export
        #:deftest
        #:find-test
        #:sequence-equal
        #:test-case-plist
        #:run-test #:run-tests ) )


(defpackage #:test-case-user
    (:nicknames #:tc-user)
    (:use #:cl #:ext #:tc) )


(in-package #:tc)

;;;; expected-kind
(deftype expected-kind () '(member
    :boolean
    :primary
    :values
    :error ))

;;;; testc-ase
(defstruct test-case
  (name             nil         :type symbol)
  (form             nil         :type t)
  (expected         nil         :type cons)
  (documentation    ""          :type string)
  (next             nil         :type test-case)
  (prev             nil         :type test-case)
  (plist            '()         :type list)
  (dependee-list    '()         :type list)
  (depender-list    '()         :type list) )


(defmethod cl:print-object ((o test-case) s)
  (print-unreadable-object (o s)
    (format s "Test-Case ~S" (test-case-name o)) ) )


(declaim (ftype (function (symbol &optional (t))
    (or test-case null) )
  tc:find-test ))

(declaim (ftype (function ((or symbol test-case) &key &allow-other-keys)
    boolean )
  tc:run-test ))

(declaim (ftype (function (t t) t) tc:sequence-equal))

;;;; *test-case-anchor*
(defvar *test-case-anchor*
    (let ((anchor (make-test-case)))
      (setf (test-case-next anchor) anchor)
      (setf (test-case-prev anchor) anchor) ) )

;;;; *test-cases*
(defvar *test-cases* (make-hash-table :test 'eq))

;;;; *test-results*
(defvar *test-results* (make-hash-table :test 'eq))

;;;;; deftest
(defmacro deftest (&whole form
                   name (&rest plist &key &allow-other-keys)
                        &rest doc-form* )
  (labels (
    (parse (form*)
      (multiple-value-bind (test-form expected-kind expected)
          (case (length form*)
            ((2) (parse-short-form form*))
            ((3) (values (first form*) (second form*) (third form*)))
            (otherwise (error "Syntax error: ~S" form)) )
        (values test-form expected-kind expected) ) )

    (parse-short-form (form*)
      (let ((expected (second form*)))
        (if (not (consp expected))
            (values (first form*) :primary expected)
          (case (first expected)
            ((values) (values (first form*)  :values (rest expected)))
            (otherwise (values (first form*) :primary expected)) )) ) )
    )
    ;; deftest
    (check-type name symbol)
    (multiple-value-bind (form* doc)
        (if (stringp (first doc-form*))
            (values (rest doc-form*) (first doc-form*))
          (values doc-form* "") )
      (multiple-value-bind (test-form expected-kind expected) (parse form*)
       `(%deftest           ',name
          :plist            ',plist
          :documentation    ,doc
          :test-form        ',test-form
          :expected         '(,expected-kind ,expected) ) )) ) )
