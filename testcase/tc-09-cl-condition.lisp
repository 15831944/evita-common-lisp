;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; evcl - test - 9 Conditions.
;;; lisp/test/t09-condition
;;;
;;; This file is NOT part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2004 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-09-cl-condition.lisp#1 $
;;;
;;; Description:
;;;  This file contains test cases of conditions.
;
(in-package :tc-user)

;;;; 9.2.28  ignore-errors 

;; OPT_CFG2SSA: extend pass: CLOSE_TAGBODY
(deftest cl-09-28-001 ()
    (multiple-value-bind (v c) (ignore-errors (|undefined function|))
      (values v (bool (typep c 'undefined-function))) )
  (values nil t) )

;; OPT_CFG2SSA: extend pass: ABORT_TAGBODY
(deftest cl-09-28-002 ()
    (multiple-value-bind (v c) (ignore-errors (error "foo"))
      (values v (bool (typep c 'simple-error))) )
  (values nil t) )

(deftest cl-09-28-003 ()
    (multiple-value-bind (v c) (ignore-errors (single-value "some"))
      (values v c) )
  (values "some" nil) )


;;;; 9.2.34  invoke-restart 
(deftest cl-09-34-001 ()
    (let ((foo (make-condition 'simple-condition)))
        (restart-case 
            (with-condition-restarts foo (list (find-restart 'alpha))
              (restart-case
                  (invoke-restart (find-restart 'alpha foo))
                (alpha () 2)))
          (alpha () 1)))
  1 )

;; 9.2.43 muffle-warning
;;  This test case checks handling of non-local exit and non-local variable
;;  access.
(deftest cl-09-43-001 ()
    (let ((warnings '()))
      (handler-bind (
        (warning (lambda (c)
          (push c warnings)
          (invoke-restart 'muffle-warning) ) )
      )
      (warn "foo")
      (warn "bar")
      (warn "baz")
      (mapcar #'simple-condition-format-control warnings ) ) )
  ("baz" "bar" "foo") )
