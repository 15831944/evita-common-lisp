;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 3 Evaluation and Compilation
;;; lisp/runtime/r03-eval.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r03-eval.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of fllowing functions:
;;;     check-allow-other-keys
;;;     destructuring-mismatch
;;;     dotted-rest-argument
;;;     invalid-keyword-argument
;;;     odd-number-of-keyword-arguments
;;;     unrecognized-keyword-argument
;
(in-package :si)

;;;; 3.4.1.4.1 Suppressing Keyword Argument Checking
;;;
;
(defun check-allow-other-keys (args suppress-p)
  (let ((scan args)
        (supplied-p nil) )
    (loop
      (when (null scan)
        (return) )

      (unless (consp scan)
        (unless suppress-p
          (dotted-rest-argument args) )
        (return) )

      (let (key val)
        (setq key (pop scan))

        (when (null scan)
          (unless suppress-p
            (odd-number-of-keyword-arguments args) ))

        (unless (consp scan)
          (unless suppress-p
            (dotted-rest-argument args) )
          (return) )

        (setq val (pop scan))

        (unless supplied-p
          (when (eq :allow-other-keys key)
            (setq suppress-p val)
            (setq supplied-p t) )) )) )
  suppress-p )


;;;; 3.5.1.7 Destructuring Mismatch
;;;
;;; Use by:
;;;  parse-destructuring
;;;
;;; Description:
;;;  Signals destructuring mismatch.
;
(defun destructuring-mismatch (runner form pattern)
  (error 'destructuring-mismatch
        :expected-form pattern
        :form form
        :position runner ) )


;;;; Dotted Rest Argument
;;;
;;; Called by:
;;;   check-allow-other-keys
;;;   VmThread::ProcessKeyArgs
;
(defun dotted-rest-argument (args)
  (error 'dotted-rest-argument :argument args) )


;;;; 3.5.1.5  Invalid Keyword Arguments 
;;; Called by:
;;;   check-allow-other-keys
;;;   VmThread::ProcessKeyArgs
;
(defun invalid-keyword-argument (datum)
  (error 'simple-program-error
         :format-control "Keyword argument must be a symbol: ~S"
         :format-arguments (list datum) ) )


;;;; 3.5.1.6  Odd Number of Keyword Arguments
;;;
;;; Called by:
;;;   check-allow-other-keys
;;;   VmThread::ProcessKeyArgs
;
(defun odd-number-of-keyword-arguments (args)
  (error 'odd-number-of-keyword-arguments :arguments args) )


;;;; 3.5.1.4 Unrecognized Keyword Arguments
;;;
;;; Called by:
;;;   check-allow-other-keys
;;;   parse-destructuring-bind
;;;   VmThread::ProcessKeyArgs
;
(defun unrecognized-keyword-argument (keyword expected-keywords)
  (if (symbolp keyword)
      (error 'unrecognized-keyword-argument 
             :keyword           keyword
             :expected-keywords expected-keywords )
    (invalid-keyword-argument keyword) ) )
