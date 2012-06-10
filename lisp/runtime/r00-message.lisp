;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 0 Localizable Messages
;;; runtime/r00-messages.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r00-message.lisp#2 $
;;;
;
(in-package :si)

;;; 2 Syntax

(define-message :reader-error-dispatch-arg
  "~C~C-reader doesn't accept argument: ~S" )

(define-message :reader-error-comma
  "A comma appears outside the scope of a backquote or too many commas." )

(define-message :reader-error-eval
  "Read-time eval is disabled by *read-eval*." )

;;;; 3 Evlauation and Compilation

(define-message :program-error-malformed-form
  "Malformd form: ~S" )

;; syntax-error program-error


;;;; 23 Reader
(define-message :reader-not-dispatch-macro-character
  "~C isn't dispatch macro character." )
