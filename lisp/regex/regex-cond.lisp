;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: EXTENSION; Base: 10 -*-
;;;;
;;;; regex - conditions
;;; lisp/regex/regx-defcond.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-cond.lisp#1 $
;;;
;;; Description:
;;;  This file contains definition of condition classes of regex facility.
;
(in-package :si)

;;;; regex-error
;;;
;;; Description:
;;;  Base class of runtime error of regex evaluation.
;
(define-condition regex-error (error)
  () )


;;;; regex-too-deep-recursion
;
(define-condition regex-too-deep-recursion (regex-error)
  () )


;;;; regex-parse-error
;
(define-condition regex-parse-error (parse-error)
  ((string
      :initarg  :string
      :initform (required)
      :type     string )
   (position
      :initarg  :position
      :initform (required)
      :type     sequence-index ))
  (:report (lambda (c s)
    (format s "~A~%; ~A~%; ~VT^"
        (class-name (class-of c))
        (slot-value c 'string)
        (+ (slot-value c 'position) 2) ) )) )


;;;; regex-invalid-backslash
;
(define-condition regex-invalid-backslash (regex-parse-error)
  ((char
      :initarg  :char
      :initform (required)
      :type     character )) )


;;;; regex-invalid-min-max
;
(define-condition regex-invalid-min-max (regex-parse-error)
  ((min
      :initarg  :min
      :initform (required)
      :type     sequence-index )
   (max
      :initarg  :max
      :initform (required)
      :type     sequence-index )) )


;;;; regex-invalid-range
;
(define-condition regex-invalid-range (regex-parse-error)
  ((start
      :initarg  :start
      :initform (required)
      :type     character )
   (end
      :initarg  :end
      :initform (required)
      :type     character )) )


;;;; regex-invalid-replacement
;
(define-condition regex-invalid-replacement (regex-parse-error)
  () )


;;;; regex-invalid-if-construct
;;;
;;; Signaled when (?(cond)then|else) has too many branches.
;
(define-condition regex-invalid-if-construct (regex-parse-error)
  () )


;;;; regex-invalid-if-condition
;;;
;;; Signaled when cond of (?(cond)then|else) is not:
;;;   1. an integer. (denoting group number)
;;;   2. positive-lookahead
;;;   3. negative-lookahead
;
(define-condition regex-invalid-if-condition (regex-parse-error)
  () )


;;;; regex-not-fixed-length
;
(define-condition regex-not-fixed-length (regex-parse-error)
  ((expression
      :initarg  :expression
      :initform (required)
      :type     regex-expr )) )


;;;; regex-not-supported
;
(define-condition regex-not-supported (regex-parse-error)
  () )


;;;; regex-simple-parse-error
;
(define-condition regex-simple-parse-error (regex-parse-error simple-condition)
  ()
  (:report (lambda (c s)
    (format s "~?~%; ~A~%; ~VT^"
        (simple-condition-format-control c)
        (simple-condition-format-arguments c)
        (slot-value c 'string)
        (+ (slot-value c 'position) 2) ) )) )


;;;; regex-syntax-error
;
(define-condition regex-syntax-error (regex-parse-error)
  () )


;;;; regex-unbound-group
;
(define-condition regex-unbound-group (regex-parse-error)
  ((index
      :initarg  :index
      :initform (required)
      :type     sequence-index )) )


;;;; regex-unclosed-pair
;
(define-condition regex-unclosed-pair (regex-parse-error)
  ((char
      :initarg  :char
      :initform (required)
      :type     character )) )


;;;; regex-unexpected-char
;
(define-condition regex-unexpected-char (regex-parse-error)
  ((expected-char
      :initarg  :expected-char
      :initform (required)
      :type     character )) )
