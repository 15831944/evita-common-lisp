;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - runtime - Variable Declarations
;;; lisp/runtime/r00-vars.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r00-vars.lisp#2 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

(defvar *plist-table*)
(defvar *setf-table*)
(defvar *value-table*)

(defvar *plist-table-latch*)
(defvar *setf-table-latch*)
(defvar *value-table-latch*)

;;;; 9.2.25 *break-on-signals*
;
(deftlv cl:*break-on-signals* nil)


;;;; 9.2.24 *debugger-hook*
;
(deftlv cl:*debugger-hook* nil)


;;;; *handler-clusters*
;;;
;;; Used by:
;;;  handler-bind
;;;  signal
;;;
;;; Description:
;;;  List of handler-cluster.
;;;
;;;  handler-cluster  ::= List of handler
;;;  handler          ::= (typespec . handler-function)
;;;  typespec         ::= a type specifier
;;;  handler-function ::= a function desination of function that accept one
;;;                       argument of an instance of condition.
;
(deftlv *handler-clusters* '())


;;;; *restart-clusters*
;;;
;;; Used by:
;;;  compute-restarts
;;;  find-restart
;;;  restart-bind
;;;
;;; Description:
;;;  List of restart-cluster.
;;;
;;;  restart-cluster  ::= ( condition-or-nil restart ...)
;;;  condition-or-nil ::= null or an instance of condition.
;;;  restart          ::= an instance of restart.
;
(deftlv *restart-clusters* '())

;;;; 10.2.8 *gensym-counter*
;
(deftlv *gensym-counter* 0)

;;;; List Of All Packages Objects
;;;
;;; Note:
;;;  Users should not alter this list.
(defvar *all-packages*)


;;;; Default Use List For Make-Package
(defvar *package-default-use-list* (list (symbol-package 't)))


;;; BUGBUG: *char-name-table* and *name-char-table* should be defined in
;;; r13-char.lisp instead of r22-printer.lisp.
(defvar *char-name-table*)
(defvar *name-char-table*)
