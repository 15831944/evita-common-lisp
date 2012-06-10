;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - BOOT - Definiions
;;; lisp/clos/o02-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o02-defs.lisp#2 $
;;;
;;; Description:
;;;  This file contains forms for creating slot accessors.
;;;
;
(in-package :si)

;;;; param-info
(defstruct param-info
  (nreqs        0   :type (integer 0 #.call-arguments-limit))
  (nopts        0   :type (integer 0 #.call-arguments-limit))
  (keys         nil :type (or list (eql t)))
  (order        nil :type list)
  (lambda-list  nil :type list) )


;;;; *eql-specializers*
;;;
;;; Description:
;;;  Hash-table for eql-specializers.
;
(defvar *eql-specializers* (make-hash-table :test #'eql))


;;;; *standard-method-combination*
;
(defvar *standard-method-combination*
  (make-instance 'standard-method-combination :type 'standard) )


;;;; *early-generic-function-alist*
;;;
;;; Description:
;;;  Contains pairs of name and generic-function defined before full CLOS
;;;  is available.
;
(defvar *early-generic-function-alist* '())

;;;; *next-methods*
;;;
;;; Description:
;;;  List of method functions. Local function call-next-method and
;;;  next-method-p referes this variable.
;;;
;;;  Effective method function binds this variable to list of emf.
;
(ext:deftlv *next-methods* nil)
