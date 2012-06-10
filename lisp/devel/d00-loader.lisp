;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - devel - Loader
;;; dev/d00-loader.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d00-loader.lisp#2 $
;;;
;;; See Also:
;;;     r00-loader
;;;     r00-compiler
;;;
;;; Description:
;;;  This file contains functions for loading definition at compile-time:
;;;     %define-compiler-macro
;;;     %define-setf-expander
;;;     %define-symbol-macro
;;;     %defclass
;;;     %defmacro
;;;     %defsetf
;;;     %defstruct
;;;     %deftype
;;;     %defun
;;;     %defvar
;
(in-package :xc)

;;;; %define-compiler-macro
;
(defun %define-compiler-macro (name lambda-list expander)
  (compile-notice 'define-compiler-macro name lambda-list)
  (when *environment*
    (setf (compiler-macro-function name) expander) )
  ;(si::maybe-set-function-name expander)
  name )


;;;; %define-setf-expander
;;;
;;; See Also: %defsetf
;
(defun %define-setf-expander (name lambda-list expander)
  (compile-notice 'define-setf-expander name lambda-list)
  (when *environment*
    (let ((table (si::.env-writers *environment*)))
      (setf (gethash name table) `(:macro (:macro . ,expander))) ) )
  ;(si::maybe-set-function-name expander)
  name )


;;;; %define-symbol-macro
;;;
;;; BUGBUG: check redefinition.
;
(defun %define-symbol-macro (name expansion)
  (compile-notice 'define-symbol-macro name)
  (let ((kind (xc::variable-information name)))
    (when (or (eq ':special kind) (eq ':constant kind))
      (error 'symbol-macro-clobbers-variable :name name) ) )
  (when *environment*
    (let ((table (si::.env-variables *environment*)))
      (setf (gethash name table)
        `(:symbol-macro (:symbol-macro . ,expansion)) ) ))
  name )


;;;; %defclass
;;;
;;; BUGBUG: This is stab. We'll use actual version during developing
;;; compiler.
;
(defun %defclass (name direct-supers direct-slots &rest initargs)
  (compile-notice 'defclass name)
  (when *environment*
    (apply #'si::%defclass name direct-supers direct-slots
        :environment *environment*
        initargs )) )


;;;; %defconstant
;;;
;;; BUGBUG: NYI: check redefinition: special=>constant, constant=>constant
;
(defun %defconstant (name value)
  (compile-notice 'defconstant name value)
  (when *environment*
    (let ((table (si::.env-variables *environment*)))
      (setf (gethash name table) `(:constant (:constant . ,value))) )) )


;;;; %defmacro
;;;
;;; BUGBUG: NYI: check redefinition.
;
(defun %defmacro (name lambda-list expander)
  (compile-notice 'defmacro name lambda-list)
  (when *environment*
    (setf (macro-function name *environment*) expander) ) )


;;;; %defparameter
;;;
;;; BUGBUG: NYI: constant => special
;
(defun %defparameter (name value)
  (compile-notice 'defparameter name value)
  (when *environment*
    (let ((table (si::.env-variables *environment*)))
      (setf (gethash name table) `(:special (:special . ,t))) )) )


;;;; %defsetf
;
(defun %defsetf (name lambda-list expander)
  (compile-notice 'defsetf name lambda-list)
  (when *environment*
    (let ((table (si::.env-writers *environment*)))
      (setf (gethash name table) `(:macro (:macro . ,expander))) ) ) )


;;;; %defstruct
;
(defun %defstruct (class-name direct-superclass-name direct-slot-specs
                   &rest initargs )
  (compile-notice 'defstruct class-name)
  (when *environment*
    (apply #'si::%defstruct class-name direct-superclass-name direct-slot-specs
        :environment *environment* initargs )) )


;;;; %deftype
;
(defun %deftype (name lambda-list expander)
  (compile-notice 'deftype name lambda-list)
  (when *environment*
    (si::install-type-macro name expander *environment*) ) )


;;;; %defun
;;;
;;; BUGBUG: NYI: check redefinition.
;
(defun %defun (name lambda-list)
  (compile-notice 'defun name lambda-list) )


;;;; %defvar
;;;
;;; BUGBUG: NYI: Check constant => special
;
(defun %defvar (name value)
    (declare (ignore value))
  (compile-notice 'defvar name)
  (when *environment*
    (let ((table (si::.env-variables *environment*)))
      (setf (gethash name table) `(:special (:special . ,t))) ))
  name )
