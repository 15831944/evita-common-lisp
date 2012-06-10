;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - devel - 24 Systm Construction - Compile File
;;; lisp/devel3/d00-loader.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel3/d24-compile-file.lisp#1 $
;;;
;;; Description:
;;;  This file constains implemenation of
;;;     compile-file
;;;     compile-file-pathname
;
(in-package :si)

(defun c::compile-notice (op name &optional lambda-list)
    (declare (ignore lambda-list))
  (when *compile-print*
    (format t "; ~S ~S~%" op name) )
  nil )

(defun c::%define-compiler-macro (name lambda-list expander)
    (declare (ignore lambda-list))
  (c::compile-notice 'define-compile-macro name)
  (setf (compiler-macro-function name) expander)
  name )


(defun c::%define-setf-expander (name lambda-list expander)
    (declare (ignore lambda-list))
  (c::compile-notice 'define-setf-expander name)
  (let ((env *environment*))
    (with-latch ((ref environment latch env) :exclusive)
      (setf (fun-info name :macro env) expander) ) )
   name )

(defun c::%define-symbol-macro (name expansion)
  (c::compile-notice 'define-symbol-macro name)
  (let ((kind (c::variable-information name)))
    (when (or (eq kind :special) (eq kind :constant))
      (error "Can't redefine ~A variable ~S as symbol-macro." kind name) ) )
  (let ((env *environment*))
    (with-latch ((ref environment latch env) :exclusive)
      (setf (var-info name :symbol-macro env) expansion) ) )
  name )

(defun c::%defclass (name supers slots &rest initargs)
  (c::compile-notice 'defclass name)
  (apply #'%defclass name supers slots initargs) )

(defun c::%defconstant (name value)
  (c::compile-notice 'defconstant name)
  (let ((env *environment*))
    (with-latch ((ref environment latch env) :exclusive)
      (setf (var-info name :constant env) value) ) )
  name )

(defun c::%defmacro (name lambda-list expander)
    (declare (ignore lambda-list))
  (c::compile-notice 'defmacro name)
  (setf (macro-function name) expander)
  name )


(defun c::%defsetf (name lambda-list expander)
    (declare (ignore lambda-list))
  (c::compile-notice 'defsetf name)
  (let ((env *environment*))
    (with-latch ((ref environment latch env) :exclusive)
      (setf (fun-info name :macro env) expander) ) )
  name )

(defun c::%defstruct (name super slots &rest initargs)
  (c::compile-notice 'defstruct name)
  (apply #'%defstruct name super slots initargs) )


(defun c::%deftlv (name initform)
  (c::compile-notice 'defvar name)
  (%deftlv name nil nil nil)
  #+nil
  (let ((env *environment*))
    (with-latch ((ref environment latch env) :exclusive)
      (setf (var-info name :special env) t)
      (setf (var-info name 'tlv env) tlvrec) ) )
  name )

(defun c::%deftype (name lambda-list expander)
    (declare (ignore lambda-list))
  (c::compile-notice 'deftype name)
  (let ((env *environment*))
    (with-latch ((ref environment latch env) :exclusive)
      (setf (gethash name (ref environment types env)) expander) ) )
  name )

(defun c::%defun (name lambda-list)
    (declare (ignore lambda-list))
  (c::compile-notice 'defun name)
  name )

(defun c::%defvar (name value)
    (declare (ignore value))
  (c::compile-notice 'defvar name)
  (let ((env *environment*))
    (with-latch ((ref environment latch env) :exclusive)
      (setf (var-info name :special env) t) ) )
  name )
