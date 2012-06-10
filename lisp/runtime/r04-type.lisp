;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 4 Types and Classes
;;; runtime/r04-type.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r04-type.lisp#4 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;     coerce      4.4.24      -- See r04-coerce.lisp
;;;     subtypep    4.4.26
;;;     type-of     4.4.27
;;;     typep       4.4.28
;
(in-package :si)

;;;; 4.4.26 subtypep
;
(defun cl:subtypep (type-1 type-2 &optional env)
    (declare (type ext:type-specifier type-1))
    (declare (type ext:type-specifier type-2))
    (declare (type t env))
    (declare (values boolean boolean))
  (let ((tyo-1 (parse-value-type type-1 env))
        (tyo-2 (parse-value-type type-2 env)) )
    (tyo-subtypep tyo-1 tyo-2) ) )


;;;; 4.4.27 type-of
;;;
;;; Note: We return class-name instead of class object even if class object
;;; is valid type specifier for cosmetic reason.
;;;
;
(defun cl:type-of (object)
    (declare (type t object))
    (declare (values ext:type-specifier))
  (let* ((class (class-of object))
         (name  (.class-name class)) )
    ;; BUGBUG: NYI: specialized vector
    ;; BUGBUG: NYI: specialized complex
    name ) )


;;;; 4.4.28 typep
;
(defun cl:typep (object type &optional env)
    (declare (type t object))
    (declare (type ext:type-specifier type))
    (declare (type t env))
    (declare (values t))
  (let ((tyo (parse-type type env)))
    (when (tyo-typep tyo (find-class 'function-type))
      (error "Can't use list form function type: ~S" type) )
    (tyo-typep object tyo) ) )
