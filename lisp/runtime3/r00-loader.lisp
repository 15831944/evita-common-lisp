;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - Macro Loaders
;;; runtime/r00-loader.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime3/r00-loader.lisp#3 $
;;;
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     %define-compiler-macro
;;;     %define-setf-expander
;;;     %define-symbol-macro
;;;     %defmacro
;;;     %defsetf
;;;     %deftype
;;;     %defun
;;;     %defvar
;;;     %in-package
;;;     (setf documentation)
;;;     proclaim
;
(in-package :si)

;;;; %define-compiler-macro
;
(defun %define-compiler-macro (name lambda-list expander doc-string)
    (declare (ignore lambda-list))
  (setf (compiler-macro-function name) expander)
  (setf (documentation name 'compiler-macro) (or doc-string ""))
  name )


;;;; %define-setf-expander
;;;
;;; See Also: %defsetf
;
(defun %define-setf-expander (name lambda-list expander)
    (declare (ignore lambda-list))
  (let ((htb (ref environment functions *environment*))
        (key (intern-setf-cell name)) )
    (setf (gethash/eq key htb) `(:macro (:macro . ,expander))) )
  name )


;;;; %define-symbol-macro
;
(defun %define-symbol-macro (name expansion)
  (let ((kind (xc::variable-information name nil)))
    (when (or (eq ':special kind) (eq ':constant kind))
      (error 'symbol-macro-clobbers-variable :name name) ) )
  (let ((htb (ref environment variables *environment*)))
    (setf (gethash/eq name htb)
      `(:symbol-macro (:symbol-macro . ,expansion)) ) )
  name )


;;;; %defconstant
;
(defun %defconstant (name value doc-string)
  (labels (
    (update-env ()
      (let ((table (ref environment variables *environment*)))
        (setf (gethash/eq name table) `(:constant (:constant . ,value))) ) )

    (update-cell ()
      (let ((cell (gethash/eq name *value-table*)))
        (unless cell
          (setq cell (.allocate-record #.(class-description 'value-cell)))
          (setf (ref value-cell name  cell) name)
          (setf (gethash/eq name *value-table*) cell) )
        (setf (ref value-cell value cell) value)
        (setf (ref value-cell type  cell) :constant) ) )
    )
    ;;
    (update-env)
    (setf (documentation name 'variable) (or doc-string ""))
    (update-cell)
   name ) )


;;;; %defmacro
;
(defun %defmacro (name lambda-list expander doc-string)
    (declare (ignore lambda-list))
  (setf (macro-function name) expander)
  (setf (documentation name 'function) (or doc-string ""))
  name )


;;;; %defparameter
;
(defun %defparameter (name value doc-string)
  (let ((table (ref environment variables *environment*)))
    (setf (gethash/eq name table) `(:special (:special . t))) )
  (setf (documentation name 'variable) (or doc-string ""))
  (setf (symbol-value name) value)
  name )


;;;; %defsetf
;
(defun %defsetf (name lambda-list expander doc-string)
    (declare (ignore lambda-list))
  (let ((table (ref environment functions *environment*))
        (key   (intern-setf-cell name)) )
    (setf (gethash/eq key table) `(:macro (:macro . ,expander))) )
  (setf (documentation `(setf ,name) 'setf) (or doc-string ""))
  name )


;;;; %deftype
;;;
;;; Note: This function is called at compile, load and execution time.
;
(defun %deftype (name lambda-list expander doc-string)
    (declare (ignore lambda-list))
  (setf (find-type name) expander)
  (setf (documentation name 'type) (or doc-string ""))
  name )


;;;; %defun
;
(defun %defun (name lambda-list function)
    (declare (ignore lambda-list))
  (setf (fdefinition name) function)
  name )


;;;; %defvar
;
(defun %defvar (name value doc-string init-p)
  (let ((table (ref environment variables *environment*)))
    (setf (gethash/eq name table) `(:special (:special . t))) )
  (setf (documentation name 'variable) (or doc-string ""))
  (when init-p
    (setf (symbol-value name) value) )
  name )


;;;; %in-package
;;;
;
(defun %in-package (name)
  (let ((package (find-package name)))
    (unless (packagep package)
      (error "No such package called ~S." name) )
    (setq *package* package) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;

;;;; 3.8.16 proclaim
;;;
;;; Syntax:
;;;     proclaim declaration-specifier => implementation-dependent
;;;
;;; BUGBUG: NYI: proclaim
;
(defun cl:proclaim (decl)
  ;(format t "; proclaim ~S~%" decl)
  (case (first decl)
    ((special)
      (dolist (name (rest decl))
        (multiple-value-bind (kind local-p alist)
            (xc::variable-information name nil)
          (unless local-p
            (when (and kind (not (eq :special kind)))
              (format t "; Warning: ~A ~S becomes special variable."
                      kind
                      name ))
            (unless (eq :special kind)
              (setf (gethash/eq name (ref environment variables *environment*))
                    (cons :special (acons :special t alist)) ))) ) ) ))
  nil )
