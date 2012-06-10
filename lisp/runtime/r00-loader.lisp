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
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r00-loader.lisp#5 $
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
(defun %define-compiler-macro (name lambda-list expander doc-string)
    (declare (ignore lambda-list))
  (setf (compiler-macro-function name) expander)
  (setf (documentation name 'compiler-macro) (or doc-string ""))
  name )


;;;; %define-setf-expander
;;;
;;; See Also: %defsetf
(defun %define-setf-expander (name lambda-list expander)
    #+nil (declare (type symbol name))
    #+nil (declare (type list lambda-list))
    #+nil (declare (type function expander))
    (declare (ignore lambda-list))
  (let ((table (.env-writers *environment*)))
    (setf (gethash name table) `(:macro (:macro . ,expander))) )
  name )


;;;; %define-symbol-macro
(defun %define-symbol-macro (name expansion)
  (let ((kind (xc::variable-information name nil)))
    (when (or (eq ':special kind) (eq ':constant kind))
      (error 'symbol-macro-clobbers-variable :name name) ) )
  (let ((table (.env-variables *environment*)))
    (setf (gethash name table)
      `(:symbol-macro (:symbol-macro . ,expansion)) ) )
  name )


;;;; %defconstant
;;; Note: intern-value-cell isn't ready during genesis.
(defun %defconstant (name value doc-string)
  ;; update environment
  (let ((table (.env-variables *environment*)))
    (setf (gethash/eq name table) `(:constant (:constant . ,value))) )
  (setf (documentation name 'variable) (or doc-string ""))
  ;; update cell
  (let ((cell (gethash/eq name *value-table*)))
    (unless cell
      (setq cell (.allocate-record #.(class-description 'value-cell)))
      (setf (.value-cell-name  cell) name)
      (setf (gethash/eq name *value-table*) cell) )
    (setf (.value-cell-value cell) value)
    (setf (.value-cell-type  cell) :constant)
    name ) )


;;;; %defmacro
(defun %defmacro (name lambda-list expander doc-string)
    (declare (ignore lambda-list))
  (setf (macro-function name) expander)
  (setf (documentation name 'function) (or doc-string ""))
  name )


;;;; %defparameter
(defun %defparameter (name value doc-string)
  (let ((table (.env-variables *environment*)))
    (setf (gethash name table) `(:special (:special . t))) )
  (setf (documentation name 'variable) (or doc-string ""))
  (setf (symbol-value name) value)
  name )


;;;; %defsetf
(defun %defsetf (name lambda-list expander doc-string)
    (declare (ignore lambda-list))
  (let ((table (.env-writers *environment*)))
    (setf (gethash name table) `(:macro (:macro . ,expander))) )
  (setf (documentation `(setf ,name) 'setf) (or doc-string ""))
  name )


;;;; %deftype
;;;
;;; Note: This function is called at compile, load and execution time.
(defun %deftype (name lambda-list expander doc-string)
    (declare (ignore lambda-list))
  (install-type-macro name expander)
  (setf (documentation name 'type) (or doc-string ""))
  name )


;;;; %defun
(defun %defun (name lambda-list fn)
    (declare (ignore lambda-list))
  (when (check-redefinition name fn)
    (setf (fdefinition name) fn) )
  name )


;;;; %defvar
(defun %defvar (name value doc-string init-p)
  (let ((table (.env-variables *environment*)))
    (setf (gethash name table) `(:special (:special . t))) )
  (setf (documentation name 'variable) (or doc-string ""))
  (when init-p
    (setf (symbol-value name) value) )
  name )


;;;; %in-package
;;;
(defun %in-package (name)
  (setq name (if (symbolp name) (symbol-name name) name))
  (dolist (package *all-packages*)
    (dolist (package-name (.package-names package))
      (when (= 0 (string-cs-compare name package-name 0 nil 0 nil))
        (setq *package* package)
        (return-from %in-package package) ) ) ) )

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
(defun cl:proclaim (decl)
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
              (setf (gethash name (.env-variables *environment*))
                    (cons :special (acons :special t alist)) ))) ) ) ))
  nil )
