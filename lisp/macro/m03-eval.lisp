;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 3 Evaluation and Compilation
;;; macro/m03-eval.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m03-eval.lisp#2 $
;;;
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;
;;; Public Macros:
;;;     declaim                     3.8.17
;;;     define-compiler-macro       3.8.8
;;;     define-symbol-macro         3.8.13
;;;     defmacro                    3.8.10
;;;     lambda                      3.8.2
;
(in-package :xc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Macros
;;;;

;;;; 3.8.17 declaim
;;;
;;; Syntax:
;;;     declaim {declaration-specifier}* => implementation-dependent
;
(defmacro cl:declaim (&rest spec*)
  (let ((form* '()))
    (dolist (spec spec*)
      (push `(proclaim ',spec) form*) )
    (if (null form*)
        nil
        `(eval-when (:compile-toplevel :load-toplevel :execute)
          ,@(nreverse form*) ) ) ) )


;;;; 3.8.8 define-compiler-macro
;;; See "3.2.1.1 Purpose of Compiler Macros".
;;; Note:
;;;  Input form can be one of:
;;;     (name ...)
;;;     (funcall (function name) ...)
;
(defmacro cl:define-compiler-macro (name lambda-list &body body)
  (unless (si::function-name-p name)
    (error 'syntax-error
           :syntax 'function-name
           :form   name ))
  (let ((var-form        '#:|form|)
        (temp-env        '#:|env|)
        (fn-syntax-error '#:|syntax-error|)
        (block-name      (if (consp name) (second name) name))
        expander-form )
    (multiple-value-bind (decl* form* doc-string) (analyze-body body t)
    (multiple-value-bind (program var-env)
        (parse-destructuring-bind
            lambda-list
            var-form
            `(if (and (eq 'funcall (first ,var-form))
                      (consp (rest ,var-form))
                      (let ((function-form (second ,var-form)))
                        (and (consp function-form)
                             (eq 'function (first function-form))
                             (consp (rest function-form))
                             (equal ',name (second function-form))
                             (null (rest (rest function-form))) ) ))
                 (cddr ,var-form)
               (cdr ,var-form) )
            (append decl* form*)
            t
            fn-syntax-error )
      (setq expander-form
        `#'(lambda (,var-form ,(or var-env temp-env))
                (declare (ext:lambda-name (compiler-macro-function ,name)))
             ,@(unless var-env `((declare (ignore ,temp-env))))
             ,@(when doc-string (list doc-string))
             (labels (
                 (,fn-syntax-error (cur src pat)
                    (syntax-error '(,name ,@lambda-list)
                                   ,var-form
                                   cur
                                   src
                                   pat ) )
                )
                (block ,block-name ,program) ) ))

      `(progn
          (eval-when (:compile-toplevel)
            (%define-compiler-macro ',name ',lambda-list ,expander-form) )
          (si::%define-compiler-macro
             ',name
             ',lambda-list
             ,expander-form
             ,doc-string )) ) ) ) )


;;;; 3.8.13 define-symbol-macro
;;;
;;; Syntax:
;;;     define-symbol-macro symbol expansion
;
(defmacro cl:define-symbol-macro (symbol expansion)
  `(progn
     (eval-when (:compile-toplevel)
        (%define-symbol-macro ',symbol ',expansion) )
     (si::%define-symbol-macro ',symbol ',expansion) ) )


;;;; 3.8.10 defmacro
;;;
;;; Syntax:
;;;   defmacro name lambda-list body => name
;
(defmacro cl:defmacro (name lambda-list &body body)
  (multiple-value-bind (expander doc-string)
      (parse-macro-aux name lambda-list body)
   `(progn
      (eval-when (:compile-toplevel)
        (%defmacro ',name ',lambda-list ,expander) )
      (si::%defmacro ',name ',lambda-list ,expander ,doc-string) ) ) )


;;;; 3.8.2 lambda
;;;
;;; Syntax:
;;;     lambda lambda-list [[{declaration}* | documentation]] {form}*
;;;         => function
;
(defmacro cl:lambda (&whole form lambda-list &body body)
  (declare (ignorable lambda-list body))
  `#',form )
