;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 4 Types and Classes
;;; macro/m04-type.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m04-type.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     deftype
;
(in-package :xc)


;;;; parse-type-macro
;
(defun parse-type-macro (name lambda-list body)
    (declare (values list (or string null)))
  (let ((var-form        '#:|form|)
        (var-env         '#:|env|)
        (fn-syntax-error '#:|syntax-error|) )
    (multiple-value-bind (decl* form* doc-string) (analyze-body body t)
    (multiple-value-bind (program var-env-user)
        (parse-destructuring-bind lambda-list
                                  var-form
                                  `(and (consp ,var-form) (rest ,var-form))
                                  (append decl* form*)
                                  t
                                  fn-syntax-error
                                  ''* )
     (values
          `#'(lambda (,var-form ,(or var-env-user var-env))
                (declare (ext:lambda-name (deftype ,name)))
              ,@(unless var-env-user `((declare (ignore var-env))))
              ,@(when doc-string (list doc-string))
              (labels (
                  (,fn-syntax-error (cur src pat)
                      (syntax-error '(,name ,@lambda-list)
                                    ,var-form
                                    cur
                                    src
                                    pat ) ) )
                  (block ,name ,program) ) )
          doc-string ) ) ) ) )


;;;; 4.4.25 deftype
;;;
;;; Syntax:
;;;   deftype name lambda-list [[{declaration}* | documentation]] {form}*
;;;     => name
;
(defmacro cl:deftype (name lambda-list &body body)
  (multiple-value-bind (expander doc-string)
      (parse-type-macro name lambda-list body)
    `(progn
       (eval-when (:compile-toplevel)
         (%deftype ',name ',lambda-list ,expander) )
       (si::%deftype ',name ',lambda-list ,expander ,doc-string) ) ) )
