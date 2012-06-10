;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 5 Data and Control Flow
;;; macro/m05-place.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m05-place.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following places:
;;;     apply
;;;     the     3.8.28
;;;     values  5.3.53
;
(in-package :xc)

;;;; 5.1.2.5 Apply Form as Places
;;;
;;; Note:
;;;     (setf (apply #'name arg*) val) == (apply #'(setf name) val arg*)
;
(define-setf-expander cl:apply ((&whole operator function name) &rest arg*)
  (unless (or (eq 'function function) (eq 'quote function))
    (syntax-error '(quote name) operator) )
  (let ((vars   (make-vars arg*))
        (stores (list (gensym))) )
    (values vars
            arg*
            stores
            `(apply #'(setf ,name) ,@stores ,@vars)
            `(apply #',name ,@vars) ) ) )


;;;; 5.1.2.4 The Forms as Places
;;;
;;; Example:
;;;     (setf (the interger (cadr x)) (+ y 3))
;;;     ==> (setf (cadr x) (the interget (+ y 3)))
;
(define-setf-expander cl:the (value-type place &environment env)
  (multiple-value-bind (vars vals stores writer-form reader-form)
      (get-setf-expansion place env)
    (values vars
            vals
            stores
            (subst `(the ,value-type ,(first stores))
                   (first stores)
                   writer-form )
            `(the ,value-type ,reader-form) ) ) )


;;;; 5.1.2.3 Values Form as Places
;;; Note:
;;;  vars and vals returned by get-setf-expansion may be shared.
;
(define-setf-expander cl:values (&rest place* &environment env)
  (let ((vars*        '())
        (vals*        '())
        (stores*      '())
        (writer-form* '())
        (reader-form* '()) )
    (dolist (place place*)
      (multiple-value-bind (vars vals stores writer-form reader-form)
          (get-setf-expansion place env)
        (setq vars*   (nreconc (copy-list vars) vars*))
        (setq vals*   (nreconc (copy-list vals) vals*))
        (setq stores* (nreconc stores           stores*))

        (push writer-form writer-form*)
        (push reader-form reader-form*) ) )

    (setq vars*        (nreverse vars*))
    (setq vals*        (nreverse vals*))
    (setq stores*      (nreverse stores*))
    (setq writer-form* (nreverse writer-form*))
    (setq reader-form* (nreverse reader-form*))

    (values vars*
            vals*
            stores*
            `(progn ,@writer-form* (values ,@stores*))
            `(values ,@reader-form*) ) ) )
