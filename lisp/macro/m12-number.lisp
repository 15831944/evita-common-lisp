;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 12 Numbers
;;; macro/m12-control.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m12-number.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     decf        12.2.33
;;;     incf        12.2.33
;;;     ldb         12.2.68
;;;     mask-field  12.2.71
;
(in-package :xc)

;;;; 12.2.33 incf, decf Macro 
;;;
;;; Syntax:
;;;     incf place [delta-form]    new-value
;;;     decf place [delta-form]    new-value
;
(define-modify-macro cl:decf (&optional (delta 1)) -)
(define-modify-macro cl:incf (&optional (delta 1)) +)


;;;; 12.2.68 ldb
;
(define-setf-expander cl:ldb (bytespec place &environment env)
  (multiple-value-bind (vars vals stores writer-form reader-form)
      (get-setf-expansion place env)
    (let ((btemp (gensym))
          (store (gensym))
          (stemp (first stores)) )
      (values (cons btemp vars)
              (cons bytespec vals)
              (list store)
              `(let ((,stemp (dpb ,store ,btemp ,reader-form)))
                 ,writer-form
                 ,store )
              `(ldb ,btemp ,reader-form) ) ) ) )


;;;; 12.2.71 mask-field
;
(define-setf-expander cl:mask-field (bytespec place &environment env)
  (multiple-value-bind (vars vals stores writer-form reader-form)
      (get-setf-expansion place env)
    (let ((btemp (gensym))
          (store (gensym))
          (stemp (first stores)) )
      (values (cons btemp vars)
              (cons bytespec vals)
              (list store)
              `(let ((,stemp (deposit-field ,store ,btemp ,reader-form)))
                 ,writer-form
                 ,store )
              `(mask-field ,btemp ,reader-form) ) ) ) )
