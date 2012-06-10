;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 14 Conses
;;; macro/m14-cons.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m14-cons.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     funcall/key     internal
;;;     getf            14.2.41
;;;     pop             14.2.20
;;;     push            14.2.19
;;;     pushnew         14.2.45
;;;     remf            14.2.44
;;;
;
(in-package :xc)


;;;; Optimize :key argument
;
(defmacro si::funcall/key (key-form arg-form)
  (let ((var-key '#:|key|)
        (var-arg '#:|obj|) )
    `(let ((,var-key ,key-form)
           (,var-arg ,arg-form) )
       (if (null ,var-key)
           ,var-arg
         (funcall ,var-key ,var-arg) ) ) ) )

;;;;  14.2.41 getf
;
(define-setf-expander cl:getf (place indicator-form &optional default-form
                               &environment env )
  (multiple-value-bind (vars vals stores writer-form reader-form)
      (get-setf-expansion place env)
    (let ((var-newval    (gensym "newval"))
          (var-indicator (gensym "indicator_"))
          (var-default   (if default-form (gensym "default_") nil)) )
      (values `(,@vars ,var-indicator ,@(if var-default `(,var-default)))
              `(,@vals ,indicator-form ,@(if var-default `(,default-form)))
              `(,var-newval)
              `(let ((,(car stores)
                      (si::plist-put ,reader-form
                                     ,var-indicator
                                     ,var-newval ) ))
                 ,writer-form
                 ,var-newval )
              `(getf ,reader-form ,var-indicator
                     ,@(if var-default `(,var-default)) )) ) ) )


;;;; 14.2.20 pop
;;;
;;; Syntax:
;;;     pop place => element
;;;
;;; BUGBUG: NYI: pop generaized-variable
;;;
;;; Expansion:
;;;  (let ((#:pop (car ,place)))
;;;    (setq ,place (cdr ,place))
;;;    #:pop )
;
(defmacro cl:pop (place &environment env)
  (multiple-value-bind (vars vals stores writer-form reader-form)
      (get-setf-expansion place env)
    (let ((var-car  (gensym "pop_"))
          (var-cons (first stores)) )
      `(let* (,.(mapcar #'list vars vals)
              (,var-cons ,reader-form)
              ,@(rest stores)
              (,var-car (car ,var-cons)) )
         (setq ,var-cons (cdr ,var-cons))
         ,writer-form
         ,var-car ) ) ) )


;;;; 14.2.19 push
;;;
;;; Syntax:
;;;     push item place => new-place-value
;;;
;;; Expansion:
;;;     (let* ((#:item item)
;;;            (var-1 val-1) ...
;;;            (store-1 (cons #:item reader-form))
;;;            store-2 ... )
;;;       writer-form )
;
(defmacro cl:push (item place &environment env)
  (multiple-value-bind (vars vals stores writer-form reader-form)
      (get-setf-expansion place env)
    (let ((var-item (gensym "item_")))
      `(let* ((,var-item ,item)
              ,.(mapcar #'list vars vals)
              (,(first stores) (cons ,var-item ,reader-form))
              ,@(rest stores) )
         ,writer-form ) ) ) )


;;;; 14.2.45 pushnew
;;;
;;; Syntax:
;;;     pushnew item place &key key test test-not
;;;        =>new-place-value
;;;
;;; Expansion:
;;;     (setf place (adjoin item place test test-not key))
;
(defmacro cl:pushnew (item place &rest args &environment env)
  (multiple-value-bind (vars vals stores writer-form reader-form)
      (get-setf-expansion place env)
    (let ((var-item (gensym "item_")))
      `(let* ((,var-item ,item)
              ,.(mapcar #'list vars vals)
              (,(first stores) (adjoin ,var-item ,reader-form ,@args))
              ,@(rest stores) )
         ,writer-form ) ) ) )


;;;; 14.2.42 remf
;;;
;;; Syntax:
;;;     place indicator => boolean
;;;
;;; Expandsion:
;;;     (let* ((var-1 val-1) ...
;;;            (#:plist reader-form)
;;;            #:removed-p
;;;            store-1 store-2 ... )
;;;        (multiple-value-setq (store-1 #:removed-p)
;;;            (plist-rem #:plist indicator) )
;;;        writer-form
;;;        #:removed-p )
;
(defmacro remf (place indicator &environment env)
  (multiple-value-bind (vars vals stores writer-form reader-form)
      (get-setf-expansion place env)
    (let ((var-plist     (gensym "plist_"))
          (var-removed-p (gensym "removed_")) )
      `(let* (,.(mapcar #'list vars vals)
              (,var-plist ,reader-form)
              ,var-removed-p
              ,@stores )
         (multiple-value-setq (,(first stores) ,var-removed-p)
            (si::plist-rem ,var-plist ,indicator) )
         ,writer-form
         ,var-removed-p ) ) ) )
