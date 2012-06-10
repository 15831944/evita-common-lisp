;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 9 Conditions
;;; macro/m09-condition.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m09-condition.lisp#2 $
;;;
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     assert                      9.2.10
;;;     check-type                  9.2.13
;;;     define-condition            9.2.29
;;;     handler-bind                9.2.26
;;;     handler-case                9.2.27
;;;     ignore-errors               9.2.28
;;;     restart-bind                9.2.36
;;;     restart-case                9.2.37
;;;     with-condition-restarts     9.2.39
;;;     with-simple-restart         9.2.40
;
(in-package :xc)

;;;; 9.2.10 assert
;;;
;;; Syntax:
;;;     assert test-form [({place}*) [datum-form {arg-form}*]]
;;;         => nil
;
(defmacro cl:assert (test-form &optional place* datum-form &rest arg-form*)
  `(loop
     (when ,test-form (return))
     (si::%assert ',test-form ',place* ,datum-form ,arg-form*)
     ,.(mapcar #'(lambda (place) `(setf ,place (si::assert-prompt ',place)))
               place* )) )


;;;; 9.2.13 check-type
;
(defmacro cl:check-type (place type &optional string)
    (declare (ignore string))
  (if (eq t type)
      (make-ignorable-form nil)
    `(loop
       (when (typep ,place ',type) (return))
       (setf ,place (si::%check-type ,place ',type)) )) )


;;;; 9.2.29 define-condition
;;;
;;; BUGBUG: NYI: stab implementation. We'll use defclass w/ condition-class
;
(defmacro cl:define-condition (name (&rest superclass*) (&rest slot-spec*)
                               &rest option* )

  (when (null superclass*)
    (setq superclass* '(condition)) )

  (let ((report nil))
    (loop for option in option* do
      (when (and (eql (si::safe-list-length option) 2)
                 (eq :report (first option)) )
        (setq report (second option))
        (when (symbolp report) (setq report `#',report))
        (setq option* (delete option option* :test #'eq)) ))

    `(progn
      (defclass ,name ,superclass* ,slot-spec* ,@option*)
      ,@(and report 
            `((defmethod print-object ((c ,name) s)
                 (if *print-escape*
                     (call-next-method)
                   (funcall ,report c s) ))))
       ',name ) ) )


;;;; 9.2.26 handler-bind
;;;
;;; Syntax:
;;;   handler-binding binding* body
;;;   binding ::= (typespec handler)
;
(defmacro cl:handler-bind (binding* &body body)
  (dolist (binding binding*)
    (unless (eql (si::safe-list-length binding) 2)
      (macro-error "Malformed binding: ~S" binding) ) )
  `(let ((si::*handler-clusters*
           (cons (list ,.(mapcar #'(lambda (x) `(cons ',(car x) ,(cadr x)))
                                 binding* ))
                 si::*handler-clusters* )) )
        (declare (dynamic-extent si::*handler-clusters*))
     ,@body ) )


;;;; 9.2.27 handler-case
;
(defmacro cl:handler-case (form &rest clause*)
  (labels (
    ;; expand-with-no-error
    (expand-with-no-error (clauses no-error)
      (let ((blk-error  (gensym "error_"))
            (blk-normal (gensym "normal_")) )
        `(block ,blk-error
           (multiple-value-call
             #'(lambda ,(second no-error)
                 (declare (ext:lambda-name (handler-case :no-error)))
                 ,@(cddr no-error) )
             (block ,blk-normal
               (return-from ,blk-error
                  (handler-case (return-from ,blk-normal ,form)
                    ,@clauses ))))) ) )

    ;; expand-without-no-error
    (expand-without-no-error (clauses)
      (loop
        with blk-normal = (gensym "normal_")
        with var-cond   = (gensym "cond_")
        with binding*   = '()
        with statement* = '()
        for clause in clauses
        for tag-handler = (gensym) do
          (destructuring-bind (typespec (&optional var) &body body) clause
            (push `(,typespec
                      #'(lambda (temp)
                          (declare (ext:lambda-name (handler-case ,typespec)))
                          (setq ,var-cond temp)
                          (go ,tag-handler) ))
                  binding* )
            (push `(return-from ,blk-normal
                     (let ,(when var `((,var ,var-cond)))
                       ,@body ))
                  statement* )
            (push tag-handler statement*) )
        finally
          (setq binding* (nreverse binding*))
          (return
            `(block ,blk-normal
               (let (,var-cond)
                   (declare (ignorable ,var-cond))
                 (tagbody
                   (handler-bind ,binding* (return-from ,blk-normal ,form))
                   ,@statement* ) )))) )
    )
    ;;
    (loop
      with no-error = nil
      for clause in clause*
        unless (>= (si::safe-list-length clause) 2)
          do (macro-error "Invalid ~S clause: ~S" 'handler-case clause)
        if (not (eq (first clause) :no-error))
          collect clause into clauses
        else if (null no-error)
          do (setq no-error clause)
        else
          do (macro-error "~S clause must be one." :no-error)
      finally
        (if no-error
            (return (expand-with-no-error clauses no-error))
          (return (expand-without-no-error clauses)) )) ) )


;;;; 9.2.28 ignore-erros
;
(defmacro cl:ignore-errors (&body form*)
  (let ((var-cond  '#:cond)
        (var-value '#:value) )
    `(handler-case (progn ,@form*)
        (error (,var-cond)
          (let ((,var-value nil))
            (values ,var-value ,var-cond) ) ) ) ) )


;;;; 9.2.36 restart-bind
;;;
;;; Syntax:
;;;     restart-bind ({(name function {key-val-pair}*)}*) decl* form*
;;;       => result*
;;;
;;;     key-val-pair ::= :interactive-function interactive-function
;;;                      :report-function      report-function
;;;                      :test-function        test-function
;
(defmacro cl:restart-bind (binding* &body body)
  (let ((restarts
          (mapcar #'(lambda (binding)
                      (destructuring-bind (name function &rest key*)
                          binding
                         `(si::make-restart
                                :name     ',name
                                :function ,function
                                ,@key* ) ) )
                          binding* )) )
    `(let ((si::*restart-clusters*
             (cons (cons nil (list ,@restarts)) si::*restart-clusters*) ))
          (declare (dynamic-extent si::*restart-clusters*))
       ,@body ) ) )


;;;; 9.2.37 restart-case
;;;
;;; Syntax:
;;;     restart-case restartable-form clause* => result*
;;;     clause ::= (case-name lambda-list
;;;                 :interactive interactive-expression
;;;                 :report      report-function
;;;                 :test        test-expression
;;;                 decl* form* )
;;;
;;; Note: If form is signal, error, cerror or warn, this form
;;; is exapnded into with-condition-restarts, otherwise it is expanded
;;; into restart-bind.
;
(defmacro cl:restart-case (restartable-form &rest clause*)
  (let ((blk-case         (gensym "restart-case_"))
        (var-params       (gensym "params_"))
        (restart-handler* '())
        (restart-binding* '())
        (restart-names    '()) )
  (labels (
    ;; hack-restartable-form
    (hack-restartable-form (form)
        (declare (values t t))
      (cond
        ((not (consp form))
          (values form nil) )
        ((eq (first form) 'cl:cerror)
          (values (hack-cerror-form form) nil) )
        (t
          (loop
            for (name . class) in '(
                    (cl:error  . simple-error)
                    (cl:signal . simple-condition)
                    (cl:warn   . simple-warn) )
             when (eq (first form) name) return
               (values (hack-error-form form class) (eq name 'error))
            finally
              (return (values form nil)) ) )) )

    ;; hack-cerror-form
    (hack-cerror-form (form)
      (destructuring-bind (cerror format-control datum &rest params)
          form (declare (ignore cerror))
        (let ((var-format (gensym "format"))
              (var-cond   (gensym "cond")) )
          `(let ((,var-format ,format-control)
                 (,var-cond
                   (si::coerce-to-condition
                      'cerror ,datum (list ,@params) 'simple-error ) ))
                (declare (type string ,var-format))
                (declare (type condition ,var-cond))
              (with-condition-restarts
                ,var-cond
                (list ,.(mapcar (lambda (name) `(find-restart ',name))
                                restart-names ))
                (cerror ,var-format ,var-cond) ) ) ) ) )

    ;; hack-error-form
    (hack-error-form (form class)
      (destructuring-bind (signal datum &rest params) form
        (let ((var-cond (gensym "cond")))
         `(let ((,var-cond
                  (si::coerce-to-condition
                      ',signal ,datum (list ,@params) ',class ) ))
              (declare (type condition ,var-cond))
            (with-condition-restarts
              ,var-cond
              (list ,.(mapcar #'(lambda (name) `(find-restart ',name))
                              restart-names ))
              (,signal ,var-cond) ) ) ) ) )
    )
    ;;
    (dolist (clause clause*)
      (destructuring-bind (case-name lambda-list &rest body) clause
        (unless (symbolp case-name)
          (macro-error "invalid restart-case name: ~S" case-name) )

        (let ((key-val* '()))

          ;; Make handler-bind/key-val-pair*
          ;;
          (loop
            (unless (and (keywordp (first body)) (consp (rest body)))
              (return) )
            (case (first body)
              ((:test)
                (pop body)
                (push :test-function key-val*)
                (push `#',(pop body) key-val*) )
              ((:interactive)
                (pop body)
                (push :interactive-function key-val*)
                (push `#',(pop body)        key-val*) )
              ((:report)
                (pop body)
                (push :report-function key-val*)
                (let ((report-fn (pop body)))
                  (when (stringp report-fn)
                    (setq report-fn
                      `(lambda (stream)
                          (declare
                            (ext:lambda-name
                              (restart-case ,case-name :report) ))
                          (write-string ,report-fn stream) ) ))
                  (push report-fn key-val*) ) )
              (otherwise (return)) ))
          (setq key-val* (nreverse key-val*))

          ;; Make restart-bind/binding*
          ;;
          (let ((tag-handler (gensym)))
            (push case-name restart-names)
            (push `(,case-name #'(lambda (&rest params)
                                   (declare (ext:lambda-name
                                      (restart-bind ,case-name) ))
                                   (setq ,var-params params)
                                   (go ,tag-handler) )
                               ,@key-val* )
                  restart-binding* )
            (push tag-handler restart-handler*)
            (push `(return-from ,blk-case
                     (apply #'(lambda ,lambda-list
                                (declare (ext:lambda-name
                                    (restart-case ,case-name) ))
                                ,@body )
                            ,var-params ) )
                  restart-handler* ) ) ) ) )

    (setq restart-binding* (nreverse restart-binding*))
    (setq restart-handler* (nreverse restart-handler*))
    (setq restart-names    (nreverse restart-names))

    ;; Check specail case of restartable-form
    ;;
    (multiple-value-bind (restartable-form unreachable)
        (hack-restartable-form restartable-form)

      ;; restart-case => restart-bind
      (let ((return-from
              (if unreachable
                  restartable-form
                `(return-from ,blk-case ,restartable-form) ) ))
       `(block ,blk-case
          (let (,var-params)
             (tagbody
               (restart-bind ,restart-binding* ,return-from)
             ,@restart-handler* ) )) ) ) ) ) )


;;;; 9.2.39 with-condition-restarts
;;;
;;; Note: this macro is used by restart-case, user program may not
;;; use this macro explicitly.
;;;
;;; BUGBUG: We should use si:make-restart-cluster function to validate
;;; condition and restart list.
;
(defmacro cl:with-condition-restarts (condition restarts &body body)
  `(let ((si::*restart-clusters*
          (cons (cons ,condition ,restarts)
                si::*restart-clusters* )) )
     ,@body ) )


;;;; 9.2.40 with-simple-restart
;
(defmacro cl:with-simple-restart ((name format-control &rest format-arg*)
                                  &rest form* )
  `(restart-case (progn ,@form*)
     (,name ()
       :report #'(lambda (stream)
                   (declare (ext:lambda-name (with-simple-restart :report)))
                   (format stream ,format-control ,@format-arg*) )
       (values nil t) ) ) )
