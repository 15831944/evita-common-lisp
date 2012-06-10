;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 6 Iteration
;;; macro/m06-do.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m06-do.lisp#4 $
;;;
;;; See Also:
;;;   macro/m06-loop.lisp
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     doplist
;;;
;;; Public Macros:
;;;     do                  6.2.1
;;;     do*                 6.2.1
;;;     dolist              6.2.2
;;;     si:dolist-named     internal
;;;     dotimes             6.2.3
;;;     si:dotimes-named    internal
;
(in-package :c)

#|
;;;; doplist
;;;
;;; Description:
;;;  Loops over property list.
;;;
;;; Note: defstruct and CLOS uses this macro.
;
(defmacro doplist ((key val plist &optional result) &body body)
  (let ((var-plist (gensym))
        (var-scan  (gensym))
        (var-key   (gensym))
        (var-val   (gensym)) )
    `(loop
       (let* ((,var-plist ,plist)
              (,var-scan  ,var-plist)
              ,var-key ,var-val )
          (when (endp ,var-scan) ,(xc::make-ignorable-form result))

          (unless (consp ,var-scan)
            (error 'invalid-property-list :list ,var-plist) )
          (setq ,var-key (pop ,var-scan))

          (unless (consp ,var-scan)
            (error 'invalid-property-list :list ,var-plist) )
          (setq ,var-val (pop ,var-scan))

          (let ((,key ,var-key)
             (,val ,var-val) )
           ,@body ) )) ) )
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Macros
;;;;

;;;; 6.2.1 do, do*
;;;
;;; Syntax:
;;      do binding* (test-form {result-form}*)
;;;       {declaration}* {tag|statement}* => {result}*
;;;
(labels (
  (expand-do (binding* test-form result-form* body op-let op-setq)
    (let ((let-binding* '())
          (setq-pair*   '())
          (tag-loop     (gensym "do-loop"))
          (tag-test     (gensym "do-test"))
          decl* form* )
        (declare (type list let-binding*))
        (declare (type list setq-pair*))

      (dolist (binding binding*)
        (if (and binding (symbolp binding))
            (push binding let-binding*)
          (destructuring-bind (var &optional init-form step-form)
              binding
            (push `(,var ,init-form) let-binding*)
            (if (null step-form)
                (push `(declare (ignorable ,var)) decl*)
              (progn
                (push var       setq-pair*)
                (push step-form setq-pair*) )) )) )

      (setq let-binding* (nreverse let-binding*))
      (setq setq-pair*   (nreverse setq-pair*))

      (multiple-value-setq (decl* form*) (analyze-body body nil))

      `(block nil
         (,op-let ,let-binding*
             ,@decl*
           (tagbody
             (go ,tag-test)
             ,tag-loop
             ,@form*
             (,op-setq ,@setq-pair*)
             ,tag-test
             ,(if test-form
                  `(if (not ,test-form) (go ,tag-loop))
                `(go ,tag-loop) ))
           ,@result-form* )) ) )
    )
    ;; body
    (defmacro cl:do (binding* (test-form &rest result-form*) &body body)
      (expand-do binding* test-form result-form* body 'let 'psetq) )

    (defmacro cl:do* (binding* (test-form &rest result-form*) &body body)
      (expand-do binding* test-form result-form* body 'let* 'setq) ) )


;;;; 6.2.3 dolist
(defmacro cl:dolist ((var list &optional (result nil result-p)) &body body)
  (let ((loop   '#:loop)
        (runner '#:runner) )
  (multiple-value-bind (decl* form*) (analyze-body body)
  (labels (
    (compute-return ()
      (if (not result-p)
          '(return)
        `(let ((,var nil))
              (declare (ignorable ,var)) ,@decl*
            (return ,result)) ) )
    )
    ;;
   `(block nil
      (let ((,runner ,list))
          (declare (type list ,runner))
        (tagbody
         ,loop
           (if (endp ,runner) ,(compute-return))
           (let ((,var (car ,runner)))
               (declare (ignorable ,var)) ,@decl*
             (tagbody ,@form*) )
           (setq ,runner (cdr ,runner))
           (go ,loop) ) ) ) ) ) ) )


;;;; 6.2.2 dotimes
(defmacro cl:dotimes ((var count &optional (result nil result-p)) &body body)
  (let ((loop '#:loop)
        (i    '#:i)
        (n    '#:n) )
  (multiple-value-bind (decl* form*) (analyze-body body)
  (labels (
    (compute-return ()
      (if (not result-p)
          '(return)
        `(let ((,var ,n))
              (declare (ignorable ,var)) ,@decl*
            (return ,result)) ) )
    )
    ;;
   `(block nil
      (let ((,i 0)
            (,n ,count) )
          (declare (type (integer 0 *) ,i))
          (declare (type integer ,n))
        (tagbody
         ,loop
           (if (>= ,i ,n) ,(compute-return))
           (let ((,var ,i))
               (declare (ignorable ,var)) ,@decl*
             (tagbody ,@form*) )
           (incf ,i)
           (go ,loop) ) ) ) ) ) ) )
