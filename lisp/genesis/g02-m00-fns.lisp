;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - genesis
;;; lisp/genesis/g01-m00-fns.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/genesis/g02-m00-fns.lisp#5 $
;;;
;;; Description:
;;;  This file contains function definitions for function defined in
;;;  "macro/m00-fns.lisp".
;
(in-package :si)

(setf (macro-function 'backquote)
  (lambda (form env)
      (declare (lambda-name (macro-function backquote)))
      (declare (ignore env))
    (process-backquote (second form)) ) )

;; For backquote
(defun list-to-vector (list)
  (let ((vector (make-simple-vector (length list)))
        (index 0) )
    (dolist (elt list vector)
      (setf (svref vector index) elt)
      (incf index) ) ) )

(defun ensure-test-function (test test-not)
  (cond
    ((and test test-not)
      (error "Can't specified both :test and :test-not") )
    (test test)
    (test-not (lambda (x y) (not (funcall test-not x y))))
    (t #'eql) ) )


;; analyze-lambda-list
(defun cl:member (item list &key (key #'identity) test test-not)
    (declare (type proper-list list))
    (declare (values proper-list))

  (setq test (ensure-test-function test test-not))

  (do ((runner list (cdr runner)))
      ((endp runner) nil)
    (when (funcall test item (funcall key (car runner)))
      (return runner) ) ) )

;; gensym
(defun cl:gensym (&optional (x "G"))
  (multiple-value-bind (prefix counter)
      (cond
        ((stringp x)
          (let ((counter *gensym-counter*))
            (setq *gensym-counter* (1+ counter))
            (values x counter) ) )

        ((and (integerp x) (>= x 0))
          (values "G" x) )
        (t
          (error 'type-error
            :datum x
            :expected-type '(or string (integer 0 *)) ) ))
    (make-symbol (format nil "~A~D" prefix counter)) ) )

(defun proper-list-p (list)
    (declare (type list list))
    (declare (values (or sequence-index null)))
  (cond
    ((null list) 0)
    ((not (consp list)) nil)
    (t
      (let ((length 0)
            (fast   list)
            (slow   list) )
         (declare (type sequence-index length))
       (loop
         (when (null fast) (return length))
         (unless (consp fast) (return nil))
         (incf length)
         (setq fast (cdr fast))

         (when (null fast) (return length))
         (unless (consp fast) (return nil))
         (incf length)
         (setq fast (cdr fast))

         (setq slow (cdr slow))
         (when (eq fast slow) (return nil)) ) ) )) )


(defun safe-list-length (list)
    (declare (type list list))
    (declare (values fixnum))
  (let ((n 0) (fast list) (slow list))
      (declare (type sequence-index n))
    (loop
      (unless (consp fast) (return))
      (setq fast (cdr fast))
      (setq n (1+ n))
      (unless (consp fast) (return))
      (setq fast (cdr fast))
      (setq n (1+ n))

      (setq slow (cdr slow))
      (when (eq fast slow) (return)) )

   (if (null fast) n (- -1 n)) ) )

(defun cl:every (predicate list)
  (dolist (elt list t)
    (unless (funcall predicate elt) (return nil)) ) )

(defun cl:notany (predicate list)
  (dolist (elt list t)
    (when (funcall predicate elt) (return nil)) ) )
