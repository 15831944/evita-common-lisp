;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - genesis
;;; lisp/genesis/g01-m02-backquote.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/genesis/g01-m02-backquote.lisp#5 $
;;;
;;; Description:
;;;  This file contains function definitions for backquote macro.
;
(in-package :si)

;;;; simple setf
;;;
;;;   (setf (fn arg ...) value) => (funcall #'(setf fn) value arg ...)
;;;
#+nil
(funcall #'(setf macro-function)
  (lambda (form env)
      (declare (lambda-name (macro-function setf)))
      (declare (ignore env))
     (let ((runner (cdr form))
           (result '()) )
       (loop
         (when (endp runner)
           (return (if (rest result)
                       (cons 'progn (nreverse result))
                     (first result) )) )
         (let ((place (pop runner))
               (value (pop runner)) )
           (if (symbolp place)
               (push (list 'setq place value) result)
             (push (list* 'funcall
                          (list 'function (list 'setf (first place)))
                          value (cdr place) )
                   result )) )) ) )
  'setf )

(defun reverse/list (list)
  (let ((result '()))
    (loop
      (when (endp list) (return))
      (push (pop list) result) )
    result ) )

(defun reverse (x) (reverse/list x))

(defun mapcar (fn list)
  (let* ((result (list 0))
         (last   result) )
    (dolist (x list (cdr result))
      (setq last (setf (cdr last) (list (funcall fn x)))) ) ) )


(defun cl:append (&rest lists)
    (declare (dynamic-extent lists))
  (do* ((result (list 1))
        (last   result)
        (scan   lists (cdr scan)) )
       ((null (cdr scan))
        (setf (cdr last) (car scan))
        (cdr result) )
    (let ((list (car scan)))
        (declare (type list list))
      (dolist (elt list)
        (let ((cons (cons elt nil)))
          (setf (cdr last) cons) 
          (setq last cons) ) ) ) ) )

(setf (macro-function 'case)
  (lambda (form env)
      (declare (lambda-name (macro-function case)))
      (declare (ignore env))
    (list 'let (list (list '.case (cadr form)))
      (cons 'cond
        (mapcar
            (lambda (clause)
              (let ((keys  (car clause))
                  (form* (cdr clause)) )
                (cond
                  ((eq keys 'otherwise)
                    (cons t form*) )
                  ((atom keys)
                    (cons (list 'eql '.case (list 'quote keys)) form*) )
                  ((null (cdr keys))
                    (cons (list 'eql '.case (list 'quote (car keys))) form*) )
                  (t
                   (cons (cons 'or
                                (mapcar (lambda (key)
                                            (list 'eql
                                                   '.case
                                                  (list 'quote key)) )
                                 keys ))
                    form* ) )) ))
            (cddr form) ))) ))
