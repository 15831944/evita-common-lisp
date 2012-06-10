;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 7 Objects
;;; lisp/runtime/r07-object.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime3/r07-object.lisp#3 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;
;
(in-package :si)

;;;; 7.7.28 find-class
(defun cl:find-class (name &optional (error-p t) env)
  (let ((env (or env *environment*)))
    (loop
      (with-latch ((ref environment latch env) :shared)
        (let ((class (gethash/eq name (ref environment classes env))))
          (when class (return-from cl:find-class class))
        (setq env (ref environment outer env))
        (when (null env) (return)) )) )
    (when error-p (error 'class-not-found :name name)) ) )


;;;; 7.7.28 (setf find-class)
(defun (setf cl:find-class) (class name &optional error-p env)
    (declare (ignore error-p))
  (let ((env (or env *environment*)))
    (with-latch ((ref environment latch env) :shared)
      (let ((htb (ref environment classes env)))
        (if class
            (setf (gethash/eq name htb) class)
          (remhash name htb) ) )) )
  class )


;;;; 7.7.2 ensure-generic-function
(defun cl:ensure-generic-function (fname &rest initargs)
    (declare (dynamic-extent initargs))
  (apply #'clos:ensure-generic-function-using-class
        (and (fboundp fname) (fdefinition fname))
        fname
        initargs ) )


;;;; intern-class
(defun intern-class (name)
  (or (find-class name nil)
      (setf (find-class name)
        (make-instance 'forward-referenced-class :name name) )) )


;;;; MOP intern-eql-specializer
;;;
;;; For: defmethod
;;; For: find-method-combination.
;
(defun clos:intern-eql-specializer (object)
  (labels (
    (intern-aux ()
      (let ((specializer (gethash object *eql-specializers*)))
        (unless specializer
          (setq specializer (make-instance 'clos:eql-specializer
                                           :object object ))
          (setf (gethash object *eql-specializers*) specializer) )
        specializer ) )
    )
    ;;
    (with-latch (*eql-specializers-latch*)
      (intern-aux) ) ) )
