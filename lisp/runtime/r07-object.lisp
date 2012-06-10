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
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r07-object.lisp#4 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;
;;; Internal Functions
;;;     copy-instance                           internal
;;;     ensure-class-designator                 internal
;;;     intern-class                            internal
;;;     make-class-description
;;;     subclassp                               internal
;;;
;;; Public Functions:
;;;     ensure-generic-function                 7.7.12
;;;     find-class                              7.7.28
;
(in-package :si)


;;;; copy-instance
;;;
;;; Called by:
;;;  change-class
;;;  clos:update-obsolete-instance
;;;
;;; Description:
;;;  Creates new instance and copies all slots of specified instance
;;;  to new one. All slots of new instance have same value even if
;;;  source slots are unbound.
;;;
;;;  This function is used for backing slots of instance being
;;;  changing layout.
;;;
;;; Note:
;;;  This function doesn't obsolete specified instance.
;
(defun copy-instance (instance)
    (declare (type instance instance))
    (declare (values instance))
  (let* ((class    (class-of instance))
         (classd   (.class-instance-description class))
         (new-instance (.allocate-instance classd)) )
    (loop for i from |lengthof Instance| below (instance-length instance) do
      (setf (instance-ref new-instance i) (instance-ref instance i)) )
    new-instance ) )


;;;; ensure-class-designator
;
(defun ensure-class-designator (class-or-name)
  (etypecase class-or-name
    (symbol (find-class class-or-name))
    (class   class-or-name) ) )


;;;; instance-length
;;;
;;; Description:
;;;  Returns number of cells used by specified instance.
;;;
;;; Note: User program should not use this function. This function is
;;; provided just for debugging, see debugger.
;;;
;
(defun instance-length (instance)
    (declare (type instance instance))
    (declare (values sequence-index))
  (let ((classd (si::classd-of instance)))
    (.classd-format-hint classd) ) )


;;;; intern-class
;;;
;;; Description:
;;;  Returns existing class named name or makes class of forward-referenced-
;;;  class.
;;;
;;; Note: This function is used by FASL.
;
(defun intern-class (name &optional env)
  (let ((class (find-class name nil env)))
    (when (null class)
      (setq class (make-instance 'clos:forward-referenced-class))
      (setf (.class-plist                   class) '())
      (setf (.class-flags                   class) 0)
      (setf (.class-direct-methods          class) '())
      (setf (.class-name class)             name)
      (setf (.class-direct-superclasses     class) '())
      (setf (.class-direct-subclasses       class) '())
      (setf (.class-class-precedence-list   class) '())
      (setf (.class-direct-slots            class) '())
      (setf (.class-slots                   class) '())
      (setf (.class-prototype               class) nil)

      (setf (find-class name nil env) class) )
    class ) )


;;;; subst-in-function
;;;
;;; Description:
;;;  Returns true when subsutituion taken place.
;
(defun subst-in-function (new-item old-item fn &optional fns)
    (declare (type function fn))
    (declare (type list fns))
    (declare (values t))
  (unless (member fn fns :test #'eq)
    (si::subst-in-function-1 new-item old-item fn (cons fn fns)) ) )


;;;; make-class-description
;;;
;;; Called by:
;;;  (boot) %define-condition
;;;  %defstruct
;;;  finalize-inheritance
;
(defun make-class-description (class slots)
  (let ((classd
          (.allocate-record
            (.class-instance-description
                #.(find-class 'si:class-description) )) )
        (nslots (length slots)) )

    (setf (.classd-class classd) class)

    (setf (.classd-hash-code classd)
      (1+ (logand (ash (si:address-of classd) -1) #xFFFF)) )

    (setf (.classd-slots classd) slots)

    (etypecase class
      (clos:funcallable-standard-class
        (setf (.classd-type-code classd) T_ByteCodeFunction)
        (setf (.classd-detail classd) |ClassD.InstanceFormat.Function|)
        (setf (.classd-format-hint classd) nslots) )

      (standard-class
        (setf (.classd-type-code classd) T_Instance)
        (setf (.classd-detail classd) |ClassD.InstanceFormat.Fixed|)
        (setf (.classd-format-hint classd)
          (let ((ncells (+ nslots |lengthof Instance|)))
            (+ ncells (logand ncells 1)) ) ) )

      (structure-class
        (setf (.classd-type-code classd) T_StructureObject)
        (setf (.classd-detail classd) |ClassD.InstanceFormat.Fixed|)
        (setf (.classd-format-hint classd)
          (let ((ncells (+ nslots |lengthof Record|)))
            (+ ncells (logand ncells 1)) ) ) )

      (built-in-class   ;; Genesis only
        (setf (.classd-type-code classd) T_Instance)
        (setf (.classd-detail classd) |ClassD.InstanceFormat.Fixed|)
        (setf (.classd-format-hint classd)
          (let ((ncells nslots))
            (+ ncells (logand ncells 1)) ) ) ))


    (assert (plusp (.classd-hash-code classd)))
    classd ) )


;;;; subclassp
;;;
;;; Description:
;;;  Returns true if class-1 is subclass of class-2.
;;;
;;; Note:
;;;  class-1 must be finalized.
;;;
;
(defun subclassp (class-1 class-2)
  (or (eq (find-class 't) class-2)
      (dolist (class (.class-class-precedence-list class-1) nil)
        (when (eq class class-2) (return t)) )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; MOP Functions
;;;;

;;;; MOP standard-instance-access
;
(defun clos:standard-instance-access (object location)
  (instance-ref object (+ |lengthof Instance| location)) )


;;;; MOP standard-instance-access
;
(defun (setf clos:standard-instance-access) (value object location)
  (setf (instance-ref object (+ |lengthof Instance| location)) value) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;

;;;; 7.7.28 find-class
;;;
;;; Syntax:
;;;     find-class symbol &optional error-p env
;;;
;;; BUGBUG: NYI: find-class env
;
(defun cl:find-class (symbol &optional (error-p t) env)
  (when (null env) (setq env si::*environment*))
  (loop
    (let ((class (gethash symbol (si::.env-classes env))))
      (when class (return class))
      (setq env (si::.env-outer env))
      (when (null env)
        (when error-p
          (error 'si::class-not-found :name symbol) )
        (return nil) ))) )


;;;; 7.7.28 (setf find-class)
(defun (setf cl:find-class) (class symbol &optional error-p env)
    (declare (values (or class null)))
    (declare (ignore error-p))
    (check-type class  (or class null))
    (check-type symbol symbol)
  (when (null env) (setq env si::*environment*))
  (let ((table (si::.env-classes env)))
    (if (null class)
        (remhash symbol table)
      (setf (gethash symbol table) class) )
    class ) )


;;;; 7.7.2 ensure-generic-function
(defun cl:ensure-generic-function (fname &rest initargs)
    (declare (dynamic-extent initargs))
  (apply #'clos:ensure-generic-function-using-class
        (and (fboundp fname) (fdefinition fname))
        fname
        initargs ) )
