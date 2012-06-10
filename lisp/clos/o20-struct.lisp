;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - Record
;;; lisp/clos/o10-struct.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o20-struct.lisp#4 $
;;;
;;; Description:
;;;  This file contains structure related methods.
;;;
;;;  Implementation:
;;;     %defstruct
;;;
;;;  MOP methods:
;;;     finalize-inheritance
;;;
;;;  Public methods:
;;;     allocate-instance
;;;     make-instance
;
(in-package :si)

;;;; MOP compute-slots structure-class
;;;
;;; Description:
;;;  Makes list effective slot definition and returns it.
;;;
;;; Note: We don't use around method for assigning instance slot location.
;;; ACL and LispWorks also don't use it.
;
(defmethod clos:compute-slots ((class structure-class))
  (let ((slotd-alist '()))
    (loop for super in (reverse (slot-value class 'class-precedence-list)) do
      (loop for slotd in (slot-value super 'direct-slots) do
        (let* ((name (slot-definition-name slotd))
               (name.slotds (assoc name slotd-alist :test #'eq)) )
          (if name.slotds
              (push slotd (cdr name.slotds))
            (push (list name slotd) slotd-alist) ) )))
    (setq slotd-alist (nreverse slotd-alist))

    (loop
      with location = 0
      for (name . slotds) in slotd-alist
      for slotd = (compute-effective-slot-definition class name slotds)
      do
        (setf (slot-value slotd 'location) location)
        (incf location)
      collect slotd ) ) )


;;;; default-direct-superclass structure-class
;
(defmethod default-direct-superclass ((class structure-class))
  (find-class 'structure-object) )


;;;; default-direct-superclass list-structure-type
;
(defmethod default-direct-superclass ((class list-structure-type))
  (find-class 'list) )


;;;; default-direct-superclass vector-structure-type
;
(defmethod default-direct-superclass ((class vector-structure-type))
  (let ((type (slot-value class 'type)))
    (if (eq type 'vector)
        (find-class 'simple-vector)
      (vector-class (second type)) ) ) )


;;;; MOP direct-slot-definition-class
;;;
;;; Called by:
;;;  shared-initialize :after (structure-class)
;;;
;;; Description:
;;;  Returns class object for direct slot definition of structure
;;;  definition. We use class structure-direct-slot-definition for keeping
;;;  read-only information for defstruct macro.
;
(defmethod clos:direct-slot-definition-class
        ((class structure-class) &rest initargs)
    (declare (dynamic-extent initargs))
    (declare (ignore initargs))
  (find-class 'structure-direct-slot-definition) )


;;;; MOP effective-slot-definition-class
;
(defmethod clos:effective-slot-definition-class
        ((class structure-class) &rest initargs)
    (declare (dynamic-extent initargs))
    (declare (ignore initargs))
  (find-class 'structure-effective-slot-definition) )


;;;; MOP finalize-inheritance
;
(defmethod clos:finalize-inheritance ((class structure-class))
  nil )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Mehtods
;;;;

;;;; allocate-instance (structure-class)
;;;
;;; Note:
;;;  class-prototype may call allocate-instance for making an instance
;;;  for filling prototype slot of class.
;
(defmethod cl:allocate-instance ((class structure-class) &rest initargs)
    (declare (ignore initargs))
  (labels (
    ;; make-prototype
    ;;  Since value of slots of newly allocated instance of structure-class
    ;;  are unspecifeid. We set initial value to slots which initforms are
    ;;  constant, e.g. nil, 0.
    (make-prototype (class)
        (declare (type structure-class class))
        (declare (values structure-object))
      (loop
        with classd   = (slot-value class 'instance-description)
        with instance = (.allocate-record classd)
        for slotd in (ref classd slots classd)
        for initform = (slot-value slotd 'initform)
        for initfunction = (slot-value slotd 'initfunction) do
          (setf (structure-instance-access
                        instance
                        (slot-value slotd 'location) )
            (if (and (functionp initfunction) (constantp initform))
                (funcall initfunction)
              '#.(unbound-marker) ))
        finally (return instance) ) )
    )
    ;;
    (let ((prototype (slot-value class 'prototype)))
      (when (null prototype)
        (setq prototype (make-prototype class))
        (setf (slot-value class 'prototype) prototype) )
      (copy-structure prototype) ) ) )


;;;;; shared-initialize (structure-object)
;;;
;;; Description:
;;;  Initializes slot of structure-object from initargs.
;
(defmethod cl:shared-initialize
        ((instance structure-object) slot-names &rest initargs)
    (declare (type list slot-names))
    (declare (values structure-object))
    (declare (dynamic-extent initargs))
    (declare (ignore slot-names))
  (let ((class (class-of instance)))
    (dolist (slotd (class-slots class) instance)
      (multiple-value-bind (indicator value found-p)
          (get-properties initargs (slot-definition-initargs slotd))
          (declare (ignore indicator))
        (cond
          (found-p
            (setf (slot-value-using-class class instance slotd) value) )

          ((not (slot-boundp-using-class class instance slotd))
            (let ((fn (slot-definition-initfunction slotd)))
              (when (functionp fn)
                (setf (slot-value-using-class class instance slotd)
                      (funcall fn) )) ) )) ) ) ) )


;;;; shared-initialize :after (structure-class)
;
(defmethod cl:shared-initialize :after
  ((class structure-class)
   slot-names
   &key (direct-superclasses     (class-direct-superclasses class))
        (direct-slots            '() direct-slots-p)
        (direct-default-initargs (class-direct-default-initargs class))
        ;documentation
   &allow-other-keys )
    (declare (ignore slot-names))

  (labels (
    ;; check-direct-superclass
    (check-direct-superclass ()
      (when (null direct-superclasses)
        (setq direct-superclasses (list (default-direct-superclass class))) )

      (when (rest direct-superclasses)
        (error "~S can't have multiple direct super classes.") )

      (let ((superclass (first direct-superclasses)))
        (unless (validate-superclass class superclass)
          (error 'incompatible-superclass
                 :class      class
                 :superclass superclass )) )
      (setf (slot-value class 'direct-superclasses) direct-superclasses) )

    ;; finalize
    ;;  Sets CPL, effective-slots and instance-description.
    ;;  Restes prototype.
    (finalize ()
      (setf (slot-value class 'class-precedence-list)
            (cons class
                (class-precedence-list (first direct-superclasses)) ))

      (let ((slots  (compute-slots class)))
        (setf (slot-value class 'slots) slots)

        (setf (slot-value class 'instance-description)
          (make-class-description class slots) )

        (setf (slot-value class 'prototype) nil)

        (install-accessors slots) ) )

    ;; install-accessors
    (install-accessors (slots)
      (loop
        for dslotd in (class-direct-slots class)
        for slotd  = (or (find (slot-definition-name dslotd) slots
                              :key  #'slot-definition-name
                              :test #'eq )
                         (error "Can't happen! dslotd=~S slots=~S"
                            dslotd slots ))
        for location = (slot-definition-location slotd) do
          (dolist (reader (slot-definition-readers dslotd))
            (install-function reader (make-reader class location) ) )

          (dolist (writer (slot-definition-writers dslotd))
             (install-function writer (make-writer class location) ) )) )

    ;; install-function
    (install-function (fname fn)
      (setf (function-name fn) fname)
      (setf (fdefinition fname) fn) )

    ;; link-classes
    ;;  Links super/sub class.
    (link-classes ()
      (let ((superclass (first direct-superclasses)))
        (add-direct-subclass superclass class) ) )

    ;; make-reader
    (make-reader (class location)
      (lambda (object)
          (declare (ext:lambda-name (:reader structure-object)))
        (unless (typep object class)
          (error 'type-error :datum object :expected-type (class-of class)) )
        (structure-instance-access object location) ) )

    ;; make-writer
    (make-writer (class location)
      (lambda (value object)
          (declare (ext:lambda-name (:writer structure-object)))
        (unless (typep object class)
          (error 'type-error :datum object :expected-type (class-of class)) )
        (setf (structure-instance-access object location) value) ) )

    ;; set-direct-default-initargs
    (set-direct-default-initargs ()
      (if (null direct-default-initargs)
          (remf (slot-value class 'plist) 'direct-default-initargs)
       (setf (getf (slot-value class 'plist) 'direct-default-initargs)
           direct-default-initargs )) )

    ;; set-direct-slots
    (set-direct-slots ()
      (when direct-slots-p
        (loop
          with slotd-class = (direct-slot-definition-class class)
          for initargs in direct-slots
          for slotd = (apply #'make-instance slotd-class initargs)
            collect slotd into slotds
            finally
              (setf (slot-value class 'direct-slots) slotds) )) )
    )
    ;;
    (check-direct-superclass)
    (link-classes)
    (set-direct-slots)
    (set-direct-default-initargs)
    (finalize) ) )


;;;; %defstruct
;;; Expansion of defstruct macro. This function makes or reinitialzes
;;; structure-class of class-name by invoking ensure-class-using-class or
;;; ensure-strucrure-type.
(defun %defstruct (class-name direct-superclass-name direct-slot-specs
                       &rest initargs
                       &key  ((:environment env))
                             type
                       &allow-other-keys )
  (labels (
    ;; ensure-structure-class
    (ensure-structure-class ()
      (apply #'ensure-class-using-class
            (let ((class (find-class class-name nil env)))
              (cond
                ((null env) class)
                ((eq (find-class class-name nil) class) nil)
                (t class) ) )
            class-name
            :direct-superclasses
                (and direct-superclass-name (list direct-superclass-name))
            :direct-slots
                direct-slot-specs
            :environment
                env
            initargs ) )

    ;; ensure-structure-type
    (ensure-structure-type ()
     (apply #'ensure-structure-using-class
        (let ((class (find-structure class-name nil env)))
          (cond
            ((null env) class)
            ((eq (find-structure class-name nil) class) nil)
            (t class) ) )
        class-name
        :direct-superclasses
            (and direct-superclass-name
                 (list (find-structure direct-superclass-name nil env)))
        :direct-slots direct-slot-specs
        :environment env
        initargs ) )
    )
    ;;
    (if type
        (ensure-structure-type)
      (ensure-structure-class) )
    class-name ) )


;;;; ensure-structure-using-class class
;
(defmethod ensure-structure-using-class ((class structure-type) name
                                          &rest args )
    (declare (ignore name))
  (multiple-value-bind (metaclass initargs)
      (ensure-class-initargs class args)
    (unless (eq (class-of class) metaclass)
      (change-class class metaclass) )
    (apply #'reinitialize-instance class initargs)
    ;; Note: PCL calls (setf find-class), but we don't need to do so.
    ;; PCL: (setf (find-class name) class)
    ;; PCL: (install-class-as-type class name)
    class ) )


;;;; ensure-structure-using-class null
;
(defmethod ensure-structure-using-class
        ((class null) name &rest args
         &key ((:environment env))
         &allow-other-keys )
  (multiple-value-bind (metaclass initargs)
      (ensure-class-initargs class args)
    (setq class (apply #'make-instance metaclass :name name initargs))
    (setf (find-structure name nil env) class)
    class ) )


;;;; validate-superclass list-structure-type class
;
(defmethod validate-superclass ((class list-structure-type)
                                (new-super class) )
  (subclassp new-super (find-class 'list)) )


;;;; validate-superclass vector-structure-type class
;
(defmethod validate-superclass ((class vector-structure-type)
                                (new-super class) )
  (subclassp new-super (find-class 'vector)) )
