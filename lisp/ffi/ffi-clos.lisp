;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - runtime - ffi - CLOS
;;; arch/generic/lisp/ffi/gen-ffi-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/ffi/ffi-clos.lisp#6 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

;;;; offsetof
(defun offsetof (thing slot-name)
    (declare (values sequence-index))
    (declare (type (or class symbol) thing))
    (declare (type symbol slot-name))
  (let ((sizeof-value #+32bit 4 #+64bit 8))
  (labels (
    ;; find-slot
    (find-slot (class slot-name)
        (declare (values effective-slot-definition))
        (declare (type class class))
        (declare (type symbol slot-name))
      (or (find slot-name (class-slots class)
                :key  #'slot-definition-name
                :test #'eq )
          (error "Class ~S doesn't have slot ~S."
            (class-name class)
            slot-name )) )

    ;; offsetof-aux
    (offsetof-aux (class)
        (declare (values sequence-index))
        (declare (type class class))
      (etypecase class
        (built-in-class
          (slot-value (find-slot class slot-name) 'location) )
        (foreign-class
          (slot-value (find-slot class slot-name) 'location) )
        (standard-class
          (* (1+ (slot-value (find-slot class slot-name) 'location))
             sizeof-value ) )
        (structure-class
          (* (1+ (slot-value (find-slot class slot-name) 'location))
             sizeof-value ) )) )
    )
    ;;
    (typecase thing
      (symbol (offsetof-aux (find-class thing)))
      (class  (offsetof-aux thing))
      (otherwise
        (error 'type-error :datum thing
            :expect-type '(or class symbol) ) )) ) ) )


;;;; sizeof
(defun sizeof (typespec &optional (toplevel-p t))
  (let ((sizeof-val #+32bit 4 #+64bit 8))
  (labels (
    ;; sizeof-aux
    (sizeof-aux (typespec toplevel-p)
      (typecase typespec
        (symbol (sizeof/symbol typespec toplevel-p))
        (cons   (sizeof/cons   typespec))
        (class  (sizeof/class  typespec toplevel-p))
        (otherwise (error "Invalid type specifier: ~S" typespec)) ) )

    ;; sizeof/class
    (sizeof/class (class toplevel-p)
        (declare (values sequence-index))
        (declare (type class class))
      (let ((slots (class-slots class)))
        (cond
          ((eq class #.(find-class 'fixnum))
            sizeof-val )
          ((or (typep class 'built-in-class)
               (typep class 'foreign-class) )
            (let ((eslotd (first (last slots))))
              (+ (slot-value eslotd 'location)
                 (sizeof-aux (slot-value eslotd 'type) nil) ) ) )
          (toplevel-p
            (* (length slots) sizeof-val) )
          (t
            sizeof-val )) ) )

    ;; sizeof/cons
    (sizeof/cons (typespec)
        (declare (values sequence-index))
      (case (first typespec)
        ((ptr) sizeof-val)
        ((vec) (* (sizeof (second typespec)) (third typespec)))
        (otherwise sizeof-val) ) )

    ;; sizeof/symbol
    (sizeof/symbol (name toplevel-p)
        (declare (values sequence-index))
      (case name
        ((uint8 int8) 1)
        ((uint16 int16) 2)
        ((uint32 int32 float32) 4)
        ((uint64 int64) 8)
        ((int uint) sizeof-val)
        ((float64) 4)
        (otherwise
          (let ((class (find-class name nil)))
            (if class
                (sizeof/class class toplevel-p)
              sizeof-val ) ) )) )
    )
    ;;
    (sizeof-aux typespec toplevel-p) ) ) )


;;;; tagof
(defun tagof (thing)
    (declare (values (unsigned-byte 8)))
    (declare (type (or class symbol) thing))
  (labels (
    ;; tagof-aux
    (tagof-aux (class)
        (declare (values (unsigned-byte 8)))
        (declare (type class class))
      (ref class-description tag-code
        (slot-value class 'instance-description) ) )
    )
    (typecase thing
      (symbol (tagof-aux (find-class thing)))
      (class  (tagof-aux thing))
      (otherwise (error 'type-error :datum thing
        :expected-type '(or class symbol) ) )) ) )


;;;; clos:compute-slots
(defmethod clos:compute-slots ((class foreign-class))
  (labels (
    (make-slot-map ()
      (let ((dslotd-alist '()))
        (loop for super in
                (reverse (slot-value class 'class-precedence-list)) do
          (loop for dslotd in (slot-value super 'direct-slots) do
            (let* ((name (slot-definition-name dslotd))
                   (name.dslotds (assoc name dslotd-alist :test #'eq)) )
              (if name.dslotds
                  (push dslotd (cdr name.dslotds))
                (push (list name dslotd) dslotd-alist) ) )))
        (nreverse dslotd-alist) ) )
    )
    ;;
    (loop
      with location = 0
      for (name . dslotds) in (make-slot-map)
      for eslotd = (compute-effective-slot-definition class name dslotds)
      for size   = (sizeof (slot-value eslotd 'type) nil)
      for align  = (if (<= size 8) (rem location size) 0) do
        (unless (zerop align) (incf location (- size align)))
        (setf (slot-value eslotd 'location) location)
        (incf location size)
      collect eslotd ) ) )


;;;; default-direct-superclass foreign-class
;
(defmethod default-direct-superclass ((class foreign-class))
  (find-class 'foreign-object) )


(defmethod direct-slot-definition-class
        ((class foreign-class) &rest initargs)
    (declare (dynamic-extent initargs))
    (declare (ignore initargs))
  (find-class 'foreign-direct-slot-definition) )


;;;; MOP effective-slot-definition-class
;
(defmethod effective-slot-definition-class
        ((class foreign-class) &rest initargs)
    (declare (dynamic-extent initargs))
    (declare (ignore initargs))
  (find-class 'foreign-effective-slot-definition) )


;;;; MOP finalize-inheritance
;
(defmethod finalize-inheritance ((class foreign-class))
  nil )

(defmethod cl:allocate-instance ((class foreign-class) &rest initargs)
  (error "NYI: allocate-instance ~S ~S" class initargs) )


(defmethod cl:shared-initialize :after
  ((class foreign-class)
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

      (let ((slots (compute-slots class)))
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
            (setf (symbol-function reader)
              (make-reader class location) ) )

          (dolist (writer (slot-definition-writers dslotd))
             (setf (fdefinition writer)
               (make-writer class location) ) )) )

    ;; link-classes
    ;;  Links super/sub class.
    (link-classes ()
      (let ((superclass (first direct-superclasses)))
        (add-direct-subclass superclass class) ) )

    ;; make-reader
    (make-reader (class location)
      (lambda (object)
          (declare (ext:lambda-name (:reader foreign-object)))
        (unless (typep object class)
          (error 'type-error :datum object :expected-type (class-of class)) )
        (foreign-instance-access object location) ) )

    ;; make-writer
    (make-writer (class location)
      (lambda (value object)
          (declare (ext:lambda-name (:writer foreign-object)))
        (unless (typep object class)
          (error 'type-error :datum object :expected-type (class-of class)) )
        (setf (foreign-instance-access object location) value) ) )

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
