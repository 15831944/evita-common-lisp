;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - Slot
;;; lisp/clos/o08-slot.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o13-slot.lisp#2 $
;;;
;;; Description:
;;;  This file contains slot related methods and functions.
;;;
;;; MOP Methods:
;;;     slot-boundp-using-class
;;;     slot-makunbound-using-class
;;;     slot-value-using-class
;;;     (setf slot-boundp-using-class)
;;;
;;; Public Functions:
;;;     slot-boundp         7.7.9   o03-boot
;;;     slot-exists-p       7.7.10
;;;     slot-makunbound     7.7.11  o03-boot
;;;     slot-missing        7.7.12
;;;     slot-value          7.7.14  o03-boot
;;;     slot-unbound        7.7.14

;
(in-package :si)

;;;; MOP slot-boundp-using-class
;;;; MOP slot-makunbound-using-class
;;;; MOP slot-value-using-class
;;;; MOP (setf slot-boundp-using-class)
;
(macrolet (
  (define (class reader writer slot-class)
    `(progn
       ;; slot-boundp-using-class
       (defmethod clos:slot-boundp-using-class
            ((class ,class)
             object
             (slotd ,slot-class) )
         (let* ((index (slotd-location slotd))
                (value (,reader object index)) )
           (not (eq '#.(unbound-marker) value)) ) )

       ;; slot-makunbound-using-class
       (defmethod clos:slot-makunbound-using-class
            ((class ,class)
             object
             (slotd ,slot-class) )
         (let ((index (slotd-location slotd)))
           (,writer '#.(unbound-marker) object index)
           object ) )

       ;; slot-value-using-class
       (defmethod clos:slot-value-using-class
            ((class ,class)
             object
             (slotd ,slot-class) )
         (let* ((index (slotd-location slotd))
                (value (,reader object index)) )
           (when (eq '#.(unbound-marker) value)
             (slot-unbound class object (slot-definition-name slotd)) )
           value ) )

       ;; (setf slot-value-using-class)
       (defmethod (setf clos:slot-value-using-class)
            (value
             (class ,class)
             object
             (slotd ,slot-class) )
         (let ((index (slotd-location slotd)))
           (,writer value object index) ) )) )

    ;; slotd-location
    ;;
    (slotd-location (slotd)
      `(if (eq (find-class 'standard-effective-slot-definition)
               (class-of ,slotd) )
           (ref standard-effective-slot-definition location
                    (ref instance storage ,slotd) )
         (slot-value ,slotd 'location) ) )

    (fsc-reader (object index)
      `(if (consp ,index)
           (cdr ,index)
         (funcallable-standard-instance-access ,object ,index) ) )

    (fsc-writer (value object index)
      `(if (consp ,index)
           (setf (cdr ,index) ,value)
         (setf (funcallable-standard-instance-access ,object ,index)
               ,value )) )

    (std-reader (object index)
      `(if (consp ,index)
           (cdr ,index)
         (standard-instance-access ,object ,index) ) )

    (std-writer (value object index)
      `(if (consp ,index)
           (setf (cdr ,index) ,value)
         (setf (standard-instance-access ,object ,index) ,value) ) )

    (str-reader (object index)
      `(structure-instance-access ,object ,index) )

    (str-writer (value object index)
      `(setf (structure-instance-access ,object ,index) ,value) )
  )
  ;;
  (define funcallable-standard-class
          fsc-reader
          fsc-writer
          standard-effective-slot-definition )

  (define standard-class
          std-reader
          std-writer
          standard-effective-slot-definition )

  (define structure-class
          str-reader
          str-writer
          structure-effective-slot-definition )
  ) ; macrolet

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;


;;;; initialize-instance :after standard-direct-slot-definition
;;;
;;; BUGBUG: NYI: Where do we store documentation of slot?
(defmethod initialize-instance :after
      ((instance standard-direct-slot-definition) &key documentation)
  (declare (ignore documentation)) )


;;;; 7.7.10 slot-exists-p
(defun cl:slot-exists-p (object slot-name)
  (clos:slot-exists-p-using-class (class-of object) object slot-name) )


;;;; 7.7.12 slot-missing
;;;
;;; BUGBUG: NYI: restart for slot-missing.
(defmethod cl:slot-missing (class object slot-name operation
                            &optional new-value )
    (declare (type (member setf slot-boundp slot-makunbound slot-value)
                   operation ))
    (declare (ignore class operation new-value))
  (error 'slot-missing-error
         :instance  object
         :slot-name slot-name ) )


;;;; 7.7.14 slot-unbound
(defmethod cl:slot-unbound (class object slot-name)
    (declare (ignore class))
  (error 'unbound-slot
         :instance object
         :name     slot-name ) )

;;;; 7.7.9  slot-boundp
;;;; 7.7.11 slot-makunbound
;;;; 7.7.14 slot-value
;;;; MOP slot-exists-p-using-class
(labels (
    ;; base-class-p
    (base-class-p (class)
      (let ((class (class-of class)))
        (or (eq class (find-class 'standard-class))
            (eq class (find-class 'funcallable-standard-class))
            (eq class (find-class 'structure-class)) ) ) )


    ;; base-slotd-p
    (base-slotd-p (slotd)
      (let ((class (class-of slotd)))
        (or (eq class (find-class 'standard-effective-slot-definition))
            (eq class (find-class 'standard-direct-slot-definition))
            (eq class (find-class 'standard-slot-definition)) ) ) )


    ;; find-slot
    (find-slot (slot-name class)
      (loop for slotd in (if (base-class-p class)
                             (ref standard-class slots
                                    (ref instance storage class) )
                           (class-slots class) )
        when (if (base-slotd-p slotd)
                 (eq (ref standard-effective-slot-definition name
                            (ref instance storage slotd) )
                     slot-name )
               (eq (slot-definition-name slotd) slot-name) )
         return slotd ) )
    )

    ;; MOP: slot-exists-p-using-class
    (defmethod clos:slot-exists-p-using-class ((class class) object slot-name)
        (declare (ignore object))
      (find-slot slot-name class) )

    (macrolet (
      (define (fn)
        (let* ((method (intern (format nil "~A-USING-CLASS" fn))) )
          `(defun ,fn (object slot-name)
             (let* ((class (class-of object))
                    (slotd (find-slot slot-name class)) )
               (unless slotd
                 (slot-missing class object slot-name ',fn) )
               (,method class object slotd) ) ) ) )
      )
      ;;
      (define cl:slot-boundp)
      (define cl:slot-makunbound) )

    ;; (setf slot-value)
    (defun (setf cl:slot-value) (value object slot-name)
      (let* ((class (class-of object))
             (eslotd (find-slot slot-name class)) )
        (unless eslotd
          (slot-missing class object slot-name 'setf) )
        (setf (slot-value-using-class class object eslotd) value) ) )

    ;;;; 7.7.14 slot-value
    ;;;
    ;;; Note: We don't call slot-value-using-class for following classes,
    ;;; since generic-function discriminator uses slot-value.
    ;;;     standard-class
    ;;;     standard-generic-function
    ;;;     standard-method
    ;;;     standard-reader-method
    ;;;
    ;;; Above classes doesn't not have class slot.
    ;
    (defun cl:slot-value (object slot-name)
        (declare (type symbol slot-name))
      (labels (
        (fsc-slot-value (object slotd)
          (let ((index (slotd-location slotd)))
              (declare (type ext:sequence-index index))
            (funcallable-standard-instance-access object index) ) )

        (slotd-location (slotd)
          (if (eq (find-class 'standard-effective-slot-definition)
                  (class-of slotd) )
              (ref standard-effective-slot-definition location
                        (ref instance storage slotd) )
            (slot-value slotd 'location) ) )

        (std-slot-value (object slotd)
          (let ((index (slotd-location slotd)))
              (declare (type ext:sequence-index index))
            (standard-instance-access object index) ) )
        )
        ;;
        ;; cl:slot-value
        ;;

        ;; We check obsolete instance here. Because of
        ;;  1. slot-value-using-class may not checks obsolete instance.
        ;;  2. A class of object could be changed to one of special handling
        ;;     classes.
        (let ((classd (classd-of object)))
          (when (zerop (ref classd hash-code classd))
            (update-obsolete-instance object) ) )

        (let* ((class (class-of object))
               (slotd (find-slot slot-name class)) )
          (unless slotd
            (slot-missing class object slot-name 'cl:slot-value) )
          (cond
            ((or (eq (find-class 'standard-class) class)
                 (eq (find-class 'standard-method) class)
                 (eq (find-class 'standard-reader-method) class) )
              (let ((value (std-slot-value object slotd)))
                (when (eq '#.(unbound-marker) value)
                  (slot-unbound class object slot-name) )
                value ) )

            ((eq (find-class 'standard-generic-function) class)
              (let ((value (fsc-slot-value object slotd)))
                (when (eq '#.(unbound-marker) value)
                  (slot-unbound class object slot-name) )
                value ) )
            (t
              (slot-value-using-class class object slotd) )) ) ) )
 ) ; labels
