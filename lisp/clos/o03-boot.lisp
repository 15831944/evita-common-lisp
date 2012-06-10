;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - Bootstrapping
;;; lisp/clos/o03-boot.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o03-boot.lisp#2 $
;;;
;;; Description:
;;;  This file contains MOP functions for bootstrapping. All functions in
;;;  this file will be replaced actual implementation.
;;;
;;; Internal Functions:
;;;     %defgeneric                             loader final
;;;     base-class-p                            META-BASE
;;;     base-slotd-p                            META-BASE
;;;     find-slot                               META-BASE
;;;
;;; MOP Functions (stab):
;;;     class-precedence-list                   MOP boot
;;;     class-prototype                         MOP boot
;;;     class-slots                             MOP boot
;;;     direct-slot-definition-class            MOP boot
;;;     ensure-generic-function-using-class     MOP boot
;;;     funcallable-standard-instance-access    MOP boot
;;;     generic-function-method-class           MOP boot
;;;     intern-eql-specializer                  MOP final
;;;     slot-definition-name                    MOP boot
;;;     slot-definition-initargs                MOP boot
;;;     standard-instance-access                MOP boot
;;;
;;; Public Functions (stab):
;;;     add-method                              7.7.35  boot
;;;     ensure-generic-function                 7.7.2   boot
;;;     slot-boundp                             7.7.9   final
;;;     slot-makunbound                         7.7.9   final
;;;     slot-value                              7.7.14  final
;;;
;;; BUGBUG: (sxhash class) assumes slot "name" is the first slot.
;
(in-package :si)

#|
;;;; base-class-p
;
(defun base-class-p (class)
  (let ((class (class-of class)))
    (or (eq class (find-class 'standard-class))
        (eq class (find-class 'funcallable-standard-class))
        (eq class (find-class 'structure-class)) ) ) )


;;;; base-slotd-p
;
(defun base-slotd-p (slotd)
  (let ((class (class-of slotd)))
    (or (eq class (find-class 'standard-effective-slot-definition))
        (eq class (find-class 'standard-direct-slot-definition))
        (eq class (find-class 'standard-slot-definition)) ) ) )


;;;; find-slot
;
(defun find-slot (slot-name class)
  (loop for slotd in (if (base-class-p class)
                         (.class-slots class)
                       (class-slots class) )
    when (if (base-slotd-p slotd)
             (eq (.slotd-name slotd) slot-name)
           (eq (slot-definition-name slotd) slot-name) )
     return slotd ) )
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; MOP
;;;;

;;;; MOP class-precedence-list
;;;
;;; For: defmethod
;;;
;;; Note: Final version should not call make-instance. It may call
;;; initfunction.
;
#+nil
(defun clos:class-precedence-list (class)
    (check-type class class)
  (.class-class-precedence-list class) )


;;;; MOP class-prototype
;;;
;;; For: defmethod
;;;
;;; Note: Final version should not call make-instance. It may call
;;; initfunction.
;
#+nil
(defun clos:class-prototype (class)
    (check-type class class)
  (let ((prototype (.class-prototype class)))
    (unless prototype
      (setq prototype (make-instance class)) )
    prototype ) )


;;;; MOP class-slots
;;;
;;; For: defclass
;;;
;;; Note: r07-object also defines class-slots.
;;; Note: We can't use slot-value.
;;;
;
#+nil
(defun clos:class-slots (class)
    (check-type class class)
  (.class-slots class) )


;;;; MOP direct-slot-definition-class
;;;
;;; For defclass
;
#+nil
(defun clos:direct-slot-definition-class (class)
    (check-type class standard-class)
  (find-class 'standard-slot-definition) )


;;;; MOP ensure-class
;
(defun clos:ensure-class (name &rest args)
    (declare (dynamic-extent args))
  (apply #'ensure-class-using-class (find-class name nil) name args) )


;;;; MOP ensure-class-using-class
;
(defun clos:ensure-class-using-class (existing name &rest args)
    (declare (ignore class))
    (declare (ignore args))
    (declare (dynamic-extent args))
  (or existing (find-class name)) )


;;;; MOP ensure-generic-function-using-class (BOOT)
;
(defun clos:ensure-generic-function-using-class
    (existing fname &rest initargs
                    &key (lambda-list nil lambda-list-p)
                         ((:argument-precedence-order apo))
                         (method-class 'standard-method) )
    (declare (dynamic-extent initargs))
    (declare (ignore initargs))

  (setq method-class (ensure-class-designator method-class))

  (let ((gf existing))
    (unless (typep gf 'generic-function)
      (setq gf (cdr (assoc fname *early-generic-function-alist*
                           :test #'equal ))))

    (unless (typep gf 'generic-function)
      (setq gf (make-instance 'standard-generic-function))
      (setf (slot-value gf 'name)         fname)
      (setf (slot-value gf 'method-class) method-class)
      (setf (slot-value gf 'method-combination) *standard-method-combination*)
      (set-funcallable-instance-function
        gf
        (compute-discriminating-function gf) )

      (when lambda-list-p
        (set-generic-function-lambda-list gf lambda-list apo) )

      (if (fboundp fname)
          (push (cons fname gf) *early-generic-function-alist*)
        (setf (fdefinition fname) gf) ))
    gf ) )


;;;; MOP intern-eql-specializer
;;;
;;; For: defmethod
;;; For: find-method-combination.
;
(defun clos:intern-eql-specializer (object)
  (let ((specializer (gethash object *eql-specializers*)))
    (unless specializer
      (setq specializer (make-instance 'clos:eql-specializer
                                       :object object ))
      (setf (gethash object *eql-specializers*) specializer) )
    specializer ) )


;;;; MOP generic-function-method-class
;;;
;;; For: defmethod
;;;
;
#+nil
(defun clos:generic-function-method-class (gf)
    (check-type gf standard-generic-function)
  (slot-value gf 'method-class) )


;;;; MOP slot-boundp-using-class
;;;; MOP slot-makunbound-using-class
;;;; MOP slot-value-using-class
;;;; MOP (setf slot-value-using-class)
;
(macrolet (
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

    ;; reader
    ;;
    (reader (object index)
      `(etypecase class
         (standard-class
            (let ((classd (classd-of ,object)))
              (when (zerop (.classd-hash-code classd))
                (update-obsolete-instance ,object) ) )
            (std-reader ,object ,index) )

         (funcallable-standard-class
            (let ((classd (.funcallable-instance-classd ,object)))
              (when (zerop (.classd-hash-code classd))
                (update-obsolete-instance ,object) ) )
            (fsc-reader ,object ,index) )

         (structure-class
           (str-reader ,object ,index) )) )

    ;; writer
    ;;
    (writer (value object index)
      `(etypecase class
         (standard-class
            (let ((classd (classd-of ,object)))
              (when (zerop (.classd-hash-code classd))
                (update-obsolete-instance ,object) ) )
           (std-writer ,value ,object ,index) )

         (funcallable-standard-class
            (let ((classd (.funcallable-instance-classd ,object)))
              (when (zerop (.classd-hash-code classd))
                (update-obsolete-instance ,object) ) )
           (fsc-writer ,value ,object ,index) )

         (structure-class
           (str-writer ,value ,object ,index) )) )
    )
    ;;

    ;; slot-boundp-using-class
    ;;
    (defun slot-boundp-using-class (class object slotd)
      (let* ((index (.slotd-location slotd))
             (value (reader object index)) )
        (not (eq '#.(unbound-marker) value)) ) )


    ;; slot-makunbound-using-class
    ;;
    (defun slot-makunbound-using-class (class object slotd)
      (let ((index (.slotd-location slotd)))
        (writer '#.(si:unbound-marker) object index) ) )


    ;; slot-value-using-class
    ;;
    (defun slot-value-using-class (class object slotd)
      (let* ((index (.slotd-location slotd))
             (value (reader object index)) )
        (when (eq '#.(si:unbound-marker) value)
          (slot-unbound class object (slot-definition-name slotd)) )
        value ) )


    ;; slot-value-using-class
    ;;
    (defun (setf slot-value-using-class) (value class object slotd)
      (let ((index (.slotd-location slotd)))
        (writer value object index) ) )
 ) ; macrolet


;;;; MOP slot-definition-initargs
;
(defun clos:slot-definition-initargs (slotd)
    (check-type slotd standard-slot-definition)
  (.slotd-initargs slotd) )


;;;; MOP slot-definition-name
;
(defun clos:slot-definition-name (slotd)
    (check-type slotd standard-slot-definition)
  (.slotd-name slotd) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;

;;;; 7.7.35 add-method
;
(defun cl:add-method (gf method)
    (check-type gf     standard-generic-function)
    (check-type method method)

  ;; Update method list in generic-function
  ;;
  (loop
    for scan on (slot-value gf 'methods)
    and prev = nil then scan
    with specializers = (slot-value method 'specializers)
    with qualifiers   = (slot-value method 'qualifiers)
    finally
      (if prev
          (setf (cdr prev) (list method))
        (setf (slot-value gf 'methods) (list method)) )
    do
      (let ((present (first scan)))
        (when (and (equal (slot-value present 'qualifiers)   qualifiers)
                   (equal (slot-value present 'specializers) specializers) )
          (setf (first scan) method)
          (return) ) ))

  (setf (slot-value method 'generic-function) gf)

  ;; Update direct-methods list in class
  ;;
  (loop
    for specializer in (slot-value method 'specializers)
    with qualifiers = (slot-value method 'qualifiers)
    do
      (let ((direct-methods
              (delete-if
                #'(lambda (method)
                    (and (eq (slot-value method 'generic-function) gf)
                         (equal (slot-value method 'qualifiers) qualifiers) ) )
                (slot-value specializer 'direct-methods)
                :count 1 ) ))
        (setf (slot-value specializer 'direct-methods)
              (cons method direct-methods) ) ))


  ;; Update generic-function
  ;;
  (unless (slot-value gf 'param-info)
    (set-generic-function-lambda-list
      gf (slot-value method 'lambda-list) nil ))
  gf )

#|
;;;; 7.7.9  slot-boundp
;;;; 7.7.11 slot-makunbound
;;;; 7.7.14 slot-value
;
(macrolet (
  (define (operation)
    (let* ((fn (intern (format nil "SLOT-~A" operation)))
           (method (intern (format nil "~A-USING-CLASS" fn))) )
      `(defun ,fn (object slot-name)
         (let* ((class (class-of object))
                (slotd (find-slot slot-name class)) )
           (unless slotd
             (slot-missing class object slot-name ',fn) )
           (,method class object slotd) ) ) ) )
  )
  ;;
  (define boundp)
  (define makunbound)
  (define value) )


(defun (setf cl:slot-value) (value object slot-name)
  (let* ((class (class-of object))
         (slotd (find-slot slot-name class)) )
    (unless slotd
      (slot-missing class object slot-name 'setf) )
    (setf (slot-value-using-class class object slotd) value) ) )
|#
