;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - genesis - Functions for CLOS bootstrap.
;;; lisp/genesis/g07-clos-slot.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/genesis/g07-clos-slot.lisp#8 $
;;;
;;; Description:
;;;  Installs slot accessors for MOP classes.
;
(in-package :si)

;;; for equal
(defun cl:pathnamep (x)
  (subclassp (class-of x) '#.(find-class 'pathname)) )


;;;; cl:find-class
;;; For o05-dfun.lisp
(define-compiler-macro cl:find-class (name &optional errorp env)
    (declare (ignore errorp env))
  (when (and (consp name)
             (eql (list-length name) 2)
             (eq (car name) 'quote) )
    (find-class (second name) nil nil) ) )

;;; ext:function-name
(set-funcallable-instance-function #'ext:function-name
    (lambda (fn)
        (declare (lambda-name (:stab function-name)))
    (typecase fn
      (native-code-function
        (ref native-code-function name fn) )
      (funcallable-instance
        (ref standard-generic-function name
            (ref funcallable-instance storage fn) ) )
      (otherwise
        (error 'type-error :datum fn :expected-type 'function) )) ) )

;;; (setf ext:function-name)
(set-funcallable-instance-function #'(setf ext:function-name)
    (lambda (name fn)
        (declare (lambda-name (:stab function-name)))
    (typecase fn
      (native-code-function
        (setf (ref native-code-function name fn) name) )
      (funcallable-instance
        (setf (ref standard-generic-function name
                    (ref funcallable-instance storage fn) )
              name ) )
      (otherwise
        (error 'type-error :datum fn :expected-type 'function) )) ) )



;;; For stream
(defconstant STREAM-FLAG-PROBE     0)
(defconstant STREAM-FLAG-INPUT     1)
(defconstant STREAM-FLAG-OUTPUT    2)
(defconstant STREAM-FLAG-BOTH      3)
(defconstant STREAM-FLAG-DIRMASK   3)

(defconstant STREAM-FLAG-CLOSED         4)
(defconstant STREAM-FLAG-INTERACTIVE    8)

;;;; cl:defclass
(defmacro cl:defclass (class-name (&rest superclass*) (&rest slot-spec*)
                       &rest option* )
  (let ((class (find-class class-name)))
  (labels (
    ;; class-direct-slots
    (class-direct-slots (class)
        (ref standard-class direct-slots
                (ref instance storage class) ) )

    ;; dslotd-name
    (dslotd-name (dslotd)
        (ref standard-direct-slot-definition name
                (ref instance storage dslotd) ) )

    ;; check-slot
    (check-slot (slot-name)
      (if (find slot-name (class-direct-slots class) :key #'dslotd-name)
          slot-name
        (error "Class ~S doesn't have slot ~S. It has ~S."
              class slot-name (class-direct-slots class) )) )

    ;; check-direct-slots
    (check-direct-slots ()
      (dolist (slot-spec slot-spec*)
        (if (consp slot-spec)
            (check-slot (first slot-spec))
          (check-slot slot-spec) ) ) )

    ;; compute-reader
    (compute-reader (name slot-name)
     `(lambda (x)
          (declare (lambda-name (:method ,name nil (,class-name))))
        (let ((class (class-of x)))
          (cond
            ,@(loop
                for subclass in (list-subclasses class nil)
                  when (finalized-p subclass)
                    collect `((eq ,subclass class)
                              (let ((st (ref ,(instance subclass) storage x)))
                                 (ref ,(class-name subclass) ,slot-name st) )))
            (t (slot-value x ',slot-name)) ) ) ) )

    ;; compute-writer
    (compute-writer (name slot-name)
     `(lambda (y x)
          (declare (lambda-name (:method ,name nil (t ,class-name))))
        (let ((class (class-of x)))
          (cond
            ,@(loop
                for subclass in (list-subclasses class nil)
                  when (finalized-p subclass)
                    collect
                     `((eq ,subclass class)
                       (let ((st (ref ,(instance subclass) storage x)))
                        (setf (ref ,(class-name subclass) ,slot-name st) y) )))
            (t (setf (slot-value x ',slot-name) y)) ) ) ) )

    ;; class-name
    (class-name (class)
      (ref standard-class name (ref instance storage class)) )

    ;; finalized-p
    (finalized-p (class)
      (ref standard-class slots (ref instance storage class)) )

    (instance (class)
        (declare (values (member instance funcallable-instance record)))
        (declare (type class class))
      (etypecase class
        (standard-class 'instance)
        (clos:funcallable-standard-class 'funcallable-instance)
        (structure-class 'record) ) )

    ;; list-subclasses
    (list-subclasses (class subclasses)
      (if (member class subclasses)
          subclasses
        (let ((subclasses (cons class subclasses)))
          (dolist (subclass (ref standard-class direct-subclasses
                              (ref instance storage class) )
                            subclasses )
            (setq subclasses (list-subclasses subclass subclasses)) ) )) )

    ;; compute-slot-spec
    (compute-slot-spec (slot-spec)
      `(list ,@(compute-slot-spec-aux slot-spec)) )

    ;; compute-slot-spec-aux
    (compute-slot-spec-aux (slot-spec)
      (if (not (consp slot-spec))
          `((quote ,slot-spec))
        (with-collector (collect)
          (collect `(quote ,(first slot-spec)))
          (loop
            with slot-name = (first slot-spec)
            for (key val) on (rest slot-spec) by #'cddr do
            (collect key)
            (collect `(quote ,val))
            (case key
              ((:initform)
                (collect :initfunction)
                (collect (compute-initfunction val slot-name)) )
              ((:reader)
                (collect :reader-function)
                (collect (compute-reader val slot-name)) )
              ((:writer)
                (collect :writer-function)
                (collect (compute-writer val slot-name)) )
              ((:accessor)
                (collect :reader-function)
                (collect (compute-reader val slot-name))
                (collect :writer-function)
                (collect (compute-writer `(setf ,val) slot-name)) ))) )) )

      ;; compute-initfunction
      (compute-initfunction (val slot-name)
        (if (constantp val)
            `(constantly ,val)
         `(lambda ()
                (declare (lambda-name (:initform ,class-name ,slot-name)))
            ,val )) )
    )
    ;;
    (check-direct-slots)
    `(%defclass ',class-name ',superclass*
        (list ,@(mapcar #'compute-slot-spec slot-spec*))
        ',option* ) ) ) )


;;;; cl:add-method
;;;; clos:add-direct-method
(labels (
  ;; method=
  ;;  Returns true if both methods has same
  ;;    o generic-function
  ;;    o specializers
  ;;    o qualifiers
  (method= (mt1 mt2)
      (declare (type method mt1 mt2))
    (let ((st1 (ref instance storage mt1))
          (st2 (ref instance storage mt2)) )
      (and (eq (ref standard-method generic-function st1)
               (ref standard-method generic-function st2) )
          (equal (ref standard-method specializers st1)
                 (ref standard-method specializers st2) )
          (equal (ref standard-method qualifiers st1)
                 (ref standard-method qualifiers st2) )) ) )

  )

  ;; cl:add-method
  (set-funcallable-instance-function #'cl:add-method
    (lambda (gf mt)
        (declare (lambda-name (:stab add-method)))
        (declare (values standard-generic-function))
        (declare (type standard-generic-function gf))
        (declare (type standard-method mt))

    (let ((present (ref standard-method generic-function
                            (ref instance storage mt) ) ))
      (when present
        (error "~S is a part of ~S." mt present) ) )

    (setf (ref standard-generic-function methods
              (ref funcallable-instance storage gf) )
      (pushnew mt (ref standard-generic-function methods
                      (ref funcallable-instance storage gf) )
               :test #'method= ))

    (dolist (specializer
               (ref standard-method specializers
                  (ref instance storage mt) ))
      (add-direct-method specializer mt) )

    (setf (ref standard-method generic-function (ref instance storage mt))
          gf ) ))

  ;; clos:add-direct-method
  (set-funcallable-instance-function #'clos:add-direct-method
    (lambda (specializer mt)
        (declare (lambda-name (:stab add-direct-method)))
        (declare (values specializer))
        (declare (type standard-generic-function gf))
        (declare (type standard-method mt))
      (pushnew mt (ref standard-class direct-methods
                      (ref instance storage specializer) )
              :test #'method= )
      specializer ))
 ) ; labels


;;;; %defclass
;;; Description:
;;;  This is genesis version of %defclass for classes reside in boot image.
;;;  See clos/o12-class.lisp for full implementation.
;;;
;;;  This function installs readers and writers.
;;;
;;; Assumptions:
;;;   class object and generic-function objects for readers and writers are
;;;   must exist. This is true for classes in boot image.
(defun %defclass (class-name super* slot-spec* option*)
  (let ((class (find-class class-name)))
  (labels (
    ;; ensure-gf
    (ensure-gf (fname)
        (declare (values standard-generic-function))
        (declare (type function-name fname))
      (let ((fn (and (fboundp fname) (fdefinition fname))))
        (if (typep fn 'standard-generic-function)
            fn
          (error "~S must be a generic-function." fname) ) ) )

    ;; install
    (install (fn)
        (declare (type function fn))
      (let* ((fname (second (function-name fn)))
             (gf    (ensure-gf fname)) )
        (add-method gf (make-method gf fn))
        (clos:set-funcallable-instance-function gf fn) ) )

    ;; make-method
    (make-method (gf fn)
        (declare (values standard-method))
        (declare (type standard-generic-function gf))
        (declare (type function fn))
        (declare (ignore gf))
      (let* ((mt (.allocate-instance '#.(class-description 'standard-method)))
             (st (ref instance storage mt))
             (fname (function-name fn)) )
        (setf (ref standard-method plist st) nil)
        (setf (ref standard-method generic-function st) nil)
        (setf (ref standard-method qualifiers st) nil)
        (setf (ref standard-method specializers st)
            (if (writer-p fname)
                (list (find-class 't) class)
              (list class) ))
        (setf (ref standard-method lambda-list st)
            (if (writer-p fname) '(y x) '(x)) )
        (setf (ref standard-method function st) fn)
        mt ) )

    ;; find-slot-spec
    (find-slot-spec (slot-name)
      (dolist (slot-spec slot-spec*)
        (etypecase slot-spec
          (symbol
            (when (eq slot-spec slot-name) (return nil)) )
          (cons
            (when (eq (first slot-spec) slot-name)
              (return (cdr slot-spec) )) )) ) )

    ;; hack-slots
    ;;  o set initarg
    ;;  o set initform and initfunction
    ;;  o set readers
    ;;  o set writers
    (hack-slots ()
      (let ((st (ref instance storage class)))
        (hack-dslots (ref standard-class direct-slots st))
        (hack-eslots (ref standard-class slots st)) ) )

    ;; hack-dslots
    (hack-dslots (slots)
      (loop
        for dslotd in slots
        for st = (ref instance storage dslotd)
        for slot-name = (ref standard-direct-slot-definition name st) do
        (loop for (key val) on (find-slot-spec slot-name) by #'cddr do
          (case key
            ((:reader)
              (pushnew val
                (ref standard-direct-slot-definition readers st) ) )
            ((:writer)
              (pushnew val
                (ref standard-direct-slot-definition writers st) ) )
            ((:accessor)
              (pushnew val
                (ref standard-direct-slot-definition readers st) )
              (pushnew `(setf ,val)
                (ref standard-direct-slot-definition writers st)
                :test #'equal ) )
            ((:initarg)
              (pushnew val
                  (ref standard-direct-slot-definition initargs st) ))
            ((:initform)
              (setf (ref standard-direct-slot-definition initform st) val) )
            ((:initfunction)
              (setf (ref standard-direct-slot-definition initfunction st)
                val ) )
            ((:reader-function :writer-function)
              (install val) )))) )

    ;; hack-eslots
    ;;  o Set initform and initfunction from the first one
    ;;  o Set initargs from all direct slots in CPL.
    (hack-eslots (slots)
      (loop
        with cpl = (ref standard-class class-precedence-list
                        (ref instance storage class) )
        for eslotd in slots
        for st = (ref instance storage eslotd)
        for slot-name = (ref standard-effective-slot-definition name st)
        for initform-p = nil do
          (setf (ref standard-effective-slot-definition initargs st) '())

          (dolist (super cpl)
            (let ((dslotd (find slot-name
                                (ref standard-class direct-slots
                                        (ref instance storage super) )
                                :key (lambda (dslotd)
                                    (ref standard-direct-slot-definition name
                                        (ref instance storage dslotd) ))) ))
              (when dslotd
                (unless initform-p
                  (setq initform-p (inherit-initform eslotd dslotd)) )
                (inherit-initargs eslotd dslotd) ) ) )) )


    ;; inherit-initargs
    (inherit-initargs (eslotd dslotd)
      (dolist (initarg (ref standard-direct-slot-definition initargs
                              (ref instance storage dslotd) ))
        (pushnew initarg
                 (ref standard-effective-slot-definition initargs
                        (ref instance storage eslotd) )) ) )

    ;; inherit-initform
    (inherit-initform (eslotd dslotd)
      (multiple-value-bind (initform initfunction)
          (let ((st (ref instance storage dslotd)))
            (values
              (ref standard-direct-slot-definition initform st)
              (ref standard-direct-slot-definition initfunction st) ) )
        (when initfunction
          (setf (ref standard-effective-slot-definition initform
                        (ref instance storage eslotd) )
                initform )
          (setf (ref standard-effective-slot-definition initfunction
                        (ref instance storage eslotd) )
                initfunction )) ) )

    ;; check-metaclass
    (check-metaclass ()
      (unless (eq (find-class (or (second (assoc :metaclass option*))
                                  'standard-class ))
                  (class-of class) )
        (error "~S has incompatible metaclass. ~S" class) ) )

    ;; check-slots
    #+nil
    (check-slots ()
      (let ((present
              (mapcar (lambda (dslotd)
                        (ref standard-direct-slot-definition name
                            (ref instance storage dslotd) ) )
                      (ref standard-class direct-slots
                            (ref instance storage class) )) )
            (specified
              (mapcar (lambda (x) (if (consp x) (first x) x)) slot-spec*) ))
        (unless (equal present specified)
          (error "~S has incompatible direct slots~%present=~S~%specified=~S"
            class present specified ) ) ) )

    ;; check-supers
    (check-supers ()
      (let ((expect
              (or super*
                  (etypecase (class-of class)
                    (standard-class '(standard-object))
                    (funcallable-standard-class '(funcallable-standard-object))
                    (structure-class '(structure-object)) )) )
            (present
              (mapcar (lambda (x)
                        (ref standard-class name (ref instance storage x)) )
                      (ref standard-class direct-superclasses
                                (ref instance storage class) ))) )
        (unless (equal expect present)
          (error "~S has incompatible super classes. expect=~S present=~S"
              class expect present ) ) ) )

    ;; writer-p
    ;;  method-function name is (:METHOD gf-name (qualifier*) (specializer+))
    (writer-p (fname)
      (cdr (fourth fname)) )
    )
    ;;
    (when *load-print* (format t "; Import ~S~%" class))
    (check-supers)
    #+nil (check-slots)
    (check-metaclass)
    (hack-slots) ) ) )
