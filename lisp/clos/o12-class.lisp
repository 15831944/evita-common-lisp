;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - Class
;;; lisp/clos/o12-class.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o12-class.lisp#5 $
;;;
;;; Description:
;;;  This file contains class related methods and functions.
;;;
;;; Local Functions:
;;;     default-initargs
;;;     ensure-class-initargs
;;;     update-obsolete-instance
;;;     validate-initargs
;;;
;;; Internal
;;;     %defclass
;;;
;;; MOP
;;;     add-dependent
;;;     add-direct-method
;;;     add-direct-subclass
;;;     class-default-initargs
;;;     class-direct-default-initargs
;;;     class-finalized-p
;;;     class-prototype
;;;     class-precedence-list
;;;     class-slots
;;;     compute-class-precedence-list
;;;     compute-default-initargs
;;;     compute-effective-slot-definition
;;;     compute-slots
;;;     direct-slot-definition-class
;;;     effective-slot-definition-class
;;;     ensure-class
;;;     ensure-class-using-class (class)
;;;     ensure-class-using-class (null)
;;;     finalize-inheritance
;;;     reader-method-class
;;;     remove-dependent
;;;     remove-direct-method
;;;     slot-value-using-class
;;;     specializer-direct-generic-functions
;;;     validate-superclass
;;;     write-method-class
;;;
;;; Public Functions:
;;;     allocate-instance (standard-class)                      7.7.3
;;;     change-class (standard-object standard-class)           7.7.8
;;;     change-class (t symbol)                                 7.7.8
;;;     (setf class-name) (t class)                             7.7.38
;;;     make-instance (class)                                   7.7.19
;;;     make-instance (symbol)                                  7.7.19
;;;     make-instances-obsolete (class)                         7.7.20
;;;     make-instances-obsolete (symbol)                        7.7.20
;;;     reinitialize-instance   :before (class)                 7.7.4
;;;     shared-initialize       :after (standard-class)         7.7.5
;;;     update-instance-for-different-class                     7.7.6
;;;     update-instance-for-redefined-class (standard-class)    7.7.7
;
(in-package :si)

;;;; add-slot-reader
;;;
;;; Called by:
;;;   shared-initialize :after (standard-class)
;;;
;;; Description:
;;;  Makes reader method and adds it to generic-function. Method funciton
;;;  created this method won't be called when generic-function will have
;;;  special discriminator.
;
(defun add-slot-reader (class fname slotd)
  (let ((gf (ensure-generic-function fname))
        (fn
          (let ((slot-name (slot-definition-name slotd)))
            #'(lambda (object)
                (slot-value object slot-name) ) ) ))

    (setf (ext:function-name fn)
          (list 'standard-reader-method fname) )

    (add-method gf
                (make-instance 'standard-reader-method
                               :lambda-list     '(object)
                               :specializers    (list class)
                               :slot-definition slotd
                               :function        fn )) ) )


;;;; add-slot-writer
;;;
;;; Called by:
;;;   shared-initialize :after (standard-class)
;
(defun add-slot-writer (class fname slotd)
  (let ((gf (ensure-generic-function fname))
        (fn
          (let ((slot-name (slot-definition-name slotd)))
            #'(lambda (value object)
                (setf (slot-value object slot-name) value) ) ) ))

    (setf (ext:function-name fn)
          (list 'standard-writer-method fname) )

    (add-method gf
                (make-instance 'standard-writer-method
                               :lambda-list     '(value object)
                               :specializers    (list (find-class 't) class)
                               :slot-definition slotd
                               :function        fn )) ) )


;;;; copy-instance-local-slot
;;; Returns:
;;;     discard - A boolean.
;;;     boundp  - A boolean. True if src slot is bound.
;;;     value   - An object. Slot value of src.
;;;
;;; Description:
;;;  This is helper function for change-class and update-obsolte-instance.
(defun copy-instance-local-slot (dst src src-eslotd)
  (labels (
    ;; find-eslotd
    (find-eslotd (name slots)
      (find name slots :key #'slot-definition-name) )
    )
    ;;
    (if (not (eq (slot-definition-allocation src-eslotd) :instance))
        (values nil nil nil nil)
      (let* ((name       (slot-definition-name src-eslotd))
             (src-class  (class-of src))
             (boundp     (slot-boundp-using-class src-class src src-eslotd))
             (src-val
               (when boundp
                 (slot-value-using-class src-class src src-eslotd) ) )
             (dst-class   (class-of dst))
             (dst-slots   (class-slots dst-class))
             (dst-eslotd  (find-eslotd name dst-slots)) )
         (cond
          ((null dst-eslotd)
            (values t boundp src-val) )
          ((not (eq (slot-definition-allocation dst-eslotd) :instance))
            (values t boundp src-val) )
          (boundp
            (setf (slot-value-using-class dst-class dst dst-eslotd) src-val)
            (values nil t src-val) )
          (t
            (values nil nil src-val) )) )) ) )


;;;; default-initargs
;;;
;;; Called by:
;;;  make-instance class
;
(defmethod default-initargs ((class class) initargs class-default-initargs)
  (let ((unspecified '(unspecified)))
    (dolist (name-form-fun class-default-initargs)
      (when (eq (getf initargs (first name-form-fun) unspecified) unspecified)
        (push (if (functionp (third name-form-fun))
                  (funcall (third name-form-fun))
                (second name-form-fun) )
              initargs )
        (push (first name-form-fun) initargs) ) )
    initargs ) )


;;;; ensure-class-initargs
;;;
;;; Syntax:
;;;     ensure-class-initargs class initargs => metaclass, new-initargs
;;;
;;; Arguments and Values:
;;;  class           a class or nil.
;;;  initargs        a list of initargs.
;;;  metaclass       a class.
;;;  new-initargs
;;     a list of initargs, which contains :direct-superclasses and
;;;    :direct-slots but :metaclass.
;;;
;;; Called by:
;;;     ensure-class-using-class
;;;
;;; Description:
;;;  Computes initargs for class and returns it.
;
(defun ensure-class-initargs (class initargs)
  (let* ((initargs    (copy-list initargs))
         (unspecified (list 1))
         (metaclass
           (getf initargs :metaclass unspecified) )
         (direct-superclasses
           (getf initargs :direct-superclasses unspecified) )
         (direct-slots
           (getf initargs :direct-slots unspecified) )
         (env
           (getf initargs :environment) ))

    ;; ensure metaclass
    ;;
    (cond
     ((not (eq metaclass unspecified))
      (setq metaclass (find-class metaclass)) )
     ((or (null class) (typep class 'forward-referenced-class))
      (setq metaclass (find-class 'standard-class)) )
     (t
      (setq metaclass (class-of class)) ))

    ;; tailor initargs
    (loop (unless (remf initargs :metaclass) (return)))
    (loop (unless (remf initargs :direct-superclasses) (return)))
    (loop (unless (remf initargs :direct-slots) (return)))

    ;; return values
    (flet (
        (ensure-superclass (name)
          (typecase name
            (class name)
            (symbol
              (or (find-class name nil env)
                  (setf (find-class name nil env)
                        (make-instance 'forward-referenced-class
                                       :name name ))) )
            (otherwise
              (error 'invalid-class-name :datum name) )) )
        )
        ;;
        (values metaclass
               (list* :direct-superclasses
                      (and (not (eq direct-superclasses unspecified))
                           (mapcar #'ensure-superclass direct-superclasses) )
                     :direct-slots
                     (and (not (eq direct-slots unspecified)) direct-slots)
                     initargs )) ) ) )


;;;; validate-initargs
;;;
;;; Called by:
;;;     make-instance class
;;;
;;; Description:
;;;  Validates specified initargs for calling of generic-functions specified
;;;  in call-list. When one of following condition mets, validation isn't
;;;  occured:
;;;     1. One of applicable methods has &allow-other-keys.
;;;     2. Specified initargs has :allow-other-keys with non-nil value.
;;;     3. Specified initargs is empty list.
;
(defun validate-initargs (class initargs call-list)
    (declare (type class class))
    (declare (type list  initargs))
    (declare (type list  call-list))
    (declare (values ext:unspecified))
  (when (or (null initargs) (getf initargs :allow-other-keys))
    (return-from validate-initargs) )

  (let ((keys
          (loop for slotd in (class-slots class)
            append (slot-definition-initargs slotd) ) ))

    ;; Collect keyword names.
    ;;
    (dolist (call call-list)
      (dolist (method (compute-applicable-methods (car call) (cdr call)))
        (multiple-value-bind (keywords allow-other-keys-p)
            (function-keywords method)
          (when allow-other-keys-p
            (return-from validate-initargs) )
          (dolist (keyword keywords)
            (pushnew keyword keys :test #'eq) ) ) ) )

    ;; Validate initargs.
    ;;
    (loop
      for scan = initargs then (rest scan)
      while scan do
        (let ((key (pop scan)))
          (unless (member key keys :test #'eq)
            (if (symbolp key)
                (error 'unrecognized-initialization-argument
                       :class class
                       :keys  keys
                       :key   key )
              (error 'invalid-initialization-argument
                       :class class
                       :key   key )))
          (when (null scan)
             (error 'odd-number-of-keyword-arguments
                    :arguments initargs )) )) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Internal Functions
;;;;

;;;; %defclass
;;;
;;; Description:
;;;  This function is called in expansion of defclass macro.
;
(defun %defclass (class-name direct-super-class-names direct-slot-specs
                      &rest initargs )
    (declare (values class))
    (declare (dynamic-extent initargs))
  (apply #'ensure-class
         class-name
         :direct-superclasses direct-super-class-names
         :direct-slots        direct-slot-specs
         initargs ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; MOP Functions
;;;;

;;;; MOP add-dependent
;
(defmethod clos:add-dependent ((dependee dependee-mixin) dependent)
  (pushnew dependent
           (getf (slot-value dependee 'plist) 'dependents)
           :test #'eq ) )


;;;; MOP add-direct-method
;;;
;;; Called by:
;;;     add-method
;
(defmethod clos:add-direct-method ((class specializer) method)
  (loop
    with methods = (slot-value class 'direct-methods)
    for present in methods
    finally
      (setf (slot-value class 'direct-methods) (cons method methods))
      (return t)
    when (eq method present)
      return nil ) )


;;;; MOP add-direct-subclass
;;;
;;; Called by:
;;;  shared-initialize
;;;
;;; See Also: remove-direct-subclass
;;;
;
(defmethod clos:add-direct-subclass ((superclass class) (subclass class))
  (let ((subclasses (slot-value superclass 'direct-subclasses)))
    (pushnew subclass subclasses :test #'eq)
    (setf (slot-value superclass 'direct-subclasses) subclasses) ) )


;;;; MOP class-default-initargs
;
(defmethod clos:class-default-initargs ((class class))
  (compute-default-initargs class) )


;;;; MOP class-direct-default-initargs
;
(defmethod clos:class-direct-default-initargs ((class class))
  (getf (slot-value class 'plist) 'direct-default-initargs) )


;;;; MOP class-finalized-p
;
(defmethod clos:class-finalized-p ((class class))
  (not (null (slot-value class 'instance-description))) )


;;;; MOP class-precedence-list
;
(defmethod class-precedence-list ((class class))
  (unless (class-finalized-p class)
    (error 'class-not-finalized :class class) )
  (slot-value class 'class-precedence-list) )


;;;; MOP class-prototype
;
(defmethod clos:class-prototype ((class class))
  (unless (class-finalized-p class)
    (error 'class-not-finalized :class class) )
  (let ((instance (slot-value class 'prototype)))
    (unless instance
      (setq instance (allocate-instance class))
      (setf (slot-value class 'prototype) instance) )
    instance ) )


;;;; MOP class-slots
;
(defmethod clos:class-slots ((class class))
  (unless (class-finalized-p class)
    (error 'class-not-finalized :class class) )
  (slot-value class 'slots) )


;;;; COMPUTE CLASS PRECEDENCE LIST method.
;;;
;;; R represents total oreder of classes and there superclasses.
;;;   R = { (C1, S1), (C2, S2) ... }
;;;     where Ci is more precenence Si. (Si is superclass of Ci.)
;;;
;;; Sc represents set of superclasses.
;;;
;
;;;; MOP compute-class-precedence-list
;;;
;;; Called by:
;;;     finalize-inheritance
;;;
(defmethod clos:compute-class-precedence-list ((class class))
  (let ((R  '())
        (Sc '()) )
    ;; Compute R and Sc
    ;;
    (labels (
      (compute-local-precedence-order (class-i)
        (pushnew class-i Sc :test #'eq)
        (unless (eq (find-class 't) class-i)
          (dolist (super (class-direct-superclasses class-i))
            (when (typep super 'forward-referenced-class)
              (error-forward-referenced-class super) )
            (push (cons class-i super) R)
            (compute-local-precedence-order super)
            (setq class-i super) )) )

      ;; error-forward-referenced-class
      (error-forward-referenced-class (forward-class)
        (error 'forward-referenced-class-error
               :forward-referenced-class forward-class
               :class                    class ) )
      )
      ;;
      (compute-local-precedence-order class) )

    ;; Topological Sorting S with R.
    ;;
    (labels (
      ;; error-inconsistent-cpl
      ;;
      (error-inconsistent-cpl (Sc R)
        (error 'inconsistent-class-precedence-list
               :superclasses Sc
               :total-order  R ) )

     ;; find-least-specific-class
     ;;
     ;; Description:
     ;;  There are several classes from Sc with no predecessors.
     ;;  Choose direct super classes for least specific class in
     ;;  CPL constructed so far.
     ;;
     (find-least-specific-class (rev-cpl N*)
       (let ((C nil))
         (dolist (Cj rev-cpl)
          (dolist (Ni N*)
             (when (member Ni (class-direct-superclasses Cj) :test #'eq)
               (setq C Ni) ))
           (when C (return)) )
         C ) )

      ;; list-precedence-classes
      ;;
      ;; Description:
      ;;  Finds precedence class C's in Sc with R.
      ;;
      (list-precedence-classes (Sc R)
        (let ((C* '()))
          (dolist (Ci Sc C*)
            (dolist (Ri R (push Ci C*))
              (when (eq Ci (cdr Ri))
                 (return) ) ) ) ) )
      )
      ;;
      (let ((rev-cpl '()))
        (loop
          (when (null Sc)
            (return (nreverse rev-cpl)) )

          (let* ((N* (list-precedence-classes Sc R))
                 (C
                  (cond
                    ((null N*)        (error-inconsistent-cpl Sc R) )
                    ((null (rest N*)) (first N*) )
                    (t (find-least-specific-class rev-cpl N*)) ) ))

            (push C rev-cpl)
            (setq Sc (delete C Sc :test #'eq))

            ;; Remove all paris of the form (C, D), D in Sc, from R.
            (setq R (delete C R :test #'eq :key #'car)) )) ) ) ) )


;;;; MOP compute-default-initargs
;;;
;;; Called by:
;;;   finalize-inheritance
;
(defmethod clos:compute-default-initargs ((class class))
  (loop
    with classes = (class-precedence-list class)
    with initargs =
      (copy-list (class-direct-default-initargs (first classes)))
    for class in (rest classes) do
      (loop
        for frob in (class-direct-default-initargs class)
        for name = (car frob) do
          (unless (find name initargs :key #'car :test #'eq)
            (push frob initargs) ))
    finally (return initargs) ) )


;;;; MOP compute-effective-slot-definition
;;;
;;; Syntax:
;;;     compute-effective-slot-definition class dslotds => slotd
;;;
;;; Arguments and Values:
;;;     class   A class.
;;;     dslotds A list of direct slot defintion order by most-specific first.
;;;
;;; Description:
;;;  Computes initargs from list of direct-slot defintions for
;;;  effective-slot-definition-class then calls
;;;  make-instance with that initargs. Initargs are merged amond dslotds
;;;  by following rules:
;;;   :name             The first one. All dslotds must have same name.
;;;   :allocation       The first one.
;;;   :initargs         Union of all dslotds.
;;;   :initform         The leftmost one.
;;;   :initfunction     The leftmost one.
;;;   :type             Intersection of all dslotds.
;;;
;
(defmethod clos:compute-effective-slot-definition((class class) name dslotds)
    (assert (and (consp dslotds)
                 (typep (first dslotds) 'direct-slot-definition) ))
  (let ((plist
           (let ((dslotd (first dslotds)))
             (list :name       name
                   :allocation (slot-value dslotd 'allocation) ) ) ))
    (loop
      for dslotd in dslotds
      with initargs    = '()
      with type         = 't
      with initform     = nil
      with initfunction = nil
      finally
        (when initfunction
          (setf (getf plist :initfunction) initfunction)
          (setf (getf plist :initform)     initform) )
        (setf (getf plist :initargs) initargs)
        (setf (getf plist :type)     type)
      do
        (unless initfunction
          (let ((initfn (slot-value dslotd 'initfunction)))
            (when initfn
              (setq initfunction initfn)
              (setq initform     (slot-value dslotd 'initform)) ) ))

        (loop for initarg in (slot-value dslotd 'initargs) do
          (pushnew initarg initargs :test #'eq) )

        (setq type (slot-value dslotd 'type))
        #+nil
        (let ((dtype (slot-value dslotd 'type)))
          (when (subtypep dtype type)
            (setq type dtype) ) ))

    (apply #'make-instance
          (effective-slot-definition-class class) plist ) ) )


;;;; MOP compute-slots standard-class
;;;
;;; Description:
;;;  Makes list effective slot definition and returns it.
;;;
;;; Note: We don't use around method for assigning instance slot location.
;;; ACL and LispWorks also don't use it.
;
(defmethod clos:compute-slots ((class standard-base-class))
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
      with class-slot-cells =
        (getf (slot-value class 'plist) 'class-slot-cells)
      for (name . slotds) in slotd-alist
      for slotd = (compute-effective-slot-definition class name slotds)
      do
        (etypecase (slot-value slotd 'allocation)
          ((eql :instance)
            (setf (slot-value slotd 'location) location)
            (incf location) )
          (class
            (setf (slot-value slotd 'location)
                  (getf class-slot-cells name) )) )
      collect slotd ) ) )


;;;; MOP direct-slot-definition-class
;;;
;;; Called by:
;;;  shared-initialize :after (standard-class)
;
(defmethod clos:direct-slot-definition-class
        ((class standard-base-class) &rest initargs)
    (declare (dynamic-extent initargs))
    (declare (ignore initargs))
  (find-class 'standard-direct-slot-definition) )


;;;; MOP effective-slot-definition-class
;
(defmethod clos:effective-slot-definition-class
        ((class standard-base-class) &rest initargs)
    (declare (dynamic-extent initargs))
    (declare (ignore initargs))
  (find-class 'standard-effective-slot-definition) )


;;;; MOP ensure-class
;
(defun clos:ensure-class (name
                          &rest args
                          &key ((:environment env))
                          &allow-other-keys )
    (declare (dynamic-extent args))
  (labels (
    (find-class-in-env (name)
      (let ((class (find-class name nil)))
        (if (null env)
            class
          (let ((class2 (find-class name nil env)))
             (if (eq class class2) nil class2) )) ) )
    )
    ;;
   (apply #'ensure-class-using-class (find-class-in-env name) name args) ) )


;;;; MOP ensure-class-using-class class
;
(defmethod clos:ensure-class-using-class ((class class) name &rest args)
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


;;;; MOP ensure-class-using-class null
;
(defmethod clos:ensure-class-using-class
        ((class null) name &rest args
         &key ((:environment env))
         &allow-other-keys )
  (multiple-value-bind (metaclass initargs)
      (ensure-class-initargs class args)
    ;; Make name as type specifier for slot definitions.
    ;; (install-class-as-type (class-prototype metaclass) name)
    (setq class (apply #'make-instance metaclass :name name initargs))
    (setf (find-class name nil env) class)
    ;; (install-class-as-type class name)
    class ) )


;;;; MOP finalize-inheritnace
;;;
;;; Called by:
;;;     make-instance (class)
;
(defmethod clos:finalize-inheritance ((class standard-base-class))
    (declare (values unspecified))
  (setf (slot-value class 'class-precedence-list)
        (compute-class-precedence-list class) )

  (setf (slot-value class 'slots) (compute-slots class))

  (if (slot-value class 'instance-description)
      (make-instances-obsolete class)
    (setf (slot-value class 'instance-description)
      (make-class-description class (slot-value class 'slots)) ))

  (setf (slot-value class 'prototype) nil)

  (labels (
    (update-accessors (class)
      (dolist (method (slot-value class 'direct-methods))
        (when (or (typep method 'standard-reader-method)
                  (typep method 'standard-writer-method) )
          (update-discriminator (slot-value method 'generic-function)) ) )

      (dolist (subclass (slot-value class 'direct-subclasses))
        (update-accessors subclass) ) )
    )
    ;;
    (update-accessors class) ) )


;;;; MOP reader-method-class
;;;
;;; See Also: writer-method-class
;;;
;;; Note: MOP describes this method for fsc. When we implement fin slot
;;; reader optimization, we'll implement that.
;;;
;
(defmethod clos:reader-method-class
        ((class standard-base-class)
         (slotd standard-direct-slot-definition)
         &rest initargs )
    (declare (dynamic-extent initargs))
    (declare (ignore initargs))
  (find-class 'standard-reader-method) )


;;;; MOP remove-dependent
;
(defmethod clos:remove-dependent ((dependee dependee-mixin) dependent)
  (setf (getf (slot-value dependee 'plist) 'dependents)
        (delete dependent (slot-value dependee 'plist)
                :test #'eq
                :count 1 )) )


;;;; MOP remove-direct-method
;;;
;;; Called by:
;;;   add-method
;;;   remove-method
;
(defmethod clos:remove-direct-method ((class specializer) method)
  (loop
    with methods = (slot-value class 'direct-methods)
    for scan on methods
    and prev = nil then scan
    finally
      (return nil)
    do
      (when (eq method (first scan))
        (if prev
            (setf (cdr prev) (cdr scan))
         (setf (slot-value class 'direct-methods) (cdr scan)) )
      (return t) )) )


;;;; MOP remove-direct-subclass
;;;
;;; Called by:
;;;  reinitialize-instance
;;;
;;; See Also: add-direct-subclass
;;;
;
(defmethod clos:remove-direct-subclass ((superclass class) (subclass class))
  (setf (slot-value superclass 'direct-subclasses)
        (delete subclass (slot-value superclass 'direct-subclasses)
                :test #'eq
                :count 1 )) )


;;;; MOP specializer-direct-generic-functions
;
(defmethod clos:specializer-direct-generic-functions
            ((specializer specializer))
  (mapcar #'method-generic-function
          (slot-value specializer 'direct-methods) ) )


;;;; MOP validate-superclass
;;;
;;; Description:
;;;  Returns true when one of following conditions:
;;;     1. Superclass argument is the class named t
;;;     2. Class of the class argument is the same as the class of the
;;;        superclass.
;;;     3. Classes one of the arguments is standard-class and the class of the
;;;        other is funcallable-standard-class.
;
(defmethod clos:validate-superclass ((class class) (new-super class))
  (or (eq (find-class 't) new-super)
      (eq (class-of new-super) (class-of class)) ) )


(defmethod clos:validate-superclass
            ((class standard-class) (new-super standard-base-class))
  (let ((new-super-meta-class (class-of new-super)))
    (or (eq (find-class 'standard-base-class) new-super-meta-class)
        (eq (class-of class) new-super-meta-class) ) ) )


(defmethod clos:validate-superclass
                ((class     funcallable-standard-class)
                 (new-super standard-base-class) )
  (let ((new-super-meta-class (class-of new-super)))
    (or (eq (find-class 'standard-base-class) new-super-meta-class)
        (eq (class-of class) new-super-meta-class) ) ) )


(defmethod clos:validate-superclass
        ((class class) (new-super built-in-class))
  (eq (find-class 't) new-super) )


;;;; MOP writer-method-class
;;;
;;; See Also: reader-method-class
;;;
;;; Note: MOP describes this method for fsc. When we implement fin slot
;;; writer optimization, we'll implement that.
;;;
;
(defmethod clos:writer-method-class
        ((class standard-base-class)
         (slotd standard-direct-slot-definition)
         &rest initargs )
    (declare (dynamic-extent initargs))
    (declare (ignore initargs))
  (find-class 'standard-writer-method) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;

;;;; 7.7.3 allocate-instance (standard-class)
;
(defmethod cl:allocate-instance ((class standard-class) &rest initargs)
    (declare (dynamic-extent initargs))
    (declare (ignore initargs))
  (unless (class-finalized-p class) (finalize-inheritance class))
  (let ((classd (slot-value class 'instance-description)))
    (.allocate-instance classd) ) )


;;;; 7.7.8 change-class (standard-object standard-class)
(defmethod cl:change-class
        ((instance standard-object)
         (new-class standard-class) )
  (labels (
    ;; process
    (process ()
      (when (obsolete-instance-p instance)
        (update-obsolete-instance instance) )

      (unless (class-finalized-p new-class)
        (finalize-inheritance new-class) )

      (loop
        with cur = instance
        with new  = (allocate-instance new-class)
        with old-class = (class-of instance)
        for old-eslotd in (class-slots old-class) do
         (copy-instance-local-slot new cur old-eslotd)
        finally
          (swap-instance-layout cur new)
          (let ((cur new) (new cur))
            (update-instance-for-different-class cur new)  ) ) )
    )
  (let ((old-class (class-of instance)))
    (unless (eq old-class new-class) (process)) )
  instance ) )


;;;; change-class standard-object funcallable-standard-class
;
(defmethod cl:change-class
        ((instance standard-object)
         (new-class funcallable-standard-class)
         &rest initargs )
    (declare (ignore initargs))
  (error 'can-not-change-class :instance instance :class new-class) )


;;;; change-class standard-object funcallable-standard-class
;
(defmethod cl:change-class
        ((instance funcallable-standard-object)
         (new-class standard-class)
         &rest initargs )
    (declare (ignore initargs))
  (error 'can-not-change-class :instance instance :class new-class) )


;;;; 7.7.8 change-class (t symbol)
;
(defmethod cl:change-class (instance (new-class symbol))
  (change-class instance (find-class new-class)) )


;;;; 7.7.19 make-instance (class)
;;;
;
(defmethod make-instance ((class class) &rest initargs)
    (declare (dynamic-extent initargs))
  (unless (class-finalized-p class)
    (finalize-inheritance class) )

  (let ((class-default-initargs (class-default-initargs class)))
    (when class-default-initargs
      (setq initargs
        (default-initargs class initargs class-default-initargs) )) )

  (let ((prototype (class-prototype class)))
    (validate-initargs class initargs
                       (list (list #'make-instance class)
                             (list #'allocate-instance   prototype)
                             (list #'initialize-instance prototype)
                             (list #'shared-initialize   prototype t) )) )

  ;; Make sure we return newly created instance instead of return value(s)
  ;; of initialize-instance.
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance ) )


;;;; 7.7.19 make-instance (symbol)
;
(defmethod make-instance ((class symbol) &rest initargs)
    (declare (dynamic-extent initargs))
  (apply #'make-instance (find-class class) initargs) )


;;;; 7.7.19 make-instances-obsolete (standard-class)
;;;
;;; Called by:
;;;  finalize-inheritance
;
(defmethod make-instances-obsolete ((class standard-base-class))
  (progn ; without-interrupt (:fast t)
    (let ((old-classd (slot-value class 'instance-description)))
      (when old-classd
        (setf (ref classd hash-code old-classd) 0)

        (let ((slots (slot-value class 'slots)))
          (setf (slot-value class 'instance-description)
            (make-class-description class slots) ) )) ) )
  class )


;;;; 7.7.19 make-instances-obsolete (symbol)
;
(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class)) )


;;;; 7.7.4 reinitialize-instance :before class
;;;
;;; BUGBUG: NYI: invalidate method cache.
;;; BUGBUG: NYI: update instance-description.
;;;
;;; Note: readers and writers can be unbound.
;
(defmethod reinitialize-instance :before ((class class) &rest initargs)
    (declare (ignore initargs))

  ;; Remove super/sub link
  (dolist (superclass (slot-value class 'direct-superclasses))
    (remove-direct-subclass superclass class) )

  ;; Remove slot accessors
  (let ((specializers (list (find-class 't) class)))
    (dolist (dslotd (slot-value class 'direct-slots))
      (dolist (reader (slot-value dslotd 'readers))
        (let* ((gf
                 (and (fboundp reader) (fdefinition reader)) )
               (method
                 (and (typep gf 'generic-function)
                      (find-method gf nil (rest specializers) nil) ) ))
          (when method (remove-method gf method)) ) )

      (dolist (writer (slot-value dslotd 'writers))
        (let* ((gf
                 (and (fboundp writer) (fdefinition writer)) )
               (method
                 (and (typep gf 'generic-function)
                      (find-method gf nil specializers nil) ) ))
          (when method (remove-method gf method)) ) ) ) ) )


;;;; 7.7.4 reinitialize-instance :after class
;;;
;
(defmethod reinitialize-instance :after ((class class) &rest initargs)
  (map-dependents
        class
        (lambda (dependent)
            (apply #'update-dependent class dependent initargs) )) )


;;;; 7.7.5 shared-initialize standard-class
;;;
;;; Description:
;;;  Updates class slots as follows:
;;;    direct-superclass
;;;     Validates direct-superclasses argument then sets it. When it is
;;;     nil, sets standard-object as direct-superclass.
;;;    direct-slots
;;;     Makes standard-direct-slot-definition objects by argument. Shared
;;;     slots are created and initialized.
;;;   direct-default-initargs
;;;     Set an argument.
;;;
;;;  Re-initialization of class object must be propagated to its descendants
;;;  in order to update instance layout and new class hierarchy of descendant
;;;  classes.
;
(defmethod cl:shared-initialize :after
          ((class standard-base-class)
           slot-names
           &key (direct-superclasses
                    (class-direct-superclasses class) )
                (direct-slots
                    '() direct-slots-p )
                (direct-default-initargs
                    (class-direct-default-initargs class) )
                ;documentation
           &allow-other-keys )
    (declare (ignore slot-names))

  ;; Direct Superclasses
  (when (null direct-superclasses)
    (setq direct-superclasses
        (list (if (typep class 'funcallable-standard-class)
                  (find-class 'funcallable-standard-object)
                (find-class 'standard-object) ))))

  (dolist (superclass direct-superclasses)
    (unless (validate-superclass class superclass)
      (error 'incompatible-superclass
             :class class
             :superclass superclass )) )
  (setf (slot-value class 'direct-superclasses) direct-superclasses)

  ;; Link super/sub class.
  (dolist (superclass direct-superclasses)
    (add-direct-subclass superclass class) )

  ;; Direct Slots
  ;; BUGBUG: NYI: How do we handle slot documentation?
  (when direct-slots-p
    (loop
      with slotd-class = (direct-slot-definition-class class)
      with class-plist = (slot-value class 'plist)
      with cells       = (getf class-plist 'class-slot-cells)
      for initargs in direct-slots
      for slotd = (apply #'make-instance slotd-class initargs)

      finally
        (setf (slot-value class 'direct-slots) slotds)
        (when class-slot-cells
          (setf (getf (slot-value class 'plist) 'class-slot-cells)
                class-slot-cells ))

      collect slotd into slotds

      do
        (loop for reader in (getf initargs :readers) do
          (add-slot-reader class reader slotd) )

      do
        (loop for writer in (getf initargs :writers) do
          (add-slot-writer class writer slotd) )

      when (eq :class (getf initargs :allocation))
        collect
          (let* ((slot-name (getf initargs :name))
                 (cell      (getf cells slot-name)) )
            (or cell
                (cons slot-name
                      (let ((fn (getf initargs :initfunction)))
                        (if (not fn) #.(unbound-marker) (funcall fn)) ))) )
          into class-slot-cells ))

  (if (null direct-default-initargs)
      (remf (slot-value class 'plist) 'direct-default-initargs)
     (setf (getf (slot-value class 'plist) 'direct-default-initargs)
           direct-default-initargs ) )

  ;; Propagate class reinitialization.
  (labels (
    (update-class (class)
      (when (class-finalized-p class)
        (finalize-inheritance class) )

      (dolist (subclass (class-direct-subclasses class))
        (update-class subclass) ) )
    )
    (update-class class) ) )


;;;; 7.7.6 update-instance-for-different-class
;;;
;;; Description:
;;;  Calls shared-initialize with list of added instance slot names.
;
(defmethod cl:update-instance-for-different-class
        ((previous standard-object)
         (current  standard-object)
         &rest initargs )
    (declare (dynamic-extent initargs))
  (let* ((current-class (class-of current))
         (added-slots (compute-added-slots current previous)) )
    (validate-initargs current-class initargs
                       (list (list #'update-instance-for-different-class
                                   previous current )
                             (list #'shared-initialize
                                   current added-slots )))
    (apply #'shared-initialize current added-slots initargs) ) )


;;;; 7.7.7 update-instance-for-redefined-class
;
(defmethod cl:update-instance-for-redefined-class
        ((instance standard-object) added-slots discarded-slots plist
          &rest initargs )
    (declare (dynamic-extent initargs))

  (validate-initargs (class-of instance) initargs
                     (list (list #'update-instance-for-redefined-class 
                                instance added-slots discarded-slots plist )
                            (list #'shared-initialize instance added-slots) ))
  (apply #'shared-initialize instance added-slots initargs) )
