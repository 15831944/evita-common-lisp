;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - Method
;;; lisp/clos/o11-method.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o11-method.lisp#3 $
;;;
;;; Description:
;;;  This file contains method implementations.
;;;
;;; MOP Functions:
;;;     compute-applicable-methods-using-classes
;;;     ensure-generic-function-using-class
;;;     fine-method-combination
;;;     generic-function-argument-precedence-order
;;;     generic-function-declarations
;;;     generic-function-lambda-list
;;;     map-dependents
;;;     slot-definition-allocation
;;;     update-dependent
;;;
;;; Public Functions:
;;;     add-method                  7.7.35
;;;     allocate-instance           7.7.3
;;;     compute-applicable-methods  7.7.32  STAB
;;;     find-method                 7.7.34
;;;     function-keywords           7.7.1
;;;     initialize-instance         7.7.36
;;;     make-load-form              7.7.21
;;;     make-load-form-saving-slots 7.7.22
;;;     no-applicable-method        7.7.16
;;;     no-next-method              7.7.17
;;;     reinitialize-instance       7.7.4
;;;     remove-method               7.7.18
;;;     shared-initialzie           7.7.5
;
(in-package :si)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; MOP Functions
;;;;

;;;; MOP compute-applicable-methods-using-classes
;;;
;;; BUGBUG: NYI: Nobody calls this method.
;
(defmethod clos:compute-applicable-methods-using-classes
        ((gf standard-generic-function) classes)

  (let* ((param-info (slot-value gf 'param-info))
         (order    (ref param-info order param-info))
         (cpls     (mapcar #'class-precedence-list classes)) )
    (labels (
      (more-specific-p (method-1 method-2)
        (loop
          for cpl     in cpls
          for class-1 in (method-specializers method-1)
          for class-2 in (method-specializers method-2)
          unless (eq class-1 class-2)
            return (member class-2
                           (member class-1 cpl :test #'eq)
                           :test #'eq )) )

      (ordered-more-specific-p (method-1 method-2)
        (loop
          with classes-1 = (method-specializers method-1)
          with classes-2 = (method-specializers method-2)
          for i   in order
          for cpl in cpls
          for class-1 = (nth i classes-1)
          for class-2 = (nth i classes-2)
          unless (eq class-1 class-2)
            return (member class-2
                           (member class-1 cpl :test #'eq)
                           :test #'eq )) )
      )
      ;;
      ;; compute-applicable-methods-using-classes
      ;;

      ;; Check classes argument.
      ;;
      (loop
        for n = 0 then (1+ n)
        for class in classes
        unless (typep class 'class)
          do (error 'type-error :datum class :expected-type 'class)
        finally
          (unless (eql n (ref param-info nreqs param-info))
            (return-from compute-applicable-methods-using-classes
                         (values nil t) )))

      ;; Collect methods
      ;;
      (loop
        for method in (generic-function-methods gf)
        for specializers = (method-specializers method)
        with methods = '()
        finally
          (if (not order)
             (return (values (sort methods #'more-specific-p) t))
           (return (values (sort methods #'ordered-more-specific-p) t)) )
        do
          (loop
            for class       in classes
            for specializer in specializers
            finally
              (push method methods)
            do
              (etypecase specializer
                (class
                  (unless (or (eq class specializer)
                              (subclassp class specializer) )
                    (return) ) )

                (eql-specializer
                  (let ((object (eql-specializer-object specializer)))
                    (when (subclassp (class-of object) class)
                      (return-from compute-applicable-methods-using-classes
                                   (values nil nil) )) ) )))) ) ) )


;;;; MOP compute-effective-method
;
(defmethod clos:compute-effective-method
      ((gf standard-generic-function) mcomb methods)
    (declare (type method-combination mcomb))
    (declare (type list methods))
    (declare (values function))
  (std-compute-effective-method gf mcomb methods) )


;;;; MOP ensure-generic-function-using-class (generic-function)
;;;
;;; Called by:
;;;   ensure-generic-function
;
(defmethod clos:ensure-generic-function-using-class
        ((existing generic-function) fname &rest initargs
         &key
           ((:generic-function-class gf-class) nil gf-class-p)
           ((:method-class mt-class) nil mt-class-p)
           ((:method-combination mt-comb) nil mt-comb-p) )
    (declare (ignore fname))
    (declare (dynamic-extent initargs))
  (remf initargs :generic-function-class)
  (remf initargs :environment)
  (remf initargs 'declare)

  (if (not gf-class-p)
      (setq gf-class (class-of existing))
    (progn
      (setq gf-class (ensure-class gf-class))
      (unless (eq (class-of existing) gf-class)
        (change-class existing gf-class) )))

  (if (not mt-class-p)
      (setf (getf initargs :method-class) (find-class 'standard-method))
    (setf (getf initargs :method-class) (ensure-class mt-class)) )

  (when mt-comb-p
    (setf (getf initargs :method-combination)
          (find-method-combination (class-prototype gf-class)
                                   (first mt-comb)
                                   (rest mt-comb) )))

  (apply #'reinitialize-instance existing initargs) )


;;;; MOP ensure-generic-function-using-class (generic-function)
;;;
;;; Called by:
;;;   ensure-generic-function
;
(defmethod clos:ensure-generic-function-using-class
        ((existing null) fname &rest initargs
         &key
           ((:generic-function-class gf-class) nil gf-class-p)
           ((:method-class mt-class) nil mt-class-p)
           ((:method-combination mt-comb) nil mt-comb-p) )
    (declare (dynamic-extent initargs))
  (remf initargs :generic-function-class)
  (remf initargs :environment)
  (remf initargs 'declare)

  (if (not gf-class-p)
      (setq gf-class (find-class 'standard-generic-function))
    (setq gf-class (ensure-class gf-class)) )

  (if (not mt-class-p)
      (setf (getf initargs :method-class) (find-class 'standard-method))
    (setf (getf initargs :method-class) (ensure-class mt-class)) )

  (if (not mt-comb-p)
      (setf (getf initargs :method-combination) *standard-method-combination*)
    (setf (getf initargs :method-combination)
          (find-method-combination (class-prototype gf-class)
                                   (first mt-comb)
                                   (rest mt-comb) )))

  (setf (fdefinition fname)
        (apply #'make-instance gf-class
                               :name fname
                               initargs )) )


;;;; MOP ensure-generic-function-using-class (generic-function)
;;;
;;; Called by:
;;;   ensure-generic-function
;
(defmethod clos:ensure-generic-function-using-class
        ((existing t) fname &rest initargs)
    (declare (ignore initargs))
  (error 'generic-clobbers-function :name fname) )



;;;; MOP find-method-combination
;
(defmethod clos:find-method-combination
    ((gf generic-function) (type (eql 'standard)) options)
  (when options
    (method-combination-error
      "The method combination type STANDARD accepts no options." ))
  *standard-method-combination* )


;;;; MOP generic-function-argument-precedence-order
;
(defmethod generic-function-argument-precedence-order
    ((gf standard-generic-function))
  (let ((param-info (slot-value gf 'param-info)))
    (or (ref param-info order param-info)
        (subseq (ref param-info lambda-list param-info)
                0 (ref param-info nreqs param-info) )) ) )


;;;; MOP generic-function-declarations
;
(defmethod generic-function-declarations ((gf standard-generic-function))
  (getf (slot-value gf 'plist) 'declarations) )


;;;; MOP generic-function-lambda-list
;
(defmethod generic-function-lambda-list ((gf standard-generic-function))
  (ref param-info lambda-list (slot-value gf 'param-info)) )


;;;; MOP map-dependents
;;;
;;; Called by:
;;;   reinitialize-instance :after class
(defmethod clos:map-dependents ((dependee dependee-mixin) function)
  (dolist (dependent (getf (slot-value dependee 'plist) 'dependents))
    (funcall function dependee dependent) ) )


;;;; MOP slot-definition-allocation
(defmethod clos:slot-definition-allocation ((slotd standard-slot-definition))
  (let ((allocation (slot-value slotd 'allocation)))
    (when (typep allocation 'class)
      (setq allocation :class) )
    allocation ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;

;;;; 7.7.5 add-method
;
(defmethod cl:add-method ((gf standard-generic-function) (method method))
  (let ((gf (slot-value method 'generic-function)))
    (when gf (error "~S is already part of ~S." method gf)) )

  ;; Update method list in generic-function
  ;;
  (let ((specializers (slot-value method 'specializers))
        (param-info     (slot-value gf 'param-info)) )

    (when param-info
      ;; BUGBUG: NYI: Check optional, rest, and key parameters
      (unless (eq (length specializers) (ref param-info nreqs param-info))
        (error 'lambda-list-mismatch
               :function    gf
               :expected-lambda-list (ref param-info lambda-list param-info)
               :lambda-list (slot-value method 'lambda-list) )))

    (loop
      for scan on (slot-value gf 'methods)
      and prev = nil then scan
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
            (dolist (specializer specializers)
              (remove-direct-method specializer present) )
            (return) ) ))
    (dolist (specializer specializers)
      (add-direct-method specializer method) )

    (setf (slot-value method 'generic-function) gf)

    ;; Update generic-function
    ;;
    (if param-info
        (update-discriminator gf)
      (multiple-value-bind (reqs opts rests keys)
          (xc::parse-lambda-list (slot-value method 'lambda-list))
        (when (and (null rests) keys)
          (setq rests '(&rest #:args)) )
        (set-generic-function-lambda-list gf `(,@reqs ,@opts ,@rests) nil) ))

    ;; Return value of add-method
    gf ) )


;;;; STAB 7.7.32 compute-applicable-methods
;;;
;;; BUGBUG: NYI: eql-specializer
;
(defmethod cl:compute-applicable-methods ((gf standard-generic-function) args)
  (std-compute-applicable-methods gf args) )

;;;; Note:
;;; we must not call compute-applicable-methods in discriminator.
(defun std-compute-applicable-methods (gf args)
  (let* ((classes  (mapcar #'class-of args))
         (cpls
           (loop for class in classes
             collect (slot-value class 'class-precedence-list) ) )
         (param-info (slot-value gf 'param-info))
         (order    (ref param-info order param-info)) )
    (labels (
      ;; more-specific-p
      (more-specific-p (method-1 method-2)
        (loop
          for cpl     in cpls
          for class-1 in (method-specializers method-1)
          for class-2 in (method-specializers method-2)
          unless (eq class-1 class-2)
            return (dolist (runner cpl nil)
                     (when (eq runner class-1) (return t))
                     (when (eq runner class-2) (return nil)) )) )

      ;; ordered-more-specific-p
      (ordered-more-specific-p (method-1 method-2)
        (loop
          with classes-1 = (method-specializers method-1)
          with classes-2 = (method-specializers method-2)
          for i in order
          for cpl     = (nth i cpls)
          for class-1 = (nth i classes-1)
          for class-2 = (nth i classes-2)
          unless (eq class-1 class-2)
            return (dolist (runner cpl nil)
                     (when (eq runner class-1) (return t))
                     (when (eq runner class-2) (return nil)) )) )
      )
      ;;
      ;; std-compute-applicable-methods
      ;;
      (loop
        for  method in (slot-value gf 'methods)
        with methods = '()
        finally
          (if (null order)
              (return (sort methods #'more-specific-p))
            (return (sort methods #'ordered-more-specific-p)) )
        do
          (loop
            for class       in classes
            for cpl         in cpls
            for specializer in (slot-value method 'specializers)
            for arg         in args
            finally
              (push method methods)
            do
              (etypecase specializer
                (class
                  (unless (or (eq class specializer)
                              (member specializer cpl :test #'eq) )
                    (return) ) )
                (eql-specializer
                  (unless (eql (eql-specializer-object specializer) arg)
                    (return) ) )))) ) ) )


;;;; 7.7.34 find-method
;
(defmethod cl:find-method ((gf standard-generic-function)
                           qualifiers
                           specializers
                           &optional (error-p t) )
  (unless (= (length (generic-function-argument-precedence-order gf))
             (length specializers) )
    (error 'specializers-count-mismatch
           :expect-to (generic-function-argument-precedence-order gf)
           :supplied  specializers ))
  (loop
    for method in (generic-function-methods gf)
    finally
      (when error-p
        (error 'method-not-found
               :generic-function    gf
               :qualifiers          qualifiers
               :specializers        specializers ))
    when (and (equal (method-qualifiers   method) qualifiers)
              (equal (method-specializers method) specializers) )
      return method ) )


;;;; 7.7.1 function-keywords
;;;
;;; Called by:
;;;   validate-initargs
;;;
;;; BUGBUG: REVIEW: We should cache keyword names instead of calling
;;; xc::parse-lambda-list.
;
(defmethod cl:function-keywords ((method standard-method))
  (multiple-value-bind (reqs opts rests keys)
      (xc::parse-lambda-list (slot-value method 'lambda-list))
      (declare (ignore reqs opts rests))
    (values (mapcar #'first (rest keys))
            (eq '&allow-other-keys (first keys)) ) ) )


;;;; 7.7.36 initialize-instance
;;;
;;; Note: validation of initargs is done in make-isntance.
;;;
;;; See Also: reinitialize-instance
;
(defmethod cl:initialize-instance (instance &rest initargs)
    (declare (dynamic-extent initargs))
  (apply #'shared-initialize instance t initargs) )


;;;; 7.7.21 make-load-form (class)
;
(defmethod cl:make-load-form ((object class) &optional env)
    (declare (ignore env))
  `(find-class ',(class-name object)) )


;;;; 7.7.21 make-load-form (standard-object)
;
(defmethod cl:make-load-form ((object standard-object) &optional env)
    (declare (ignore env))
  (error 'can-not-make-load-form :object object) )


;;;; 7.7.21 make-load-form (structure-object)
;
(defmethod cl:make-load-form ((object structure-object) &optional env)
    (declare (ignore env))
  (error 'can-not-make-load-form :object object) )


;;;; 7.7.22 make-load-form-saving-slots
;
(defun cl:make-load-form-saving-slots (object
                                       &key (slot-names nil slot-names-p)
                                            environment )
    (declare (type list slot-names))
    (declare (values list list))
    (declare (ignore environment))
  (let ((class (class-of object)))
  (labels (
    ;; make-create-form
    (make-create-form ()
      `(allocate-instance (find-class ',(class-name class))) )

    ;; make-init-form
    (make-init-form ()
      (loop
        for slotd in (class-slots class)
        for slot-name = (slot-definition-name slotd)
        finally
          (return (and place-value-list `(setf ,@place-value-list)))
        when (and (or (not slot-names-p)
                      (member slot-name slot-names :test 'eq) )
                  (slot-boundp-using-class class object slotd) )
          collect `(slot-value ,object ',slot-name)
            into place-value-list
          and
          collect (make-value-form slotd)
            into place-value-list ) )

    ;; make-value-form
    (make-value-form (slotd)
        (declare (type effective-slot-definition))
        (declare (values t))
      (let ((value (slot-value-using-class class object slotd)))
        (when (or (consp value) (symbolp value))
          (setq value `(quote ,value)) )
        value ) )
    )
    ;;
    (values (make-create-form) (make-init-form)) ) ) )


;;;; 7.7.16 no-applicable-method
;
(defmethod cl:no-applicable-method (gf &rest args)
  (error "There is no applicable method for ~S with ~{~S~^, ~}." gf args) )


;;;; 7.7.17 no-next-method
;
(defmethod cl:no-next-method ((gf standard-generic-function)
                              (method standard-method)
                              &rest args )
  (error  "There is no next method for ~S with ~{~S~^, ~}." method args) )


;;;; 7.7.4 reinitialize-instance
;;;
;;; Note: validation of initargs is done in make-isntance.
;;;
;;; See Also: initialize-instance
;
(defmethod cl:reinitialize-instance (instance &rest initargs)
    (declare (dynamic-extent initargs))
  (apply #'shared-initialize instance nil initargs) )


;;;; 7.7.18 remove-method
;
(defmethod cl:remove-method ((gf generic-function) (mt method))
    (declare (values generic-function))
  (when (eq gf (method-generic-function mt))
    (setf (slot-value gf 'methods)
          (delete mt (slot-value gf 'methods) :test #'eq) )

    (dolist (specializer (method-specializers mt))
      (remove-direct-method specializer mt) )

    (update-discriminator gf)

    (map-dependents
        gf
        #'(lambda (dependent)
            (update-dependent gf dependent 'remove-method mt) )))
  gf )


;;;; 7.7.5 shared-initalize (standard-object)
;;;
;;; Note: Caller must pass valid initargs.
;
(defmethod cl:shared-initialize
        ((instance standard-object) slot-names &rest initargs)
    (declare (type (or list (eql t)) slot-names))
    (declare (values standard-object))
    (declare (dynamic-extent initargs))
  (let ((class (class-of instance)))
    (cond
      ((eq slot-names 't)
        ;; from initialize-instance
        (dolist (slotd (class-slots class))
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
                          (funcall fn) )) ) )) ) ) )

      ((eq slot-names 'nil)
        ;; from reinitialize-instance
        (dolist (slotd (class-slots class))
          (multiple-value-bind (indicator value found-p)
              (get-properties initargs (slot-definition-initargs slotd))
              (declare (ignore indicator))
            (when found-p
              (setf (slot-value-using-class class instance slotd)
                    value )) ) ) )

      (t
        ;; from update-instance-for-different-class, or
        ;; from update-instance-for-redefined-class
        (dolist (slotd (class-slots class))
          (multiple-value-bind (indicator value found-p)
              (get-properties initargs (slot-definition-initargs slotd))
              (declare (ignore indicator))
            (cond
             (found-p
              (setf (slot-value-using-class class instance slotd) value) )

             ((and (not (slot-boundp-using-class class instance slotd))
                   (member (slot-definition-name slotd) slot-names
                           :test #'eq ))
               (let ((fn (slot-definition-initfunction slotd)))
                 (when (functionp fn)
                   (setf (slot-value-using-class class instance slotd)
                         (funcall fn) )) ) )) ) ) ))
    instance ) )
