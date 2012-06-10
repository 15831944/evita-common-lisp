;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 7 Objects
;;; macro/m07-object.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m07-object.lisp#5 $
;;;
;;; Description:
;;;  This file contains following macros:
;;;
;;; MOP Functions:
;;;     extract-lambda-list         MOP
;;;     extract-speclizers          MOP
;;;
;;; Public Macros:
;;;     call-method                 7.7.30
;;;     call-next-method            7.7.31
;;;     defclass                    7.7.25
;;;     defgeneric                  7.7.26
;;;     define-method-combination   7.7.33  NYI
;;;     defmethod                   7.7.27
;;;     make-method                 7.7.30
;;;     next-method-p               7.7.29
;;;     with-accessors              7.7.23
;;;     with-slots                  7.7.24
;
(in-package :xc)


;;;; MOP extract-lambda-list
;
(defun clos:extract-lambda-list (lambda-list &aux (names '()))
  (labels (
    (check-name (name)
      (unless (and name (symbolp name) (not (keywordp name)))
        (error-name-bad name) )
      (when (member name names :test #'eq)
        (error-name-dup name) )
      (push name names) )

    (error-lambda-list (lambda-list)
      (macro-error "Specailzied lambda list syntax error: ~S."
                   lambda-list ) )

    (error-name-bad (name)
      (macro-error "Invalis parameter name: ~S" name) )

    (error-name-dup (name)
      (macro-error "Parameter ~S appeared more than once." name) )

    (error-param-bad (param)
      (macro-error "Invalid specialized parameter: ~S" param) )

    (error-spec-bad (spec)
      (macro-error "Invalid specializer: ~S" spec) )
    )
    ;;
    ;; extract-lambda-list
    ;;
    (multiple-value-bind (next reqs opts rests keys auxs)
        (analyze-lambda-list lambda-list)

      (when next (error-lambda-list lambda-list))

      (dolist (spec reqs)
        (cond
          ((symbolp spec)
            (check-name spec) )

          ((eql 2 (si::safe-list-length spec))
            (check-name (first spec))
            (let ((specializer (second spec)))
              (cond
                ((and specializer
                      (symbolp specializer)
                      (not (keywordp specializer)) ))
                ((and (one-argument-form-p specializer 'eql)))
                (t (error-spec-bad spec)) ) ) )
            (t
              (error-param-bad spec) )) )
      (setq names (nreverse names))
      `(,@names
        ,@opts
        ,@(and rests rests)
        ,@(and keys
            `(&key
              ,@(loop
                  for (key var svar initform initform-p) in (rest keys)
                  collect
                    `(,@(if (string= (symbol-name key) (symbol-name var))
                            (list var)
                          (list (list key var)))
                      ,@(and initform-p (list initform))
                      ,@(and svar (list svar)) ))
             ,@(and (eq '&allow-other-keys (first keys))
                    (list '&allow-other-keys) )))
        ,@(and auxs (cons '&aux auxs)) ) ) ) )


;;;; MOP extract-specializer-names
;
(defun clos:extract-specializer-names (lambda-list)
  (loop
    for param in (nth-value 1 (analyze-lambda-list lambda-list))
    collect
      (if (consp param) (second param) 't) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;

;;;; 7.7.30 call-method
;
(defmacro cl:call-method (method &optional next-method-list)
    (declare (ignore method next-method-list))
  (error 'invalid-local-macro-call
         :name   'cl:call-method
         :parent 'cl:define-method-combination ) )


;;;; 7.7.31 call-next-method
;
(defmacro cl:call-next-method (&rest args)
    (declare (ignore args))
  (error 'invalid-local-function-call
         :name   'cl:call-next-method
         :parent 'cl:defmethod ) )


;;;; 7.7.25 defclass
;
(defmacro cl:defclass (class-name (&rest superclass*) (&rest slot-spec*)
                       &rest option*
                       &environment env )
  (labels (
    ;; check-slot-name
    ;;
    (check-slot-name (name)
      (unless (and name (symbolp name) (not (keywordp name)))
        (error* "Invalid slot name: ~S" name) ) )

    ;; collect-initargs
    ;;
    (collect-initargs (class initargs)
      (dolist (slotd (clos:class-direct-slots class))
        (setq initargs (union initargs (clos:slot-definition-initargs slotd))) )

      (dolist (superclass (clos:class-direct-superclasses class))
        (setq initargs (collect-initargs superclass initargs)) )

      initargs )

    ;; error*
    ;;
    (error* (ctrl-string &rest args)
        (declare (dynamic-extent args))
      (macro-error "Class ~S: ~?" class-name ctrl-string args) )

    ;; error-accessor-bad
    ;;
    (error-accessor-bad (slot-name name)
      (error* "Slot ~S: Invalid accessor name ~S."
                   slot-name name ) )

    ;; error-accessor-dup
    ;;
    (error-accessor-dup (slot-name name)
      (error* "Slot ~S: Accessor ~S appeared more than once."
                   slot-name name) )

    ;; error-initarg-bad
    ;;
    (error-initarg-bad (name)
      (error* "Invalid initarg name ~S." name) )

    ;; error-initarg-dup
    ;;
    (error-initarg-dup (name)
      (error* "Initarg ~S appeared more than once." name) )

    ;; error-initarg-missing
    ;;
    (error-initarg-missing (name)
      (error* "Missing value for initarg ~S" name) )

    ;; error-option-dup
    ;;
    (error-option-dup (option)
      (error* "Option ~S appeared more than once." (first option)) )

    ;; error-option-syntax
    ;;
    (error-option-syntax (option)
      (error* "Option syntax error: ~S" option) )

    ;; error-slot-dup
    ;;
    (error-slot-dup (slot-name)
      (error* "Slot ~S appeared more than once." slot-name) )

    ;; error-slot-initarg-bad
    ;;
    (error-slot-initarg-bad (slot-name name)
      (error* "Slot ~S: Invalid initarg name ~S." slot-name name) )

    ;; error-slot-initarg-dup
    ;;
    (error-slot-initarg-dup (slot-name name)
      (error* "Slot ~S: Initarg ~S appeared more than once."
                   slot-name name ) )

    ;; error-slot-missing
    ;;
    (error-slot-missing (slot-name name)
      (error* "Slot ~S: Missing value for slot option ~S"
                   slot-name name) )

    ;; error-slot-not-list
    ;;
    (error-slot-not-list (slot-name name value)
      (error* "Slot ~S: Value of option ~S must be a list: ~S"
              slot-name
              name
              value ) )

    ;; error-slot-option-dup
    ;;
    (error-slot-option-dup (slot-name option)
      (error* "Slot ~S: Option ~S appeared more than once."
                   slot-name
                   option ) )

    ;; error-slot-syntax
    ;;
    (error-slot-syntax (spec)
      (error* "Slot specifier syntax error: ~S" spec) )

    ;; functionly
    ;;
    (functionly (name form)
      (if (constantp form env)
          `(constantly ,form)
        `(lambda () (declare (ext:lambda-name ,name)) ,form) ) )

    ;; make-arg-from-alist
    ;;
    (make-arg-from-alist (alist)
      (loop
        for (name . value) in alist
        with forms = '()
        finally (return forms)
        do
          (push value forms)
          (push (if (keywordp name) name `',name) forms) ) )

    ;; parse-options
    ;;
    (parse-options (option*)
      (let ((alist '()))
        (dolist (option option*)
          (unless (consp option)
            (error-option-syntax option) )

          (case (first option)
            ((:default-initargs)
              (when (assoc (first option) alist)
                (error-option-dup option) )
              (let ((scan      (rest option))
                    (initargs  '()) )
                (loop
                  (when (null scan) (return))
                  (let* ((name (pop scan))
                         (value (if (null scan)
                                    (error-initarg-missing name)
                                  (pop scan) ) ))
                    (unless (and name (symbolp name))
                      (error-initarg-bad name) )
                    (when (member name initargs :key #'second)
                      (error-initarg-dup name) )
                    (push `(list ',name ',value
                                 ,(functionly
                                    (list 'si::class-default-initargs
                                          class-name
                                          name )
                                    value ))
                          initargs )))
                (push (cons :direct-default-initargs
                            `(list ,@initargs) )
                      alist ) ) )

            ((:documentation)
              (when (assoc(first option) alist)
                (error-option-dup option) )
              (unless (eql (si::safe-list-length option) 2)
                (error-option-syntax option) )
              (unless (stringp (second option))
                (error-option-syntax option) )
              (push (cons :documentation (second option)) alist) )

            ((:metaclass)
              (when (assoc (first option) alist)
                (error-option-dup option) )
              (unless (eql (si::safe-list-length option) 2)
                (error-option-syntax option) )
              (unless (symbolp (second option))
                (error-option-syntax option) )
              (push (cons :metaclass `',(second option)) alist) )

            (otherwise
              (push (cons (first option) `(quote ,(rest option))) alist) )) )
        (make-arg-from-alist alist) ) )

    ;; parse-slot
    ;;
    (parse-slot-option (slot-name options metaclass option-names)
      (let ((alist     '())
            (readers   '())
            (writers   '())
            (setfs     '())
            (initargs  '()) )

        ;; Transform :initargs into :initarg
        ;;
        (loop for scan = options then (rest scan) do
          (when (null scan) (return))
          (let* ((name (pop scan))
                 (value
                   (if (null scan)
                       (error-slot-missing slot-name name)
                    (first scan) ) )
                 (name-1
                   (cdr (assoc name '((:accessors . :accesor)
                                      (:initargs  . :initarg)
                                      (:readers   . :reader)
                                      (:writers   . :writer) )
                               :test #'eq ) )))
            (when name-1
              (warn-slot-option-not-good slot-name name name-1)
              (unless (listp value)
                (error-slot-not-list slot-name name value) )
              (setq options
                    (nconc (mapcan #'(lambda (x) (list name-1 x)) value)
                           options )) ) ))

        ;; Parse slot option
        ;;
        (loop for (name value) on options by #'cddr do
          (case name
            ((:accessor)
              (unless (and value (symbolp value))
                (error-accessor-bad slot-name value) )
              (when (or (member value readers) (member value setfs))
                (error-accessor-bad slot-name value) )
              (push value readers)
              (push value setfs) )

            ((:allocation)
              (when (assoc name alist)
                (error-slot-option-dup slot-name name) )
              (push (cons name `,value) alist) )

            ((:documentation)
              (when (assoc name alist)
                (error-slot-option-dup slot-name name) )
              (push (cons name value) alist) )

            ((:initarg)
              (unless (and value (symbolp value))
                (error-slot-initarg-bad slot-name value) )
              (when (member value initargs)
                (error-slot-initarg-dup slot-name value) )
              (push value initargs) )

            ((:initform)
              (when (assoc name alist)
                (error-slot-option-dup slot-name name) )
              (push (cons name `',value) alist)
              (push (cons :initfunction
                          (functionly
                            (list 'si::slot-definition-initfunction
                                  class-name
                                  slot-name )
                            value ))
                    alist ) )

            ((:reader)
              (unless (and value (symbolp value))
                (error-accessor-bad slot-name value) )
              (when (or (member value readers)
                        (member value writers) )
                (error-accessor-dup slot-name value) )
              (push value readers) )

            ((:type)
              (when (assoc name alist)
                (error-slot-option-dup slot-name name) )
              (push (cons name `',value) alist) )

            ((:writer)
              (cond
                ((and value (symbolp value))
                  (when (or (member name readers)
                            (member name writers) )
                    (error-accessor-dup slot-name value) )
                  (push value writers) )

                ((si:function-name-p value)
                  (when (member (second value) setfs)
                    (error-accessor-dup slot-name value) )
                  (push (second value) setfs) )

                (t
                  (error-accessor-bad slot-name value) )) )

            (otherwise
              (unless (find name option-names)
                (warn-slot-option metaclass slot-name name) )
              (push (cons name `',value) alist) )) )
        ;;
        (setq writers (nconc writers
                             (mapcar #'(lambda (x) `(setf ,x)) setfs) ))

        `(,slot-name
          ,@(loop for (name . value) in alist
               collect name
               collect value )
          ,@(and initargs `(:initargs ',(nreverse initargs)))
          ,@(and readers  `(:readers  ',(nreverse readers)))
          ,@(and writers  `(:writers  ',(nreverse writers))) ) ) )

    ;; parse-slots
    ;;
    (parse-slots (spec* metaclass)
      (let ((slot-alist '())
            (option-names (and metaclass (collect-initargs metaclass '()))) )
        (dolist (spec spec*)
          (cond
            ((and spec (symbolp spec))
              (check-slot-name spec)
              (when (assoc spec slot-alist)
                (error-slot-dup spec) )
              (push (list spec) slot-alist) )

            ((>= (si::safe-list-length spec) 1)
              (let ((slot-name (first spec)))
                (check-slot-name slot-name)
                (when (assoc slot-name slot-alist)
                  (error-slot-dup slot-name) )

                (push (parse-slot-option slot-name (rest spec)
                                         metaclass option-names )
                      slot-alist ) ) )
            (t
              (error-slot-syntax spec) )) )
        (loop
          for (slot-name . slot-args) in slot-alist
          with args = '()
          finally (return args)
          do
            (push `(list :name ',slot-name ,@slot-args) args) ) ) )

    ;; warn-slot-option
    ;;
    (warn-slot-option (metaclass slot-name name)
      (if (eq (find-class 'standard-class) metaclass)
          (error* "Slot ~S: Invalid slot option: ~S" slot-name name)
        (style-warn "Slot ~S: Unknown slot option: ~S" slot-name name) ) )

    (warn-slot-option-not-good (slot-name name name-1)
      (style-warn "Slot ~S: Should use ~S instead of ~S."
                  slot-name
                  name-1
                  name ) )
    )
    ;;
    ;; defclass
    ;;

    ;; Check direct super class names
    ;;
    (loop for (super-name . rest) on superclass* do
      (unless (and super-name (symbolp super-name))
        (error* "Class name must be a symbol: ~S" super-name) )

      (when (member super-name rest :test #'eq)
        (error* "Super class ~S appeared more than once." super-name) )

      (when (eq super-name class-name)
        (error* "Can't include itself as super class.") )

      (unless (find-class super-name nil env)
        (style-warn "Super class ~S isn't defined." super-name) ))

    ;; Check slot specifiers and options.
    ;;
    (let* ((options (parse-options option*))
           (metaclass
             (let* ((name
                      (let ((form (getf options :metaclass 'standard-class)))
                        (when (consp form) (setq form (second form)))
                        form ) )
                    (metaclass  (find-class name nil env)) )
               (unless metaclass
                 (style-warn "Metaclass ~S isn't defined." name) )
               metaclass ) )
           (slots   (parse-slots slot-spec* metaclass))
           (arg* `(',class-name ',superclass* (list ,@slots) ,@options)) )
      `(progn
         (eval-when (:compile-toplevel)
           (%defclass ,@arg*) )
         (si::%defclass ,@arg*) ) ) ) )


;;;; 7.7.26 defgeneric
;
(defmacro cl:defgeneric (fname lambda-list &rest options &environment env)
  (labels (
    ;; check-class
    ;;
    (check-class (name super-name)
      (unless (and name (symbolp name))
        (macro-error "Invalid class name: ~S" name) )
      (let ((class       (find-class name nil env))
            (super-class (find-class super-name t env)) )
        (unless class 
          (error-class-not-found name) )
        (unless (si::subclassp class super-class)
          (macro-error "Class ~S must be subclass of ~S."
                       name
                       super-name ))
        class ) )

    ;; error-class-not-found
    ;;
    (error-class-not-found (name)
      (macro-error "No such class: ~S" name) )

    ;; error-lambda-list
    ;;
    (error-lambda-list-initform (type name)
      (macro-error "Can't use initform for ~A parameter: ~S"
                   type name ) )
    )
    ;;
    ;; defgeneric
    ;;
    (let ((var-gf     (gensym "gf"))
        (args '())
        (decls      '())
        (methods    '()) )

      ;; Check gf-lambda-list
      ;;
      (multiple-value-bind (reqs opts rest keys auxs)
          (xc::parse-lambda-list lambda-list)
          (declare (ignore rest))

        (dolist (spec (rest opts))
          (when (rest spec)
            (error-lambda-list-initform '&optional (first spec)) ) )

        (dolist (spec (rest keys))
          (when (fifth spec)
            (error-lambda-list-initform '&key (second spec)) ) )

        (when auxs
          (macro-error "Can't use &aux for generic-function lambda-list.") )

        ;; Process options
        ;;
        (dolist (option options)
          (unless (consp option)
            (macro-error "Invalid defgeneric option: ~S" option) )

          (when (getf args (first option))
            (macro-error "Option ~S appeared more than once."
                         (first option) ))

          (case (first option)
            ((:argument-precedence-order)
              (let ((rest (copy-list reqs)))
                (dolist (arg (rest option))
                  (unless (and arg (symbolp arg))
                    (macro-error "Invalid argument name: ~S." arg) )

                  (unless (position arg reqs :test #'eq)
                    (macro-error "~S isn't required argument." arg) )

                  (unless (member arg rest :test #'eq)
                    (macro-error "~S appeared more than once." arg) )

                  (setq rest (delete arg rest :test #'eq)) )
                (when rest
                  (macro-error "~S doesn't contain ~{~S~^, ~}."
                               (first option)
                               rest ))
                (setq args
                  (list* (first option) `',(rest option) args) ) ) )

            ((declare)
              (when (null (rest option))
                (macro-error "No declaration in: ~S" option) )
              (setq decls (append decls (rest option))) )

            ((:documentation)
              (unless (eql (si::safe-list-length option) 2)
                (macro-error "Syntax error: ~S" option) )
              (setq args
                (list* :documentation (second option) args) ) )

            ((:generic-function-class)
              (unless (eql (si::safe-list-length option) 2)
                (macro-error "Syntax error: ~S" option) )
              (check-class (second option) 'generic-function)
              (setq args
                (list* (first option) `',(second option) args) ) )

            ((:method-class)
              (unless (eql (si::safe-list-length option) 2)
                (macro-error "Syntax error: ~S" option) )
              (check-class (second option) 'method)
              (setq args
                (list* (first option) `',(second option) args) ) )

            ((:method-combination)
              (unless (>= (si::safe-list-length option) 2)
                (macro-error "Syntax error: ~S" option) )
              (setq args
                (list* :method-combination
                          `(list ',(second option) ,@(cddr option))
                       args )) )

            ((:method)
              (push `(defmethod ,fname ,@(rest option)) methods) )

            (otherwise
              (macro-error "Unknown defgeneric option: ~S" option) )) ) )

      (when decls
        (setq args (list* ''declare `',decls args)) )

      ;; Expansion
      ;;
      `(progn
         (eval-when (:compile-toplevel)
           (compile-notice 'defgeneric ',fname ',lambda-list) )
         (let ((,var-gf (si::%defgeneric ',fname ',lambda-list ,@args)))
           ,@methods
           ,var-gf )) ) ) )


;;;; 7.7.27 defmethod
;;;
;;;
;;; BUGBUG: NYI: Check lambda-list parameter name duplication.
;;;
;;; Note: defmethod macro must pass method-lambda to %defmethod for
;;; compilation of method-lambda. This prevents dynamic compilation
;;; of method function.
;
(defmacro cl:defmethod (fname &rest body &environment env)
  (let* ((qualifiers
           (loop while (not (listp (first body)))
             collect (pop body) ) )
         (slambda-list (pop body))
         (lambda-list  (clos:extract-lambda-list slambda-list))
         (specializers (clos:extract-specializer-names slambda-list))
         (lambda-name
           `(method ,fname ,qualifiers ,specializers) )
         (gf
           (let ((gf (and (fboundp fname) (fdefinition fname))))
             (if (typep gf 'generic-function)
                 gf
               (make-instance
                 'standard-generic-function
                 :name         fname
                 :method-class (find-class 'standard-method) )) ) )
         specializer* ignorable*
         method-lambda-list
         decl* form* )

    (multiple-value-setq (specializer* ignorable*)
      (loop for specializer in specializers
            for var in slambda-list
        finally
          (when undefs
            (style-warn
                "Method ~S is specialized for undefined class~:[ ~{~S~}~;es:~{~#[~;and~] ~S~^, ~}~]."
                fname
                (/= 1 (length undefs))
                undefs ))
          (return (values specializer* ignorable*))

        collect (if (symbolp specializer)
                    `(find-class ',specializer)
                  `(clos:intern-eql-specializer ,(second specializer)) )
          into specializer*

        when (and (symbolp specializer)
                  (not (find-class specializer nil env)) )
          collect specializer into undefs

        when (consp var)
          collect (first var) into ignorable* ))

    ;; Lambda-list for (real) method-function
    ;;
    (setq method-lambda-list
      (loop
        for token in lambda-list
        with key-p = nil
        with list = nil
          when (eq '&key token) do (setq key-p t)
          when (eq '&allow-other-keys token) do (setq key-p nil)
          when (and (eq '&aux token) key-p)
            do (push '&allow-other-keys list)
               (setq key-p nil)
          do (push token list)
       finally
         (when key-p (push '&allow-other-keys list))
         (return (nreverse list)) ))

    ;; Make method-lambda
    ;;
    ;; Note: We can't use labels for method-lambda. Because of fname is
    ;; bound to generic-function instead of local-function. Thus, referencing
    ;; fname means referencing generic-function instead of local-function
    ;; in function body.
    ;;
    (multiple-value-setq (decl* form*) (analyze-body body nil))

    ;; Note: We can't declare type of parameters by using type declaration.
    ;; We should have another declaration such as si::discrimatnor-type.

    ;; Add type declaration for specializers.

    (multiple-value-bind (method-function initargs)
        (clos:make-method-lambda
            gf
            (clos:class-prototype (clos:generic-function-method-class gf))
            `(lambda ,method-lambda-list
                ,@(loop
                    for specializer in specializers
                    for var in method-lambda-list
                      unless (eq specializer 't)
                        collect `(declare (type ,specializer ,var)) )
                ,@decl*
                (let
                    ,(loop for var in ignorable* collect `(,var ,var))
                    ,@(and ignorable* `((declare (ignorable ,@ignorable*))))
                  (block ,(if (consp fname) (second fname) fname)
                    ,@form* ) ) )
            env )

      (setf (cddr method-function)
        (cons `(declare (ext:lambda-name ,lambda-name))
              (cddr method-function) ))

      ;; Expansion
      `(progn
         (eval-when (:compile-toplevel)
           (compile-notice 'defmethod ',fname ',specializers) )
         (si::%defmethod ',fname
                         ',qualifiers
                         ',lambda-list
                         (list ,@specializer*)
                         #',method-function
                         ,@initargs ) ) ) ) )


;;;; 7.7.30 make-method
;
(defmacro cl:make-method (form)
    (declare (ignore form))
  (error 'invalid-local-macro-call
         :name   'cl:make-method
         :parent 'cl:define-method-combination ) )


;;;; 7.7.31 next-method-p
;
(defmacro cl:next-method-p ()
  (error 'invalid-local-function-call
         :name   'cl:next-method-p
         :parent 'cl:defmethod ) )


;;;; 7.7.23 with-accessors
;
(defmacro cl:with-accessors ((&rest slot-entry*) instance-form &body body)
  (let ((var-instance (gensym "instance_"))
        (bindings '()) )
    (loop for slot-entry in slot-entry*
          with vars = '()
          do (destructuring-bind (var accessor) slot-entry
               (unless (and var (symbolp var))
                 (error 'simple-program-error
                        :format-control   "Variable name must be a symbol: ~S"
                        :format-arguments (list var) ))

               (when (member var vars :test #'eq)
                 (error 'simple-program-error
                        :format-control   "Variable ~S is already appeared."
                        :format-arguments (list var) ))

               (unless (and accessor (symbolp accessor))
                 (error 'simple-program-error
                        :format-control   "Accessor name must be a symbol: ~S"
                        :format-arguments (list accessor) ))

               (push var vars)
               (push `(,var (,accessor ,var-instance)) bindings) ))
    (setq bindings (nreverse bindings))
    `(let ((,var-instance ,instance-form))
       (symbol-macrolet ,bindings ,@body) ) ) )


;;;; 7.7.24 with-slots
;
(defmacro cl:with-slots((&rest slot-entry*) instance-form &body body)
  (let ((var-instance (gensym "instance_"))
        (bindings '()) )
    (loop for slot-entry in slot-entry*
          with vars = '()
          do (multiple-value-bind (var slot-name)
                 (cond
                   ((and slot-entry (symbolp slot-entry))
                     (values slot-entry slot-entry) )
                   ((eql 2 (si::safe-list-length slot-entry))
                     (values (first slot-entry) (second slot-entry)) )
                   (t
                     (error 'simple-program-error
                            :format-control  "Invalid slot-entry: ~S"
                            :format-arguments slot-entry ) ))

               (unless (and var (symbolp var))
                 (error 'simple-program-error
                        :format-control   "Variable name must be a symbol: ~S"
                        :format-arguments (list var) ))

               (when (member var vars :test #'eq)
                 (error 'simple-program-error
                        :format-control   "Variable ~S is already appeared."
                        :format-arguments (list var) ))

               (unless (and slot-name (symbolp slot-name))
                 (error 'simple-program-error
                        :format-control   "Slot name must be a symbol: ~S"
                        :format-arguments (list slot-name) ))
               (push var vars)
               (push `(,var (slot-value ,var-instance ',slot-name))
                     bindings ) ))
    (setq bindings (nreverse bindings))
    `(let ((,var-instance ,instance-form))
       (symbol-macrolet ,bindings ,@body) ) ) )


;;;; define-unsafe-accessors
;
(defmacro define-unsafe-accessors (class-name)
    (declare (type symbol class-name))
    (declare (values unspecified))
  (labels (
    ;; get-raw-reader-name
    (get-raw-reader-name (class)
        (declare (type class class))
        (declare (values symbol))
      (etypecase class
        (standard-class 'clos:standard-instance-access)
        (clos:funcallable-standard-class
            'clos:funcallable-standard-instance-access )
        (structure-class 'si::structure-instance-access) ) )

    ;; make-accessor-name
    (make-accessor-name (slot-name)
        (declare (type symbol))
        (declare (values symbol))
      (intern (format nil ".~A-~A" class-name slot-name)) )

    ;; make-accessors
    (make-accessors (class)
        (declare (type class class))
        (declare (values list))
      (loop
        with op = (get-raw-reader-name class)
        for slotd in (clos:class-slots class)
        for name     = (make-accessor-name (clos:slot-definition-name slotd))
        for location = (clos:slot-definition-location slotd)
        collect (make-reader op name location)
        collect (make-writer op name location) ) )

    ;; make-reader
    (make-reader (op name location)
        (declare (type symbol op))
        (declare (type symbol name))
        (declare (type ext:sequence-index location))
        (declare (values list))
      `(define-compiler-macro ,name (object)
          `(,',op ,object ,,location) ) )

    ;; make-writer
    (make-writer (op name location)
        (declare (type symbol op))
        (declare (type symbol name))
        (declare (type ext:sequence-index location))
        (declare (values list))
      `(define-compiler-macro (setf ,name) (value object)
         `(setf (,',op ,object ,,location) ,value) ) )
    )
    ;;
    ;; define-unsafe-accessors
    ;;
    (let ((class (find-class class-name)))
      (unless (si::class-finalized-p class)
        (si::finalize-inheritance class) )
      `(progn ,@(make-accessors class)) ) ) )


;;;; define-method-combination
;
(defmacro cl:define-method-combination (name &rest options)
  (error "NYI: define-method-combination ~S ~S" name options) )

