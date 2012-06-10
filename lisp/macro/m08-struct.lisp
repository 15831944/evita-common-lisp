;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 8 Structures
;;; macro/m08-struct.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m08-struct.lisp#7 $
;;;
;;; Description:
;;;  This file contains following macros:
;;;     defstruct   8.1.1
;;;
;;; BUGBUG: NYI: :include with :type.
;;; BUGBUG: NYI: Remember defstruct has standard ctor or not. Reader
;;; macro #S calls standard ctor. Also, default structure printer
;;; checks it whether using #S notation or print-unreadable-object.
;
(in-package :xc)

;;;; Slot Definition Property List
;;;
;;; Schema
;;;     :name           a symbol denotes name of slot.
;;;     :type           a type specifier. Default is t.
;;;     :initform       a form.
;;;     :initform-p     a boolean. True if slot has an initform.
;;;     :location       a non-negative integer.
;;;
;;; Note: For structure-object, location is start at zero.
;;; Note: slot :initform must have non-nil value for specialized vector.
;;; Note: :initform for included-slot is
;;;   (funcall
;;;     (clos:slot-definition-function
;;;         (si::find-effective-slot class slot-name) ))
;;;
;;; Description:
;;;  Property list represents defstruct slot-description.
;;;

;;;; make-defstruct-ctor
;;;
;;; Arguments and Values:
;;;  name           a symbol denotes name of structure.
;;;  allocate-form  a list denotes structure allocation form.
;;;  ctors   a list of symbol or list.
;;;  slots          a slot definition property list.
;;;
;;; Description:
;;;  Returns list of contrustor defun forms for defstruct macro.
;;;
;;; Note: For instance of structure-object, uninitialized slots that are
;;; introduced by &aux parameters with initforms are a value of initform
;;; if initform is a constant or nil otherwise.
;;;
;;; BUGBUG: NYI: We will use name parameter for error message.
;
(defun make-defstruct-ctors (typespec allocate-form ctors slots)
  (labels (
    ;; compute-ctor/symbol
    ;;  => cons-name lambda-list checker* writer*
    (compute-ctor/cons (ctor origin)
        (declare (type (member :default :user) origin))
        (declare (values symbol list list list list))
      (multiple-value-bind (requireds optionals rests keywords auxiliaries)
                (parse-lambda-list (second ctor))
      (let ((slots       (copy-list slots))
            (lambda-list '())
            (typell      '())
            (checker*    '())
            (writer*     '()) )
      (labels (
        ;; locate-slot
        ;;  Removes slot-plist of slot-name from slots and adds checker
        ;;  and writer.
        (locate-slot (slot-name)
          (let ((runner slots)
                (prev nil) )
            (loop
              (when (endp runner) (return nil))
              (let ((slot (car runner))
                    (next (cdr runner)) )
                (when (eq (getf slot :name) slot-name)
                  (if prev
                      (setf (cdr prev) next)
                    (setq slots next) )
                  (return slot) )
                (setq prev runner)
                (setq runner next) )) ) )

        ;; make-param-name
        (make-param-name (slot-name)
          (ecase origin
            ((:user) slot-name)
            ((:default) (make-symbol (symbol-name slot-name))) ) )

        ;; process-auxiliaries
        ;;    spec ::= var | (var initform)
        (process-auxiliaries ()
          (push '&aux lambda-list)
          (dolist (spec (rest auxiliaries))
              (declare (type cons spec))
            (ecase (length spec)
              ((1)  ; (name)
                ;; We won't initialize slot.
                (locate-slot (first spec)) )
              ((2)  ; (name initform)
                ;; We'll initialize slot
                (let ((var (process-slot (first spec))))
                  (push  (list var (second spec)) lambda-list) ) )) ) )

        ;; process-keywords
        ;;  spec ::= (key var svar-p initform initform-p)
        (process-keywords ()
          (push '&key lambda-list)
          (push '&key typell)

          (dolist (spec (rest keywords))
            (let* ((slot-name (second spec))
                   (slot      (locate-slot slot-name))
                   (var       (make-param-name slot-name)) )

              (push `(,(first spec) ,(getf slot :type t)) typell)

              (when slot (process-slot2 var slot))

              (unless (fifth spec)
                (setf (fourth spec) (getf slot :initform)) )

            (if (not (third spec))
                (push `((,(first spec) ,var)
                        ,(fourth spec) )
                      lambda-list )
              (let* ((slot-name (third spec))
                     (slot      (locate-slot slot-name))
                     (svar      (make-param-name slot-name)) )
                (when slot (process-slot2 svar slot))
                (push `((,(first spec) ,var)
                        ,(fourth spec)
                        ,svar )
                      lambda-list ) )) ) )

          (when (eq (first keywords) '&allow-other-keys)
            (push '&allow-other-keys typell)
            (push '&allow-other-keys lambda-list) ) )

        ;; process-not-appeared
        (process-not-appeared ()
          (loop
            for slot in slots
            for initform-p   = (getf slot :initform-p)
            for initform     = (getf slot :initform)
            for slot-name    = (getf slot :name) do
              (when (and (symbolp slot-name) initform-p)
                (unless auxiliaries
                  (push '&aux lambda-list)
                  (setq auxiliaries '&aux) )
                (let ((var (make-param-name slot-name)))
                  (push `(,var ,initform) lambda-list)
                  (process-slot2 var slot) )) ) )

        ;; process-optionals
        ;;    spec ::= var | (var initform svar-p)
        (process-optionals ()
          (push '&optional lambda-list)
          (push '&optional typell)

          (dolist (spec (rest optionals))
            (let* ((slot-name (first spec))
                   (slot (locate-slot slot-name)) )
              (ecase (length spec)
                ((1)  ; (slot-name)
                  (let ((var (make-param-name slot-name)))
                  (if (null slot)
                      (setq spec var)
                    (let ((initform (getf slot :initform)))
                      (process-slot2 var slot)
                      (if initform
                          (setq spec (list var initform))
                        (setq spec var) ))) ) )
                ((2)  ; (slot-name initform)
                  (setq spec
                    (list (process-slot-aux slot)
                          (second spec) )) )
                ((3)  ; (slot-name initform suppliedp)
                  (setq spec
                    (list (process-slot-aux slot)
                          (second spec)
                          (process-slot (third spec)) )) ))

               (push (getf slot :type t) typell)
               (push spec lambda-list) ) ) )

        ;; process-slot
        ;;  Adds check and writer then returns parameter name.
        (process-slot (slot-name)
          (process-slot-aux (locate-slot slot-name)) )

        (process-slot-aux (slot)
          (let ((var  (make-param-name (getf slot :name))))
            (when slot (process-slot2 var slot))
            var ) )

        ;; process-slot2
        (process-slot2 (var slot)
          (let ((type (getf slot :type t)))
            (unless (eq type 't)
              (push `(check-type ,var ,type) checker*) ) )
          (push `(setf ,(getf slot :reader-form) ,var) writer*) )
        )
        ;;
        (dolist (slot-name requireds)
          (let ((slot (locate-slot slot-name)))
            (push (process-slot-aux slot) lambda-list)
            (push (getf slot :type t) typell) ) )

        (when optionals
          (process-optionals) )

        (when rests
          (let ((slot (locate-slot (second rests))))
            (push '&rest lambda-list)
            (push (process-slot-aux slot) lambda-list)
            (push '&rest typell)
            (push 't typell) ) )

        (when keywords
          (process-keywords) )

        (when auxiliaries
          (process-auxiliaries) )

        ;; slots aren't appeared in BOA lambda-list.
        (when slots
          (process-not-appeared) )

        (let ((cons-name (first ctor)))
          (unless (and cons-name (symbolp cons-name))
            (macro-error "Invalid ctor name: ~S" cons-name) )

          (values cons-name
                  (nreverse lambda-list)
                  (nreverse typell)
                  (nreverse writer*)
                  (nreverse checker*) ) ) ) ) ) )

    ;; compute-ctor/symbol
    ;;  => cons-name lambda-list checker* writer*
    (compute-ctor/symbol (cons-name)
        (declare (values symbol list list list list))
      (loop
        for slot in slots
        for slot-name = (getf slot :name)
          when (symbolp slot-name)
            collect slot-name into keys
        finally
          (return
            (compute-ctor/cons `(,cons-name (&key ,@keys)) :default) )) )
    )
    ;;
    (let ((form* '()))
      (dolist (ctor ctors (nreverse form*))
        (multiple-value-bind (cons-name ll typell writer* checker*)
            (etypecase ctor
              (symbol
                (compute-ctor/symbol ctor) )
              (cons
                (compute-ctor/cons   ctor :user) ))
            (declare (ignore checker*))
          (push `(declaim (ftype (function ,typell ,typespec) ,cons-name))
                form* )
          (push `(defun ,cons-name ,ll
                    #+nil ,@checker*
                    (let ((object ,allocate-form))
                      ,@writer*
                      object ))
                form* ) ) ) ) ) )


;;;; parse-defstruct-options
;;;
;;; Description:
;;;  Parses defstruct options.
;;;
;;; conc-name-option ::=
;;;   :conc-name |            ; no prefix
;;;   (:conc-name) |          ; no prefix
;;;   (:conc-name nil)        ; no prefix
;;;   (:conc-name prefix)     ; prefix + slot-name.
;;;   prefix is string designator.
;;;
;;; ctor-option*   ::=
;;;   :constructor |          ; default consturctor MAKE-<structure-name>
;;;   (:constructor) |        ; default constructor MAKE-<structure-name>
;;;   (:constructor nil) |    ; no constructor
;;;   (:constructor name [arglist])   ; BOA-constructor
;;;
;;; copier-option ::=
;;;   :copier |             ; default copier. COPY-<structure name>
;;;   (:copier) |           ; default copier. COPY-<structure name>
;;;   (:copier nil)         ; no copier
;;;   (:copier name)        ; Use name as copier.
;;;   name is symbol.
;;;
;;; include-option ::= (:include name slot-desc*)
;;; initial-offset-option ::= (:initial-offset offset)
;;; named-option          ::= :named
;;; predicate-option ::=
;;;   :predicate |          ; default predicate. <structure name>-P
;;;   (:predicate) |        ; default predicate. <structure name>-P
;;;   (:predicate nil) |    ; no predicate.
;;;   (:predicate name)     ; Use name as predicate.
;;;
;;; printer-option ::=
;;;   (:print-object) |             ; Specialized method.
;;;   (:print-object printer) |     ; Specialized method.
;;;   (:print-function) |           ; Specialized method.
;;;   (:print-function printer)     ; Specialized method.
;;; printer ::= function-name | lambda-expression
;;;
;;; type-option ::= (:type type)
;;; type ::= list | vector | (vector type)
;
(defun parse-defstruct-options (sname option* env)
  (let ((conc-name          nil)
        (conc-name-p        nil)
        (copier             nil)
        (copier-p           nil)
        (ctors              '())
        (include            nil)
        (named              nil)
        (initial-offset     nil)
        (predicate          nil)
        (predicate-p        nil)
        (printer            nil)
        (include-slots       '())
        (type               nil) )
    (declare (type (or class null) incldue))
  (labels (
    ;; find-include-slot
    (find-include-slot (name class)
      (or (find name (clos:class-direct-slots class)
                :key #'clos:slot-definition-name )
          (dolist (super (clos:class-direct-superclasses class))
            (let ((dslotd (find-include-slot name super)))
              (when dslotd (return dslotd)) ) )) )

    ;; invalid-slot-desc
    (invalid-slot-desc (desc)
      (macro-error "Invalid slot-description: ~S" desc) )

    ;; invalid-name
    (invalid-name (option name)
      (macro-error "Invalid ~S option: ~S" option name) )

    ;; multiple-options
    (multiple-options (name)
      (macro-error "A defstruct option ~S appeared more than once." name) )

    ;; parse-option
    (parse-option (option)
      (typecase option
        (keyword (parse-option-aux option option))
        (cons    (parse-option-aux (first option) option))
        (otherwise
          (macro-error "Malformed defstruct option: ~S" option) )) )

    ;; parse-option-aux
    (parse-option-aux (name option)
      (case name
        ((:conc-name)       (parse-option/conc-name option))
        ((:constructor)     (parse-option/ctor option))
        ((:copier)          (parse-option/copier option))
        ((:initial-offset)  (parse-option/initial-offset option))
        ((:include)         (parse-option/include option))
        ((:named)           (parse-option/named option))
        ((:predicate)       (parse-option/predicate option))
        ((:print-function)  (parse-option/printer option))
        ((:print-object)    (parse-option/printer option))
        ((:type)            (parse-option/type option))
        (otherwise
          (macro-error "Unknown dedfstruct option ~S" name) )) )

    ;; parse-option/conc-name
    (parse-option/conc-name (option)
      (when conc-name-p (multiple-options :conc-name))
      (setq conc-name-p t)
      (ecase (check-syntax option 1 2
                '(:conc-name &optional string-designator) )
        ((1)
          (setq conc-name nil) )    ; reader = slot-name
        ((2)
          (let ((arg (second option)))
            (setq conc-name
              (typecase arg
                (null      nil)     ; reader = slot-name
                (symbol    (symbol-name arg))
                (string    arg)
                (character (make-string 1 :initial-element arg))
                (otherwise
                  (macro-error
                    "~S option requires a string desginator instead of ~S."
                    :conc-name
                    arg )))) ) )) )

    ;; prase-option/copier
    (parse-option/copier (option)
      (when copier-p (multiple-options :copier))
      (setq copier-p t)
      (ecase (check-syntax option 1 2 '(:copier &optional name))
        ((1) (setq copier (intern (format nil "COPY-~A" sname))))
        ((2)
          (setq copier (second option))
          (unless (symbolp copier) (invalid-name :copier copier)) )) )

    ;; parse-option/ctor
    (parse-option/ctor (option)
      (let ((ctor
              (ecase (check-syntax option 1 3
                        '(:constructor &optional name (&rest parameter)) )
                ((1) (intern (format nil "MAKE-~A" sname)))
                ((2) (second option))
                ((3) (rest option)) ) ))

        ;; Check user wants to have no ctor by (:constructor nil).
        (when (or (and (not ctor) ctors)
                  (and ctors (null (first ctors))) )
            (multiple-options :constructor) )

        (push ctor ctors) ) )

    ;; parse-option/include
    (parse-option/include (option)
      (when include (multiple-options :include))

      (check-syntax option 2 most-positive-fixnum
        '(:include name &rest slot-name) )


      (let ((super-name (second option))
            (slot-desc* (cddr option)) )

        (when (eq super-name sname)
          (macro-error "Can't include itself.") )
        (setq include
          (if type
              (si::find-structure super-name t env)
            (find-class super-name t env) ))

        (typecase include
          (si::structure-type
            (unless type
              (macro-error "Can't include ~S to typed strcture." super-name) )
            (let ((super-type (slot-value include 'si::type)))
              (unless (subtypep type super-type)
                (macro-error "Type ~S of include structure ~S isn't compatible with ~S."
                    super-type
                    include
                    type )) ))
          (structure-class
            (when type
              (macro-error "Can't include ~S." super-name) ) )
          (otherwise
            (macro-error "Can't include ~S." super-name) ))

        (dolist (slot-desc slot-desc*)
          (parse-option/include/slot slot-desc include) ) ) )

    ;; parse-option/include/slot
    (parse-option/include/slot (slot-desc superclass)
      (destructuring-bind (name
                           &optional (initform nil initform-p)
                           &key (type nil type-p)
                                (read-only nil read-only-p) )
          (typecase slot-desc
            (symbol    (list slot-desc))
            (cons       slot-desc)
            (otherwise (invalid-slot-desc slot-desc)) )
        (let ((dslotd (find-include-slot name superclass)))
          (when (null dslotd)
            (macro-error "Structure ~S doesn't have slot ~S."
                include
                name ))

          (dolist (slot include-slots)
            (when (eq (getf slot :name) name)
              (macro-error "Slot ~S appeared more than once." name) ) )

          (unless initform-p
            (setq initform-p (clos:slot-definition-initfunction dslotd))
            (setq initform (clos:slot-definition-initform dslotd)) )

          (let ((supertype (clos:slot-definition-type dslotd)))
            (if (not type-p)
                (setq type supertype)
              (let ((type2 (type-and supertype type env)))
                (when (null type2)
                  (macro-error "Slot ~S has incompatible types ~S and ~S."
                    name
                    type
                    supertype ))
                (setq type type2) )) )

          (let ((super-read-only (slot-value dslotd 'si::read-only)))
            (cond
              ((not read-only-p)
                (setq read-only super-read-only) )
              ((and super-read-only (not read-only))
                (macro-error "Slot ~S must be read-only." name) )) )

          (let ((include-slot
                  (list :name name
                        :type type
                        :read-only read-only
                        :initform-p initform-p
                        :initform   initform ) ))
            (push include-slot include-slots) ) ) ) )

    ;; parse-option/initial-offset
    (parse-option/initial-offset (option)
      (when initial-offset (multiple-options (first option)))
      (check-syntax option 2 2 '(:initial-offset offset))
      (setq initial-offset (first option))
      (unless (typep initial-offset
                     '(integer 0 #.(1- array-total-size-limit)) )
        (macro-error "Bad ~S: ~S" :initial-offset initial-offset) ) )

    ;; parse-option/named
    (parse-option/named (option)
      (when named (multiple-options :named))
      (check-syntax option -1 -1 ':named)
      (setq named t) )

    ;; prase-option/predicate
    (parse-option/predicate (option)
      (when predicate-p (multiple-options :predicate))
      (setq predicate-p t)
      (ecase (check-syntax option 1 2 '(:predicate &optional name))
        ((1) (setq predicate (intern (format nil "~A-P" sname))))
        ((2)
          (setq predicate (second option))
          (unless (symbolp predicate)
            (invalid-name :predicate predicate)) )) )

    ;; parse-option/priner
    (parse-option/printer (option)
      (when printer (multiple-options (first option)))
      (setq printer option)
      (ecase (check-syntax option 1 2
                `(,(if (consp option) (first option) option)
                  &optional name ))
        ((1))
        ((2)
          (typecase (second option)
            ((null)
              (style-warn "We don't recommend to use NIL as printer-name.") )
            (symbol)
            (otherwise
              (invalid-name (first option) (second option)) )) )) )

    ;; parse-option/type
    ;;    (:type <type>)
    ;;    type ::= list | vector | (vector <size>)
    (parse-option/type (option)
      (check-syntax option 2 2 '(:type type))
      (when type (multiple-options :type))
      (setq type (second option))
      (cond
        ((eq type 'list))
        ((eq type 'vector))
        ((and (eql (si::safe-list-length type) 2)
                   (eq (first type) 'vector) ))
        (t (macro-error "Bad type option value: ~S" type)) ) )
    )
    ;;
    (dolist (option option*) (parse-option option))

    (cond
      ((not type)
        (setq named t)
        (when initial-offset
          (macro-error "Can't use ~S option with ~S."
                 :initial-offset
                 'structure-object )) )
      (printer
        (macro-error "Can't use ~S option with ~S." printer :type) )

      (named
        (when (consp type)
          (multiple-value-bind (subtypep valid-p)
              (subtypep 'symbol (second type) env)
            (when (and valid-p (not subtypep))
              (macro-error "Can't use ~S option with type ~S."
                :named
                type )) )) )

      (predicate
        (macro-error "Can't use ~S option for anonymous structure."
            :predicate ) ))

    (when (and named (not predicate-p))
      (setq predicate (intern (format nil "~A-P" sname))) )

    (values
      (if conc-name-p conc-name (format nil "~A-" sname))
      (cond
        ((null ctors) (list (intern (format nil "MAKE-~A" sname))) )
        ((null (first ctors)) nil )
        (t ctors) )
      (if copier-p copier (intern (format nil "COPY-~A" sname)))
      (cond
        ((null type) (find-class 'structure-class))
        ((eq type 'list) (find-class 'si::list-structure-type))
        (t (find-class 'si::vector-structure-type)) )
      include
      named
      (or initial-offset 0)
      predicate
      printer
      include-slots
      type ) ) ) )


;;;; parse-destruct-slots
;;;
;;; Description:
;;;  Parses defstruct slot-descriptions.
;;;
;;; slot-description ::= slot-name |
;;;                      (slot-name [slot-initform] slot-option*)
;;; slot-option ::= :type slot-type | :read-only boolean
;
(defun parse-defstruct-slots (sname slot-desc* include env)
    (declare (type symbol sname))
    (declare (type list slot-desc*))
    (declare (type (or class null) include))
    (declare (values list))

    (declare (ignore sname))
    (declare (ignore env))

  (labels (
    ;; check-name
    (check-name (name slots)
      (unless (symbolp name)
        (macro-error "Invalid slot name: ~S" name) )

      (cond
        ((keywordp name)
          (style-warn "We recomend not to use keyword ~S as slot-name."
            name ) )
        ((null name)
          (style-warn "We recomend not to use ~S as slot-name."
            nil ) ))

      (let ((string (symbol-name name)))
        (loop
          for slot in slots
          for present = (getf slot :name) do
            (when (string= (symbol-name present) string)
              (macro-error "Slot ~S conflicts to ~S."
                  name
                  present )) )

        (when include
          (loop
            for eslotd in (clos:class-slots include)
            for present = (clos:slot-definition-name eslotd) do
              (when (string= (symbol-name present) string)
                (macro-error "Slot ~S conflicts to ~S in ~S."
                    name
                    present
                    include )))) ) )

    ;; prase-slot-desc
    (parse-slot-desc (slot-desc slots)
      (destructuring-bind (name
                           &optional (initform nil initform-p)
                           &key (type 't type-p)
                                 read-only )
          (typecase slot-desc
            (symbol (list slot-desc))
            (cons   slot-desc)
            (otherwise
              (macro-error "Invalid slot-description: ~S" slot-desc) ))
         (let ((slot (list :location 0 :reader-form nil)))
            (check-name name slots)

            (when read-only
              (setq slot (list* :read-only t slot)) )

            ;; BUGBUG: NYI: validate type specifier.
            (when type-p
              (setq slot (list* :type type slot)) )

            (when initform-p
              (setq slot (list* :initform initform :initform-p t slot)) )

            (list* :name name slot) )) )
    )
    ;;
    (loop for slot-desc in slot-desc*
      collect (parse-slot-desc slot-desc slots) into slots
      finally (return slots) ) ) )


;;;; 8.1.1 defstruct
;;;
;;; Printer option processing:
;;;   no :print-object option
;;;     No specialized method is generated.
;;;   (:print-object)
;;;     Using specialized print-object which prints structure using #S.
;;;   (:print-object <printer>)
;;;     Using specialized print-object which calls <priner>.
;;;
;;; Note: We don't check length of list.
;;; Note: Vector which is longer than number of slots satisfies predicate.
;
(defmacro cl:defstruct (name &rest slot* &environment env)
  (let ((option* (if (consp name)
                     (prog1 (rest name) (setq name (first name)))
                   nil ))
        (doc-string (when (stringp (first slot*)) (pop slot*))) )

    (unless (symbolp name)
      (macro-error "Structure name ~S must be a symbol." name) )

    (when (null name)
      (style-warn "We recommend not to use NIL as structure name.") )

  (multiple-value-bind (conc-name
                        cons-names
                        copier
                        metaclass
                        include
                        named
                        initial-offset
                        predicate
                        printer ; nil, (:print-xxx), or (:print-xxx {name})
                        include-slots
                        type )
         (parse-defstruct-options name option* env)
  (let ((typespec
          (cond
            ((not type) name)
            (predicate `(satisfies ,predicate))
            (t type) ) ))
  (labels (
    ;; compute-check-type-form
    (compute-check-type-form (slots size)
      (cond
        ((not type)
         `(check-type object ,name) )
        (predicate
         `(check-type object (satisfies ,predicate)) )
        (t
         `(unless ,(compute-typep-form slots size)
            (error 'type-error
                   :expected-type ',(compute-type-name)
                   :datum object )) )) )

    ;; compute-initfunction
    (compute-initfunction (initform)
      (if (constantp initform)
          `(constantly ,initform)
        `(lambda () ,initform) ) )

    ;; compute-predicate
    (compute-predicate (slots size)
      (cond
        ((not type)
          `(si::subclassp (class-of object) (find-class ',name)) )
        ((eq type 'list)
         `(and (consp object)
               ; (eql (si::safe-list-length object) ,size)
             ,@(loop
                  for slot in slots
                    when (eql (getf slot :name) 0)
                      collect `(eq (nth ,(getf slot :location) object)
                                    ',(getf slot :marker) ))) )
        (t
         `(and (typep object ',(compute-type-name))
               (>= (length object) ,size)
             ,@(loop
                 for slot in slots
                   when (eql (getf slot :name) 0)
                   collect `(eq (svref object ,(getf slot :location))
                                ',(getf slot :marker) ))) )) )

    ;; compute-reader-form
    (compute-reader-form (location)
      (cond
        ((not type) `(si::structure-instance-access object ,location))
        ((eq type 'list) `(nth ,location object))
        ((not (consp type)) `(svref object ,location))
        (t
          (let ((reader
                  (let ((element-type
                          (upgraded-array-element-type (second type) env) ))
                    (case element-type
                      ((t)   'svref)
                      ((bit) 'sbit)
                      ((character) 'schar)
                      (otherwise 'row-major-aref) ) ) ))
            `(,reader object ,location) ) )) )

    ;; compute-slots
    ;;  => list, sequence-index
    (compute-slots (direct-slots)
      (multiple-value-bind (slots location)
          (if (null include)
              (values nil 0)
            (compute-slots/include include) )

        (when type
          (incf location initial-offset)
          (when named
            (push `(:name 0
                    :location ,location
                    :marker ,name )
                  slots )
            (incf location) ))

        (dolist (direct-slot direct-slots)
          (setf (getf direct-slot :location) location)
          (push direct-slot slots)
          (incf location) )

        ;; Set reader-form.
        (dolist (slot slots)
          (unless (eql (getf slot :name) 0)
            (let ((location (getf slot :location)))
              (setf (getf slot :reader-form)
                (compute-reader-form location) ) )) )

        (values (nreverse slots) location) ) )

    ;; compute-slots/include
    ;;  => list, sequence-index
    ;;  Note:
    ;;      initform    not inherited
    ;;      read-only   inherited
    ;;      type        inherited
    (compute-slots/include (include)
        (declare (type class include))
        (declare (values list ext:sequence-index))
      (loop
        with location = 0
        with slots = '()
        for class in (reverse (clos:class-precedence-list include)) do
          (when (typep class 'si::structure-type)
            (incf location (slot-value class 'si::initial-offset))
            (when (slot-value class 'si::named)
              (push `(:name 0 :location ,location :marker ,(class-name class))
                    slots )
              (incf location) ))
          (loop
            for dslotd in (clos:class-direct-slots class)
            for slot-name = (clos:slot-definition-name dslotd)
            for slot = (find-slot slot-name slots) do
              (multiple-value-bind (initform initform-p read-only type)
                  (let ((override (find-slot slot-name include-slots)))
                    (if override
                        (values
                            (getf override :initform)
                            (getf override :initform-p)
                            (getf override :read-only)
                            (getf override :type) )
                      (values
                        (clos:slot-definition-initform dslotd)
                        (clos:slot-definition-initfunction dslotd)
                        (slot-value dslotd 'si::read-only)
                        (clos:slot-definition-type dslotd) )) )

                (when (and initform-p (not (constantp initform)))
                  (setq initform
                   `(funcall
                      (clos:slot-definition-initfunction
                        (si::find-effective-slot
                          (find-class ',name)
                          ',slot-name )))))

                (if (null slot)
                    (progn
                      (setq slot
                        (list :name        slot-name
                              :location    location
                              :initform    initform
                              :initform-p  initform-p
                              :read-only   read-only
                              :type        type
                              :reader-form nil ))
                      (incf location)
                      (push slot slots) )
                  (progn
                    (setf (getf slot :initform)   initform)
                    (setf (getf slot :initform-p) initform-p)
                    (when read-only
                      (setf (getf slot :read-only) read-only) )
                    (setf (getf slot :type)
                      (type-and (getf slot :type) type) )))))
          finally
            (return (values slots location)) ) )

    ;; compute-type-name
    (compute-type-name ()
      (cond
        ((not type) name)
        ((eq type 'list) 'list)
        ((not (consp type)) `simple-vector)
        (t `(simple-array ,(second type) (*))) ) )

    ;; compute-typep-form
    (compute-typep-form (slots size)
      (cond
        ((not type) `(typep object ',name))
        (predicate  `(,predicate object))
        (t (compute-predicate slots size)) ) )

    ;; find-slot
    (find-slot (slot-name slots)
      (dolist (slot slots)
        (when (eq (getf slot :name) slot-name)
          (return slot) ) ) )

    ;; make-defstruct-allocate-form
    ;;   Returns structure allocation form.
    ;;
    ;; Note: We must not use function vector for allocating vector type
    ;; structure. Since, call-arguments-limit is less than maximum number
    ;; of slots. In this case, we can initialize only half of
    ;; call-arguments-limit slots.
    ;;   (make-foo :slot1 val1 :slot2 :val2 ... :slotN valN)
    ;; where N is (truncate call-arguments-limit 2)
    ;;
    (make-alloc-form (slots size)
      (cond
        ((not type)
         `(si::allocate-structure (find-class ',name)) )
        ((eq type 'list)
         `(let ((object (make-list ,size)))
            ,@(loop for slot in slots
                when (eql (getf slot :name) 0)
                  collect `(setf (nth ,(getf slot :location) object)
                                 ',(getf slot :marker) ))
            object ) )
        (named
          `(let ((object (make-array ,size)))
            ,@(loop for slot in slots
                when (eql (getf slot :name) 0)
                  collect `(setf (svref object ,(getf slot :location))
                                 ',(getf slot :marker) ))
            object ) )
        ((not (consp type))
          `(make-array ,size) )
        (t
          `(make-array ,size :element-type ',(second type)) )) )

    ;; make-ctor-forms
    (make-ctor-forms (slots size)
      (when cons-names
        (make-defstruct-ctors
            typespec (make-alloc-form slots size) cons-names slots )) )

    ;; make-copier
    ;;  Returns defun-form of structure copy function definition.
    (make-copier (slots size)
      (cond
        ((not type)
         `(defun ,copier (object) (copy-structure object)) )
        ((eq type 'list)
         `(defun ,copier (object)
              ,(compute-check-type-form slots size)
              (copy-list object) ) )
        (t
         `(defun ,copier (object)
              ,(compute-check-type-form slots size)
              (copy-seq object) ) )) )

    ;; make-direct-slot-forms
    (make-direct-slot-forms (direct-slots)
      (loop for slot in direct-slots
        collect (make-direct-slot-form slot) ) )

    ;; make-direct-slot-form
    (make-direct-slot-form (slot)
      `(list :name         ',(getf slot :name)
             :type         ',(getf slot :type 't)
             ,@(let ((initform-p (getf slot :initform-p))
                     (initform   (getf slot :initform)) )
                 (when initform-p
                   `(:initform     ',initform
                     :initfunction ,(compute-initfunction initform) )) )
             ,@(when (getf slot :read-only) `(:read-only t)) ) )

    ;; make-predicate
    ;;  Returns defun-form of type predicate definition.
    ;; BUGBUG: It seems we should not test length of list.
    (make-predicate (slots size)
      (cond
        ((not type)
         `((declaim (ext:type-predicate ,predicate ,name))
            (defun ,predicate (object)
             (si::subclassp (class-of object) (find-class ',name)) )) )
        ((eq type 'list)
        `((defun ,predicate (object)
            (and (consp object)
                 ;(eql (si::safe-list-length object) ,size)
                 ,@(loop
                    for slot in slots
                     when (eql (getf slot :name) 0)
                       collect `(eq (nth ,(getf slot :location) object)
                                    ',(getf slot :marker) ))) )) )
        (t
        `((defun ,predicate (object)
            (and (typep object ',(compute-type-name))
                 (>= (length object) ,size)
                 ,@(loop
                    for slot in slots
                     when (eql (getf slot :name) 0)
                       collect `(eq (svref object ,(getf slot :location))
                                    ',(getf slot :marker) ))) ) ) )) )

    ;; make-printer
    (make-printer ()
     `(defmethod cl:print-object ((o ,name) s)
       ,(ecase (first printer)
          ((:print-object)
            (if (null (rest printer))
               `(si::print-structure o s)
              `(,(second printer) o s) ) )

          ((:print-function)
            (if (null (rest printer))
               `(si::print-structure o s)
             `(,(second printer) o s si::*printer-level*) ) ))
        o ) )

    ;; make-slot-accessors
    (make-slot-accessors (slots size)
      (loop
        with check-type-form = (compute-check-type-form slots size)
        for slot in slots
        for slot-name = (getf slot :name)
          when (symbolp slot-name)
            nconc (make-slot-accessor slot check-type-form) ) )

    ;; make-slot-accessor
    (make-slot-accessor (slot check-type-form)
      (let* ((slot-name   (getf slot :name))
             (read-only-p (getf slot :read-only))
             (slot-type   (getf slot :type 't))
             (reader-form (getf slot :reader-form))
             (reader
               (if (null conc-name)
                   slot-name
                 (intern (format nil "~A~A" conc-name slot-name)) ) ))
        (list*
         `(declaim (ftype (function (,typespec) ,slot-type) ,reader))
         `(declaim (inline ,reader))
         `(defun ,reader (object)
             ,check-type-form 
             ,reader-form )
          (unless read-only-p
             (list
               `(declaim (ftype (function (,slot-type ,typespec) ,slot-type)
                            (setf ,reader) ))
               `(declaim (inline (setf ,reader)))
               `(defun (setf ,reader) (value object)
                  ,check-type-form
                  ,@(unless (eq slot-type t)
                      `((check-type value ,slot-type)) )
                  (setf ,reader-form value) )))) ) )
    )
    ;;
    (let ((direct-slots (parse-defstruct-slots name slot* include env)))
    (multiple-value-bind (slots size)
        (compute-slots direct-slots)
    (let ((args
           `(',name
             ',(and include (class-name include))
             (list ,@(make-direct-slot-forms include-slots)
                   ,@(make-direct-slot-forms direct-slots) )
             :metaclass ',(class-name metaclass)
             ,@(when type
                `(:type ',type
                  :initial-offset ,initial-offset
                  :named ,named ))) ))
     `(progn
        (eval-when (:compile-toplevel)
          (%defstruct ,@args) )
        (si::%defstruct
            ,@args
            ,@(when doc-string `(:documentation ,doc-string)) )
        ,@(when predicate
           `((declaim (ftype (function (t) t) ,predicate))
             ,@(make-predicate slots size) ))
        ,@(when copier
            `((declaim (ftype (function (,typespec) ,typespec) ,copier))
              ,(make-copier slots size) ))
        ,@(make-ctor-forms slots size)
        ,@(make-slot-accessors slots size)
        ,@(when printer (list (make-printer)))
        ,(make-ignorable-form `',name) ) ) ) ) ) ) ) ) )
