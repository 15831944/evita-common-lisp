;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - Class Declaration
;;; lisp/clos/o00-defclass.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o10-defclass.lisp#3 $
;;;
;;; Description:
;;;  This file contains class declarations defined in MOP.
;;;
;
(in-package :si)

(defclass clos:metaobject () ())

(defclass plist-mixin ()
  ((plist
        :type       list
        :initform   '() )) )

(defclass dependee-mixin (plist-mixin) ())

;;;; MOP specializer
;;;
;;; Note: For static slot optimization, we put dependee-mixin onto direct
;;; super class of specializer instead of class. This allows us, locations
;;; of flags and direct-method are equal to both of class and eql-specializer.
;;;
;
(defclass clos:specializer (metaobject dependee-mixin)
  ((flags                                       ; +3
        :type       fixnum
        :initform   0 )
   (direct-methods                              ; +4
        :type       list
        :initform   '()
        :reader     clos:specializer-direct-methods )) )


;;;; eql-specializer
;
(defclass clos:eql-specializer (specializer)
  ((object                                      ; +5
        :initarg    :object
        :reader     clos:eql-specializer-object )) )


;;;; class
(defclass class (specializer)
  ((name                                        ; +5
        :type       symbol
        :initarg    :name
        :initform   nil
        :accessor   class-name )
   (direct-superclasses                         ; +6
        :type       list
        ;:initarg    :direct-superclasses
        :initform   '()
        :reader     clos:class-direct-superclasses )
   (direct-subclasses                           ; +7
        :type       list
        ;:initarg    :direct-subclasses
        :initform   '()
        :reader     clos:class-direct-subclasses )
   (class-precedence-list                       ; +8
        :type       list
        :initform   nil )
   (instance-description                        ; +9
        :type       si:class-description
        :initform   nil )
   (direct-slots                                ; +10
        :type       list
        ;:initarg    :direct-slots
        :initform   '()
        :reader     clos:class-direct-slots )
   (slots                                       ; +11
        :type       list
        :initform   '() )
   (prototype                                   ; +12
        :initform   nil )) )


;;;; built-in-class
;
(defclass cl:built-in-class (class) ())


;;;; standard-base-class
;;; Description:
;;;  Implementation specific common superclass of the classes standard-class
;;;  and funcallable-standard-class. This class is act as like as "std-class"
;;;  in PCL.
(defclass standard-base-class (class) ())

;;;; standard-class
;;;; standard-object
;
(defclass cl:standard-class (standard-base-class) ())
(defclass cl:standard-object (t) ())

(defclass clos:forward-referenced-class (class) ())

;;;; funcallable-standard-class
;
(defclass clos:funcallable-standard-class (standard-base-class) ())


;;;; funcallable-standard-object
;
(defclass clos:funcallable-standard-object
    (standard-object funcallable-instance)
  ()
  (:metaclass funcallable-standard-class) )


;;;; generic-function
;;;
;;; Note:
;;; argument-precedence-order is stored in param-info.
;;; declarations is stored in plist.
;;; lambda-list is stored in param-info.
;
(defclass cl:generic-function (metaobject
                               funcallable-standard-object
                               dependee-mixin )
  ((methods                                         ; [1]
        :type       list
        :initarg    :methods
        :initform   '()
        :reader     generic-function-methods )
   (name                                            ; [2]
        :type       function-name
        :initarg    :name
        :initform   nil
        :accessor   generic-function-name )
   (method-class                                    ; [3]
        :type       class
        :initarg    :method-class
        :initform   nil
        :reader     generic-function-method-class )
   (method-combination                              ; [4]
        :type       method-combination
        :initarg    :method-combination
        :initform   nil
        :reader     generic-function-method-combination )
   (param-info                                       ; [5]
        :type       (or null param-info)
        :initform   nil ) )
  (:metaclass funcallable-standard-class) )


;;;; standard-generic-function
;;;
;;; Note: doc-string is stored in another place.
;;; Note: default values of method-class and method-combination are
;;; suppiled by ensure-generic-function-using-class instead of
;;; default-initargs defclass option.
;
(defclass cl:standard-generic-function (generic-function)
  ()
  (:metaclass funcallable-standard-class) )


;;;; method
;;;
;;; Note: doc-string is stored in another place.
;
(defclass cl:method (metaobject) ())


;;;; standard-method
;
(defclass cl:standard-method (method plist-mixin)
  ((generic-function                            ; +4
        ;; BUGBUG: Why do we allow nil for method-generic-function?
        :type       (or generic-function null)
        :initarg    :generic-function
        :initform   nil
        :reader     method-generic-function )
   (specializers                                ; +5
        :type       list
        :initarg    :specializers
        :initform   nil
        :reader     method-specializers )
   (qualifiers                                  ; +6
        :type       list
        :initarg    :qualifiers
        :initform   '()
        :reader     method-qualifiers )
   (lambda-list                                 ; +7
        :type       list
        :initarg    :lambda-list
        :initform   nil
        :reader     method-lambda-list )
   (function                                    ; +8
        ;; BUGBUG: Why do we allow nil for method-function?
        :type       (or function null)
        :initarg    :function
        :initform   nil
        :reader     method-function )) )

;;;; standard-accessor-method
;
(defclass standard-accessor-method (standard-method)
  ((slot-definition
        :type       direct-slot-definition
        :initarg    :slot-definition
        :reader     accessor-method-slot-definition ) ) )


;;;; standard-reader-method
;
(defclass standard-reader-method (standard-accessor-method) ())


;;;; standard-writer-method
;
(defclass standard-writer-method (standard-accessor-method) ())


;;;; slot-definition
;;;
;;; Note: For static optimization, we put allocation on slot-definition rather
;;; than class standard-slot-definition
;
(defclass slot-definition (metaobject)
  ((name                                    ; +2
        :type       symbol
        :initarg    :name
        :initform   nil
        :reader     slot-definition-name )
   (allocation                              ; +3
        :type       (or (eql :instance) class)
        :initarg    :allocation
        :initform   :instance )
   (type                                    ; +4
        :type       (or symbol list)
        :initform   't
        :initarg    :type
        :reader     slot-definition-type )
   (initargs                                ; +5
        :type       list
        :initform   '()
        :initarg    :initargs
        :reader     slot-definition-initargs )
   (initform                                ; +6
        :initarg    :initform
        :initform   nil
        :reader     slot-definition-initform )
   (initfunction                            ; +7
        :type       (or (eql quote) function)
        :initform   nil
        :initarg    :initfunction
        :reader     slot-definition-initfunction )) )


;;;; direct-slot-definition
;;;
;;; Note: In PCL, readers and writers in class slot-definition, but MOP
;;; doesn't requires to do so.
;
(defclass direct-slot-definition (slot-definition)
  ((readers                                 ; +8
        :type       list
        :initform   '()
        :initarg    :readers
        :reader     slot-definition-readers )
   (writers                                 ; +9
        :type       list
        :initform   '()
        :initarg    :writers
        :reader     slot-definition-writers )) )


;;;; effective-slot-definition
;
(defclass effective-slot-definition (slot-definition)
  ((location                                ; +8
        :type       (or null cons fixnum)
        :initform   nil
        :reader     slot-definition-location )) )


(defclass standard-slot-definition (slot-definition) ())

(defclass standard-direct-slot-definition
    (standard-slot-definition direct-slot-definition) () )

(defclass standard-effective-slot-definition
    (standard-slot-definition effective-slot-definition) () )


;;;; method-combination
;;;
;;; Description:
;;;  An abstract class.
;
(defclass cl:method-combination (metaobject) ())


;;;; standard-method-combination
;;;
;;; Description:
;;;  A class for standard method-combination and super class of
;;;  long and short method-combination.
;;;
;
(defclass standard-method-combination (method-combination)
  ((type
    ;:reader     method-combination-type
    :initarg    :type )
   (options
    ; :reader     method-combination-options
    :initarg    :options
    :initform   nil )) )


;;;; long-method-combination
;
(defclass long-method-combination (standard-method-combination) ())


;;;; short-method-combination
;
(defclass short-method-combination (standard-method-combination)
  ((operator
    :type       symbol
    :initarg    :operator )
   (identity-with-one-argument
    :type       boolean
    :initarg    :identity-with-one-argument ) ))




;;;; structure-class
;;;; structure-object
;
(defclass cl:structure-class (class) ())
(defclass cl:structure-object (t) () (:metaclass structure-class))

(defclass structure-slot-definition (slot-definition) ())

;;;; structure-direct-slot-definition
;
(defclass structure-direct-slot-definition
    (structure-slot-definition direct-slot-definition)
  ((read-only                           ; [8]
        :initarg  :read-only
        :initform nil
        :type     boolean )) )

;;;; structure-effective-slot-definition
;
(defclass structure-effective-slot-definition
    (structure-slot-definition effective-slot-definition)
 () )


;;;; si::structure-type
;
(defclass si::structure-type (structure-class)
  ((initial-offset
      :initarg  :initial-offset
      :type     ext:sequence-index
      :initform 0 )
   (named
      :initarg  :named
      :type     boolean
      :initform 't )
   (type
      :initarg :type
      :type ext:type-specifier
      :initform (ext:required) )) )


;;;; list-structure-type
;
(defclass si::list-structure-type (si::structure-type) ())


;;;; vector-structure-type
;
(defclass si::vector-structure-type (si::structure-type) ())

