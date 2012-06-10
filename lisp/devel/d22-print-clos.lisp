;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 22 Printer
;;; lisp/clos/or25-print.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d22-print-clos.lisp#3 $
;;;
;;; Description:
;;;  This file constains printer for CLOS objects:
;;;     effective-slot-definition
;;;     eql-specializer
;;;     method
;;;     slot-definition
;;;
;
(in-package :si)


;;;; print-named-object
;
(defun print-named-object (object stream)
  (print-unreadable-object (object stream :type t)
    (if (slot-boundp object 'name)
        (prin1 (slot-value object 'name) stream)
      (write-string "Unnamed" stream) )) )


;;;; print-object   class
;
(defmethod print-object ((object class) stream)
  (print-named-object object stream) )


;;;; print-object   effective-slot-definition
;
(defmethod print-object ((object effective-slot-definition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (and (slot-boundp object 'name)
               (slot-boundp object 'location) )
      (format stream "~@<~S ~D~:>"
              (slot-value object 'name)
              (slot-value object 'location) ))) )


;;;; print-object   eql-specializer
;
(defmethod print-object ((object eql-specializer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (slot-boundp object 'object)
      (prin1 (slot-value object 'object) stream) )) )


;;;; print-object   generic-function
;
(defmethod print-object ((object generic-function) stream)
  (print-named-object object stream) )


;;;; print-object   method
;
(defmethod print-object ((object method) stream)
  (labels (
    (print-initialized ()
      (print-unreadable-object (object stream :type t)
          (format stream "~S~@[ ~_~{~S~^ ~}~] ~_~S"
                  (let ((gf (method-generic-function object)))
                    (if (typep gf 'generic-function)
                        (generic-function-name gf)
                      gf ) )
                  (method-qualifiers object)
                  (mapcar #'specializer-name
                    (method-specializers object) ))) )
    )
    ;;
  (if (and (slot-boundp object 'generic-function)
           (slot-boundp object 'qualifiers)
           (slot-boundp object 'specializers) )
      (print-initialized)
    (call-next-method) )
  object ) )


;;;; print-object   slot-definition
;
(defmethod print-object ((object slot-definition) stream)
  (print-named-object object stream) )
