;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 22 Printer
;;; lisp/runtime3/r22-print-object2.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r22-print-object.lisp#4 $
;;;
;;; Description:
;;;  This fils contains declarations for type system.
;
(in-package :si)


;;;; print-host
(defmethod print-object ((host basic-host) stream)
  (print-unreadable-object (host stream :type t)
    (prin1 (host-name host) stream) )
  host )


;;;; print-object class-description
(defmethod cl:print-object ((classd class-description) stream)
  (print-unreadable-object (classd stream)
    (format stream "Class-Description ~S ~D"
        (class-name (ref classd class classd))
        (ref classd hash-code classd) ) )
   classd )


;;;; print-latch
(defmethod cl:print-object ((latch ext:latch) stream)
    (declare (type stream stream))
  (print-unreadable-object (latch stream :type t :identity t)
    (format stream "~S ~A" (ref latch name latch) (ref latch state latch)) )
  latch )


;;;; print-mutex
(defmethod cl:print-object ((mutex ext:mutex) stream)
    (declare (type stream stream))
  (print-unreadable-object (mutex stream :type t :identity t)
    (format stream "~S ~A" (ref mutex name mutex) (ref mutex state mutex)) )
  mutex )


;;;; print-object function
(defmethod cl:print-object ((fn function) stream)
  (labels (
    (print-class (fn)
      (let ((class (class-of fn)))
        (case class
          ((#.(find-class 'native-code-function))
            (write-string "Function" stream) )
          ((#.(find-class 'native-code-closure))
            (write-string "Closure" stream) )
          (otherwise
            (format stream "~:(~A~)" (class-name class)) )) ) )
    )
    ;;
   (print-unreadable-object (fn stream :identity t)
     (print-class fn)
     (format stream " ~S" (function-name fn)) )
   fn ) )


;;;; print-object   effective-slot-definition
(defmethod cl:print-object ((object effective-slot-definition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (and (slot-boundp object 'name)
               (slot-boundp object 'location) )
      (format stream "~@<~S ~D~:>"
              (slot-value object 'name)
              (slot-value object 'location) )))
  object )


;;;; print-object   eql-specializer
(defmethod cl:print-object ((object eql-specializer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (slot-boundp object 'object)
      (prin1 (slot-value object 'object) stream) ))
  object )


;;;; print-object generic-function
(defmethod cl:print-object ((fn generic-function) stream)
  (print-unreadable-object (fn stream :type t :identity t)
    (prin1 (function-name fn) stream) )
  fn )


;;;; print-object   method
(defmethod cl:print-object ((object method) stream)
  (print-unreadable-object (object stream :type t)
    (when (and (slot-boundp object 'generic-function)
               (slot-boundp object 'qualifiers)
               (slot-boundp object 'specializers) )
      (format stream "~S~@[ ~_~{~S~^ ~}~] ~_~S"
              (let ((gf (method-generic-function object)))
                (if (typep gf 'generic-function)
                    (generic-function-name gf)
                  gf ) )
              (method-qualifiers object)
              (mapcar #'specializer-name (method-specializers object)) )))
  object )


;;; BUGBUG: We should move restart-name to another file.
;;;; restart-name
(defun cl:restart-name (o) (check-type o restart) (ref restart name o))


;;;; print-object restart
(defmethod cl:print-object ((o restart) stream)
  (let ((printer (unless *print-escape* (ref restart report-function o))))
    (cond
     ((functionp printer)
       (funcall printer stream) )
     ((stringp printer)
       (write-string printer stream) )
     (t
        (print-unreadable-object (o stream :type t :identity t)
          (prin1 (ref restart name o) stream) ) ))
    o ) )


;;;; print-object   slot-definition
(defmethod cl:print-object ((object slot-definition) stream)
  (print-unreadable-object (object stream :type t)
    (if (slot-boundp object 'name)
        (prin1 (slot-value object 'name) stream)
      (write-string "Unnamed" stream) ))
   object )


;;;; print-object tlv-record
(defmethod cl:print-object ((o tlv-record) s)
  (print-unreadable-object (o s :type t)
    (format s "~S ~D" (ref tlv-record name o) (ref tlv-record index o)) )
  o )


;;;; print-object value-cell
(defmethod cl:print-object ((cell value-cell) stream)
    (declare (type stream stream))
  (print-unreadable-object (cell stream :identity t)
    (format stream "~:(~A~)-Cell ~S"
            (ref value-cell type cell)
            (ref value-cell name cell) ) )
  cell )


;;;; print-object setf-cell
(defmethod cl:print-object ((cell setf-cell) stream)
    (declare (type stream stream))
  (print-unreadable-object (cell stream :identity t)
    (format stream "Setf-Cell ~S"
            (ref setf-cell name cell) ) )
  cell )


;;;; print-object weak-pointer
(defmethod cl:print-object ((object ext:weak-pointer) stream)
    (declare (type stream stream))
    (declare (values ext:weak-pointer))
  (print-unreadable-object (object stream :type t :identity t)
    (princ
        (if (ref weak-pointer value object)
            "fill"
            "empty" )
        stream )
    object ) )
