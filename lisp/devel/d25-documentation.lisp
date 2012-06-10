;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 25 Environemnt - Documentation
;;; lisp/devel/dd25-documentation.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-documentation.lisp#2 $
;;;
;;; Description:
;;;
;;; Public Functions:
;;;     documentation 25.2.15
;;;
;;; Standard doc-type:
;;      compiler-macro
;;;     function
;;;     method-combination
;;;     setf
;;;     structure
;;;     t
;;;     structure
;;;     type
;;;     variable
;
(in-package :devel)

(defvar *documentation-latch* (ext:make-latch 'documentation))

;;;; *documentation-table*
;;;
;;; Description:
;;;  Contains mapping to symbol to alist.
;
(defvar *documentation-table* (make-hash-table :test 'eq))


(defgeneric cl:documentation (x doc-type)
  (:argument-precedence-order doc-type x) )


(defgeneric (setf cl:documentation) (new-value x doc-type)
  (:argument-precedence-order doc-type x new-value) )


;;;; documentation symbol
;
(defmethod cl:documentation ((x symbol) doc-type)
  (ext:with-latch (*documentation-latch* :shared)
    (cdr (assoc doc-type (gethash x *documentation-table*) :test #'eq)) ) )


(defmethod cl:documentation (x doc-type)
    (declare (ignore x doc-type))
  nil )


;;;; (setf documentation) symbol
;
(defmethod (setf cl:documentation) (new-value (x symbol) doc-type)
    (check-type new-value string)
  (ext:with-latch (*documentation-latch* :exclusive)
    (let ((alist (gethash x *documentation-table*)))
      (if (null alist)
          (unless (string= new-value "")
            (setf (gethash x *documentation-table*)
              (list (cons doc-type new-value)) ))
        (let ((cons (assoc doc-type alist :test #'eq)))
          (cond
            (cons
              (if (not (string= new-value ""))
                  (setf (cdr cons) new-value)
                (setf (gethash x *documentation-table*)
                  (delete doc-type alist :key #'car) )) )
            ((not (string= new-value ""))
              (nconc alist (list (cons doc-type new-value))) )) ))
      new-value ) ) )


;;; Note: ANSI-CL defines new-value is string. There is no way to remove
;;; documentation from object. Empty string may be used for remove
;;; documentation.
;;;
;;; BUGBUG: We should check new-value is string.
;
(defmethod (setf cl:documentation) (new-value x doc-type)
    (declare (ignore x doc-type))
  new-value )
