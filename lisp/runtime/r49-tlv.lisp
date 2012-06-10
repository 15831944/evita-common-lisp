;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 49 Internal - Thread Local variable
;;; runtime/r49-tlv.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r49-tlv.lisp#2 $
;;;
;;; Description:
;;;  This function contains internal functions:
;;;     find-tlv
;;;     get-tlv
;;;     intern-tlv
;;;
;
(in-package :si)

(defvar *tlv-map*)

;;;; find-tlv
;
(defun find-tlv (name &optional (error-p t))
    (declare (type symbol name))
    (declare (values (or simple-vector null)))
  (let ((tlv-rec
          (with-latch (*value-table-latch* :shared)
            (gethash/eq name *value-table*) ) ))
    (or tlv-rec
        (when error-p (error "~S isn't thread local variable." name)) ) ) )


;;;; get-tlv
;;;
;;; See: vm_memory.h for definition of VmTLVMap
;
(defun get-tlv (index)
    (declare (type sequence-index index))
    (declare (values simple-vector))
  (with-latch ((object-ref *tlv-map* 2) :shared)
    (let ((count  (object-ref *tlv-map* 3))
          (vector (object-ref *tlv-map* 4)) )
      (if (<= 0 index (1- count))
          (svref vector index)
        (error "Invalid TLV index: ~D" index) ) ) ) )


;;;; intern-tlv
;
(defun intern-tlv (name
                   &optional (initform nil initform-p) (init-p initform-p) )
    (declare (type symbol name))
    (declare (values symbol))
  (labels (
    (intern-tlv-aux (name)
        (declare (type symbol name))
        (declare (values (member intern nil error)))
      (with-latch (*value-table-latch*)
        (let ((tlv-rec (gethash/eq name *value-table*)))
          (etypecase tlv-rec
            (null
              (let ((tlv-index
                      (with-latch ((object-ref *tlv-map* 2))
                        (incf (object-ref *tlv-map* 3)) ) ))
                (setf (gethash/eq name *value-table*)
                  (vector name tlv-index initform init-p) )
                'intern ) )
            (simple-vector
              (setf (svref tlv-rec 2) initform)
              (setf (svref tlv-rec 3) init-p)
              nil )
            (value-cell
              'error )) ) ) )
    )
    ;;
    (ecase (intern-tlv-aux name)
      ((intern)
        (synchronize-tlv) )
      ((nil))
      ((error)
        (error
            "Can't make special variable ~S as thread local variable."
            name )) )
    name ) )
