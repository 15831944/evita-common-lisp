;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 08 Structure
;;; arch/generic/lisp/runtime/gen-r08-classd.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r08-struct.lisp#4 $
;;;
;;; Description:
;;;  TBD
;
(in-package :si)

;;;; allocate-structure
(defun allocate-structure (class &rest initargs)
    (declare (values structure-object))
    (declare (type structure-class class))
    (declare (ignore initargs))
  (labels (
    ;; make-prototype
    (make-prototype ()
      (let* ((st (ref instance storage class))
             (p  (.allocate-structure
                    (ref structure-class instance-description st) ) ))
        (loop
          for eslotd in (ref structure-class slots st)
          for est = (ref instance storage eslotd)
          for loc = (ref structure-effective-slot-definition location est)
          for initfunction =
            (ref structure-effective-slot-definition initfunction est)
            when (functionp initfunction) do
              (setf (structure-instance-access p loc)
                (funcall initfunction) ) )
        (setf (ref structure-class prototype st) p) ) )
    )
    ;;
    (copy-structure
            (or (ref structure-class prototype
                        (ref instance storage class) )
                (make-prototype) )) ) )


;;;; cl:copy-structure
(defun cl:copy-structure (src)
    (declare (values structure-object))
    (declare (type structure-object src))
  (loop
    with classd = (classd-of src)
    with dst = (.allocate-structure classd)
    for eslotd in (ref class-description slots classd)
    for st  = (ref instance storage eslotd)
    for loc = (ref structure-effective-slot-definition location st) do
      (setf (structure-instance-access dst loc)
              (structure-instance-access src loc) )
    finally
      (return dst) ) )
