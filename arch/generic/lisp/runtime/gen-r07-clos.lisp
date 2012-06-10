;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 07 Objects
;;; arch/generic/lisp/runtime/gen-r07-object.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r07-clos.lisp#2 $
;;;
;;; Description:
;;;  This file contains implemenation of lisp implementation dependent
;;;  functions for generic architecture. Thease functions are called by
;;;  lisp system independent CLOS implementation found in "lisp/clos/"
;;;  directory.
;;;
;;;     compute-added-slots
;;;     obsolete-instance-p
;;;     swap-instance-layout
;;;     update-obsolete-instance
;
(in-package :si)

;;;; compute-added-slots
;;; Description:
;;;  Returns list of slot name which isn't in old slots.
;;;
;;; Called by:
;;;     update-instance-for-different-class
;;;     update-obsolete-instance
;;;
;;; Note:
;;;   We can't use function CLASS-SLOTS for class of previous instance
;;;   when this function is called by UPDATE-OBSOLETE-INSTANCE since class
;;;   of current and previous is same.
(defun compute-added-slots (current previous)
  (loop
    for cur-slotd in (class-slots (class-of current))
    for slot-name = (slot-definition-name cur-slotd)
    with old-slotds = (ref class-description slots (classd-of previous))
    when (and (eq :instance (slot-definition-allocation cur-slotd))
              (not (find slot-name old-slotds
                         :key  #'slot-definition-name
                         :test #'eq )))
      collect slot-name ) )


;;;; obsolete-instance-p
(defun obsolete-instance-p (instance)
  (eql (ref class-description hash-code (classd-of instance)) 0) )


#|
;;;; allocate-storage
(defun allocate-storage (classd)
  (let ((instance (.allocate-instance classd)))
    (ref instance storage instance) ) )


;;;; storage-access
(defun storage-access (storage index)
    (declare (type storage storage))
    (declare (type sequence-index indx))
  (!elt 't storage index) )


;;;; storage-access
(defun (setf storage-access) (value storage index)
    (declare (type storage storage))
    (declare (type sequence-index indx))
  (setf (!elt 't storage index) value) )
|#


;;;; swap-instance-layout
;;; For change-class and update-obsolete-instance
(defun swap-instance-layout (i1 i2)
  (etypecase i1
    (funcallable-instance
      (let ((classd1  (ref funcallable-instance classd  i1))
            (storage1 (ref funcallable-instance storage i1))
            (classd2  (ref funcallable-instance classd  i2))
            (storage2 (ref funcallable-instance storage i2)) )
        (progn #+nil "Must be atomic"
          (setf (ref funcallable-instance classd  i1) classd2)
          (setf (ref funcallable-instance storage i1) storage2)
          (setf (ref funcallable-instance classd  i2) classd1)
          (setf (ref funcallable-instance storage i2) storage1) ) ) )
    (standard-object
      (let ((classd1  (ref instance classd  i1))
            (storage1 (ref instance storage i1))
            (classd2  (ref instance classd  i2))
            (storage2 (ref instance storage i2)) )
        (progn #+nil "Must be atomic"
          (setf (ref instance classd  i1) classd2)
          (setf (ref instance storage i1) storage2)
          (setf (ref instance classd  i2) classd1)
          (setf (ref instance storage i2) storage1) ) ) ))
  i1 )


;;;; update-obsolete-instance
;;;
;;; Called by:
;;;   dispatch-n
;;;
;;;     local   --> local   transfer
;;;     local   --> shared  discard
;;;     local   --> n/a     discard
;;;     shared  --> local   transfer
;;;     shared  --> shared  discard
;;;     shared  --> n/a     discard
;;;     n/a     --> local   add
;;;     n/a     --> shared  nothing to do
(defun update-obsolete-instance (instance)
  (labels (
    ;; process
    (process (instance)
        (declare (values list list list))
      (let* ((old instance)
             (new (allocate-instance (class-of instance)))
             (added (compute-added-slots new old)) )
        (multiple-value-bind (discarded plist)
            (process-discarded new old)
          (swap-instance-layout new old)
          (values added discarded plist) ) ) )

    ;; process-discarded
    ;;  Returns discarded slot name list and property list contains
    ;;  discared slot and its value.
    ;;
    ;;  This function also copy value in old storage to new storage if
    ;;  both old class and new class have instance slot with same name.
    ;;
    (process-discarded (new old)
      (loop
        with plist = '()
        with discarded = '()
        ;;
        with old-classd = (classd-of old)
        with old-slots  = (ref class-description slots old-classd)
        for old-eslotd in old-slots do
          (multiple-value-bind (discard boundp value)
              (copy-instance-local-slot new old old-eslotd)
            (when discard
              (let ((name (slot-definition-name old-eslotd)))
                (push name discarded)
                (when boundp (setf (getf plist name) value)) ) ) )
        finally
          (return (values discarded plist)) ) )
    )
    ;;
    ;; update-obsolete-instance
    ;;
    (multiple-value-bind (added-slots discarded-slots plist)
        (process instance)
      (update-instance-for-redefined-class
            instance
            added-slots
            discarded-slots
            plist ) ) ) )
