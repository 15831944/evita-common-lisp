;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 50 Extensions - Lock
;;; runtime/r50-lock.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r50-lock.lisp#2 $
;;;
;;; Description:
;;;  This function contains lock related functions.
;;;
;
(in-package :si)

;;;; make-latch
;
(defun ext:make-latch (name)
    (declare (values ext:latch))
  (let ((latch (.allocate-record #.(class-description 'ext:latch))))
    (setf (.latch-name   latch) name)
    (setf (.latch-thread latch) 0)
    (setf (.latch-state  latch) :free)
    (setf (.latch-count  latch) 0)
    (setf (.latch-spin-lock latch) 0)
    latch ) )



;;;; make-mutex
;
(defun ext:make-mutex (name)
    (declare (values ext:mutex))
  (let ((mutex (.allocate-record (class-description 'ext:mutex))))
    (setf (.mutex-name   mutex) name)
    (setf (.mutex-thread mutex) 0)
    (setf (.mutex-state  mutex) 0)
    mutex ) )
