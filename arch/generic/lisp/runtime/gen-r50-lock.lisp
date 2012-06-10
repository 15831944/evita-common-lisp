;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 50 Extension - Locks
;;; arch/generic/lisp/runtime/r50-lock.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r50-lock.lisp#1 $
;;;
;;; Description:
;;;  This fils contains declarations for type system.
;
(in-package :si)

;;;; make-latch
(defun ext:make-latch (name)
    (declare (type symbol name))
    (declare (values latch))
  (let ((latch (.allocate-record #.(class-description 'latch))))
    (setf (ref latch lock-count latch) 0)
    (setf (ref latch name       latch) name)
    (setf (ref latch state      latch) nil)
    (setf (ref latch spinlock   latch) nil)
    (setf (ref latch thread     latch) nil)
    latch ) )


;;;; make-mutex
(defun ext:make-mutex (name)
    (declare (type symbol name))
    (declare (values mutex))
  (let ((mutex (.allocate-record #.(class-description 'mutex))))
    (setf (ref mutex name       mutex) name)
    (setf (ref mutex state      mutex) nil)
    (setf (ref mutex thread     mutex) nil)
    mutex ) )
