;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 50 Extension - Weak Object
;;; lisp/runtime/r50-finalization.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime3/r50-weak.lisp#4 $
;;;
;;; Description:
;;;  This fils contains declarations for type system.
;
(in-package :si)

;;;; make-weak-pointer
(defun ext:make-weak-pointer (obj)
  (let ((wp (.allocate-weakobj #.(class-description 'weak-pointer))))
    (setf (ref weak-pointer value wp) obj)
    wp ) )

;;;; weak-pointer-value
(defun ext:weak-pointer-value (wp)
    (check-type wp weak-pointer)
  (ref weak-pointer value wp) )
