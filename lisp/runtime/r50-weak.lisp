;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 50 Extensions - Weak Object
;;; runtime/r50-weak.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r50-weak.lisp#4 $
;;;
;;; Description:
;;;  This function contains extension functions.
;;;
;
(in-package :si)

;;;; make-weak-pointer
;
#+nil
(defun ext:make-weak-pointer (object)
    (declare (values ext:weak-pointer))
  (let ((wp (.allocate-record (class-description 'ext:weak-pointer))))
    (setf (weak-pointer-value wp) object)
    (.register-weak-object wp) ) )


;;;; weak-cons
;
#+nil
(defun ext:weak-cons (car cdr)
    (declare (values ext:weak-cons))
  (let ((wc (.allocate-record (class-description 'ext:weak-cons))))
    (setf (.weak-cons-car wc) car)
    (setf (.weak-cons-cdr wc) cdr)
    (.register-weak-object wc) ) )
