;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 50 Extensions - Platform
;;; runtime/r50-platform.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r50-platform.lisp#2 $
;;;
;;; Description:
;;;  This function contains extension functions.
;;;
;
(in-package :ext)

(defvar si::*env-table*)
(defvar si::*env-table-state*)
(defvar si::*env-table-latch*)

;;;; current-directory
;;;
;;; Syntax:
;;;     current-directory &optional dirve => pathname
;;;
;;; Arguments and Values:
;;;     drive       nil or 1 through 26.
;;;     pathname    phyisical pathname.
;
(defun ext:current-directory (&optional drive)
  (values (parse-namestring (si::.current-directory drive)
                            (si::make-windows-host (short-site-name)))) )


;;;; (setf current-directory)
;
(defun (setf ext:current-directory) (dirspec &optional drive)
  (let ((pathname (si::ensure-physical-pathname dirspec)))
    (setf (si::.current-directory drive) (directory-namestring pathname)) )
  dirspec )


;;;; ext:getenv
;
(defun ext:getenv (name)
    (declare (type string name))
    (declare (values (or string null)))
  (with-latch (si::*env-table-latch* :shared)
    (values (gethash name si::*env-table*)) ) )


;;;; (setf ext:getenv)
;
(defun (setf ext:getenv) (value name)
    (declare (type string value))
    (declare (type string name))
    (declare (values string))
  (setf si::*env-table-state* 'setf)
  (with-latch (si::*env-table-latch* :exclusive)
    (setf (gethash name si::*env-table*) value) ) )
