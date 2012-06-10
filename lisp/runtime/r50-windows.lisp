;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 50 Extensions - Windows
;;; runtime/r50-windows.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r50-windows.lisp#2 $
;;;
;;; Description:
;;;  This function contains extension functions.
;;;
;
(in-package :win)

(defvar *command-line*)
(defvar *environ*)

(defun si::initialize-platform ()
  (labels (
    ;; init-env-table
    (init-env-table ()
       (let ((htb si::*env-table*))
         (clrhash htb)
         (loop for (name . value) in *environ* do
           (setf (gethash name htb) value) )
         (setq si::*env-table-state* nil) ) )
    )
    ;;
    (init-env-table) ) )
