;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 24 System Construction - Module
;;; runtime/r24-module.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r24-module.lisp#2 $
;;;
;;; Description:
;;;  TBD
;;;
;
(in-package :si)

(defvar *modules*)

;;;; 24.2.11 provide
;
(defun cl:provide (name)
  (setq name (string name))
  (push name *modules*)
  name )


;;;; 24.2.11 require
;
(defun cl:require (name &optional list)
  (setq name (string name))
  (loop
    (cond
      ((find name *modules* :test #'string=)
        (return name) )
      ((null list)
        (return nil) )
      (t
        (let ((pathname (make-pathname :name name :defaults (pop list))))
          (ignore-errors (load pathname)) ) ))) )
