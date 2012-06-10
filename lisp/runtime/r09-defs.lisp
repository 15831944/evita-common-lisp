;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 9 Conditions
;;; lisp/runtime/r09-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r09-defs.lisp#2 $
;;;
;
(in-package :si)

;;;; 9.2.31 restart
;
(defstruct (cl:restart (:print-object print-restart))
  (name                 nil     :type symbol :read-only t)
  (function             nil     :type (or symbol function) :read-only t)
  (test-function        nil     :type (or symbol function) :read-only t)
  (report-function      nil     :type (or symbol function) :read-only t)
  (interactive-function nil     :type (or symbol function) :read-only t) )


;;;; print-restart
;
(defun print-restart (restart stream)
  (let ((printer (unless *print-escape* (restart-report-function restart))))
    (cond
     ((functionp printer)
       (funcall printer stream) )
     ((stringp printer)
       (write-string printer stream) )
     (t
        (print-unreadable-object (restart stream :type t :identity t)
          (prin1 (restart-name restart) stream) ) )) ) )
