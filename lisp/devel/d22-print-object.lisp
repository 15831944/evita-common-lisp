;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 22 Printer
;;; lisp/clos/or25-print.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d22-print-object.lisp#3 $
;;;
;;; Description:
;;;  This file constains printers.
;
(in-package :si)

;;;; print-object c::environment
;
(defmethod cl:print-object ((env c::environment) stream)
  (print-unreadable-object (env stream :type t :identity t)
    (cond
      ((null (.env-outer env))
        (write-string "runtime" stream) )
      ((not (.env-local-p env))
        (write-string "compilation" stream) )
      (t
        (write-string "local" stream) )) ) )
