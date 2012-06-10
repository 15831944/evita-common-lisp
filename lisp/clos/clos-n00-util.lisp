;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;  evcl - devel - 25 Environment - Functions
;;;; lisp/evm/evm-d00-fns.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/clos-n00-util.lisp#2 $
;;;
;;; Description:
;;;  This file contains EVM specific functions:
;;;     function-arity
;;;     funciton-frame-variable
;
(in-package :si)


;;;; validate-make-instance-initargs
;;;
;;; Description:
;;;  Used for checking initargs during compilation.
;
(defun validate-make-instance-initargs (class args env)
  (labels (
    ;; check-initarg
    (validate-initarg (key)
      (loop
        for slotd in (class-slots class)
        for initargs = (slot-definition-initargs slotd) do
          (when (member key initargs :test #'eq)
            (return t) )) )
    )
    ;;
    (block nil
      (unless (class-finalized-p class)
        (return t) )
      (loop
        with valid-p = t
        for key in args by #'cddr do
          (multiple-value-bind (constantp value)
              (c::constant-value-p key env)
            (when (and constantp (not (validate-initarg value)))
              (xc::style-warn
                    "Class ~S doesn't accept ~S as initarg."
                    (class-name class) value )
              (setq valid-p nil) ) )
        finally (return valid-p) )) ) )
