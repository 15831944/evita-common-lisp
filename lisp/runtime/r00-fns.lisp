;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - Macro Loaders
;;; runtime/r00-fns.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r00-fns.lisp#2 $
;;;
;;;
;;; Description:
;;;  This file contains implementation of following system internal
;;;  functions:
;;;     CAN-NOT-HAPPEN
;;;     function-name-p
;
(in-package :si)

;;;; CAN-NOT-HAPPEN
;
(defun CAN-NOT-HAPPEN ()
  (error "CAN NOT HAPPEN!") )


;;;; function-name-p
;;;
;;; Note: We can use NIL and keyword as function name.
;
(defun function-name-p (name)
  (or (symbolp name)
      (and (consp name)
           (eq (car name) 'setf)
           (consp (cdr name))
           (symbolp (cadr name))
           (null (cddr name)) )) )


;;;; runtime-program-error
;
#+nil
(defun runtime-program-error (ctrl-string &rest args)
  (error 'simple-program-error
         :format-control ctrl-string
         :format-arguments args ) )


;;;; runtime-type-error
;
(defun runtime-type-error (expected-type datum ctrl-string &rest args)
  (error 'simple-type-error
         :expected-type     expected-type
         :datum             datum
         :format-control    ctrl-string
         :format-arguments  args ) )


;;; BUGBUG: We should define ext:required in another file.
(defun ext:required ()
  (error "Required argument is not specified.") )

;;; BUGBUG: We should define ext:unspecified in another file.
(defun ext:unspecified () nil)
