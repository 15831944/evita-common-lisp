;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 5 Data and Control Flow
;;; lisp/runtime/r05-control.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r05-control.lisp#4 $
;;;
;;; Description:
;;;  Implements functions defined in section "5 Data and Control Flow".
;;;
;
(in-package :si)

;;;; 5.3.4 fboundp
(defun cl:fboundp (fname)
  (cond
    ((symbolp fname)
      (ref symbol function fname) )
    ((function-name-p fname)
      (let ((cell (find-setf-cell (second fname))))
        (and cell (ref setf-cell function cell)) ) )
    (t
      (error 'type-error
             :expected-type 'function-name
             :datum fname ) )) )


;;;;  5.3.3 fdefinition
(defun cl:fdefinition (fname)
  (cond
    ((symbolp fname)
      (symbol-function fname) )
    ((function-name-p fname)
      (let ((cell (find-setf-cell (second fname))))
        (or (and cell (ref setf-cell function cell))
            (error 'undefined-function :name fname) ) ) )
    (t
      (error 'type-error :expected-type 'function-name :datum fname) )) )


;;;; 5.3.3 (setf fdefinition)
(defun (setf cl:fdefinition) (fn fname)
    (check-type fn function)
  (cond
    ((symbolp fname)
      (setf (symbol-function fname) fn) )
    ((function-name-p fname)
      (let ((cell (intern-setf-cell (second fname))))
        (update-callers cell fn)
        (setf (ref setf-cell function cell) fn) ) )
    (t
      (error 'type-error :expected-type 'function-name :datum fname) )) )

