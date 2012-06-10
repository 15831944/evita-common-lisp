;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 10 Symbols - Low
;;; lisp/runtime/r10-low.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r10-symbol.lisp#6 $
;;;
;
(in-package :si)


;;;; 10.2.17 boundp
(defun cl:boundp (sym)
  (let ((val
          (let ((cell (find-value-cell sym)))
            (etypecase cell
              (value-cell (ref value-cell value cell))
              (tlv-record (tlv (ref tlv-record index cell)))
              (null '#.(unbound-marker)) ) ) ))
    (not (eq '#.(unbound-marker) val)) ) )


;;;; 10.2.5 make-symbol
(defun cl:make-symbol (name)
  (typecase name
    (simple-string (!make-symbol name))
    (string        (make-symbol (copy-seq name)))
    (otherwise     (error 'type-error :datum name :expected-type 'string)) ) )


;;;; 10.2.18 makunbound
(defun cl:makunbound (sym)
  (let ((cell (find-value-cell sym)))
    (etypecase cell
      (value-cell
        (setf (ref value-cell value cell) #.(unbound-marker)) )
      (tlv-record
        (setf (tlv (ref tlv-record index cell)) #.(unbound-marker)) )
      (null) )
    sym ) )


;;;; 12.2.10 (setf symbol-function)
(defun (setf cl:symbol-function) (fn sym)
    (check-type fn function)
    (check-type sym symbol)
  (update-callers sym fn)
  (setf (ref symbol function sym) fn) )


;;;; 10.2.13 symbol-plist
(defun cl:symbol-plist (symbol)
    (declare (type symbol symbol))
    (declare (values list))
  (ref symbol plist symbol) )


;;;; 10.2.13 (setf symbol-plist)
(defun (setf cl:symbol-plist) (plist symbol)
    (declare (type symbol symbol))
    (declare (values list))
  (setf (ref symbol plist symbol) plist) )
