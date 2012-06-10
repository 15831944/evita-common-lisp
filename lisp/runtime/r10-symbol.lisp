;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 10 Symbols
;;; lisp/runtime/r10-symbol.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r10-symbol.lisp#2 $
;;;
;;; Description:
;;;  This fils contains following functions:
;;;     boundp              10.2.17 intrinsic
;;;     copy-symbol         10.2.6
;;;     gensym              10.2.7
;;;     *gensym-counter*    10.2.8  (per-thread)
;;;     gentemp             10.2.9  (deprecated)
;;;     get                 10.2.15
;;;     make-symbol         10.2.5  native  BAD!
;;;     makunbound          10.2.18 intrinsic
;;;     keywordp            10.2.4
;;;     remprop             10.2.16
;;;     set                 10.2.19
;;;     symbol-function     10.2.10
;;;     symbol-name         10.2.11
;;;     symbol-package      10.2.12
;;;     symbol-plist        10.2.13
;;;     symbolp             10.2.3  boot
;;;     symbol-value        10.2.14 vm-op
;;;     unbound-variable    10.2.20 condition)
;
(in-package :si)

;;;; 10.2.6 copy-symbol
(defun cl:copy-symbol (symbol &optional copy-prop-p)
  (let ((new-symbol (make-symbol (symbol-name symbol))))
    (when copy-prop-p
      (when (boundp symbol)
        (setf (symbol-value new-symbol) (symbol-value symbol)) )

      (when (fboundp symbol)
        (setf (symbol-function new-symbol) (symbol-function symbol)) )

      (setf (symbol-plist new-symbol) (copy-list (symbol-plist symbol))) )

    new-symbol ) )


;;;; 10.2.7 gensym
(defun cl:gensym (&optional (x "G"))
  (multiple-value-bind (prefix counter)
      (etypecase x
        (string
          (let ((counter *gensym-counter*))
            (setq *gensym-counter* (1+ counter))
            (values x counter) ) )

        ((integer 0 *)
          (values "G" x) ))
    (make-symbol (format nil "~A~D" prefix counter)) ) )


;;;; 10.2.15 get
(defun cl:get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default) )


(defun (setf cl:get) (new-value symbol indicator &optional default)
    (declare (ignore default))
  (setf (symbol-plist symbol)
        (plist-put (symbol-plist symbol) indicator new-value) )
  new-value )


;;;; 10.2.4 keywordp
(defun cl:keywordp (object)
  (and object
       (symbolp object)
       (eq (symbol-package object) '#.(symbol-package :key)) ) )


;;;; 10.2.16 remprop
(defun cl:remprop (symbol indicator)
  (multiple-value-bind (plist removed-p)
      (plist-rem (symbol-plist symbol) indicator)
    (setf (symbol-plist symbol) plist)
    removed-p ) )


;;;; 10.2.19 set
(defun cl:set (symbol value)
  (setf (symbol-value symbol) value) )
