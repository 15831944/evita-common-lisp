;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - genesis - Load Script
;;; lisp/genesis/g05-runtime.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/genesis/g05-runtime.lisp#7 $
;;;
;;; Description:
;;;  TBD
;
(in-package :si)

;;; for caar, ... cddddr
(define-compiler-macro cl:make-string
        (n &rest args &key initial-element (element-type 'character))
    (declare (ignore initial-element))
  (when (or (null args) (equal element-type '(quote character)))
    `(.allocate-string ,n) ) )
