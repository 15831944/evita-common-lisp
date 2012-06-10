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
;;;  Package definition of "Editor".
;
(in-package :cl-user)

(defpackage :editor
  (:nicknames :ed)
  (:import-from :system
    #:sequence-end
    #:sequence-index
    #:string-data
    )
  (:import-from :extension
    #:make-weak-pointer
    #:required
    #:weak-pointer
    #:weak-pointer-value )
 )
