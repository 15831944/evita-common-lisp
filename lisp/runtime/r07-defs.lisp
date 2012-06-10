;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 7 Objects - Declarations
;;; runtime/r07-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r07-defs.lisp#2 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;
;
(in-package :si)

;;;; *next-methods*
;;;
;;; Description:
;;;  List of method functions. Local function call-next-method and
;;;  next-method-p referes this variable.
;;;
;;;  Effective method function binds this variable to list of emf.
;
(ext:deftlv si::*next-methods* '())


;;;; *standard-method-combination*
;
(defvar si::*standard-method-combination*
  (make-instance 'clos:standard-method-combination :type 'standard) )
