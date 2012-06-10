;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Devel - 25 Environment - CMDL
;;; devel/d25-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d00-defpackage.lisp#2 $
;;;
;;; Description:
;;;  This file contains package definition for developer module.
;
(in-package :cl-user)

(defpackage #:developer
  (:nicknames #:devel #:cmdl #:command-loop)
  (:use #:common-lisp #:extension)
  (:export
    #:defcommand
    #:execute-command
    #:prompt-for
    #:remember-value
    #:remember-values ) )
