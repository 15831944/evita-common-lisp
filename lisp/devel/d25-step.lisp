;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Developer - 25 Environment
;;; devl/d25-step.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-step.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     step    25.2.9
;;;
;;; Note: We can't trace functions which use non-standard calling convention.
;;; BUGBUG: NYI: Detect functio in R/O area, and tell user that we can't trace
;;; functions in R/O area.
;
(in-package :devel)

;;;; 25.2.9 step
;
(defmacro step (form)
  (error "NYI: step") )
