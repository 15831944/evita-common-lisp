;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 25 Environment
;;; macro/m25-env.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m25-env.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     step            NYI
;;;     time            25.2.10
;;;     trace           NYI
;;;     untrace         NYI
;
(in-package :xc)

;;;; 25.2.10 time
;;;
;;; Syntax:
;;;     time form => {result}*
;
(defmacro time (form)
  `(si::time-it #'(lambda () ,form)) )
