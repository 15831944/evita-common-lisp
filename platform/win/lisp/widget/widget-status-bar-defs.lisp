;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Windowing - Status Bar
;;; editor/win-defs-status-bar.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/platform/win/lisp/widget/widget-status-bar-defs.lisp#1 $
;;;
;;; Description:
;;;  Class definition of Text Buffer Basic.
;
(in-package :widget)

;;;; Status Bar
(defclass status-bar (widget) () (:metaclass structure-class))

(declaim
  (ftype (function (list status-bar) list)
    (setf status-bar-parts) )

  (ftype (function (t status-bar) list)
    (setf status-bar-simple-p) )

  (ftype (function (simple-string status-bar &optional fixnum) list)
    (setf status-bar-text) )

 ) ;declaim
