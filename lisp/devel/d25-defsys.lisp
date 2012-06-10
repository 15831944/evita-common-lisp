;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 25 Environment - CMDL
;;;; dev/d25-cmdl.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-defsys.lisp#2 $
;;;
;;; Description:
;;;  This file contains cmdl commands for defsystem facility.
;;;
;;;     clean-system
;;;     compile-system
;;;     load-system
;
(in-package :devel)

(defvar *last-system* nil)

(defun get-system-name ()
  (or *last-system*
      (prompt-for "System name: ") ) )


;;;; clean-system
;
(defcommand clean-system (&optional ('system (get-system-name)))
  "Clean system."
  (setq *last-system* system)
  (ds:clean-system system) )


;;;; compile-system
;
(defcommand compile-system (&optional ('system (get-system-name)))
  "Compile system."
  :alias cs
  (setq *last-system* system)
  (ds:compile-system system) )


;;;; load-system
;
(defcommand load-system (&optional ('system (get-system-name)))
  "Load system."
  :alias lds
  (setq *last-system* system)
  (ds:load-system system) )
