;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Windowing - Status Bar
;;; editor/win-status-bar.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/platform/win/lisp/widget/widget-status-bar-fns.lisp#1 $
;;;
;;; Description:
;;;  Class definition of Text Buffer Basic.
;
(in-package :widget)

;;;; status-bar-parts
(defun status-bar-parts (status-bar)
  (let* ((nparts (win32::|SendMessage/ii| status-bar win32::SB_GETPARTS 0 -1))
         (partv  (make-array nparts :element-type '(signed-byte 32))) )
    (win32::|SendMessage/is| status-bar win32::SB_GETPARTS nparts partv)
    (coerce partv 'list) ) )


;;;; (setf status-bar-parts)
(defun (setf status-bar-parts) (parts status-bar)
  (let* ((nparts (length parts))
         (partv  (make-array nparts
                      :element-type '(signed-byte 32)
                      :initial-contents parts )) )
      (win32::|SendMessage/is| status-bar win32::SB_SETPARTS nparts partv)
      parts ) )


;;;; (setf status-bar-simple-p)
(defun (setf status-bar-simple-p) (simplep status-bar)
  (win32::|SendMessage/ii| status-bar win32::SB_SIMPLE (if simplep 1 0) 0) )


;;;; (setf status-bar-text)
(defun (setf status-bar-text) (text status-bar
                               &optional (type win32::SB_SIMPLEID) )
  (win32::|SendMessage/is| status-bar win32::SB_SETTEXT type text) )
