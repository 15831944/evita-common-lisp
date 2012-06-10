;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Editor - Edit Journal
;;; editor/ed-edit-journal.lisp
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
;;;  Class definition of Text Buffer Basic.
;
(in-package :editor)

;;;; edit-journal-check-point
(defun edit-journal-check-point (buffer)
    (declare (values edit-journal))
    (declare (type edit-buffer buffer))
  (let ((journal (slot-value buffer 'journal)))
    journal ) )


;;;; edit-journal-record-delete
(defun edit-journal-record-delete (buffer start end)
    (declare (values edit-journal))
    (declare (type edit-buffer buffer))
    (declare (type position start end))
  (let ((journal (slot-value buffer 'journal)))
    journal ) )


;;;; edit-journal-record-insert
(defun edit-journal-record-insert (buffer start end)
    (declare (values edit-journal))
    (declare (type edit-buffer buffer))
    (declare (type position start end))
  (let ((journal (slot-value buffer 'journal)))
    journal ) )
