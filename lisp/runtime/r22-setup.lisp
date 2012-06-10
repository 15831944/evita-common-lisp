;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Runtime - 22 Printer - Setup
;;; runtime/r22-setup.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r22-setup.lisp#2 $
;;;
;;; Description:
;;;  Setup printer related variables.
;
(in-package :si)


;;;; Install pretty printers

(set-pprint-dispatch 'array         'pprint-array)
(set-pprint-dispatch 'cons          'pprint-fill)
(set-pprint-dispatch 'vector        'pprint-vector)


;;; Make current entires as initial entry.
(dolist (entry (pp-dispatch-table-entries *standard-pprint-dispatch*))
  (setf (second entry) nil) )
