;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 25 Environment - Toplevel
;;; dev/d25-toplevel.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-toplevel.lisp#3 $
;;;
;;; Description:
;;;  This file contains functions for development:
;
(in-package :cmdl)

;;;; read-eval-print-loop
;
(defun batch-command-loop ()
  (loop
    with eof = '(eof)
    for form = (read nil nil eof)
    until (eq form eof) do
      (print (eval form)) ) )


;;;; toplevel
;
(defun toplevel ()
  (setq *break-on-signals*     nil)
  (setq *debugger-hook*        nil)

  (setq si::*handler-clusters* '())
  (setq si::*restart-clusters* '())

  (setq *cmdl-level*           0)
  (setq *load-level*           0)
  (setq *condition*            nil)
  (setq *restarts*             '())
  (setq *frame-index*          6)
  (setq *last-abort*           nil)
  ;;
  (setq *input-history-last*  (list nil))
  (setq *input-history-head*  *input-history-last*)
  (setq *input-history-count* 0)
  (setq *last-pathname*       nil)

  (setq *readtable*             (copy-readtable nil))
  (setq *print-pprint-dispatch* (copy-pprint-dispatch nil))

  (setq *cmdl-standard-input*  *standard-input*)
  (setq *cmdl-standard-output* *standard-output*)

  ;; Printer
  (setq si::*printer-level*         0)
  (setq si::*printer-label-table*   nil)
  (setq si::*printer-label*         nil)
  (setq si::*printer-stream*        nil)

  (if (interactive-stream-p *standard-input*)
      (command-loop)
    (batch-command-loop) ) )


;;;; 9.2.22 invoke-debugger
;;;
;;; Syntax:
;;;     invoke-debugger condition => |
;
(defun invoke-debugger (condition)
  (when *debugger-hook*
    (let ((hook *debugger-hook*)
          (*debugger-hook* nil) )
      (funcall hook condition hook) ))

  (unless (interactive-stream-p *standard-input*)
    (force-output)
    (force-output *error-output*)
    (format t ";;; Error: ~A~%" condition)
    (format t ";;; Backtrace~%")
    (loop
      for frame = (si::.next-frame nil) then (si::.next-frame frame)
      for nth = 1 then (1+ nth)
      while frame do
       (format t ";; ~D ~S~%" nth (si:record-ref frame 1)) )
    (force-output)
    (si::.exit-lisp 1) )

  ;; Invoke interactive debugger
  (debugger condition) )
