;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - genesis - Toplevel
;;; lisp/devel/d99-toplevel.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel3/d99-toplevel.lisp#14 $
;;;
;;; Description:
;;;  This file contains toplevel.
;
(in-package :si)

;;;; start-application
(defun start-application (arg)
    (declare (ignore arg))
  (labels (
    (init ()
      (setq *environment* *runtime-environment*)
      (setq *features* *default-features*)
      (setq *object-pool* (make-hash-table :test 'eq))
      (setq *package* (find-package :cl-user))
      (setq *readtable* (copy-readtable))
      (setq c::*target* #+x86 :x86 #+x64 :x64)

      (let ((host    (make-windows-host (short-site-name)))
            (syshost (make-logical-host "SYS")) )
        (setq *pathname-hosts* (list host syshost))
        (setq *default-pathname-defaults*
          (make-pathname-using-host host nil nil nil nil nil) ) )

      (setf (logical-pathname-translations "SYS") '(
        ("ROOT;**;"             "d:/proj/evcl3/**/")
        ("SITE;*.TRANSLATIONS"  "SYS:ROOT;*.XLTS")
        ("SYSTEM;"              "SYS:ROOT;LISP;")

        ("SOURCE;**;"           "SYS:ROOT;LISP;**;")
        ("TESTCASE;**;"         "SYS:ROOT;TESTCASE;**;")

        #+x86 ("DEBUG;**;"            "SYS:ROOT;DEBUG;WIN32;LISP;**;")
        #+x86 ("RELEASE;**;"          "SYS:ROOT;RELEASE;WIN32;LISP;**;")

        #+x64 ("DEBUG;**;"            "SYS:ROOT;DEBUG;X64;LISP;**;")
        #+x64 ("RELEASE;**;"          "SYS:ROOT;RELEASE;X64;LISP;**;")
        ) )

      (unless *terminal-io*
        (setq *terminal-io*
            (make-two-way-stream *standard-input* *standard-output*) )
        (unless *debug-io* (setq *debug-io* *terminal-io*))
        (unless *query-io* (setq *query-io* *terminal-io*))
        (unless *trace-output* (setq *trace-output* *terminal-io*)) )

      (setq *random-state* (make-random-state t))

      #+x86
      (populate-paltfotm-environment)

      (setq *debug-output*
        (let ((s (make-instance 'debug-output-stream)))
          (realize-instance s)
          s ) ) )

    ;; batch-loop
    (batch-loop ()
      (setq *stack-overflow-handler*
        (lambda (fn)
            (declare (lambda-name batch-stack-overflow-handler))
          (format t "~&;;; Stack overflow in ~S.~%" fn)
          (format t ";;; Exit~%")
          (exit-process 1) ) )
      (let ((eof '(eof)))
        (loop
          (let ((form (read nil nil eof)))
            (when (eq form eof) (return))
            (eval form) )) ) )

    ;; batch-toplevel
    (batch-toplevel ()
      (handler-case (batch-loop)
        (error (c) (format t "~A~%" c) (exit-process 1)) )
      (exit-process 0) )

    ;; interactive-p
    (interactive-p ()
      (let ((cmdl (get-command-line)))
        (or (search "--interactive" cmdl)
            (and (interactive-stream-p *standard-input*)
                 (not (search "--batch" cmdl)) )) ) )

    ;; interactive-herald
    (interactive-herald()
      (format t ";;;; ~A ~A~%"
        (lisp-implementation-type)
        (lisp-implementation-version) )
      (format t ";;; Copyright (C) 1996-2007 by Project Vogue.~%")
      (multiple-value-bind (s n h d m y)
            (decode-universal-time *image-save-time*)
          (format t ";;;   Created at ~A ~D, ~D ~2,'0D:~2,'0D:~2,'0D.~%"
            (svref #("***"
                     "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec" )
                   m )
            d y h n s ) )
      (format t ";;;  ~S~%" *features*)
      (format t ";~2%")
      (setq *stack-overflow-handler*
        (lambda (fn)
            (declare (lambda-name stack-overflow-handler))
          (setq *handler-clusters* nil)
          (setq *restart-clusters* nil)
          (setq devel::*cmdl-level* 0)
          (format t "~&;;; Stack overflow in ~S.~%" fn)
          (format t ";;; Restart~%")
          (interactive-toplevel-2) )) )

    ;; interactive-toplevel
    (interactive-toplevel ()
      (interactive-herald)
      (interactive-toplevel-2) )

    ;; interactive-toplevel-2
    (interactive-toplevel-2 ()
      (with-standard-io-syntax
        (setq *package* #.(find-package :si))
        (loop
          (restart-case (devel::command-loop)
            (abort ()
              :report "Return to toplevel."
              (format t "~&; Back to toplevel~%") ))
          (unless (interactive-stream-p *standard-input*)
            (exit-process 0) )) ) )
    )
    ;;
    (init)
    (if (interactive-p)
        (interactive-toplevel)
      (batch-toplevel) ) ) )


;;;; keyboard-interupt
;;;
;;; Descriltion:
;;;  This function is called by VM when keyboard interrupt is detected.
(defun keyboard-interrupt ()
  (restart-case (error 'keyboard-interrupt)
    (continue () :report "Continue to execute.") ) )


;;;; cl:error
(defun cl:error (datum &rest args)
    (declare (type condition-designator datum))
    (declare (values nil))
  (let ((condition (coerce-to-condition 'error datum args 'simple-error)))
    (signal condition)
    (invoke-debugger condition) ) )


;;;; cl:invoke-debugger
(defun cl:invoke-debugger (cond)
  (let ((hook *debugger-hook*))
    (when hook (let ((*debugger-hook* nil)) (funcall hook cond hook))) )
  (devel::debugger cond) )
