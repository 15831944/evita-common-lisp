;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x86 - runtime - 25 Environment
;;; platform/win/lisp/runtime/pl-win-r25-command-line.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/platform/win/lisp/runtime/pl-win-r25-getenv.lisp#1 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

;;; Name of environment variable is case insensitive.
(defvar *environ* (make-hash-table :test 'equalp))

;;; This function is called once at lisp startup
(defun populate-paltfotm-environment ()
  (labels (
    ;; copy
    (copy (string start end)
        (declare (type simple-string string))
        (declare (type sequence-index start end))
      (loop
        for i from 0
        for p from start below end by 2 do
          (setf (schar string i)
            (code-char (ref uint16* value (.unbox-int p))) )) )
    )
    ;;
    (loop
      with state = :start
      with name-start = 0
      with name-end   = 0
      with envstr = (win32-get-environment-strings)
      for runner from envstr by 2
      for code = (ref uint16* value (.unbox-int runner)) do
       (ecase state
         ((:start)
           (when (eql code 0) (return))
           (setq state :name)
           (setq name-start runner) )
         ((:name)
          (when (eql code #.(char-code #\=))
            (setq state :value name-end runner) ) )
         ((:value)
           (when (eql code 0)
             (let* ((name  (make-string (ash (- name-end name-start) -1)))
                    (value-start (+ name-end 2))
                    (value-end   runner)
                    (value (make-string (ash (- value-end value-start) -1))) )
               (copy name  name-start name-end)
               (copy value value-start value-end)
               (setf (gethash name *environ*) value)
               (setq state :start) )) ))
      initially
        (clrhash *environ*)
      finally
        (win32-free-environment-strings envstr) ) ) )


(let ((latch (make-latch 'environment)))
  (defun getenv (name)
      (declare (values (or simple-string null)))
    (with-latch (latch :shared)
      (values (gethash name *environ*)) ) )

  (defun (setf getenv) (value name)
      (declare (values string))
      (declare (type string value name))
    (with-latch (latch :exclusive)
      (setf (gethash name *environ*) value) ) )
 ) ; latch
