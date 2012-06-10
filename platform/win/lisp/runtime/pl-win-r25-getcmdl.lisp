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
;;; @(#)$Id: //proj/evcl3/mainline/platform/win/lisp/runtime/pl-win-r25-getcmdl.lisp#1 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

(defun get-command-line()
  (labels (
    ;; import-cstring
    (import-cstring (cstr)
      (loop
        with str = (make-string (strlen cstr))
        for p = cstr then (+ p 2)
        for i of-type sequence-index from 0
        for ch = (ref uint16* value (.unbox-int p))
        until (eql ch 0)
        do (setf (schar str i) (code-char ch))
        finally (return str) ) )

    ;; strlen
    (strlen (cstr)
      (loop
        for n of-type sequence-index = 0 then (1+ n)
        for p = cstr then (+ p 2)
        for ch = (ref uint16* value (.unbox-int p))
        until (eql ch 0)
        finally (return n) ) )
    )
    ;;
    (import-cstring (win32-get-command-line)) ) )
