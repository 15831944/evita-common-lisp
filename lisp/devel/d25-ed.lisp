;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Developer - 25 Environment
;;; devl/d25-ed.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-ed.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     ed  25.2.17
;;;
;;; Note: We can't trace functions which use non-standard calling convention.
;;; BUGBUG: NYI: Detect functio in R/O area, and tell user that we can't trace
;;; functions in R/O area.
;
(in-package :devel)

(ext:deftlv *editor-program*
    "c:\\program files\\evita\\evita.exe" )

(ext:deftlv *editor-command-line*
    (format nil "~A \"~~A\"" *editor-program*) )

;;;; ed
;
(defun cl:ed (&optional x)
  (let ((command-line
          (etypecase x
            (null *editor-program*)
            (pathname
              (format nil "~A \"~A\"" *editor-program*
                (truename (translate-logical-pathname x)) ) )
            (string
              (format nil "~A \"~A\"" *editor-program* x) )
            (ext:function-name
              ;; BUGBUG: NYI: locate source of function definition.
              *editor-program* )) ))
    (win::resume-process (win::make-process command-line))
    x ) )
