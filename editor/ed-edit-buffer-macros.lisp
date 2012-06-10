;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Editor - Edit Buffer Macros
;;; editor/ed-edit-buffer-macros.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r00-fns.lisp#2 $
;;;
;;;
;;; Description:
;;;  Class definition of Edit Buffer.
;
(in-package :editor)

(defmacro with-buffer-iterator ((next buffer start) &body body)
  (let ()
   `(let* ((.buffer ,buffer)
           (.posn   ,start)
           (.intv   (get-interval .buffer .posn)) )
      (macrolet (
        (,next ()
         '(if (>= .posn (buffer-length .buffer))
              (values nil nil)
            (let ((char (buffer-char .buffer .posn))
                  (intv .intv) )
              (incf .posn)
              (when (>= .posn (slot-value .intv 'end))
                (setq .intv (slot-value .intv 'next)) )
              (values char intv) )) )
        ) ,@body ) ) ) )
