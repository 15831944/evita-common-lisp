;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Editor - Text Buffer Core
;;; editor/ed-buffer-core-defs.lisp
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
;;;  Editor conditions
;
(in-package :editor)

(define-condition editor-error (error) ())

(define-condition buffer-error (error)
  ((buffer
        :initarg :buffer
        :type    text-buffer-core )) )


(define-condition buffer-not-ready (buffer-error) ())
(define-condition buffer-read-only (buffer-error) ())

(define-condition invalid-position (buffer-error)
  ((position
        :initarg :position
        :type    position ) ))

(define-condition invalid-positions (buffer-error)
  ((end
        :initarg :end
        :type    position )
   (start
        :initarg :start
        :type    position ) ) )
