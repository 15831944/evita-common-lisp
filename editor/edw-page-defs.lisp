;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Editor Windowing
;;; editor/edw-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r00-fns.lisp#2 $
;;;
;;; Description:
;;;  Class definition of Text Buffer Basic.
;
(in-package :editor)

;;;; tv-box-style
(defclass tv-box-style (style)
  (
    (font
        :type win32:font )
  )
  (:metaclass structure-class) )


;;;; tv-box
(defclass tv-box (editor-object)
  (
   (ascent
      :initform 0
      :type     fixnum )
   (buffer-end
      :initarg :buffer-end
      :type    position )
   (buffer-start
      :initarg :buffer-start
      :type    position )
   (descent
      :initform 0
      :type     fixnum )
   (font
      :initarg  :font
      :type     w:font )
   (rect
      :initform (w:make-rect)
      :type     w:rect )
   (style
      :initform (make-instance 'tv-box-style)
      :type     tv-box-style )
  )
  (:metaclass structure-class) )


;;;; tv-marker
(deftype tv-marker ()
  '(member :continue :end-of-buffer :end-of-line :tab :wrap) )


;;;; tv-marker-box
(defclass tv-marker-box (tv-box)
  (
    (marker
        :initarg :marker
        :type    tv-marker )
  )
  (:metaclass structure-class) )


;;;; tv-text-box
(defclass tv-text-box (tv-box)
  (
   (string
      :initarg :string
      :type    string )
  )
  (:metaclass structure-class) )


;;;; tv-xtext-box
(defclass tv-xtext-box (tv-text-box)
  (
  )
  (:metaclass structure-class) )


;;;; tv-boxes
(define-dlinked-anchor tv-boxes tv-box)

;;;; tv-line
(defclass tv-line (editor-object)
  (
   (ascent
      :initform 0
      :type     fixnum )
   (boxes
      :initform (make-instance 'tv-boxes)
      :type     tv-boxes )
   (buffer-end
      :initform 0
      :type     position )
   (buffer-start
      :initform 0
      :type     position )
   (descent
      :initform 0
      :type     fixnum )
   (next
      :initform nil
      :type     (or tv-line null) )
   (prev
      :initform nil
      :type     (or tv-line null) )
   (rect
      :initform (w:make-rect) )
  )
  (:metaclass structure-class) )


;;;; tv-lines
(define-dlinked-anchor tv-lines tv-line)


;;;; tv-page
(defclass tv-page (editor-object)
  (
   (lines
      :initform (make-instance 'tv-lines)
      :type     tv-lines )
   (rect
      :initform (w:make-rect)
      :type     w:rect )

   ;; selection-start
   ;; selection-end
   ;; selection-color
   ;; selection-background

  )
  (:metaclass structure-class) )


;;;; format-context
(defclass format-context (editor-object)
  (
   (buffer
      :initarg :buffer
      :type    edit-buffer )
   (interval
      :type    interval )
   (page
      :initarg :page
      :type    edit-page )
   (posn
      :initarg :position
      :type    position )
   (rect
      :initarg :rect
      :type    w:rect )
   (selection
      :initarg :selection
      :type    selection )
  )
  (:metaclass structure-class) )
