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
;;; @(#)$Id: //proj/evcl3/mainline/platform/win/lisp/widget/widget-defs.lisp#1 $
;;;
;;; Description:
;;;  Class definition of Text Buffer Basic.
;
(in-package :widget)

;;;; application
(defclass application (structure-object)
  ()
  (:metaclass structure-class) )

;;;; defapplication
(defmacro defapplication (name super &rest slots)
  `(defclass ,name (,super) ,slots (:metaclass structure-class)) )

;;;; defwidget
(defmacro defwidget (name super &rest slots)
  `(defclass ,name (,super) ,slots (:metaclass structure-class)) )

;;;; widget
(defwidget widget win32:window
  (child-widgets
    :initform '()
    :type     list )
  (parent
    :initform nil
    :type     (or widget null) ) )

;;;; composite-widget
(defwidget composite-widget widget)

;;;; toplevel-widget
(defwidget toplevel-widget composite-widget
  (title
    :initform "No title"
    :type     string ) )

;;;; application-widget
(defwidget application-widget toplevel-widget)

;;;; pane-widget
(defwidget pane-widget widget)

(defgeneric on-widget-draw   (widget) (:method ((w widget))))
(defgeneric on-widget-resize (widget) (:method ((w widget))))
(defgeneric realize-widget   (widget))

(declaim
  (ftype (function (composite-widget widget) composite-widget)
    add-widget )

  (ftype (function (widget font x-point y-point fixnum w:rect string
                    &key (:end sequence-end) (:start sequence-index) )
                   t )
         draw-text )

  (ftype (function (font string
                    &key (:end sequence-end) (:start sequence-index) )
                   (values fixnum fixnum) )
         get-text-extent )

  (ftype (function (widget) unspecified)
    on-widget-draw
    on-widget-resize )

  (ftype (function (widget) widget)
    realize-widget )
 ) ; declaim
