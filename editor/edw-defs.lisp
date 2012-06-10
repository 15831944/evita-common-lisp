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

(w:defwidget editor-frame w:application-widget
  (pane
        :type w:pane-widget )
  (status-pane
        :type w:pane-widget ) )

(w:defwidget editor-pane  w:composite-widget)
(w:defwidget edit-pane    w:pane-widget)

(defclass selection (range)
  ()
  (:metaclass structure-class) )
