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
;;; @(#)$Id: //proj/evcl3/mainline/platform/win/lisp/widget/widget-fns.lisp#1 $
;;;
;;; Description:
;;;  Class definition of Text Buffer Basic.
;
(in-package :widget)

;;;; add-child
(defun add-child (parent child)
    (declare (values widget))
    (declare (type composite-widget parent))
    (declare (type widget child))
  (push child (slot-value parent 'child-widgets))
  (setf (slot-value child 'parent) parent)
  parent )


;;;; draw-text
(defun draw-text (w font x y options rc string &key end (start 0))
  (multiple-value-bind (string start end) (string-data string start end)
    (let ((old-font (win32::|SelectFont| w font)))
      (win32:!draw-text w x y options rc string start end)
      (win32::|SelectObject| w old-font) ) ) )


;;;; get-text-extent
(defun get-text-extent (font string &key end (start 0))
  (multiple-value-bind (string start end) (string-data string start end)
    (let ((old-font (win32::|SelectFont| win32:+null-window+ font)))
      (multiple-value-bind (cx cy)
        (win32:!get-text-extent win32:+null-window+ string start end)
        (win32::|SelectObject| win32:+null-window+ old-font)
        (values cx cy) ) ) ) )


;;;; initialize
(defun initialize ()
  (win32:initialize) )


;;;; realize-widget
(defmethod realize-widget ((widget application-widget))
  (win32:create-window widget
    :style (logior win32::WS_OVERLAPPEDWINDOW
                   win32::WS_VISIBLE )
    :title (slot-value widget 'title) )
  widget )


;;;; realize-widget
(defmethod realize-widget ((window widget))
  (win32:create-window window
      :style  (logior win32::WS_CHILD win32::WS_VISIBLE)
      :parent (slot-value window 'parent) )
  window )


;;;; set-position
(defun set-position (widget &key after
                                 (bottom (required))
                                 (left   (required))
                                 (right  (required))
                                 (top    (required)) )
    (declare (values widget))
    (declare (type widget widget))
    (declare (type rect rc))
  (win32::|SetWindowPos|
        widget
        (or after win32:+null-window+)
        left
        top
        (- right left)
        (- bottom top)
        (if after 0 win32::SWP_NOZORDER) )
  widget )


;;;; window-procedure
(defmethod win32:window-procedure ((w widget) uMsg wParam lParam)
  (labels (
    ;; do-create
    (do-create ()
      (dolist (child (slot-value w 'child-widgets))
        (realize-widget child) ) )

    ;; do-draw
    (do-draw ()
      (on-widget-draw w) )

    ;; do-key-down
    (do-key-down ()
      (format t "; key-down: ~S w=~X l=~X~%" w wParam lParam) )

    ;; do-size
    (do-size ()
      (win32::|GetClientRect| w (window-rect w))
      (on-widget-resize w) )
    )
    (case uMsg
      (#.win32::WM_CREATE   (do-create) 0)
      (#.win32::WM_KEYDOWN  (do-key-down) 0)
      (#.win32::WM_PAINT    (do-draw)   0)
      (#.win32::WM_SIZE     (do-size)   0) ) ) )
