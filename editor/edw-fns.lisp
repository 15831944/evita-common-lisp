;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Editor Windowing
;;; editor/edw-fns.lisp
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

;;;; make-editor
(defun make-editor ()
  (let ((frame (make-instance 'editor-frame))
        (pane  (w:add-child (make-instance 'editor-pane)
                            (make-instance 'edit-pane) )) )
    (setf (slot-value frame 'pane) pane)
    (w:add-child frame pane) ) )


(defmethod w::on-widget-resize ((frame editor-frame))
  (let ((pane (slot-value frame 'pane))
        (rc   (w:window-rect frame)) )
    (w:set-position pane
        :left   (+ (w:rect-left   rc) 5)
        :top    (+ (w:rect-top    rc) 5)
        :right  (- (w:rect-right  rc) 5)
        :bottom (- (w:rect-bottom rc) 5) ) ) )

(defmethod w::on-widget-resize ((w editor-pane))
  (let ((pane (first (slot-value w 'w::child-widgets)))
        (rc   (w:window-rect w)) )
    (w:set-position pane
        :left   (+ (w:rect-left   rc) 0)
        :top    (+ (w:rect-top    rc) 0)
        :right  (- (w:rect-right  rc) 0)
        :bottom (- (w:rect-bottom rc) 0) ) ) )


(defmethod w::on-widget-draw ((w editor-frame))
  (let ((rc (w:window-rect w)))
    (win32::|FillRect| w rc win32::+brush.graytext+) ) )


(defmethod w::on-widget-draw ((w edit-pane))
  (let ((rc (w:window-rect w)))
    (win32::|FillRect| w rc win32::+brush.btnface+)
    (win32::|ExtTextOut| w 100 100 0 rc (format nil "This is ~S." w)) ) )
