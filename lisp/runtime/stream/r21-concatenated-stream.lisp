;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 21 Streams - Concatenated Stream
;;; runtime/r21-concatenated-stream.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/stream/r21-concatenated-stream.lisp#3 $
;;;
;;; Description:
;;;  This file contains methods of concatenated-stream.
;;;
;;; Note: pretty printer uses concatenated stream for populating label
;;; table for shared object labeling.
;
(in-package :si)

;;;; stream-read-char
(defmethod ext:stream-read-char ((stream concatenated-stream))
    (declare (values (or character (eql :eof))))
  (loop
    (let ((stream-1 (first (slot-value stream 'streams))))
      (when (null stream-1) (return :eof))
      (let ((char (stream-read-char stream-1)))
        (unless (eq char :eof) (return char))
        (pop (slot-value stream 'streams)) ) )) )


;;;; stream-unread-char
(defmethod ext:stream-unread-char ((stream concatenated-stream) ch)
    (declare (values null))
  (let ((stream-1 (first (slot-value stream 'streams))))
    (if stream-1
        (stream-unread-char stream-1 ch)
      (error "Can't unread-char") ) ) )


;;;; 21.2.41 make-concatenated-stream
(defun cl:make-concatenated-stream (&rest streams)
    (declare (values concatenated-stream))
  (dolist (stream streams)
    (unless (input-stream-p stream)
      (error 'type-error
            :expected-type 'input-stream
            :datum         stream )) )
  (make-instance 'concatenated-stream :streams streams) )
