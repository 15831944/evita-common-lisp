;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 21 Streams - Broadcast Stream
;;; runtime/r21-broadcast-stream.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/stream/r21-broadcast-stream.lisp#2 $
;;;
;;; Description:
;;;  This file contains methods of broadcast-stream.
;;;
;;; Note: pretty printer uses broadcast stream for populating label
;;; table for shared object labeling.
;
(in-package :si)

;;;; stream-line-column
;
(defmethod ext:stream-line-column ((stream broadcast-stream))
  (some #'ext:stream-line-column (broadcast-stream-streams stream)) )


;;;; stream-write-char
;
(defmethod ext:stream-write-char ((stream broadcast-stream) char)
    (declare (type character char))
    (declare (values character))
  (dolist (stream-1 (broadcast-stream-streams stream) char)
    (stream-write-char stream-1 char) ) )


;;;; 21.2.41 make-broadcast-stream
;
(defun cl:make-broadcast-stream (&rest streams)
    (declare (values broadcast-stream))
  (dolist (stream streams)
    (unless (output-stream-p stream)
      (error 'type-error
            :expected-type 'output-stream
            :datum         stream )) )
  (make-instance 'broadcast-stream :streams streams) )
