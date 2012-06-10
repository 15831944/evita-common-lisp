;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 21 Streams - Echo Stream
;;; runtime/r21-echo-string.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/stream/r21-echo-stream.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of echo-stream.
;
(in-package :si)

;;;; 21.2.45 make-echo-stream
;
(defun cl:make-echo-stream (input-stream output-stream)
    (declare (type stream input-stream))
    (declare (type stream output-stream))
    (declare (values echo-stream))
  (make-instance 'echo-stream
        :input-stream  input-stream
        :output-stream output-stream ) )


;;;; stream-read-char
;
(defmethod ext:stream-read-char ((stream echo-stream))
    (declare (values (or character (eql :eof))))
  (let ((char (stream-read-char (echo-stream-input-stream stream))))
    (unless (eq char :eof)
      (if (slot-value stream 'unread-p)
          (setf (slot-value stream 'unread-p) nil)
        (stream-write-char (echo-stream-output-stream stream) char) ))
    char ) )


;;;; stream-unread-char
;
(defmethod ext:stream-unread-char ((stream echo-stream) char)
    (declare (type character char))
    (declare (values null))
 (when (slot-value stream 'unread-p)
   (error "Can't unread more than one character.") )
 (setf (slot-value stream 'unread-p) t)
 (stream-unread-char (echo-stream-input-stream stream) char) )


(macrolet (
  (define (name slot-name &rest spec*)
    `(defmethod ,name ((stream echo-stream) ,@spec*)
       (,name (slot-value stream ',slot-name)
          ,.(loop
              for spec in spec*
              unless (eq spec '&optional)
                collect (if (consp spec) (first spec) spec) )) ) )
  )
   (define ext:stream-advance-to-column output-stream column)
   (define ext:stream-clear-input input-stream)
   (define ext:stream-finish-output output-stream)
   (define ext:stream-force-output output-stream)
   (define ext:stream-fresh-line output-stream)
   (define ext:stream-line-number output-stream)
   (define ext:stream-line-column input-stream)
   (define ext:stream-listen input-stream)
   (define ext:stream-output-width output-stream)
   (define ext:stream-peek-char input-stream)
   (define ext:stream-read-byte input-stream)
   (define ext:stream-read-char-no-hang input-stream)
   (define ext:stream-read-line input-stream)
   (define ext:stream-read-sequence
        input-stream sequence &optional (start 0) end )
   (define ext:stream-start-line-p output-stream)
   (define ext:stream-terpri output-stream)
   (define ext:stream-write-byte output-stream byte)
   (define ext:stream-write-char output-stream char)
   (define ext:stream-write-sequence
        output-stream sequence &optional (start 0) end )
   (define ext:stream-write-string
        output-stream string &optional (start 0) end )

   (define cl:interactive-stream-p input-stream)
 ) ; macrolet
