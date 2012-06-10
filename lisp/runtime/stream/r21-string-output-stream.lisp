;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 21 Streams - String Stream
;;; runtime/r21-stream-string.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/stream/r21-string-output-stream.lisp#3 $
;;;
;;; Description:
;;;  This file contains default methods of stream operation.
;
(in-package :si)


(define-pool string-output-stream ()
  :constructor make-string-output-stream
  :finalizer
    (lambda (stream &aux (st (ref instance storage stream)))
      (setf (fill-pointer (ref string-output-stream string st)) 0)
      (setf (ref string-output-stream column st) 0)
      (setf (ref string-output-stream flags st) STREAM-FLAG-OUTPUT) ) )


;;;; 21.2.48 get-output-stream-string
;
(defun cl:get-output-stream-string (stream)
    (declare (type string-output-stream stream))
    (declare (values simple-string))
  (let* ((st (ref instance storage stream))
         (string (ref string-output-stream string st)) )
    (prog1
        (subseq string 0 (length string))
      (setf (fill-pointer string) 0)
      (setf (ref string-output-stream column st) 0) ) ) )


;;;; 21.2.50 make-string-output-stream
;
(defun cl:make-string-output-stream ()
    (declare (values string-output-stream))
  (make-instance 'string-output-stream) )


;;;; stream-line-column
(defmethod ext:stream-line-column ((stream string-output-stream))
    (declare (values ext:sequence-index))
  (ref string-output-stream column
            (ref instance storage stream) ) )


;;;; stream-write-char
;
(defmethod ext:stream-write-char ((stream string-output-stream) char)
    (declare (type character char))
    (declare (values character))
  (let ((st (ref instance storage stream)))
    (if (char= char #\Newline)
        (setf (ref string-output-stream column st) 0)
      (incf (ref string-output-stream column st)) )
    (let ((string (ref string-output-stream string st)))
      (vector-push-extend char string)
      char ) ) )
