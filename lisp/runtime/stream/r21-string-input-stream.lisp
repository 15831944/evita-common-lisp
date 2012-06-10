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
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/stream/r21-string-input-stream.lisp#2 $
;;;
;;; Description:
;;;  This file contains default methods of stream operation.
;
(in-package :si)

;;;; 21.2.12 stream-element-type
;
(defmethod cl:stream-element-type ((stream string-stream))
  'character )


;;;; 21.2.49 make-string-input-stream
;
(defun cl:make-string-input-stream (string &optional (start 0) end)
  (make-instance 'string-input-stream :string string 
                 :start start :end (or end (length string)) ) )


;;;; stream-read-char
;
(defmethod ext:stream-read-char ((stream string-input-stream))
  (with-slots (index end string) stream
    (if (>= index end)
        :eof
      (prog1 (char string index)
             (incf index) )) ) )


;;;; stream-read-line
;
(defmethod ext:stream-read-line ((stream string-input-stream))
  (with-slots (index end string) stream
    (let* ((endline (position #\newline string :start index :end end))
           (line (subseq string index endline)))
      (if endline
          (progn (setq index (1+ endline))
                 (values line nil))
        (progn (setq index end)
               (values line t) )) ) ) )


;;;; stream-unread-char
;
(defmethod ext:stream-unread-char ((stream string-input-stream) character)
  (with-slots (index end string) stream
    (decf index)
    (assert (eql (char string index) character))
    nil ) )
