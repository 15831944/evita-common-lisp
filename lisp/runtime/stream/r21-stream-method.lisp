;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 21 Streams - Stream Methods
;;; runtime/r21-stream-methods.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/stream/r21-stream-method.lisp#2 $
;;;
;;; Description:
;;;  This file contains default methods of stream operation.
;
(in-package :si)

;;;; stream-advance-to-column
(defmethod ext:stream-advance-to-column ((stream stream) column)
    (declare (type ext:sequence-index column))
    (declare (values t))
  (let ((current (stream-line-column stream)))
    (when current
      (loop while (> column current) do
        (stream-write-char stream #\Space)
        (incf current) )
      t ) ) )


;;;; clear-input
;;;; clear-output
;;;  finish-output
;;;  force-output
(macrolet (
  (define (dir op)
    (let* ((api-fn    (intern (format nil "CL-~A" op)))
           (dir-fn    (intern (format nil "~A-STREAM-P" dir)))
           (stream-fn (intern (format nil "STREAM-~A" op))) )
      `(defmethod ,stream-fn ((stream stream))
              (declare (values null))
            (unless (,dir-fn stream)
              (error 'unsupported-stream-operation
                :stream stream
                :operation ',api-fn ))
            nil ) ) )
    )
    (define input  clear-input)
    (define output clear-output)
    (define output finish-output)
    (define output force-output)
 ) ; macrolet


;;;; stream-external-format
;;;
;;; Note: ANSI-CL requies stream argument to stream-external-format
;;; function as file stream
(defmethod cl:stream-external-format ((stream stream))
    (declare (values t))
  :default )


;;;; stream-fresh-line
;
(defmethod ext:stream-fresh-line ((stream stream))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)
    t ) )


;;;; stream-line-number
;;;
;;; Note: evcl extension
(defmethod ext:stream-line-number ((stream stream))
    (declare (values (or ext:sequence-index null)))
  nil )


;;;; stream-line-column
(defmethod ext:stream-line-column ((stream stream))
    (declare (values (or ext:sequence-index null)))
  (unless (output-stream-p stream)
    (error 'unsupported-stream-operation
      :stream stream
      :operation 'stream-line-column ) )
  nil )


;;;; stream-listen
(defmethod ext:stream-listen ((stream stream))
    (declare (values t))
  (let ((char (stream-read-char-no-hang stream)))
    (when (characterp char) (stream-unread-char stream char) t) ) )


;;;; stream-output-width
(defmethod ext:stream-output-width ((stream stream))
    (declare (values (or null ext:sequence-index)))
  nil )


;;;; stream-pathname
;;;
;;; Note: CLIM:GRAY
;
(defmethod ext:stream-pathname ((stream stream))
  (error 'unsupported-stream-operation
    :stream stream
    :operation 'pathname ) )


;;;; stream-peek-char
;
(defmethod ext:stream-peek-char ((stream stream))
    (declare (values (or character (eql :eof))))
  (let ((char (stream-read-char stream)))
    (when (characterp char) (stream-unread-char stream char))
    char ) )


;;;; stream-read-byte
;
(defmethod ext:stream-read-byte ((stream stream))
    (declare (values (or integer (eql :eof))))
  (error 'unsupported-stream-operation
    :stream stream
    :operation 'read-byte ) )


;;;; stream-read-char
;
(defmethod ext:stream-read-char ((stream stream))
    (declare (values (or character (eql :eof))))
  (error 'unsupported-stream-operation
    :stream stream
    :operation 'read-char ) )


;;;; stream-read-char-no-hand
;
(defmethod ext:stream-read-char-no-hang ((stream stream))
    (declare (values (or character (eql :eof) null)))
  (stream-read-char stream) )


;;;; stream-read-line
;
(defmethod ext:stream-read-line ((stream stream))
    (declare (values simple-string t))
   (let ((line (make-array 80 :element-type 'character
                              :fill-pointer 0 :adjustable t)))
      (loop
        (let ((char (stream-read-char stream)))
          (when (eq char :eof)
            (return (values line t)) )
          (when (eql char #\Newline)
            (return (values line nil)) )
          (vector-push-extend char line) )) ) )


(defmethod ext:stream-read-sequence
        ((stream stream) sequence &optional (start 0) end)
    (declare (type sequence sequence))
    (declare (type stream stream))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-end end))
  (labels (
    ;; read-bytes
    (read-bytes ()
      (loop
        for index from start below end
        for char = (stream-read-byte stream) do
          (when (eq char :eof)
            (return index) )
          (setf (elt sequence index) char)
        finally
          (return index) ) )

    ;; read-chars
    (read-chars ()
      (loop
        for index from start below end
        for char = (stream-read-char stream) do
          (when (eq char :eof)
            (return index) )
          (setf (elt sequence index) char)
        finally
          (return index) ) )
    )
    ;; read-sequence
    ;;
    (setq end (ensure-bounding-indexes sequence start end))
    (if (subtypep (stream-element-type stream) 'character)
        (read-chars)
      (read-bytes) ) ) )


;;;; stream-start-line-p
;
(defmethod ext:stream-start-line-p ((stream stream))
    (declare (values t))
  (eql (stream-line-column stream) 0) )


;;;; stream-terpri
;
(defmethod ext:stream-terpri ((stream stream))
    (declare (values null))
  (stream-write-char stream #\Newline)
  nil )


;;;; stream-truename
;;;
;;; Note: CLIM:GRAY
;
(defmethod ext:stream-truename ((stream stream))
  (error 'unsupported-stream-operation
    :stream stream
    :operation 'truename ) )


;;;; stream-unread-char
;
(defmethod ext:stream-unread-char ((stream stream) char)
    (declare (type character char))
    (declare (values null))
    (declare (ignore char))
  (error 'unsupported-stream-operation
    :stream stream
    :operation 'unread-char ) )


;;;; stream-write-byte
;
(defmethod ext:stream-write-byte ((stream stream) byte)
    (declare (type integer byte))
    (declare (values integer))
    (declare (ignore byte))
  (error 'unsupported-stream-operation
    :stream stream
    :operation 'write-byte ) )


;;;; stream-write-char
;
(defmethod ext:stream-write-char ((stream stream) char)
    (declare (type character char))
    (declare (values character))
    (declare (ignore char))
  (error 'unsupported-stream-operation
    :stream stream
    :operation 'write-char ) )


;;;; stream-write-sequence
;
(defmethod ext:stream-write-sequence
        (stream sequence &optional (start 0) end)
    (declare (type sequence sequence))
    (declare (type stream stream))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-end end))
    (declare (values sequence))
  (labels (
    ;; write-bytes
    (write-bytes ()
      (loop for index from start below end do
        (stream-write-byte stream (elt sequence index)) ) )

    ;; write-chars
    (write-chars ()
      (loop for index from start below end do
        (stream-write-char stream (elt sequence index)) ) )
    )
    ;; write-sequence
    ;;
    (setq end (ensure-bounding-indexes sequence start end))
    (if (subtypep (stream-element-type stream) 'character)
        (write-chars)
      (write-bytes) )
    sequence ) )


;;;; stream-write-string
;
(defmethod ext:stream-write-string
        ((stream stream) string &optional (start 0) end)
    (declare (type string string))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-end end))
    (declare (values string))
  (multiple-value-bind (string start end) (string-data string start end)
    (loop for index of-type sequence-index from start below end do
      (stream-write-char stream (schar string index)) ) )
  string )
