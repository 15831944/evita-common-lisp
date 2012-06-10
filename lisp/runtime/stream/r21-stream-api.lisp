;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - runtime - 21 Streams
;;; runtime/stream/r21-stream-api.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/stream/r21-stream-api.lisp#6 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;
;;;     ensure-input-stream
;;;     ensure-output-stream
;;;
;;; Public Functions:
;;;     clear-input             21.2.35
;;;     clear-output            21.2.36
;;;     close                   21.2.32
;;;     file-length             21.2.26     platform
;;;     file-position           21.2.27     platform
;;;     file-string-length      21.2.26     NYI
;;;     finish-output           21.2.36
;;;     fresh-line              21.2.19
;;;     force-output            21.2.36
;;;     input-stream-p          21.2.9
;;;     interactive-stream-p    21.2.10
;;;     listen                  21.2.34
;;;     open                    21.2.12
;;;     open-stream-p           21.2.11
;;;     output-stream-p         21.2.9
;;;     read-byte               21.2.14
;;;     read-char               21.2.17
;;;     read-char-no-hand       21.2.18
;;;     read-line               21.2.22
;;;     streamp                 21.2.13
;;;     stream-element-type     21.2.12
;;;     stream-external-format  21.2.30
;;;     terpri                  21.2.19
;;;     unread-char             21.2.20
;;;     write-byte              21.2.15
;;;     write-char              21.2.21
;;;     write-line              21.2.23
;;;     write-sequence          21.2.25
;;;     write-string            21.2.23
;;;     y-or-n-p                21.2.37
;;;     yes-or-no-p             21.2.37
;
(in-package :si)

;;;; close/2
;;; for with-open-file
(defun close/2 (stream abort)
    (declare (type (or stream null)))
  (when stream (close stream :abort abort)) )


;;;; Ensure Input Stream function.
(defun ensure-input-stream (stream)
  (let ((stream
          (etypecase stream
            ((and stream (satisfies input-stream-p)) stream)
            (null    *standard-input*)
            ((eql t) *terminal-io*) ) ))
    (unless (open-stream-p stream)
      (error 'closed-stream :stream stream) )
    stream ) )


;;;; Ensure Output Stream function.
(defun ensure-output-stream (stream)
  (let ((stream
          (etypecase stream
            ((and stream (satisfies output-stream-p)) stream)
            (null    *standard-output*)
            ((eql t) *terminal-io*) ) ))
    (unless (open-stream-p stream)
      (error 'closed-stream :stream stream) )
    stream ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;

;;;; 21.2.36 clear-input
;;;; 21.2.36 clear-output
;;;; 21.2.36 finish-output
;;;; 21.2.36 force-output
;
(macrolet (
  (define (dir op)
    (let* ((ensure-fn (intern (format nil "ENSURE-~A-STREAM" dir)))
           (type      (intern (format nil "~A-STREAM-DESIGNATOR" dir)))
           (api-fn    op)
           (stream-fn (intern (format nil "STREAM-~A" op))) )
    `(defun ,api-fn (&optional stream)
        (declare (type ,type stream))
        (declare (values null))
      (setq stream (,ensure-fn stream))
      (,stream-fn stream) ) ) )
    )
    (define input  clear-input)
    (define output clear-output)
    (define output finish-output)
    (define output force-output)
 ) ; macrolet


;;;; 21.2.32 close
;
(defmethod cl:close ((stream stream) &key abort)
    (declare (type stream stream))
    (declare (values t))
    (declare (ignore abort))
  (when (open-stream-p stream)
    (let ((st (ref instance storage stream)))
      (setf (ref file-stream flags st)
        (logior (ref file-stream flags st) STREAM-FLAG-CLOSED) )
      t )) )


;;;; 21.2.19 fresh-line
;
(defun cl:fresh-line (&optional stream)
    (declare (type output-stream-designator))
    (declare (values t))
  (setq stream (ensure-output-stream stream))
  (stream-fresh-line stream) )


;;;; 21.2.9 input-stream-p
;
(defmethod cl:input-stream-p ((stream stream))
    (declare (values t))
  (let ((flags (stream-flags stream)))
    (not (zerop (logand flags STREAM-FLAG-INPUT))) ) )


;;;; 21.2.9 interactive-stream-p
;
(defmethod cl:interactive-stream-p ((stream stream))
    (declare (values t))
  (let ((flags (stream-flags stream)))
    (not (zerop (logand flags STREAM-FLAG-INTERACTIVE ))) ) )


;;;; 21.2.23 listen
;
(defun cl:listen (&optional stream)
  (setq stream (ensure-input-stream stream))
  (stream-listen stream) )


;;;; 21.2.29 open
;
(defun cl:open (filespec &rest args &key
                    (direction       :input)
                    (external-format :default)
                    (element-type    'character)
                    if-exists
                    &allow-other-keys )
    (declare (dynamic-extent args))
    (declare (values (or null stream)))
  (let* ((pathname   (pathname filespec))
         (plist      '()) )
  (labels (
    ;; internal-open
    (internal-open ()
      (let ((stream
                (make-instance 'file-stream
                    :pathname        pathname
                    :element-type    element-type
                    :external-format external-format
                    :plist           plist ) ))
        (let* ((physical   (ensure-physical-pathname pathname))
               (namestring (namestring physical)) )
          (apply #'realize-instance stream :filename namestring args) ) ) )

    ;; prepare
    (prepare ()
      (case if-exists
        ((:rename) (rename-if-needed))
        ((:rename-and-delete)
          (when (rename-if-needed)
            (setf (getf plist 'rename-and-delete) t) ) )) )

    ;; rename-if-needed
    (rename-if-needed ()
      (let ((renamed (rename-if-exists pathname)))
        (when renamed
          (setf (getf plist 'renamed) (namestring renamed))) ) )
    )
    ;;
    (ecase direction
      ((:output) (prepare))
      ((:io)     (prepare))
      ((:input)  #+nil "nothing to do")
      ((:probe)  #+nil "nothing to do") )
    (internal-open) ) ) )


;;;; 21.2.11 open-stream-p
;
(defmethod cl:open-stream-p ((stream stream))
    (declare (values t))
  (let ((flags (stream-flags stream)))
    (zerop (logand flags STREAM-FLAG-CLOSED)) ) )


;;;; 21.2.9 output-stream-p
;
(defmethod cl:output-stream-p ((stream stream))
  (let ((flags (stream-flags stream)))
    (not (zerop (logand flags STREAM-FLAG-OUTPUT))) ) )


;;;; 21.2.16 peek-char
;
(defun cl:peek-char (&optional type stream eof-error-p eof-value recursive-p)
  (labels (
    (peek-next ()
      (let ((char (stream-peek-char stream)))
        (cond
          ((not (eq char :eof)) char)
          (eof-error-p (error 'end-of-file :stream stream))
          (t eof-value) ) ) )

    ;; skip-to
    (skip-to ()
      (loop
        (let ((char (read-char stream nil nil recursive-p)))
          (cond
            (char
              (when (eql char type)
                (unread-char char stream)
                (return char) ) )
            (eof-error-p
              (error 'end-of-file :stream stream) )
            (t (return eof-value)) ) )) )

    ;; skip-whitespace
    (skip-whitespace ()
      (loop
        (let ((char (read-char stream nil nil recursive-p)))
          (cond
            (char
              (unless (whitespace-char-p char)
                (unread-char char stream)
                (return char) ) )
            (eof-error-p (error 'end-of-file :stream stream))
            (t (return eof-value)) ) )) )
    )
    ;;
    ;; peek-char
    (setq stream (ensure-input-stream stream))
    (cond
      ((null type) (peek-next))
      ((eq type t) (skip-whitespace))
      (t           (skip-to)) ) ) )


;;;; 21.2.14 read-byte
;
(defun cl:read-byte (stream &optional (eof-error-p t) eof-value)
    (declare (type input-stream-designator stream))
    (declare (values t))
  (let ((byte (stream-read-byte stream)))
    (cond
      ((not (eq byte :eof)) byte)
      ((not eof-error-p) eof-value)
      (t (error 'end-of-file :stream stream)) ) ) )


;;;; 21.2.17 read-char
;
(defun cl:read-char (&optional stream (eof-error-p t) eof-value recursive-p)
    (declare (type input-stream-designator stream))
    (declare (values t))
    (declare (ignore recursive-p))
  (setq stream (ensure-input-stream stream))

  (let ((char (stream-read-char stream)))
    (cond
      ((not (eq char :eof)) char)
      ((not eof-error-p)    eof-value)
      (t (error 'end-of-file :stream stream)) ) ) )


;;;; 21.2.18 read-char-no-hang
;
(defun cl:read-char-no-hang (&optional stream
                               (eof-error-p t)
                               eof-value
                               recursive-p )
    (declare (type input-stream-designator stream))
    (declare (values t))
    (declare (ignore recursive-p))

  (setq stream (ensure-input-stream stream))

  (let ((char (stream-read-char-no-hang stream)))
    (cond
      ((not (eq char :eof)) char)
      ((not eof-error-p)    eof-value)
      (t (error 'end-of-file :stream stream)) ) ) )


;;;; 21.2.21 read-line
;
(defun cl:read-line (&optional stream
                               (eof-error-p t)
                               eof-value
                               recursive-p )
    (declare (type input-stream-designator stream))
    (declare (values t t))
    (declare (ignore recursive-p))
  (setq stream (ensure-input-stream stream))

  (multiple-value-bind (line eof-p) (stream-read-line stream)
    (cond
      ((not eof-p) (values line eof-p))
      ((not (zerop (length line))) (values line eof-p))
      ((not eof-error-p) (values eof-value eof-p))
      (t (error 'end-of-file :stream stream)) ) ) )


;;;; 21.2.24 read-sequence
;
(defun cl:read-sequence (sequence stream &key (start 0) end)
    (declare (type sequence sequence))
    (declare (type stream stream))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-end end))
  (stream-read-sequence stream sequence start end) )


;;;; 21.2.13 streamp
;
(defun cl:streamp (object)
  (si::subclassp (class-of object) (find-class 'stream)) )


;;;; 21.2.12 stream-element-type
;;;; 21.2.30 stream-external-format


;;;; 21.2.19 terpri
;
(defun cl:terpri (&optional stream)
    (declare (type output-stream-designator stream))
    (declare (values null))
  (setq stream (ensure-output-stream stream))
  (stream-terpri stream) )


;;;; 21.2.20 unread-char
;
(defun cl:unread-char (char &optional stream)
    (declare (type character char))
    (declare (type input-stream-designator stream))
    (declare (values null))
  (setq stream (ensure-input-stream stream))
  (stream-unread-char stream char)
  nil )


;;;; 21.2.15 write-byte
;
(defun cl:write-byte (byte stream)
    (declare (type integer byte))
    (declare (type stream stream))
  (stream-write-byte stream byte)
  byte )


;;;; 21.2.21 write-char
;;;
(defun cl:write-char (char &optional stream)
    (declare (type character char))
    (declare (type output-stream-designator stream))
    (declare (values character))
  (setq stream (ensure-output-stream stream))
  (stream-write-char stream char)
  char )


;;;; 21.2.23 write-line
;
(defun cl:write-line (string &optional stream &key (start 0) end)
    (declare (type string string))
    (declare (type output-stream-designator stream))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-end end))
    (declare (values string))
  (setq stream (ensure-output-stream stream))
  (stream-write-string stream string start end)
  (stream-terpri stream)
  string )


;;;; 21.2.25 write-sequence
;
(defun cl:write-sequence (sequence stream &key (start 0) end)
    (declare (type sequence sequence))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-end end))
    (declare (values sequence))
  (stream-write-sequence stream sequence start end) )


;;;; 21.2.23 write-string
;
(defun cl:write-string (string &optional stream &key (start 0) end)
    (declare (type string string))
    (declare (type ext:input-stream-designator stream))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-end end))
    (declare (values string))

  (setq stream (ensure-output-stream stream))
  (setq end (ensure-bounding-indexes string start end))
  (stream-write-string stream string start end)
  string )


;;;; 21.2.37 y-or-n-p
;
(labels (
  (ask (control arguments yes no)
    (when control
      (fresh-line *query-io*)
      (apply #'format *query-io* control arguments) )
    (loop
      (let ((answer (read-line *query-io*)))
        (cond
          ((string-equal answer yes) (return t))
          ((string-equal answer no)  (return nil))
          (t (format *query-io* "Please type ~S for yes or ~S for no.~%"
                yes no )) ) )) )
    )
    (defun cl:y-or-n-p (&optional (control "" control-p) &rest arguments)
        (declare (values boolean))
        (declare (type format-control control))
        (declare (dynamic-extent arguments))
      (ask (when control-p control) arguments "y" "n") )

    (defun cl:yes-or-no-p (&optional (control "" control-p) &rest arguments)
        (declare (values boolean))
        (declare (type format-control control))
        (declare (dynamic-extent arguments))
      (ask (when control-p control) arguments "yes" "no") )
  ) ; labels
