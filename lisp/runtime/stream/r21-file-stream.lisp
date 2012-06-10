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
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/stream/r21-file-stream.lisp#4 $
;;;
;;; Description:
;;;  This file implements following functions:
;
(in-package :si)

;;;; print-object file-stream
(defmethod cl:print-object ((object file-stream) stream)
  (labels (
    ;; encode-flag
    (encode-flag (object)
      (let ((flags (ref file-stream flags (ref instance storage object))))
        (case (logand flags STREAM-FLAG-DIRMASK)
          ((0) :probe)
          ((1) :input)
          ((2) :output)
          ((3) :io) ) ) )
    )
    ;;
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~@[~A ~]~S~:_ ~S~:_ ~S"
          (unless (open-stream-p stream) 'closed)
          (stream-element-type object)
          (encode-flag object)
          (ref file-stream pathname (ref instance storage object)) ) ) ) )


;;;; stream-element-type
(defmethod cl:stream-element-type ((stream file-stream))
    (declare (values type-specifier))
  (ref file-stream element-type (ref instance storage stream)) )


;;;; stream-pathname
;;;
;;; Note: CLIM:GRAY
;
(defmethod ext:stream-pathname ((stream file-stream))
    (declare (values pathname))
  (ref file-stream pathname (ref instance storage stream)) )


;;;; stream-truename
;;;
;;; Note: CLIM:GRAY
;
(defmethod ext:stream-truename ((stream file-stream))
    (declare (values pathname))
  (truename (ref file-stream pathname (ref instance storage stream))) )


;;;; close around file-stream
;;; Description:
;;;  Cleanup at closing:
;;;    Abort:
;;      o Delete created file.
;;;     o Rename to original file name.
;;;    Normal
;;;     o Delete original file if stream is created with :if-exists
;;;        rename-and-delete.
(defmethod cl:close :around ((stream file-stream) &key abort)
  (labels (
    (cleanup ()
      (when (output-stream-p stream)
        (if abort
            (let* ((filename (namestring (slot-value stream 'pathname)))
                   (plist   (slot-value stream 'plist))
                   (renamed (getf plist 'renamed)) )
              (.delete-file filename)
              (when renamed (.rename-file renamed filename)) )
          (let* ((plist   (slot-value stream 'plist))
                 (renamed (getf plist 'renamed)) )
            (when (and renamed (getf plist 'rename-and-delete))
              (.delete-file renamed) ) ))) )
    )
    ;;
    (when (call-next-method) (cleanup) t) ) )
