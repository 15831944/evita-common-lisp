;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 21 Streams - Declarations
;;; runtime/r21-stream-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/stream/r21-stream-defs.lisp#4 $
;;;
;;; Description:
;;;  This file contains default methods of stream operation.
;;;
;;;     stream
;;;         input-stream
;;;             concatenated-stream
;;;         io-stream
;;;             echo-stream
;;;             synonym-stream
;;;             two-way-stream
;;;         output-stream
;;;             broadcast-stream
;;;         platform-stream
;;;             console-stream
;;;             file-stream
;;;             null-stream
;;;             pipe-strem
;;;             socket-stream
;;;
;
(in-package :si)

(defconstant STREAM-FLAG-PROBE     0)
(defconstant STREAM-FLAG-INPUT     1)
(defconstant STREAM-FLAG-OUTPUT    2)
(defconstant STREAM-FLAG-BOTH      3)
(defconstant STREAM-FLAG-DIRMASK   3)

(defconstant STREAM-FLAG-CLOSED         4)
(defconstant STREAM-FLAG-INTERACTIVE    8)


;;;; 21.2.1 stream
;
(defclass cl:stream ()
  ((flags
      :initarg :flags
      :initform 0
      :reader   stream-flags
      :type fixnum )) )


(defclass input-stream (stream)
  ((flags :initform STREAM-FLAG-INPUT)) )


(defclass io-stream (stream)
  ((flags :initform STREAM-FLAG-BOTH)) )


(defclass output-stream (stream)
  ((flags :initform STREAM-FLAG-OUTPUT)) )



;;;; 21.2.2  broadcast-stream
;;;; 21.2.40 broadcast-stream-streams
;
(defclass cl:broadcast-stream (output-stream)
  ((streams
        :initarg :streams
        :type    list
        :reader  cl:broadcast-stream-streams )) )


;;;; 21.2.3  concatenated-stream
;;;  21.2.46 concatenated-stream-streams
;
(defclass cl:concatenated-stream (input-stream)
  ((streams
        :initarg :streams
        :type    list
        :reader  cl:concatenated-stream-streams )) )


;;;; 21.2.4 echo-stream
;;;; 21.2.44 echo-stream-input-stream
;;;; 21.2.44 echo-stream-output-stream
;
(defclass cl:echo-stream (io-stream)
  ((input-stream
        :initarg    :input-stream
        :type       stream
        :reader     cl:echo-stream-input-stream )
   (output-stream
        :initarg    :output-stream
        :type       stream
        :reader     cl:echo-stream-output-stream)
   (unread-p
        :initform nil
        :type t) ) )


;;;; platform-stream
(defclass platform-stream (stream)
  ((blob :type fixnum :initform 0)
   (external-format
        :initarg    :external-format
        :initform   0
        :type       t )) )


;;;; 21.2.5 file-stream
(defclass cl:file-stream (platform-stream)
  ((element-type
        :type    type-specifier
        :initarg :element-type )
   (pathname
        :type    pathname
        :initarg :pathname )
   (plist
        :type       list
        :initarg    :plist
        :initform   nil )) )


;;;; console-stream
(defclass console-stream (platform-stream) ())


;;;; pipe-stream
(defclass pipe-stream (platform-stream io-stream) ())


;;;; 21.2.6 string-stream
(defclass cl:string-stream (stream) ())


;;;; string-input-stream
(defclass string-input-stream (string-stream input-stream)
  ((string
        :initarg :string
        :type string )
   (index
        :initarg :start
        :type ext:sequence-index )
   (end
        :initarg :end
        :type ext:sequence-index ) ) )


;;;; string-output-stream
(defclass string-output-stream (string-stream output-stream)
  ((string
        :initarg :string
        :type string
        :initform (make-array 40 :element-type 'character
                                 :fill-pointer 0
                                 :adjustable t ))
   (column
        :initform 0
        :type ext:sequence-index )) )


;;;; 21.2.7  synonym-stream
;;;; 21.2.39 synonym-stream-symbol
;;; synonym-stream doesn't use slot flags.
(defclass cl:synonym-stream (stream)
  ((symbol
        :initarg :symbol
        :type    symbol
        :reader  cl:synonym-stream-symbol )) )


;;;; 21.2.8  two-way-stream
;;;; 21.2.43 two-way-stream-input-stream
;;;; 21.2.43 two-way-stream-output-stream
;
(defclass cl:two-way-stream (io-stream)
  ((input-stream
        :initarg :input-stream
        :type stream
        :reader cl:two-way-stream-input-stream )
   (output-stream
        :initarg :output-stream
        :type stream
        :reader cl:two-way-stream-output-stream )) )
