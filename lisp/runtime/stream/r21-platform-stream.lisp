;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 21 Streams - Stream Stream
;;; runtime/r21-stream-stream.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/stream/r21-platform-stream.lisp#2 $
;;;
;;; Description:
;;;  This file contains stream methods for stream tream.
;
(in-package :si)


;;;; stream-element-type
;
(defmethod cl:stream-element-type ((stream platform-stream))
    (declare (values type-specifier))
  'character )


;;;; stream-external-format
;
(defmethod cl:stream-external-format ((stream platform-stream))
    (declare (values t))
  (.platform-stream-external-format stream) )


;;;; unrealize-instance
;
(defmethod ext:unrealize-instance ((stream platform-stream))
  (close stream) )
