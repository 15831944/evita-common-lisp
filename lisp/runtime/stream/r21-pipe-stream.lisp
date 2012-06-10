;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 21 Streams - Pipe
;;; runtime/r21-pipe-stream.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/stream/r21-pipe-stream.lisp#3 $
;;;
;;; Description:
;;;  This file contains default methods of stream operation.
;
(in-package :si)

;;;; make-pipe-stream
;
(defun make-pipe-stream (&key (external-format :default))
    (declare (values pipe-stream))
  (let ((pipe-stream
          (make-instance 'pipe-stream
            :external-format external-format ) ))
    (realize-instance pipe-stream) ) )
