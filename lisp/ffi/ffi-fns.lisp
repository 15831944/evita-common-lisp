;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - runtime - ffi - functions
;;; /lisp/ffi/ffi-fns.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/ffi/ffi-fns.lisp#1 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

;;;; print-object dll-proc-info
(defmethod cl:print-object ((o dll-proc-info) s)
  (print-unreadable-object (o s :type t)
    (let ((lib (slot-value o 'file-info)))
      (format s "~S in ~S"
        (slot-value o 'proc-name)
        (slot-value lib 'filename) ) ) ) o )


;;;; print-object dll-file-info
(defmethod cl:print-object ((o dll-file-info) s)
  (print-unreadable-object (o s :type t)
    (prin1 (slot-value o 'filename) s) ) )
