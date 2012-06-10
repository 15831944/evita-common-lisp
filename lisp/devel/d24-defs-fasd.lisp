;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 24 System Construction - Defintions
;;; lisp/devel/d24-defs-fasd.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d24-defs-fasd.lisp#5 $
;;;
;;; Description:
;;;  This file contains constants and special variables used by FASt Dump
;;; (FASD) and FASt Loader (FASL).
;
(in-package :devel)

;;;
;;; fasd.objtab
;;;  Contains hash-table to map object to occurence indicator:
;;;    first
;;;       Specified object is processed at once.
;;;    second
;;;       Specified object is occured more than once.
;;;    positive integer
;;;       Specified object is labeled by positive integer n.
;;;   (occurence create-fn init-fn)
;;;     Specified object will be created by calling function create-fn and
;;;     initialized by init-fn. This state implements object with
;;;     make-load-form and load-time-value.
;;;
;;;     Occurence is one of following symobls:
;;;       first     -- create-fn and init-fn are compiled and set.
;;;       second    -- object is referenced more than once.
;;;       nil       -- during prepareing create-fn
;;;       load-time-value -- from notice-load-time-value.

;;;; fasd
;
(defclass fasd ()
 ((objtab
    :type       hash-table
    :initarg    :objtab )
  (qhead
    :type       cons
    :initarg    :queue)
  (qtail
    :type       cons
    :initarg    :queue)
  (label-count
    :type       fixnum
    :initform   0 )
  (stream
    :type       stream
    :initarg    :stream ))
 (:metaclass structure-class) )

(deftype fasl-opcode () '(unsigned-byte 8))


(declaim (ftype (function (fasd t) null) fasd-close))
(declaim (ftype (function (fasd t) t) fasd-enque))
(declaim (ftype (function (fasd t) t) fasd-enque-object))
(declaim (ftype (function (fasd) null) fasd-force-output))
(declaim (ftype (function (pathname-designator) fasd) fasd-open))
(declaim (ftype (function (fasd t) t) fasd-prepare))
(declaim (ftype (function (fasd t) unspecified) fasd-serialize))
(declaim (ftype (function (t fasd) unspecified) fasd-write))
(declaim (ftype (function (fasd (unsigned-byte 8)) unspecified)
    fasd-write-byte ))
(declaim (ftype (function (fasd integer) unspecified) fasd-write-int))
(declaim (ftype (function (fasd fasl-opcode &optional t) unspecified)
    fasd-write-op ))
(declaim (ftype (function (fasd fasl-opcode string &optional t) unspecified)
    fasd-write-op-string ))
(declaim (ftype (function (fasd fasl-opcode unsigned-byte &optional t)
                          unspecified )
    fasd-write-op-uint ))
(declaim (ftype (function (fasd string) unspecified)
    fasd-write-string ))
(declaim (ftype (function (fasd unsigned-byte &optional (unsigned-byte 8))
                          unspecified )
    fasd-write-uint ))

(declaim (ftype (function (fasd function) unspecified)
    fasd-prepare-funobj ))

(declaim (ftype (function (fasd function t) unspecified)
    fasd-serialize-funobj ))
