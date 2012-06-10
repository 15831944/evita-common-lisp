;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 19 Filenames - Definitions
;;; lisp/runtime/r19-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r19-defs.lisp#2 $
;;;
;;; Description:
;;;  This file contains definitions for filename.
;
(in-package :si)


#|
;;;; basic-host
;
(defstruct (basic-host
              (:conc-name "HOST-")
              (:constructor nil)
              (:predicate nil) )
  (name           nil :type string)
  (local-case     nil :type (member :mixed :upcase :downcase :preserve))
  (customary-case nil :type (member :uppder :lower))
  (default-device :unspecific) )
|#

(defclass basic-host (structure-object)
  ((name
        :initarg    :name
        :type       string )
   (local-case
        :initarg    :local-case
        :type       (member :mixed :upcase :downcase :preserve) )
   (customary-case
        :initarg    customary-case
        :type       (member :uppder :lower) )
   (default-device
        :initarg    :default-device
        :initform   :unspecific) )
  (:metaclass structure-class) )


;;;; pathname
;
(defclass pathname (structure-object)
  ((host
        :initarg    :host
        :initform   nil )
   (device
        :initarg    :device
        :initform   nil )
   (directory
        :initarg    :directory
        :initform   nil )
   (name
        :initarg    :name
        :initform   nil )
   (type
        :initarg    :type
        :initform   nil )
   (version
        :initarg    :version
        :initform   nil ))
  (:metaclass structure-class) )


;;;; logical-pathname
;
(defclass logical-pathname (pathname) () (:metaclass structure-class))


;;;; physical-pathname
;
(defclass physical-pathname (pathname) () (:metaclass structure-class))



;;;; *pathname-hosts*
;;;
;;; Used by:
;;;  parse-namestring
;;;
;;; Description:
;;;  List of installed pathname hosts.
;
(defvar *pathname-hosts* '())
(defvar *pathname-hosts-latch* (make-latch '*pathname-hosts*))
