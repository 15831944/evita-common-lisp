;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - runtime - ffi - definitions
;;; /lisp/ffi/ffi-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/ffi/ffi-defs.lisp#2 $
;;;
;;; Description:
;;;  This file contains definition of foreign-class releated classed.

;;; Note:
;;;  Classes must reside in boot.image.
;
(in-package :si)

(defclass foreign-class (class) ())

(defclass foreign-slot-definition (slot-definition) ())

(defclass foreign-direct-slot-definition
    (foreign-slot-definition direct-slot-definition) () )

(defclass foreign-effective-slot-definition
    (foreign-slot-definition effective-slot-definition) () )

(defclass foreign-object (t)
  ()
  (:metaclass foreign-class) )
