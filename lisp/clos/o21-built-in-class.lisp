;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - Built-In-Class
;;; lisp/clos/o21-built-in-class.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o21-built-in-class.lisp#3 $
;;;
;;; Description:
;;;  This file contains structure related methods.
;;;
;;;  MOP methods:
;;;     finalize-inheritance
;;;
;;;  Public methods:
;;;     allocate-instance
;
(in-package :si)

;;;; allocate-instance built-in-class
;
(defmethod cl:allocate-instance ((class built-in-class) &rest initargs)
    (declare (ignore initargs))
  (error "Can't allocate instance for ~S.~%" class) )


;;;; class-finalized-p built-in-class
;
#+nil
(defmethod clos:class-finalized-p ((class built-in-class))
  t )
