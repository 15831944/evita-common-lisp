;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - Forward Referenced Class
;;; lisp/clos/o12-class.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o19-forward-referenced-class.lisp#2 $
;;;
;;; Description:
;;;  This file contains method implementations of forward-referenced-class.
;
(in-package :si)

;;;; MOP finalize-inheritance forward-referenced-class
(defmethod clos:finalize-inheritance ((class forward-referenced-class))
  (error "Can't finalize forward referenced class: ~S" class) )


;;;; MOP validate-superclass forward-referenced-class
;
(defmethod clos:validate-superclass
    ((class class) (new-super forward-referenced-class))
  t )

