;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;  evcl - devel - 25 Environment - Functions
;;;; lisp/evm/evm-d00-fns.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/clos-d25-fns.lisp#2 $
;;;
;;; Description:
;;;  This file contains EVM specific functions:
;;;     function-arity
;;;     funciton-frame-variable
;
(in-package :devel)

;;;; function-arity generic-funciton
;;;     => min, max, nrest, nlocs
;
(defmethod function-arity ((fn clos:funcallable-standard-object))
  (function-arity (si::funcallable-instance-function fn)) )


;;;; function-frame-variable
;;; => name, value
;
(defmethod function-frame-variable
        ((fn clos:funcallable-standard-object) ip fp sp nth)
  (function-frame-variable
    (si::funcallable-instance-function fn) ip fp sp nth ) )
