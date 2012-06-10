;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - Generic-function
;;; lisp/clos/o11-gf.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o14-gf.lisp#2 $
;;;
;;; Description:
;;;  This file contains generic-function implementations.
;;;
;
(in-package :si)

;;;; 7.7.3 allocate-instance (funcallable-standard-class)
;
(defmethod cl:allocate-instance ((class funcallable-standard-class)
                                 &rest initargs )
    (declare (ignore initargs))
  (unless (class-finalized-p class) (finalize-inheritance class))
  (let ((classd (slot-value class 'instance-description)))
    (.allocate-funcallable-instance classd) ) )


;;;; 7.7.5 shard-initialize :after (generic-function)
;;;
;;; Description:
;;;  Updates param-info structure.
;
(defmethod cl:shared-initialize :after
        ((gf generic-function)
         slot-names
         &key
         ((:argument-precedence-order apo) nil apo-p)
         (lambda-list nil lambda-list-p) )
    (declare (ignore slot-names))
  (when (or lambda-list-p apo-p)
    (set-generic-function-lambda-list gf lambda-list apo) ) )


;;;; ext:function-name generic-function
;
(defmethod ext:function-name ((fn generic-function))
  (generic-function-name fn) )
