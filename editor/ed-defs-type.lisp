;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Editor - Text Buffer Core
;;; editor/ed-buffer-core-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r00-fns.lisp#2 $
;;;
;;;
;;; Description:
;;;  Editor types
;
(in-package :editor)

(deftype count ()
    '(integer #.(ash most-negative-fixnum -1)
              #.(ash most-positive-fixnum -1) ) )

(deftype length () 'sequence-index)
(deftype position () 'sequence-index)

(defclass editor-object (structure-object)
  ()
  (:metaclass structure-class) )


(defmethod cl:print-object ((o editor-object) stream)
  (print-unreadable-object (o stream :type t :identity t))
  o )
