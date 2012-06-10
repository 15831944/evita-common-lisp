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
;;;  Class definition of Text Buffer Core.
;
(in-package :editor)

;;;; editor-buffer
(defclass editor-buffer (edit-buffer)
  ((filename
        :initform nil
        :type     (or pathname null) )
   (filetime
        :initform 0
        :type     (integer 0 *) )
   (name
        :initarg  :name
        :initform nil
        :type     simple-string )
   (next
        :initform nil
        :type     (or editor-buffer null) )
   (prev
        :initform nil
        :type     (or editor-buffer null) ) )
  (:metaclass structure-class) )


(define-dlinked-anchor editor-buffers editor-buffer)


;;;; editor-core
(defclass editor-core (editor-object)
  ((buffers
        :initform (make-instance 'editor-buffers)
        :type     editor-buffers ) )
  (:metaclass structure-class) )
