;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Editor - Text Buffer Core
;;; editor/ed-text-buffer-defs.lisp
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

(defconstant text-buffer-extension          1024)
(defconstant text-buffer-initial-length     1024)
(defconstant text-buffer-minimum-gap-length 512)

;;;; text-buffer
(defclass text-buffer (editor-object)
  ((chars
        :initform (make-string text-buffer-initial-length)
        :type     simple-string )
   (gap-end
        :initform text-buffer-initial-length
        :type     sequence-index )
   (gap-start
        :initform 0
        :type     sequence-index )
   (length
        :initform 0
        :type     sequence-index ))
  (:metaclass structure-class) )


(declaim
    (ftype (function (text-buffer position) character)
        buffer-char )

    (ftype (function (text-buffer) position)
        buffer-length )

    (ftype (function (text-buffer position position &optional simple-string)
                     simple-string )
        buffer-substr )

    (ftype (function (text-buffer position position) count)
        core-delete )

    (ftype (function (text-buffer position count) text-buffer)
        core-extend )

    (ftype (function (text-buffer
                      position
                      character
                      count )
                     count )
        core-insert-chars )

    (ftype (function (text-buffer
                      position
                      string sequence-index sequence-end )
                     count )
        core-insert-string )

    (ftype (function (text-buffer position) text-buffer)
        core-move-gap )

    (ftype (function (text-buffer position) position)
        validate-position )

    (ftype (function (text-buffer position position) length)
        validate-positions )
 )
