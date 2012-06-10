;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 50 Extensions - Thread Local Variable
;;; macro/m50-tlv.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m50-tlv.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     deftlv
;
(in-package :xc)

;;;; ext:deftlv
;;;
;;; Syntax:
;;;     ext:deftlv name &optional initform doc-string
;;;         => name
;
(defmacro ext:deftlv (name &optional (initform nil initform-p) doc-string)
  (cond
    (doc-string `(defvar ,name ,initform ,doc-string))
    (initform-p `(defvar ,name ,initform))
    (t          `(defvar ,name)) ) )
