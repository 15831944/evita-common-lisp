;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: EXTENSION; Base: 10 -*-
;;;;
;;;; regex - unicode
;;; lisp/regex/regx-unicode.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-unicode.lisp#1 $
;;;
;;; Description:
;;;  TBD
;
(in-package :si)

;;;; regex-space-char-p
;
(defun regex-space-char-p (char)
    (declare (type character char))
    (declare (values t))
  (or (eql char #\Space)
      (eql char #\Tab)
      (eql char #\Return)
      (eql char #\Newline) ) )


;;;; regex-word-char-p
;
(defun regex-word-char-p (char)
    (declare (type character char))
    (declare (values t))
  (or (alphanumericp char) (eql char #\_)) )
