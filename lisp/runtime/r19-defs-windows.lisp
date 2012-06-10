;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 19 Filenames - Windows File System
;;; lisp/runtime/r19-defs-windows.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r19-defs-windows.lisp#2 $
;;;
;
(in-package :si)


; device    string, nil or :unspecific  (no wild)
; directory cons, nil, or :unspecific. This isn't (:relative).
; name      string, cons, nil, :wild, :wild-1, or :unspecific
; type      string, cons, nil, :wild, :wild-1, or :unspecific
; version   nil, :newest, or :unspecific


;;; o Maximum path length is 259. (MAX_PATH includes trailing zero)
;;; o CON, PRN, AUX, CLOCK$, NUL, COM[1-9], LPT[1-9] are reserved word.
;;; o Reserved words followed by an extension are invalid. e.g. NUL.txt.

;;; Syntax:
;;;  namestring  ::= remote-namestring | local-namestring
;;;  remote-namestring ::= '//' name '/' name '/' directory* name ['.' type]
;;;  local-namestring  ::= [ letter ':' ] ['/'] directory* name ['.' type]
;;;


;;;; windows-host
;
(defclass windows-host (basic-host)
  ((local-case      :initform   :preserved)
   (customary-case  :initform   :downcase)
   (default-device  :initform   nil) )
  (:metaclass structure-class) )


;;;; windows-pathname
;
(defclass windows-pathname (physical-pathname)
  ()
  (:metaclass structure-class) )


#|
;;;; *windows-hosts*
;
(defvar *windows-hosts* '())
|#


;;;; +windows-reserved-chars+
;;;
;;; Description:
;;;  String which contains reserved characters in Windows file system.
;;;  Windows also reserved U+0000 through U+001F.
;
(defconstant +windows-reserved-chars+ "<>:/\"\\|")
