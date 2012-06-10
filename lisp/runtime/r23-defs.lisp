;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 23 Reader
;;; runtime/r23-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r23-defs.lisp#2 $
;;;
;;; Description:
;;;  This file implements following functions:
;
(in-package :si)

#|
    TYPE_Invalid     = 0x000000,
    TYPE_Cons        = 0x000001,    // constituent
    TYPE_NMacro      = 0x000002,    // non-terminating macro
    TYPE_TMacro      = 0x000003,    // terminating macro
    TYPE_Space       = 0x000004,
    TYPE_SEscape     = 0x000005,    // single escape
    TYPE_MEscape     = 0x000006,    // multiple escape
    TYPE_Mask        = 0x00000F,

    TRAIT_Invalid     = 0x000000,
    TRAIT_Alphabetic  = 0x000100,   // bit 8
    TRAIT_Digit       = 0x000200,   // bit 9
    TRAIT_Package     = 0x000400,   // bit 10
    TRAIT_Dot         = 0x000800,   // bit 11
    TRAIT_Decimal     = 0x001000,   // bit 12
    TRAIT_Plus        = 0x002000,   // bit 13
    TRAIT_Minus       = 0x004000,   // bit 14
    TRAIT_Ratio       = 0x008000,   // bit 15
    TRAIT_Dmarker     = 0x010000,   // bit 16
    TRAIT_Emarker     = 0x020000,   // bit 17
    TRAIT_Fmarker     = 0x040000,   // bit 18
    TRAIT_Lmarker     = 0x080000,   // bit 19
    TRAIT_Smarker     = 0x100000,   // bit 20
|#

;;;; Reader Token

(defstruct (reader-token (:constructor (make-reader-token (cstring astring))))
  (cstring  nil :type simple-string)
  (astring  nil :type simple-vector)
  (length   0   :type sequence-index) )

;(xc::define-unsafe-accessors token)


;;;; Reader Control Variables

;;;; 23.2.13
(deftlv *read-base*     10.)

;;;; 23.2.14
(deftlv *read-default-float-format* 'single-float) 

;;;; 23.2.15
(deftlv *read-eval* 't) 

;;;; 23.2.16
(deftlv *read-suppress* nil) 

;;;; 23.2.17
(deftlv *readtable*)


;;;; Reader In Backquote?
;;;
;;; Used by:
;;;     read
;;;     |`-reader|
;;;     |,-reader|
;
(deftlv *read-backquote* nil)


;;;; *reader-label-table*
;;;
;;; For: #n=
;;; For: #n#
;;;
;;; Description:
;;;  A-list for mapping label (fixnum) to object.
;
(deftlv *read-labels*)


;;;; *read-start-line-number*
;;;
;;; Description:
;;;  Line number where current list started or nil. reader-eof-error uses this
;;;  variable to report unclosed list.
;
(deftlv *read-start-line-number* nil)

(deftlv *read-preserving-whitespace* nil)

(deftlv *read-pooled-token* '())


;;;; *reader-linenumber-table*
;;;
;;; Description:
;;;  When this variable contains hash-table, which is EQ-hash-table, line
;;;  number of list start is recorded.
;;;
;;;  Function compile-file binds this variable.
;;;
;
(deftlv *read-line-number-table* nil)


;;;; *standard-readtable*
(defvar *standard-readtable*)


;;;; fast-read-char
;;;
;;; BUGBUG: For performance reason, we use fast-read-char instead of read-char.
;;; Once, we have compiler, we don't use this macro.
;
(define-compiler-macro fast-read-char (stream eof-error-p)
  (unless (symbolp stream)
    (error "Stream variable must be symbol: ~S" stream) )
  (let ((char '#:char))
    (if (not eof-error-p)
        `(let ((,char (stream-read-char ,stream)))
           (if (eq ,char :eof) nil ,char) )
      `(let ((,char (stream-read-char ,stream)))
         (when (eq ,char :eof) (error 'end-of-file :stream ,stream))
         ,char )) ) )
