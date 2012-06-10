;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - package
;;; lisp/regex/regx-defpackage.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-package.lisp#2 $
;;;
;;; Description:
;;;  This file contains definition of external symbols of RegEx facility.
;
(in-package :si)

(dolist (x '(
    ;; Classes
    regex
    regex-match

    ;; Functions
    compile-regex
    eval-regex

    regex-escape
    regex-group-names
    regex-match
    regex-replace
    regex-split

    ;; Methods of regex-match
    first-match
    match-after
    match-before
    match-end
    match-position
    match-start
    match-string
    match-target
    match-target-start
    match-target-end
    matched-p
    next-match

    ;; Macros
    do-match

    ;; Conditions
    regex-error
    regex-parse-error
    ))
    (import x :ext)
    (export x :ext) )
