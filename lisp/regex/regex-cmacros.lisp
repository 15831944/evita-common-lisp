;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - Compiler Macros API
;;; lisp/regex/regx-cmacros.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-cmacros.lisp#2 $
;;;
;;; Description:
;;;  This file contains compiler macros for Regex facility.
;
(in-package :si)

;;;; compile-regex
;
(define-compiler-macro compile-regex (source &rest options)
  (labels (
    ;; make-regex-form
    (make-regex-form ()
      (multiple-value-bind (expr name-vector from-end modifiers)
          (apply #'parse-regex source options)

      (multiple-value-bind (scanner-form min-len flags hint)
          (regex-lisp-compile-main expr from-end)

        `(load-time-value
           (make-instance 'regex
              :function    ,scanner-form
              :min-length  ,min-len
              :flags       ,flags
              :scan-hint   ,hint
              :name-vector ,name-vector
              :source      ,source
              :modifiers   ,modifiers )) ) ) )
        )
    ;;
    (when (stringp source) (make-regex-form)) ) )


;;;; eval-regex
;
(define-compiler-macro eval-regex (pattern string &rest keys)
  (when (stringp pattern)
    `(eval-regex (compile-regex ,pattern) ,string ,@keys) ) )


;;;; regex-escape
;
(define-compiler-macro regex-escape (string &key (start 0) end)
  (when (and (stringp string) (integerp start) (or (null end) (integerp end)))
    (regex-escape string :start start :end end) ) )


;;;; regex-match
;;;
;;; Description:
;;;  Compiles pattern into lisp function if it is string.
;
(define-compiler-macro regex-match (pattern string
                                        &rest options
                                        &key (start 0) end )
  (when (stringp pattern)
    `(funcall ,(regex-lisp-compile-inline pattern options)
              ,string ,start ,end ) ) )


;;;; regex-replace
;
(define-compiler-macro regex-replace (replacement pattern string
                                          &rest options )
  (cond
    ((and (stringp pattern) (stringp replacement))
      `(regex-replace ',(regex-compile-replacement replacement)
                      (compile-regex ,pattern)
                      ,string
                      ,@options ) )
    ((stringp pattern)
      `(regex-replace ,replacement
                      (compile-regex ,pattern)
                      ,string
                      ,@options ) )
    ((stringp replacement)
      `(regex-replace ',(regex-compile-replacement replacement)
                      ,pattern
                      ,string
                      ,@options ) )) )


;;;; regex-split
;;;
;;; BUGBUG: NYI: For simple regex such as " ", "[ \t]" and ":", we should
;;; inline regex-split without regex-match object.
;
(define-compiler-macro regex-split (pattern string &rest keys)
  (when (stringp pattern)
    `(regex-split (compile-regex ,pattern) ,string ,@keys) ) )
