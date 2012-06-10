;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - definition classes and types
;;; lisp/regex/regx-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-defs.lisp#4 $
;;;
;;; Description:
;;;  This file contains definition of external symbols of Regex facility.
;
(in-package :si)

(defconstant regex-infinity cl:array-total-size-limit)

(deftype regex-case ()
    '(member nil :capitalized :downcase :upper) )

(deftype regex-designator ()
    '(or regex string regex-match) )

(deftype regex-expr ()
  '(or string cons symbol character fixnum) )

(deftype regex-hint ()
  '(or simple-vector null) )

;;;; regex-context
(defclass regex-context ()
  (;; Parse time workarea
   (buffer
      :reader regex-context-buffer
      :initform (make-array 100 :element-type 'character
                    :fill-pointer 0
                    :adjustable t )
      :type string )

   ;; Compile time workarea
   (codevec
      :initform (make-array 100 :fill-pointer 0 :adjustable t)
      :type     vector )
   (capture-vector
      :initform (make-array 10)
      :type     simple-vector )
   (bitvec
      :initform (make-array 65536 :element-type 'bit)
      :type     simple-bit-vector )

   ;; Runtime workarea
   (vstack
      :initform (make-array 99)
      :type     simple-vector )
   (cstack
      :initform (make-array 199)
      :type     simple-vector ))
  (:metaclass structure-class) )


;;;; regex
;;;
;;; Description:
;;;  Represents regular expression in internal form.
(defclass regex ()
  ((function
      ; "A function which execute this regex."
      :initarg  :function
      :initform (required)
      :type     function )
   (min-length
      ; "Minimum number of characters to match this regex."
      :initarg  :min-length
      :initform (required)
      :type     sequence-index )
   (name-vector
      ; "Each element contains group name or nil if anonymous."
      :initarg  :name-vector
      :initform (required)
      :type     simple-vector )
   (flags
      :initarg  :flags
      :initform 0
      :type     fixnum )
   (scan-hint
      :initarg  :scan-hint
      :initform nil
      :type     (or simple-vector nil) )

   ;; for describe-object
   (codevec
      :initarg  :codevec
      :initform nil
      :type     t )

   ;; For print-object
   (source
      ; "Source form of this regular expression."
      :initarg  :source
      :initform (required)
      :type     string )
   (modifiers
      ; "Modifiers when this regex was compiled."
      :initarg  :modifiers
      :initform (required)
      :type     fixnum )) )

;;;; REGEX-MOD
;;;
;;; Description:
;;;  Modifiers for parsing regular expression.
(ext::defenum REGEX-MOD ()
  (IGNORE-CASE      1)
  (MULTILINE        2)
  (SINGLE-LINE      4)
  (IGNORE-SPACE     8)
  (UNICODE         16) )


;;;; REGEX-FLAG
(ext::defenum REGEX-FLAG ()
  (SCAN-FORWARD     0)
  (SCAN-BACKWARD    1)

  (SCAN-ALL.F        0  "Try all characters (forward)")
  (SCAN-ALL.B        1  "Try all characters (backward)")
  (SCAN-BOS.F        2  "Try once at start of string.")
  (SCAN-BOS.B        3  "Try once at start of string.")
  (SCAN-EOS.F        4  "Try once at end of string.")
  (SCAN-EOS.B        5  "Try once at end of string.")
  (SCAN-POS.F        6  "Try once at end of last match (forward).")
  (SCAN-POS.B        7  "Try once at end of last match (backward).")
  (SCAN-MBOL.F       8  "Try at every start of lines.")
  (SCAN-MBOL.B       9  "Try at every start of lines.")
  (SCAN-MEOL.F      10  "Try at every start of lines.")
  (SCAN-MEOL.B      11  "Try at every start of lines.")
  (SCAN-SEOL.F      12  "Try at every end of lines.")
  (SCAN-SEOL.B      13  "Try at every end of lines.")
  (SCAN-FUN.F       14  "Using forward scanner.")
  (SCAN-FUN.B       15  "Using backward scanner.")
  (SCAN-MASK        15)

  (SCAN-ONCE        16  "Match once") )


;;;; regex-match
;;;
;;; Slots:
;;;   position -- A fixnum
;;;    Position where starts matching.
(defclass regex-match ()
  ((regex
      :initarg  :regex
      :initform (required)
      :type     regex )
   (string
      :reader   match-target
      :initarg  :string
      :initform (required)
      :type     simple-string )
   (start
      :reader   match-target-start
      :initarg  :start
      :initform (required)
      :type     sequence-index )
   (end
      :reader   match-target-end
      :initarg  :end
      :initform (required)
      :type     sequence-index )
   (start-vector
      :initarg  :start-vector
      :initform (required)
      :type     simple-vector )
   (end-vector
      :initarg  :end-vector
      :initform (required)
      :type     simple-vector )
   (position
      :accessor match-position
      :initarg  :position
      :initform (required)
      :type     fixnum )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function Declarations
;;;

(declaim (ftype (function (
                    string
                    &key
                        (:end   sequence-end)
                        (:start sequence-index)
                        &allow-other-keys )
                    regex )
    compile-regex ) )


(declaim (ftype (function (
                    regex-designator
                    string
                    &key
                        (:end   sequence-end)
                        (:start sequence-index)
                        &allow-other-keys )
                    (or regex-match null) )
    eval-regex ) )


(declaim (ftype (function
    (regex-match string &optional sequence-index sequence-end)
    regex-match )
  first-match ))

(declaim (ftype (function
    (regex-match)
    (or string null) )
  match-after
  match-before ))

(declaim (ftype (function
    (regex-match &optional (or sequence-index keyword))
    (or sequence-index null) )
  match-end
  match-start ))

(declaim (ftype (function
    (regex-match)
    sequence-index )
  match-group-count ))


(declaim (ftype (function
    (regex-match &optional (or sequence-index keyword))
    (or string null) )
  match-string ))

(declaim (ftype (function
    (regex-match)
    t )
  matched-p ))


(declaim (ftype (function (regex-match) t) next-match))


(declaim (ftype (function (
                    string
                    &key
                        (:capture           t)
                        (:debug             t)
                        (:end               sequence-end)
                        (:extended-syntax   t)
                        (:from-end          t)
                        (:ignore-case       t)
                        (:multiple-lines    t)
                        (:single-line       t)
                        (:start             sequence-index)
                        (:target            t)
                        (:unicode           t) )
                  (values regex-expr simple-vector t fixnum) )
    parse-regex ) )


(declaim (ftype (function (regex) t) regex-backward-p))

(declaim (ftype (function (string &rest t) regex)
    regex-byte-compile ) )

(declaim (ftype (function (
            regex-expr  ; expr
            t )         ; from-end
        simple-vector )
    regex-byte-compile-matcher ) )

(declaim (ftype (function (
            fixnum          ; flags
            regex-hint )    ; hint
        t )
    regex-byte-compile-scanner ) )

(declaim (ftype (function (
            regex-match
            simple-vector )
        t )
    regex-execute-byte-code ) )

(declaim (ftype (function (string &optional sequence-index sequence-end)
                          list )
  regex-compile-replacement ) )

(declaim (ftype (function (regex-match (or sequence-index keyword))
                          (values sequence-end) )
  regex-ensure-group-index ) )

(declaim (ftype (function (
                    string
                    &key
                        (:end sequence-end)
                        (:start sequence-index) )
                    string )
    regex-escape ) )

(declaim (ftype (function (list regex-match t stream) unspecified)
    regex-eval-replacement-backward
    regex-eval-replacement-forward ) )

(declaim (ftype (function () regex-context)
    regex-get-context ) )

(declaim (ftype (function (regex) list)
    regex-group-names ) )

(declaim (ftype (function (string &rest t) regex)
    regex-lisp-compile ) )

(declaim (ftype (function (
                    cons                ; scanner-form
                    cons                ; matcher-form
                    t                   ; from-end
                    sequence-index      ; max-local-index
                    form                ; string-start
                    form                ; string-end
                    form                ; string
                    form                ; position
                    form                ; start-vec
                    form )              ; end-vec
                cons )
    regex-lisp-compile-aux ) )

(declaim (ftype (function (string list) t)
    regex-lisp-compile-inline ) )

(declaim (ftype (function (regex-expr t)
                          (values
                                t
                                sequence-index
                                fixnum
                                (or simple-vector null) ))
    regex-lisp-compile-main ) )


(declaim (ftype (function (
            regex-expr  ; expr
            t           ; from-end
            symbol )    ; match
        (values form sequence-index) )
    regex-lisp-compile-matcher ) )

(declaim (ftype (function (
            regex-expr
            fixnum
            regex-hint )
        (values form sequence-index fixnum regex-hint) )
    regex-lisp-compile-scanner ) )

(declaim (ftype (function (
            regex-designator
            simple-string
            sequence-index
            sequence-index
            &rest t )
        regex-match )
    regex-make-match ) )


(declaim (ftype
    (function (
            regex-designator
            string
            &key
                (:capture   t)
                (:end       sequence-end)
                (:start     sequence-index)
            &allow-other-keys )
        t )
    regex-match ) )

(declaim (ftype (function (character) t)
    regex-name-char-p ) )

(declaim (ftype (function (
            (or string list symbol function)
            regex-designator
            string
            &key
                (:count         (or integer null))
                (:end           sequence-end)
                (:from-end      t)
                (:preserve-case t)
                (:start         sequence-index) )
        string )
    regex-replace ) )

(declaim (ftype (function (character) t)
    regex-space-char-p
    regex-word-char-p ) )

(declaim (ftype (function (
        regex-designator
        string
        &key
            (:end   sequence-end)
            (:limit sequence-end)
            (:start sequence-index) )
        list )
    regex-split ) )

(declaim (ftype (function (regex-expr) (values fixnum fixnum t t))
    regex-study-length ) )

(declaim (ftype (function (regex-expr t) (values fixnum regex-hint))
    regex-study-scan ) )

(declaim (ftype (function (list) (values list list))
    regex-study-union ) )

(declaim (ftype (function (list) list)
    regex-study-union-2 ) )

(declaim (ftype (function (string) regex-case)
    regex-string-case ))

(declaim (ftype (function (
        string
        regex-case
        stream
        &optional
            sequence-index
            sequence-end )
        unspecified )
    write-case-string ))

(declaim (ftype (function
                    (string regex-case stream
                        &optional sequence-index sequence-end )
                    unspecified )
    write-reverse-case-string ))

(declaim (ftype (function (string stream &key
                                (:start sequence-index)
                                (:end sequence-end) )
                           string )
  write-reverse-string ) )


;;;; *regex-context*
(deftlv *regex-context* nil)


;;;; *regex-unicode-block-table*
;;;
;;; Description:
;;;  This variable contains hash-table for mapping Unicode block name (string)
;;;  to code point range (cons).
;;;
;;;  Block name is normalized with following rules:
;;;   1. All characters are convert to uppercase equaivalent.
;;;   2. All spaces and punctuations are removed.
;;;
;;;  See regex-unicode-blocks.lisp contains program for populating this
;;;  hash-table.
;
(defvar *regex-unicode-block-table*)


;;;; *regex-unicode-category-table*
;
(defvar *regex-unicode-category-table*)


;;;; debug-format
;;;
;;; For debugging.
;
(defvar *regex-debug* nil)

(defmacro REGEX-DEBUG (control &rest args)
 `(when *regex-debug*
    (format t ,control ,@args) ) )
