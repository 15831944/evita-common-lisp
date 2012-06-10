;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - byte-code definitions
;;; lisp.Regex.Regx-byte-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-byte-defs.lisp#3 $
;;;
;;; Description:
;;;  This file contains regex compiler.
;
(in-package :si)

;;;; *regex-instruction-vector*
;
(defvar *regex-instruction-vector* (make-array 100))


;;;; Regex Byte Code Interpreter Instruction Set
;
(macrolet (
  (define-instruction-set (&rest format*)
    (loop
      for opcode = 0 then (1+ opcode)
      for (mnemonic . operand*) in format*
      for symbol = (intern (format nil "REGEX-OP-~A" mnemonic))
        collect `(defconstant ,symbol ,opcode) into forms
        collect `(setf (svref *regex-instruction-vector* ,opcode)
                    #(,(1+ (length operand*)) ,mnemonic ,operand*) )
          into forms
      finally (return `(progn ,@forms)) ) )
  )
  ;;
  (define-instruction-set
      (capture.F            nth)
      (capture.B            nth)
      (capture-if-not       label nth)
      (capture=.F           nth)
      (capture=.B           nth)
      (capture-equal.F      nth)
      (capture-equal.B      nth)
      (capture-makunbound   nth)

      (category=.F      category)
      (category=.B      category)
      (category/=.F     category)
      (category/=.B     category)
      (category<=.F     category category)
      (category<=.B     category category)
      (category>.F      category category)
      (category>.B      category category)

      (char=.B          char)
      (char/=.B         char)

      (char=.F          char)
      (char/=.F         char)

      (char-eq.B        char)
      (char-ne.B        char)

      (char-eq.F        char)
      (char-ne.F        char)

      (char<=.B         min-char max-char)
      (char>.B          min-char max-char)

      (char<=.F         min-char max-char)
      (char>.F          min-char max-char)

      (char-le.B        min-char max-char)
      (char-gt.B        min-char max-char)

      (char-le.F        min-char max-char)
      (char-gt.F        min-char max-char)

      (charset=.B       min-char max-char string)
      (charset=.F       min-char max-char string)

      (charset/=.B      min-char max-char string)
      (charset/=.F      min-char max-char string)

      (charset-eq.B     min-char max-char string)
      (charset-eq.F     min-char max-char string)

      (charset-ne.B     min-char max-char string)
      (charset-ne.F     min-char max-char string)

      (fail -)   ; has dummy operand for branch optimation.

      (go label)

      (max label n)  ; for counter loop
      (min label n)  ; for counter loop

      (last.F rest)
      (last.B rest)

      (nulc label)
      (null label)

      (or label)

      (push label)   ; or L1, go L2, L1
      (push-int n)
      (push-pos)

      (repeat-any.F     min-rest)
      (repeat-any.B     min-rest)

      (repeat-char=.F   min-rest char)
      (repeat-char=.B   min-rest char)
      (repeat-char/=.F  min-rest char)
      (repeat-char/=.B  min-rest char)
      (repeat-char<=.F  min-rest min-char max-char)
      (repeat-char<=.B  min-rest min-char max-char)

      (repeat-char-eq.F  min-rest char)
      (repeat-char-eq.B  min-rest char)
      (repeat-char-ne.F  min-rest char)
      (repeat-char-ne.B  min-rest char)

      (rest.F min-rest)
      (rest.B min-rest)

      (restore-cxp)      ; for lookaround and atomic-grouping
      (restore-pos)      ; for lookaround and atomic-grouping

      (save-cxp)         ; for lookaround and atomic-grouping
      (save-pos)         ; for positive-lookaround

      (success -)        ; has dummy operand for branch optimation.

      (string=.F string)
      (string=.B string)
      (string-equal.F string)
      (string-equal.B string)

      (:any.F)           ; Matches any character. ".", "\p{ANY}"
      (:any.B)           ; Matches any character. ".", "\p{ANY}"

      (:boundary=)       ; Matches word boundary. "\b"
      (:boundary/=)      ; Matches not word boundary. ("\B")

      (:bos)             ; Matches start of string. "(?-m)^", "\A"
      (:eos)             ; Matches end of string. "\z"

      (:mbol)            ; Matches start of line. "(?m:^)"
      (:meol.F)          ; Matches end of line. "(?m:$)"

      (:pos.F)           ; Matches end of last match. "\G"
      (:pos.B)           ; Matches end of last match. "\G"

      (:seol.F)          ; Matches end of line. "(?-m:$)", "\Z"

      (:space=.F)        ; Matches space. "\s"
      (:space=.B)        ; Matches space. "\s"
      (:space/=.F)       ; Matches not space. "\S".
      (:space/=.B)       ; Matches not space. "\S".

      (:word=.F)         ; Matches word character. "\w"
      (:word=.B)         ; Matches word character. "\w"
      (:word/=.F)        ; Matches not word character. "\W".
      (:word/=.B)        ; Matches not word character. "\W".
) )


;;;; Regex Byte Code Interpreter Backtrack Code
(ext::defenum REGEX-CTRL ()
  CAPTURE       ; nth start end             -- 0
  CONTINUE      ; next-pc pos               -- 1
  FAIL          ; n/a                       -- 2
  POP-INT       ; int                       -- 3
  POP-POS       ; pos                       -- 4
  PUSH-INT      ; int                       -- 5
  PUSH-POS      ; pos                       -- 6
  REPEAT.F      ; next-pc pos min-pos       -- 7
  REPEAT.B      ; next-pc pos max-pos       -- 8
  SAVE-CXP      ; cxp vsp                   -- 9
  SAVE-POS      ; pos                       -- 10
 ) ; REGEX-CTRL


;;;; regex-instruction-size
(defun regex-instruction-size (opcode)
    (declare (type sequence-index opcode))
    (declare (values sequence-index))
  (let ((info (svref *regex-instruction-vector* opcode)))
    (when (null info)
      (error "Undefined regex opcode: ~S" opcode) )
    (svref info 0) ) )
