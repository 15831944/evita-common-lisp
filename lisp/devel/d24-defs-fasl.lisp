;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 24 System Construction - Defintions
;;; dev/d24-defs-fasl.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d24-defs-fasl.lisp#5 $
;;;
;;; Description:
;;;  This file contains constants and special variables used by FASt Dump
;;; (FASD) and FASt Loader (FASL).
;
(in-package :devel)

;;;; FASL-OP
;;;
;;; Description:
;;;  This enumeration contains operation code of FASL.
;
(ext::defenum FASL-OP ()
    NOP                     ; #x00

    CLASS                   ; #x01
    CLASSD                  ; #x02
    CLEAR                   ; #x03
    DEFUN                   ; #x04
    FUNCALL                 ; #x05
    FUNCALL-1               ; #x06
    IN-PACKAGE              ; #x07
    REFLABEL                ; #x08
    SETLABEL                ; #x09
    START                   ; #x0A

    POSINT                  ; #x0B
    NEGINT                  ; #x0C
    COMPLEX                 ; #x0D
    RATIO                   ; #x0E
    SINGLE-FLOAT            ; #x0F
    DOUBLE-FLOAT            ; #x10

    LIST                    ; #x11
    LIST*                   ; #x12
    CONS                    ; #x13
    NIL                     ; #x14

    HOME                    ; #x15
    KEYWORD                 ; #x16
    SYMBOL                  ; #x17
    GENSYM                  ; #x18
    PACKAGE                 ; #x19

    CHAR                    ; #x1A
    MARKER                  ; #x1B

    VALUE-CELL              ; #x1C
    SETF-CELL               ; #x1D

    FUNOBJ                  ; #x1E

    SIMPLE-STRING
    SIMPLE-VECTOR
    SIMPLE-BIT-VECTOR
    VECTOR
    ARRAY
    UBYTE8-VECTOR
    UBYTE16-VECTOR
    UBYTE32-VECTOR
    SBYTE8-VECTOR
    SBYTE16-VECTOR
    SBYTE32-VECTOR
    SINGLE-FLOAT-VECTOR
    DOUBLE-FLOAT-VECTOR

    PATHNAME                ; for GCL/ansi-test

    #+evm BYTE-CODE-FUNCTION
 ) ; FASL-OP
