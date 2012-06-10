;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XC; Base: 10 -*-
;;;;
;;;; evcl - X86/X64 Assembler - Declarations
;;; arch/x86x64/lisp/devel/asm/x86x64-asm-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/asm/x86x64-asm-defs.lisp#1 $
;;;
;;; Description:
;;;  This fils contains declarations for X86 assembler:
;;;
;;; Macros:
;;;   defasm
;;;
;;; Note:We don't support 16-bit addressing mode.
;
(in-package :x86x64)

(export '(assemble defasm))

;;;; asm-anchor
(defclass asm-anchor ()
  ((address
        :type sequence-index
        :initform   0
        :initarg    :address ))
  (:metaclass structure-class) )


;;;; asm-annot
(defclass asm-annot (asm-anchor)
  ((kind
        :type symbol
        :initarg    :kind )
   (datum
        :type       t
        :initarg    :datum ))
  (:metaclass structure-class) )


;;;; asm-bblock
(defclass asm-bblock (asm-anchor)
  ((offset
        :type       sequence-index
        :initarg    :offset
        :initform   0 )
   (name
        :type       symbol
        :initarg    :name
        :initform   nil )
   (align
        :type       (member 1 2 4 8 16)
        :initarg    :align
        :initform   1 )
   (original
        :type       sequence-index
        :initarg    :address
        :initform   0 )
   (kind
        :type       (member nil :code :align)
        :initarg    :kind
        :initform   nil ))
  (:metaclass structure-class) )


;;;; asm-operand
(defclass asm-operand ()
  ((size
        :type (integer 0 8 16 32 64 128)
        :initarg  :size
        :initform 0 ) )
  (:metaclass structure-class) )


;;;; asm-ea
(defclass asm-ea (asm-operand)
  ((base
        :type       (or asm-gpr null)
        :initarg    :base
        :initform   nil )
   (index
        :type       (or asm-idx null)
        :initarg    :index
        :initform   nil )
   (disp
        :type       (or integer asm-bblock)
        :initarg    :disp
        :initform   0 ))
  (:metaclass structure-class) )


;;;; asm-labelref
(defclass asm-labelref (asm-anchor)
  ((kind
        :type       (member :absolute :relative)
        :initform   (required)
        :initarg    :kind )
   (offset
        :type       sequence-index
        :initform   0
        :initarg    :offset )
   (longp
        :type       boolean
        :initform   nil )
   (bblock
        :type       asm-bblock
        :initarg    :bblock ))
  (:metaclass structure-class) )


;;;; asm-context
(defclass asm-context ()
  ((frame-size
        :type       sequence-index
        :initform   #+x86 4 #+x64 8
        :initarg    :frame-size )
   (frame-type
        :type       (member :fixed :restiry)
        :initform   :fixed
        :initarg    :frame-type )
   (class
        :type       symbol
        :initform   'si::native-code-function  )

   (isa
        :type       isa
        :initarg    :isa )
   ;;
   (failures
        :type       list
        :initform   '() )
   (warnings
        :type       list
        :initform   '() )
   (forms
        :type       list
        :initform   '() )
   ;;
   (address
        :type       sequence-index
        :initform   0 )
   (offset
        :type       sequence-index
        :initform   0 )
   (bblock
        :type       asm-bblock
        :initarg    :bblock )
   ;;
   (anchors
        :type       list
        :initform   '() )
   (bblocks
        :type       list
        :initform   '()
        :initarg    :bblocks )
   (codebuf
        :type       (simple-array (unsigned-byte 8) (*))
        :initform   (make-array 256 :element-type '(unsigned-byte 8)) ))
  (:metaclass structure-class) )


;;;; %assemble
(declaim (ftype (function (t list list) function)
  %assemble ))


;;;; parse-assemble
(declaim (ftype (function (isa symbol list list) list)
  parse-assemble ))


;;;; assemble
(defmacro assemble (name lambda-list &body form*)
  (let ((isa (find-isa #+x86 :x86 #+x64 :x64)))
    (let ((initargs (parse-assemble isa name lambda-list form*)))
      (if initargs
          `(%assemble ',name ',lambda-list ',initargs)
        (error "Failed to assemble ~S" name) ) ) ) )


;;;; defasm
(defmacro defasm (name lambda-list &body form*)
 `(progn
    (eval-when (:compile-toplevel)
       (c::%defun ',name ',lambda-list) )
    (si::%defun ',name ',lambda-list
        (assemble ,name ,lambda-list ,@form*) )) )


;;;; define-pseudo-instruction
(defmacro define-pseudo-instruction (name lambda-list &body decl*-form*)
  (let ((key (intern (symbol-name name) :x86x64)))
   `(progn
      (setf (gethash ',key *asm-pseudo-insn-table*)
        (lambda (ctx form)
            (declare (type asm-context ctx))
            (declare (type cons form))
            (declare (lambda-name (psuedo-instruction ,key)))
          (block ,name
            (destructuring-bind ,lambda-list (rest form)
              ,@decl*-form* )) ))
      ',key ) ) )


;;;; *asm-context*
(deftlv *asm-context*)
(declaim (type asm-context *asm-context*))


;;;; *asm-pseudo-insn-table*
(defvar *asm-pseudo-insn-table* (make-hash-table :test 'eq))

(declaim (ftype (function (function-name list list) function-name)
  %defasm ) )

(declaim (ftype (function (asm-context cons list) unspecified)
  asm-encode-insn ))

(declaim (ftype (function (asm-context (unsigned-byte 8)) (unsigned-byte 8))
  asm-emit-u8 ) )

(declaim (ftype (function (asm-context string &rest t) null)
  asm-error ))

(declaim (ftype (function (asm-context t) t)
  asm-parse-form ))

(declaim (ftype (function (asm-context t) t)
  asm-parse-operand ))

(defgeneric asm-match-operand-p (opd opdfmt opdsiz))

(declaim (ftype (function (t cons (member 8 16 32 64 128)) t)
  asm-match-operand-p ))
