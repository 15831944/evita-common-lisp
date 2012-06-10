;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: X86x64; Base: 10 -*-
;;;;
;;;; evcl - X86/X64 Assembler - Declarations
;;; arch/x86x64/lisp/devel/asm/x86x64-asm-parse.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/asm/x86x64-asm-parse.lisp#4 $
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

;;;; asm-match-operand-p asm-annot
(defmethod asm-match-operand-p ((opd asm-annot) opdfmt opdsiz)
  (case opdfmt
    ((Iz) (eql opdsiz 32))
    ((Iv) t)
    ((Jv) (eq (slot-value opd 'kind) :function)) ) )


;;;; asm-match-operand-p asm-ea
(defmethod asm-match-operand-p ((opd asm-ea) opdfmt opdsiz)
  (case opdfmt
    ((Eb)
      (eql (slot-value opd 'size) 8) )
    ((Ed)
      (eql (slot-value opd 'size) 32) )
    ((Ev Ev64)
      (eql (slot-value opd 'size) opdsiz) )
    ((Ew)
      (eql (slot-value opd 'size) 16) )
    ((Ob)
      (and (eql (slot-value opd 'size) 8)
           (null (slot-value opd 'base))
           (null (slot-value opd 'index)) ) )
    ((Ov)
      (and (eql (slot-value opd 'size) opdsiz)
           (null (slot-value opd 'base))
           (null (slot-value opd 'index)) ) )
    ((Wpd Wps)
      (eql (slot-value opd 'size) 128) )
    ((Wsd)
      (eql (slot-value opd 'size) 64) )
    ((Wss)
      (eql (slot-value opd 'size) 32) )) )


;;;; asm-match-operand-p register
(defmethod asm-match-operand-p ((opd register) opdfmt opdsiz)
  (labels (
    ;; match-gpr-p
    (match-gpr-p (opd)
      (case opdfmt
        ((AL AH BL BH CL CH DL DH)
          (string= opdfmt (slot-value opd 'name)) )
        ((DX)
          (string= opdfmt (slot-value opd 'name)) )
        ;;
        ((eAX)
          (match-gpr-p-aux opd 0) )
        ((eBP)
          (match-gpr-p-aux opd 5) )
        ((eBX)
          (match-gpr-p-aux opd 3) )
        ((eCX)
          (match-gpr-p-aux opd 1) )
        ((eDI)
          (match-gpr-p-aux opd 7) )
        ((eDX)
          (match-gpr-p-aux opd 2) )
        ((eSP)
          (match-gpr-p-aux opd 4) )
        ((eSI)
          (match-gpr-p-aux opd 6) )
        ;;
        ((Eb Gb)
          (eql (slot-value opd 'size) 8) )
        ((Ed Gd)
          (eql (slot-value opd 'size) 32) )
        ((Ev Gv Ev64)
          (eql (slot-value opd 'size) opdsiz) )
        ((Ew Gw)
          (eql (slot-value opd 'size) 16) )) )

    ;; match-gpr-p-aux
    (match-gpr-p-aux (opd code)
      (and (eql (slot-value opd 'size) opdsiz)
           (eql (logand (slot-value opd 'code) 7) code) ) )

    ;; match-xmm-p
    (match-xmm-p (opd)
      (case opdfmt
        ((Vpd Vps Vsd Vss Wpd Wps Wsd Wsp)
          (eq (slot-value opd 'regclass) :xmm) )) )
    )
    (ecase (slot-value opd 'regclass)
      ((:gpr) (match-gpr-p opd))
      ((:xmm) (match-xmm-p opd)) ) ) )


;;;; asm-match-operand-p asm-labelref
(defmethod asm-match-operand-p ((opd asm-labelref) opdfmt opdsiz)
    (declare (ignore opdsiz))
  (case opdfmt
    ((Iz) t)
    ((Jb) t)
    ((Jv) t) ) )


;;;; asm-match-operand-p integer
(defmethod asm-match-operand-p ((opd integer) opdfmt opdsiz)
  (case opdfmt
    ((1) (eql opd 1))
    ((Ib) (<= (integer-length opd) 7))
    ((Iw) (<= (integer-length opd) 16))
    ((Iz) (<= (integer-length opd) 32)) 
    ((Iv) (<= (integer-length opd) opdsiz)) ) )


;;;; asm-parse-operand
(defun asm-parse-operand (ctx form)
    (declare (type asm-context ctx))
  (labels (
    ;; invalid
    (invalid (form form1)
        (declare (ignore form1))
      (asm-error ctx "Invalid effective address ~S" form) )

    ;; parse/cons
    (parse/cons (form)
        (declare (type cons form))
      (if  (list-length form)
           (or (parse-annot form form) (parse-ea form))
        (invalid form form) ) )

    ;; parse/symbol
    ;;  register ::= symbol
    ;;  label    ::= symbol
    (parse/symbol (sym)
        (declare (type symbol sym))
      (or (parse/symbol/register sym)
          (parse-label sym sym :absolute) ) )

    ;; parse/symbol/register
    (parse/symbol/register (sym)
      (let* ((key (find-symbol (symbol-name sym) :x86x64))
             (reg (gethash key (slot-value (slot-value ctx 'isa) 'regtab))) )
        (when (typep reg 'register)
          (if (not (eq (slot-value reg 'regclass) :idx))
              reg
            (asm-error ctx
                "Scaled index ~A is used other than effective address."
                reg ))) ) )

    ;; parse-annot
    (parse-annot (form form1)
      (case (parse-op form1)
        ((:clit)      (parse-annot/2 form form1 :clit))
        ((:cvar)      (parse-annot/2 form form1 :cvar))
        ((:dll)       (parse-annot/dll form form1))
        ((:function)  (parse-annot/2 form form1 :function))
        ((:offsetof)  (parse-offsetof form form1))
        ((:quote)     (parse-annot/2 form form1 :lispval))
        ((:relative)  (parse-label   form form1 :relative))
        ((:symval)    (parse-annot/2 form form1 :symval))
        ((:symsetf)   (parse-annot/2 form form1 :symsetf))
        ((:tlv)       (parse-annot/2 form form1 :tlvofs)) ) )

    ;; parse-annot/2
    (parse-annot/2 (form form1 kind)
        (declare (ignore form))
      (make-instance 'asm-annot
            :kind   kind
            :datum  (second form1) ))

    ;; parse-annot/dll
    (parse-annot/dll (form form1)
        (declare (ignore form))
      (destructuring-bind (dll lname pname) form1
            (declare (ignore dll))
        (unless (stringp lname)
          (asm-error ctx "DLL library name must be a string: ~S" lname) )
        (unless (stringp pname)
          (asm-error ctx "DLL procedure name must be a string: ~S" pname) )
        (make-instance 'asm-annot
            :kind   :dllproc
            :datum  (cons pname lname) ) ) )

    ;; parse-ea
    (parse-ea (form)
      (case (parse-op form)
        ((:byte)      (parse-ea-aux (rest form) 8))
        ((:dword)     (parse-ea-aux (rest form) 32))
        ((:dqword)    (parse-ea-aux (rest form) 128))
        ((:qword)     (parse-ea-aux (rest form) 64))
        ((:word)      (parse-ea-aux (rest form) 16))
        (otherwise    (parse-ea-aux form 0)) ) )

    ;; parse-ea-aux
    (parse-ea-aux (form opdsiz)
        (declare (type cons form))
        (declare (type (member 0 8 16 32 64 128) opdsiz))
      (let ((base  nil)
            (index nil)
            (disp  nil)
            (annot nil) )
        (dolist (form1 form)
          (let ((opd (parse-ea-1 form form1)))
            (etypecase opd
              ((or asm-annot asm-labelref)
                (cond
                  ((and (null disp) (null annot))
                    (setq disp opd)
                    (setq annot opd) )
                  (t
                    (asm-error ctx "Only one displacemtn is allowd: ~S"
                        form ) )) )
              (register
                (case (slot-value opd 'regclass)
                  ((:gpr)
                    (cond
                      ((null base)  (setq base opd))
                      ((null index) (setq index opd))
                      (t (invalid form form1)) ) )
                  ((:idx)
                    (cond
                      ((null index) (setq index opd))
                      (t (invalid form form1)) ) )
                  (otherwise
                    (invalid form form1) )) )
              (integer
                (cond
                  ((and (null disp) (null annot)) (setq disp opd))
                  (t (invalid form form1)) ) ) )) )
        (when annot
          (cond
            ((null base))
            ((null index)
              (setq base index)
              (setq index nil) )))
        (make-instance 'asm-ea
            :size   opdsiz
            :base   base
            :index  index
            :disp   (or disp 0) ) ) )

    ;; parse-ea-1
    (parse-ea-1 (form form1)
      (typecase form1
        (cons   (parse-ea/cons form form1))
        (integer form1)
        (symbol (parse-ea/symbol form form1))
        (otherwise (invalid form form1)) ) )

    ;; parse-ea/cons
    (parse-ea/cons (form form1)
      (or (parse-annot form form1)
          (invalid form form1) ) )

    ;; parse-ea/symbol
    (parse-ea/symbol (form form1)
      (let ((key (find-symbol (symbol-name form1) :x86x64)))
        (or (gethash key (slot-value (slot-value ctx 'isa) 'regtab))
            (parse-label form form1 :absolute) ) ) )

    ;; parse-label
    (parse-label (form sym kind)
        (declare (ignore form))
      (make-instance 'asm-labelref
            :kind   kind
            :bblock (parse-label-aux sym) ))

    ;; parse-label-aux
    (parse-label-aux (sym)
      (or (find sym (slot-value ctx 'bblocks)
                    :key (lambda (x) (slot-value x 'name)) )
          (let ((bb (make-instance 'asm-bblock :name sym)))
            (push bb (slot-value ctx 'bblocks))
            bb )) )

    ;; parse-offsetof
    (parse-offsetof (form form1)
        (declare (ignore form))
      (let ((class-name (second form1))
            (slot-name  (third  form1)) )
        (- (si::offsetof class-name slot-name) (si::tagof class-name)) ) )

    ;; parse-op
    (parse-op (form)
      (let ((op (first form)))
        (and (symbolp op) (find-symbol (symbol-name op) :keyword)) ) )
    )
  (typecase form
    (cons   (parse/cons form))
    (integer form)
    (null    (make-instance 'asm-annot :kind :lispval :datum nil))
    (symbol  (parse/symbol form))
    (otherwise
      (asm-error ctx "Invalid oeprand ~S" form)
      0 )) ) )


;;;; asm-parse-form
(defun asm-parse-form (ctx form)
  (labels (
    ;; get-format
    (get-format (mnemonic operands opdsiz)
      (labels (
        ;; invalid
        (invalid (formats)
          (asm-error ctx "Invalid operands ~S. ~A"
            form
            (mapcar #'fourth formats) ) )

        ;; match-p
        (match-p (format &aux (opdfmts (fourth format)))
          (when (eql (length opdfmts) (length operands))
            (loop
              for opdfmt in opdfmts
              for opd in operands do
                (unless (asm-match-operand-p opd opdfmt opdsiz) (return nil))
              finally (return t) )) )
        )
        ;;
      (let ((formats (gethash mnemonic *mnemonic-table*)))
        (if (null formats)
            (asm-error ctx "Undefined mnemonic ~A" mnemonic)
          (dolist (format formats (invalid formats))
            (when (match-p format) (return format)) )) ) ) )

    ;; parse-form
    (parse-form (form)
      (typecase form
        (cons       (parse-form/cons form))
        (null       (asm-error ctx "Can't use NIL as label."))
        (symbol     (parse-form/symbol form))
        (otherwise  (asm-error ctx "Invalid form: ~S" form)) ) )

    ;; parse-form/cons
    (parse-form/cons (form)
      (let ((mnemonic (first form)))
        (if (not (symbolp mnemonic))
            (asm-error ctx "Invalid mnemonic ~S" mnemonic)
          (let ((mnemonic (find-symbol (symbol-name mnemonic) :x86x64)))
            (if (null mnemonic)
                (asm-error ctx "Undefined mnemonic ~A" (first form))
              (let ((fn (gethash mnemonic *asm-pseudo-insn-table*)))
                (if fn
                   (funcall fn ctx form)
                  (multiple-value-bind (operands opdsiz)
                      (parse-operands form)
                    (let ((format (get-format mnemonic operands opdsiz)))
                      (when format
                        (asm-encode-insn ctx format operands)) ) )) )) )) ) )

    ;; parse-form/symbl
    (parse-form/symbol (form)
      (let ((bb (or (find form (slot-value ctx 'bblocks)
                        :key (lambda (bb) (slot-value bb 'name))
                        :test #'eq )
                    (let ((bb (make-instance 'asm-bblock
                                    :name form ) ))
                      (push bb (slot-value ctx 'bblocks))
                      bb )) ))
        (if (slot-value bb 'kind)
            (asm-error ctx "Label ~S is appeared more than once." form)
          (progn
            (setf (slot-value bb 'address)  (slot-value ctx 'address))
            (setf (slot-value bb 'original) (slot-value ctx 'address))
            (setf (slot-value bb 'offset)   (slot-value ctx 'offset))
            (setf (slot-value bb 'kind)     :code)
            (setf (slot-value ctx 'bblock) bb) )) ) )

    ;; parse-operans
    (parse-operands (form)
      (let ((operands
              (loop for form1 in (rest form)
                collect (asm-parse-operand ctx form1) ) ))
        (fix-operands-size operands) ) )

    ;; fix-operands-size
    (fix-operands-size (operands)
      (let ((opdsiz (compute-operands-size operands)))
        (dolist (opd operands (values operands opdsiz))
          (when (and (typep opd 'asm-ea) (eql (slot-value opd 'size) 0))
            (setf (slot-value opd 'size) opdsiz) ) ) ) )

    ;; compute-operands-size
    (compute-operands-size (operands)
      (let ((opdsiz 0))
        (dolist (opd operands (if (eql opdsiz 0) 32 opdsiz))
          (when (eql opdsiz 0)
            (typecase opd
              (asm-ea
                (unless (eql (slot-value opd 'size) 0)
                  (setq opdsiz (slot-value opd 'size)) ) )
              (register
                (setq opdsiz (slot-value opd 'size)) )
              (asm-labelref
                (setq opdsiz
                  (ecase (slot-value opd 'kind)
                    ((:relative) 32)
                    ((:absolute)
                      (slot-value (slot-value ctx 'isa) 'size) ))) ))) ) ) )
    )
    (parse-form form) ) )
