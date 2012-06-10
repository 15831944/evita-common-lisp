;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x86 - runtime - 3 Evaluation and Compilation
;;; arch/x86/lisp/runtime/x86-r03-funobj.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/runtime/x86x64-r03-funobj.lisp#6 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

;;; Layout of Function Object
;;;
;;;                 +---------------+
;;;             +4  |   classd      |
;;;                 +---------------+
;;;             +8  |   cbFunObj    |
;;;                 +---------------+
;;;             +12 |   cookie      |
;;;                 +---------------+
;;;             +16 |   name        |
;;;                 +---------------+
;;;                 |               |
;;;                 |               |
;;;                 | .. codevec .. |
;;;                 |               |
;;;                 |               |
;;;                 +---------------+
;;;                 | ... pad ...   |
;;;                 +---------------+
;;;                 |               |<------+
;;;                 | ... annots ...|       |
;;;                 |               |       |
;;;                 +---------------+       |
;;;                 |               |<--+   |
;;;                 | ... gcmap ... |   |   |
;;;                 |               |   |   |
;;;                 +---------------+   |   |
;;;  cbFunObj-16    |  ofs gcmap  o-+---+   |
;;;                 +---------------+       |
;;;  cbFunObj-12    |  ofs annots o-+-------+
;;;                 +---------------+
;;;  cbFunObj-8     |   frame       |
;;;                 +---------------+
;;;  cbFunObj-4     |   code size   |
;;;                 +---------------+
;;;  cbFunObj



;;;; allocate-funobj
(defun allocate-funobj (classd &key
            annotations
            codevec
            (frame 0)
            gcmap
            name )
    (declare (values native-code-function))
    (declare (type list annotations))
    (declare (type class-description classd))
    (declare (type fixnum frame))
    (declare (type (or (simple-array (unsigned-byte 8) (*)) null)))
    (declare (type (or symbol list) name))
    (declare (ignore frame name))
  (labels (
    ;; roundup
    (roundup (n k)
      (let ((r (logand n (1- k))))
        (if (zerop r) n (+ n (- k r))) ) )

    ;; sizeof-annot
    (sizeof-annot ()
        (declare (values sequence-index))
      (ash (length annotations) 2) )

    ;; sizeof-funobj
    (sizeof-funobj ()
        (declare (values sequence-index))
      (roundup (sizeof-funobj-aux) 16) )

    ;; sizeof-funobj-aux
    (sizeof-funobj-aux ()
        (declare (values sequence-index))
      (+ #.(sizeof 'native-code-function)
         (sizeof-codevec)
         (sizeof-annot)
         (sizeof-gcmap)
         #.(sizeof 'fundesc)) )

    ;; sizeof-codevec
    (sizeof-codevec ()
        (declare (values sequence-index))
      (roundup (length codevec) 4) )

    ;; sizeof-gcmap
    (sizeof-gcmap ()
        (declare (values sequence-index))
      (if (null gcmap)
           4
        (roundup (length gcmap) 4) ) )
    )
    ;;
    (.allocate-funobj classd (sizeof-funobj)) ) )


;;;; initialize-funobj
(defun initialize-funobj (funobj &key
            annotations
            codevec
            (frame 0)
            gcmap
            name )
    (declare (values native-code-function))
    (declare (type native-code-function funobj))
    (declare (type list annotations))
    (declare (type fixnum frame))
    (declare (type native-code-function funobj))
    (declare (type (or (simple-array (unsigned-byte 8) (*)) null)))
    (declare (type (or symbol list) name))
  (labels (
    ;; encode-annon
    (encode-annon (typ)
        (declare (values (unsigned-byte 4)))
        (declare (type keyword typ))
      (or (position typ '(
                :lispval        ; 0
                :ncallee        ; 1
                :lcallee        ; 2
                :not-used-3     ; 3
                :symfun         ; 4
                :symval         ; 5
                :symsetf        ; 6
                :delta          ; 7
                :tlvofs         ; 8
                :xpoint         ; 9
                :clit           ; 10
                :cvar           ; 11
                :dllproc        ; 12
                :not-used-13    ; 13
                :label          ; 14
                :not-used-15 ) ) ; 15
          (error "Invalid annotation type: ~S" typ) ) )

    ;; roundup
    (roundup (n k)
      (let ((r (logand n (1- k))))
        (if (zerop r) n (+ n (- k r))) ) )

    ;; sizeof-annots
    (sizeof-annots ()
        (declare (values sequence-index))
      (ash (length annotations) 2) )
    )
  (let* ((sizeof-header  #.(sizeof 'native-code-function))
         (sizeof-fundesc #.(sizeof 'fundesc))
         (tagof-funobj   #.(tagof  'native-code-function))
         (cookie         #x0F0BCEF4)
         (cb             (ref native-code-function length funobj))
         (ofs-annots     (+ (roundup (length codevec) 4) sizeof-header))
         (ofs-gcmap      (+ ofs-annots (sizeof-annots))) )
    (declare (type sequence-index cb ofs-annots ofs-gcmap))
  (labels (
    ;; fundesc
    (fundesc ()
      (let ((ofs (- cb sizeof-fundesc tagof-funobj)))
        (the fundesc (.unbox-int (+ (.box-int funobj) ofs))) ) )

    ;; init-annot
    (init-annot (typ ofs datum last)
      (case typ
        ((:lispval) (setf (!elt 't funobj ofs) datum))
        ((:ncallee) (init-annot/callee  ofs (register-caller datum funobj)))
        ((:lcallee) (init-annot/callee  ofs datum))
        ((:symfun)  (init-annot/symfun  ofs datum))
        ((:symval)  (init-annot/symval  ofs datum))
        ((:symsetf) (init-annot/symsetf ofs datum))
        ((:delta)   (init-annot/delta   ofs datum last))
        ((:tlvofs)  (init-annot/tlvofs  ofs datum))
        ((:xpoint)  (init-annot/xpoint  ofs datum))
        ((:clit)    (init-annot/clit    ofs datum))
        ((:cvar)    (init-annot/cvar    ofs datum))
        ((:dllproc) (init-annot/dllproc ofs datum))
        ((:label)   (init-annot/label   ofs datum)) ) )

    ;; init-annot/callee
    ;; Note: tag of callee and funobj are same. So, we don't need to think
    ;; about pointer tag.
    (init-annot/callee (ofs callee)
        (declare (type sequence-index ofs))
        (declare (type function callee))
      (setf (!elt 'int32 funobj ofs)
        (- (.box-int callee) (+ (.box-int funobj) ofs 4)) )
      callee )

    ;; init-annot/clit
    (init-annot/clit (ofs clit)
        (declare (type sequence-index ofs))
      (setf (!elt 't funobj ofs) clit) )

    ;; init-annot/cvar
    (init-annot/cvar (ofs cvar)
        (declare (type sequence-index ofs))
      (setf (!elt 't funobj ofs) cvar) )

    ;; init-annot/delta
    (init-annot/delta (ofs obj2 obj1)
        (declare (type sequence-index ofs))
      (setf (!elt 'int funobj ofs)
        (- (.box-int obj1) (.box-int obj2)) )
      obj2 )

    ;; init-annot/dllproc
    #+x86
    (init-annot/dllproc (ofs proc.lib)
      (let ((entry (intern-foreign-entry (cdr proc.lib) (car proc.lib))))
        (setf (!elt 't funobj ofs) entry) ) )

    #+x64
    (init-annot/dllproc (ofs proc.lib)
      (let ((entry (intern-foreign-entry (cdr proc.lib) (car proc.lib))))
        (setf (!elt 'int32 funobj ofs)
          (- (.box-int entry)
              (+ (.box-int funobj)
                  #.(- (sizeof 'native-code-function)
                       (tagof 'native-code-function)
                       -4 ))
              ofs ))
        entry ) )

    ;; init-annot/label
    (init-annot/label (ofs ra)
        (declare (type sequence-index ofs))
        (declare (type sequence-index ra))
      (setf (!elt 'uint funobj ofs)
        (+ (- (.box-int funobj) tagof-funobj) sizeof-header ra) )
      nil )

    ;; init-annot/symfun
    (init-annot/symfun (ofs name)
        (declare (type sequence-index ofs))
        (declare (type symbol name))
      (let ((cell name))
        (setf (!elt 'uint funobj ofs)
            (+ (.box-int cell)
               #.(- (offsetof 'symbol 'function) (tagof 'symbol)) ))
        cell ) )

    ;; init-annot/symsetf
    (init-annot/symsetf (ofs name)
        (declare (type sequence-index ofs))
        (declare (type symbol name))
      (let ((cell (intern-setf-cell name)))
        (setf (!elt 'uint funobj ofs)
            (+ (.box-int cell)
               #.(- (offsetof 'setf-cell 'function) (tagof 'setf-cell)) ))
        cell ) )

    ;; init-annot/symval
    (init-annot/symval (ofs name)
        (declare (type sequence-index ofs))
        (declare (type symbol name))
      (let ((cell (intern-value-cell name)))
        (setf (!elt 'uint funobj ofs)
            (+ (.box-int cell)
               #.(- (offsetof 'value-cell 'value) (tagof 'value-cell)) ))
        cell ) )

    ;; init-annot/tlvofs
    (init-annot/tlvofs (ofs name)
        (declare (type sequence-index ofs))
        (declare (type symbol name))
      (let ((tlvrec (find-value-cell name)))
        (setf (!elt 'uint32 funobj ofs)
          (ash (+ (ref tlv-record index tlvrec) 279) #+x86 2 #+x64 3) )
        tlvrec ) )

    ;; init-annot/xpoint
    (init-annot/xpoint (ofs label)
        (declare (type sequence-index ofs))
      (setf (!elt 'fixnum funobj ofs) label) )

    ;; init-annots
    (init-annots ()
      (loop
        with last = nil
        for aofs = (- ofs-annots sizeof-header) then (+ aofs 4)
        for (typ ofs datum) in annotations do
          (setq last (init-annot typ ofs datum last))
          (setf (!elt 'uint32 funobj aofs)
            (logior (ash ofs 4) (encode-annon typ)) )) )

    ;; init-codevec
    (init-codevec ()
      (let ((runner 0))
        (loop for i below (length codevec) do
          (setf (!elt 'uint8 funobj runner) (elt codevec i))
          (incf runner) )
        (loop until (zerop (logand runner 3)) do
          (setf (!elt 'uint8 funobj runner) #x90)
          (incf runner) ) ) )

    ;; init-desc
    (init-desc ()
        ;; FIXME 2007-02-24: foreign-object isn't compatible with RUNTIMECAST
        ;; instruction. We need to avoid emitting RUNTIMECAST other than
        ;; using safety=0.
        ;(declare (optimize (safety 0)))
      (let ((cb-code (length codevec)))
        (without-garbage-collection
          (let ((desc (fundesc)))
            (setf (ref fundesc code-size    desc) cb-code)
            (setf (ref fundesc frame        desc) frame)
            (setf (ref fundesc annot-offset desc) ofs-annots)
            (setf (ref fundesc gcmap-offset desc) ofs-gcmap) ) ) ) )

    ;; init-gcmap
    (init-gcmap ()
      (let ((ofs (- ofs-gcmap sizeof-header)))
        (if (null gcmap)
            (setf (!elt 'uint32 funobj ofs) 0)
          (loop for i below (length gcmap) do
            (setf (!elt 'uint8 funobj ofs) (elt gcmap i))
            (incf ofs) )) ) )

    ;; init-header
    (init-header ()
      (setf (ref native-code-function name   funobj) name)
      (setf (ref native-code-function cookie funobj) cookie) )
    )
    ;;
    (init-desc)
    (init-codevec)
    (init-gcmap)
    (init-annots)
    (init-header)
    funobj ) ) ) )
