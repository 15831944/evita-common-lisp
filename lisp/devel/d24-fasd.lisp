;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - devl - 24 System Construction - FASD
;;; lisp/devel/d24-fasd.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d24-fasd.lisp#13 $
;;;
;;; Description:
;;;  This file contains functions for FASt Dump (FASD).
;;;
;;; See Also: sys:src;d24-fasl.lisp
;;;
;;; Internal Functions:
;;;     fasd-enque
;;;     fasd-enque-command
;;;     fasd-serialize
;;;
;;; Public Functions:
;;;     fasd-close
;;;     fasd-force-output
;;;     fasd-open
;;;     fasd-write
;
(in-package :devel)

;;;; fasd-enque
;
(defun fasd-enque (fasd entry)
    (declare (type fasd fasd))
  (setf (slot-value fasd 'qtail)
    (let ((tail (slot-value fasd 'qtail)))
      (setf (cdr tail) (list entry)) ))
  entry )


;;;; fasd-enque-object
;;;
;;; Called by:
;;;   fasd-write
;;;
;;; Description:
;;;  Registers specified object into object table for labeled reference.
;
(defun fasd-enque-object (fasd object)
    (declare (type fasd fasd))
  (fasd-prepare fasd object)
  (fasd-enque fasd (list 'quote object))
  object )


;;;; fasd-prepare
;;;
;;; Description:
;;;   Populates objtab for circular reference.
;
(defun fasd-prepare (fasd object)
    (declare (type fasd fasd))
    (declare (values t))
  (let ((objtab (slot-value fasd 'objtab)))
  (labels (
    ;; good-class-name-p
    (good-class-name-p (class-name)
      (and class-name (symbolp class-name) (symbol-package class-name)) )

    ;; make-load-fun
    ;;  Compiles lambda form as load function of object and returns it.
    ;;  We bind xc::*situation* to compile-file to make compiler process
    ;;  load-time-value form in create-form or init-form.
    (make-load-fun (object lambda-form)
      (let ((fn (let ((xc::*situation* 'compile-file))
                  (compile nil lambda-form) ) ))
        (unless fn (error "Can't create load function for ~S." object))
        fn ) )

    ;; prepare
    ;;  Walk through object to populate object table for detecting identical
    ;;  object.
    (prepare (object)
      ;; Have we already registered?
      (let ((state (gethash object objtab)))
        (case state
          ((nil)
            (prepare-1 object) )
          ((first)
            (setf (gethash object objtab) 'second) )
          ((second))
          (otherwise
            (ecase (first state)
              ((first)
                (setf (first state) 'second) )
              ((second))
              ((load-time-value)
                (prepare/load-time-value state) )
              ((nil)
                (error "Can't reference object itself in createion form: ~S"
                  object ) )) )) ) )

    ;; prepare-1
    (prepare-1 (object)
      (typecase object
        ((or null package fixnum character))
        (number (remember object))
        (symbol (remember object))

        (cons
          (remember object)
          (let ((runner object))
            (loop
              (prepare (pop runner))
              (when (null runner) (return))
              (unless (consp runner) (prepare runner) (return)) ) ) )

        (function
          (remember object)
          (fasd-prepare-funobj fasd object) )

        ((array t)
          (remember object)
          (dotimes (index (array-total-size object))
            (prepare (row-major-aref object index)) ) )

        (array
          (remember object) )

        (class
          ;; Note: class name should not be uninterned symbol. But, it can
          ;; be.
          (let ((class-name (class-name object)))
            (if (good-class-name-p class-name)
                (prepare class-name)
              (prepare/make-load-form object) ) ) )

        (si::class-description
          (let ((class (ref si::class-description class object)))
            (if (and class (good-class-name-p (class-name class)))
                (prepare (class-name class))
              (prepare/make-load-form object) ) ) )

        (si::marker
          (remember object)
          (prepare (ref si::marker si::name object)) )

        (si:setf-cell
          (remember object)
          (prepare (ref si:setf-cell si::name object)) )

        (si:value-cell
          (remember object)
          (prepare (ref si:value-cell si::name object)) )

        (pathname
          (remember object) )

        (otherwise
          (prepare/make-load-form object) )) )

    ;; prepare/load-time-value
    (prepare/load-time-value (state)
      (destructuring-bind (load-fun read-only-p) (rest state)
          (declare (ignore read-only-p))
        (setf (first  state) nil)
        (setf (second state) load-fun)
        (setf (third  state) nil)
        (prepare-1 load-fun)
        (setf (first  state) 'first) ) )

    ;; prepare/make-load-form
    ;;  Note: creation form MUST NOT contains reference to object. In other
    ;;  words, fn can't contain reference to object being prepared. This is
    ;;  unusual situation or bug of user implemented make-load-form method.
    (prepare/make-load-form (object)
      (multiple-value-bind (create-form init-form)
          (make-load-form object)
        (let* ((class-name (class-name (class-of object)))
               (create-fn
                 (make-load-fun object
                   `(lambda ()
                      (declare (ext:lambda-name
                        (make-load-form :create ,class-name )))
                      ,create-form  )) )
               (init-fn
                 (when init-form
                   (let ((var '#:object))
                     (nsubst var object init-form :test #'eq)
                     (make-load-fun object
                        `(lambda (,var)
                            (declare (ext:lambda-name
                              (make-load-form :initialize ,class-name)))
                            ,init-form ) ) )) )
               (state `(nil ,create-fn ,init-fn)) )
          (setf (gethash object objtab) state)
          (prepare-1 create-fn)
          (setf (first state) 'first)
          (when init-fn (prepare-1 init-fn)) ) ) )

    ;; remember
    ;;  Remembers specified objects for labeling.
    (remember (object)
      (setf (gethash object objtab) 'first) )
    )
    (declare (inline remember))
    ;;
    ;; fasd-prepare
    ;;
    (prepare object)
    object ) ) )


;;;; fasd-serialize
;;;
;;; Called by:
;;;   fasd-force-output
;;;
;;; Note: We could make no forward label reference in shared list case:
;;;  #1=(a b (c #1#) d #1#)
;;; However, we must distinct circular list case:
;;;  #1=(a b (c #1#) . #1#)
;;;
;
(defun fasd-serialize (fasd object)
    (declare (type fasd fasd))
    (declare (type t object))
    (declare (values ext:unspecified))
  (let ((objtab (slot-value fasd 'objtab)))
    (declare (type hash-table objtab))
  (labels (
    ;; labeled-p
    (labeled-p (object)
      (let ((state (gethash object objtab)))
        (when state
          (or (not (eq state 'first))
              (and (consp state) (not (eq state 'first))) )) ) )

    ;; new-label
    (new-label (object)
        (declare (values fixnum))
      (let ((label (incf (slot-value fasd 'label-count))))
        (setf (gethash object objtab) label) ) )

    ;; serialize
    (serialize (object)
      (typecase object
        (null
          (fasd-write-byte fasd FASL-OP-NIL) )

        (fixnum
          (if (minusp object)
              (fasd-write-op-uint fasd FASL-OP-NEGINT (- object))
            (fasd-write-op-uint fasd FASL-OP-POSINT object) ) )

        (character
          (fasd-write-op-uint fasd FASL-OP-CHAR (char-code object)) )

        (si::marker
          (fasd-write-op fasd FASL-OP-MARKER)
          (serialize (ref si::marker si::name object)) )

        (si:setf-cell
          (fasd-write-op fasd FASL-OP-SETF-CELL)
          (serialize (ref si::setf-cell si::name object)) )

        (si:value-cell
          (fasd-write-op fasd FASL-OP-VALUE-CELL)
          (serialize (ref si::value-cell si::name object)) )

        (class
          (fasd-write-op fasd FASL-OP-CLASS)
          (serialize (class-name object)) )

        (si:class-description
          (fasd-write-op fasd FASL-OP-CLASSD)
          (serialize (class-name (ref si::classd class object))) )

        (package
          (fasd-write-op-string fasd FASL-OP-PACKAGE
                                (package-name object) ) )

        (t
          (let ((state (gethash object objtab)))
            (etypecase state
              ((eql first)
                (serialize-1 object nil) )
              ((eql second)
                (serialize-1 object (new-label object)) )
              (integer
                (fasd-write-op-uint fasd FASL-OP-REFLABEL state) )
              (cons
                (destructuring-bind (occurence create-fn init-fn) state
                  (when init-fn
                    (fasd-write-op fasd FASL-OP-FUNCALL-1) )

                  (ecase occurence
                    ((first))
                    ((second)
                      (fasd-write-op fasd FASL-OP-SETLABEL) ) )

                  ;; Mark this state invalid. create-fn can't reference
                  ;; object being constructed.
                  (setf (first state) nil)

                  (fasd-write-op fasd FASL-OP-FUNCALL)

                  (serialize create-fn)

                  (when (eq occurence 'second)
                    (new-label object) )

                  (when init-fn
                    (serialize init-fn) ) ) )
              (null
                ;; BUGBUG: For native-code-function
                (serialize-1 object nil) )) ) )) )

    ;; serialize-1
    (serialize-1 (object label)
      (etypecase object
        (symbol
         (serialize-symbol object label) )

        (cons
          (serialize-cons object label) )

        (function
          (fasd-serialize-funobj fasd object label) )

        (simple-string
          (fasd-write-op-string fasd FASL-OP-SIMPLE-STRING object label) )

        (simple-vector
          (let ((length (array-total-size object)))
            (fasd-write-op-uint fasd FASL-OP-SIMPLE-VECTOR length label)
            (dotimes (index length)
              (serialize (row-major-aref object index)) ) ) )

        (simple-bit-vector
          (serialize-binvec FASL-OP-SIMPLE-BIT-VECTOR object label) )

        (si:unsigned-byte-8-vector
          (serialize-binvec FASL-OP-UBYTE8-VECTOR object label) )

        (si:unsigned-byte-16-vector
          (serialize-binvec FASL-OP-UBYTE16-VECTOR object label) )

        (si:unsigned-byte-32-vector
          (serialize-binvec FASL-OP-UBYTE32-VECTOR object label) )

        (si:signed-byte-8-vector
          (serialize-binvec FASL-OP-SBYTE8-VECTOR object label) )

        (si:signed-byte-16-vector
          (serialize-binvec FASL-OP-SBYTE16-VECTOR object label) )

        (si:signed-byte-32-vector
          (serialize-binvec FASL-OP-SBYTE32-VECTOR object label) )

        (si:single-float-vector
          (let ((n (length object)))
            (fasd-write-op-uint fasd FASL-OP-SINGLE-FLOAT-VECTOR n label)
            (dotimes (index n)
              (let ((hx (si::decode-float32 (row-major-aref object index))))
                (fasd-write-uint fasd (logand hx #xffffffff)) ) ) ) )

        (si:double-float-vector
          (let ((n (length object)))
            (fasd-write-op-uint fasd FASL-OP-DOUBLE-FLOAT-VECTOR n label)
            (dotimes (index n)
              (multiple-value-bind (hx lx)
                  (si::decode-float64 (row-major-aref object index))
                (fasd-write-uint fasd (logand hx #xffffffff))
                (fasd-write-uint fasd (logand lx #xffffffff)) ) ) ) )

        ((array t)
          (fasd-write-op-uint fasd FASL-OP-ARRAY (array-rank object) label)
          (dolist (dimension (array-dimensions object))
            (fasd-write-uint fasd dimension) )
          (loop for index from 0 below (array-total-size object) do
            (serialize (row-major-aref object index)) ) )

        (bignum
          (if (minusp object)
              (fasd-write-op-uint fasd FASL-OP-NEGINT (- object) label)
           (fasd-write-op-uint fasd FASL-OP-POSINT object label) ) )

        (ratio
          (fasd-write-op fasd FASL-OP-RATIO label)
          (serialize (numerator object))
          (serialize (denominator object)) )

        (complex
          (fasd-write-op fasd FASL-OP-COMPLEX label)
          (serialize (realpart object))
          (serialize (imagpart object)) )

        (single-float
          (fasd-write-op fasd FASL-OP-SINGLE-FLOAT label)
          (let ((hx (si::decode-float32 object)))
            (fasd-write-uint fasd (logand hx #xffffffff)) ) )

        (double-float
          (fasd-write-op fasd FASL-OP-DOUBLE-FLOAT label)
          (multiple-value-bind (hx lx) (si::decode-float64 object)
            (fasd-write-uint fasd (logand hx #xffffffff))
            (fasd-write-uint fasd (logand lx #xffffffff)) ) )

        (pathname
          (fasd-write-op-string fasd
                FASL-OP-PATHNAME
                (namestring object)
                label ) )
        ) ) ; seiralize-1

    ;; serialize-cons
    (serialize-cons (object label)
        (declare (type cons object))
        (declare (values unspecified))

      ;; Count number of elements in list.
      (let ((nelts 1)
            (runner  object) )
          (declare (type sequence-index nelts))
        (loop
          (setq runner (cdr runner))
          (when (null runner) (return))
          (unless (consp runner) (return))
          (when (labeled-p runner) (return))
          (incf nelts) )

        (cond
          ((null runner)
            ;; An object is a proper-list.
            (fasd-write-op-uint fasd FASL-OP-LIST nelts label) )
          ((eql nelts 1)
            ;; An object is a cons.
            (fasd-write-op fasd FASL-OP-CONS label) )
          (t
            ;; An object is a dotted-list. +1 for dotted object.
            (fasd-write-op-uint fasd FASL-OP-LIST* (1+ nelts) label) )) )

      ;; Serialize elements in list.
      (let ((runner object))
        (loop
          (serialize (car runner))
          (setq runner (cdr runner))
          (when (null runner) (return))
          (when (or (not (consp runner)) (labeled-p runner))
            (serialize runner)
            (return) )) ) )

    ;; serialize-symbol
    (serialize-symbol (object label)
      (cond
        ((null (symbol-package object))
          (fasd-write-op-string fasd FASL-OP-GENSYM
                                (symbol-name object) label ) )

        ((eq (symbol-package object) #.(symbol-package :keyword))
          (fasd-write-op-string fasd FASL-OP-KEYWORD
                                (symbol-name object) label ) )

        ((eq (find-symbol (symbol-name object)) object)
          (fasd-write-op-string fasd FASL-OP-HOME
                                (symbol-name object) label ) )

        (t
          (fasd-write-op-string fasd FASL-OP-SYMBOL
                                       (symbol-name object) label )

          (let* ((package (symbol-package object))
                 (pname1  (package-name package))
                 (pname2  (first (package-nicknames package))) )
            (fasd-write-string fasd
              (cond
                ((not pname2) pname1)
                ((< (length pname2) (length pname1)) pname2)
                (t pname1) )) ) )) )

    ;; serialize-binvec
    (serialize-binvec (op-code vector label)
      (let ((nelts (array-total-size vector)))
        (fasd-write-op-uint fasd op-code nelts label)
        (loop
          for index from 0 below nelts do
            (fasd-write-uint fasd (row-major-aref vector index)) ) ) )
    )
    ;;
    ;; fasd-serialize
    ;;
    (serialize object) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;


;;;; fasd-close
(defun fasd-close (fasd abort-p)
    (declare (type fasd fasd))
    (declare (values null))
  (unwind-protect
      ;; Note: We should not cause error during fasd-force-output. However,
      ;; it can be occured. Even if error is occured, we must close FASD
      ;; stream.
      ;;
      ;; make-load-form is user defined function. So, it can cause error
      ;; other than our bug.
      (unless abort-p
        (setq abort-p t)
        (fasd-force-output fasd)
        (setq abort-p nil) )
    (let ((stream (slot-value fasd 'stream)))
      (close stream :abort abort-p) )) )


;;;; fasd-force-output
;;;
;;; Description:
;;;  Processes FASD queue then clear it. We don't keep object table for further
;;;  shared object references.
;
(defun fasd-force-output (fasd)
    (declare (type fasd fasd))
    (declare (values null))

  (dolist (entry (rest (slot-value fasd 'qhead)))
    (cond
      ((eq 'defun entry)
        (fasd-write-op fasd FASL-OP-DEFUN) )

      ((eq 'funcall entry)
          (fasd-write-op fasd FASL-OP-FUNCALL) )

      ((eq 'in-package entry)
          (fasd-write-op fasd FASL-OP-IN-PACKAGE) )

      ((xc::one-argument-form-p entry 'quote)
          (fasd-serialize fasd (second entry)) )

      (t
        (error "Unknown FASD entry: ~S" entry) )) )

    (fasd-write-op fasd FASL-OP-CLEAR)

    (let ((qhead (slot-value fasd 'qhead)))
      (setf (cdr qhead) nil)
      (setf (slot-value fasd 'qtail) qhead)
      (clrhash (slot-value fasd 'objtab)) )
    nil )


;;;; fasd-open
;
(defun fasd-open (pathname)
    (declare (type ext:pathname-designator pathname))
    (declare (values fasd))
  (let* ((stream (open pathname :direction    :output
                                :element-type 'unsigned-byte
                                :if-exists    :supersede ))
         (fasd
           (make-instance 'fasd
              :stream stream
              :objtab (make-hash-table :test #'equal)
              :queue  (list 0) ) ))

    (fasd-write-byte      fasd FASL-OP-START)
    (fasd-write-op-string fasd FASL-OP-SIMPLE-STRING "FaSl1")

    ;; Note: To make comparison of two FASL files easier, we don't put
    ;; timestamp into FASL file. This change is requested by compiler
    ;; team.
    #+nil
    (fasd-write-op-uint fasd FASL-OP-POSINT
      (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 2002 0)) )

    fasd ) )


;;;; fasd-write
;
(defun fasd-write (command fasd)
    (declare (type fasd fasd))
  (case (first command)
    ((defun)
      (fasd-enque fasd 'defun)
      (fasd-enque-object fasd (rest command)) )
    ((funcall)
      (fasd-enque fasd 'funcall)
      (fasd-enque-object fasd (second command)) )
    ((in-package)
      (fasd-enque fasd 'in-package)
      (fasd-enque-object fasd (second command)) )
    (otherwise
      (error "Bad FASD command: ~S" command) )) )


;;;; fasd-write-byte
;
(defun fasd-write-byte (fasd byte)
    (declare (type fasd fasd))
    (declare (type (unsigned-byte 8) byte))
  (write-byte byte (slot-value fasd 'stream)) )


;;;; fasd-write-int
;
(defun fasd-write-int (fasd int)
    (declare (type fasd fasd))
  (if (not (minusp int))
      (fasd-write-uint fasd 0)
    (progn
      (fasd-write-uint fasd 1)
      (setq int (- int)) ))
    (fasd-write-uint fasd int) )


;;;; fasd-write-op
;
(defun fasd-write-op (fasd op-code &optional label)
    (declare (type fasd fasd))
  (when label (setq op-code (logior #x80 op-code)))
  (fasd-write-byte fasd op-code) )


;;;; fasd-write-op-string
;
(defun fasd-write-op-string (fasd op-code string &optional label)
    (declare (type fasd fasd))
  (fasd-write-op     fasd op-code label)
  (fasd-write-string fasd string) )


;;;; fasd-write-op-uint
;
(defun fasd-write-op-uint (fasd op-code uint &optional label)
    (declare (type fasd fasd))
  (fasd-write-op  fasd op-code label)
  (fasd-write-uint fasd uint) )


;;;; fasd-write-string
;
(defun fasd-write-string (fasd string)
    (declare (type fasd fasd))
  (fasd-write-uint fasd (length string))
  (loop for char across string do
    (fasd-write-uint fasd (char-code char)) ) )


;;;; fasd-write-uint
;
(defun fasd-write-uint (fasd uint &optional (flag 0))
    (declare (type fasd fasd))
    (declare (type unsigned-byte uint))
    (declare (type (unsigned-byte 8) flag))
    (declare (values ext:unspecified))
  (when (>= uint #x80)
    (fasd-write-uint fasd (ash uint -7) #x80) )
  (fasd-write-byte fasd (logior flag (ldb (byte 7 0) uint))) )
