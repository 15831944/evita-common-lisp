;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Runtime - 22 Printer - Pool
;;; runtime/r22-defs-pprint.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r22-defs-pprint.lisp#8 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;
;
(in-package :si)

;;;; pprint-dispatch-table
;;;
;;; Slots:
;;;   cons-entries -- A hash-table.
;;;    Maps symbol at car to dispatch entry.
;;;
;;;   entries -- A list of dispatch entry.
;;;    A dispatch entry is list of three elements, type specifier, priorty, and
;;;    pretty printer function.
;;;
;;; See Also:
;;;     copy-pprint-dispatch
;;;     pprint-dispatch
;;;     set-pprint-dispatch
;;;
;;; BUGBUG: We should make dispatch function from entries, instead of scan
;;; them at runtime.
;
(defstruct (pp-dispatch-table
             (:print-object   print-structure-unreadably)
             (:constructor  make-pp-dispatch-table ())
             (:copier       nil)
             (:predicate    nil) )
  (cons-entries (make-hash-table :test #'eq) :type hash-table)
  (entries      '() :type list) )


;;;; pp-stream
;;;
;;; Description:
;;;  Stream for pretty-printing.
;;;
;;; BUGBUG: left-margin?
;;;
;;; Slots:
;;;  stream     A base stream. Results of pretty printing are sent to this
;;;             stream.
;;;
;;; Character buffer related slots:
;;;  cbuffer    Character buffer.
;;;  cindex     Number of characters in cbuffer.
;;;  suffix-len Length of suffix string.
;;;
;;; Heap:
;;;  heap       A simple-vector represents heap to store pp-op. Zeroth
;;;             element contains free block list.
;;;
;;; Formatting:
;;;   right-margin
;;;   column
;;;   nlines    number of lines printed so far. For *print-lines*
;;;
;
(defclass pp-stream (output-stream)
  ((stream       :initform nil :type (or null stream))

   (right-margin :initform 0    :type ext:sequence-index)
   (column       :initform 0    :type ext:sequence-index)
   (nlines       :initform 0    :type ext:sequence-index)

   (cbuffer      :initform nil  :type (or simple-string null))
   (cindex       :initform 0    :type ext:sequence-index)
   (suffix-len   :initform 0    :type ext:sequence-index)

   (block-ptr    :initform 0    :type ext:sequence-index)

   (heap        :initform #()   :type simple-vector)
   (head-ptr    :initform 0     :type ext:sequence-index)
   (scan-ptr    :initform 0     :type ext:sequence-index)
   (tail-ptr    :initform 0     :type ext:sequence-index) )
 ) ; pp-stream


;;;; pp-newline
;;;
;;; Description:
;;;  A kind of conditional newline.
(deftype pp-newline ()
  '(member :fill :linear :literal :mandatory :miser) )


;;;; pp-operation
;;;
;;; Description:
;;;  A type of pp-op structure.
(deftype pp-operation ()
  '(member :block :current :end :fill :linear :miser :start) )


(declaim (ftype (function (pp-stream) unspecified) pp-clear-output))
(declaim (ftype (function (stream sequence-index) t) pp-fit-line-p))

(declaim (ftype (function (pp-stream sequence-index t)  unspecified)
  pp-line-break ) )

(declaim (ftype (function (pp-stream t) unspecified) pp-line-breaks))
(declaim (ftype (function (pp-stream sequence-index) t) pp-misering-p))
(declaim (ftype (function (pp-operation) (integer 5 12)) pp-op-size))

(declaim (ftype (function (pp-stream simple-vector sequence-index)
    sequence-index )
  pp-pop-op ) )

(declaim (ftype (function (pp-stream sequence-index) unspecified)
  pp-prepare ) )

(declaim (ftype (function (pp-stream sequence-index) sequence-index)
  pp-process ) )

(declaim (ftype (function (pp-stream pp-operation sequence-index)
    (values simple-vector sequence-index) )
  pp-push-op ) )

(declaim (ftype (function (pp-stream) sequence-index) pp-section-column))

(declaim (ftype (function (pp-stream sequence-index t) unspecified)
  pp-send-buffer ) )

(declaim (ftype (function (character pp-stream) character)
  pp-write-char ) )

(declaim (ftype (function (string pp-stream sequence-index sequence-end)
    string )
  pp-write-string ) )

(declaim (ftype (function (string pp-stream sequence-index sequence-end)
    unspecified )
  pp-write-string-aux ) )

(declaim (ftype (function (stream t string t string function)
    null )
  pprint-logical-block-function ) )

(declaim (ftype (function (pp-stream) unspecified)  pprint-block-end))

(declaim (ftype (function (pp-stream string (or string null) string)
    unspecified )
  pprint-block-start ) )

(declaim (ftype (function (pp-stream) unspecified) pprint-end))

(declaim (ftype (function (pp-newline pp-stream) unspecified)
  pprint-newline-aux ) )

(declaim (ftype (function (t stream) unspecified) pprint-object))

(declaim (ftype (function ((and stream (not pp-steam))) pp-stream)
  pprint-start ) )


;;; pp-op structure.
;;;
;;; Description:
;;;  A structure pp-op is a internal representation of logical block and
;;;  conditional newline. For performance reason, pp-op is represents by
;;;  simple-vector instead of structure-object.
;;;
;;; There are seven types of pp-op:
;;;   1. :block     pprint-indent
;;;   2. :current   pprint-indent
;;;   3. :end       pprint-logical-block
;;;   4. :fill      pprint-newline
;;;   5. :linear    pprint-linear
;;;   6. :miser     pprint-miser
;;;   7. :start     pprint-logical-block
;;;
;;; :block, :current
;;;  +0 :block
;;;  +1 next
;;;  +2 prev
;;;  +3 parent-block
;;;  +4 column          -- (not used for :block)
;;;  +5 cindex
;;;  +6 n
;;;
;;; :end
;;;  +0 :end
;;;  +1 next
;;;  +2 prev
;;;  +3 parent-block
;;;  +4 start-ptr
;;;
;;; :fill, :linear, :miser
;;;  +0 :fill
;;;  +1 next
;;;  +2 prev
;;;  +3 parent-block
;;;  +4 start-col
;;;  +5 cindex
;;;  +6 end-col
;;;
;;; :start
;;;  +0 :start
;;;  +1 next
;;;  +2 prev
;;;  +3 parent-block
;;;  +4 start-col
;;;  +5 cindex
;;;  +6 end-col
;;;  +7 section-col     start column of the last section
;;;  +8 section-row     start row of the last section.
;;;  +9 indent
;;; +10 prefix
;;; +11 suffix


;;;; pp-op-size
;;;
;;; Description:
;;;  Returns size of specified type of pp-op. This function is used during
;;;  compilation time only and called by #. reader macro.
;
(defun pp-op-size (op)
    (declare (type pp-operation op))
    (declare (values (integer 5 12)))
  (ecase op
    ((:block)    7)
    ((:current)  7)
    ((:end)      5)
    ((:fill)     7)
    ((:linear)   7)
    ((:miser)    7)
    ((:start)   12) ) )


;;;; pp-op-* accessors
;;;
;;; Description:
;;;  Following form defines macro for accessing slots of pp-op.
;;;  Since to reduce typing, these macros reference variable "heap".
(macrolet (
  (define-op-accessor (name offset)
    (let ((fn (intern (format nil "PP-OP-~A" name))))
      `(progn
         (defmacro ,fn (qindex)
           `(svref heap (+ ,qindex ,',offset)) ) ) ) )
  )
  ;;
  (define-op-accessor type          0)
  (define-op-accessor next-ptr      1)
  (define-op-accessor prev-ptr      2)
  (define-op-accessor parent-ptr    3)
  (define-op-accessor start-col     4)
  (define-op-accessor cindex        5)
  (define-op-accessor end-col       6)
  (define-op-accessor section-col   7)
  (define-op-accessor section-row   8)
  (define-op-accessor indent        9)
  (define-op-accessor prefix       10)
  (define-op-accessor suffix       11)

  (define-op-accessor column        4)
  (define-op-accessor n             6)

  (define-op-accessor start-ptr     4)
 ) ; macrolet


;;;; pp-chunk
(define-pool pp-chunk ()
  :constructor #'(lambda () (make-array 100))
  :finalizer   #'(lambda (chunk) (fill chunk nil)) )


;;;; pp-stream pool
(define-pool pp-stream ()
  :constructor
    #'(lambda ()
        (make-instance 'pp-stream) )

  :initializer
    #'(lambda (stream &aux (st (ref instance storage stream)))
        (setf (ref pp-stream flags      st) STREAM-FLAG-OUTPUT)

        (setf (ref pp-stream cindex     st) 0)
        (setf (ref pp-stream suffix-len st) 0)

        (setf (ref pp-stream block-ptr  st) 0)

        (setf (ref pp-stream nlines     st) 0)

        (setf (ref pp-stream head-ptr   st) 0)
        (setf (ref pp-stream scan-ptr   st) 0)
        (setf (ref pp-stream tail-ptr   st) 0) )

  :finalizer
    #'(lambda (stream &aux (st (ref instance storage stream)))
        (when (ref pp-stream cbuffer st)
          (free-pooled-format-string (ref pp-stream cbuffer st))
          (setf (ref pp-stream cbuffer st) nil) )

        (when (ref pp-stream heap st)
          (free-pooled-pp-chunk (ref pp-stream heap st))
          (setf (ref pp-stream heap st) #()) ) ) )


;;;; *standard-pprint-dispatch*
;;;
;;; Updated by:
;;;   set-pprint-dispatch
;;;
;;; Referenced by:
;;;   copy-pprint-dispatch
;;;   pprint-dispatch
;;;
;;; Description:
;;;  The standard pprint dispatch table. This table maps type specifier to
;;;  pritty printer for the type.
;
(defvar *standard-pprint-dispatch* (make-pp-dispatch-table))


;;;; stream-line-column
;
(defmethod ext:stream-line-column ((stream pp-stream))
    (declare (values ext:sequence-index))
  (ref pp-stream column (ref instance storage stream)) )


;;;; stream-write-char
;
(defmethod ext:stream-write-char ((stream pp-stream) char)
    (declare (type character char))
    (declare (values character))
  (pp-write-char char stream) )


;;;; stream-write-string
;
(defmethod ext:stream-write-string
      ((stream pp-stream) string &optional (start 0) end)
    (declare (type string string))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-end end))
    (declare (values string))
  (pp-write-string string stream start end) )
