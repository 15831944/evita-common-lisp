;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 15 Arrays
;;; arch/generic/lisp/runtime/gen-r15-array.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r15-array.lisp#16 $
;
(in-package :si)

;;;; array-data
(defun array-data (array)
    (declare (values data-vector sequence-index))
    (declare (type array array))
  (labels (
    ;; follow
    (follow (runner offset)
        (declare (values data-vector sequence-index))
        (declare (type array runner))
        (declare (type sequence-index offset))
      (etypecase runner
        (data-vector
          (values runner offset) )
        (vector
          (follow (ref vector-object displaced-to runner)
                  (+ (ref vector-object offset runner) offset) ) )
        (array
          (follow (ref array-object displaced-to runner)
                  (+ (ref array-object offset runner) offset) ) )) )
    )
    ;; array-data
    (multiple-value-bind (data offset)
        (etypecase array
          (data-vector
            (values array 0) )
          (vector
            (multiple-value-bind (data offset)
                (follow (ref vector-object displaced-to array)
                        (ref vector-object offset array) )
              (values data offset) ) )
          (array
            (multiple-value-bind (data offset)
                (follow (ref array-object displaced-to array)
                        (ref array-object offset array) )
              (values data offset) ) ))
      (values data offset) ) ) )


;;;; internal-adjust-vector
(defun internal-adjust-vector
    (array length displaced-p displaced-to offset fill-pointer-p fill-pointer)

  (setf (ref vector-object displaced-to array) displaced-to)
  (setf (ref vector-object offset       array) offset)
  (setf (ref vector-object total-size   array) length)
  (setf (ref vector-object fill-pointer array) fill-pointer)

  (setf (ref vector-object flags array)
    (cond
      ((and fill-pointer-p displaced-p) 3)
      (displaced-p 1)
      (fill-pointer-p 2)
      (t 0) ))

  array )


;;;; internal-make-array
(defun internal-make-array
        (dimensions displaced-p displaced-to offset  adjustable)
  (labels (
    (compute-classd ()
      (multiple-value-bind (gclassd sclassd) (compute-classd-aux)
        (if (or adjustable displaced-p) gclassd sclassd) ) )

    ;; compute-classd-aux
    (compute-classd-aux ()
      (typecase displaced-to
        ((or simple-bit-vector
             simple-bit-array-object
             bit-vector-object
             bit-array-object )
          (values #.(class-description 'bit-array-object)
                  #.(class-description 'simple-bit-array-object) ) )
        (otherwise
          (values #.(class-description 'general-array-object)
                  #.(class-description 'simple-general-array-object) ) )) )
    )
    ;;
  (let* ((rank (length dimensions))
         (array (.allocate-vector (compute-classd) rank)) )
       (declare (type array-object array))
    (setf (ref array-object displaced-to array) displaced-to)
    (setf (ref array-object flags        array) (if displaced-p 1 0))
    (setf (ref array-object offset       array) offset)

    (let ((total-size 1))
      (dotimes (i rank)
        (let ((dimension (pop dimensions)))
          (setf (!elt 'sequence-index array i) dimension)
          (setq total-size (* total-size dimension)) ) )
      (setf (ref array-object total-size   array) total-size) )

    array ) ) )


;;;; internal-make-vector
(defun internal-make-vector
        (length displaced-p displaced-to offset fill-pointer element-type)
  (let ((attr.fill-pointer 2) (attr.displaced 1))
  (multiple-value-bind (flags fill-pointer)
      (cond
        ((eq fill-pointer t)
          (values attr.fill-pointer length) )
        ((eq fill-pointer nil)
          (values 0 length) )
        (t
          (unless (<= 0 fill-pointer length)
            (runtime-type-error
                `(integer 0 ,length)
                fill-pointer
                "Fill-pointer ~S must be less or equal to ~S."
                fill-pointer
                length ))
          (values attr.fill-pointer fill-pointer) ))

    (when displaced-p
      (setq flags (logior flags attr.displaced)) )

    (let ((vector
            (.allocate-record
                (case element-type
                  ((character)
                    #.(class-description 'string-object) )
                  ((bit)
                    #.(class-description 'bit-vector-object) )
                  (otherwise
                    #.(class-description 'general-vector-object) ))) ))
        (setf (ref vector-object total-size   vector) length)
        (setf (ref vector-object flags        vector) flags)
        (setf (ref vector-object displaced-to vector) displaced-to)
        (setf (ref vector-object offset       vector) offset)
        (setf (ref vector-object fill-pointer vector) fill-pointer)
    vector ) ) ) )


;;;; make-data-vector
;;; Note: element-type must be result of upgraded-array-element-type.
(defun make-data-vector (length element-type)
    (declare (values data-vector))
    (declare (type sequence-index length))
    (declare (type type-specifier element-type))
  (case element-type
    ((t) (make-simple-vector length)) ; short cut
    (otherwise
      (loop
        for class in (ref built-in-class direct-subclasses
                          (ref instance storage (find-class 'data-vector)) )
        for classd = (ref built-in-class instance-description
                          (ref instance storage class) )
        for elty = (ref classd element-type classd)
          when (equal elty element-type)
            return
                (case (ref classd format classd)
                  ((7 8)
                    (.allocate-binvec classd length) )
                  (otherwise
                    (.allocate-vector classd length) ))
        finally
          (error "Can't create vector for element-type ~S."
                    element-type )) )) )


;;;; string-data
;;;; vector-data
;;; Returns simple-string, start and end of string-data.
(macrolet (
  (define (name type simple general)
   `(defun ,name (vector &optional (start 0) end)
        (declare (values ,simple sequence-index sequence-index))
        (declare (type ,type vector))
        (declare (type sequence-index start))
        (declare (type sequence-end   end))
      (labels (
        ;; follow
        (follow (runner offset)
            (declare (values ,simple sequence-index))
            (declare (type array runner))
            (declare (type sequence-index offset))
          (typecase runner
            (,simple
              (values runner offset) )
            (,general
              (follow (ref ,general displaced-to runner)
                      (+ (ref ,general offset runner) offset) ) )
            (array-object
              (follow (ref array-object displaced-to runner)
                      (+ (ref array-object offset runner) offset) ) )
            (otherwise (error "Broken vector ~S." runner)) ) )
        )
        ;; body
        (multiple-value-bind (data offset length)
            (typecase vector
              (,simple
                (values vector 0 (ref ,simple length vector)) )
              (,general
                (multiple-value-bind (data offset)
                    (follow (ref ,general displaced-to vector)
                            (ref ,general offset vector) )
                  (values data offset (ref ,general fill-pointer vector))) )
              (otherwise
                (error 'type-error :datum vector :expected-type ',type) ))

          (let ((end (or end length)))
            (unless (<= 0 start length)
              (sequence-index-error vector start) )

            (unless (<= start end length)
              (bounding-index-error vector start end) )

            (values data (+ start offset) (+ end offset)) ) ) ) ) )
    )
    ;;
    (define ext:string-data string simple-string string-object)
    (define ext:vector-data vector data-vector   vector-object) )


;;;; cl:array-dimension
(defun cl:array-dimension (array axis)
  (typecase array
    (vector
      (if (eql axis 0)
          (length array)
        (error 'type-error :datum axis :expected-type '(eql 0)) ) )
    (array-object
      (if (<= 0 axis (1- (array-rank array)))
          (!elt 'sequence-index array axis)
        (error 'type-error :datum axis
               :expected-type `(integer 0 ,(1- (array-rank array))) )) )
    (otherwise (error 'type-error :datum array :expected-type 'array)) ) )


;;;; cl:array-dimensions
(defun cl:array-dimensions (array)
  (typecase array
    (vector (list (length array)))
    (array-object
      (with-collector (collect)
        (dotimes (axis (array-rank array))
          (collect (!elt 'sequence-index array axis)) ) ) )
    (otherwise (error 'type-error :datum array :expected-type 'array)) ) )


;;;; cl:array-displacement
(defun cl:array-displacement (array)
  (etypecase array
    (simple-array (values nil 0))
    (array
      (if (logbitp 0 (ref array-object flags array))
          (values (ref array-object displaced-to array)
                  (ref array-object offset array) )
        (values nil 0) ) )) )


;;;; cl:array-element-type
(defun cl:array-element-type (array)
  (let ((data (array-data array)))
    (let ((classd (ref data-vector classd data)))
      (ref classd element-type classd) ) ) )


;;;; cl:array-has-fill-pointer-p
(defun cl:array-has-fill-pointer-p (array)
  (etypecase array
    (simple-array nil)
    (vector (logbitp 1 (ref vector-object flags array)))
    (array  nil) ) )


;;;; cl:array-rank
(defun cl:array-rank (array)
  (etypecase array
    (vector 1)
    (array  (ref array-object rank array)) ) )


;;;; 15.2.19 array-total-size
(defun cl:array-total-size (array)
    (declare (values sequence-index))
  (etypecase array
    (data-vector (ref data-vector length array))
    (vector      (ref vector-object total-size  array))
    (array       (ref array-object  total-size  array)) ) )


;;;; 15.2.21 fill-pointer
(defun cl:fill-pointer (vector)
  (if (array-has-fill-pointer-p vector)
      (ref vector-object fill-pointer vector)
    (error 'not-have-fill-pointer :datum vector) ) )


(defun (setf cl:fill-pointer) (fill-pointer vector)
  (unless (array-has-fill-pointer-p vector)
    (error 'not-have-fill-pointer :datum vector) )

  (unless (<= 0 fill-pointer (ref vector-object total-size vector))
    (error 'type-error
           :expected-type `(integer 0 ,(ref vector-object total-size vector))
           :datum fill-pointer ))

  (setf (ref vector-object fill-pointer vector) fill-pointer) )


;;;; cl:row-major-aref
(defun cl:row-major-aref (array index)
    (declare (values t))
    (declare (type array array))
    (declare (type sequence-index index))
  (labels (
    (vref (vector index)
      (unless (<= 0 index (1- (ref data-vector length vector)))
        (error 'sequence-index-error
            :expected-type `(integer 0 ,(1- (ref data-vector length vector)))
            :datum index
            :sequence vector ))

      (typecase vector
        (simple-vector           (!elt 't vector index))
        (simple-string           (schar  vector index))
        (simple-bit-vector       (sbit/2 vector index))

        (single-float-vector     (!elt 'float32  vector index))
        (double-float-vector     (!elt 'float64  vector index))

        (signed-byte-8-vector    (!elt 'int8    vector index))
        (signed-byte-16-vector   (!elt 'int16   vector index))
        (signed-byte-32-vector   (!elt 'int32   vector index))

        (unsigned-byte-8-vector  (!elt 'uint8   vector index))
        (unsigned-byte-16-vector (!elt 'uint16  vector index))
        (unsigned-byte-32-vector (!elt 'uint32  vector index))

        #+64bit (signed-byte-64-vector   (!elt 'int64  vector index))
        #+64bit (unsigned-byte-64-vector (!elt 'uint64 vector index))

        (otherwise (error "NYI: row-major-aref")) ) )
    )
    ;;
    (multiple-value-bind (vector offset) (array-data array)
      (vref vector (+ index offset)) ) ) )


;;;; (setf cl:row-major-aref)
(defun (setf cl:row-major-aref) (datum array index)
    (declare (values t))
    (declare (type array array))
    (declare (type sequence-index index))
  (labels (
    ((setf vref) (datum vector index)
      (unless (<= 0 index (1- (ref data-vector length vector)))
        (error 'sequence-index-error
            :expected-type `(integer 0 ,(1- (ref data-vector length vector)))
            :datum index
            :sequence vector ))

      (typecase vector
        (simple-vector       (setf (!elt 't vector index) datum))
        (simple-string       (setf (schar  vector index) datum))
        (simple-bit-vector   (setf (sbit/2 vector index) datum))

        (single-float-vector (setf (!elt 'float32 vector index) datum))
        (double-float-vector (setf (!elt 'float64 vector index) datum))

        (unsigned-byte-8-vector  (setf (!elt 'uint8   vector index) datum))
        (unsigned-byte-16-vector (setf (!elt 'uint16  vector index) datum))
        (unsigned-byte-32-vector (setf (!elt 'uint32  vector index) datum))

        (signed-byte-8-vector    (setf (!elt 'int8    vector index) datum))
        (signed-byte-16-vector   (setf (!elt 'int16   vector index) datum))
        (signed-byte-32-vector   (setf (!elt 'int32   vector index) datum))

        #+64bit (signed-byte-64-vector
                    (setf (!elt 'int64   vector index) datum) )
        #+64bit (unsigned-byte-64-vector
                    (setf (!elt 'uint64  vector index) datum) )

        (otherwise (error "NYI: setf row-major-aref")) ) )
    )
    ;;
    (multiple-value-bind (vector offset) (array-data array)
      (setf (vref vector (+ index offset)) datum) ) ) )


;;;; 15.2.23 upgraded-array-element-type
;;; Note: direct-subclasses of simple-array must be orderd by subtypep or
;;; bit < unsigned < signed.
;;;
;;; Note: character, single-float, double-float, (complex single-float),
;;; and (complex-double-float) are disjoint types.
;;;
;;; Note: simple-vector msut be the last.
(defun cl:upgraded-array-element-type (typespec &optional env)
    (declare (type type-specifier typespec))
  (cond
    ((eq typespec 't) 't)
    ((subtypep typespec 'nil env)
      ;; Note: (array t) can't containt a object of type nil, however
      ;; there is no such object.
      'nil )
    (t
      (loop
        for class in (ref built-in-class direct-subclasses
                          (ref instance storage (find-class 'data-vector)) )
        for classd = (ref built-in-class instance-description
                          (ref instance storage class) )
        for elty = (ref classd element-type classd)
          when (subtypep typespec elty)
            return elty
        finally (return 't) ))) )


;;;; make-simple-vector
;;;
;;; Description:
;;;  Making simple-vector as fast as we can.
;
(defun make-simple-vector (length &optional (initval nil initval-p))
    (declare (type array-total-size length))
    (declare (type t initval))
    (declare (values simple-vector))
  (let ((vector
          (.allocate-vector #.(class-description 'simple-vector) length) ))
    (when initval-p
      (dotimes (i length)
        (setf (svref vector i) initval) ))
    vector ) )
