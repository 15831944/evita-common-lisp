;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 15 Array
;;; lisp/runtime/r15-array.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r15-array.lisp#6 $
;;;
;;; Description:
;;;  This fils contains following functions:
;;;     adjust-array                    15.2.8
;;;     adjustable-array-p              15.2.9
;;;     arrayp                          15.2.20 boot
;;;     array-dimension                 15.2.11
;;;     array-dimensions                15.2.12
;;;     array-displacement              15.2.15
;;;     array-element-type              15.2.13
;;;     array-has-fill-pointer-p        15.2.14
;;;     array-in-bounds-p               15.2.16
;;;     array-rank                      15.2.17
;;;     array-row-major-index           15.2.18
;;;     array-total-size                15.2.19
;;;     aref                            15.2.10
;;;     arrayp                          15.2.20 instrinsic
;;;     bit                             15.2.33
;;;     bit-and                         15.2.34
;;;     bit-andc1                       15.2.34
;;;     bit-andc2                       15.2.35
;;;     bit-eqv                         15.2.35
;;;     bit-ior                         15.2.35
;;;     bit-nand                        15.2.35
;;;     bit-nor                         15.2.35
;;;     bit-not                         15.2.35
;;;     bit-orc1                        15.2.35
;;;     bit-orc2                        15.2.35
;;;     bit-xor                         15.2.35
;;;     bit-vector-p                    15.2.35 boot
;;;     fill-pointer                    15.2.21
;;;     make-array                      15.2.7
;;;     row-major-aref                  15.2.22
;;;     simple-bit-vector-p             15.2.36 boot
;;;     simple-vector-p                 15.2.27 boot
;;;     svref                           15.2.28
;;;     vector                          15.2.29
;;;     vector-pop                      15.2.30
;;;     vector-push                     15.2.31
;;;     vector-push-extend              15.2.31
;;;     vectorp                         15.2.32 intrinsic
;;;     sbit                            15.2.33
;;;     upgraded-array-element-type     15.2.23
;
(in-package :si)

;;;; array-index-error
(defun array-index-error (array index)
  (error 'array-index-error
         :expected-type `(integer 0 ,(1- (array-total-size array)))
         :array         array
         :datum         index ) )

;;;; error-initial-element-and-contents
(defun error-initial-element-and-contents ()
  (error "Can't use ~S and ~S same time."
    :initial-element
    :initial-contents ) )

;;;; error-displaced-index-offset-without-to
(defun error-displaced-index-offset-without-to ()
  (error "Can't use ~S without ~S."
    :displaced-index-offset
    :displaced-to ) )

;;;; error-displaced-with-initializer
(defun error-displaced-with-initializer (key)
  (error "Can't use ~S with ~S."
    key
    :displaced-to ))


;;;; ensure-dimensions-argument
;;; => total-size, rank
;;;
;;; Called by:
;;;   adjust-array
;;;   make-array
;
(defun ensure-dimensions-argument (dimensions)
    (declare (values array-total-size array-rank))
  (etypecase dimensions
    (integer
      (let ((length dimensions))
        (cond
          ((>= length array-total-size-limit)
            (runtime-type-error
                '(integer 0 #.(1- array-total-size-limit))
                length
                "Length of vector ~D exceeds array-total-size-limit ~D."
                length
                array-total-size-limit ) )
          ((< length 0)
            (runtime-type-error
                '(integer 0 #.(1- array-total-size-limit))
                length
                "Length of vector ~D must be a non-negative integer."
                length ) )
          (t (values length 1) )) ) )
    (list
      (loop
        with total-size of-type integer = 1
        for rank of-type sequence-index = 0 then (1+ rank)
        for dimension in dimensions do
          (unless (integerp dimension)
            (runtime-type-error
                '(integer 0 #.(1- array-dimension-limit))
                dimension
                "Invalid dimension: ~S"
                dimension ) )

          (unless (<= 0 dimension (1- array-dimension-limit))
            (runtime-type-error
                '(integer 0 #.(1- array-dimension-limit))
                dimension
                "Dimension ~D exceeds array-dimenion-limit ~D."
                dimension
                array-dimension-limit ))

          (setq total-size (* total-size dimension))

          (when (>= total-size array-total-size-limit)
            (runtime-type-error
                '(integer 0 #.(1- array-total-size-limit))
                dimensions
                "Dimensions ~S exceeds array-total-size-limit ~D."
                dimensions
                array-total-size-limit ))
        finally
          (when (>= rank array-rank-limit)
            (runtime-type-error
                '(integer 0 #.(1- array-total-size-limit))
                rank
                "Array rank ~D must be less than ~D."
                rank
                array-rank-limit ))
          (return (values total-size rank)) ) )) )


;;;; init-array-contents
;;;
;;; Called by:
;;;     adjust-array
;;;     make-array
;;;
;;; Description:
;;;  Initialize data-vector of array with contents.
;
(defun init-array-contents (data contents dimensions axis dstart)
    (declare (type (simple-array * (*)) data))
    (declare (type sequence contents))
    (declare (type list dimensions))
    (declare (type array-rank axis))
    (declare (type sequence-index dstart))
    (declare (values unspecified))
  (cond
    ((null dimensions)
      (unless (zerop dstart)
        (error "Bad initial-contents at axis ~D: ~S" axis contents) )
      (setf (row-major-aref data dstart) contents) )

    ((null (rest dimensions))
      (let ((length (first dimensions)))
        (unless (= (length contents) length)
          (error "Bad initial-contents at axis ~D, expect ~D element~:P: ~S"
              axis
              length
              contents ))
        (replace data contents :start1 dstart) ) )
    (t
      (let ((dimension (pop dimensions))
            (width     (reduce #'* dimensions)) )
        (unless (= (length contents) dimension)
          (error "Bad initial-contents at axis ~D, expect ~D element~:P: ~S"
              axis
              dimension
              contents ))

        (incf axis)

        (etypecase contents
          (list
            (loop
              for content in contents
              for dindex = dstart then (+ dindex width) do
                (init-array-contents
                    data content dimensions axis dindex) ) )
          (vector
            (loop
              for content across contents
              for dindex = dstart then (+ dindex width) do
                (init-array-contents
                    data content dimensions axis dindex )) )) ) )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;

;;;; 15.2.8 adjust-array
;;;
;;; 1. Not adjustable => make new-array
;;; 2. New rank is greater than old rank => make new-array
;;; 3. Displacement
;;;     3-1 become displaced-to
;;;     3-2 become not displaced-to
;;;         3-2-1 was not displaced-to
;;;         3-2-1 was displaced-to  => copy old contents
;;; 4. Dimensions
;;;    4-1 rank != 1        => array or simple-array
;;;    4-2 no fill-pointer  => vector
;;;    4-3 fill-pointer     => vector
;
;;;; adjust-array
;
(defun cl:adjust-array
        (array new-dimensions
            &key
              (element-type           (array-element-type array))
              (initial-element        nil initial-element-p)
              (initial-contents       nil initial-contents-p)
              (fill-pointer           nil fill-pointer-p)
              (displaced-to           nil displaced-p)
              ((:displaced-index-offset offset) 0 offset-p) )
    (declare (type array array))
    (declare (type (or sequence-index list) new-dimensions))
    (declare (values array))
  (multiple-value-bind (total-size rank)
      (ensure-dimensions-argument new-dimensions)
      (declare (type array-total-size total-size))
      (declare (type array-rank rank))
  (labels (
    ;; adjust-1
    ;;  Adjusts vector or rank one array.
    (adjust-1 ()
        (declare (values vector))

      (setq displaced-to (adjust-contents))

      (cond
        ((not (array-has-fill-pointer-p array))
          (when fill-pointer (error-fill-pointer))
          (setq fill-pointer-p nil)
          (setq fill-pointer total-size) )
        ((null fill-pointer)
          (setq fill-pointer-p t)
          (setq fill-pointer (fill-pointer array)) )
        ((eq fill-pointer t)
          (setq fill-pointer total-size) )
        ((not (and (integerp fill-pointer) (<= 0 fill-pointer total-size)))
          (error 'type-error
                :expected-type `(integer 0 ,total-size)
                :datum         fill-pointer ) ))

      (internal-adjust-vector array total-size
        displaced-p displaced-to offset
        fill-pointer-p fill-pointer ) )

    ;; adjust-n
    ;;  Adjusts array rank other than one.
    (adjust-n ()
        (declare (values array))

      (when fill-pointer (error-fill-pointer))

      (setq displaced-to (adjust-contents))

      (internal-adjust-array array new-dimensions
        displaced-p displaced-to offset ) )

    ;; adjust-contents
    (adjust-contents ()
        (declare (values array))
      (cond
        (displaced-p
          (cond
            (initial-element-p
              (error-displaced-with-initializer :initial-element) )
            (initial-contents-p
              (error-displaced-with-initializer :initial-contents) ))

          (unless (equal (array-element-type array)
                         (array-element-type displaced-to) )
            (error "Can't displace an array of ~S to array of ~S."
                (array-element-type array)
                (array-element-type displaced-to) ) )

          (unless (>= (- (array-total-size displaced-to)
                         offset )
                      total-size )
            (error "Displaced array must contain at least ~D elements."
                   total-size ))

          displaced-to )

        (offset-p
          (ERROR-DISPLACED-INDEX-OFFSET-WITHOUT-TO) )

        (t
          (let ((data (make-data-vector total-size element-type)))
            (cond
              ((and initial-element-p initial-contents-p)
                (error-initial-element-and-contents) )

              (initial-element-p
                (fill data initial-element) )

              (initial-contents-p
                (case (array-rank array)
                  ((0))
                  ((1)
                    (unless (= (length initial-contents) total-size)
                      (error "Bad initial-contents: ~S"
                         initial-contents ))
                    (replace data initial-contents) )
                  (otherwise
                    (init-array-contents
                        data initial-contents new-dimensions 0 0 ) )) ))
            (copy-elements data
                           new-dimensions
                           array
                           (array-dimensions array) ) ) )) )

    ;; compute-initargs
    ;;  Computes initargs for new array
    (compute-initargs ()
      (let ((initargs (list :element-type element-type)))

        (unless displaced-p
          (when offset-p
            (ERROR-DISPLACED-INDEX-OFFSET-WITHOUT-TO) )
          (multiple-value-setq (displaced-to offset)
              (array-displacement array) )
          (setq displaced-p displaced-to) )

        (cond
          ((array-has-fill-pointer-p array)
            (when (null fill-pointer)
              (setq fill-pointer (fill-pointer array)) ) )
          (fill-pointer
            (error-fill-pointer) ))

        (cond
          ((and initial-element-p initial-contents-p)
            (error-initial-element-and-contents) )
          (initial-element-p
            (setq initargs
                (list* :initial-element initial-element initargs)) )
          (initial-contents-p
            (setq initargs
                (list* :initial-contents initial-contents initargs)) ))

        (when displaced-p
          (setq initargs
            (list* :displaced-to displaced-to
                   :offset offset
                   initargs )))

        (when fill-pointer
          (setq initargs
            (list* :fill-pointer fill-pointer initargs) ))

        initargs ) )

    ;; copy-elements
    (copy-elements (dst ddims src sdims)
        (declare (values array))
      (unless (listp ddims) (setq ddims (list ddims)))

      (let ((dwidth (reduce #'* (rest ddims)))
            (swidth (reduce #'* (rest sdims)))
            (dsize  (reduce #'* ddims))
            (ssize  (reduce #'* sdims))
            (dindex 0)
            (sindex 0) )
        (loop
          (when (>= dindex dsize) (return))
          (when (>= sindex ssize) (return))

          (dotimes (i (min dwidth swidth))
            (setf (row-major-aref dst (+ i dindex))
                  (row-major-aref src (+ i sindex)) ) )

          (incf dindex dwidth)
          (incf sindex swidth) ) ) 
      dst )

    ;; error-fill-pointer
    (error-fill-pointer ()
      (error "Can't use non-nil ~S for array which has no fill-pointer: ~S"
        :fill-pointer
        array ) )

    ;; make-new
    ;;  Make new array with specified arguments with defaults.
    (make-new ()
        (declare (values array))
      (copy-elements (apply #'make-array new-dimensions (compute-initargs))
                     new-dimensions
                     array
                     (array-dimensions array) ) )
    )
    ;;
    (setq element-type (upgraded-array-element-type element-type))

    (unless (equal (array-element-type array) element-type)
      (error "Can't change element-type ~S to ~S."
          (array-element-type array)
          element-type ))

    (cond
      ((not (eql (array-rank array) rank))
        (runtime-type-error
            `(integer ,(array-rank array) ,(array-rank array))
            rank
            "Number of dimensions ~S not equal to rank of array ~S"
            new-dimensions
            array ) )
      ((not (adjustable-array-p array))
        (make-new) )
      ((eql rank 1) (adjust-1))
      (t (adjust-n)) ) ) ) )


;;;; 15.2.9 adjustable-array-p
(defun cl:adjustable-array-p (array)
  (etypecase array
    (simple-array nil)
    (array  t) ) )


;;;; 15.2.10 aref
(defun cl:aref (array &rest subscripts)
    (declare (dynamic-extent subscripts))
  (let ((index
          (etypecase array
            (vector (first subscripts))
            (array  (apply #'array-row-major-index array subscripts)) ) ))
    (row-major-aref array index) ) )


;;;; (setf aref)
(defun (setf cl:aref) (new-elt array &rest subscripts)
    (declare (dynamic-extent subscripts))
  (let ((index
          (etypecase array
            (vector (first subscripts))
            (array  (apply #'array-row-major-index array subscripts)) ) ))
    (setf (row-major-aref array index) new-elt) ) )


;;;; 15.2.14 array-dimensions
;
(defun cl:array-dimensions (array)
    (declare (type array array))
    (declare (values list))
  (loop
    for axis-number from 0 below (array-rank array)
      collect (array-dimension array axis-number) ) )


;;;; 15.2.16 array-in-bounds-p
(defun cl:array-in-bounds-p (array &rest subscripts)
    (declare (dynamic-extent subscripts))

  (unless (eql (array-rank array) (length subscripts))
    (error 'invalid-array-subscripts :array array :subscripts subscripts) )

  (let ((axis-number 0))
    (dolist (subscript subscripts t)
      (unless (and (>= subscript 0)
                   (<  subscript (array-dimension array axis-number)) )
        (return nil) )
      (incf axis-number) ) ) )


;;;; 15.2.18 array-row-major-index
;;; BUGBUG: NYI: Check subscript bound.
(defun cl:array-row-major-index (array &rest subscripts)
    (declare (dynamic-extent subscripts))

  (unless (eql (array-rank array) (length subscripts))
    (error 'invalid-array-subscripts :array array :subscripts subscripts) )

  (let ((index (if (null subscripts) 0 (pop subscripts)))
        (axis-number 1) )
    (dolist (subscript subscripts)
      (setq index (+ subscript (* index (array-dimension array axis-number))))
      (incf axis-number) )
    index ) )


;;;; 15.2.33 bit
;;;
;
(defun cl:bit (array &rest subscripts)
    (declare (type (array bit) array))
    (declare (values bit))
    (declare (dynamic-extent subscripts))
  (check-type array (array bit))
  (apply #'aref array subscripts) )


;;;; (setf bit)
;
(defun (setf cl:bit) (new-element array &rest subscripts)
    (declare (type (array bit) array))
    (declare (type bit new-element))
    (declare (values bit))
    (declare (dynamic-extent subscripts))
  (check-type array (array bit))
  (setf (apply #'aref array subscripts) new-element) )


;;;; 15.2.7 make-array
;
(defun cl:make-array (dimensions
            &key
              adjustable
              (element-type t)
              (initial-element nil initial-element-p)
              (initial-contents nil initial-contents-p)
              fill-pointer
              (displaced-to nil displaced-p)
              ((:displaced-index-offset offset) 0 displaced-index-offset-p) )
    (declare (values array))
  (labels (
    ;; check-displacement
    (check-displacement (length)
      (cond
        (initial-element-p
          (error-displaced-with-initializer :initial-element) )
        (initial-contents-p
          (error-displaced-with-initializer :initial-contents) ))

      (unless (<= 0 offset (array-total-size displaced-to))
        (runtime-type-error
          `(integer 0 ,(array-total-size displaced-to))
          offset
          "Displaced-index-offset ~D must be less or equal to ~D."
          offset
          (array-total-size displaced-to) ))

      (unless (<= (+ offset length) (1+ (array-total-size displaced-to)))
        (runtime-type-error
          `(integer 0 ,(- (array-total-size displaced-to) length 1))
          offset
          "Length ~D with offset ~D is too larget for displacment ~S."
          length
          offset
          displaced-to ))

      (unless (equal (array-element-type displaced-to) element-type)
        (runtime-type-error
          `(array ,element-type)
          displaced-to
          "Can't displace an array of type ~S to another array of type ~S."
          element-type
          (array-element-type displaced-to) )) )

    ;; init-1
    ;;  Initialize data-vector with initial-element or initial-contents
    ;;  keyword parametes.
    (init-1 (data)
        (declare (type (simple-array * (*)) data))
        (declare (values (simple-array * (*))))
      (cond
        ((and initial-element-p initial-contents-p)
          (error-initial-element-and-contents) )
        (initial-element-p
          (fill data initial-element) )
        (initial-contents-p
          (unless (= (length initial-contents) (length data))
            (error "Bad initial contents, it must have ~D elements: ~S"
                (length data)
                initial-contents ))
          (replace data initial-contents) )
        (t data) ) )

    ;; init-n
    (init-n (data dimensions)
        (declare (type (simple-array * (*)) data))
        (declare (type sequence-index start1 end1))
        (declare (values unspecified))
      (cond
        ((and initial-element-p initial-contents-p)
          (error-initial-element-and-contents) )
        (initial-element-p
          (fill data initial-element) )
        (initial-contents-p
          (init-array-contents data initial-contents dimensions 0 0) )) )

    ;; make-1
    (make-1 (length)
      (unless (<= 0 length (1- array-total-size-limit))
        (if (minusp length)
            (runtime-type-error
                '(integer 0 #.(1- array-total-size-limit))
                length
                "Length ~D of vector must be non-negative integer."
                length )
          (runtime-type-error
              '(integer 0 #.(1- array-total-size-limit))
              length
              "Length ~D of vector must be less than ~D."
              array-total-size-limit )))

      (if (or fill-pointer displaced-p adjustable)
          (make-1-adjustable length)
        (init-1 (make-data-vector length element-type)) ) )

    ;; make-1-adjustable
    (make-1-adjustable (length)
        (declare (type sequence-index index))
        (declare (values vector))
      (cond
        (displaced-to ; We allow :displaced-to nil.
          (check-displacement length) )
        (displaced-index-offset-p
          (ERROR-DISPLACED-INDEX-OFFSET-WITHOUT-TO) )
        (t
          (setq displaced-to (make-data-vector length element-type))
          (init-1 displaced-to) ))

      (internal-make-vector
        length displaced-p displaced-to offset fill-pointer element-type ) )

    ;; make-n
    (make-n (total-size)
        (declare (values array))
      (when fill-pointer
        (error "Array can't have fill-pointer.") )

      (cond
        (displaced-to   ; We allow :displaced-to nil.
          (check-displacement total-size) )
        (displaced-index-offset-p
          (error-displaced-index-offset-without-to) )
        (t
          (setq displaced-to (make-data-vector total-size element-type))
          (init-n displaced-to dimensions) ))

      (internal-make-array
        dimensions displaced-p displaced-to offset adjustable ) )
    )
    ;;
    (setq element-type (upgraded-array-element-type element-type))

    (when (null element-type)
      (error "Can't create array of element-type ~S." element-type) )

    (multiple-value-bind (total-size rank)
        (ensure-dimensions-argument dimensions)
      (if (eql rank 1)
          (make-1 total-size)
        (make-n total-size) ) ) ) )


;;;; 15.2.33 sbit
(defun cl:sbit (array &rest subscripts)
    (declare (type (array bit) array))
    (declare (values bit))
    (declare (dynamic-extent subscripts))
  (check-type array (simple-array bit))
  (apply #'aref array subscripts) )


;;;; (setf sbit)
(defun (setf cl:sbit) (new-element array &rest subscripts)
    (declare (type (array bit) array))
    (declare (type bit new-element))
    (declare (values bit))
    (declare (dynamic-extent subscripts))
  (check-type array (simple-array bit))
  (setf (apply #'aref array subscripts) new-element) )


;;;; 15.2.30 vector-pop
(defun cl:vector-pop (vector)
  (let ((fill-pointer (1- (fill-pointer vector))))
    (when (< fill-pointer 0)
      (error 'fill-pointer-is-zero :vector vector) )
    (prog1
        (row-major-aref vector fill-pointer)
      (setf (fill-pointer vector) fill-pointer) ) ) )


;;;; 15.2.31 vector-push
(defun cl:vector-push (new-elt vector)
    (declare (values (or sequence-index null)))
  (let ((fill-pointer (fill-pointer vector))
        (length       (array-total-size vector)) )
    (if (>= fill-pointer length)
        nil
      (prog1
          fill-pointer
        (setf (row-major-aref vector fill-pointer) new-elt)
        (setf (fill-pointer vector) (1+ fill-pointer) ))) ) )


;;;; 15.2.31 vector-push-extend
;
(defun cl:vector-push-extend (new-elt vector &optional
                              (extension 1 extension-p) )
    (declare (values sequence-index))
  (labels (
    ;; extend
    (extend ()
      (let ((length (array-total-size vector)))
        (unless extension-p
          (setq extension (max (ceiling length 10) 1)) )
        (adjust-array vector (+ length extension)) ) )

    )
    (or (vector-push new-elt vector)
        (progn
          (extend)
          (vector-push new-elt vector) )) ) )


;;;; 15.2.29 vector
;;;
;;; Note: We can't use function make-array, since CLOS calls this
;;; function during computing discriminator and make-array calls
;;; upgraded-array-element-type or using type system based on CLOS.
;
(defun cl:vector (&rest objects)
    (declare (dynamic-extent objects))
  (let ((length (length objects)))
    (if (eql length 0)
        #()
      (loop
        with vector = (make-simple-vector length)
        for i of-type sequence-index = 0 then (1+ i)
        for object in objects do
          (setf (elt/vector vector i) object)
        finally (return vector) )) ) )
