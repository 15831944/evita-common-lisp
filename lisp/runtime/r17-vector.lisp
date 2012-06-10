;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 17 Sequences (Vector)
;;; runtime/r17-vector.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r17-vector.lisp#5 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;     count                       17.3.10
;;;     delete                      17.3.22
;;;     delete-if                   17.3.22
;;;     elt                         17.3.3
;;;     find                        17.3.14
;;;     fill                        17.3.4
;;;     length                      17.3.10
;;;     map                         17.3.7  NYI
;;;     map-into                    17.3.8
;;;     mismatch                    17.3.17 NYI
;;;     nreverse                    17.3.12
;;;     nsubsitute                  17.3.19
;;;     nsubsitute-if               17.3.19
;;;     position                    17.3.15
;;;     position-if                 17.3.15
;;;     reduce                      17.3.9
;;;     replace                     17.3.18
;;;     reverse                     17.3.12
;;;     search                      17.3.16 NYI
;;;     sort                        17.3.13
;;;     stable-sort                 17.3.13
;;;     subseq                      17.3.6
;
(in-package :si)

;;;; 17.3.10 count
;;;; 17.3.10 count-if
;;;; 17.3.10 count-if-not
;;;; 17.3.14 find
;;;; 17.3.14 find-if
;;;; 17.3.14 find-if-not
;;;; 17.3.15 position
;;;; 17.3.15 position-if
;;;; 17.3.15 position-if-not
;
(macrolet (
  ;; backward
  (backward (found-form test-form)
   `(multiple-value-bind (vector beg end)
        (vector-data vector start end)
      (loop
        for posn from (1- end) downto beg
        for elt-2 = (elt/vector vector posn)
        for key-2 = (funcall/key key elt-2) do
          (when ,test-form ,found-form) ) ) )

  ;; forward
  (forward (found-form test-form)
   `(multiple-value-bind (vector beg end)
        (vector-data vector start end)
      (loop
        for posn from beg below end
        for elt-2 = (elt/vector vector posn)
        for key-2 = (funcall/key key elt-2) do
          (when ,test-form ,found-form) ) ) )

  ;; define
  (define (name found-form)
   `(progn
      (define-aux backward ,name ,found-form)
      (define-aux forward  ,name ,found-form) ) )

  ;; define-aux
  (define-aux (dir name found-form)
    (multiple-value-bind (values binding* decl* result*)
        (if (not (eq name 'count))
            (values 't nil nil nil)
          (values 'sequence-index
                  '((count 0))
                  '((declare (type sequence-index count)))
                  '(count) ))
     `(progn
        (defun ,(intern (format nil "~A/VECTOR/~A" name dir))
                  (item vector start end key test)
            (declare (values ,values))
          (let ,binding* ,@decl*
            (,dir ,found-form (funcall test item key-2))
            ,@result* ) )

        (defun ,(intern (format nil "~A-IF/VECTOR/~A" name dir))
                  (predicate vector start end key)
            (declare (values ,values))
          (let ,binding* ,@decl*
            (,dir ,found-form (funcall predicate key-2))
            ,@result* ) )

        (defun ,(intern (format nil "~A-IF-NOT/VECTOR/~A" name dir))
                  (predicate vector start end key)
            (declare (values ,values))
          (let ,binding* ,@decl*
            (,dir ,found-form (not (funcall predicate key-2)))
            ,@result* ) )) ) )
  )
  ;;
  (define count    (incf count))
  (define find     (return elt-2))
  (define position (return (+ (- posn beg) start)))
 ) ; macrolet


;;;; 17.3.22 delete
;;;; 17.3.23 delete-duplicates
;;;; 17.3.22 delete-if
;;;; 17.3.22 remove
;;;; 17.3.23 remove-duplicates
;;;; 17.3.22 remove-if
;;;
;;; Notes: delete-duplicates must search duplicated element from source
;;; instead of destination.
;;;     (delete-duplicates #(A B C B D D E)) => #(A C B D E)
;;;
;;; (remove/adjustable item vector)
;;;  ==
;;;   (remove/data-vector item (copy-seq (data-vector vector)))
;
(macrolet (
  ;; delete/backward => dst-start dst-end
  (delete/backward (count test-form)
   `(loop
      with dst-idx of-type sequence-index = src-end
      for src-idx downfrom (1- src-end) to src-start
      for elt = (elt src-vec src-idx) do
        (if (not ,test-form)
            (progn
              (decf dst-idx)
              (setf (elt src-vec dst-idx) elt) )
          ,@(when count `((decf count))) )
      finally (return (values dst-idx src-end)) ) )

  ;; backward => dst-start, dst-end
  (remove/backward (count test-form)
   `(loop
      with dst-idx = src-end
      for src-idx downfrom (1- src-end) to src-start
      for elt = (elt src-vec src-idx) do
        (if (not ,test-form)
            (progn
              (decf dst-idx)
              (setf (elt dst-vec dst-idx) elt) )
          ,@(when count `((decf count))) )
      finally
        (replace dst-vec dst-vec
            :start1 src-start
            :start2 dst-idx
            :end2 src-end )
        (return (+ src-start (- src-end dst-idx))) ) )

  ;; delete/forward => dst-start dst-end
  (delete/forward (count test-form)
   `(loop
      with dst-idx of-type sequence-index = src-start
      for src-idx from src-start below src-end
      for elt = (elt src-vec src-idx) do
        (if (not ,test-form)
            (progn
              (setf (elt src-vec dst-idx) elt)
              (incf dst-idx) )
          ,@(when count `((decf count))) )
      finally (return (values src-start dst-idx)) ) )

  ;; forward => dst-end
  (remove/forward (count test-form)
   `(loop
      with dst-idx = src-start
      for src-idx from src-start below src-end
      for elt = (elt src-vec src-idx) do
        (if (not ,test-form)
            (progn
              (setf (elt dst-vec dst-idx) elt)
              (incf dst-idx) )
          ,@(when count `((decf count))) )
      finally
        (return dst-idx) ) )

  ;; defun*
  (defun* (fname lambda-list decl &body body)
    `(defun ,fname ,lambda-list
        ,@(when decl (list decl))
        (declare (type vector vector))
        (declare (type (or function symbol) key))
        (declare (type sequence-index start))
        (declare (type sequence-end end))
        ,@(when (member 'test lambda-list)
            '((declare (type (function (t t) t) test))) )
        ,@(when (member 'count lambda-list)
            '((declare (type (or integer null) count))) )
        (declare (values vector))
      (block nil
        (multiple-value-bind (src-vec src-start src-end)
            (vector-data vector start end)
          ,@(when (member 'count lambda-list)
              '((cond
                  ((null count) (setq count src-end))
                  ((<= count 0) (return vector)) )))
          ,@body )) ) )

  ;; define-delete
  (define-delete (fname lambda-list decl scanner test-form)
    `(define-remove ,fname ,lambda-list ,decl ,scanner ,test-form) )

  #|
  (define-delete (fname lambda-list decl scanner test-form
                  &aux (count (first (member 'count lambda-list))) )
   `(defun* ,fname ,lambda-list ,decl
      (multiple-value-bind (dst-start dst-end)
          (,scanner ,count ,test-form)
        (when (eql dst-start dst-end)
          ;; Removed nothing
          (return vector) )

        (unless (eql dst-start src-start)
          ;; Move dst to src
          (replace src-vec src-vec
                :start1 src-start
                :start2 dst-start
                :end2   dst-end ) )

        ;; Move rest
        (replace src-vec src-vec
              :start1 (+ src-start (- dst-end dst-start))
              :start2 dst-end
              :end2   (fill-pointer vector) )

        (let ((delta (- (- dst-end dst-start) (- src-end src-start))))
          (decf (array-total-size vector) delta)

          (when (> (fill-pointer vector)
                   (array-total-size vector) )
            (setf (fill-pointer vector)
               (array-total-size vector) ))

          vector ) ) ) )
    |#

  ;; define-remove
  (define-remove (fname lambda-list decl scanner test-form
                  &aux (count (member 'count lambda-list)) )
   `(defun* ,fname ,lambda-list ,decl
      (let ((dst-vec (make-data-vector (length src-vec)
                                       (array-element-type src-vec) ) ))

        (replace dst-vec src-vec :end2 src-start)

        (let ((dst-end (,scanner ,count ,test-form )))
          (replace dst-vec src-vec :start1 dst-end :start2 src-end)
          (subseq dst-vec
                  0
                  (+ dst-end (- (length src-vec) src-end)) ) ) ) ) )

  ;; define
  (define (suffix lambda-list decl backward-test-form
           &optional (forward-test-form backward-test-form) )
    (when (member 'count lambda-list)
      (setq backward-test-form `(and (plusp count) ,backward-test-form))
      (setq forward-test-form  `(and (plusp count) ,forward-test-form)) )

    (let ((args
            (let* ((runner lambda-list)
                   (first
                     (unless (eq (first runner) 'vector)
                      (list (pop runner)) ) ))
              `(,@first
                (subseq/vector vector 0 nil)
                ,@(rest runner) ) ) )
          (remove-data-backward
            (intern (format nil "REMOVE~A/DATA-VECTOR/BACKWARD" suffix)) )
          (remove-data-forward
            (intern (format nil "REMOVE~A/DATA-VECTOR/FORWARD" suffix)) ))
     `(progn
        (define-remove
            ,remove-data-backward ,lambda-list
            ,decl
            remove/backward
            ,backward-test-form )

        (define-remove
            ,remove-data-forward ,lambda-list
            ,decl
            remove/forward
            ,forward-test-form )

        (define-delete
            ,(intern (format nil "DELETE~A/ADJUSTABLE-VECTOR/BACKWARD"
                                  suffix ) )
            ,lambda-list
            ,decl
            delete/backward
            ,backward-test-form )

        (define-delete
            ,(intern (format nil "DELETE~A/ADJUSTABLE-VECTOR/FORWARD"
                             suffix ))
            ,lambda-list
            ,decl
            delete/forward
            ,forward-test-form )

        (defun ,(intern (format nil "REMOVE~A/ADJUSTABLE-VECTOR/BACKWARD"
                                    suffix ))
               ,lambda-list
          (,remove-data-backward ,@args) )

        (defun ,(intern (format nil "REMOVE~A/ADJUSTABLE-VECTOR/FORWARD"
                                    suffix ))
               ,lambda-list
          (,remove-data-forward ,@args) )) ) )
  )
  ;;

  ;; remove
  (define "" (item vector start end key test count)
      nil
      (funcall test item (funcall/key key elt)) )

  ;; remove-duplicates
  (labels (
    (find (elt-1 vector start end key test)
      (loop
        with key-1 = (funcall/key key elt-1)
        for index of-type sequence-index from start below end
        for elt-2 = (elt vector index)
        for key-2 = (funcall/key key elt-2)
          when (funcall test key-1 key-2) return t  ) )
    )
    ;;
    (define "-DUPLICATES" (vector start end key test)
      nil
      (find elt src-vec src-start    src-idx key test)
      (find elt src-vec (1+ src-idx) src-end key test) ) )

  ;; remove-if
  (define "-IF" (predicate vector start end key count)
    (declare (type (function (t) t) predicate))
    (funcall predicate (funcall/key key elt)) )

  ;; remove-if-not
  (define "-IF-NOT" (predicate vector start end key count)
    (declare (type (function (t) t) predicate))
    (not (funcall predicate (funcall/key key elt))) )
 ) ; macrolet


;;;; 17.3.3 elt
;;;; 17.3.3 (setf elt)
;;;
;;; elt/vector is implemented as byte-code intrinsic function.


;
;;;; 17.3.4 fill
;
(defun fill/vector (vector item start end)
  (multiple-value-bind (vector start end)
      (vector-data vector start end)
    (typecase vector
      (simple-vector
        (do ((index start (1+ index)))
            ((= end index))
          (setf (svref vector index) item) ) )
      (simple-string
        (do ((index start (1+ index)))
            ((= end index))
          (setf (schar vector index) item) ) )
      (simple-bit-vector
        (cond
          ((not (and (zerop start) (= end (length vector))))
            (do ((index start (1+ index)))
                ((= end index))
            (setf (sbit vector index) item) ) )
          ((eql item 0)
            (bit-xor vector vector t) )
          ((eql item 1)
            (bit-eqv vector vector t) )
          (t
            (error 'type-error :datum item :expected-type 'bit) )) )
      (otherwise
        (do ((index start (1+ index)))
            ((= end index))
          (setf (elt/vector vector index) item) ) )) )
  vector )


;;;; 17.3.8 map-into
;
(defun map-into/vector (result function &rest sequences)
    (declare (type vector result))
    (declare (dynamic-extent sequences))
  (let ((end (array-total-size result)))
    (dolist (sequence sequences)
      (setq end (min end (length sequence))) )

    (when (array-has-fill-pointer-p result)
      (setf (fill-pointer result) end) )

    (multiple-value-bind (vector start) (vector-data result)
      (let ((index 0)
            (jndex start) )
        (loop
          (when (= end index) (return))
          (setf (elt/vector vector jndex)
                (apply function (mapcar #'(lambda (seq) (elt seq index))
                                        sequences )))
          (incf jndex)
          (incf index) )
        result ) ) ) )


;;;; 17.3.17 mismatch
;
(defun mismatch/vector (vector1 sequence2
                        start1 end1 start2 end2 test key from-end )
    (declare (type vector vector1))
  (typecase sequence2
    (list
      (if from-end
          (mismatch/vector/list/backward
            vector1 sequence2
            start1 end1
            start2 end2
            test
            key )
          (mismatch/vector/list/forward
            vector1 sequence2
            start1 end1
            start2 end2
            test
            key )) )
    (vector
      (if from-end
          (mismatch/vector/vector/backward
            vector1 sequence2
            start1 end1
            start2 end2
            test
            key )
          (mismatch/vector/vector/forward
            vector1 sequence2
            start1 end1
            start2 end2
            test
            key )) )) )


;;;; mismatch/vector/list/backward
;
(defun mismatch/vector/list/backward
        (vector1 list2 start1 end1 start2 end2 test key)
    (declare (type vector vector1))
    (declare (type list   list2))
  (multiple-value-bind (vector2 end2)
      (coerce-sublist-to-vector list2 start2 end2)
    (mismatch/vector/vector/backward
        vector1
        vector2
        start1 end1
        0 (- end2 start2)
        test
        key ) ) )


;;;; mismatch/vector/list/forward
;
(defun mismatch/vector/list/forward
        (vector1 list2 start1 end1 start2 end2 test key)
    (declare (type vector vector1))
    (declare (type list   list2))
  (setq end1 (ensure-bounding-indexes vector1 start1 end1))
  (setq end2 (ensure-bounding-indexes list2   start2 end2))
  (loop
    for index2 = start2 then (1+ index2)
    for index1 from start1 below end1
    for runner2 = (nthcdr start2 list2) then (cdr runner2) do
      (when (eql index2 end2)
        (return index1) )

      (let* ((elt1 (elt/vector vector1 index1))
             (key1 (funcall/key key elt1))
             (elt2 (first runner2))
             (key2 (funcall/key key elt2)) )
        (unless (funcall test key1 key2)
          (return index1) ) )
    finally
      (return (if (eql index2 end2) nil index1)) ) )


;;;; mismatch/vector/vector/backward
;
(defun mismatch/vector/vector/backward
        (vector1 vector2 start1 end1 start2 end2 test key)
  (multiple-value-bind (vector1 beg1 end1) (vector-data vector1 start1 end1)
  (multiple-value-bind (vector2 beg2 end2) (vector-data vector2 start2 end2)
  (loop
    for index2 = (1- end2) then (1- index2)
    for index1 from (1- end1) downto beg1 do
      (when (< index2 beg2)
        (return (+ (- (1+ index1) beg1) start1)) )

      (let* ((elt1 (elt/vector vector1 index1))
             (key1 (funcall/key key elt1))
             (elt2 (elt/vector vector2 index2))
             (key2 (funcall/key key elt2)) )
        (unless (funcall test key1 key2)
          (return (+ (- (1+ index1) beg1) start1)) ) )
    finally
      (unless (eql (1+ index2) beg2)
        (return (+ (- (1+ index1) beg1) start1)) )) ) ) )


;;;; mismatch/vector/vector/forward
;
(defun mismatch/vector/vector/forward
        (vector1 vector2 start1 end1 start2 end2 test key)
  (multiple-value-bind (vector1 beg1 end1) (vector-data vector1 start1 end1)
  (multiple-value-bind (vector2 beg2 end2) (vector-data vector2 start2 end2)
  (loop
    for index2 = beg2 then (1+ index2)
    for index1 from beg1 below end1 do
      (when (eql index2 end2)
        (return (+ (- index1 beg1) start1)) )
      (let* ((elt1 (elt/vector vector1 index1))
             (key1 (funcall/key key elt1))
             (elt2 (elt/vector vector2 index2))
             (key2 (funcall/key key elt2)) )
        (unless (funcall test key1 key2)
          (return (+ (- index1 beg1) start1)) ) )
    finally
      (unless (eql index2 end2)
        (return (+ (- index1 beg1) start1)) )) ) ) )


;;;; 17.3.12 nreverse
;
(defun nreverse/vector (vector)
  (multiple-value-bind (data-vector index jndex) (vector-data vector)
    (loop
      (when (>= index jndex) (return vector))
      (decf jndex)
      (rotatef (elt/vector data-vector index)
               (elt/vector data-vector jndex) )
      (incf index) ) ) )


;;;; 17.3.19 nsubstitute
;;;; 17.3.19 nsubstitute-if
;
(macrolet (
  ;; define
  (define (suffix lambda-list test-form)
   `(progn
      (define-1 backward ,suffix ,lambda-list ,test-form)
      (define-1 forward  ,suffix ,lambda-list ,test-form) ) )

  ;; define forward
  (define-1 (dir suffix lambda-list test-form)
    (let ((fname (intern (format nil "NSUBSTITUTE~A/VECTOR/~A" suffix dir)))
          (for*
            (if (eq dir 'forward)
                '(for index from start below end)
              '(for index from (1- end) downto start) ) ))
     `(defun ,fname ,lambda-list
        (block nil
          (setq end (ensure-bounding-indexes vector start end))

          (cond
            ((null count) (setq count end))
            ((<= count 0) (return vector)) )

          (multiple-value-bind (data-vector start end)
              (vector-data vector start end)
            (loop
              ,@for*
              for elt-1 = (elt/vector data-vector index)
              for key-1 = (funcall/key key elt-1) do
                (when ,test-form
                  (setf (elt/vector data-vector index) newitem)
                  (decf count)
                  (when (eql count 0) (loop-finish)) )
              finally (return vector) ) )) ) ) )
  )
  ;;

  (define "" (newitem olditem vector start end key count test)
    (funcall test olditem key-1) )

  (define "-IF" (newitem predicate vector start end key count)
    (funcall predicate key-1) )

  (define "-IF-NOT" (newitem predicate vector start end key count)
    (not (funcall predicate key-1)) )
 ) ; macrolet


;;;; 17.3.9 reduce
;
(defun reduce/vector (operator vector key from-end start end
                      initial-value-p value )
  (multiple-value-bind (vector start end)
      (vector-data vector start end)
    (cond
      ((= end start)
        (if initial-value-p
            (funcall operator value)
          (funcall operator) ) )
      (from-end
        ;; Right associative
        (unless initial-value-p
          (decf end)
          (let ((elt (elt/vector vector end)))
           (setq value (funcall/key key elt)) ))
        (loop
          for posn from (1- end) downto start
          for elt = (elt/vector vector posn)
          for value2 = (funcall/key key elt) do
            (setq value (funcall operator value2 value))
          finally (return value) ) )
      (t
        ;; Left associative
        (unless initial-value-p
          (let ((elt (elt/vector vector start)))
            (setq value (funcall/key key elt)) )
          (incf start) )
        (loop
          for posn from start below end
          for elt = (elt/vector vector posn)
          for value2 = (funcall/key key elt) do
            (setq value (funcall operator value value2))
          finally (return value) ) )) ) )


;;;; 17.3.18 replace
;
(defun replace/vector (vector1 sequence2 start1 end1 start2 end2)
  (etypecase sequence2
    (vector
      (replace/vector/vector vector1 sequence2 start1 end1 start2 end2) )

    (list
      (replace/vector/list vector1 sequence2 start1 end1 start2 end2) )) )


;;;; replace/vector/list
;
(defun replace/vector/list (vector1 list2 start1 end1 start2 end2)
  (multiple-value-bind (bv1 start1 end1) (vector-data vector1 start1 end1)
    (setq end2 (ensure-bounding-indexes list2 start2 end2))
    (setq list2 (nthcdr start2 list2))
    (setq end1 (+ start1 (min (- end1 start1) (- end2 start2))))

    (let ((index1 start1))
      (loop
        (when (= end1 index1) (return))
        (setf (elt/vector bv1 index1) (pop list2))
        (incf index1) ) ) )
  vector1 )


;;; Note: .replace-vector handles overlaped region.
;
(defun replace/vector/vector (vector1 vector2 start1 end1 start2 end2)
  (multiple-value-bind (bv1 start1 end1) (vector-data vector1 start1 end1)
  (multiple-value-bind (bv2 start2 end2) (vector-data vector2 start2 end2)
    (if (eq (class-of bv1) (class-of bv2))
        (.replace-vector bv1 bv2 start1 end1 start2 end2)
      (let ((index1 start1)
            (index2 start2)
            (count  (min (- end1 start1) (- end2 start2))) )
        (setq end1 (+ start1 count))
        (loop
          (when (= end1 index1) (return))
          (setf (elt/vector bv1 index1) (elt/vector bv2 index2))
          (incf index1)
          (incf index2) ) )) ) )
  vector1 )


;;;; 17.3.12 reverse
;
(defun reverse/vector (vector)
  (multiple-value-bind (vector start end) (vector-data vector)
    (let ((result (make-array (- end start)
                    :element-type (array-element-type vector) ) )
          (index start)
          (jndex end) )
      (loop
        (when (= index end) (return))
        (decf jndex)
        (setf (elt/vector result jndex) (elt/vector vector index))
        (incf index) )
      result ) ) )


;;;; 17.3.16 search
;
(defun search/vector (vector1 sequence2
                      start1 end1 start2 end2 test key from-end )
    (declare (type vector vector1))
  (typecase sequence2
    (list
      (if from-end
          (search/vector/list/backward
                vector1 sequence2
                start1 end1
                start2 end2
                test
                key )
          (search/vector/list/forward
                vector1 sequence2
                start1 end1
                start2 end2
                test
                key )) )
    (vector
      (if from-end
          (search/vector/vector/backward
                vector1 sequence2
                start1 end1
                start2 end2
                test
                key )
          (search/vector/vector/forward
                vector1 sequence2
                start1 end1
                start2 end2
                test
                key )) )) )


;;;; search/vector/list/backward
;;;
(defun search/vector/list/backward
        (vector1 list2 start1 end1 start2 end2 test key)
  (multiple-value-bind (vector2 end2)
      (coerce-sublist-to-vector list2 start2 end2)
    (let ((posn
            (search/vector/vector/backward
                vector1
                vector2
                start1 end1
                0 (- end2 start2)
                test
                key ) ))
      (when posn (+ posn start2)) ) ) )


;;;; search/vector/list/forward
;;;
;;; Description:
;;;  Loops over list-2 and returns start position where elements of vector-1
;;;  match to elements of list-2.
;;;
;;;  If from-end is true, loops over entire list-2 even if match is found,
;;;  then returns the last matched position.
;;;
;
(defun search/vector/list/forward
        (vector1 list2 start1 end1 start2 end2 test key)
  (labels (
    ;; match
    (match (vector1 list2 start1 end1)
      (loop
        for index1 from start1 below end1
        for elt1 = (elt/vector vector1 index1)
        for key1 = (funcall/key key elt1)

        for runner2 = list2 then (rest runner2)
        for elt2 = (first runner2)
        for key2 = (funcall/key key elt2)

         unless (funcall test key1 key2) return nil
       finally (return t) ) )
    )
    ;;
    (setq end1 (ensure-bounding-indexes vector1 start1 end1))
    (setq end2 (ensure-bounding-indexes list2   start2 end2))
    (loop
      for runner2 = (nthcdr start2 list2) then (rest runner2)
      for index2 from start2 below end2
        when (match vector1 runner2 start1 end1) return index2 ) ) )


;;;; search/vector/vector/backward
;
(defun search/vector/vector/backward
        (vector1 vector2 start1 end1 start2 end2 test key)
  (multiple-value-bind (vector1 beg1 end1) (vector-data vector1 start1 end1)
  (multiple-value-bind (vector2 beg2 end2) (vector-data vector2 start2 end2)
  (labels (
    ;; matach
    (match (end2)
      (loop
        for index1 from (1- end1) downto beg1
        for elt1 = (elt/vector vector1 index1)
        for key1 = (funcall/key key elt1)

        for index2 = (1- end2) then (1- index2)
        for elt2 = (elt/vector vector2 index2)
        for key2 = (funcall/key key elt2)
          unless (funcall test key1 key2) return nil
        finally (return t) ) )
    )
    ;;
    (loop
      with len1 = (- end1 beg1)
      for index2 from end2 downto (+ beg2 len1)
        when (match index2)
          return (+ (- index2 len1) start2)) ) ) ) )


;;;; search/vector/vector/forward
;
(defun search/vector/vector/forward
        (vector1 vector2 start1 end1 start2 end2 test key)
  (multiple-value-bind (vector1 beg1 end1) (vector-data vector1 start1 end1)
  (multiple-value-bind (vector2 beg2 end2) (vector-data vector2 start2 end2)
  (labels (
    ;; matach
    (match (beg2)
      (loop
        for index1 from beg1 below end1
        for elt1 = (elt/vector vector1 index1)
        for key1 = (funcall/key key elt1)

        for index2 = beg2 then (1+ index2)
        for elt2 = (elt/vector vector2 index2)
        for key2 = (funcall/key key elt2)
          unless (funcall test key1 key2) return nil
        finally (return t) ) )
    )
    ;;
    (loop
      with len1 = (- end1 beg1)
      for index2 from beg2 to (- end2 len1)
        when (match index2)
          return (+ (- index2 beg2) start2) ) ) ) ) )


;;; 17.3.13 sort
;;;
;;; Description:
;;;  Sorts vector by heap sort.
;
(defun sort/vector (vector predicate key)
    (declare (type vector vector))
  (labels (
    (heapify (vector index last)
      (let ((child (1- (ash (1+ index) 1))))
        (when (<= child last)
          (let ((child2 (1+ child)))
            (when (and (<= child2 last)
              (funcall predicate
                       (funcall/key key (elt vector child))
                       (funcall/key key (elt vector child2)) ))
              (setq child child2) )

            (when (funcall predicate
                          (funcall/key key (elt vector index))
                          (funcall/key key (elt vector child)) )
              (rotatef (elt vector index) (elt vector child)) )

            (heapify vector child last) ) )) )
    )
    ;;
    (let ((last (1- (length vector))))
      ;; build heap.
      (do ((index (ash last -1) (1- index)))
          ((< index 0))
        (heapify vector index last) )

      ;; sort.
      (do ((index last (1- index)))
          ((<= index 0))
        (rotatef (elt vector index) (elt vector 0))
        (heapify vector 0 (1- index)) )

      vector ) ) )


;;; 17.3.13 stable-sort/vector
;;;
;;; Description:
;;;  Use stable-sort/list.
;;;
;;; BUGBUG: Should make vector instead of cons. Cons requires two times
;;; bigger storage.
;;;
(defun stable-sort/vector (vector predicate key)
    (declare (type vector vector))
  (let ((list (stable-sort/list (coerce vector 'list) predicate key)))
    (dotimes (index (length vector))
      (setf (elt/vector vector index) (pop list)) )
    vector ) )


;;;; 17.3.6 subseq
;
(defun subseq/vector (vector start end)
    (declare (type vector vector))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
    (declare (values (simple-array * (*))))
  (multiple-value-bind (data-vector start end) (vector-data vector start end)
    (let ((result (make-array (- end start)
                    :element-type (array-element-type vector) ) ))
      (.replace-vector result data-vector 0 nil start end)
      result ) ) )
