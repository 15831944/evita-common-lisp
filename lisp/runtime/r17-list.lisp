;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 17 Sequences (List)
;;; runtime/r17-sequence.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r17-list.lisp#4 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;     count                       17.3.10
;;;     delete                      17.3.22
;;;     delete-duplicates           17.3.23
;;;     delete-if                   17.3.22
;;;     delq                        17.3.22
;;;     elt                         17.3.3
;;;     find                        17.3.14
;;;     fill                        17.3.4
;;;     length                      17.3.11
;;;     map                         17.3.7  NYI
;;;     map-into                    17.3.8
;;;     mismatch                    17.3.17
;;;     position                    17.3.15
;;;     position-if                 17.3.15
;;;     reduce                      17.3.9
;;;     replace                     17.3.18
;;;     reverse                     17.3.12
;;;     search                      17.3.16
;;;     sort                        17.3.13
;;;     stable-sort                 17.3.13
;;;     subseq                      17.3.6
;;;
;;; Note:
;;;  We dont' call symbol-function for key and test for saving space.
;
(in-package :si)

;;;; coerce-sublist-to-vector
;;;     => vector end
;
(defun coerce-sublist-to-vector (list start end)
    (declare (type list list))
    (declare (type sequence-index start))
    (declare (type sequence-end   end))
    (declare (values simple-vector sequence-index))
  (labels (
    ;; compute-end
    (compute-end (runner end)
        (declare (values sequence-index))
      (let ((len (length runner)))
        (cond
          ((null end) (+ len start))
          ((<= start end (+ len start)) end)
          (t (bounding-index-error list start end)) ) ) )

    ;; compute-start
    (compute-start (runner)
        (declare (values list))
      (loop
        repeat start do
          (when (endp runner)
            (sequence-index-error list start) )
          (setq runner (rest runner))
       finally (return runner) ) )
    )
    ;;
    (loop
      with runner = (compute-start list)
      with end    = (compute-end runner end)
      with length of-type sequence-index = (- end start)
      with result = (make-simple-vector length)
      for index from 0 below length do
        (setf (svref result index) (pop runner))
      finally (return (values result end)) ) ) )


;;;; 17.3.10 count
;;;; 17.3.10 count-if
;;;; 17.3.14 find
;;;; 17.3.14 find-if
;;;; 17.3.15 position
;;;; 17.3.15 position-if
;
(macrolet (
  ;; backward
  (backward (found-form test-form)
   `(multiple-value-bind (vector end)
        (coerce-sublist-to-vector list start end)
        (declare (type simple-vector vector))
        (declare (type sequence-index end))
      (loop
        for posn from (1- end) downto start
        for index = (1- (length vector)) then (1- index)
        for elt-2 = (svref vector index)
        for key-2 = (funcall/key key elt-2) do
          (when ,test-form ,found-form) ) ) )

  ;; forward
  (forward (found-form test-form)
   `(let ((end (ensure-bounding-indexes list start end)))
      (loop repeat start do (setq list (rest list)))
      (loop
        for posn from start below end
        for elt-2 = (pop list)
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
        (defun ,(intern (format nil "~A/LIST/~A" name dir))
                  (item list start end key test)
            (declare (values ,values))
          (let ,binding* ,@decl*
            (,dir ,found-form (funcall test item key-2))
            ,@result* ) )

        (defun ,(intern (format nil "~A-IF/LIST/~A" name dir))
                  (predicate list start end key)
            (declare (values ,values))
          (let ,binding* ,@decl*
            (,dir ,found-form (funcall predicate key-2))
            ,@result* ) )

        (defun ,(intern (format nil "~A-IF-NOT/LIST/~A" name dir))
                  (predicate list start end key)
            (declare (values ,values))
          (let ,binding* ,@decl*
            (,dir ,found-form (not (funcall predicate key-2)))
            ,@result* ) )) ) )
  )
  ;;
  (define count    (incf count))
  (define find     (return elt-2))
  (define position (return posn))
 ) ; macrolet


;;;; 17.3.22 delete
;;;; 17.3.23 delete-duplicates
;;;; 17.3.22 delete-if
;;;; 17.3.22 delete-if-not
;;;; 17.3.22 remove
;;;; 17.3.23 remove-duplicates
;;;; 17.3.22 remove-if
;;;; 17.3.22 remove-if-not
;;;
;;; The process of removing elements from list is done in three steps:
;;;   1. Process head portion -- from 0 below start
;;;   2. Process body portion -- from start below end
;;;   3. Process tail portion -- from end below (1- (length list))
;
(macrolet (
  ;; define
  (define (suffix lambda-list backward-form forward-form)
   `(progn
      (define-1 backward delete ,suffix ,lambda-list ,backward-form)
      (define-1 forward  delete ,suffix ,lambda-list ,forward-form)
      (define-1 backward remove ,suffix ,lambda-list ,backward-form)
      (define-1 forward  remove ,suffix ,lambda-list ,forward-form) ) )

  ;; define-1
  (define-1 (dir prefix suffix lambda-list test-form)
    (let ((fname   (intern (format nil "~A~A/LIST/~A"  prefix suffix dir)))
          (prepare (intern (format nil "PREPARE-~A"    prefix)))
          (process (intern (format nil "PROCESS-~A"    dir)))
          (cons-form
            (if (eq prefix 'delete) 'curr '(list elt-1)) )
          (count-form*
            (when (member 'count lambda-list)
              (setq test-form `(and (>= count 1) ,test-form))
              '((decf count)) ) ))
     `(defun ,fname ,lambda-list
        (block nil
          (let ((end (ensure-bounding-indexes list start end))
                (runner           list)
                (result.head      nil)
                (result.head.last nil)
                (result.body      nil)
                (result.body.last nil)
                (result.tail      nil)
                (runner.end       nil) )
              (declare (ignorable runner.end))

           ,(if (null count-form*)
               `(when (eql start end)
                 ,(if (eq prefix 'delete)
                     '(return list)
                   '(return (copy-list list)) ))
             `(progn
                (setq count (or count end))
                (when (or (eql start end) (<= count 0))
                 ,(if (eq prefix 'delete)
                     '(return list)
                   '(return (copy-list list)) ))))

            (,prepare ,dir)
            (,process ,test-form ,cons-form ,count-form*)

            ;; Concatenate result.body and result.tail
            (if (null result.body.last)
                (setq result.body result.tail)
              (setf (cdr result.body.last) result.tail) )

            ;; Concatenate result.head and result.body
            (if (null result.head.last)
                result.body
              (progn
                (setf (cdr result.head.last) result.body)
                result.head )) )) ) ) )

  ;; member-1-2-p
  (member-1-2-p (key-form-1 list-form-2)
    `(let ((key-1   ,key-form-1)
           (runner2 ,list-form-2) )
      (loop
        (when (eq runner2 runner.end) (return nil))
        (let ((key-2 (funcall/key key (car runner2))))
          (when (funcall test key-1 key-2)
            (return t) ) )
        (setq runner2 (cdr runner2)) ) ) )

  ;; member-2-1-p
  (member-2-1-p (key-form-2 list-form-1)
   `(let ((key-2 ,key-form-2))
      (dolist (elt-1 ,list-form-1)
        (let ((key-1 (funcall/key key elt-1)))
          (when (funcall test key-1 key-2)
            (return t) ) ) ) ) )

  ;; prepare-delete
  (prepare-delete (dir)
   `(progn
      ;; Process head
      (when (>= start 1)
        (setq result.head runner)
        (loop repeat start do
          (setq result.head.last runner)
          (pop runner) ))

      ;; Skip body
     ,(if (eq dir 'forward)
         `(let ((runner runner))
            (loop repeat (- end start) do
              (setq result.tail runner)
              (pop runner) )
            (shiftf result.tail (cdr result.tail) nil) )
       `(progn
          (setq result.tail runner)
          (setq runner nil)
          (loop repeat (- end start) do
            (rotatef (cdr result.tail) runner result.tail) ) ))) )

  ;; prepare-remove
  (prepare-remove (dir)
   `(progn
      ;; Process head
      (when (>= start 1)
        (setq result.head (list (pop runner)))
        (setq result.head.last result.head)
        (loop repeat (1- start) do
          (setq result.head.last
            (setf (cdr result.head.last) (list (pop runner))) )))

      ;; Process tail - copy conses in tail
     ,(if (eq dir 'forward)
         `(let ((runner runner))
            (loop repeat (- end start) do (pop runner))
            (setq runner.end runner)
            (setq result.tail (copy-list runner)) )
       `(let ((work '()))
          (loop repeat (- end start) do (push (pop runner) work))
          (setq result.tail (copy-list runner))
          (setq runner work) )) ) )

  ;; process-backward
  (process-backward (test-form cons-form count-form*)
      (declare (ignore cons-form))
   `(loop
      (when (null runner) (return))
      (let* ((curr runner)
             (elt-2 (pop runner))
             (key-2 (funcall/key key elt-2)) )
        (if (not ,test-form)
            (progn
              (when (null result.body.last)
                (setq result.body.last curr) )
              (setf (cdr curr) result.body)
              (setq result.body curr) )
          ,@count-form* ) )) )

  ;; process-forward
  (process-forward (test-form cons-form count-form*)
   `(loop
      (when (eq runner runner.end) (return))
      (let* ((curr  runner)
             (elt-1 (pop runner))
             (key-1 (funcall/key key elt-1)) )
          (declare (ignorable curr))
        (if (not ,test-form)
            (let ((curr ,cons-form))
              (if result.body.last
                  (setf (cdr result.body.last) curr)
                (setq result.body curr) )
              (setq result.body.last curr) )
          ,@count-form* ) )) )
  )
  ;;
  (define "" (item list start end key test count)
    (funcall test item key-2)
    (funcall test item key-1) )

  (define "-DUPLICATES" (list start end key test)
    (member-2-1-p key-2 runner)
    (member-1-2-p key-1 runner) )

  (define "-IF" (predicate list start end key count)
    (funcall predicate key-2)
    (funcall predicate key-1) )

  (define "-IF-NOT" (predicate list start end key count)
    (not (funcall predicate key-2))
    (not (funcall predicate key-1)) )

 ) ; macrolet


;;;; 17.3.3 elt
;
(defun elt/list (list index)
  (ensure-sequence-index list index)
  (nth index list) )


;;;; 17.3.3 (setf elt)
;
(defun (setf elt/list) (new-elt list index)
  (ensure-sequence-index list index)
  (setf (nth index list) new-elt) )


;;;; 17.3.4 fill
;
(defun fill/list (list item start end)
  (setq end (ensure-bounding-indexes list start end))
  (let ((index 0)
        (scan  list) )
    (loop
      (when (= start index) (return))
      (incf index)
      (setq scan (cdr scan)) )
    (loop
      (when (= end index) (return))
      (setf (car scan) item)
      (incf index)
      (setq scan (cdr scan)) ) )
  list )


;;;; 17.3.11 length
;
(defun length/list (list)
    (declare (type list list))
    (declare (values sequence-index))
    (declare (optimize (speed 3) (safety 0)))
  (let ((length (safe-list-length list)))
    (when (minusp length)
      (error 'si::not-proper-list :datum list) )
    length ) )


;;;; 17.3.8 map-into
;
(defun map-into/list (result function &rest sequences)
    (declare (type list result))
    (declare (dynamic-extent sequences))
  (let ((end (length result)))
    (dolist (sequence sequences)
      (setq end (min end (length sequence))) )
    (let ((index 0)
          (scan result) )
      (loop
        (when (= end index) (return))
        (setf (car scan)
          (apply function (mapcar #'(lambda (seq) (elt seq index))
                                  sequences )))
        (setq scan (cdr scan))
        (incf index) ) ) )
  result )


;;;; 17.3.17 mismatch
;
(defun mismatch/list (list1 sequence2
                      start1 end1 start2 end2 test key from-end )
    (declare (type list list1))
  (etypecase sequence2
    (list
      (if from-end
          (mismatch/list/list/backward
              list1 sequence2 start1 end1
              start2 end2
              test
              key )
          (mismatch/list/list/forward
              list1 sequence2 start1 end1
              start2 end2
              test
              key )) )
    (vector
      (if from-end
          (mismatch/list/vector/backward
              list1 sequence2 start1 end1
              start2 end2
              test
              key )
          (mismatch/list/vector/forward
              list1 sequence2 start1 end1
              start2 end2
              test
              key )) )) )


;;;; mismatch/list/list/backward
;
(defun mismatch/list/list/backward
        (list1 list2 start1 end1 start2 end2 test key)
  (multiple-value-bind (vector1 end1)
      (coerce-sublist-to-vector list1 start1 end1)
  (multiple-value-bind (vector2 end2)
      (coerce-sublist-to-vector list2 start2 end2)
  (let ((index1
          (mismatch/vector/vector/backward
              vector1
              vector2
              0 (- end1 start1)
              0 (- end2 start2)
              test
              key ) ))
    (when index1 (+ index1 start1)) ) ) ) )


;;;; mismatch/list/list/forward
;
(defun mismatch/list/list/forward
        (list1 list2 start1 end1 start2 end2 test key)
    (declare (type list list1))
    (declare (type list list2))
  (setq end1 (ensure-bounding-indexes list1 start1 end1))
  (setq end2 (ensure-bounding-indexes list2 start2 end2))

  (let ((index1  start1)
        (index2  start2)
        (runner1 (nthcdr start1 list1))
        (runner2 (nthcdr start2 list2)) )
   (loop
     (when (= end1 index1)
       (return (if (/= end2 index2) index1 nil)) )

     (when (= end2 index2)
       (return index1) )

     (let* ((elt1 (first runner1))
            (key1 (funcall/key key elt1))
            (elt2 (first runner2))
            (key2 (funcall/key key elt2)) )
       (unless (funcall test key1 key2) (return index1)) )

     (incf index1)
     (incf index2)
     (setq runner1 (rest runner1))
     (setq runner2 (rest runner2)) ) ) )


;;;; mismatch/list/vector/backward
;
(defun mismatch/list/vector/backward
        (list1 vector2 start1 end1 start2 end2 test key)
  (multiple-value-bind (vector1 end1)
      (coerce-sublist-to-vector list1 start1 end1)
  (let ((index1
          (mismatch/vector/vector/backward
              vector1
              vector2
              0 (- end1 start1)
              start2 end2
              test
              key ) ))
    (when index1 (+ index1 start1)) ) ) )


;;;; mismatch/list/vector/forward
;
(defun mismatch/list/vector/forward
        (list1 vector2 start1 end1 start2 end2 test key)
    (declare (type list   list1))
    (declare (type vector vector2))
  (setq end1 (ensure-bounding-indexes list1 start1 end1))
  (setq end2 (ensure-bounding-indexes vector2 start2 end2))

  (let ((index1  start1)
        (index2  start2)
        (runner1 (nthcdr start1 list1)) )
   (loop
     (when (= end1 index1)
       (return (if (/= end2 index2) index1 nil)) )

     (when (= end2 index2)
       (return index1) )

     (let* ((elt1 (first runner1))
            (key1 (funcall/key key elt1))
            (elt2 (row-major-aref vector2 index2))
            (key2 (funcall/key key elt2)) )
       (unless (funcall test key1 key2) (return index1)) )

     (incf index1)
     (incf index2)
     (setq runner1 (rest runner1)) ) ) )


;;;; 17.3.12 nreverse
;
(defun nreverse/list (list)
  (let ((runner nil))
    (loop
      (when (endp list) (return))
      (rotatef (cdr list) runner list) )
    runner ) )


;;;; 17.3.13 nsubstitute
;;;; 17.3.13 nsubstitute-if
;
(macrolet (
  ;; define
  (define (suffix lambda-list test-form)
   `(progn
      (define-backward ,suffix ,lambda-list ,test-form)
      (define-forward  ,suffix ,lambda-list ,test-form) ) )

  ;; define-backward
  (define-backward (suffix lambda-list test-form)
    (let ((fname (intern (format nil "NSUBSTITUTE~A/LIST/BACKWARD" suffix))))
     `(defun ,fname ,lambda-list
        (block nil
          (setq end (ensure-bounding-indexes list start end))

          (cond
            ((null count) (setq count end))
            ((<= count 0) (return list)) )

          (let* ((runner      (nthcdr start list))
                 (result.tail runner) )

            ;; Reverse result.body
            (progn
              (setq runner nil)
              (loop repeat (- end start) do
                (rotatef (cdr result.tail) runner result.tail) ) )

            ;; Process result.body
            (loop
              (when (null runner) (return))
              (let* ((curr  runner)
                     (elt-1 (pop runner))
                     (key-1 (funcall/key key elt-1)) )
                (setf (cdr curr) result.tail)
                (setq result.tail curr)
                (when ,test-form
                  (setf (car curr) newitem)
                  (decf count)
                  (when (eql count 0)
                    (loop
                      (when (null runner) (return))
                      (let ((curr runner))
                        (pop runner)
                        (setf (cdr curr) result.tail)
                        (setq result.tail curr) ))
                    (return) )) ))

          list )) ) ) )

  ;; define-forward
  (define-forward (suffix lambda-list test-form)
    (let ((fname (intern (format nil "NSUBSTITUTE~A/LIST/FORWARD" suffix))))
     `(defun ,fname ,lambda-list
        (block nil
          (setq end (ensure-bounding-indexes list start end))

          (cond
            ((null count) (setq count end))
            ((<= count 0) (return list)) )

          (let ((runner (nthcdr start list)))
            (loop repeat (- end start) do
              (let* ((curr  runner)
                     (elt-1 (pop runner))
                     (key-1 (funcall/key key elt-1)) )
                (when ,test-form
                  (setf (car curr) newitem)
                  (decf count)
                  (when (eql count 0) (return)) ) )) )

          list ) ) ) )
  )
  ;;
  (define "" (newitem olditem list start end key count test)
    (funcall test olditem key-1) )

  (define "-IF" (newitem predicate list start end key count)
    (funcall predicate key-1) )

  (define "-IF-NOT" (newitem predicate list start end key count)
    (not (funcall predicate key-1)) )
 ) ; macrolet


;;;; 17.3.9 reduce
;
(defun reduce/list (function list key from-end start end
                    initial-value-p value )
  (setq end (ensure-bounding-indexes list start end))

  (when (zerop (- end start))
    (return-from reduce/list
                 (if initial-value-p
                     (funcall function value)
                     (funcall function) )))

  (if from-end
      (setq list (nreverse (subseq/list list start end)))
    (setq list (nthcdr start list)) )

  (unless initial-value-p
    (setq value (pop list))
    (setq value (funcall/key key value))
    (incf start) )

  (if from-end
      (dolist (elt list)
        (setq elt (funcall/key key elt))
        (setq value (funcall function elt value)) )
    (dolist (elt list)
      (when (= start end) (return))
      (incf start)
      (setq elt (funcall/key key elt))
      (setq value (funcall function value elt)) ))
  value )


;;;; 17.3.18 replace
;
(defun replace/list (list1 sequence2 start1 end1 start2 end2)
  (etypecase sequence2
    (list
      (replace/list/list   list1 sequence2 start1 end1 start2 end2) )

    (vector
      (replace/list/vector list1 sequence2 start1 end1 start2 end2) ) ) )


;;;; replace/list/list
;;;
;;; Note: We don't need to save source region when overlap as 112222.
;;;
;
(defun replace/list/list (list1 list2 start1 end1 start2 end2)
  (setq end1 (ensure-bounding-indexes list1 start1 end1))
  (setq end2 (ensure-bounding-indexes list2 start2 end2))

  (let ((scan1 (nthcdr start1 list1))
        (scan2 (nthcdr start2 list2)) )

    (setq end1 (+ start1 (min (- end1 start1) (- end2 start2))))

    ;; overlap :  221111
    ;;
    (when (and (eq list1 list2) (< start1 end2 end1))
      (setq scan2 (subseq scan2 0 (- end1 start1))) )

    (let ((index1 start1))
      (loop
        (when (= end1 index1) (return))
        (setf (car scan1) (car scan2))
        (setq scan1 (cdr scan1))
        (setq scan2 (cdr scan2))
        (incf index1) ) ) )

  list1 )


(defun replace/list/vector (list1 vector2 start1 end1 start2 end2)
  (setq end1 (ensure-bounding-indexes list1 start1 end1))
  (setq end2 (ensure-bounding-indexes vector2 start2 end2))

  (let ((scan1 (nthcdr start1 list1)))

    (multiple-value-bind (data-vector offset) (vector-data vector2)
      (setq vector2 data-vector)
      (incf start2 offset)
      (incf end2   offset) )

    (setq end2 (+ start2 (min (- end1 start1) (- end2 start2))))

    (let ((index2 start2))
      (loop
        (when (= end2 index2) (return))
        (setf (car scan1) (row-major-aref vector2 index2))
        (setq scan1 (cdr scan1))
       (incf index2) ) ) )

  list1 )


;;;; 17.3.12 reverse
;
(defun reverse/list (list)
  (let ((result '()))
    (loop
      (when (endp list) (return))
      (push (pop list) result) )
    result ) )


;;;; 17.3.16 search
;
(defun search/list (list1 sequence2
                    start1 end1 start2 end2 test key from-end )
    (declare (type list list1))
  (etypecase sequence2
    (list
      (if from-end
        (search/list/list/backward
            list1 sequence2
            start1 end1
            start2 end2
            test
            key )
        (search/list/list/forward
            list1 sequence2
            start1 end1
            start2 end2
            test
            key )) )
    (vector
      (if from-end
        (search/list/vector/backward
            list1 sequence2
            start1 end1
            start2 end2
            test
            key )
        (search/list/vector/forward
            list1 sequence2
            start1 end1
            start2 end2
            test
            key )) )) )


;;;; search/list/list/backward
;
(defun search/list/list/backward
        (list1 list2
         start1 end1 start2 end2
         test key )
  (multiple-value-bind (vector1 end1)
      (coerce-sublist-to-vector list1 start1 end1)
  (multiple-value-bind (vector2 end2)
      (coerce-sublist-to-vector list2 start2 end2)
    (let ((posn2
            (search/vector/vector/backward
                vector1
                vector2
                0 (- end1 start1)
                0 (- end2 start2)
                test
                key ) ))
      (when posn2 (+ posn2 start2)) ) ) ) )


;;;; search/list/list/forward
;
(defun search/list/list/forward
        (list1 list2
         start1 end1 start2 end2
         test key )
    (declare (values (or sequence-index null)))
  (labels (
    (match (runner1 runner2 len1)
      (loop repeat len1 do
        (let* ((elt1 (pop runner1))
               (key1 (funcall/key key elt1))
               (elt2 (pop runner2))
               (key2 (funcall/key key elt2)) )
          (unless (funcall test key1 key2)
            (return nil) ))
       finally (return t) ) )
    )
    ;;
    (setq end1 (ensure-bounding-indexes list1 start1 end1))
    (setq end2 (ensure-bounding-indexes list2 start2 end2))
    (loop
      with runner1 = (nthcdr start1 list1)
      with len1    = (- end1 start1)
      for runner2  = (nthcdr start2 list2) then (cdr runner2)
      for posn2 from start2 to (- end2 len1)
        when (match runner1 runner2 len1) return posn2 ) ) )


;;;; search/list/vector/backward
;
(defun search/list/vector/backward
        (list1 vector2
         start1 end1
         start2 end2
         test key )
    (declare (values (or sequence-index null)))
  (multiple-value-bind (vector1 end1)
      (coerce-sublist-to-vector list1 start1 end1)
    (search/vector/vector/backward
        vector1
        vector2
        0 (- end1 start1)
        start2 end2
        test key ) ) )


;;;; search/list/vector/forward
;
(defun search/list/vector/forward
        (list-1 vector-2
         start1 end1
         start2 end2
         test key )
  (setq end1 (ensure-bounding-indexes list-1 start1 end1))
  (setq end2 (ensure-bounding-indexes vector-2 start2 end2))

  (decf end2 (- end1 start1 1))

  (when (<= end2 start2)
    ;; list-1 is longer than vector-2.
    (return-from search/list/vector/forward nil) )

  ;; loop over vector-2
  ;;
  (let ((scan1  (nthcdr start1 list-1))
        (lend1  (nthcdr end1   list-1))
        (index2  start2)
        (mindex2 nil) )
    (loop
      (when (= end2 index2)
        (return-from search/list/vector/forward mindex2) )
      ;; loop over list-1
      ;;
      (let ((jndex2 index2)
            (scan1  scan1) )
        (loop
          (when (eq scan1 lend1)
            (return-from search/list/vector/forward index2) )

          (let* ((elt-1 (car scan1))
                (key-1 (funcall/key key elt-1))
                (elt-2 (row-major-aref vector-2 jndex2))
                (key-2 (funcall/key key elt-2)) )
              (unless (funcall test key-1 key-2)
                (return) ) )

         (incf jndex2)
         (setq scan1 (cdr scan1)) ) )

      ;; go next element
      ;;
      (incf index2) ) ) )


;;;; 17.3.13 sort
;
(defun sort/list (list predicate key)
  (stable-sort/list list predicate key) )


;;;; 17.3.13 stable-sort
;
;;; Description:
;;;  Sorts list by iterative list merge sort.
;;;
(defun stable-sort/list (list predicate key)
  (flet (
    ;; merge-lists
    ;; Description:
    ;;  Merges two sorted lists and put it on list.
    ;;
    ;;  result ... scan - sorted list result
    ;;  list1           - sorted list input 1
    ;;  list2           - sorted list input 2
    ;;
    (merge-lists (list1 list2 predicate key)
      (let ((scan list1))
        (loop
          (when (null list2)
            (return) )

          (when (null list1)
            (setf (cdr scan) list2)
            (return) )

          (let ((elt1 (car list1))
                (elt2 (car list2)) )
            (if (funcall predicate
                         (funcall/key key elt2)
                         (funcall/key key elt1) )
                (progn
                  (setf (car list1) elt2)
                  (setf (car list2) elt1)

                  (shiftf scan list2 (cdr list2))
                  (shiftf (cdr scan) (cdr list1) scan)
                  (setq list1 scan) )

              (shiftf scan list1 (cdr list1)) ) )) ) ) )

    ;; stable-sort/list
    ;; * Outer loop iterates k=1..n, where n is length of list.
    ;; * Inner loop merged paired k elements sorted lists.
    ;;
    ;; list1, list2 - sorted list.
    ;; last1, last2 - last cons of list1 and list2 respectably.
    ;;                When length of list2 is less than k, last2 is nil.
    ;;
    (let ((k-1 0))
      (loop
        (let ((scan list))
          (loop
            (let* ((list1 scan)
                   (last1 (nthcdr k-1 list1))
                   list2
                   last2 )
              (when (null last1)
                (return) )

              (shiftf list2 (cdr last1) nil)

              (when (null list2)
                (return) )

              (setq last2 (nthcdr k-1 list2))

              (when last2
                (shiftf scan (cdr last2) nil) )

              (merge-lists list1 list2 predicate key)

              (when (null last2)
                (return) )

              #|
              (assert (or (and (cdr last1) (null (cdr last2)))
                          (and (null (cdr last1)) (cdr last2)) ))
              |#

              (if (null (cdr last1))
                  (setf (cdr last1) scan)
                (setf (cdr last2) scan) ) ))

          (when (eq scan list)
            (return) ) )

        (setq k-1 (1- (* 2 (1+ k-1)))) ) )

    list ) )



;;;; 17.3.6 subseq
;
(defun subseq/list (list start end)
    (declare (type list list))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
  (let* ((end    (- (ensure-bounding-indexes list start end) start))
         (list   (nthcdr start list))
         (result (list nil))
         (last   result) )
      (declare (type sequence-index end))
    (loop
      (when (= end 0) (return))
      (decf end)
      (setf (cdr last) (list (pop list)))
      (setq last (cdr last)) )
    (cdr result) ) )
