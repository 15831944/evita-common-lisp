;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 17 Sequences
;;; runtime/r17-sequence.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r17-sequence.lisp#2 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;     ensure-sequence-index       internal
;;;     ensure-bounding-indexes     internal
;;;
;;;     concatenate                 17.3.20
;;;     count                       17.3.10
;;;     count-if                    17.3.10
;;;     copy-seq                    17.3.2
;;;     delete                      17.3.22
;;;     delete-duplicates           17.3.23
;;;     delete-if                   17.3.22
;;;     elt                         17.3.3
;;;     fill                        17.3.4
;;;     find                        17.3.14
;;;     find-if                     17.3.14
;;;     length                      17.3.11
;;;     make-sequence               17.3.5
;;;     map                         17.3.7
;;;     map-into                    17.3.8
;;;     merge                       17.3.21
;;;     mismatch                    13.3.17
;;;     nreverse                    17.3.12
;;;     nsubstitute                 17.3.19
;;;     nsubstitute-if              17.3.19
;;;     position                    17.3.15
;;;     position-if                 17.3.15
;;;     reduce                      17.3.9
;;;     remove                      17.3.22
;;;     remove-duplicates           17.3.22
;;;     remove-if                   17.3.22
;;;     replace                     17.3.18
;;;     reverse                     17.3.12
;;;     search                      17.3.16
;;;     sort                        17.3.13
;;;     stable-sort                 17.3.13
;;;     subseq                      17.3.6
;;;     substitute                  17.3.19
;;;     substitute-if               17.3.19
;
(in-package :si)

;;;; 17.3.20 concatenate
(defun cl:concatenate (result-type &rest sequences)
    (declare (dynamic-extent sequences))
  (let ((length 0))
    (dolist (sequence sequences)
      (incf length (length sequence)) )
    (let ((result (make-sequence result-type length)))
      (typecase result
        (list
          (let ((scan result))
            (dolist (sequence sequences)
              (let ((nelts (length sequence)))
                (replace/list scan sequence 0 nil 0 nil)
                (setq scan (nthcdr nelts scan)) ) ) ) )

        (vector
          (let ((index 0))
            (dolist (sequence sequences)
              (replace/vector result sequence index nil 0 nil)
              (incf index (length sequence)) ) ) ))
      result ) ) )


;;;; 17.3.10 count
;;;; 17.3.10 count-if
;;;; 17.3.14 find
;;;; 17.3.14 find-if
;;;; 17.3.15 position
;;;; 17.3.15 position-if
;;;
;;; Note: from-end doesn't affect result but it affects when test function
;;; is called.
;;;
;
(macrolet (
  ;; define
  (define (name)
   `(progn
      (define-1 ,name item)
      (define-1 ,(intern (format nil "~A-IF" name)) predicate)
      (define-1 ,(intern (format nil "~A-IF-NOT" name)) predicate) ) )

  ;; define-1
  (define-1 (fname first)
    (multiple-value-bind (lambda-list args)
      (if (eq first 'item)
          (values '(item sequence &key key from-end (start 0) end
                    test test-not )
                  '(item sequence start end key test) )
        (values '(predicate sequence &key key from-end (start 0) end)
                '(predicate sequence start end key) ))
     `(defun ,fname ,lambda-list
        ,@(when (eq first 'item)
            '((setq test (ensure-test-function test test-not))) )

        (etypecase sequence
          (list
            (if from-end
                (,(intern (format nil "~A/LIST/BACKWARD" fname)) ,@args)
              (,(intern (format nil "~A/LIST/FORWARD" fname)) ,@args) ) )
          (vector
            (if from-end
                (,(intern (format nil "~A/VECTOR/BACKWARD" fname)) ,@args)
              (,(intern (format nil "~A/VECTOR/FORWARD" fname))
               ,@args ) ) )) ) ) )
  )
  ;;
  (define cl:count)
  (define cl:find)
  (define cl:position)
 ) ; macrolet


;;;; 17.3.2 copy-seq
;
(defun cl:copy-seq (sequence)
    (declare (type sequence sequence))
    (declare (values sequence))
  (subseq sequence 0) )


;;;; 17.3.22 delete
;;;; 17.3.23 delete-duplicates
;;;; 17.3.22 delete-if
;;;; 17.3.22 remove
;;;; 17.3.23 remove-duplicates
;;;; 17.3.22 remove-if
;
(macrolet (
  (define (prefix suffix first test)
    (let ((args
            `(,@(when first (list first))
              sequence
              start end
              key
              ,@(when test  '(test))
              ,@(when first '(count)) ) ))
     `(defun ,(intern (format nil "~A~A" prefix suffix))
                (,@(when first (list first))
                 sequence
                 &key (start 0) end
                      key
                      from-end
                     ,@(when test  '(test test-not))
                     ,@(when first '(count)) )
        ,@(when test '((setq test (ensure-test-function test test-not))))
        (etypecase sequence
          (list
            (if from-end
                (,(intern (format nil "~A~A/LIST/BACKWARD" prefix suffix))
                 ,@args )
                (,(intern (format nil "~A~A/LIST/FORWARD"  prefix suffix))
                 ,@args )) )
          (data-vector
            (if from-end
                (,(intern (format nil "REMOVE~A/DATA-VECTOR/BACKWARD" suffix))
                 ,@args )
                (,(intern (format nil "REMOVE~A/DATA-VECTOR/FORWARD"  suffix))
                 ,@args )) )
          (vector
            (if from-end 
                (,(intern (format nil "~A~A/ADJUSTABLE-VECTOR/BACKWARD"
                                  prefix suffix ))
                 ,@args )
                (,(intern (format nil "~A~A/ADJUSTABLE-VECTOR/FORWARD"
                                  prefix suffix ))
                 ,@args )) )) ) ) )
  )
  ;;
  (define delete ""             item test)
  (define delete "-DUPLICATES"  nil test)
  (define delete "-IF"          predicate nil)
  (define delete "-IF-NOT"      predicate nil)

  (define remove ""             item test)
  (define remove "-DUPLICATES"  nil test)
  (define remove "-IF"          predicate nil)
  (define remove "-IF-NOT"      predicate nil)
 ) ; macrolet


;;;; 17.3.3 elt
;;;
;;; Syntax:
;;;     elt sequence index
;;;     (setf (elt sequence index) new-object)
;
(defun cl:elt (sequence index)
    (declare (type sequence sequence))
    (declare (type ext:sequence-index start))
    (declare (values t))
  (etypecase sequence
    (list    (elt/list sequence index))
    (vector  (elt/vector sequence index)) ) )


;;;; (setf elt)
;
(defun (setf cl:elt) (new-object sequence index)
    (declare (type sequence sequence))
    (declare (type ext:sequence-index start))
    (declare (values t))
  (etypecase sequence
    (list      (setf (elt/list   sequence index) new-object))
    (vector    (setf (elt/vector sequence index) new-object)) ) )


;;;; 17.3.4 fill
;;;
;;; Syntax:
;;;     fill sequence item &key start end
;
(defun cl:fill (sequence item &key (start 0) end)
    (declare (type sequence sequence))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-end end))
    (declare (values sequence))
  (etypecase sequence
    (list      (fill/list   sequence item start end))
    (vector    (fill/vector sequence item start end)) ) )


;;;; 17.3.11 length
;
(defun cl:length (sequence)
    (declare (type sequence sequence))
    (declare (values ext:sequence-index))
  (etypecase sequence
    (list   (length/list   sequence))
    (vector (length/vector sequence)) ) )


;;;; 17.3.5 make-sequence
;
(defun cl:make-sequence (result-type size
                         &key (initial-element nil init-p) )
    (declare (type ext:type-specifier result-type))
    (declare (type ext:sequence-index size))
    (declare (values sequence))
  (multiple-value-bind (type element-type length)
      (parse-sequence-type result-type)
    (ecase type
      ((list)
        (make-list size :initial-element initial-element) )

      ((vector)
        (when (and length (/= size length))
          (error 'type-error
                 :datum size
                 :expected-type `(integer ,length ,length) ))
        (if init-p
            (make-array size :element-type element-type
                             :initial-element initial-element )
          (make-array size :element-type element-type) ) )) ) )


;;;; 17.3.7 map
;
(defun cl:map (result-type function sequence-1 &rest sequences)
    (declare (type ext:type-specifier result-type))
    (declare (type function function))
    (declare (values sequence))
    (declare (dynamic-extent sequences))
  (push sequence-1 sequences)
  (let ((end (length (first sequences))))
    (dolist (sequence (rest sequences))
      (setq end (min end (length sequence))) )

    (if (null result-type)
        (let ((index 0))
          (loop
            (when (= end index) (return))
            (apply function (mapcar #'(lambda (seq) (elt seq index))
                                    sequences ))
            (incf index) ) )
      (multiple-value-bind (type element-type length)
          (parse-sequence-type result-type)
        (ecase type
          ((list)
            (let ((result '())
                  (index 0) )
              (loop
                (when (= end index) (return))
                (push (apply function (mapcar #'(lambda (seq) (elt seq index))
                                              sequences ))
                      result )
                (incf index) )
              (nreverse result) ) )

          ((vector)
            (let ((result (make-array end :element-type element-type) )
                  (index 0) )
              (loop
                (when (= end index) (return))
                (setf (row-major-aref result index)
                      (apply function (mapcar #'(lambda (seq) (elt seq index))
                                              sequences )))
                (incf index) )
              (when (and length (/= length end))
                (error 'type-error :expected-type result-type :datum result) )
              result ) )) )) ) )


;;;; 17.3.8 map-into
;
(defun cl:map-into (result function &rest sequences)
    (declare (type function-designator function))
    (declare (dynamic-extent sequences))
    (declare (values sequence))
 (etypecase result
   (list
     (apply #'map-into/list   result function sequences) )
   (vector
     (apply #'map-into/vector result function sequences) )) )


;;;; 17.3.21 merge
;;;
;;; BUGBUG: NYI: merge/list/list/list
;;; BUGBUG: NYI: merge/list/list/vector
;;; BUGBUG: NYI: merge/list/vector/list
;;; BUGBUG: NYI: merge/list/vector/vector
;;; BUGBUG: NYI: merge/vector/list/list
;;; BUGBUG: NYI: merge/vector/list/vector
;;; BUGBUG: NYI: merge/vector/vector/list
;;; BUGBUG: NYI: merge/vector/vector/vector
;
(defun cl:merge (result-type sequence-1 sequence-2 predicate &key key)
  (let* ((length-1 (length sequence-1))
         (length-2 (length sequence-2))
         (result (make-sequence result-type (+ length-1 length-2)))
         (index-1 0)
         (index-2 0)
         (jndex   0) )
    (loop
      (when (= length-1 index-1)
        (when (< index-2 length-2)
          (replace result sequence-2 :start1 jndex :start2 index-2) )
        (return) )

      (when (= length-2 index-2)
        (replace result sequence-1 :start1 jndex :start2 index-1)
        (return) )

      (assert (and (< index-1 length-1) (< index-2 length-2)))

      (let* ((elt-1 (elt sequence-1 index-1))
             (key-1 (funcall/key key elt-1))
             (elt-2 (elt sequence-2 index-2))
             (key-2 (funcall/key key elt-2)) )
        (cond
          ((funcall predicate key-1 key-2)
             (setf (elt result jndex) elt-1)
             (incf jndex)
             (incf index-1) )

          ((funcall predicate key-2 key-1)
             (setf (elt result jndex) elt-2)
             (incf jndex)
             (incf index-2) )

          (t
            (setf (elt result jndex) elt-1)
            (incf jndex)
            (incf index-1) )) ))
    result ) )


;;;; 13.3.17 mismatch
;
(defun cl:mismatch (sequence-1 sequence-2
                    &key (start1 0) end1 (start2 0) end2
                         test test-not key from-end )
  (setq test (ensure-test-function test test-not))
  (etypecase sequence-1
    (list
      (mismatch/list   sequence-1 sequence-2
                                 start1 end1 start2 end2
                                 test key from-end ) )

    (vector
      (mismatch/vector sequence-1 sequence-2
                                 start1 end1 start2 end2
                                 test key from-end ) )) )


;;;; 17.3.12 nreverse
;
(defun cl:nreverse (sequence)
  (etypecase sequence
    (list      (nreverse/list   sequence))
    (vector    (nreverse/vector sequence)) ) )


;;;; 17.3.13 nsubstitute
;;;; 17.3.13 nsubstitute-if
;;;; 17.3.13 nsubstitute-if-not
;
(macrolet (
  ;; define
  (define (fname second &rest test*)
    (let ((lambda-list
            `(newitem ,second sequence
              &key key from-end (start 0) end count ,@test* ) )
          (args
            `(newitem ,second sequence start end key count
              ,@(when test* '(test)) ) ))

     `(defun ,fname ,lambda-list
       ,@(when test* '((setq test (ensure-test-function test test-not))))
        (etypecase sequence
          (list
            (if from-end
                (,(intern (format nil "~A/LIST/BACKWARD" fname)) ,@args)
              (,(intern (format nil "~A/LIST/FORWARD"  fname)) ,@args) ) )
          (vector
            (if from-end
                (,(intern (format nil "~A/VECTOR/BACKWARD" fname)) ,@args)
              (,(intern (format nil "~A/VECTOR/FORWARD"  fname)) ,@args) ) )) )
      ) ) ; define
  )
  ;;
  (define cl:nsubstitute        olditem test test-not)
  (define cl:nsubstitute-if     predicate)
  (define cl:nsubstitute-if-not predicate)
 ) ; macrolet


;;;; 17.3.9 reduce
;
(defun cl:reduce (function sequence
                  &key key from-end
                       (start 0)end
                       (initial-value nil initial-value-p) )
  (etypecase sequence
    (list
      (reduce/list   function sequence
                     key from-end start end
                     initial-value-p
                     initial-value ) )
    (vector
      (reduce/vector function sequence
                     key from-end start end
                     initial-value-p
                     initial-value ) )) )


;;;; 17.3.18 replace
;
(defun cl:replace (sequence1 sequence2
                    &key (start1 0) end1
                         (start2 0) end2 )
  (etypecase sequence1
    (list
      (replace/list   sequence1 sequence2 start1 end1 start2 end2) )

    (vector
      (replace/vector sequence1 sequence2 start1 end1 start2 end2) ) ) )


;;;; 17.3.12 reverse
;
(defun cl:reverse (sequence)
  (etypecase sequence
    (list   (reverse/list   sequence))
    (vector (reverse/vector sequence))) )


;;;; 17.3.16 search
;
(defun cl:search (sequence-1 sequence-2
                  &key (start1 0) end1 (start2 0) end2
                       test test-not
                       key from-end )
  (setq test (ensure-test-function test test-not))
  (etypecase sequence-1
    (list
        (search/list   sequence-1 sequence-2
                       start1 end1 start2 end2
                       test key from-end ) )

    (vector
        (search/vector sequence-1 sequence-2
                       start1 end1 start2 end2
                       test key from-end ) )) )


;;;; 17.3.13 sort
;;;
;;; Syntax:
;;;     sort sequence predicate &key key => sorted-sequence
;;;
;;; Description:
;;;  We use stable-sort/list for sort/list. Because, known good algorithm for
;;;  sorting list is list merge sort.
;;;
;;; See: stable-sort/list
;
(defun cl:sort (sequence predicate &key key)
  (etypecase sequence
    (list    (stable-sort/list sequence predicate key))
    (vector  (sort/vector sequence predicate key)) ) )


;;;; 17.3.13 stable-sort
;;;
;;; Syntax:
;;;     stable-sort sequence predicate &key key => sorted-sequence
;;;
;;; Note:
;;;  When x = y,
;;;     (funcall predicate x y) = false and
;;;     (funcall predicate y x) = false
;;;  So, when you use operators that include equality, such as >=, <=,
;;;  the sorting isn't stable.
;
(defun cl:stable-sort (sequence predicate &key key)
  (etypecase sequence
    (list    (stable-sort/list   sequence predicate key))
    (vector  (stable-sort/vector sequence predicate key)) ) )


;;;; 17.3.6 subseq
;
(defun cl:subseq (sequence start &optional end)
 (etypecase sequence
   (list   (subseq/list   sequence start end))
   (vector (subseq/vector sequence start end)) ) )


;;;; 17.3.13 substitute
;
(defun cl:substitute (newitem olditem sequence &rest args)
    (declare (dynamic-extent args))
  (apply #'nsubstitute newitem olditem (copy-seq sequence) args) )


;;;; 17.3.13 substitute-if
;
(defun cl:substitute-if (newitem predicate sequence &rest args)
    (declare (dynamic-extent args))
  (apply #'nsubstitute-if newitem predicate (copy-seq sequence) args) )


;;;; 17.3.13 substitute-if-not
;
(defun cl:substitute-if-not (newitem predicate sequence &rest args)
    (declare (dynamic-extent args))
  (apply #'nsubstitute-if-not newitem predicate (copy-seq sequence) args) )
