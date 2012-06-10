;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 18 Hash Tables
;;; lisp/runtime/r18-hash-table.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r18-hash-table.lisp#5 $
;;;
;;; Description:
;;;  This fils contains following functions:
;;;     clrhash                     18.2.13
;;;     gethash                     18.2.9
;;;     hash-table-count            18.2.4
;;;     hash-table-p                18.2.3      boot
;;;     hash-table-rehash-size      18.2.5      boot
;;;     hash-table-rehash-threshold 18.2.6
;;;     hash-table-size             18.2.7
;;;     hash-table-test             18.2.8
;;;     make-hash-table             18.2.2      arch
;;;     maphash                     18.2.11
;;;     remhash                     18.2.10
;;;
;;; Note: When using prime number for hash-table-size, it doesn't give
;;; good distribution. Because of keys may have trend. So, we don't try
;;; to use prime-number.
;
(in-package :si)

;;;; 18.2.13 clrhash
;
(defun cl:clrhash (htb)
    (declare (values hash-table))
  (check-type htb hash-table)
  (let ((vec (ref hash-table vector htb)))
    (let ((threshold (svref vec 1)))
      (fill vec #.(free-slot-marker))
      (setf (svref vec 0) 0)
      (setf (svref vec 1) threshold) )
    htb ) )


;;;; 18.2.9 gethash
;;;; 18.2.9 (setf gethash)
;;;; 18.2.10 remhash
;;; Description:
;;;  Removes specified key from the hash-table and returns T when specified
;;;  key is in the table, otherwise returns NIL.
;;;
;;;  To remove the key from hash-table, we put removed-slot-marker
;;;  or free-slot-marker on the slot. When following slot is key or
;;   removed-slot-marker, we put removed-slot-marker. When following slot
;;;  is free-slot-marker, we put free-slot-marker.
;;;
;;;  A removed-slot-marker denotes search chain. When hash table scanner
;;;  sees removed-slot-marker, it thinks there are other keys to be tested.
;;;
;;;  Case 1: Removing key-A
;;;   Before:
;;;     slot-1  key-A
;;;     slot-1  free-slot-marker
;;;
;;;   After:
;;;     slot-1  free-slot-marker
;;;     slot-2  free-slot-marker
;;;
;;; Case 2: Removing key-B
;;;   Before:
;;;     slot-2  key-B
;;;     slot-3  key-C   (key-C may have same (hash-code % size) of key-B.)
;;;
;;;   After:
;;;     slot-2  removed-slot-marker
;;;     slot-3  key-C
;;;
(macrolet (
  ;; for-slot
  (for-slot ((htb hash-code &optional result) &body decl*-form*)
   `(with-slot (,htb ,hash-code)
      (loop
        (locally ,@decl*-form*)
        (next-slot)
        (when (eql runner start)
          (return ,@(when result (list result))) )) ) )

  ;; next-slot
  (next-slot ()
   '(progn
      (incf runner 2)
      (when (eql runner btm)
        (setq runner top) )) )

  ;; with-slot
  (with-slot ((htb hash-code) &body decl*-form*)
    `(let* ((vec     (ref hash-table vector ,htb))
            (top    2)
            (btm    (length vec))
            (size   (ash (- btm top) -1))
            (start  (+ (ash (rem ,hash-code size) 1) top))
            (runner start)
            (rest   (svref vec 0)) )
          (declare (ignorable rest))
          ,@decl*-form* ) )

  ;; get-index
  (get-index () 'runner)

  ;; get-key/get-val
  (get-key () '(svref vec runner))
  (get-val () '(svref vec (1+ runner)))

  ;; set-key/set-val
  (set-key (key) `(setf (svref vec runner) ,key))
  (set-val (val) `(setf (svref vec (1+ runner)) ,val))

  ;; get-tomb-stone
  (get-tomb-stone ()
   `(let ((next (+ runner 2)))
      (when (eql next btm) (setq next top))
      (if (eq #.(free-slot-marker) (svref vec next))
          #.(free-slot-marker)
        #.(removed-slot-marker) ) ) )

  ;; htb-get
  (htb-get (test-fn hash-fn)
   `(block outer
      (for-slot (htb (,hash-fn key))
        (let ((present (get-key)))
          (cond
            ((eq #.(free-slot-marker) present) (return))
            ((eq #.(removed-slot-marker) present))
            ((,test-fn present key)
              (return-from outer (values (get-val) t)) )
            ((zerop (decf rest)) (return)) ) ))
      (values default nil) ) )

  ;; htb-rem
  (htb-rem (test-fn hash-fn)
   `(for-slot (htb (,hash-fn key) nil)
      (let ((present (get-key)))
        (cond
          ((eq #.(free-slot-marker) present)
            (return nil) )
          ((,test-fn present key)
            (decf (svref vec 0))
            (set-key (get-tomb-stone))
            (set-val 0)
            (return t) )) ) ) )

  ;; htb-set
  (htb-set (test-fn hash-fn)
   `(block htb-set
      (with-slot (htb (,hash-fn key))
        (let ((home 0))
          (loop
            (let ((present (get-key)))
              (cond
                ((eq #.(free-slot-marker) present)
                  (when (zerop home) (setq home (get-index)))
                  (return) )
                ((,test-fn key present)
                  (return-from htb-set (set-val val)) )
                ((eq #.(removed-slot-marker) present)
                  (when (zerop home) (setq home (get-index))) )) )
            (next-slot)
            (when (eql runner start) (return)) )
          (assert (not (zerop home)))
          (incf (svref vec 0))
          (setf (svref vec (+ home 0)) key)
          (setf (svref vec (+ home 1)) val)
          (when (need-rehash-p) (rehash ,hash-fn))
          val ) )) )

  ;; need-rehash-p
  (need-rehash-p ()
   '(>= (* (svref vec 0) 100) (svref vec 1)) )

  ;; rehash
  (rehash (hash-fn)
   `(let ((oldvec vec))
      (let* ((oldsize (1- (ash (length oldvec) -1)))
             (newsize
               (let ((rs (ref hash-table rehash-size htb)))
                 (etypecase rs
                   (fixnum (+ oldsize rs))
                   (single-float (truncate (* oldsize rs))) ) ) )
             (newvec (make-simple-vector (ash (1+ newsize) 1)
                                         #.(free-slot-marker) ) ))
        (setf (ref hash-table vector htb) newvec)
        (setf (svref newvec 0) (svref oldvec 0))
        (setf (svref newvec 1)
          (* (truncate (svref oldvec 1) oldsize) newsize) ) )
      (loop named outer
        with count = (svref oldvec 0)
        for i from 2 below (length oldvec) by 2
        for key = (svref oldvec i)
        unless (or (eq #.(removed-slot-marker) key)
                   (eq #.(free-slot-marker)    key) ) do
          (for-slot (htb (,hash-fn key))
            (when (eq (get-key) #.(free-slot-marker))
              (set-key key)
              (set-val (svref oldvec (1+ i)))
              (when (zerop (decf count)) (return-from outer))
              (return) ) )) ) )
  )
  ;;
  (defun cl:gethash (key htb &optional default)
     (ecase (hash-table-test htb)
       (eq     (htb-get eq     sxhash/eq))
       (eql    (htb-get eql    sxhash/eql))
       (equal  (htb-get equal  sxhash))
       (equalp (htb-get equalp sxhash/equalp)) ) )

  (defun (setf cl:gethash) (val key htb &optional default)
        (declare (ignore default))
     (ecase (hash-table-test htb)
       (eq     (htb-set eq     sxhash/eq))
       (eql    (htb-set eql    sxhash/eql))
       (equal  (htb-set equal  sxhash))
       (equalp (htb-set equalp sxhash/equalp)) ) )

  (defun cl:remhash (key htb)
     (ecase (hash-table-test htb)
       (eq     (htb-rem eq     sxhash/eq))
       (eql    (htb-rem eql    sxhash/eql))
       (equal  (htb-rem equal  sxhash))
       (equalp (htb-rem equalp sxhash/equalp)) ) ) )


;;;; 18.2.7 hash-table-count
(defun cl:hash-table-count (htb)
    (declare (values ext:sequence-index))
  (check-type htb hash-table)
  (svref (ref hash-table vector htb) 0) )


;;;; 18.2.6 hash-table-rehash-threshold
(defun cl:hash-table-rehash-threshold (htb)
  (check-type htb hash-table)
  (let ((vec (ref hash-table vector htb)))
    (/ (float (svref vec 1)) (* (hash-table-size htb) 100)) ) )


;;;; 18.2.7 hash-table-size
(defun cl:hash-table-size (htb)
    (declare (values ext:sequence-index))
  (check-type htb hash-table)
  (1- (ash (length (ref hash-table vector htb)) -1)) )


;;;; 18.2.7 hash-table-test
(defun cl:hash-table-test (htb)
    (declare (values (member eq eql equal equalp)))
  (check-type htb hash-table)
  (ref hash-table test htb) )

;;;; 18.2.2 make-hash-table
(defun cl:make-hash-table (&key (test             'eql)
                                (size             20)
                                (rehash-size      1.3)
                                (rehash-threshold 0.65) )
      (when (functionp test) (setq test (function-name test)))
      (check-type test             (member eq eql equal equalp))
      (check-type size             (integer 1 *))
      (check-type rehash-size      (or (integer 1 *) (float (1.0) *)))
      (check-type rehash-threshold (real 0 1))
    (let* ((size (max size 17))
           (htb
             (.allocate-record #.(class-description 'hash-table)) )
           (vec
             (make-simple-vector (ash (1+ size) 1) #.(free-slot-marker)) ))
      (setf (ref hash-table rehash-size htb) rehash-size)
      (setf (ref hash-table test        htb) test)
      (setf (ref hash-table vector      htb) vec)

      (setf (svref vec 0) 0)
      (setf (svref vec 1) (truncate (* size rehash-threshold 100)))

      htb ) )

;;;; 18.2.11 maphash
(defun cl:maphash (fn htb)
  (with-hash-table-iterator (next htb)
    (loop
      (multiple-value-bind (more-p key val) (next)
        (unless more-p (return nil))
        (funcall fn key val) )) ) )
