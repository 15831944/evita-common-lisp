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
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r17-fns.lisp#3 $
;;;
;
(in-package :si)

;;;; bounding-index-error
;
(defun bounding-index-error (sequence start end)
  (error 'bounding-index-error
        :expected-type `(integer ,start ,(length sequence))
        :sequence sequence
        :start    start
        :datum    end ) )


;;;; ensure-bounding-indexes
;;;
;;; Syntax:
;;;     ensure-bounding-indexes sequence start end => end
;;;
;;; Description:
;;;   Ensures bounding indixes.
;
(defun ensure-bounding-indexes (sequence start end)
    (declare (type sequence sequence))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-end end))
    (declare (values ext:sequence-index))
  (let* ((length
           (if (vectorp sequence)
               (length/vector sequence)
             (let ((length (safe-list-length sequence)))
               (when (minusp length)
                 (error 'not-proper-list :datum sequence) )
               length )) )
         (end (or end length)) )

    (unless (<= 0 start end length)
      (bounding-index-error sequence start end) )
     end ) )


;;;; ensure-sequence-index
;;;
;;; Syntax:
;;;     ensure-sequence-index sequence index => index
;;;
;;; Description:
;;;  Ensures sequence index.
;
(defun ensure-sequence-index (sequence index)
    (declare (type sequence sequence))
    (declare (type ext:sequence-index index))
    (declare (values ext:sequence-index))
  (let ((length
          (if (vectorp sequence)
              (length/vector sequence)
            (let ((length (safe-list-length sequence)))
              (when (minusp length)
                (error 'not-proper-list :datum sequence) )
              length )) ))

    (unless (and (integerp index) (<= 0 index (1- length)))
      (sequence-index-error sequence index) )

    index ) )


;;;; sequence-index-error
;
(defun sequence-index-error (sequence index)
  (error 'sequence-index-error
         :expected-type `(integer 0 ,(1- (length sequence)))
         :sequence      sequence
         :datum         index ) )
