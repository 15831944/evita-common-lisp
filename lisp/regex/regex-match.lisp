;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - Method of regex-match object.
;;; lisp/regex/regx-match.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-match.lisp#3 $
;;;
;;; Description:
;;;  This file contains methods of regex-match object.
;
(in-package :si)

;;;; first-match
;;;
;;; Description:
;;;  Evaluates regex on specified string.
;
(defun first-match (match string &optional (start 0) end)
    (declare (type regex-match match))
    (declare (type string string))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
    (declare (values t))
  (multiple-value-bind (string start end) (string-data string start end)
    (setf (slot-value match 'string)   string)
    (setf (slot-value match 'start)    start)
    (setf (slot-value match 'end)      end)
    (setf (slot-value match 'position)
      (if (regex-backward-p (slot-value match 'regex)) end start) )
    (fill (slot-value match 'start-vector) nil)
    (fill (slot-value match 'end-vector) nil)
    (next-match match) ) )


;;;; match-after
;
(defun match-after (match)
    (declare (type regex-match match))
    (declare (values (or string null)))
  (let ((match-end (match-end match)))
    (when match-end
      (subseq (slot-value match 'string)
              match-end
              (slot-value match 'end) )) ) )


;;;; match-before
;
(defun match-before (match)
    (declare (type regex-match match))
    (declare (values (or string null)))
  (let ((match-start (match-start match)))
    (when match-start
      (subseq (slot-value match 'string)
              (slot-value match 'start)
              match-start )) ) )


;;;; match-end
;
(defun match-end (match &optional  (name-or-nth 0))
    (declare (type regex-match match))
    (declare (type (or sequence-index keyword) name-or-nth))
    (declare (values (or sequence-index null)))
  (let ((index (regex-ensure-group-index match name-or-nth))
        (end-vec (slot-value match 'end-vector)) )
    (when (and index (<= 0 index (1- (length end-vec))))
      (svref end-vec index) ) ) )


;;;; match-group-count
;
(defun match-group-count (match)
  (length (slot-value match 'start-vector)) )


;;;; match-start
;
(defun match-start (match &optional (name-or-nth 0))
    (declare (type regex-match match))
    (declare (type (or sequence-index keyword) name-or-nth))
    (declare (values (or sequence-index null)))
  (let ((index (regex-ensure-group-index match name-or-nth))
        (start-vec (slot-value match 'start-vector)) )
    (when (and index (<= 0 index (1- (length start-vec))))
      (svref start-vec index) ) ) )


;;;; match-string
;
(defun match-string (match &optional (name-or-nth 0))
    (declare (type regex-match match))
    (declare (type (or sequence-index keyword) name-or-nth))
    (declare (values (or simple-string null)))
  (let ((index (regex-ensure-group-index match name-or-nth))
        (start-vec (slot-value match 'start-vector)) )
    (when (and index (<= 0 index (1- (length start-vec))))
      (let ((start (svref start-vec index)))
        (when start
          (let ((end (svref (slot-value match 'end-vector) index)))
            (subseq (slot-value match 'string) start end) )) )) ) )


;;;; matched-p
;
(defun matched-p (match)
    (declare (type regex-match match))
    (declare (values t))
  (svref (slot-value match 'start-vector) 0) )


;;;; next-match
;
(defun next-match (match)
    (declare (type regex-match match))
    (declare (values t))
  (let ((regex (slot-value match 'regex)))
    (funcall (slot-value regex 'function) match) ) )
