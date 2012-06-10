;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - API
;;; lisp/regex/regx-api.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-api.lisp#2 $
;;;
;;; Description:
;;;  This file contains string replace function using regex.
;
(in-package :si)

;;;; compile-regex
(defun compile-regex (source &rest options)
    (declare (type string source))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
    (declare (values regex))
    (declare (dynamic-extent options))
    (if (eq (getf options :target) :common-lisp)
      (apply #'regex-lisp-compile source options)
      (apply #'regex-byte-compile source options) ) )


;;;; eval-regex
(defun eval-regex (pattern string &key (start 0) end)
    (declare (type (or regex string regex-match) pattern))
    (declare (type string string))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
    (declare (values (or regex-match null)))
  (let ((match (regex-make-match pattern string start end)))
    (next-match match)
    match ) )


;;;; regex-escape
(defun regex-escape (string &key (start 0) end)
    (declare (type string string))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
    (declare (values string))
  (with-output-to-string (s)
    (multiple-value-bind (string start end) (string-data string start end)
      (loop
        for pos from start below end
        for char = (schar string pos) do
          (when (or (find char ".?*+|()[]{}\\^$#")
                    (eql (char-category char)
                          unicode:category-space-separator ))
            (write-char #\\ s) )
          (write-char char s) )) ) )


;;;; regex-group-names
(defun regex-group-names (regex)
    (declare (type regex regex))
    (declare (values list))
  (loop
    with name-vec = (slot-value regex 'name-vector)
    for nth below (length name-vec)
    for name = (svref name-vec nth)
      collect (or name (1+ nth)) ) )


;;;; regex-match
(defun regex-match (pattern string
                        &rest options
                        &key (start 0) end (capture nil capture-p)
                        &allow-other-keys )
    (declare (type string string))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
    (declare (values t))
    (declare (ignore capture))
  (remf options :start)
  (remf options :end)
  (unless capture-p (setq options (list* :capture nil options)))
  (let ((match (apply #'regex-make-match pattern string start end options)))
    (next-match match) ) )


;;;; regex-replace
(defun regex-replace (replacement pattern string
                          &key (start 0) end
                                count
                                from-end
                                preserve-case )
    (declare (type (or regex string regex-match) pattern))
    (declare (type string string))
    (declare (type (or string list symbol function) replacement))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
    (declare (type (or null integer) count))
    (declare (values string))
  (labels (
    ;; process-backward
    (process-backward (s)
        (declare (type stream s))
      (loop
        with match = (regex-make-match pattern string start end
                        :from-end t )
        with string-start     of-type sequence-index =
            (slot-value match 'start)
        with string-end       of-type sequence-index =
            (slot-value match 'end)
        with last-match-start of-type sequence-index = string-end
        for counter = (or count most-positive-fixnum) then (1- counter)
        while (and (plusp counter) (next-match match)) do
          (let ((match-start (match-start match))
                (match-end   (match-end   match)) )

            (write-reverse-string string s
              :start match-end
              :end   last-match-start )

            (setq last-match-start match-start)

            (if (listp replacement)
                (regex-eval-replacement-backward
                    replacement match preserve-case s )
              (let* ((continue-p t)
                     (result
                       (with-output-to-string (s)
                         (setq continue-p (funcall replacement match s)) ) ))
                (write-reverse-string result s)
                (unless continue-p (return)) ))

            (when (< match-start string-start)
              (loop-finish) ) )
        finally
          (write-reverse-string string s
            :start string-start
            :end   last-match-start )) )

    ;; process-forward
    (process-forward (s)
        (declare (type stream s))
      (loop
        with match = (regex-make-match pattern string start end)
        with string-start   of-type sequence-index = (slot-value match 'start)
        with string-end     of-type sequence-index = (slot-value match 'end)
        with last-match-end of-type sequence-index = string-start
        for counter = (or count most-positive-fixnum) then (1- counter)
        while (and (plusp counter) (next-match match)) do
          (let ((match-start (match-start match))
                (match-end   (match-end   match)) )

            (write-string string s
                :start last-match-end
                :end   match-start )

            (setq last-match-end match-end)

            (if (listp replacement)
                (regex-eval-replacement-forward
                    replacement match preserve-case s )
              (unless (funcall replacement match s) (return)) )

            (when (> match-end string-end)
              (loop-finish) ) )
        finally
          (write-string string s
            :start last-match-end
            :end   string-end )) )
    )
    ;;
    (when (stringp replacement)
      (setq replacement (regex-compile-replacement replacement)) )
    (when (null count) (setq count most-positive-fixnum))
    (if (not from-end)
        (with-output-to-string (s) (process-forward s))
      (let ((result (with-output-to-string (s) (process-backward s))))
        (nreverse result) )) ) )


;;;; regex-split
;
(defun regex-split (pattern string &key limit (start 0) end)
    (declare (type regex-designator pattern))
    (declare (type string string))
    (declare (type (or sequence-index null) limit))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
    (declare (values list))
  (labels (
    ;; empty-p
    (empty-p (datum)
      (or (null datum) (string= datum "")) )
    )
    ;;
    ;; regex-split
    ;;
    (multiple-value-bind (string start end) (string-data string start end)

      (when (eql start end)
        (return-from regex-split nil) )

      (when (null limit) (setq limit 0))

      (when (eql limit 1)
        (return-from regex-split
          (list (if (and (eql start 0) (eql (length string) end))
                    string
                  (subseq string start end) ))))

      (loop
        with fields = '()
        with match  = (regex-make-match pattern string start end)

        with group-count  = (match-group-count match)
        with rest         = limit
        with field-start  = start
        while (next-match match) do
          (let ((match-start (match-start match))
                (match-end   (match-end   match)) )
            (cond
              ((or (not (eql match-start match-end)))
                (push (subseq string field-start match-start) fields)

                (loop for nth from 1 below group-count do
                  (push (match-string match nth) fields) )

                (setq field-start match-end)

                (decf rest)
                (when (eql rest 1) (loop-finish)) )

              (t
                (unless (eql field-start match-start)
                  (push (subseq string field-start match-start) fields)

                  (loop for nth from 1 below group-count do
                    (push (match-string match nth) fields) )


                  (setq field-start match-end)

                  (decf rest)
                  (when (eql rest 1) (loop-finish)) ) ))

            (when (eql field-start end) (loop-finish)) )
        finally
          (unless (eql rest 0)
            (push (subseq string field-start end) fields) )

          (when (zerop limit)
            (loop while (and fields (empty-p (first fields))) do
              (pop fields) ))
          (return (nreverse fields)) ) ) ) )
