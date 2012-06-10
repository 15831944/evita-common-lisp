;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - functions
;;; lisp/regex/regx-fns.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-fns.lisp#2 $
;;;
;;; Description:
;;;  This file contains definition of external symbols of RegEx facility.
;
(in-package :si)

;;;; make-load-form regex
;
(defmethod cl:make-load-form ((re regex) &optional env)
    (declare (ignore env))
    (declare (values form form))
  (make-load-form-saving-slots re) )


;;;; REGEX-INTERNAL-ERROR
;
(defun REGEX-INTERNAL-ERROR (datum &rest args)
  (apply #'error datum args) )


;;;; regex-backward-p
;
(defun regex-backward-p (regex)
    (declare (type regex regex))
    (declare (values t))
  (not (zerop (logand (slot-value regex 'flags) REGEX-FLAG-SCAN-BACKWARD))) )


;;;; regex-compile-replacement
;
(defun regex-compile-replacement (string &optional (start 0) end)
  (multiple-value-bind (string start end) (string-data string start end)
  (labels (
    ;; invalid-replacement
    (invalid-replacement (pos)
      (error 'regex-invalid-replacement
        :string (subseq string start end)
        :position pos ) )
    )
    ;;
    (with-collector (collect)
      (loop
        with strbuf = (regex-context-buffer (regex-get-context))
        for pos of-type sequence-index = start then (1+ pos)
        with state = :normal
        with nchars = 0
        with acc = 0
        while (< pos end) do
          (let ((char (schar string pos)))
            (ecase state
              ((:normal)
                (case char
                  ((#\$)
                    (when (plusp nchars)
                      (collect (subseq string (- pos nchars) pos))
                    (setq nchars 0) )
                    (setq state :dollar) )
                  ((#\\)
                    (when (plusp nchars)
                      (collect (subseq string (- pos nchars) pos))
                    (setq nchars 0) )
                    (setq state :escape) )
                  (otherwise
                    (incf nchars) )) )
              ((:dollar)
                (case char
                  ((#\$)
                    (collect #\$)
                    (setq state :normal) )
                  ((#\&)
                    (collect 0)
                    (setq state :normal) )
                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                    (setq acc (digit-char-p char))
                    (setq state :capture) )
                  ((#\`)
                    (collect 'match-before)
                    (setq state :normal) )
                  ((#\')
                    (collect 'match-after)
                    (setq state :normal) )
                  ((#\+)
                    (collect '+)
                    (setq state :normal) )
                  ((#\_)
                    (collect 'match-target)
                    (setq state :normal) )
                  ((#\u007B)
                    (setq state :ref) )
                  (otherwise
                    (INVALID-REPLACEMENT pos) )) )
              ((:capture)
                (let ((digit (digit-char-p char)))
                  (if digit
                      (progn
                        (setq acc (+ (* acc 10) digit)) )
                    (progn
                      (collect acc)
                      (decf pos)
                      (setq state :normal) )) ) )
              ((:escape)
                (block escape
                  (case char
                    ((#\a) (collect (code-char #x01)))
                    ((#\b) (collect (code-char #x08)))
                    ((#\c) (setq state :escape-c) (return-from escape))
                    ((#\e) (collect (code-char #x1B)))
                    ((#\f) (collect (code-char #x0C) #+nil #\Page))
                    ((#\n) (collect (code-char #x0A) #+nil #\Newline))
                    ((#\r) (collect (code-char #x0D) #+nil #\Return))
                    ((#\t) (collect (code-char #x09) #+nil #\Tab))
                    ((#\u)
                      (setq nchars 0)
                      (setq acc 0)
                      (setq state :escape-u)
                      (return-from escape) )
                    ((#\v) (collect (code-char #x0B)))
                    ((#\x)
                      (setq state :escape-x)
                      (return-from escape) )
                    ((#\\) (setq nchars 1))
                    (otherwise
                      (let ((digit (digit-char-p char 8)))
                        (when (null digit) (invalid-replacement char))
                        (setq nchars 1)
                        (setq acc digit)
                        (setq state :escape-0)
                        (return-from escape) )))
                  (setq state :normal) ) )
              ((:escape-0)
                (let ((digit (digit-char-p char 8)))
                  (if (null digit)
                      (progn
                        (decf pos)
                        (setq nchars 3) )
                    (progn
                      (setq acc (logior (ash acc 3) digit))
                      (incf nchars) ))
                  (when (eql nchars 3)
                    (collect (code-char acc))
                    (setq nchars 0)
                    (setq state :normal) ) ) )
              ((:escape-c)
                (cond
                  ((char<= #\@ char #\u005F)
                    (collect (code-char (- (char-code char) #x40))) )
                  ((char<= #\a char #\z)
                    (collect (code-char (- (char-code char) #x60))) )
                  (t
                    (invalid-replacement pos) ))
                (setq state :normal) )
              ((:escape-u)
                (let ((digit (digit-char-p char 16)))
                  (unless digit (invalid-replacement pos))
                  (setq acc (logior (ash acc 4) digit))
                  (incf nchars)
                  (when (eql nchars 4)
                    (collect (code-char acc))
                    (setq nchars 0)
                    (setq state :normal) ) ) )
              ((:escape-x)
                (if (char= char #\u007B)
                    (progn
                      (setq acc 0)
                      (setq nchars 0)
                      (setq state :escape-x4) )
                  (let ((digit (digit-char-p char 16)))
                    (when (null digit) (invalid-replacement pos))
                    (setq acc digit)
                    (setq nchars 3)
                    (setq state :escape-u) )) )
              ((:escape-x4)
                (if (eql nchars 4)
                    (progn
                      (unless (char= char #\u007D) (invalid-replacement pos))
                      (collect (code-char acc))
                      (setq nchars 0)
                      (setq state :normal) )
                  (let ((digit (digit-char-p char 16)))
                    (unless digit (invalid-replacement pos))
                    (setq acc (logior (ash acc 4) digit))
                    (incf nchars) )) )
              ((:ref)
                (setq acc (digit-char-p char))
                (cond
                  (acc (setq state :ref-nth))
                  ((alpha-char-p char)
                    (setf (fill-pointer strbuf) 0)
                    (decf pos)
                    (setq state :ref-name) )
                  (t (invalid-replacement pos)) ) )
              ((:ref-name)
                (if (eql char #\u007D)
                    (let ((name (intern strbuf :keyword)))
                      (collect name)
                      (setq state :normal) )
                  (progn
                    (setq char (regex-name-char-p char))
                    (vector-push-extend char strbuf) )) )
              ((:ref-nth)
                (if (eql char #\u007D)
                    (progn
                      (collect acc)
                      (setq state :normal) )
                  (let ((digit (digit-char-p char)))
                    (when (null char) (invalid-replacement pos))
                    (setq acc (+ (* acc 10) digit)) ) ) )) )
        finally
          (case state
            ((:normal)
              (when (plusp nchars)
                (collect (subseq string (- pos nchars) pos)) ) )
            ((:dollar)
              (invalid-replacement pos) )
            ((:capture)
              (collect acc) )
            (otherwise
              (invalid-replacement pos) ))) ) ) ) )


;;;; regex-ensure-group-index
;
(defun regex-ensure-group-index (match name-or-nth)
    (declare (type regex-match match))
    (declare (values (or sequence-index null)))
  (let ((nth
          (etypecase name-or-nth
            (integer name-or-nth)
            (keyword
              (loop
                with regex    = (slot-value match 'regex)
                with name-vec = (slot-value regex 'name-vector)
                for nth below (length name-vec)
                for name = (svref name-vec nth)
                  when (eq name name-or-nth) return nth ) )) ))
    (when (<= 0 nth (1- (length (slot-value match 'start-vector))))
      nth ) ) )


;;;; regex-eval-replacement-backward
;;;
;;; Called by:
;;;     regex-replace
;;; Description:
;;;  Evaluates replacement list with match.
;
(defun regex-eval-replacement-backward (elts match preserve-case s)
    (declare (type list elts))
    (declare (type regex-match match))
    (declare (type stream))
    (declare (values unspecified))
  (let ((case (and preserve-case (regex-string-case (match-string match)))))
  (labels (
    ;; emit-string
    (emit-string (string &optional (start 0) end)
      (write-reverse-case-string string case s start end)

      (when (and (eq case :capitalized) (string/= string ""))
        (setq case :downcase) ) )
    )
    ;;
    (dolist (elt elts)
      (etypecase elt
        (string    (emit-string elt))
        (character (write-char elt s))
        (fixnum
          (let ((capture (match-string match elt)))
            (when capture (emit-string capture)) ) )
        (keyword
          (let ((capture (match-string match elt)))
            (when capture (emit-string capture)) ) )
        (symbol
          (ecase elt
            ((match-string)
              (emit-string (match-string match)) )
            ((match-before)
              (emit-string (match-before match)) )
            ((match-after)
              (emit-string (match-after  match)) )
            ((match-target)
              (emit-string (match-target match)
                  (match-target-start match)
                  (match-target-end match) ) )
            ((+)
              (loop
                for nth downfrom (1- (match-group-count match)) to 1
                for match-start = (match-start match nth) do
                  (when match-start
                    (emit-string (match-target match)
                      match-start
                      (match-end match nth) ))) )) )) ) ) ) )


;;;; regex-eval-replacement-forward
;;;
;;; Called by:
;;;     regex-replace
;;; Description:
;;;  Evaluates replacement list with match.
;
(defun regex-eval-replacement-forward (elts match preserve-case s)
    (declare (type list elts))
    (declare (type regex-match match))
    (declare (type stream))
    (declare (values unspecified))
  (let ((case (and preserve-case (regex-string-case (match-string match)))))
  (labels (
    ;; emit-string
    (emit-string (string &optional (start 0) end)
      (write-case-string string case s start end)

      (when (and (eq case :capitalized) (string/= string ""))
        (setq case :downcase) ) )
    )
    ;;
    (dolist (elt elts)
      (etypecase elt
        (string (emit-string elt))
        (character
          (ecase case
            ((nil)      (write-char elt s))
            ((:downcase)   (write-char (char-downcase elt) s))
            ((:upcase)   (write-char (char-upcase elt) s))
            ((:capitalized)
              (write-char (char-upcase elt) s)
              (setq case :downcase) )) )
        (fixnum
          (let ((capture (match-string match elt)))
            (when capture (emit-string capture)) ) )
        (keyword
          (let ((capture (match-string match elt)))
            (when capture (emit-string capture)) ) )
        (symbol
          (ecase elt
            ((match-string) (emit-string (match-string match)))
            ((match-before) (emit-string (match-before match)))
            ((match-after)  (emit-string (match-after  match)))
            ((match-target)
              (emit-string (match-target match)
                  (match-target-start match)
                  (match-target-end match) ) )
            ((+)
              (loop
                for nth downfrom (1- (match-group-count match)) to 1
                for match-start = (match-start match nth) do
                  (when match-start
                    (emit-string (match-target match)
                      match-start
                      (match-end match nth) )
                    (return) )) )) )) ) ) ) )


;;;; regex-make-match
;
(defun regex-make-match (pattern string start end &rest options)
    (declare (type (or string regex regex-match) pattern))
    (declare (type simple-string string))
    (declare (type sequence-index start))
    (declare (type sequence-index key))
    (declare (dynamic-extent options))
    (declare (values regex-match))
  (multiple-value-bind (string start end) (string-data string start end)
  (labels (
    ;; make-match
    (make-match (regex)
      (let ((capture-count (1+ (length (slot-value regex 'name-vector))))
            (from-end (regex-backward-p regex)) )
        (make-instance 'regex-match
            :regex          regex
            :string         string
            :start          start
            :end            end
            :position       (if from-end end start)
            :start-vector
              (make-array capture-count :initial-element nil)
            :end-vector
              (make-array capture-count :initial-element nil) ) ) )
    )
    ;;
    (etypecase pattern
      (regex  (make-match pattern))
      (string (make-match (apply #'compile-regex pattern options)))
      (regex-match
        (let* ((match pattern)
               (regex    (slot-value match 'regex))
               (from-end (regex-backward-p regex)) )
          (setf (slot-value match 'string)   string)
          (setf (slot-value match 'start)    start)
          (setf (slot-value match 'position) (if from-end end start))
          (setf (slot-value match 'end)      end)
          (fill (slot-value match 'start-vector) nil)
          (fill (slot-value match 'end-vector) nil)
          match ) )) ) ) )


;;;; regex-get-context
;
(defun regex-get-context ()
  (or *regex-context*
      (setq *regex-context* (make-instance 'regex-context)) ) )


;;;; regex-name-char-p
;;;
;;; Description:
;;;  Returns case converted character if it is valid for capture name,
;;;  otherwise returns false.
;;;
;;; Called by:
;;;     (:internal parse-regex get-token/capname)
;;;     regex-compile-replacement
;
(defun regex-name-char-p (char)
  (cond
    ((get-macro-character char) nil)
    ((eql (char-category char) unicode:category-space-separator)
      nil )
    (t
      (ecase (readtable-case *readtable*)
        ((:upcase)   (char-upcase char))
        ((:downcase) (char-downcase char))
        ((:preserve) char)
        ((:invert)
          (if (upper-case-p char)
              (char-downcase char)
            (char-upcase char) ) )) )) )


;;;; regex-string-case
;;;
;;; Description:
;;;  Returns case of specified string as one of NIL, :CAPITALIZED, :DOWNCASE,
;;;  or :UPCASE. We ignore non-case characters to determine string case.
;
(defun regex-string-case (string)
    (declare (type string string))
    (declare (values (member nil :capitalized :downcase :upcase)))
  (labels (
    ;; lower-case-string-p
    (lower-case-string-p (string start end)
      (loop
        for pos from start below end
        for char = (schar string pos) do
          (when (both-case-p char)
            (unless (lower-case-p char) (return nil)) )
        finally (return :downcase) ) )

    ;; upper-case-string-p
    (upper-case-string-p (string start end)
      (loop
        for pos from start below end
        for char = (schar string pos) do
          (when (both-case-p char)
            (unless (upper-case-p char) (return nil)) )
        finally (return :upcase) ) )
    )
    ;;
    (multiple-value-bind (string start end) (string-data string)
    (cond
      ((eql start end) nil)
      ((eql (1+ start) end)
        (let ((char (char string 0)))
          (cond
            ((lower-case-p char) :downcase)
            ((upper-case-p char) :upcase) ) ) )
      ((lower-case-p (schar string 0))
        (lower-case-string-p string (1+ start) end) )
      ((upper-case-p (schar string 0))
        (let ((char (schar string 1)))
          (cond
            ((lower-case-p char)
              (when (lower-case-string-p string (+ start 2) end)
                :capitalized ) )
            ((upper-case-p char)
              (upper-case-string-p string (+ start 2) end) )) ) )
      (t
        (loop
          for pos from start below end
          for char = (schar string end) do
            (cond
              ((lower-case-p char)
                (return (lower-case-string-p string pos end)) )
              ((upper-case-p char)
                (return (upper-case-string-p string pos end)) ))) )) ) ) )


;;;; write-case-string
;
(defun write-case-string (string case stream &optional (start 0) end)
    (declare (type string string))
    (declare (type (member nil :capitalized :downcase :upcase) case))
    (declare (type stream))
  (multiple-value-bind (string start end) (string-data string start end)
  (labels (
    ;; write-downcase-string
    (write-downcase-string (start end)
      (loop
        for pos from start below end
        for char = (schar string pos) do
          (write-char (char-downcase char) stream) ) )

    ;; write-upcase-string
    (write-upcase-string (start end)
      (loop
        for pos from start below end
        for char = (schar string pos) do
          (write-char (char-upcase char) stream) ) )
    )
    ;;
    (ecase case
      ((nil)
        (write-string string stream :start start :end end) )
      ((:capitalized)
        (write-char (char-upcase (char string 0)) stream)
        (write-downcase-string (1+ start) end) )
      ((:downcase)
        (write-downcase-string start end) )
      ((:upcase)
        (write-upcase-string start end) )) ) ) )


;;;; write-reverse-case-string
;
(defun write-reverse-case-string (string case stream &optional (start 0) end)
    (declare (type string string))
    (declare (type (member nil :capitalized :downcase :upcase) case))
    (declare (type stream))
  (multiple-value-bind (string start end) (string-data string start end)
  (labels (
    ;; write-downcase-string
    (write-downcase-string (start end)
      (loop
        for pos from (1- end) downto start
        for char = (schar string pos) do
          (write-char (char-downcase char) stream) ) )

    ;; write-upcase-string
    (write-upcase-string (start end)
      (loop
        for pos from (1- end) downto start
        for char = (schar string pos) do
          (write-char (char-upcase char) stream) ) )
    )
    ;;
    (unless (eql start end)
      (ecase case
        ((nil)
          (write-reverse-string string stream :start start :end end) )
        ((:capitalized)
          (write-downcase-string (1+ start) end)
          (write-char (char-upcase (char string 0)) stream) )
        ((:downcase)
          (write-downcase-string start end) )
        ((:upcase)
          (write-upcase-string start end) ))) ) ) )


;;;; write-reverse-string
;
(defun write-reverse-string (string stream &key (start 0) end)
    (declare (type string string))
    (declare (type stream stream))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
    (declare (values string))
  (multiple-value-bind (string start end) (string-data string start end)
    (loop
      for pos from (1- end) downto start
      for char = (schar string pos) do
        (write-char char stream) ) )
  string )
