;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - 16 Strings
;;; lisp/runtime/r16-string.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r16-string.lisp#3 $
;;;
;;; Description:
;;;  This fils contains following functions:
;;;     nstring-capitalize-1
;;;     %string-capitalize      internal
;;;     %string-capitalize-1    internal    for ~@(...~)
;;;     string-case             internal
;;;     %string-downcase        internal
;;;     %string-upcase          internal
;;;     string-capitalize-1
;;;
;;;     char                    16.2.6
;;;     make-string             16.2.12
;;;     nstring-capitalize      16.2.8
;;;     nstring-downcase        16.2.8
;;;     nstring-upcase          16.2.8
;;;     schar                   16.2.6  boot
;;;     (setf schar)            16.2.6  boot
;;;     simple-string-p         16.2.5  boot
;;;     string                  16.2.7
;;;     string=                 16.2.10
;;;     string/=                16.2.10
;;;     string<                 16.2.10
;;;     string<=                16.2.10
;;;     string>                 16.2.10
;;;     string>=                16.2.10
;;;     string-capitalize       16.2.8
;;;     string-downcase         16.2.8
;;;     string-equal            16.2.10
;;;     string-greaterp         16.2.10
;;;     string-left-trim        16.2.9
;;;     string-lessp            16.2.10
;;;     string-not-equal        16.2.10
;;;     string-not-greaterp     16.2.10
;;;     string-not-lessp        16.2.10
;;;     stringp                 16.2.11 boot
;;;     string-right-trim       16.2.9
;;;     string-trim             16.2.9
;;;     string-upcase           16.2.8
;
(in-package :si)

;;;; %string-capitalize
;
(defun %string-capitalize (string start end)
    (declare (values string))
    (check-type string string)
  (let ((end (ensure-bounding-indexes string start end)))
    (multiple-value-bind (data start end)
        (string-data string start end)
      (loop
        with in-word-p = nil
        for index of-type sequence-index from start below end
        for char = (schar data index) do
          (setf (schar data index)
            (if in-word-p (char-downcase char) (char-upcase char)) )
          (setq in-word-p (alphanumericp char)) )
      string ) ) )


;;;; nstring-capitalize-1
;;;
;;; Description:
;;;  Capitalizes the first word and make dowcase after that.
;;;
;;; Note: This function is used for implementing: ~@(...~)
;
(defun %string-capitalize-1 (string start end)
    (check-type string string)
  (let ((end (ensure-bounding-indexes string start end)))
    (multiple-value-bind (data start end)
        (string-data string start end)
      (let ((index start))
        ;; Make the first alpaha numeric character upper.
        (loop
          (when (= index end) (return))
          (let ((char (schar data index)))
            (when (alphanumericp char)
              (setf (schar data index) (char-upcase char))
              (incf index)
              (return) )
            (incf index) ))

        ;; Make rest of all downcase.
        (loop
          (when (= index end) (return))
          (setf (schar data index) (char-downcase (schar data index)))
          (incf index) )

        string ) ) ) )


;;;; %string-downcase
;;;; %string-upcase
;
(macrolet (
  (define-string-op (op)
    (let ((fname   (intern (format nil "%STRING-~A" op)))
          (char-op (intern (format nil "CHAR-~A" op))) )
     `(defun ,fname (string start end)
           (declare (type string string))
           (declare (type sequence-index start))
           (declare (type sequence-end end))
           (declare (values string))
        (let ((end (ensure-bounding-indexes string start end)))
          (multiple-value-bind (string start end)
                (string-data string start end)
             (loop for pos from start below end do
               (setf (schar string pos) (,char-op (schar string pos))) ) )
           string ) ) ) )
  )
  ;;
  (define-string-op downcase)
  (define-string-op upcase) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;

;;;; 16.2.6 char
;
(defun cl:char (string index)
  (multiple-value-bind (string index) (string-data string index index)
    (schar string index) ) )


(defun (setf cl:char) (new-char string index)
  (multiple-value-bind (string index) (string-data string index index)
    (setf (schar string index) new-char) ) )


;;;; 16.2.8 nstring-capitalize
;;;; 16.2.8 nstring-downcase
;;;; 16.2.8 nstring-upcase
;;;; 16.2.8 string-capitalize
;;;; 16.2.8 string-downcase
;;;; 16.2.8 string-upcase
(macrolet (
  (define-converter (method)
    (let ((fn-sname (intern (format nil "STRING-~A"  method)))
          (fn-nname (intern (format nil "NSTRING-~A" method)))
          (fn-rname (intern (format nil "%STRING-~A" method))) )
     `(progn
        (defun ,fn-sname (string &key (start 0) end)
          (let ((string (copy-seq (string string))))
            (,fn-rname string start end)  ) )

        (defun ,fn-nname (string &key (start 0) end)
          (,fn-rname string start end) )) ) )
   )
   ;;
   (define-converter capitalize)
   (define-converter capitalize-1)
   (define-converter downcase)
   (define-converter upcase) )


;;;; 16.2.7 string
(defun cl:string (x)
  (etypecase x
    (string x)
    (symbol (symbol-name x))
    (character (make-string 1 :initial-element x)) ) )


;;;; 16.2.10 string=
(defun cl:string= (string1 string2 &key (start1 0) (end1 nil)
                                        (start2 0) (end2 nil) )
  (let ((string1 (string string1))
        (string2 (string string2)) )
    (eql (string-cs-compare string1 string2 start1 end1 start2 end2) 0) ) )


;;;; 16.2.10 string-equal
(defun cl:string-equal (string1 string2 &key (start1 0) (end1 nil)
                                             (start2 0) (end2 nil) )
  (let ((string1 (string string1))
        (string2 (string string2)) )
    (eql (string-ci-compare string1 string2 start1 end1 start2 end2) 0) ) )


;;;; 16.2.10 string/=
;;;; 16.2.10 string<
;;;; 16.2.10 string<=
;;;; 16.2.10 string>
;;;; 16.2.10 string>=
;;;; 16.2.10 string-equal
;;;; 16.2.10 string-greaterp
;;;; 16.2.10 string-lessp
;;;; 16.2.10 string-not-greaterp
;;;; 16.2.10 string-not-lessp
(macrolet (
  (define-compare (op cs &optional (suffix op))
    (let ((name    (intern (format nil "STRING~A" suffix)))
          (compare (intern (format nil "STRING-~A-COMPARE" cs))) )
      `(defun ,name (string1 string2 &key (start1 0) (end1 nil)
                                          (start2 0) (end2 nil) )
         (let ((string1 (string string1))
               (string2 (string string2)) )
           (multiple-value-bind (diff mismatch-index)
              (,compare string1 string2 start1 end1 start2 end2)
             (if (,op diff 0) mismatch-index nil) ) ) ) ) )
  )
  ;;
  (define-compare /= cs)
  (define-compare <  cs)
  (define-compare <= cs)
  (define-compare >  cs)
  (define-compare >= cs)

  (define-compare /= ci -not-equal)
  (define-compare <  ci -lessp)
  (define-compare <= ci -not-greaterp)
  (define-compare >  ci -greaterp)
  (define-compare >= ci -not-lessp)
) ; macrolet


;;;; 16.2.9 string-left-trim
;;;
;
(defun cl:string-left-trim (chars string)
  (let ((string (string string)))
    (multiple-value-bind (sstring start end) 
            (string-data string 0 (length string))
        (declare (type simple-string   sstring))
        (declare (type sequence-index  start))
        (declare (type sequence-index  end))
      (loop
        for index of-type sequence-index from start below end
        while (position (schar sstring index) chars :test #'char=)
        finally
          (unless (eql index start)
            (setq string (subseq sstring index end)) ))
      string ) ) )


;;;; 16.2.9 string-right-trim
;;;
;
(defun cl:string-right-trim (chars string)
  (let ((string (string string)))
    (multiple-value-bind (sstring start end)
            (string-data string 0 (length string))
        (declare (type simple-string sstring))
        (declare (type sequence-index  start))
        (declare (type sequence-index  end))
      (let ((index end))
          (declare (type sequence-index index))
        (loop
          (when (= index start) (return))
          (decf index)
          (unless (position (schar sstring index) chars :test #'char=)
            (incf index)
            (return) ))
        (unless (eql end index)
          (setq string (subseq sstring start index)) ) )
     string ) ) )


;;;; 16.2.9 string-trim
;
(defun cl:string-trim (chars string)
  (let ((string (string string)))
    (multiple-value-bind (sstring start end)
            (string-data string 0 (length string))
        (declare (type simple-string sstring))
        (declare (type sequence-index  start))
        (declare (type sequence-index  end))
      (let* ((sindex
               (loop for index from start below end
                     while (position (schar sstring index) chars :test #'char=)
                     finally (return index) ) )
             (eindex
               (loop with index = end
                     while (> index sindex)
                     do (decf index)
                        (unless (position (schar sstring index) chars
                                :test #'char= )
                          (return (1+ index)) )
                     finally (return index) ) ))
        (unless (and (eql start sindex) (eql end eindex))
          (setq string (subseq sstring sindex eindex)) )
        string ) ) ) )
