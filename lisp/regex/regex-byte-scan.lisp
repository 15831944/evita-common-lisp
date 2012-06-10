;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - byte-code scanner compiler
;;; lisp/regex/regex-byte-scan.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-byte-scan.lisp#2 $
;;;
;;; Description:
;;;  This file contains scanner compiler for byte-code regex.
;
(in-package :si)

;;;; regex-byte-compile-scanner
;;;
;;; Description:
;;;  Returns scanner function for specified regex.
;;;
;;;  There are four types of scanner functions:
;;;     1. Character range scanner
;;;     2. Bit-vector scanner
;;;     3. One character scanner
;;;     4. String scanner
;
(defun regex-byte-compile-scanner (flags hint)
    (declare (type fixnum flags))
    (declare (type regex-hint hint))
    (declare (values t))
  (let ((from-end (not (zerop (logand flags REGEX-FLAG-SCAN-BACKWARD)))))
  (macrolet (
    (scanner-template (name fn)
      (let ((fwd-name (intern (format nil "FORWARD-~A" name)))
            (bck-name (intern (format nil "BACKWARD-~A" name))) )
       `(if from-end
          (lambda (string start end)
              (declare (lambda-name ,bck-name))
              (declare (type simple-string start))
              (declare (type sequence-index start end))
              (declare (values (or sequence-index null)))
            (loop
              for pos downfrom (- end offset 1) to start
              for char = (schar string pos)
               when ,fn return (+ pos offset 1) ) )
          (lambda (string start end)
              (declare (lambda-name ,fwd-name))
              (declare (type simple-string start))
              (declare (type sequence-index start end))
              (declare (values (or sequence-index null)))
            (loop
              for pos from (+ start offset) below end
              for char = (schar string pos)
               when ,fn return (- pos offset) ) )) ) )
    )
  (labels (
    ;; compile-bit-vector
    (compile-bit-vector (offset which min-code bitvec)
      (let ((max-index-1 (1- (length bitvec))))
        (ecase which
          ((=)
            (scanner-template scan-bitvec
                (let ((index (- (char-code char) min-code)))
                    (and (<= 0 index max-index-1)
                         (eql (sbit bitvec index) 1) ) ) ) )
          ((/=)
            (scanner-template scan-bitvec
                (let ((index (- (char-code char) min-code)))
                    (and (<= 0 index max-index-1)
                         (eql (sbit bitvec index) 0) ) ) ) )) ) )

    ;; compile-char=
    (compile-char= (offset datum)
        (scanner-template scan-char= (char= char datum)) )

    ;; compile-char-equal
    (compile-char-equal (offset datum)
      (scanner-template scan-char-equal (char-equal char datum)) )

    ;; compile-not
    (compile-not (offset subexpr)
      (typecase subexpr
        (character
          (let ((datum subexpr))
            (scanner-template scan-char/= (char/= char datum)) ) )
        (fixnum
          (let ((datum (code-char subexpr)))
            (scanner-template scan-char-not-equal
                (char-not-equal char datum) ) ) )
        (cons
          (ecase (first subexpr)
            ((category) nil)
            ((:range)
              (destructuring-bind (min max) (rest subexpr)
                (etypecase min
                  (character
                    (scanner-template
                        scan-not-char<= (not (char<= min char max)) ) )
                  (fixnum
                    (scanner-template
                        scan-greaterp (char-greaterp min char max) ) )) ) )
            ((union) nil) ) )) )

    ;; compile-range
    (compile-range (offset min max)
      (etypecase min
        (character
          (scanner-template scan-char<=
              (char<= min char max) ) )
        (fixnum
          (setq min (code-char min))
          (setq max (code-char max))
          (scanner-template scan-char<=
              (char-not-greaterp min char max) ) )) )

    ;; compile-string=
    (compile-string= (offset pattern ignore-case)
      (let ((matcher
                (make-string-search-function pattern
                        :ignore-case ignore-case
                        :from-end    from-end ) ))
        (if from-end
          (let ((patlen (length pattern)))
            (lambda (string start end)
                (declare (lambda-name backward-scan-string=))
                (declare (type simple-string string))
                (declare (type sequence-index start end))
                (declare (values (or sequence-index null)))
              (let ((pos (funcall matcher string start (- end offset))))
                (and pos (+ pos offset patlen)) ) ) )
          (lambda (string start end)
              (declare (lambda-name foward-scan-string=))
              (declare (type simple-string string))
              (declare (type sequence-index start end))
              (declare (values (or sequence-index null)))
            (let ((pos (funcall matcher string (+ start offset) end)))
              (and pos (- pos offset)) ) )) ) )

    ;; make-scanner
    (make-scanner (offset expr)
        (declare (values (or function null)))
      (etypecase expr
        (character
          (let ((pat-char expr))
            (compile-char= offset pat-char) ) )
        (fixnum
          (let ((pat-char (code-char expr)))
            (if (both-case-p pat-char)
                (compile-char-equal offset pat-char)
              (compile-char= offset pat-char) ) ) )
        (string
          (let ((string expr))
            (if (eql (length string) 1)
                (make-scanner offset (char string 0))
              (compile-string= offset string nil) ) ) )
        (cons
          (ecase (first expr)
            ((bit-vector)
             (destructuring-bind (which min-code bitvec) (rest expr)
                (compile-bit-vector offset which min-code bitvec) ) )
            ((not)
              (compile-not offset (second expr)) )
            ((:range)
              (destructuring-bind (min max) (rest expr)
                (compile-range offset min max) ) )
            ((string-equal)
              (let ((string (second expr)))
                (cond
                  ((notany #'both-case-p string)
                    (make-scanner offset string) )
                  ((eql (length string) 1)
                    (make-scanner offset (char-code (char string 0))) )
                  (t
                    (compile-string= offset string t) )) ) )
            ((union)
              nil )) )) )
    )
    ;;
    (when hint (make-scanner (svref hint 2) (svref hint 3))) ) ) ) )
