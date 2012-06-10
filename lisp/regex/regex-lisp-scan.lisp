;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - lisp scanner
;;; lisp/regex/regex-lisp-scan.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-lisp-scan.lisp#2 $
;;;
;;; Description:
;;;  This file contains scanner compiler into lisp.
;
(in-package :si)

;;;; regex-lisp-compile-scanner
;
(defun regex-lisp-compile-scanner (expr flags hint)
    (declare (type regex-expr expr))
    (declare (type fixnum flags))
    (declare (type regex-hint hint))
    (declare (values form sequence-index fixnum regex-hint))
  (multiple-value-bind (min-length max-length cap-p det-p)
      (regex-study-length expr)
      (declare (ignore max-length cap-p det-p))
  (labels (
    ;; compose
    (compose ()
      (ecase (logand flags REGEX-FLAG-SCAN-MASK)
        ((#.REGEX-FLAG-SCAN-ALL.F)
          (if hint (compose-fun.f) (compose-all.f)) )
        ((#.REGEX-FLAG-SCAN-BOS.F)  (compose-bos.f))
        ((#.REGEX-FLAG-SCAN-EOS.F)  (compose-eos.f))
        ((#.REGEX-FLAG-SCAN-MBOL.F) (compose-mbol.f))
        ((#.REGEX-FLAG-SCAN-MEOL.F) (compose-meol.f))
        ((#.REGEX-FLAG-SCAN-POS.F)  (compose-pos))
        ((#.REGEX-FLAG-SCAN-SEOL.F) (compose-seol.f))

        ((#.REGEX-FLAG-SCAN-ALL.B)
          (if hint (compose-fun.b) (compose-all.b)) )
        ((#.REGEX-FLAG-SCAN-BOS.B)  (compose-bos.b))
        ((#.REGEX-FLAG-SCAN-EOS.B)  (compose-eos.b))
        ((#.REGEX-FLAG-SCAN-MBOL.B) (compose-mbol.b))
        ((#.REGEX-FLAG-SCAN-MEOL.B) (compose-meol.b))
        ((#.REGEX-FLAG-SCAN-POS.B)  (compose-pos))
        ((#.REGEX-FLAG-SCAN-SEOL.B) (compose-seol.b)) ) )

    ;; compose/test
    (compose/test (expr)
      (etypecase expr
        (character
          (let ((pat-char expr))
            `(char= char ,pat-char) ) )
        (fixnum
          (let ((pat-char (code-char expr)))
            (if (both-case-p pat-char)
                `(char-equal char ,pat-char)
                `(char= char ,pat-char) ) ) )
        (string
          (let ((string expr))
            (when (eql (length string) 1)
              (compose/test (char string 0)) ) ) )
        (cons
          (ecase (first expr)
            ((bit-vector)
              (destructuring-bind (which min-code bitvec) (rest expr)
                `(let ((index (- (char-code char) ,min-code)))
                   (and (<= 0 index ,(1- (length bitvec)))
                        (,which (sbit ,bitvec index) 1) ) ) ) )

            ((category)
              (destructuring-bind (min &optional max) (rest expr)
                (if max
                    `(<= ,min (char-category char) ,max)
                  `(eql (char-category char) ,min) ) ) )
            ((not)
              (let ((form (compose/test (second expr))))
                (when form
                  (if (eq (first form) 'not)
                      (second form)
                    `(not ,form) ) ) ) )
            ((:range)
              (destructuring-bind (min max) (rest expr)
                (etypecase min
                  (character `(char<= ,min char ,max))
                  (fixnum
                    (let ((min-char (code-char min))
                          (max-char (code-char max)) )
                      `(char-not-greaterp ,min-char char ,max-char) ) )) ) )
            ((string-equal)
              (let ((string (second expr)))
                (when (eql (length string) 1)
                  (let ((char (char string 0)))
                    (if (both-case-p char)
                        (compose/test (char-code char))
                      (compose/test char) ) )) ) )
            ((union)
              (loop
                for subexpr in (rest expr)
                for form = (compose/test subexpr)
                collect form into forms
                finally
                  (assert forms)
                  (assert (not (find nil forms)))
                  (if (rest forms)
                      (return `(or ,@forms))
                    (return (first forms)) )) )) )) )

    ;; compose/fun
    (compose/fun (offset expr from-end)
      (let ((test-form (compose/test expr)))
        (cond
          (test-form
            (compose/fun/1 test-form offset from-end) )
          ((stringp expr)
            (compose/fun/string expr offset from-end nil) )
          ((and (consp expr) (eq (first expr) 'string-equal))
            (let ((string (second expr)))
              (if (notany #'both-case-p string)
                  (compose/fun/string string offset from-end nil)
                (compose/fun/string string offset from-end t) ) ) )) ) )

    ;; compose/fun/1
    (compose/fun/1 (fn offset from-end)
      (if from-end
         `(loop
            for pos downfrom (- scan-start ,offset 1) to string-start
            for char = (schar string pos) do
              (when ,fn
                (let ((match-start (execute (+ pos ,offset 1))))
                  (when match-start (return match-start)) )))
        `(loop
            for pos from (+ string-start ,offset) below string-end
            for char = (schar string pos) do
              (when ,fn
                (let ((match-end (execute (- pos ,offset))))
                  (when match-end (return match-end)) ))) ) )

    ;; compose/fun/string
    ;;  Note: make-string-search-function for backward seach returns start of
    ;;  pattern string in subject string. For example, to search pattern "foo"
    ;;  in subject string "012foo678", function returns 3 instead of 6.
    (compose/fun/string (patstr offset from-end ignore-case)
      (if from-end
         `(loop
            for scan-pos from (- scan-start ,offset)
                       downto (+ string-start ,min-length)
            for match-start =
              (funcall (make-string-search-function ,patstr
                            :ignore-case ,ignore-case
                            :from-end t )
                string string-start scan-pos )
            while match-start do
              (let ((match-end
                      (execute (- match-start ,(- offset (length patstr)))) ))
                (when match-end (return match-end)) ))
         `(loop
            for scan-pos from (+ scan-start ,offset)
                           to (- string-end ,min-length)
            for match-start =
              (funcall (make-string-search-function ,patstr
                            :ignore-case ,ignore-case )
                string scan-pos string-end )
            while match-start do
              (let ((match-end (execute (- match-start ,offset))))
                (when match-end (return match-end)) )) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Scanner for anchor
    
    ;; compose-all.b
    (compose-all.b ()
     `(loop
        for match-end downfrom scan-start to (+ string-start ,min-length)
        for match-start = (execute match-end) do
          (when match-start (return match-start)) ) )

    ;; compose-all.f
    (compose-all.f ()
     `(loop
        for match-start from scan-start to (- string-end ,min-length)
        for match-end = (execute match-start) do
          (when match-end (return match-end)) ) )

    ;; compose-bos.b
    (compose-bos.b ()
     '(when (>= scan-start string-start)
        (execute string-start) ) )

    ;; compose-bos.f
    (compose-bos.f ()
     '(when (eql scan-start string-start)
        (execute string-start) ) )

    ;; compose-eos.b
    (compose-eos.b ()
     '(when (eql scan-start string-end)
        (execute string-end) ) )

    ;; compose-eos.f
    (compose-eos.f ()
     '(when (<= scan-start string-end)
        (execute string-end) ) )

    ;; compose-fun.b
    (compose-fun.b ()
      (compose/fun (svref hint 2) (svref hint 3) t) )

    ;; compose-fun
    (compose-fun.f ()
      (compose/fun (svref hint 2) (svref hint 3) nil) )

    ;; compose-mbol.b
    ;;  1. Match at string-start
    ;;  2. Match at at end when string[end-1] = \n
    (compose-mbol.b ()
     '(block scanner
        (when (eql scan-start string-start)
          (when (execute scan-start) (return-from scanner t)) )

        (loop for end from scan-start above string-start do
          (when (eql (schar string (1- end)) #\Newline)
            (when (execute end) (return-from scanner t)) ))) )

    ;; compose-mbol.f
    ;;  Tries at each start of line (= next of newline) or
    ;;  start of string.
    (compose-mbol.f ()
     '(block scanner
        (let ((pos scan-start))
          (when (eql pos string-start)
            (when (execute pos) (return-from scanner t))
            (incf pos) )

          (loop for start from pos below string-end do
            (when (eql (schar string (1- start)) #\Newline)
              (when (execute start) (return-from scanner t)) )) )) )

    ;; compose-meol.f
    ;;  1. Match at string-end
    ;;  2. Match at pos wher string[pos] = \n
    (compose-meol.f ()
     '(block scanner
        (when (eql scan-start string-end)
          (when (execute scan-start) (return-from scanner t)) )

        (loop for start from scan-start below string-end do
          (when (eql (schar string start) #\Newline)
            (when (execute start) (return-from scanner t)) ))) )

    ;; compose-meol.b
    ;;  1. Match at string-end
    ;;  2. Match at pos when string[pos] = \n
    (compose-meol.b ()
     '(block scanner
        (when (eql scan-start string-end)
          (when (execute scan-start) (return-from scanner t)) )

        (loop for end from pos above string-start do
          (when (eql (schar string (1- end)) #\Newline)
            (when (execute end) (return-from scanner t)) ))) )

    ;; compose-pos
    (compose-pos ()
      '(execute scan-start) )

    ;; compose-seol.f
    ;;  1. Match at string-end
    ;;  2. Match at string-end - 1 when string(string-end - 1) = \n
    (compose-seol.f ()
     '(cond
        ((execute string-end) t)
        ((eql (schar string (1- string-end)) #\Newline)
          (execute (1- string-end)) )) )

    ;; compose-seol.b
    ;;  1. Match at string-end
    (compose-seol.b ()
     '(when (eql scan-start string-end)
        (execute string-end) ) )
    )
    ;;
    (let ((scanner-form (compose)))
      (when (null scanner-form)
        (setq flags (logandc2 flags REGEX-FLAG-SCAN-MASK))
        (if (zerop (logand flags REGEX-FLAG-SCAN-BACKWARD))
            (progn
              (setq scanner-form (compose-all.f))
              (setq flags (logior flags REGEX-FLAG-SCAN-ALL.F)) )
            (progn
              (setq scanner-form (compose-all.b))
              (setq flags (logior flags REGEX-FLAG-SCAN-ALL.B)) ))
        (setq hint nil) )

      (unless (eql min-length 0)
        (setq scanner-form
          `(when (>= (- string-end string-start) ,min-length)
             ,scanner-form )) )

      (values scanner-form min-length flags hint) ) ) ) )
