;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - 13 Characters
;;; lisp/runtime/r13-char.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r13-char.lisp#5 $
;;;
;;; Description:
;;;  This file contains following functions:
;;;     whitespace-char-p   internal
;;;
;;;     alpha-char-p        13.2.8      intrinsic
;;;     alphanumericp       13.2.8      intrinsic
;;;     both-case-p         13.2.15
;;;     char=               13.2.5
;;;     char/=              13.2.5
;;;     char<               13.2.5
;;;     char<=              13.2.5
;;;     char>               13.2.5
;;;     char>=              13.2.5
;;;     char-equal          13.2.5
;;;     char-not-equal      13.2.5
;;;     char-lessp          13.2.5
;;;     char-not-greaterp   13.2.5
;;;     char-greaterp       13.2.5
;;;     char-not-lessp      13.2.5
;;;     character           13.2.6
;;;     characterp          13.2.7      boot
;;;     char-code           13.2.16
;;;     char-downcase       13.2.14     intrinsic
;;;     char-int            13.2.17
;;;     char-name           13.2.20
;;;     char-upcase         13.2.14     intrinsic
;;;     code-char           13.2.18     intrinsic
;;;     digit-char          13.2.10
;;;     digit-char-p        13.2.11
;;;     graphic-char-p      13.2.12
;;;     name-char           13.2.21
;;;     lower-case-p        13.2.15     intrinsic
;;;     standard-char-p     13.2.13
;;;     upper-case-p        13.2.15     intrinsic
;

(in-package :si)

;;;; 13.2.5 char=
;;;; 13.2.5 char/=
;;;; 13.2.5 char<
(macrolet (
  ;; for char= and char-equal
  (define-= (name)
   `(defun ,name (char-1 &rest chars)
        (declare (type character char-1))
        (declare (dynamic-extent chars))
      (dolist (char-2 chars t)
        (unless (,(intern (format nil "~A/2" name)) char-1 char-2)
          (return nil) ) ) ) )

  ;; for char/= and char-not-equal
  ;;  -- all characters are not equal.
  (define-/= (name)
   `(defun ,name (char-1 &rest chars)
        (declare (type character char-1))
        (declare (dynamic-extent chars))
      (if (null chars)
          t
        (loop
          (when (null chars) (return t))
          (dolist (char-2 chars)
            (unless (,(intern (format nil "~A/2" name)) char-1 char-2)
              (return-from ,name nil) ) )
          (setq char-1 (pop chars)) ) ) ) )

  ;; for char<, char<=, char>, char>=, char-lessp, and so on.
  (define-<>(name)
   `(defun ,name (char-1 &rest chars)
        (declare (type character char-1))
        (declare (dynamic-extent chars))
      (if (null chars)
          t
        (dolist (char-2 chars t)
          (unless (,(intern (format nil "~A/2" name)) char-1 char-2)
            (return nil) )
          (setq char-1 char-2) )) ) )
  )
  ;;
  (define-=  cl:char=)  (define-=  cl:char-equal)
  (define-/= cl:char/=) (define-/= cl:char-not-equal)

  (define-<> cl:char<)  (define-<> cl:char-lessp)
  (define-<> cl:char<=) (define-<> cl:char-not-greaterp)

  (define-<> cl:char>)  (define-<> cl:char-greaterp)
  (define-<> cl:char>=) (define-<> cl:char-not-lessp) )


;;;; 13.2.6 character
(defun cl:character (charspec)
  (typecase charspec
    (character  charspec)
    ((string 1) (char charspec 0))
    (symbol     (character (symbol-name charspec)))
    (otherwise
        (error 'type-error
            :datum charspec
            :expected-type 'character-designator ) )) )


;;;; 13.2.20 char-name
(defun cl:char-name (char)
    (declare (type character char))
  (or
    (gethash/eq char *char-name-table*)
    (let ((code (char-code char)))
      (if (<= #x21 code #x7F)
          (string char)
        (let ((name (make-string 5)))
          (setf (schar name 0) #\u)
          (setf (schar name 1) (digit-char (ldb (byte 4 12) code) 16))
          (setf (schar name 2) (digit-char (ldb (byte 4  8) code) 16))
          (setf (schar name 3) (digit-char (ldb (byte 4  4) code) 16))
          (setf (schar name 4) (digit-char (ldb (byte 4  0) code) 16))
          name )) )) )


;;;; 13.2.10 digit-char
(defun cl:digit-char (weight &optional (radix 10))
    (declare (type unsigned-byte weight))
    (declare (type (integer 2 36) radix))
  (cond
   ((minusp weight) nil)
   ((not (< weight radix)) nil)
   ((< weight 10) (code-char (+ 48 weight)))
   (t (code-char (+ 55 weight))) ) )


;;;; 13.2.11 digit-char-p 
(defun cl:digit-char-p (char &optional (radix 10))
    (declare (type character char))
    (declare (type (integer 2 36) radix))
  (let ((code (char-code char)))
    (cond
      ((<= radix 10)
        (if (<= 48 code (+ 47 radix)) (- code 48) nil) )
      ((<= 48 code 57)           (- code 48))
      ((<= 65 code (+ 54 radix)) (- code 55))
      ((<= 97 code (+ 86 radix)) (- code 87))
      (t nil) ) ) )


;;;; 13.2.21 name-char
;;; Description:
;;;  Supports well-know character names, e.g. Newline, Space, and so on,
;;;  one character and uXXXX.
;
(defun cl:name-char (name)
    (declare (values (or character null)))
  (let* ((name (string name))
         (char (gethash name *name-char-table*)) )
    (cond
      (char char)
      ((eql (length name) 1) (char name 0))
      ((and (eql (length name) 5) (char-equal #\u (char name 0)))
        (code-char (parse-integer name :start 1 :radix 16)) )
      ((and (eql (length name) 9)
            (<= #x3F (char-code (char-upcase (char name 8))) #x5F)
            (or (eql (char name 7) #\+) (eql (char name 7) #\-))
            (string-equal "Control" name :end2 7) )
        (code-char (logxor (char-code (char-upcase (char name 8)))
                           #x40 )) )) ) )
