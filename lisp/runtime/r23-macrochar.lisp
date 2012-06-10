;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 2 Syntax
;;; runtime/r02-syntax.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r23-macrochar.lisp#3 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;     Left-Parenthesis    2.4.1           native
;;;     Right-Parenthesis   2.4.2           native
;;;     Single-Quote        2.4.3           native
;;;     Semicolon           2.4.4           native
;;;     Double-Quote        2.4.5           native
;;;     Backquote(`)        2.4.6
;;;     Comma(,)            2.4.7
;;;     #<newline>          2.4.8.21
;;;     #<page>             2.4.8.21
;;;     #<return>           2.4.8.21
;;;     #<tab>              2.4.8.21
;;;     #<space>            2.4.8.21
;;;     ##                  2.4.8.16
;;;     #'                  2.4.8.2 
;;;     #(                  2.4.8.3
;;;     #)                  2.4.8.22
;;;     #+                  2.4.8.17
;;;     #-                  2.4.8.18
;;;     #*                  2.4.8.4
;;;     #.                  2.4.8.6
;;;     #\                  2.4.8.1
;;;     #:                  2.4.8.5
;;;     #=                  2.4.8.15
;;;     #A                  2.4.8.12
;;;     #B                  2.4.8.7
;;;     #C                  2.4.8,11
;;;     #O                  2.4.8.8
;;;     #P                  2.4.8.14
;;;     #R                  2.4.8.10
;;;     #S                  2.4.8.13
;;;     #X                  2.4.8.9         native
;;;     #|                  2.4.8.19        native
;
(in-package :si)

;;;; 2.4.5 Double-Quote reader
(defun |"-reader| (stream delimiter &aux (readtable *readtable*))
  (labels (
    ;; collect-chars
    (collect-chars ()
      (let ((tokens '())
            (length 0)
            (bufidx 0)
            (buflen 0)
            (buffer nil)
            (token  nil) )
        (loop for char = (fast-read-char stream nil) do
          (cond
            ((not char)                               ; end-of-file
              (reader-eof-error stream) )

            ((eql (get-char-syntax char readtable) 5)   ; single escape
              (setq char (fast-read-char stream nil))
              (unless char (reader-eof-error stream)) )

            ((eql delimiter char)                   ; delimiter
              (when (null tokens) (return ""))
              (setf (ref reader-token length token) bufidx)
              (let ((string (make-string length)))
                (loop for token in tokens
                      with index = length do
                        (decf index (ref reader-token length token))
                        (replace string (ref reader-token cstring token)
                                :start1 index )
                        (free-pooled-token token) )
                (return string) ) ))

          (when (eql buflen bufidx)
            (when token (setf (ref reader-token length token) buflen))
            (setq token (make-pooled-token))
            (push token tokens)
            (setq buffer (ref reader-token cstring token))
            (setq buflen (length buffer))
            (setq bufidx 0) )

          (setf (schar buffer bufidx) char)
          (incf length)
          (incf bufidx) ) ) )

    ;; discard-chars
    ;;   Discard characters enclosed in double-quote.
    (discard-chars ()
      (loop for char = (fast-read-char stream nil) do
        (cond
          ((null char)
            (reader-eof-error stream) )
          ;; BUGBUG: We should use symbolic name "single escape" instead
          ;; of "5".
          ((eql (get-char-syntax char readtable) 5)
            (fast-read-char stream t) )
          ((eq delimiter char)
            (return nil) ))) )
    )
    ;;
    ;; |"-reader|
    ;;
    (let ((*read-start-line-number* (stream-line-number stream)))
      (if *read-suppress*
          (discard-chars)
        (collect-chars) ) ) ) )


;;;; 2.4.3 Single-Quote(') reader
;
(defun |'-reader| (stream char)
    (declare (ignore char))
  (let ((form (read stream t nil t)))
    (if *read-suppress*
        nil
      (list 'quote form) ) ) )

;;;; 2.4.1 Left-Parenthesis reader
;
(defun |(-reader| (stream char)
    (declare (ignore char))
  (read-delimited-list #\u0029 stream t) )


;;;; 2.4.2 Right-Parenthesis reader
;
(defun |)-reader| (stream char)
  (simple-reader-error stream "Unmatched ~C" char) )


;;;; 2.4.7 Comma(,) reader
;
(defun |,-reader| (stream char)
    (declare (ignore char))
  (let ((char (fast-read-char stream t))
        operator
        object )
    (cond
      ((eql #\@ char)
        (setq operator 'unquote-splicing) )

      ((eql #\. char)
        (setq operator 'unquote-nsplicing) )

      (t
        (unread-char char stream)
        (setq operator 'unquote) ))

    (setq object (read stream t nil t))

    (cond
      (*read-suppress*       nil)
      (*read-backquote* (list operator object))
      (t
        (simple-reader-error stream
          "Comma(,) must be used with backquote."  ) )) ) )


;;;; 2.4.4 Semi-Colon(;) reader
;
(defun |;-reader| (stream char)
    (declare (ignore char))
  (loop for char = (fast-read-char stream nil)
        until (or (not char) (eql #\Newline char))
        finally (return (values)) ) )


;;;; 2.4.6 Backquote(`) reader
;
(defun |`-reader| (stream char)
    (declare (ignore char))
  (let ((*read-backquote* t))
    (let ((object (read stream t nil t)))
      (if *read-suppress*
          nil
        (list 'backquote object) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions For Sharpsign Dispatching Macro Character
;;;


;;;; invalid-dispatch-character
;
(defun invalid-dispatch-character (stream sub-char arg)
    (declare (ignore arg))
  (simple-reader-error stream
    "Invalid dispatch character #~:C" sub-char ) )


;;;; 2.4.16 Sharpsign Sharpsign
;;;
;;; Syntax:
;;;   #n#
;;;
;;; Description:
;;;  Top level |#=-reader| changes label.object cons to reference object.
;
(defun |##-reader| (stream sub-char arg)
  (cond
    (*read-suppress*
      nil )
    ((not arg)
      (reader-error-missing-infix-arg stream sub-char) )
    (t
      (let ((label.object (assoc arg *read-labels*)))
        (unless label.object
          (simple-reader-error stream
            "No such label: ~D" arg ))
        label.object ) )) )


;;;; 2.4.8.2 Sharpsign Single-Quote
;;;
;;; Syntax:
;;;     #'object    => (function object)

;;;; 2.4.8.4 Sharpsign Left-Parenthesis
;;;
;;; Syntax:
;;;     #[n](object...) => simple-vector
;
(defun |#(-reader| (stream sub-char arg)
    (declare (ignore sub-char))

  (when *read-suppress*
    (read-delimited-list #\u0029 stream t)
    (return-from |#(-reader| nil) )

  (let* ((contents (read-delimited-list #\u0029 stream t))
         (length   (proper-list-p contents)) )

    (unless length
      (simple-reader-error stream
        "Invalid vector contents: ~S" contents ))

    (cond
      ((not arg)
        (setq arg length) )
      ((< arg length)
        (simple-reader-error stream
          "Too many vector elements: ~S" contents )))

    (let ((vector (make-array length :initial-element nil))
          (nth 0) )
      (dolist (elt contents vector)
        (setf (svref vector nth) elt)
        (incf nth) ) ) ) )


;;;; 2.4.8.2 Sharpsign Single-Quote
;
(defun |#'-reader| (stream sub-char arg)
  (when arg (reader-error-extra-infix-arg stream sub-char arg))
  (let ((form (read stream t nil t)))
    (if *read-suppress*
        nil
      (list 'function form) ) ) )


;;;; 2.4.8.4 Sharpsign Asterisk
;;;
;;; Syntax:
;;;     #[n]*{bits}* => simple-bit-vector
;;;
;;; Description:
;;;  Reads "0" and "1".
;
(defun |#*-reader| (stream sub-char arg)
    (declare (ignore sub-char))
  (labels (
    (error-invalid-char (char)
      (simple-reader-error stream
        "Invalid character for #*: ~C" char ) )

    (error-no-element ()
      (simple-reader-error stream
        "#~D* requires at least one element." arg ) )
    )
    ;;
    ;; #*-reader
    ;;
    (let ((readtable *readtable*))
      (cond
        (*read-suppress*
          (discard-token nil stream) )
        (arg
          (loop for char = (fast-read-char stream nil)
                for index = 0 then (1+ index)
                with bitvec = (make-array arg :element-type 'bit)
                do
                  (when char
                    (ecase (get-char-syntax char readtable)
                      ((1))         ;  constituent
                      ((0 2 5 6)
                        (error-invalid-char char) )
                      ((3 4)        ; 3=terminating macro, 4=space
                        (unread-char char stream)
                        (setq char nil) )))

                  (when (not char)
                    (cond
                      ((= index arg)
                        (return bitvec) )
                      ((= index 0)
                        (error-no-element) )
                      (t
                        (fill bitvec (sbit bitvec (1- index))
                              :start index
                              :end   arg )
                        (return bitvec) )))

                  (let ((bit (digit-char-p char 2)))
                    (unless bit (error-invalid-char char))
                    (setf (sbit bitvec index) bit) )) )
        (t
          (loop for char = (fast-read-char stream nil)
                with bitvec = (make-array 100
                                          :element-type 'bit
                                          :fill-pointer 0
                                          :adjustable   t )
                do
                  (when char
                    (ecase (get-char-syntax char readtable)
                      (1)
                      ((0 2 5 6)
                        (error-invalid-char char) )
                      ((3 4)
                        (unread-char char stream)
                        (return (subseq bitvec 0 (fill-pointer bitvec))) )) )

                  (let ((bit (digit-char-p char 2)))
                    (unless bit (error-invalid-char char))
                    (vector-push-extend bit bitvec) )) )) ) ) )


;;;; 2.4.8.17 Sharpsign Plus
;;;; 2.4.8.18 Sharpsign Minus
;;;
;;; Syntax:
;;;     #+test expression
;;;     #-test expression
;
(labels (
  ;; check
  (check (stream expr)
    (cond
      ((symbolp expr)
        (not (null (member expr *features* :test #'eq))) )
      ((consp expr)
       (case (first expr)
         ((:or)
           (dolist (expr (rest expr) nil)
             (when (check stream expr) (return t)) ) )
         ((:and)
           (dolist (expr (rest expr) t)
             (unless (check stream expr) (return nil)) ) )
         ((:not)
           (not (check stream (second expr))) )
         (otherwise
           (invalid-feature-expr stream expr) )) )
      (t
        (invalid-feature-expr stream expr) )) )

  ;; conditional-read
  (conditional-read (stream cond)
    (if *read-suppress*
        (progn
          (read stream t nil t) ; feature
          (read stream t nil t) ; form
          (values) )
      (let ((test (let ((*package* #.(symbol-package :key)))
                    (read stream t nil t) )) )
        (if (eq (check stream test) cond)
            (read stream t nil t)
          (let ((*read-suppress* t))
            (read stream t nil t)
            (values) )) )) )

  ;; invalid-feature-expr
  (invalid-feature-expr (stream expr)
    (simple-reader-error stream
      "Invalid feature expression: ~S" expr ) )
  )
  ;;
  ;;
  (defun |#+-reader| (stream sub-char arg)
    (when arg (reader-error-extra-infix-arg stream sub-char arg))
    (conditional-read stream t) )

  (defun |#--reader| (stream sub-char arg)
    (when arg (reader-error-extra-infix-arg stream sub-char arg))
    (conditional-read stream nil) )
 ) ; labels


;;;; 2.4.8.6 Sharpsign Dot
;;;
;;; Syntax:
;;;     #.foo => (eval foo)
;;;
;
(defun |#.-reader| (stream sub-char arg)
  (when arg (reader-error-extra-infix-arg stream sub-char arg))
  (let ((form (read stream t nil t)))
    (unless *read-suppress*
      (if *read-eval*
          (eval form)
        (simple-reader-error stream
          "Read-time eval is suppressed: ~S" form ))) ) )


;;;; 2.4.8.5 Sharpsign Colon
;
(defun |#:-reader| (stream sub-char arg)
  (when *read-suppress*
    (discard-token nil stream)
    (return-from |#:-reader| nil) )

  (when arg (reader-error-extra-infix-arg stream sub-char arg))

  (multiple-value-bind (type token)
      (read-token nil stream)
    (ecase type
      ((:eof)
        (reader-eof-error stream) )
      ((:macro)
        (simple-reader-error stream
          "Invalid symbol name: ~S" token ))
      ((:token)) )
    (loop
      with astring = (ref reader-token astring token)
      with cstring = (subseq (ref reader-token cstring token) 0 (ref reader-token length token))
      for i below (ref reader-token length token)
      for attr = (svref astring i) do
        (when (logbitp 10 attr)    ; TRAIT_Package
          (simple-reader-error stream
            "Can't include package marker in symbol: ~S"
            cstring ))
      finally (return (make-symbol cstring)) ) ) )


;;;; 2.4.15 Sharpsign Equal
;
(defun |#=-reader| (stream sub-char arg)
  (cond
    (*read-suppress*
      (read stream t nil t)
      nil )

    ((not arg)
      (reader-error-missing-infix-arg stream sub-char) )

    ((assoc arg *read-labels*)
      (simple-reader-error stream
        "#~D~C is already used." arg sub-char ) )

    (t
      (let ((label.object (cons arg nil)))
        (push label.object *read-labels*)
        (setf (cdr label.object) (read stream t nil t)) ) )) )


;;;; 2.4.8.12 Sharpsign A
;;;
;;; Syntax:
;;;     #nA( ... contents ...) => array
;;;
;
(defun |#A-reader| (stream sub-char arg)
  (let ((contents (read stream t nil t)))
    (cond
      (*read-suppress* nil)

      (arg
        (let ((dimensions '())
              (rank       (or arg 0))
              (content    contents) )
          (dotimes (i rank)
            (let ((dimension (length content)))
              (push dimension dimensions)
              (if (zerop dimension)
                  (setq content nil)
                (setq content (elt content 0)) ) ) )
          (setq dimensions (nreverse dimensions))
          (make-array dimensions :initial-contents contents) ) )

      (t
       (reader-error-missing-infix-arg stream sub-char) )) ) )

;;;; 2.4.8.7 Sharpsign B
;;;; 2.4.8.7 Sharpsign O
;;;; 2.4.8.7 Sharpsign X
(macrolet (
  (define-radix-reader (char radix)
     (let ((name (intern (format nil "#~C-reader" char))))
       `(defun ,name (stream sub-char arg)
          (when arg (reader-error-extra-infix-arg stream sub-char arg))
          (funcall #'|#R-reader| stream sub-char ,radix) ) ) )
  )
  ;;
  (define-radix-reader #\B 2)
  (define-radix-reader #\O 8)
  (define-radix-reader #\X 16)
 ) ; macrolet


;;;; 2.4.8.11 Sharpsign C
;;;
;;; Syntax:
;;;     #C(real imag) == #.(complex (quote real) (quote imag))
;
(defun |#C-reader| (stream sub-char arg)
  (when arg (reader-error-extra-infix-arg stream sub-char arg))
  (let ((list (read stream t nil t)))
    (unless *read-suppress*
      (unless (= (safe-list-length list) 2)
        (simple-reader-error stream
           "Requires two elements list isntead of ~S" list ))
      (complex (first list) (second list)) ) ) )


;;;; 2.4.8.14 Sharpsign P
;;;
;;; Syntax:
;;;     #p " <namestring> "
;
(defun |#P-reader| (stream sub-char arg)
  (when arg (reader-error-extra-infix-arg stream sub-char arg))
  (let ((namestring (read stream t nil )))
    (unless *read-suppress*
      (unless (stringp namestring)
        (simple-reader-error stream
          "Requires string instead of ~S" namestring ))
      (pathname namestring) ) ) )


;;;; 2.4.8.10 Sharpsign R
;
(defun |#R-reader| (stream sub-char radix)
  (when *read-suppress*
    (discard-token nil stream)
    (return-from |#R-reader| nil) )

  (unless radix
    (simple-reader-error stream
      "#~C requires radix between # and ~C." sub-char sub-char ))

  (unless (and (integerp radix) (<= 2 radix 36))
    (simple-reader-error stream
      "~S is invalid radix for #~C." radix sub-char ))

  (multiple-value-bind (type token) (read-token nil stream)
    (ecase type
      ((:eof)
        (reader-eof-error stream) )
      ((:macro)
       (simple-reader-error stream
         "Character '~:C' isn't allowed after #~C" token sub-char ) )
      (:token) )

    (let ((index   0)
          (sign    nil)
          (num     nil)
          (den     0)
          (ndigits 0)
          (cstring (ref reader-token cstring token))
          (astring (ref reader-token astring token))
          (end     (ref reader-token length  token)) )
      (loop
        (when (= end index) (return))
        (let ((trait (logand (svref astring index) #x1FFF00)))
          (cond
            ((logbitp  9 trait)     ; digit
              (unless sign (setq sign 1))
              (let ((digit (digit-char-p (schar cstring index) radix)))
                (unless digit (return))
                (setq den (+ (* den radix) digit))
                (incf ndigits) ) )

            ((logbitp 13 trait)     ; plus
              (when sign (return))
              (setq sign 1) )

            ((logbitp 14 trait)     ; minus
              (when sign (return))
              (setq sign -1) )

            ((logbitp 15 trait)     ; ratio
              (when num (return))
              (setq num den)
              (setq den 0)
              (setq ndigits 0) )

            (t (return)) ) )
        (incf index) )

      (when (or (/= index end) (zerop ndigits))
        (simple-reader-error stream
          "Number syntax error: ~S" (subseq cstring 0 end) ))

      (* sign (if num (/ num den) den)) ) ) )


;;;; 2.4.8.13 Sharpsign S
;
(defun |#S-reader| (stream sub-char arg)
  (labels (
    (error-bad-class (name)
      (simple-reader-error stream
        "#~C: ~S isn't structure-class name." sub-char name ) )

    (error-bad-form (list)
      (simple-reader-error stream
        "Malformed structure form: #~C~S" sub-char list ) )

    (error-bad-key (name)
      (simple-reader-error stream
        "#~C: Invalid key: ~S" sub-char name ) )
    )
    ;;
    ;; #S-reader
    ;;
    (when arg (reader-error-extra-infix-arg stream sub-char arg))
    (unless *read-suppress*
      (let* ((list
               (let ((list (read stream t nil t)))
                 (unless (consp list) (error-bad-form list))
                 list ) )
             (class
               (let* ((name  (first list))
                      (class (find-class name nil)))
                 (unless (typep class 'structure-class) (error-bad-class name))
                 class ) ))

      (unless (proper-list-p list) (error-bad-form list))

      (loop
        for scan on (rest list) by #'cddr
        for key = (first scan)
        when (null (rest scan)) do (error-bad-form list)
        do
          (unless (symbolp key) (error-bad-key key))
          (setf (first scan)
                (intern (symbol-name key) (find-package :keyword)) ))

     (apply #'make-structure class (rest list)) )) ) )


;;;; 2.4.8.1 Sharpsign Backslash
;;;
;;; Syntax:
;;;  #\<token>  => character
(defun |#\\-reader| (stream sub-char arg)
  (unread-char sub-char stream)
  (cond
    (*read-suppress* (discard-token nil stream) nil)
    (arg (reader-error-extra-infix-arg stream sub-char arg))
    (t
      (multiple-value-bind (type token) (read-token nil stream)
        (ecase type
          ((:eof)
            (reader-eof-error stream) )
          (:token) )
        (let ((string (ref reader-token cstring token))
              (length (ref reader-token length  token)) )
          (if (eql length 1)
              (schar string 0)
            (let ((name (subseq string 0 length)))
              (or (name-char name)
                  (error 'invalid-character-name :string name) ) )) ) ) )) )


;;;; 2.4.8.19 Sharpsign Vertical-Bar
;
(defun |#\|-reader| (stream sub-char arg)
  (when arg (reader-error-extra-infix-arg stream sub-char arg))
  (loop with level = 0
        with last = #\*
        for char = (fast-read-char stream t)
        do  (case last
              ((#\|)
                (when (eql #\# char)
                  (when (zerop level) (return))
                  (decf level)
                  (setq char #\*) ) )
              ((#\#)
                (when (eql #\| char)
                  (incf level)
                  (setq char #\*) ) ))
            (setq last char) )
  (values) )
