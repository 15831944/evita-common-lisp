;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 23 Reader
;;; runtime/r23-reader.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r23-reader.lisp#3 $
;;;
;;; Description:
;;;  This file implements following functions:
;
(in-package :si)

;;;; discard-token
;;;
;;; Description:
;;;  When *read-suppress* is bound to true, this function is called
;;;  instead of read-token.
;
(defun discard-token (delimiter stream)
    (declare (values symbol (or character null)))
    (declare (type (or character null) delimiter))
    (declare (type stream stream))
  (let ((readtable *readtable*)
        (char nil)
        (attr 0) )
    (declare (type (or character null) char))
    (declare (type fixnum attr))
  (tagbody
    :step-1
      (multiple-value-setq (char attr) (read-char-and-attr stream readtable))

      (unless char
        (return-from discard-token (values :eof nil)) )

      (when (and delimiter (eql delimiter char))
        (return-from discard-token (values :delimiter char)) )

      (ecase (logand attr #xF)
        ((1)  ;  constituent
          (go :step-7) )
        ((2)  ; non-terminating macro
          (return-from discard-token (values :macro char)) )
        ((3)  ; terminating macro
          (return-from discard-token (values :macro char)) )
        ((4)  ; space
          (go :step-1) )
        ((5)  ; single-escape
          (setq char (fast-read-char stream t))
          (go :step-8) )
        ((6)  ; multiple-escape
          (go :step-9) )
        ((0)  ; invalid
          (go :step-8) ))

    :step-7
    :step-8
      (multiple-value-setq (char attr) (read-char-and-attr stream readtable))

      (unless char (go :step-10))

      (ecase (logand attr #xF)
        ((1)  ;  constituent
          (go :step-7) )
        ((2)  ; non-terminating macro
          (go :step-7) )
        ((3)  ; terminating macro
           (unread-char char stream)
           (go :step-10) )
        ((4)  ; space
          (when *read-preserving-whitespace*
            (unread-char char stream) )
          (go :step-10) )
        ((5)  ; single escape
          (setq char (fast-read-char stream t)) )
        ((6)  ; multiple-escape
          (go :step-9) )
        ((0)  ; invalid
          (go :step-7) ))

    :step-9
      (multiple-value-setq (char attr) (read-char-and-attr stream readtable))

      (unless char
        (reader-eof-error stream) )

      (case (logand attr #xF)
        ((6)
          (go :step-8) )
        ((5)
          (setq char (fast-read-char stream t)) ))

      (go :step-9)

    :step-10
      (return-from discard-token (values nil nil)) ) ) )


;;;; parse-token
;;;
;;; Syntax:
;;;   parse-token token stream
;;;     => object, type
;;;
;;; Arguments and Values:
;;;   token     - a token to be parsed.
;;;   stream    - a input-stream for signaling error.
;;;   object    - a symbol or real.
;;;   type      - :object or :dot
;;;
;;; Called by:
;;;   reader-main-2
;
(defun parse-token (token stream)
    (declare (values t (member :dot :object)))
    (declare (type reader-token token))
    (declare (type stream stream))
  (let ((cstring (ref reader-token cstring token))
        (astring (ref reader-token astring token))
        (end     (ref reader-token length  token)) )
  (labels (
    ;; error-internal-symbol
    (error-external-symbol (name package)
      (simple-reader-error stream
        "The symbol ~S isn't external symbol of ~A package."
        name (package-name package) ) )

    ;; error-internal-symbol
    (error-internal-symbol (name package)
      (simple-reader-error stream
        "Can't access internal symbol ~S in ~A package."
        name (package-name package) ) )

    ;; error-invalid-token
    (error-invalid-token (cstring end)
      (simple-reader-error stream
        "Invalid token: ~A"
        (subseq cstring 0 end) ) )

    ;; error-no-such-package
    (error-no-such-package (name)
      (simple-reader-error stream
        "Package ~S not found."
        name ) )

    ;; error-no-such-symbol
    (error-no-such-symbol (name package)
      (simple-reader-error stream
         "There is no such symbol ~S in package ~S."
         name (package-name package) ) )

    ;; error-too-many-colons
    (error-too-many-colons (cstring end)
      (simple-reader-error stream
        "Too many colons: ~A"
        (subseq cstring 0 end) ) )

    ;; make-float
    (make-float (real marker)
      (ecase marker
        (double-float (float real 1d0))
        (long-float   (float real 1l0))
        (short-float  (float real 1s0))
        (single-float (float real 1f0)) ) )

    ;; parse-digits
    (parse-digits (base cstring start end astring)
      (loop for index from start below end
            with n = 0
            unless (logbitp 9 (svref astring index))
              return (values n index)
            do (let ((digit (digit-char-p (schar cstring index) base)))
                 (unless digit (return (values n index)))
                 (setq n (+ (* n base) digit)) )
            finally (return (values n index)) ) )

    ;; parse-real
    (parse-real (cstring end astring)
      (multiple-value-bind (minus? start)
          (let ((traits (svref astring 0)))
            (cond
              ((logbitp 13 traits)  ; +
                (values nil 1) )
              ((logbitp 14 traits)  ; -
                (values t 1) )
              (t
                (values nil 0) )) )
        (let ((real (parse-real-aux cstring start end astring)))
          (cond
            ((not real) (parse-symbol cstring end astring))
            (minus? (- real))
            (t real) ) ) ) )

    ;; parse-real-aux
    ;;
    ;;  cstring - a string of consistuents.
    ;;  astring - a vector of attributes.
    ;;  start   - a bounding index of cstring and astring. It points digit.
    ;;  end     - a bounding index of cstring and astring. It points at end
    ;;            of string.
    ;;
    (parse-real-aux (cstring start end astring)
      (multiple-value-bind (real index)
          (parse-digits 10 cstring start end astring)
        (cond
          ((eql end index)
            ;; {decimal-digit}+
            (unless (eql *read-base* 10)
              (multiple-value-setq (real index)
                  (parse-digits *read-base* cstring start end astring) )
              (unless (= end index) (setq real nil)) )
            (return-from parse-real-aux real) )

          ((logbitp 12 (svref astring index))
            ;; {decimal-digit}+ decimal-point
            (incf index)  ; skip decimal-point

            ;; integer ::= {decimal-digit}+ decimal-point
            (when (= end index)
              (return-from parse-real-aux real) )

            (let ((integer real))
              (setq start index)
              (multiple-value-setq (real index)
                (parse-digits 10 cstring index end astring) )

              ;; No decimal-digit after decimal-point
              (when (eql start index)
                (return-from parse-real-aux nil) )

              (setq real (+ integer (/ real (expt 10 (- index start)))))

              ;; float ::= {decimal-digit}+ decimal-point {decimal-digit}+
              (when (= end index)
                (return-from parse-real-aux
                  (make-float real *read-default-float-format*) )) ) )

          (t
            (unless (eql *read-base* 10)
              ;; non-decmal-digit+
              (multiple-value-setq (real index)
                (parse-digits *read-base* cstring start end astring) )

              (when (= end index)
                (return-from parse-real-aux real) ))

            (when (eql start index)
              (return-from parse-real-aux nil) )

            (when (logbitp 15 (svref astring index))
                ;; {digit}+ ratio-marker
                (incf index)    ; skip ratio-marker

                ;; ratio ::= digit+ ratio-marker digit+
                (multiple-value-bind (den index)
                    (parse-digits *read-base* cstring index end astring)
                  (unless (= end index)
                    (return-from parse-real-aux nil) )
                  (return-from parse-real-aux (/ real den)) ) )

            (unless (eql *read-base* 10)
              (return-from parse-real-aux nil) ) ))

        ;; Here, we read one of followings:
        ;;  1. decimal-digit+
        ;;  2. decimal-digit+ decimal-point decimal-digit+
        ;;
        (let ((marker
                (let ((traits (svref astring index)))
                  (cond
                    ((logbitp 16 traits) 'double-float)
                    ((logbitp 17 traits) *read-default-float-format*)
                    ((logbitp 18 traits) 'single-float)
                    ((logbitp 19 traits) 'long-float)
                    ((logbitp 20 traits) 'short-float)
                    (t
                      (return-from parse-real-aux nil) )) ))
              (plus?    t)
              (exponent 0) )
          (incf index) ; skip exponent-marker

          ;; No character after exponent-marker
          (when (= end index)
            (return-from parse-real-aux nil) )

          ;; Check sign of exponent
          (let ((traits (svref astring index)))
            (cond
              ((logbitp 13 traits)      ; plus
                (incf index) )
              ((logbitp 14 traits)      ; minus
                (setq plus? nil)
                (incf index) )
              ((and (logbitp 9 traits)  ; digit
                    (char<= #\0 (schar cstring index) #\9))
                )
              (t
                (return-from parse-real-aux nil) )) )

          ;; No character after exponent sign
          (when (= end index)
            (return-from parse-real-aux nil) )

          ;; Get exponent
          (setq start index)
          (multiple-value-setq (exponent index)
            (parse-digits 10 cstring index end astring) )

          (unless (= end index)
            (return-from parse-real-aux nil) )

          (setq exponent (expt 10 exponent))

          (if plus?
              (setq real (* real exponent))
            (setq real (/ real exponent)) )

          (make-float real marker) ) ) )

    ;; parse-symbol
    (parse-symbol (cstring end astring)
      (let ((package nil)
            (ncolons 0)
            (state   'package)  ; #\: symbol
            (start   0) )
        (dotimes (index end)
          (let ((traits (svref astring index)))
            (cond
              ((not (zerop (logand #x300 traits)))  ; alphabet and digit
                (ecase state
                  ((package))
                  ((symbol))
                  ((#\:)
                    (setq start index)
                    (setq state 'symbol) )) )

              ((logbitp 10 traits)  ; package
                (ecase state
                  ((package) (setq state #\:) (incf ncolons))
                  ((symbol)  (error-invalid-token cstring end))
                  ((#\:)     (incf ncolons)) ) )

              (t (error-invalid-token cstring end)) ) ) )

        ;; Find package
        ;;
        (when (eql state #\:)
          (setq start end) )

        (cond
          ((zerop start)
            (setq package *package*) )

          ((>= ncolons 3)
            (error-too-many-colons cstring end) )

          ((= ncolons start)
           (setq ncolons 2)
           (setq package #.(symbol-package :key)) )

          (t
            (let ((name (subseq cstring 0 (- start ncolons))))
              (setq package (find-package name))
              (unless package
                (error-no-such-package name) ) ) ))

        ;; Intern symbol
        ;;
        (let ((name (subseq cstring start end)))
          (ecase ncolons
            ((0 2)
              (intern name package) )
            (1
              (multiple-value-bind (symbol status) (find-symbol name package)
                (ecase status
                  ((:external)
                    symbol )
                  ((:inherited)
                    (error-external-symbol name package) )
                  ((:internal)
                    (error-internal-symbol name package) )
                  ((nil)
                    (error-no-such-symbol name package) )) ) )) ) ) )
    )
    ;;
    ;; parse-token
    ;;
    (when (zerop end)
      (return-from parse-token (values (intern "") :object)) )

    ;; Check dot only token, e.g. ".", "..", "...", and so on.
   (loop
     for index from 0 below end
     and ndots = 0 then (1+ ndots)
     unless (logbitp 11 (svref astring index))   ; TRAIT_Dot
       do (return)
     finally
       (if (= 1 ndots)
           (return-from parse-token (values nil :dot))
         (simple-reader-error stream "Can't use dot-only token.") ))

    ;; Token maybe a symbol or a real.
    (let ((traits (svref astring 0)))
      (cond
        ((logbitp 9 traits)   ; TRAIT_Digit
          (values (parse-real cstring end astring) :object) )

        ((and (not (zerop (logand #x007000 traits))) ; plus, minus decimal
              (>= end 2)
              (logbitp 9 (svref astring 1)) ) ; digit
          (values (parse-real cstring end astring) :object) )

        (t
          (values (parse-symbol cstring end astring) :object) )) ) ) ) )


;;;; read-char-and-attr
(defun read-char-and-attr (stream readtable)
    (declare (values (or character null) fixnum))
  (labels (
    (get-char-attr (char readtable)
      (let ((info (get-char-info char readtable)))
        (if (consp info) (car info) info) ) )
    )
    ;;
    ;; read-char-and-attr
    ;;
    (let ((char (fast-read-char stream nil)))
      (if (not char)
          (values nil 0)
        (values char (get-char-attr char readtable)) ) ) ) )


;;;; read-token
;;;
;;; Syntax:
;;;   read-token delimiter stream readtable
;;;     => token-type, token-or-char
;;;
;;; Arguments and Values:
;;;  token-type     A symbol :eof, :macro, :token, or nil.
;;;  token-or-char  A character or token.
;;;
;;; Called by:
;;;   reader-main-2
;;;  |#*-reader|
;;;  |#:-reader|
;;;  |#\\-reader|
;
(defun read-token (delimiter stream)
    (declare (type (or null character) delimiter))
    (declare (type stream stream))
    (declare (values (member :delimiter :eof :macro :token)
                     (or character null reader-token) ))
  (let* ((token          (make-pooled-token))
         (readtable      *readtable*)
         (readtable-case (readtable-case readtable)) )
    (labels (
      ;; add-char
      (add-char (char attr)
        (let* ((cstring (ref reader-token cstring token))
               (astring (ref reader-token astring token))
               (index   (ref reader-token length  token))
               (length  (length cstring)) )
          (when (= length index)
            (let* ((new-length  (truncate (* length 120) 100))
                   (new-cstring (make-string new-length))
                   (new-astring (make-array  new-length)) )
              (setq cstring
                  (setf (ref reader-token cstring token)
                          (replace new-cstring cstring) ))
              (setq astring
                  (setf (ref reader-token astring token)
                    (replace new-astring astring) )) ))
          (setf (schar cstring index) char)
          (setf (svref astring index) attr)
          (setf (ref reader-token length token) (1+ index)) ) )

      ;; error-read-invalid-char
      (error-read-invalid-char (char stream)
        (error 'reader-read-invalid-char
               :stream    stream
               :character char ) )
      )
      ;;
      ;; read-token
      ;;
      (prog (char attr)
    :step-1
      (multiple-value-setq (char attr) (read-char-and-attr stream readtable))

      (unless char
        (free-pooled-token token)
        (return-from read-token (values :eof nil)) )

      (when (and delimiter (eq delimiter char))
        (free-pooled-token token)
        (return-from read-token (values :delimiter nil)) )

      (ecase (logand attr #xF)
        ((1)  ;  constituent
          (go :step-7) )
        ((2 3)  ; non-terminating macro, terminating macro
          (free-pooled-token token)
          (return-from read-token (values :macro char)) )
        ((4)  ; space
          (go :step-1) )
        ((5)  ; single-escape
          (setq char (fast-read-char stream t))
          (add-char char #x101)
          (go :step-8) )
        ((6)  ; multiple-escape
          (go :step-9) )
        ((0)  ; invalid
          (error-read-invalid-char char stream) ))

    :step-7
      (ecase readtable-case
        ((:upcase)   (setq char (char-upcase char)))
        ((:downcase) (setq char (char-downcase char)))
        ((:invert)   (setq char (if (upper-case-p char)
                                  (char-downcase char)
                                (char-upcase char) )))
        (:preserve) )
      (add-char char attr)

    :step-8
      (multiple-value-setq (char attr) (read-char-and-attr stream readtable))

      (unless char (go :step-10))

      (ecase (logand attr #xF)
        ((1)  ;  constituent
          (go :step-7) )
        ((2)  ; non-terminating macro
          (go :step-7) )
        ((3)  ; terminating macro
           (unread-char char stream)
           (go :step-10) )
        ((4)  ; space
          (when *read-preserving-whitespace*
            (unread-char char stream) )
          (go :step-10) )
        ((5)  ; single escape
          (setq char (fast-read-char stream t))
          (add-char char #x101) ; TRAIT_Alphabetic | TYPE_Cons
          (go :step-8) )
        ((6)  ; multiple-escape
          (go :step-9) )
        ((0)  ; invalid
          (error-read-invalid-char char stream) ))

    :step-9
      ;; Read characters after multiple escape character.
      (multiple-value-setq (char attr) (read-char-and-attr stream readtable))

      (unless char
        (error 'end-of-file :stream stream) )

      (case (logand attr #xF)
        ((6)  ; multiple-escape
          (go :step-8) )
        ((5)  ; single-escape
          (setq char (fast-read-char stream t)) )
        ((0)
          (error-read-invalid-char char stream) ))

      (add-char char #x101) ; TRAIT_Alphabetic | TYPE_Cons

      (go :step-9)

    :step-10
      (return-from read-token (values :token token)) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;


;;;; 23.2.6 read-from-string
;
(defun cl:read-from-string (string &optional (eof-error-p t) eof-value
                                   &key (start 0) end preserve-whitespace )
  (let (index object)
    (with-input-from-string (stream string :start start :end end :index index)
      (if preserve-whitespace
          (setq object (read-preserving-whitespace stream
                                                   eof-error-p eof-value ))
        (setq object (read stream eof-error-p eof-value)) ))
    (values object index) ) )


;;;; 23.2.4 read
;;;; 23.2.4 read-preserving-whitespace
;;;; 23.2.5 read-delimited-list
;
(labels (
  ;; call-reader-macro
  ;;
  (call-reader-macro (stream char)
    (let ((fn (get-macro-character char)))
      (unless fn
        (simple-reader-error stream "~S is broken." *readtable*) )
      (funcall fn stream char) ) )

  ;; label-p
  ;;  Returns true if specified object is label in this reader session.
  (label-p (object labels)
    (and (consp object) (car (member object labels :test #'eq))) )

  ;; patch
  ;;  Resolves forward label reference introduced by #n# reader-syntax.
  (patch (object labels)
    (let (label.object)
      (typecase object
        (cons
          (loop
            (if (setq label.object (label-p (car object) labels))
                (setf (car object) (cdr label.object))
              (patch (car object) labels) )

            (when (setq label.object (label-p (cdr object) labels))
              (setf (cdr object) (cdr label.object))
              (return) )

            (setq object (cdr object))

            (unless (consp object)
              (patch object labels)
              (return) )) )

        ;; BUGBUG: NYI: We'll support binary slot.
        (structure-object
          (loop
            for eslotd in (class-slots (class-of object))
            for location = (slot-definition-location eslotd)
            for object = (structure-instance-access object location)
            for label.object = (label-p object labels)
              if label.object
                do (setf (structure-instance-access object location)
                            (cdr label.object) )
              else
                do (patch object labels) ) )

        ((array t)
          (loop
            for index from 0 below (array-total-size object)
            for label.object = (label-p (row-major-aref object index) labels)
            if label.object 
              do (setf (row-major-aref object index) (cdr label.object))
            else
              do (patch (row-major-aref object index) labels) ) )) ) )

  ;; read-list
  (read-list (delimiter stream recursive?)
    (let* ((head  (list nil))
           (tail  head)
           (state 'first) ); list cdr last
      (loop
        (multiple-value-bind (object type)
            (reader-main nil delimiter stream t nil recursive?)
          (ecase state
            ((first)
            (ecase type
              ((:delimiter)
                (return) )
              ((:dot)
                (error 'reader-read-dot :stream stream :where state) )
              ((:object)
                (setq tail (setf (cdr tail) (list object)))
                (setq state 'list) )) )
            ((list)
              (ecase type
                ((:delimiter)
                   (return) )
                ((:dot)
                  (setq state 'cdr) )
                ((:object)
                   (setq tail (setf (cdr tail) (list object))) )) )
            ((cdr)
              (ecase type
                ((:delimiter)
                  (error 'reader-read-delimiter
                         :stream stream
                         :delimiter delimiter ) )
                ((:dot)
                  (error 'reader-read-dot :stream stream :where state) )
                ((:object)
                  (setf (cdr tail) object)
                  (setq state 'last) )) )
            ((last)
              (ecase type
                ((:delimiter)
                  (return) )
                ((:dot)
                  (error 'reader-read-dot :stream stream :where state) )
                ((:object)
                  (error 'reader-read-object-after-dot :stream stream) )) ))
          (setq recursive? t) ))
      (cdr head) ) )

  ;; reader-main
  ;;
  (reader-main (whitespace? delimiter stream eof-error? eof-value recursive?)
    (if recursive?
        (reader-main-2 delimiter stream eof-error? eof-value)
      (let ((*read-backquote*             nil)
            (*read-preserving-whitespace* whitespace?)
            (*read-labels*                  '()) )
        (multiple-value-bind (object type)
            (reader-main-2 delimiter stream eof-error? eof-value)
          (when *read-labels*
            (patch object *read-labels*) )
          (values object type) ) )) )

  ;; reader-main-2
  ;;
  ;; BUGBUG: REVIEW: We should use tail-recursive call instead of loop for
  ;; readability of code. Since, C1 compiler produces inefficient code for
  ;; tail-recursive call.
  ;;
  ;; Note: Iteration occurs only when reader macro returns no value, e.g.
  ;; comment.
  ;;
  (reader-main-2 (delimiter stream eof-error? eof-value)
      (declare (values t t))
    (loop
      (multiple-value-bind (type token-or-char)
          (if *read-suppress*
              (discard-token delimiter stream)
            (read-token delimiter stream) )
        (ecase type
          ((:delimiter)
            (return (values token-or-char :delimiter)) )

          ((:dot)
            (unless delimiter
              (error 'reader-read-dot :stream stream) )
            (return (values token-or-char :dot)) )

          ((:eof)
            (when eof-error? (reader-eof-error stream))
            (return (values eof-value :object)) )

          ((:macro)
            (let ((values (multiple-value-list
                              (call-reader-macro stream token-or-char) )))
              (when values
                (return (values (first values) :object)) ) ) )

          ((nil)
            (return (values nil :object)) )

          ((:token)
            (multiple-value-bind (object type)
                    (parse-token token-or-char stream)
              (free-pooled-token token-or-char)
              (when (and (eq :dot type) (not delimiter))
                (error 'reader-read-dot :stream stream) )
              (return (values object type)) ) )) )) )
  )
  ;;
  ;;
  (defun cl:read (&optional stream (eof-error? t) eof-value recursive?)
    (setq stream (ensure-input-stream stream))
    (values (reader-main nil nil stream eof-error? eof-value recursive?)) )

  (defun cl:read-preserving-whitespace
      (&optional stream (eof-error? t) eof-value recursive?)
    (setq stream (ensure-input-stream stream))
    (values (reader-main t nil stream eof-error? eof-value recursive?)) )

  (defun cl:read-delimited-list (delimiter &optional stream recursive?)
    (setq stream (ensure-input-stream stream))
    (let ((*read-start-line-number* (stream-line-number stream)))
      (let ((list (read-list delimiter stream recursive?)))
        (when *read-line-number-table*
          (setf (gethash list *read-line-number-table*)
            *read-start-line-number* ))
        list ) ) )

    ;; Initialize standard readtable
    (let (
        ;(*readtable* *standard-readtable*)
        )
      (set-macro-character #\" '|"-reader|)
      (set-macro-character #\' '|'-reader|)
      (set-macro-character #\( '|(-reader|)
      (set-macro-character #\) '|)-reader|)
      (set-macro-character #\, '|,-reader|)
      (set-macro-character #\; '|;-reader|)
      (set-macro-character #\` '|`-reader|)

      ;;; Sharp
      (set-dispatch-macro-character #\# #\Backspace
        'invalid-dispatch-character )

      (set-dispatch-macro-character #\# #\Newline
        'invalid-dispatch-character )

      (set-dispatch-macro-character #\# #\Page
        'invalid-dispatch-character )

      (set-dispatch-macro-character #\# #\Return
        'invalid-dispatch-character )

      (set-dispatch-macro-character #\# #\Space
        'invalid-dispatch-character )

      (set-dispatch-macro-character #\# #\Tab
        'invalid-dispatch-character )

      ; !
      ; "
      (set-dispatch-macro-character #\# #\# '|##-reader|)
      ; $
      ; %
      ; &
      (set-dispatch-macro-character #\# #\' '|#'-reader|)
      (set-dispatch-macro-character #\# #\( '|#(-reader|)
      (set-dispatch-macro-character #\# #\) 'invalid-dispatch-character)
      (set-dispatch-macro-character #\# #\* '|#*-reader|)
      (set-dispatch-macro-character #\# #\+ '|#+-reader|)
      ; ,
      (set-dispatch-macro-character #\# #\- '|#--reader|)
      (set-dispatch-macro-character #\# #\. '|#.-reader|)
      ; /
      (set-dispatch-macro-character #\# #\: '|#:-reader|)
      (set-dispatch-macro-character #\# #\<  'invalid-dispatch-character)
      (set-dispatch-macro-character #\# #\= '|#=-reader|)
      (set-dispatch-macro-character #\# #\>  'invalid-dispatch-character)
      ; ?
      ; @
      (set-dispatch-macro-character #\# #\A '|#A-reader|)
      (set-dispatch-macro-character #\# #\B '|#B-reader|)
      (set-dispatch-macro-character #\# #\C '|#C-reader|)
     ; D
     ; E
     ; F
     ; G
     ; H
     ; I
     ; J
     ; K
     ; L
     ; M
     ; N
     (set-dispatch-macro-character #\# #\O '|#O-reader|)
     (set-dispatch-macro-character #\# #\P '|#P-reader|)
     ; Q
     (set-dispatch-macro-character #\# #\R '|#R-reader|)
     (set-dispatch-macro-character #\# #\S '|#S-reader|)
     ; T
     ; U
     ; V
     ; W
     (set-dispatch-macro-character #\# #\X '|#X-reader|)
     ; Y
     ; Z
     ; [
     (set-dispatch-macro-character #\# #\\ '|#\\-reader|)
     ; ]
     ; ^
     ; _
     ; `
     ; {
     (set-dispatch-macro-character #\# #\| '|#\|-reader|)
     ; }
     ; ~
     ) ; let
 ) ; labels
