;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; -*-
;;;;
;;;; evcl - Runtime - 22 Printer
;;; runtime/r22-printer.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2008 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r22-print-object.lisp#12 $
;
(in-package :si)

;;;; print-object array
(defmethod cl:print-object ((array array) stream)
  (labels (
    ;; print-array-like-list
    ;;
    (print-array-like-list (array stream)
      (format stream "#~Da" (array-rank array))
      (multiple-value-bind (vector start) (array-data array)
        (print-array-contents stream
                              vector start (array-dimensions array) ) ) )

    ;; print-array-contents
    ;;
    (print-array-contents (stream vector start dims)
      (let ((*printer-level* (1+ *printer-level*)))
        (cond
          ((null dims)
            (write-object (aref vector start) stream) )

          ((and *print-level* (> *printer-level* *print-level*))
            (write-char #\# stream) )

          ((plusp (first dims))
            (write-char #\( stream)
            (loop
              with dim   = (pop dims)
              with step  = (reduce #'* dims)
              with count = 0
              for  index = start then (+ index step) do
                (when (and *print-length* (>= count *print-length*))
                  (write-string "..." stream)
                  (return) )
                (print-array-contents stream vector index dims)
                (incf count)
                (when (= count dim) (return))
                (write-char #\Space stream) )
            (write-char #\) stream) )
          (t
            (write-string "()" stream) )) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; print-array-readably
    ;;
    (print-array-readably (stream array)
      (cond
        ((not *read-eval*)
         (error 'print-not-readable :object array) )

        ((array-displacement array)
          (error 'print-not-readable :object array) )

        (t
          (let ((form
                  (loop
                    for index from 0 below (array-total-size array)
                      collect `(setf (row-major-aref array ,index)
                                     ',(row-major-aref array index)) )) )
            (setq form
              `(let ((array (make-array ',(array-dimensions array)
                              :adjustable ',(adjustable-array-p array)
                              :element-type ',(array-element-type array) )))
                 ,@form ))
            (print form stream) ) )) )
    )
    ;;
    ;; print-array
    ;;
    (cond
      (*print-readably* (print-array-readably array stream))
      (*print-array*    (print-array-like-list array stream))
      (t                (print-array-unreadably array stream)) )
     array ) )


;;;; print-object character
(defmethod cl:print-object ((char character) stream)
  (labels (
    (print-char-name (char stream)
      (let ((name (gethash/eq char *char-name-table*))
            (code (char-code char)) )
        (cond
          (name
            (format stream "#\\~A" name) )
          ((<= #x21 code #x7E)
            (format stream "#\\~C" char) )
          ((<= code #xFFFF)
            (format stream "#\\u~4,'0X" code) )
          (t
            (setq name (gethash/eq (logand code #x100FF) *char-name-table*))
            (if (not name)
                (format stream "#\\v~6,'0X" code)
              (progn
                (write-string "#\\" stream)
                (when (logbitp  8 code) (write-string "Shift+"))
                (when (logbitp  9 code) (write-string "Control+"))
                (when (logbitp 10 code) (write-string "Alt+"))
                (write-string name stream) )) )) ) )
    )
    ;;
    (if *print-escape*
        (print-char-name char stream)
      (write-char char stream) )
    char ) )


;;;; print-object class-description
(defmethod cl:print-object ((o class) stream)
  (if (not (slot-boundp o 'name))
      (print-unreadable-object (o stream :type t :identity t)
        (write-string "uninitialized" stream) )
    (print-unreadable-object (o stream :type t)
      (print-object (class-name o) stream) ))
  o )


;;;; print-object condtion
(defmethod cl:print-object ((object condition) stream)
  (labels (
    ;; get-report-fn - get user defined report function
    (get-report-fn ()
      (let* ((class     (class-of object))
             (plist     (ref standard-class plist
                                (ref instance storage class) ) ))
        (getf plist 'report #'report) ) )

    ;; print-unreadably
    (print-unreadably (object stream)
      (print-unreadable-object (object stream :type t :identity t)) )

    ;; report - default report function
    (report (object stream)
      (loop
        with class = (class-of object)
        for slotd in (clos:class-slots class)
        for slot-name = (clos:slot-definition-name slotd)
        with tab-stop =
          (loop
            for slotd in (clos:class-slots class)
            for slot-name = (clos:slot-definition-name slotd)
              when (slot-boundp object slot-name)
                maximize (length (princ-to-string slot-name)) )
        initially
          (princ (string-capitalize (class-name class)) stream)
          (write-char #\: stream)
          (write-char #\Newline stream)
        do
          (when (slot-boundp object slot-name)
            (format stream "    ~A~1,V@T~S~%"
                    slot-name
                    tab-stop
                    (slot-value object slot-name) )) ) )
    )
    ;;
    ;; print-condition
    ;;
    (cond
      (*print-readably* (print-unreadably object stream))
      ((not *print-escape*) (funcall (get-report-fn) object stream))
      (t (print-unreadably object stream)) )
    object ) )

;;;; print-object cons
;;;
;;; (dotimes (k 10)
;;;     (let ((*print-length* k))
;;;       (format t "~D ~S~%" k (list* 1 2 3 4 5 6 7)) ) )
;;; 0 (...)
;;; 1 (1 ...)
;;; 2 (1 2 ...)
;;; 3 (1 2 3 ...)
;;; 4 (1 2 3 4 ...)
;;; 5 (1 2 3 4 5 ...)
;;; 6 (1 2 3 4 5 6 . 7)     ... print last atom
;;; 7 (1 2 3 4 5 6 . 7)
;;; 8 (1 2 3 4 5 6 . 7)
;;; 9 (1 2 3 4 5 6 . 7)
;;;
(defmethod cl:print-object ((cons cons) stream)
  (labels (
    (print-cons (cons)
      (let ((*printer-level*
                (1+ *printer-level*) )
            (length
              (if (typep *print-length* 'fixnum)
                  *print-length*
                most-positive-fixnum ) ))

        (write-char #\( stream)

        (loop
          (when (<= length 0)
            (write-string "..." stream)
            (return) )

          (decf length)

          (write-object (car cons) stream)
          (setq cons (cdr cons))

          ;; Proper list end?
          (unless cons
            (return) )

          ;; Dotted list or Circular list?
          (when (or (not (consp cons))
                    (and *printer-label-table*
                         (gethash/eq cons *printer-label-table*) ))
            (write-string " . " stream)
            (write-object cons stream)
            (return) )

          (write-char #\Space stream) )

        (write-char #\) stream) ) )
    )
    ;;
    (if (and *print-level* (>= *printer-level* *print-level*))
        (write-char #\# stream)
      (print-cons cons) )
    cons ) )

;;;; print-object function
(defmethod cl:print-object ((object function) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (function-name object) stream) ) )


;;;; print-object (hash-table)
(defmethod cl:print-object ((hash-table hash-table) stream)
  (if (or (not *print-readably*) (not *read-eval*))
      (print-unreadable-object (hash-table stream :type t :identity t)
        (format stream "~S ~D/~D"
                (hash-table-test  hash-table)
                (hash-table-count hash-table)
                (hash-table-size  hash-table) ))
    (format stream "#.~S"
      `(let ((hash-table
               (make-hash-table
                :size             ,(hash-table-size hash-table)
                :test            ',(hash-table-test hash-table)
                :rehash-threshold ,(hash-table-rehash-threshold hash-table)
                :rehash-size      ,(hash-table-rehash-size hash-table) ) ) )
         ,@(let ((setf* '()))
            (maphash #'(lambda (key val)
                        (push `(setf (gethash ',key hash-table) ',val)
                              setf* ) )
                    hash-table )
            setf* ) ) )) )


;;;; print-object integer
(defmethod cl:print-object ((integer integer) stream)
  (let ((base *print-base*))
    (unless (and (typep base 'fixnum) (<= 2 base 36))
      (setq base 10) )
    (when (and (/= 10 base) *print-radix*)
      (write-char #\# stream)
      (write-char (case base
                    (2    #\b)
                    (8    #\o)
                    (16   #\x)
                    (otherwise
                      (print-fixnum-aux base stream 10)
                      #\r ))
                   stream ))
    (when (minusp integer)
      (write-char #\- stream)
      (setq integer (- integer)) )

    (if (typep integer 'fixnum)
        (print-fixnum-aux integer stream base)
      (print-bignum-aux integer stream
                        base
                        (aref *bignum-divisor-vector* base)
                        (aref *fixnum-ndigits-vector* base) ))

    (when (and (= 10 base) *print-radix*)
      (write-char #\. stream) )
    integer ) )


;;;; print-object package
(defmethod cl:print-object ((package package) stream)
    (declare (type package package))
    (declare (type stream stream))
  (labels (
    ;; print-package-readably
    (print-package-readably ()
      (print-object `(find-package ',(package-name package)) stream) )

    ;; print-package-unreadably
    (print-package-unreadably ()
      (write-string "#<Package " stream)

      (write-string (package-nickname package) stream)

      (when *print-escape*
        (format stream " Ext=~D Int=~D"
          (svref (ref package external-table package) 0)
          (svref (ref package internal-table package) 0) ))

      (write-char #\> stream) )
    )
    ;;
    ;; print-package
    ;;
    (if *print-readably*
        (print-package-readably)
      (print-package-unreadably) )
    package ) )


;;;; print-pathname
;
(defmethod cl:print-object ((pathname pathname) stream)
    (declare (type stream stream))
  (let ((host (ref pathname host pathname)))
    (cond
      ((not (typep host 'basic-host))
        (print-unreadable-object (pathname stream :type t :identity t)) )
      (*print-escape*
        (write-string "#p" stream)
        (prin1 (namestring pathname) stream) )
      (t
        (princ (namestring pathname) stream) ))
     pathname ) )


;;;; print-object (readtable)
;
(defmethod cl:print-object ((readtable readtable) stream)
    (declare (type stream stream))
  (print-unreadable-object (readtable stream :type t :identity t)
    (format stream "~S" (readtable-case readtable)) ) )


;;;; print-object string
(defmethod cl:print-object ((o string) stream)
  (multiple-value-bind (string start end) (string-data o 0 nil)
    (print-simple-string string stream start end)
    o ) )


;;;; print-object structure-object
;;;
;;; Description:
;;;  Standard structure printer.
(defmethod cl:print-object ((object structure-object) stream)
  (print-structure object stream)
  object )


;;;; print-object symbol
(defmethod cl:print-object ((symbol symbol) stream)
    (declare (type stream stream))
  (let ((package      (symbol-package symbol))
        (read-case    (readtable-case *readtable*))
        (print-case   *print-case*)
        (print-escape *print-escape*) )
   (labels (
      ;; need-escape-p
      ;;
      ;; Description:
      ;;  Returns nil when name doesn't contain macro character and case of
      ;;  all characters are same as readtable-case.
      ;;
      (need-escape-p (name)
        (ecase read-case
          ((:upcase)
            (loop for char across name
                  for first-p = t then nil
                  when (lower-case-p char)
                    return 't

                  when (non-cons-char-p char first-p)
                    return 't ) )

          ((:downcase)
            (loop for char across name
                  for first-p = t then nil
                  when (upper-case-p char)
                    return 't

                  when (non-cons-char-p char first-p)
                    return 't ) )

          ((:invert :preserve)
            (loop for char across name
                  for first-p = t then nil
                  when (non-cons-char-p char first-p)
                    return 't ) )) )

      ;; non-cons-char-p
      ;;
      ;; BUGBUG: REVIEW: Should we use get-macro-character?
      ;;
      (non-cons-char-p (char first-p)
        (let ((syntax (get-char-syntax char *readtable*)))
          (if (and (not first-p) (eql syntax 2))
               nil
             (/= 1 syntax) ) ) )

      ;; potential-number-p
      ;;
      (potential-number-p (name)
        (let* ((base *read-base*)
               (end  (length name))
               (char (schar name (1- end))) )

          ;; 4. The token does not end with a sign.
          (when (or (char= #\+ char) (char= #\- char))
            (return-from potential-number-p nil) )

          (setq char (schar name 0))

          (unless (find char "0123456789+-^_.")
            (return-from potential-number-p nil) )

          (loop for char across name
                with ndigit10s       = 0
                with ndigits         = 0
                with decimal-point-p = nil
                with letter-p        = nil
                finally
                  (return (or (plusp ndigit10s) (plusp ndigits)))
                do
                  (cond
                    ((char<= #\0 char #\9)                  ; decimal-digit
                      (setq letter-p nil)
                      (incf ndigit10s)

                      (unless decimal-point-p
                        (when (digit-char-p char base)
                          (incf ndigits) )) )

                    ((or (char= #\+ char) (char= #\+ char)) ; sign
                      (setq letter-p nil) )

                    ((or (char= #\^ char) (char= #\_ char)) ; extension
                      (setq letter-p nil) )

                    ((char= #\. char)                       ; decimal-point
                      (setq letter-p nil)
                      (setq ndigits 0)
                      (setq decimal-point-p t) )

                    ((char= #\/ char)                       ; ratio-marker
                      (setq letter-p nil) )

                    ((and (not letter-p)                    ; exponent-marker
                          (or (char= #\D char) (char= #\d char)
                              (char= #\E char) (char= #\e char)
                              (char= #\F char) (char= #\f char)
                              (char= #\L char) (char= #\l char)
                              (char= #\S char) (char= #\s char) ))
                      (setq letter-p t)
                      (when (and (not decimal-point-p)
                                 (digit-char-p char base) )
                        (incf ndigits) ) )

                    (decimal-point-p
                      (return-from potential-number-p nil) )

                    ((digit-char-p char base)               ; digit
                      (setq letter-p t)
                      (incf ndigits) )

                    (t
                      (return-from potential-number-p nil) ))) ) )

      ;; print-token
      ;;
      (print-token (token)
        (cond
          ((and print-escape
                (or (zerop (length token))
                    (need-escape-p      token)
                    (potential-number-p token) ))
              (write-char #\| stream)
              (loop for char across token
                    when (or (char= #\\ char) (char= #\| char))
                      do (write-char #\\ stream)
                    do (write-char char stream) )
              (write-char #\| stream) )

          ((eq :preserve read-case)
            (write-string token stream) )

          ((eq :invert read-case)
            (loop for char across token
                  if (lower-case-p char)
                    do (setq char (char-upcase char))
                  else
                    do (setq char (char-downcase char))
                  end
                  do (write-char char stream) ) )

          ((eq :capitalize print-case)
            (loop for char across token
                  and in-word-p = nil then (alphanumericp char)
                  if in-word-p
                    do (write-char (char-downcase char) stream)
                  else
                    do (write-char (char-upcase char) stream)
                  end ) )

          ((eq :upcase read-case)
            (ecase print-case
              ((:upcase)
                (write-string token stream) )
              ((:downcase)
                (loop for char across token
                      do (write-char (char-downcase char) stream) ) )) )

          (t ; (eq :downcase read-case)
            (ecase print-case
              ((:upcase)
                (loop for char across token
                      do (write-char (char-upcase char) stream) ) )
              ((:downcase)
                (write-string token stream) )) )) )
      )
      ;;
      ;; print-symbol
      ;;

      ;; Print package
      (when print-escape
        (cond
          ((null package)
            (when *print-gensym* (write-string "#:" stream)) )

          ((eq '#.(symbol-package :key) package)
            (write-char #\: stream) )

          (t
            (let ((name (symbol-name symbol)))
              (multiple-value-bind (present status) (find-symbol name)
                (when (and (null present) status)
                  (setq present symbol) )

                (unless (and status (eq present symbol))
                  (print-token (package-nickname package))

                  (if (eq :internal (nth-value 1 (find-symbol name package)))
                    (write-string "::" stream)
                  (write-char #\: stream) )) ) ) )))

      ;; Print symbol name
      (print-token (symbol-name symbol))
      symbol ) ) )


;;;; print-object vector
;;;
;;; BUGBUG: NYI: Detecting printable character
;
(defmethod cl:print-object ((vector vector) stream)
  (labels (
    ;; print-bit-vector
    (print-bit-vector (bv stream)
      (multiple-value-bind (bv start end) (vector-data bv)
          (declare (type simple-bit-vector bv))
          (declare (type sequence-index start end))
        (write-string "#*" stream)
        (loop for pos from start below end do
          (write-char (digit-char (sbit bv pos)) stream) ) ) )

    ;; print-vector-like-list
    (print-vector-like-list (vector stream start end)
        (declare (type data-vector vector))
      (write-string "#(" stream)
      (loop for index from start below (or (and *print-length*
                                                (min *print-length* end) )
                                            end )
            do (when (> index start) (write-char #\Space stream))
               (write-object (row-major-aref vector index) stream)
            finally
              (unless (= index end)
                (when (> index (1+ start)) (write-char #\Space stream))
                (write-string "..." stream) ))
      (write-char #\u0029 stream) )

    ;; print-vector-readably
    (print-vector-readably (vector stream)
      (cond
        ((simple-vector-p vector)
          (print-vector-like-list vector stream 0 (length vector)) )

        ((simple-bit-vector-p vector)
          (print-bit-vector vector stream) )

        ((simple-string-p vector)
          (print-simple-string vector stream) )

        ((not *read-eval*)
         (error 'print-not-readable :object vector) )

        ((array-displacement vector)
          (error 'print-not-readable :object vector) )

        ((array-has-fill-pointer-p vector)
          (format stream "#.~S"
            `(let ((vector (make-array ,(array-dimension vector 0)
                            :fill-pointer ',(fill-pointer vector)
                            :adjustable   ',(adjustable-array-p vector)
                            :element-type ',(array-element-type vector) ) ) )
              ,@(let ((set* '()))
                  (dotimes (i (fill-pointer vector))
                    (push `(setf (row-major-aref vector ,i)
                                 ',(row-major-aref vector i) )
                          set* ) )
                  (nreverse set*) ) )) )
        (t
          (format stream "#.~S"
            `(make-array ,(array-dimension vector 0)
               :adjustable       ',(adjustable-array-p vector)
               :element-type     ',(array-element-type vector)
               :initial-contents ',(coerce vector 'list) ) ) )) )
    )
    ;;
    ;; print-vector
    ;;
    (cond
      (*print-readably*
        (print-vector-readably vector stream) )

      ((not *print-array*)
        (print-vector-unreadably vector stream) )

      ((bit-vector-p vector)
        (print-bit-vector vector stream) )

      (t
        (let ((*printer-level*  (1+ *printer-level*)))
          (multiple-value-bind (vector start end) (vector-data vector 0 nil)
            (print-vector-like-list vector stream start end) ) ) ))
    vector ) )


;;;; print-object t
;
(defmethod cl:print-object (object stream)
  (print-unreadable-object (object stream :type t :identity t))
  object )


;;;; print-object complex
;
(defmethod cl:print-object ((x complex) stream)
    (declare (type stream stream))
  (format stream "#c(~S ~S)" (realpart x) (imagpart x))
  x )


;;;; print-object ratio
;
(defmethod cl:print-object ((x ratio) stream)
    (declare (type stream stream))
  (format stream "~S/~S" (numerator x) (denominator x))
  x )


;;;; print-object standard-object
;
(defmethod cl:print-object ((object standard-object) stream)
  (print-unreadable-object (object stream :type t :identity t))
  object )
