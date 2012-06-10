;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Runtime - 22 Printers
;;; runtime/r22-printer.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r22-printer.lisp#7 $
;
(in-package :si)

;;;; print-array-unreadably
;;;
;;; Called by:
;;;     print-object array
;;;     pprint-array
;
(defun print-array-unreadably (array stream)
  (print-unreadable-object (array stream :type t :identity t)
    (format stream "~S~A~:[ -~; ~{~D~^x~}~]"
           (array-element-type array)
           (if (array-displacement array) " indirect" "")
            (plusp (array-rank array))
            (array-dimensions array) ) ) )


;;;; print-bignum-aux
;;;
;;; Arguments:
;;;     divisor - a fixnum. (expt base (1- ndigits))
;;;     ndigits - a fixnum.
;;;
;;; Description:
;;;  Each call generates power-1 digits by using print-fixnum-aux.
;;;
;;; See:
;;;  TAOC: Vol.2. Section 4.4 Multiple-precision conversion.
;;;
;
(defun print-bignum-aux (bignum stream base divisor ndigits)
    (declare (type bignum bignum))  ; (assert (plusp bignum))
    (declare (type stream stream))
    (declare (type (integer 2 36) base))
  (multiple-value-bind (quotient remainder) (truncate bignum divisor)
    (if (typep quotient 'fixnum)
        (print-fixnum-aux quotient stream base)
      (print-bignum-aux quotient stream base divisor ndigits) )

    (loop for nzeros = (1- ndigits) then (1- nzeros)
          and base-power = base then (* base-power base)
          until (> base-power remainder)
          finally
            (loop repeat nzeros do (write-char #\0 stream))
            (print-fixnum-aux remainder stream base) ) ) )


;;;; print-fixnum-aux
;;;
;;; Called by:
;;;  print-array-as-list
;;;  print-integer
;;;  print-object float
;;;  print-float-digits-E
;;;
;;; Description:
;;;  Emits degits of specified POSITIVE fixnum in specified base into
;;;  given stream.
;;;
;;; Note: Until we have stack-string, we use recursive method.
;
(defun print-fixnum-aux (fixnum stream base)
    (declare (type fixnum fixnum))  ; (assert (not (minusp fixnum)))
    (declare (type stream stream))
    (declare (type (integer 2 36) base))
  (multiple-value-bind (quotient remainder) (truncate fixnum base)
    (when (plusp quotient)
      (print-fixnum-aux quotient stream base) )
    (write-char (digit-char remainder base) stream) ) )


;;;; print-simple-string
;;;
;;; Called by:
;;;     print-vector
(defun print-simple-string (string stream &optional (start 0) end)
  (if (not *print-escape*)
      (write-string string stream :start start :end end)
    (progn
      (write-char #\u0022 stream)
      (loop for index from start below (or end (length string))
            do (let* ((char (schar string index))
                      (code (char-code char)) )
                 (when (or (= #x22 code) (= #x5C code))
                   (write-char #\u005C stream) )
                 (write-char char stream) ))
      (write-char #\u0022 stream) )) )


;;; Using escape sequence for unprintable character is NOT standard.
#+ignore
(defun print-simple-string (string stream &optional (start 0) end)
  (if (not *print-escape*)
      (write-string string stream :start start :end end)
    (progn
      (write-char #\" stream)
      (loop for index from start below (or end (length string))
            do (let* ((char (schar string index))
                      (code (char-code char)) )
                 (when (or (= #x22 code) (= #x5C code))
                   (write-char #\\ stream) )
                 (if (<= #x20 code #x7E)
                     (write-char char stream)
                   (format stream "\\u~4,'0X" code) ) ))
      (write-char #\" stream) )) )


;;;; print-structure
;;;
;;; Description:
;;;  This is defulat structure-object printer. Prints structure in following
;;;  syntax:
;;;     #s(class-name {slot-name slot-value}*)
;;;
;;;  This function is used for structure classes defined without
;;;  print-function and print-object option.
(defun print-structure (object stream)
    (declare (type structure-object object))
    (declare (type stream stream))
    (declare (values structure-object))
  (labels (
    ;; print-readably
    ;; BUGBUG: NYI: Check standard constructor available when using #S
    ;; notation.
    (print-readably ()
      (print-sharp-S) )

    (print-sharp-S ()
      (pprint-logical-block (stream nil :prefix "#s(" :suffix ")")
        (let ((class (class-of object)))
          (print-object (class-name class) stream)
          ;(pprint-indent :block 1 stream)
          (loop for slotd in (clos:class-slots class) do
              (write-char #\Space stream)
              (pprint-newline :linear stream)
              (print-object
                (or (first (clos:slot-definition-initargs slotd))
                    (clos:slot-definition-name slotd) )
                stream )
              (write-char #\Space stream)
              (pprint-newline :fill stream)
              (print-object
                (structure-instance-access
                  object
                  (clos:slot-definition-location slotd) )
                stream )) )) )
    )
    ;;
    ;; print-object
    ;;
    (cond
      (*print-readably*    (print-readably))
      ((not *print-array*) (print-structure-unreadably object stream))
      ((and *print-level* (>= *printer-level* *print-level*))
        (write-char #\# stream) )
      (t (print-sharp-S) ))
    object ) )


;;;; print-structure-unreadably
(defun print-structure-unreadably (o s)
  (print-unreadable-object (o s :type t :identity t)) o )


;;;; print-vector-unreadably
;;;
;;; Called by:
;;;     print-object vector
;;;     pprint-vector
(defun print-vector-unreadably (vector stream)
  (cond
    ((not (adjustable-array-p vector))
      (print-unreadable-object (vector stream :type t :identity t)
        (format stream "~D" (array-total-size vector)) ) )
    ((array-has-fill-pointer-p vector)
      (print-unreadable-object (vector stream :type t :identity t)
        (format stream "~S~A ~D/~D"
               (array-element-type vector)
               (if (array-displacement vector) " indirect" "")
               (fill-pointer vector)
               (array-total-size vector) ) ) )
    (t
      (print-unreadable-object (vector stream :type t :identity t)
        (format stream "~S~A ~D"
               (array-element-type vector)
               (if (array-displacement vector) " indirect" "" )
               (length vector) ) ) )) )


;;;; print-unreadable-object-function
;;;
;;; Called by:
;;;     print-unreadable-object
(defun print-unreadable-object-function (object stream type identity fn)
    (declare (type stream stream))
    (declare (type (or function null) fn))
    (declare (values null))

  (when *print-readably*
    (error 'print-not-readable :object object) )

  (write-string "#<" stream)

  (when type
    (let ((class-name (class-name (class-of object))))
      (format stream "~:(~A~)" class-name) ))

  (when fn
    (when type (write-char #\Space stream))
    (funcall fn object stream) )

  (when identity
    (when (or type fn)
      (write-char #\Space stream) )

    ;; Note: Avoid bignum operation.
    #+64bit
    (format stream "@ ~X" (address-of object))
    #-64bit
    (format stream "@ ~4,'0X~4,'0X"
            (logand (ash (address-of object) -14) #xFFFF)
            (ash (logand (address-of object) #x3FFF) 2) ) )

  (write-char #\> stream)
  nil )


;;;;  write-chars
;;; Called by:
;;;     print-object float
;;;     print-float-digits
;;;     "~%", "~~", "~|"
(defun write-chars (char n stream)
    (declare (type character char))
    (declare (type fixnum n))
    (declare (type stream stream))
    (declare (values unspecified))
  (loop repeat n do
    (stream-write-char stream char) ) )


;;;; write-object
;;;
;;; Note: It is more beautiful that we use print-object around method. But,
;;; for performance reason, we use write-object instead of that.
(defun write-object (object stream)
    (declare (type stream stream))
    (declare (values unspecified))
  (labels (
    ;; check-and-print-it
    (check-and-print-it (object stream label-table)
      (let ((label (gethash/eq object label-table)))
        (cond
          ((or (not label) (eq 't label))
 #+debug-printer
    (format t ";   check: ~D ~:[not found~;once~]~%" (sxhash object) label)
            (print-it object stream) )

          ((zerop label)
 #+debug-printer (format t ";   check: ~D zero~%" (sxhash object))
            (setq label (incf *printer-label*))
            (setf (gethash object label-table) label)
            (write-char #\# stream)
            (print-fixnum-aux label stream 10)
            (write-char #\= stream)
            (print-it object stream) )

          (t
 #+debug-printer (format t ";   check: ~D ~D~%" (sxhash object) label)
            (write-char #\# stream)
            (print-fixnum-aux label stream 10)
            (write-char #\# stream) )) ) )

    ;; print-it
    (print-it (object stream)
      (if (not *print-readably*)
          (if (not *print-pretty*)
              (safe-print-object object stream)
            (safe-pprint-object object stream) )
        (let ((*print-escape* t)
              (*print-level*  nil)
              (*print-length* nil) )
          (if (not *print-pretty*)
              (safe-print-object object stream)
            (safe-pprint-object object stream) ) )) )

    ;; register-it
    (register-it (object stream label-table)
 #+debug-printer
    (format t ";   register-it: o=~D s=~D~%" (sxhash object) (sxhash object))
      (if (gethash/eq object label-table)
          (setf (gethash object label-table) 0)
        (progn
          (setf (gethash object label-table) t)
          (print-it object stream) )) )

    ;; start
    (start (object stream)
 #+debug-printer
    (format t "; start: o=~D s=~D~%" (sxhash object) (sxhash stream))
      (let* ((*printer-stream*      (make-broadcast-stream))
             (*printer-level*       0)
             ;; BUGBUG: NYI: Should use pooled hash-table.
             (label-table           (make-hash-table :test #'eq))
             (*printer-label-table* label-table)
             (*printer-label*       nil) )
          (register-it object *printer-stream* label-table)
          (setq *printer-label* 0)
          (setq *printer-stream* stream)
          (check-and-print-it object stream label-table) ) )

    ;; safe-print-object
    ;;  Call print-object within handler-case if object isn't built-in object.
    ;;  Since print of built-in object must be safe.
    (safe-print-object (o s)
      (if (eq (class-of (class-of o)) #.(find-class 'built-in-class))
          (print-object o s)
        (handler-case (print-object o s)
          (keyboard-interrupt (c) (signal c))
          (error (c) (print-object-with-error o s c)) ) ) )

    ;; safe-pprint-object
    (safe-pprint-object (o s)
      (handler-case (pprint-object o s)
        (keyboard-interrupt (c) (signal c))
        (error (c) (print-object-with-error o s c)) ) )

    ;; print-object-with-error
    (print-object-with-error (o s c)
      #+64bit
      (format s "#<Printer-Error #<~S @ ~X> ~S>"
        (type-of o)
        (address-of o)
        c )
      #-64bit
      (format s "#<Printer-Error #<~S @ ~4,'0X~4,'0X> ~S>"
        (type-of o)
        (ash (address-of o) -4)
        (logand (address-of o) 15)
        c ) )
    )
    ;;
    ;; write-object
    ;;
    ;; BUGBUG: NYI: resolve synonym stream
    (cond
      ((or (not *print-circle*)
           (numberp object)
           (and (symbolp object) (symbol-package object))
           (characterp object) )
        (if (eq *printer-stream* stream)
            (print-it object stream)
          (let ((*printer-stream* stream)
                (*printer-level*  0) )
            (print-it object stream) )) )

      ((not (eq *printer-stream* stream))
        (start object stream) )

      (*printer-label*
 #+debug-printer (format t "; *printer-label*: ~D~%" (sxhash object))
        (check-and-print-it object stream *printer-label-table*) )

      (*printer-label-table*
 #+debug-printer (format t "; *printer-label-table*: ~D~%" (sxhash object))
        (register-it object stream *printer-label-table*) )

      (t
        (start object stream) )) ) )
