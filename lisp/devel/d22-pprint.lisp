;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;;; evcl - Dev - 25 Environment - Pretty Printer
;; dev/d25-pprint.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d22-pprint.lisp#2 $
;;;
;;; Description:
;;;  This file contains pretty printers for Common Lisp special form.
;;;
;;; Note: Most of them are sniffed from CMUCL.
;;;
;
(in-package :devel)

;;;; print-unreadable-object
;;;
;;; Description:
;;;  Replacement of non-pretty-print version.
;
(defun si::print-unreadable-object-function
    (object stream type identity fn)

  (when *print-readably*
    (error 'print-not-readable :object object) )

  (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
    (when type
      (let ((class-name (class-name (class-of object))))
        (format stream "~:(~A~)" class-name) ))

      (when fn
        (when type
          (write-char #\Space stream)
          (pprint-newline :fill stream) )
        (funcall fn object stream) )

      (when identity
        (when (or type fn)
          (write-char #\Space stream)
          (pprint-newline :fill stream) )

        ;; Note: Avoid bignum operation.
        (format stream "@ ~4,'0X~4,'0X"
               (logand (ash (si::address-of object) -14) #xFFFF)
               (ash (logand (si::address-of object) #x3FFF) 2) ) ))
  nil )


;;;; pprint-backquote
;
(defun pprint-backquote (stream list)
  (funcall (formatter "`~W") stream (second list)) )


;;;; pprint-block
;
(defun pprint-block (stream list &rest args)
  (declare (ignore args))
  (funcall (formatter "~:<~^~W~^~3I ~:_~W~1I~@{ ~_~W~}~:>") stream list) )


;;;; pprint-flet
;;;
;;; We use more compact form. Change first "~@_"(miser) to "~:@_" (mandatory).
;
(defun pprint-flet (stream list &rest args)
  (declare (ignore args))
  (funcall (formatter
            "~:<~^~W~^ ~:@_~:<~@{~:<~^~W~^~3I ~:_~/CMDL::PPRINT-LAMBDA-LIST/~1I~:@_~@{~W~^ ~_~}~:>~^ ~_~}~:>~1I~@:_~@{~W~^ ~_~}~:>")
            stream
            list ) )
#+nil
(defun pprint-flet (stream list &rest args)
  (declare (ignore args))
  (funcall (formatter
            "~:<~^~W~^ ~@_~:<~@{~:<~^~W~^~3I ~:_~/CMDL::PPRINT-LAMBDA-LIST/~1I~:@_~@{~W~^ ~_~}~:>~^ ~_~}~:>~1I~@:_~@{~W~^ ~_~}~:>")
            stream
            list ) )


;;;; pprint-function
;
(defun pprint-function (stream list)
  (if (and (consp (cdr list)) (null (cddr list)))
      (funcall (formatter "#'~W") stream (second list))
    (pprint-fill stream list) ) )


;;;; pprint-lambda
;
(defun pprint-lambda (stream list &rest args)
  (declare (ignore args))
  (funcall (formatter
    "~:<~^~W~^~3I ~:_~/CMDL::PPRINT-LAMBDA-LIST/~1I~@{ ~_~W~}~:>")
    stream list ) )


;;;; pprint-lambda-list
;
(defun pprint-lambda-list (stream lambda-list &rest args)
  (declare (ignore args))
  (pprint-logical-block (stream lambda-list :prefix "(" :suffix ")")
    (let ((state :required)
          (first t))
      (loop
        (pprint-exit-if-list-exhausted)
        (unless first
          (write-char #\space stream))
        (let ((arg (pprint-pop)))
          (unless first
            (case arg
              (&optional
               (setf state :optional)
               (pprint-newline :linear stream))
              ((&rest &body)
               (setf state :required)
               (pprint-newline :linear stream))
              (&key
               (setf state :key)
               (pprint-newline :linear stream))
              (&aux
               (setf state :optional)
               (pprint-newline :linear stream))
              (t
               (pprint-newline :fill stream))))
          (ecase state
            (:required
             (pprint-lambda-list stream arg))
            ((:optional :key)
             (pprint-logical-block
                 (stream arg :prefix "(" :suffix ")")
               (pprint-exit-if-list-exhausted)
               (if (eq state :key)
                   (pprint-logical-block
                       (stream (pprint-pop) :prefix "(" :suffix ")")
                     (pprint-exit-if-list-exhausted)
                     (si::write-object (pprint-pop) stream)
                     (pprint-exit-if-list-exhausted)
                     (write-char #\space stream)
                     (pprint-newline :fill stream)
                     (pprint-lambda-list stream (pprint-pop))
                     (loop
                       (pprint-exit-if-list-exhausted)
                       (write-char #\space stream)
                       (pprint-newline :fill stream)
                       (si::write-object (pprint-pop) stream)))
                   (pprint-lambda-list stream (pprint-pop)))
               (loop
                 (pprint-exit-if-list-exhausted)
                 (write-char #\space stream)
                 (pprint-newline :linear stream)
                 (si::write-object (pprint-pop) stream))))))
        (setf first nil)))))


;;;; pprint-let
;
(defun pprint-let (stream list &rest args)
  (declare (ignore args))
  (funcall (formatter "~:<~^~W~^ ~@_~:<~@{~:<~^~W~@{ ~_~W~}~:>~^ ~_~}~:>~1I~:@_~@{~W~^ ~_~}~:>")
           stream
           list))

;;;; pprint-progn
;
(defun pprint-progn (stream list &rest args)
  (declare (ignore args))
  (funcall (formatter "~:<~^~W~@{ ~_~W~}~:>") stream list))


;;;; pprint-progv
;
(defun pprint-progv (stream list &rest args)
  (declare (ignore args))
  (funcall (formatter "~:<~^~W~^~3I ~_~W~^ ~_~W~^~1I~@{ ~_~W~}~:>")
           stream list))


;;;; pprint-quote
;
(defun pprint-quote (stream list)
  (if (and (consp (cdr list)) (null (cddr list)))
      (funcall (formatter "'~W") stream (second list))
    (pprint-fill stream list) ) )


;;;; pprint-setq
;
(defun pprint-setq (stream list &rest args)
  (declare (ignore args))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (si::write-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :miser stream)
    (if (and (consp (cdr list)) (consp (cddr list)))
        (loop
          (pprint-indent :current 2 stream)
          (si::write-object (pprint-pop) stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\space stream)
          (pprint-newline :linear stream)
          (pprint-indent :current -2 stream)
          (si::write-object (pprint-pop) stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\space stream)
          (pprint-newline :linear stream))
        (progn
          (pprint-indent :current 0 stream)
          (si::write-object (pprint-pop) stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\space stream)
          (pprint-newline :linear stream)
          (si::write-object (pprint-pop) stream)))))


(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro pprint-tagbody-guts (stream)
  `(loop
     (pprint-exit-if-list-exhausted)
     (write-char #\space ,stream)
     (let ((form-or-tag (pprint-pop)))
       (pprint-indent :block 
                      (if (atom form-or-tag) 0 1)
                      ,stream)
       (pprint-newline :linear ,stream)
       (si::write-object form-or-tag ,stream))))
) ; eval-when


(defun pprint-tagbody (stream list &rest args)
  (declare (ignore args))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (si::write-object (pprint-pop) stream)
    (pprint-tagbody-guts stream)))

(defun pprint-case (stream list &rest args)
  (declare (ignore args))
  (funcall (formatter
            "~:<~^~W~^ ~3I~:_~W~1I~@{ ~_~:<~^~:/CMDL::PPRINT-FILL/~^~@{ ~_~W~}~:>~}~:>")
           stream
           list))

;;;; pprint-defun
;
(defun pprint-defun (stream list &rest args)
    (declare (ignore args))
  (funcall (formatter
            "~:<~^~W~^ ~@_~:I~W~^ ~:_~/CMDL::PPRINT-LAMBDA-LIST/~1I~@{ ~_~W~}~:>")
           stream
           list))
#+nil
(defun pprint-defun (stream list &rest args)
    (declare (ignore args))
  (funcall (formatter
    "~:<~1I~W~^ ~@_~W~^ ~@_~:/pprint-fill/~^~@{ ~_~W~^~}~:>" )
    stream list ) )


;;;; pprint-destructuring-bind
;
(defun pprint-destructuring-bind (stream list &rest args)
  (declare (ignore args))
  (funcall (formatter
        "~:<~^~W~^~3I ~_~:/CMDL::PPRINT-LAMBDA-LIST/~^ ~_~W~^~1I~@{ ~_~W~}~:>")
           stream list))


;;;; pprint-do
;
(defun pprint-do (stream list &rest args)
    (declare (ignore args))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (si::write-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-indent :current 0 stream)
    (funcall (formatter "~:<~@{~:<~^~W~^ ~@_~:I~W~@{ ~_~W~}~:>~^~:@_~}~:>")
             stream
             (pprint-pop))
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :linear stream)
    (pprint-linear stream (pprint-pop))
    (pprint-tagbody-guts stream)))


;;;; pprint-dolist
;
(defun pprint-dolist (stream list &rest args)
  (declare (ignore args))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (si::write-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (pprint-indent :block 3 stream)
    (write-char #\space stream)
    (pprint-newline :fill stream)
    (funcall (formatter "~:<~^~W~^ ~:_~:I~W~@{ ~_~W~}~:>")
             stream
             (pprint-pop))
    (pprint-tagbody-guts stream)))


;;;; pprint-function-form
;
(defun pprint-function-form (stream list)
  (if (> (length (symbol-name (car list))) 12)
      (funcall (formatter "~:<~1I~@{~W~^ ~_~}~:>") stream list)
    (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>") stream list)) )


;;;; pprint-function-call
;
(defun pprint-function-call (stream list &rest args)
  (declare (ignore args))
  (funcall (formatter "~:<~^~W~^ ~:_~:I~@{~W~^ ~_~}~:>")
           stream
           list))


;;;; pprint-prog
;
(defun pprint-prog (stream list &rest args)
  (declare (ignore args))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (si::write-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :miser stream)
    (pprint-fill stream (pprint-pop))
    (pprint-tagbody-guts stream)))


;;;; pprint-typecase
;
(defun pprint-typecase (stream list &rest args)
  (declare (ignore args))
  (funcall (formatter
            "~:<~^~W~^ ~3I~:_~W~1I~@{ ~_~:<~^~W~^~@{ ~_~W~}~:>~}~:>")
           stream
           list))

;;;; pprint-unquote
;
(defun pprint-unquote (stream list)
  (funcall (formatter ",~W") stream (second list)) )


;;;; pprint-unquote-nsplicing
;
(defun pprint-unquote-nsplicing (stream list)
  (funcall (formatter ",.~W") stream (second list)) )


;;;; pprint-unquote-splicing
;
(defun pprint-unquote-splicing (stream list)
  (funcall (formatter ",@~W") stream (second list)) )


;;; Install pretty printers
;
(dolist (op.fn '(
    (lambda                 . pprint-lambda)

    (si::backquote          . pprint-backquote)
    (si::unquote            . pprint-unquote)
    (si::unquote-nsplicing  . pprint-unquote-nsplicing)
    (si::unquote-splicing   . pprint-unquote-splicing)


    ;; Special operators
    (block                  . pprint-block)
    (catch                  . pprint-block)
    (eval-when              . pprint-block)
    (flet                   . pprint-flet)
    (function               . pprint-function)
    (labels                 . pprint-flet)
    (let                    . pprint-let)
    (let*                   . pprint-let)
    (locally                . pprint-progn)
    (macrolet               . pprint-flet)
    (multiple-value-call    . pprint-block)
    (multiple-value-prog1   . pprint-block)
    (progn                  . pprint-progn)
    (progv                  . pprint-progv)
    (quote                  . pprint-quote)
    (return-from            . pprint-block)
    (setq                   . pprint-setq)
    (symbol-macrolet        . pprint-let)
    (tagbody                . pprint-tagbody)
    (throw                  . pprint-block)
    (unwind-protect         . pprint-block )

    ;; Macros
    (case                   . pprint-case)
    (ccase                  . pprint-case)
    (ctypecase              . pprint-typecase)
    (defconstant            . pprint-block)
    (define-compiler-macro  . pprint-defun)
    (define-modify-macro    . pprint-defun)
    (define-setf-expander   . pprint-defun)
    (defmacro               . pprint-defun)
    (defparameter           . pprint-block)
    (deftype                . pprint-defun)
    (defun                  . pprint-defun)
    (defvar                 . pprint-block)
    (destructuring-bind     . pprint-destructuring-bind)
    (do                     . pprint-do)
    (do*                    . pprint-do)
    (do-all-symbols         . pprint-dolist)
    (do-external-symbols    . pprint-dolist)
    (do-symbols             . pprint-dolist)
    (dolist                 . pprint-dolist)
    (dotimes                . pprint-dolist)
    (ecase                  . pprint-case)
    (etypecase              . pprint-typecase)
    #+NYI (handler-bind . pprint-handler-bind)
    #+NYI (handler-case . pprint-handler-case)
    #+NYI (loop         . pprint-loop)
    (multiple-value-bind    . pprint-progv)
    (multiple-value-setq    . pprint-block)
    (pprint-logical-block   . pprint-block)
    (print-unreadable-object    . pprint-block)
    (prog                       . pprint-prog)
    (prog*                      . pprint-prog)
    (prog1                      . pprint-block)
    (prog2                      . pprint-progv)
    (psetf                      . pprint-setq)
    (psetq                      . pprint-setq)
    #+NYI (restart-bind         . pprint-restart-bind)
    #+NYI (restart-case         . pprint-restart-case)
    (setf                       . pprint-setq)
    (step                       . pprint-progn)
    (time                       . pprint-progn)
    (typecase                   . pprint-typecase)
    (unless                     . pprint-block)
    (when                       . pprint-block)
    (with-compilation-unit      . pprint-block)
    #+NYI (with-condition-restarts . pprint-with-condition-restarts)
    (with-hash-table-iterator   . pprint-block)
    (with-input-from-string     . pprint-block)
    (with-open-file             . pprint-block)
    (with-open-stream           . pprint-block)
    (with-output-to-string      . pprint-block)
    (with-package-iterator      . pprint-block)
    (with-standard-io-syntax    . pprint-progn)
    ) )
  (set-pprint-dispatch `(cons (eql ,(car op.fn))) (cdr op.fn)) )


(defun function-form-p (x)
  (and (consp x) (symbolp (first x)) (fboundp (first x))) )

(set-pprint-dispatch '(satisfies function-form-p) 'pprint-function-form -1)

(set-pprint-dispatch '(cons (eql if))
  (formatter "~:<~W~^ ~@_~:I~W ~_~^~W~^~1I ~_~@{~W~^ ~:_~}~:>") )


(set-pprint-dispatch 'package
  (lambda (stream package)
      (declare (ext:lambda-name (pprint . package)))
      (declare (type stream stream))
      (declare (type pacakge package))
    (print-unreadable-object (package stream :type t)
      (princ (package-name package) stream) ) ) )
