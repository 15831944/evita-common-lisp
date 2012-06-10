;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - devel - load
;;; lisp/devel/d24-load.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel3/d24-load.lisp#3 $
;;;
;;; Description:
;;;  This file constains implemenation of
;;;     load
;
(in-package :si)

(deftlv *load-level* 0)

;;;; load
(defun cl:load (filespec &key (external-format :default)
                              (if-does-not-exist t)
                              ((:print *load-print*) *load-print*)
                              ((:verbose *load-verbose*) *load-verbose*) )
  (labels (
    ;; herald
    (herald (pathname which)
      (when *load-verbose*
        (loop repeat *load-level* do (write-char #\;))
        (ecase which
          ((:end)   (format t " End of loading ~S~%" pathname))
          ((:start) (format t " Loading ~S~%" pathname)) ) ) )

    ;; load-fasl
    (load-fasl (pathname)
      (devel::fasl-load pathname) )

    ;; load-sexp
    (load-sexp (pathname)
      (with-open-file (in pathname
                        :if-does-not-exist (and if-does-not-exist :error)
                        :external-format external-format )
        (when in
          (herald pathname :start)
          (load-sexp-stream in pathname)
          (herald pathname :end)
          t ) ) )

    ;; load-sexp-stream
    (load-sexp-stream (stream pathname)
      (let ((*load-pathname* pathname)
            (*load-truename* (truename pathname))
            (*package* *package*)
            (*readtable* *readtable*)
            (*read-line-number-table* (make-hash-table :test 'eq))
            (c::*situation* 'load) )
        (loop
          with eof = '(eof)
          for form = (read stream nil eof)
          until (eq form eof) do
            (let ((values (multiple-value-list (eval form))))
              (when *load-print*
                (if (null (rest values))
                    (format t "; ~:W~%" (first values))
                  (loop
                    for value in values
                    for nth = 0 then (1+ nth) do
                      (format t " ; [~D] ~:W~%" nth value) ))) )) ) )

    ;; load-some
    (load-some (defaults)
      (dolist (type '("LISP" "CL" "LSP" "L") (not-found defaults))
        (let* ((pathname (make-pathname :type type :case :common
                                       :defaults defaults ) )
               (stream (open pathname :if-does-not-exist nil)) )
          (when stream
            (with-open-stream (stream stream)
              (herald pathname :start)
              (load-sexp-stream stream pathname)
              (herald pathname :end)
              (return t) )) ) ) )

    ;; not-found
    (not-found (defaults)
      (when if-does-not-exist (error "No such file: ~S" defaults)) )

    ;; runtime-environment
    (runtime-environment ()
      (let ((env *environment*))
        (loop
          (let ((parent (ref environment outer env)))
            (when (null parent) (return env))
            (setq env parent) )) ) )
    )
    ;;
    (let* ((*load-level* (1+ *load-level*))
           (*environment* (runtime-environment))
           (pathname (pathname filespec))
           (type (pathname-type pathname :case :common)) )
      (cond
        ((null type)
          (load-some pathname) )
        ((string= "FASL" type)
          (load-fasl pathname) )
        (t
          (load-sexp pathname) )) ) ) )
