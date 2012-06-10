;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 24 System Construction
;;; dev/d24-load.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d24-load.lisp#3 $
;;;
;;; Description:
;;;  This file contains functions for development:
;;;
;;; Public Functions:
;;;     compile-file            24.2.1
;;;     compile-file-pathname   24.2.2
;;;     load                    24.2.3  genesis
;;;     provide                 24.2.10 deprecated
;;;     require                 24.2.11 deprecated
;
(in-package :devel)

;;;; 24.2.3 load
;;;
;;; Syntax:
;;;     load file &key verbose print if-does-not-exist external-format
;;;     => boolean
;;;
;;; BUGBUG: Please implement load.
;;; Option 1: Should be embedded into geneis by C-assembler. Because,
;;;     C-function doesn't do finally.
;;;
;;; Option 2: Creates C-load then replaces it by lisp-load.
;
(defun cl:load (filespec &rest args
                         &key (verbose *load-verbose*)
                              (print   *load-print*)
                              (if-does-not-exist :error)
                              (external-format :default) )
    (declare (dynamic-extent args))
  (labels (
    ;; load-sexp
    ;;
    (load-sexp (pathname)
      (with-open-file (in pathname
                       :if-does-not-exist if-does-not-exist
                       :external-format   external-format )
        (unless in
          (warn "Failed to open ~S." pathname)
          (return-from load-sexp nil) )

        (let ((eof             '(end-of-file))
              (*load-verbose*  verbose)
              (*load-print*    print)
              (*load-pathname* pathname)
              (*load-truename* (truename pathname))
              (*load-level*    (1+ *load-level*))
              (*package*       *package*)
              (*readtable*     *readtable*)
              (si::*read-line-number-table* (make-hash-table :test 'eq))
              (xc::*situation* 'load) )

          (when verbose
            (fresh-line)
            (write-char #\;)
            (loop repeat *load-level* do (write-char #\Space))
            (format t "Loading ~S ...~%" pathname) )

          (loop
            for form = (read in nil eof)
            until (eq eof form) do
              (let ((values (multiple-value-list (eval form))))
                (clrhash si::*read-line-number-table*)
                (when (and print values)
                  (if (null (rest values))
                      (format t "~:W~%" (first values))
                    (loop
                      for value in values
                      for nth = 0 then (1+ nth) do
                        (format t "; Value[~D]=~:W~%" nth value) )) ) ))

           (when verbose
             (fresh-line)
             (write-char #\;)
             (loop repeat *load-level* do (write-char #\Space))
             (format t "Finish loading ~S~%" pathname) )
           t ) ) )
    )
    ;;
    ;; load
    ;;
    (setq filespec (pathname filespec))

    (unless (pathname-type filespec)
      (setq filespec
            (merge-pathnames filespec
                             (make-pathname :host (pathname-host filespec)
                                            :type "LISP"
                                            :case :common ))))

    (let ((type (pathname-type filespec :case :common)))
      (cond
        ((string= "FASL" type)
          (apply #'fasl-load filespec args) )

        (t
          (load-sexp filespec) )) ) ) )
