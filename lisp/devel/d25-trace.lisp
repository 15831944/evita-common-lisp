;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Developer - 25 Environment
;;; devl/d25-trace.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-trace.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     trace           25.2.8
;;;     untrace         25.2.8
;;;
;;; Note: We can't trace functions which use non-standard calling convention.
;;; BUGBUG: NYI: Detect functio in R/O area, and tell user that we can't trace
;;; functions in R/O area.
;
(in-package :devel)

(defvar *capsulated-function-table* (make-hash-table :test 'eq))

(ext:deftlv *trace-level* 0)

;;;; trace
;
(defmacro trace (&rest fname*)
  `(%trace ',fname*) )


;;;; untrace
;
(defmacro untrace (&rest fname*)
  `(%untrace ',fname*) )


;;;; %trace
;;;  %untrace
;
(labels (
  ;; encapsulate
  (encapsulate (fname)
      (declare (values ext:unspecified))
   (let* ((key (hashkey fname))
          (kind.orig (gethash key *capsulated-function-table*)) )
    (when (eq (car kind.orig) 'trace)
      (return-from encapsulate) )

    (let ((fn (fdefinition fname)))
      (if (not (typep fn 'generic-function))
          (progn
            (setf (fdefinition fname) (make-trace-wrapper fname fn))
            (setf (gethash key *capsulated-function-table*)
                (cons 'trace fn) ))
        (let ((gf fn))
          (setq fn (si::funcallable-instance-function gf))
          (clos:set-funcallable-instance-function
              gf
              (make-trace-wrapper fname fn) )
          (setf (gethash key *capsulated-function-table*)
            (list* 'trace gf fn) ) )) ) ) )

  ;; hashkey
  (hashkey (fname)
    (typecase fname
       (symbol fname)
       ((cons (eql setf) (cons symbol null))
         (si::intern-setf-cell (second fname)) )
       (otherwise
         (format t "; Invalid function name: ~S~%" fname)
         nil )) )

  ;; list-traced-function
  (list-traced-function ()
      (declare (values list))
    (loop
      for key being each hash-key in *capsulated-function-table*
        using (hash-value kind.orig)
      when (eq (car kind.orig) 'trace)
        collect (if (typep key 'si:value-cell)
                    `(setf ,.(si::.value-cell-name key))
                  key )) )

  ;; make-trace-wrapper
  ;;  Note: We subsitute self reference in original function for tracing
  ;;    self-recursive function.
  ;;  Note: We MUST NOT subsitiute non-standard-call.
  ;;  Note: Fn can be read-only. So, we MUST check fn writable.
  (make-trace-wrapper (fname fn)
      (declare (type (or symbol list) fname))
      (declare (type function fn))
      (declare (values function))
    (let ((wrapper (make-trace-wrapper-aux fname fn)))
      (si::subst-in-function wrapper fn fn)
      (setf (ext:function-name wrapper) `(:trace ,fname))
      wrapper ) )

  ;; make-trace-wrapper-aux
  (make-trace-wrapper-aux (fname fn)
      (declare (type (or symbol list) fname))
      (declare (type function fn))
      (declare (values function))
    (lambda (&rest args)
        (declare (ext:lambda-name trace-wrapper))
        (declare (dynamic-extent args))
      (let ((*trace-level* (1+ *trace-level*)))
        (print-indent *trace-level*)

        (format *trace-output* "~D Enter ~S~{ ~S~}~%"
          *trace-level* fname args )

        (let ((vals (multiple-value-list (apply fn args))))
          (print-indent *trace-level*)
          (format *trace-output* "~D Exit  ~S~{ ~S~}~%"
              *trace-level* fname vals )
          (values-list vals) ) ) ) )

  ;; print-indent
  (print-indent (level)
      (declare (type (unsigned-byte 28) level))
      (declare (values ext:unspecified))
    (format *trace-output* "; ")
    (dotimes (i (1- level))
      (if (evenp i)
          (format *trace-output* "|  ")
        (format *trace-output* "  ") ) ) )

  ;; untrace-one
  (untrace-one (fname)
      (declare (values ext:unspecified))
    (let* ((key (hashkey fname))
           (kind.orig (gethash key  *capsulated-function-table*)) )
      (when (null kind.orig)
        (format t "; Function ~S is not traced.~%" fname)
        (return-from untrace-one) )

      (untrace-1 key (cdr kind.orig)) ) )

  ;; untrace-all
  (untrace-all ()
      (declare (values list))
    (loop
      for key being each hash-key in *capsulated-function-table*
        using (hash-value kind.orig)
      when (eq (car kind.orig) 'trace)
        collect (untrace-1 key (cdr kind.orig)) ) )

  ;; untrace-1
  ;;  Returns name of untraced function.
  (untrace-1 (key frob)
      (declare (type (or symbol si:value-cell) key))
      (declare (values (or symbol cons)))
    (let ((fname
            (etypecase key
              (symbol key)
              (si:value-cell `(setf ,(si::.value-cell-name key))) ) ))
      (if (consp frob)
          (let ((gf (car frob))
                (fn (cdr frob)) )
            (clos:set-funcallable-instance-function gf fn) )
        (let ((orig frob)
              (wrapper (fdefinition fname)) )
          (si::subst-in-function orig wrapper orig)
          (setf (fdefinition fname) orig) ))

      (remhash key *capsulated-function-table*)
      fname ) )
  )
  ;; %trace
  (defun %trace (fnames)
      (declare (type list fname))
      (declare (values list))
    (if (null fnames)
        (list-traced-function)
      (ext:with-collector (collect)
        (dolist (fname fnames)
          (if (not (fboundp fname))
              (format t "; Function ~S is undefined." fname)
            (progn
              (encapsulate fname)
              (collect fname) )) )) ) )

  ;; %defun
  (defun si::%defun (fname lambda-list fn)
      (declare (ignore lambda-list))
    (when (si::check-redefinition fname fn)
      (let* ((key (hashkey fname))
             (kind.orig (gethash key *capsulated-function-table*)) )
        (when kind.orig
          (setf (cdr kind.orig) fn)
          (setq fn (make-trace-wrapper fname fn)) )
        (setf (fdefinition fname) fn) ))
      fname )

  ;; %untrace
  (defun %untrace (fnames)
      (declare (type list fnames))
      (declare (values list))
    (if (null fnames)
        (untrace-all)
      (dolist (fname fnames fnames)
        (untrace-one fname) )) )
 ) ; labels
