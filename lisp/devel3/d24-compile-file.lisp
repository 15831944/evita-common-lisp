;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - devel - 24 Systm Construction - Compile File
;;; lisp/devel3/d24-compile-file.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel3/d24-compile-file.lisp#3 $
;;;
;;; Description:
;;;  This file constains implemenation of
;;;     compile-file
;;;     compile-file-pathname
;
(in-package :si)

(deftlv *compile-level* 0)

;;;; compile-toplevel-form
(defun compile-toplevel-form (stream form fasd)
    (declare (values fixnum fixnum))
    (declare (type stream stream))
    (declare (type devel::fasd fasd))
  (let ((total-warns 0) (total-errors 0))
  (labels (
    ;; cm-warn
    (cm-warn (format &rest args)
      (format *error-output* "~A(~D): ~?~%"
        (truename (stream-pathname stream))
        (1+ (stream-line-number stream))
        format args )
      (incf total-warns) )

    ;; compile-form
    (compile-form (form)
      (multiple-value-bind (fn errors warns cookies)
          (c::compile-form form)
        (when warns  (incf total-warns))
        (when (or (null fn) errors) (incf total-errors))
        (loop
          with objtab = (slot-value fasd 'devel::objtab)
          for (cookie . form) in cookies do
            (setf (gethash cookie objtab) `(load-time-value ,form nil)) )
        fn ) )

    ;; process
    (process (form)
      (cond
        ((not (consp form))
          (cm-warn "Ignore toplevel literal form: ~S" form) )
        ((not (proper-list-p form))
          (cm-warn "Ignore malformed cons form: ~S" form) )
        (t (process-cons form)) ) )

    ;; process-cons
    (process-cons (form)
      (case (first form)
        ((defun)      (process/defun form))
        #+nil ((eval-when)  (process/eval-when form))
        ((in-package) (process/in-package form))
        ((progn)      (process/progn form))
        (otherwise    (process/others form)) ) )

    ;; process/defun
    (process/defun (form)
      (destructuring-bind (defun name lambda-list &body body) form
          (declare (ignore defun))
        (let ((fn (compile-form
                `(labels ((,name ,lambda-list ,@body)) #',name) ) ))
          (when fn
            (let ((datum `(defun ,name ,lambda-list ,(funcall fn))))
              (devel::fasd-write datum fasd) )) ) ) )

    ;; process/eval-when
    #+nil
    (process/eval-when (form)
      (destructuring-bind (eval-when (&rest situation*) &rest form*) form
        (let ((ct nil) (ex nil) (lt nil))
          (dolist (situation situation*)
            (case situation
              ((:compile-toplevel cl:compile) (setq ct t))
              ((:execute cl:eval) (setq ex t))
              ((:load-toplevel cl:load) (setq lt t))
              (otherwise (cm-warn "Unknown situation ~S" situation)) ) )
          (let ((c::*processing-mode*
                    (if ex :compile-time-too c::*processing-mode*) ))
            (when ct (eval `(progn ,@form*)))
            (when lt (dolist (form1 form*) (process form1))) ) ) ) )

    ;; process/in-package
    (process/in-package (form)
      (if (/= (length form) 2)
          (cm-warn "Syntax error: ~S" form)
        (progn
          (%in-package (second form))
          (devel::fasd-write form fasd) )) )

    ;; process/others
    (process/others (form)
      (let ((fn (compile-form form)))
        (when fn (devel::fasd-write `(funcall ,fn) fasd)) ) )

    ;; process/progn
    (process/progn (form)
      (dolist (form1 (rest form))
        (process form1) ) )
    )
    ;;
    (process form)
    (values total-warns total-errors) ) ) )


;;;; funcall-with-compilation-unit
(defun c::funcall-with-compilation-unit (options fn)
    (declare (type list options))
    (declare (type function fn))
  (labels (
    ;; make-env -- make compilation environment
    (make-env ()
      (let ((env (.allocate-record #.(class-description 'environment))))
        (setf (ref environment latch env) (make-latch 'environment))
        (setf (ref environment outer env) *environment*)

        (setf (ref environment variables env) (make-hash-table :test 'eq))
        (setf (ref environment functions env) (make-hash-table :test 'eq))
        (setf (ref environment types     env) (make-hash-table :test 'eq))
        (setf (ref environment classes   env) (make-hash-table :test 'eq))
        (setf (ref environment others    env) (make-hash-table :test 'eq))
        env ) )
    )
    ;;
    (let ((*compile-level*
              (if (getf options :override) 1 (1+ *compile-level*)) ))
      (if (> *compile-level* 1)
          (funcall fn)
        (let ((*environment* (make-env))
              ;; FIXME 2007-07-07 yosi@msn.com bind c::*environment* for
              ;; backward compatibility for d00-loadup.lisp. We should remove
              ;; this binding.
              (c::*environment* *environment*)
              (c::*optimization* c::*optimization*)
              (c::*processing-mode* nil) )
          (funcall fn) ) ) ) ) )


;;;; 24.2.1 compile-file
(defun cl:compile-file (input-file
                        &key (output-file     nil output-file-p)
                             (verbose         *compile-verbose*)
                             (print           *compile-print*)
                             (external-format :default) )
  (unless (eq :default external-format)
    (error "Support only :defulat external-format.") )
  (let ((end-of-file  '(end-of-file))
        (reader-error '(reader-error)) )
  (labels (
    ;; compile-form
    (compile-form (input form fasd)
      (handler-case (compile-toplevel-form input form fasd)
        (program-error (c) (report-error input c) (values 0 1)) ) )

    ;; read-form
    (read-form (input)
      (handler-case (read input nil end-of-file)
        (reader-error (c) (report-error input c) reader-error) ) )

    ;; report-error
    (report-error (input condition)
      (format *error-output* "~A(~D): ~A~%"
        (truename (ext:stream-pathname input))
        (1+ (ext:stream-line-number input))
        condition ) )
    )
    ;;
    (setq input-file (pathname input-file))

    (unless (pathname-type input-file)
      (setq input-file
            (merge-pathnames input-file
                (make-pathname :host (pathname-host input-file)
                               :type "LISP"
                               :case :common ))))
    (unless output-file-p
      (setq output-file (compile-file-pathname input-file)) )

    (let ((*compile-file-pathname*  input-file)
          (*compile-file-truename*  (truename input-file))
          (*compile-verbose*        verbose)
          (*compile-print*          print)
          (*package*                *package*)
          (*readtable*              *readtable*)
          (*read-line-number-table*
              (make-hash-table :test 'eq) )
          (c::*situation*           'compile-file)
          (output-truename          (truename output-file))
          (total-nwarns             0)
          (total-nfails             0) )
      (with-open-file (input input-file)
        (when verbose
          (format t "~%; Compiling ~S~%;      into ~S~%;~%"
                  input-file #+nil *compile-file-truename*
                  output-file #+nil output-truename ))

        (with-compilation-unit ()
          (let ((fasd  (devel::fasd-open output-file))
                (abort-p t))
            (unwind-protect
                (loop
                  for form = (read-form input)
                  until (eq form end-of-file) do
                    (multiple-value-bind (nwarns nfails)
                         (if (eq form reader-error)
                             (values 0 1)
                           (compile-form input form fasd) )
                      (incf total-nwarns nwarns)
                      (incf total-nfails nfails) )
                  finally
                    (setq abort-p nil) )
              (when (and verbose abort-p)
                (format t "~&;*** Abort compilation of ~S~%"
                    input-file ))
              (devel::fasd-close fasd (or abort-p (plusp total-nfails)) ) )) )

        (when verbose
          (format t "~&; End of compilation of ~S~%"
                  input-file ) ))

      (values output-truename
              (if (zerop total-nwarns) nil total-nwarns)
              (if (zerop total-nfails) nil total-nfails) ) ) ) ) )


;;;; 24.2.2 compile-file-pathname
(defun cl:compile-file-pathname (input-file
                              &key (output-file nil output-file-p)
                              &allow-other-keys )
  (setq input-file (pathname input-file))
  (unless output-file-p
    (setq output-file (make-pathname :host (pathname-host input-file)
                                     :type "fasl" )))
  (setq output-file (pathname output-file))
  (setq output-file (merge-pathnames output-file input-file))
  output-file )
