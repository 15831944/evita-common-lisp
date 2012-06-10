;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 24 System Construction
;;; dev/d24-compile.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d24-compile.lisp#6 $
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

;;;; compile-error
;
(defun compile-error (datum &rest args)
    (declare (dynamic-extent args))
  (format t "; Compilation Error: ~?~%" datum args)
  (incf *compile-nfails*) )


;;;; compile-notice
;;;
;;; Description:
;;;   Notice During Load.
;;;
;;; BUGBUG: We should remember ftype.
;
(defun xc::compile-notice (operator name &optional lambda-list)
  (when (and *compile-file-pathname* *compile-print*)
    (case operator
      (defmethod
        (format t "; Compile ~S ~S ~S~%" operator name lambda-list) )
      (otherwise
        (when (eq operator 'defun)
          (when *undefined-functions*
            (setf (gethash name *undefined-functions*) t) ))
        (format t "; Compile ~S ~S~%" operator name) ))) )


;;;; compile-warn
;
(defun compile-warn (datum &rest args)
    (declare (dynamic-extent args))
  (format t "; Compilation Warning: ~?~%" datum args)
  (incf *compile-nwarns*) )


;;;; xc::funcall-with-compilation-unit
;
(defun xc::funcall-with-compilation-unit (options fn)
  (let ((*compile-level*
          (if (getf options :override)
              1
            (1+ *compile-level*) )) )
    (if (> *compile-level* 1)
        (funcall fn)
      (let ((xc::*environment*      (xc::make-environment))
            (*undefined-functions*  (make-hash-table :test #'equal)) )
        (multiple-value-prog1
            (funcall fn)
          (let* ((undefs *undefined-functions*)
                 (nundefs
                   (loop for callers being each hash-value of undefs
                       count (consp callers) ) ))
            (when (plusp nundefs)
              (loop
                for name being each hash-key of undefs
                    using (hash-value callers)
                when (consp callers)
                    do (format t ";    ~S~30T ~{~S~^, ~}~%" name callers)
                initially
                  (compile-warn ";~%; Threre are ~D undefined function~:P:~%"
                                nundefs )
                finally
                 (write-line ";") )) )) )) ) )


;;;; make-environment
;
(defun xc::make-environment ()
    (declare (values xc::environment))
  (let ((env (si::allocate-structure 'xc::environment)))
    (setf (si::.env-variables    env) (make-hash-table :test 'eq))
    (setf (si::.env-functions    env) (make-hash-table :test 'eq))
    (setf (si::.env-writers      env) (make-hash-table :test 'eq))

    (if (null xc::*environment*)
        (progn
          (setf (si::.env-outer   env) si::*environment*)
          (setf (si::.env-types   env) (make-hash-table :test 'eq))
          (setf (si::.env-classes env) (make-hash-table :test 'eq))
          (setf (si::.env-others  env) (make-hash-table :test 'eq))
          (setf (si::.env-local-p env) nil) )
        (progn
          (setf (si::.env-outer   env) xc::*environment*)
          (setf (si::.env-types   env) (si::.env-types   xc::*environment*))
          (setf (si::.env-classes env) (si::.env-classes xc::*environment*))
          (setf (si::.env-others  env) (si::.env-others  xc::*environment*))
          (setf (si::.env-local-p env) t) ))
    env ) )


;;;; notice-undefined-function
;;;
;;; Description:
;;;  Compiler calls this function when it compiles function form whose
;;;  operator is undefined.
;
(defun xc::notice-undefined-function (name caller)
  (let ((undefs *undefined-functions*))
    (when (hash-table-p undefs)
      (let ((callers (gethash name undefs)))
        (when (listp callers)
          (pushnew caller callers :test #'equal)
          (setf (gethash name undefs) callers) ) )) ) )


;;;; compile-file-compile
;;;
;;; BUGBUG: NYI: *compile-print*
;
(defun compile-file-compile (form fasd)
  (labels (
    ;; compile-ltv-form
    (compile-ltv-form (form)
      (let ((lambda-form
                `(lambda ()
                    (declare (ext:lambda-name (load-time-value function)))
                   ,form ) ))
        (compile-form lambda-form) ) )

    ;; compile-form
    (compile-form (form)
        (declare (values (or function null)))
      (multiple-value-bind (fn warnings-p failure-p cookies)
          (xc::compile-form form)
       (when warnings-p (incf *compile-nwarns*))
       (when failure-p  (incf *compile-nfails*))
       (loop
         with objtab = (slot-value fasd 'objtab)
         for (cookie . form) in cookies
         for load-fun = (compile-ltv-form form) do
           (setf (gethash cookie objtab) `(load-time-value ,load-fun nil)) )
       (unless failure-p fn) ) )
    )
    ;;
    ;; compile-file-compile
    ;;
    (let ((*compile-nwarns* 0)
          (*compile-nfails* 0) )

      #+nil
      (when *compile-print*
        ;; BUGBUG: Should use pretty printer
        (let ((line (with-output-to-string (stream) (prin1 form stream))))
          (if (< (length line) 70)
              (format t "~S~%" line)
            (format t "~S ..~%" (subseq line 0 70)) ) ))

      (cond
        ((not (consp form))
          ;; BUGBUG: NYI: style-warning
          (compile-warn "Ignore top-level literal form: ~S" form) )

        ((not (si::proper-list-p form))
          (compile-error "Malformed cons form: ~S" form) )

        ((xc::one-argument-form-p form 'in-package)
          (let ((package (second form)))
            (si::%in-package package)
            (fasd-write form fasd) ) )

        ((eq (first form) 'defun)
          (destructuring-bind (defun name lambda-list &body body) form
              (declare (ignore defun))
            (when *undefined-functions*
              (setf (gethash name *undefined-functions*) t) )
            (when *compile-print*
              (format t "; Compile DEFUN ~S~%" name) )

            (let ((fn (compile-form
                        `(labels ((,name ,lambda-list ,@body)) #',name) )))

              (when fn
                (setq fn (funcall fn))
                (fasd-write `(defun ,name ,lambda-list ,fn) fasd) ) ) ) )

        ((not (compile-file-defasm form fasd)) 
          (case (first form)
            ((defmethod)
              (destructuring-bind (defmethod name lambda-list &body body) form
                  (declare (ignore defmethod))
                  (declare (ignore lambda-list))
                  (declare (ignore body))
                (setf (gethash name *undefined-functions*) t) ) )

            ((defgeneric)
              (destructuring-bind (defgeneric name lambda-list &body body) form
                  (declare (ignore defgeneric))
                  (declare (ignore lambda-list))
                  (declare (ignore body))
                (setf (gethash name *undefined-functions*) t) ) ))

          (let ((fn (compile-form form)))
            (when fn (fasd-write `(funcall ,fn) fasd)) ) ))
      (values *compile-nwarns* *compile-nfails*) ) ) )


;;;; compile-file-defasm
;;;
;;; Stab version.
;
(defun compile-file-defasm (form fasd)
    (declare (ignore form fasd))
  nil )


;;;; 24.2.1 compile-file
;;;
;;; Syntax:
;;;     compile-file input-file &key ...
;;;       => output-truename, warning-p, failure-p
;
(defun cl:compile-file (input-file
                        &key (output-file     nil output-file-p)
                             (verbose         *compile-verbose*)
                             (print           *compile-print*)
                             (target          :evm)
                             (external-format :default) )

  (let ((end-of-file  '(end-of-file))
        (reader-error '(reader-error)) )
  (labels (
    ;; compile-form
    (compile-form (input form fasd)
      (handler-case (compile-file-compile form fasd)
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

    (unless (eq :default external-format)
      (error "Support only :defulat external-format.") )

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
          (si:*read-line-number-table*
              (make-hash-table :test 'eq) )
          (xc::*situation*          'compile-file)
          (xc::*target*             target)
          (output-truename          (truename output-file))
          (total-nwarns             0)
          (total-nfails             0) )
      (with-open-file (input input-file)
        (when verbose
          (format t "~%; Compiling ~S~%;      into ~S~%;~%"
                  input-file #+nil *compile-file-truename*
                  output-file #+nil output-truename ))

        (with-compilation-unit ()
          (let ((fasd  (fasd-open output-file))
                (abort-p t))
            (unwind-protect
                (loop
                  for form = (read-form input)
                  until (eq form end-of-file) do
                    (multiple-value-bind (nwarns nfails)
                         (if (eq form reader-error)
                             (values nil t)
                           (compile-form input form fasd) )
                      (clrhash si:*read-line-number-table*)
                      (incf total-nwarns nwarns)
                      (incf total-nfails nfails) )
                  finally
                    (when (zerop total-nfails) (setq abort-p nil)) )
              (when (and verbose abort-p)
                (format t "~&;*** Abort compilation of ~S~%"
                    input-file ))
              (fasd-close fasd abort-p) ) ))

        (when verbose
          (format t "~&; End of compilation of ~S~%"
                  input-file ) ))

      (values output-truename
              (if (zerop total-nwarns) nil total-nwarns)
              (if (zerop total-nfails) nil total-nfails) ) ) ) ) )


;;;; 24.2.2 compile-file-pathname
;
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
