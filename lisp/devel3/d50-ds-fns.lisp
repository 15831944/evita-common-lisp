;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;;
;;; Defsystem - System Construction Utility (Defsystem)
;;; lisp/devel/d50-ds-fns.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel3/d50-ds-fns.lisp#5 $
;;;
;;; Description:
;;;  Implements defsystem macro and functions.
;;;
;
(in-package :ds)

;;;; print-object file-module
(defmethod print-object ((o module) s)
  (print-unreadable-object (o s :type t :identity t))
  o )


;;;; print-object file-module
(defmethod print-object ((o lisp-module) s)
  (print-unreadable-object (o s :type t)
    (prin1 (and (slot-boundp o 'filename) (slot-value o 'filename)) s) )
  o )


;;;; print-object system-module
(defmethod print-object ((o system-module) s)
  (print-unreadable-object (o s :type t)
    (princ (slot-value o 'name) s) )
  o )


;;;; %defsystem
;;; Loader of defsystem macro.
(defun %defsystem (name option* form*)
  (let ((module-table (make-hash-table :test 'equal)))
  (labels (
    ;; intern-sysmod
    (intern-sysmod (name)
      (let ((sysmod (or (find-system name nil nil) (make-sysmod name))))
        sysmod ) )

    ;; make-sysmod
    (make-sysmod (name)
      (let ((sysmod (make-instance 'system-module)))
        (setf (slot-value sysmod 'name)    name)
        (setf (find-system name) sysmod) ) )

    ;; parse-configs
    (parse-configs (form*)
      (let ((configs '()) (modspec nil))
        (dolist (form form* (values configs modspec))
          (case (and (consp form) (first form))
            ((:config)
              (let ((name     (second form))
                    (options  (cddr form)) )
                (setf (getf configs name) options) ) )
            (otherwise
              (when modspec
                 (error "Toplevel module specification is appeared more than once.") )
              (setq modspec form) )) ) ) )

    ;; parse-module
    (parse-module (modspec depend)
      (typecase modspec
        (string (parse-module/string modspec depend))
        (symbol (parse-module/symbol modspec depend))
        (cons   (parse-module/cons   modspec depend))
        (otherwise (error "Invalid module specification: ~S" modspec)) ) )

    ;; parse-module/cons
    (parse-module/cons (modspec depend)
      (case (first modspec)
        ((:module)
          (parse-module/cons/module modspec depend) )
        ((:parallel)
          (let ((mods '()))
            (dolist (modspec1 (rest modspec))
              (push (parse-module modspec1 nil) mods) )
            (make-instance 'parallel-module
                :members (nreverse mods)
                :depend  depend ) ) )
        ((:serial)
          (dolist (modspec1 (rest modspec) depend)
            (setq depend (parse-module modspec1 depend)) ) )
        (otherwise
          (error "Invalid module specification: ~S" modspec) )) )

    ;; parse-module/cons/module
    (parse-module/cons/module (modspec depend)
      (multiple-value-bind (name filename options)
        (if (stringp (second modspec))
            (values nil (second modspec) (cddr modspec))
          (values (second modspec) (third modspec) (cdddr modspec)) )
        (let* ((key (or name filename))
               (present (gethash key module-table)) )
          (when present (error "Module ~S appeared more than once." key))
          (setf (gethash key module-table)
            (make-module name filename options depend) ) ) ) )

    ;; make-module
    (make-module (name filename options depend)
      (typecase filename
        (string
          (make-instance (getf option* :module-class 'lisp-module)
              :name     name
              :filename filename
              :depend   depend
              :options  options ) )
        (symbol
          (let* ((key (intern (symbol-name filename) :keyword))
                 (mod (intern-sysmod key)) )
            (make-instance 'subsystem-module
                :name           name
                :system-module  mod
                :depend         depend
                :options        options ) ) )) )

    ;; parse-module/string
    (parse-module/string (filename depend)
      (parse-module `(:module nil ,filename) depend) )

    ;; parse-module/sysmbol
    (parse-module/symbol (sysname depend)
      (parse-module `(:module nil ,sysname) depend) )

    ;; validate-name
    (validate-name (name)
      (if (symbolp name)
          (intern (symbol-name name) :keyword)
        (error "System name must be a symbol: ~S" name) ) )

    )
  (let* ((name (validate-name name))
         (sysmod (intern-sysmod name)) )
    (setf (slot-value sysmod 'options) option*)
    (multiple-value-bind (configs modspec)
        (parse-configs form*)
      (setf (slot-value sysmod 'configs) configs)
      (setf (slot-value sysmod 'depend) (parse-module modspec nil)) )

    (setf (slot-value sysmod 'core-filename) *load-truename*)
    (setf (slot-value sysmod 'core-datetime) (get-universal-time))

    sysmod ) ) ) )


;;;; clean-system
(defun ds:clean-system (name &rest options)
  (let ((sysmod (find-system name)))
    (if (eq (load-sysdef sysmod) :failed)
        :failed
      (clean-action sysmod (make-context sysmod options)) ) ) )


;;;; compile-system
(defun ds:compile-system (name &rest options)
  (labels (
    ;; process
    (process (sysmod)
      (let ((*features* (append (getf options :features) *features*))
            (*compile-verbose* nil)
            (*compile-print* nil)
            (*load-verbose* nil)
            (*load-print* nil) )
        (with-compilation-unit ()
          (compile-action sysmod (make-context sysmod options)) ) ) )
    )
  (let ((sysmod (find-system name)))
    (if (eq (load-sysdef sysmod) :failed)
        (progn
          (format t "; Failed to load definition of ~A.~%" name)
          :failed )
      (let ((result (process sysmod)))
        (ecase result
          ((nil)
            (format t "; System ~A is up-to-date.~%" name) )
          ((:failed)
            (format t "; Failed to compile system ~A.~%" name) )
          ((:succeeded)
            (format t "; Compile system ~A succeeded.~%" name) ))
        result )) ) ) )


;;;; find-system
(defun ds:find-system (name &optional (errorp t) (loadp t))
  (labels (
    ;; load-sysdef
    ;; FIXME 2007-04-09: Should we wrap function LOAD with with-compilation-
    ;; unit with override to avoid compilation context dependency of
    ;; loading system definition file?
    (load-sysdef (key)
      (ignore-errors (load (format nil "sys:system;~A.system" key))) )
    )
  (let ((key
          (etypecase name
            (string (intern name :keyword))
            (symbol (intern (symbol-name name) :keyword)) ) ))
    (or (gethash key *system-modules*)
        (when (and loadp (not (eq (load-sysdef name) :failed)))
          (gethash key *system-modules*) )
        (when errorp (error "No such system module ~A." key)) ) ) ) )


(defun (setf ds:find-system) (sysmod name &optional errorp loadp)
    (declare (ignore errorp loadp))
  (let ((key
          (etypecase name
            (string (intern name :keyword))
            (symbol (intern (symbol-name name) :keyword)) ) ))
    (setf (gethash key *system-modules*) sysmod) ) )


;;;; load-sysdef
;;; Description:
;;;  Loads system definition file of specified system if needed.
(defun load-sysdef (sysmod)
    (declare (values t))
    (declare (type system-module sysmod))
  (cond
    ((null (slot-value sysmod 'core-filename))
      #+nil "System is defined by interactive session."
      nil )
    ((>= (slot-value sysmod 'core-datetime)
         (file-datetime (slot-value sysmod 'core-filename)) )
      #+nil "System is loaded."
      nil )
    ((load (slot-value sysmod 'core-filename) :if-does-not-exist nil)
      :succeeded )
    (t
      :failed )) )


;;;; load-system
(defun ds:load-system (name &rest options)
  (labels (
    ;; process
    (process (sysmod)
      (let ((*features* (append (getf options :features) *features*))
            (*compile-verbose* nil)
            (*compile-print* nil)
            (*load-verbose* nil)
            (*load-print* nil) )
        (with-compilation-unit ()
          (load-action sysmod (make-context sysmod options)) ) ) )
    )
  (let ((sysmod (find-system name)))
    (if (eq (load-sysdef sysmod) :failed)
        (progn
          (format t "; Failed to load system definition for ~A.~%" name)
          :failed )
      (let ((result (process sysmod)))
        (ecase result
          ((nil)
            (format t "; System ~A is up-to-date.~%" name) )
          ((:failed)
            (format t "; Failed to compile system ~A.~%" name) )
          ((:succeeded)
            (format t "; Load system ~A succeeded.~%" name) ))
        result )) ) ) )


;;;; ds:map-system
(defun ds:map-system (operation sysmod)
    (declare (type system-module sysmod))
    (declare (type (or function symbol) operation))
  (map-module-action operation sysmod) )


;;;; make-context
(defun make-context (sysmod options)
  (labels (
    (normalize ()
      (let* ((configs (slot-value sysmod 'configs))
             (config  (getf options :config :debug))
             (options (append options
                              (getf configs config)
                              (getf configs :default) )) )
        (list*
          :system-module sysmod
          :config       (getf options :config :debug)
          :features     `(,config ,@(getf options :features))
          :default-output-pathname
            (truename (or (getf options :default-output-pathname)
                          (make-pathname) ))
          :default-source-pathname
            (truename (or (getf options :default-source-pathname)
                          (make-pathname) ))
          :execute     (getf options :execute t)
          :verbose     (getf options :verbose t)
          options ) ) )
    )
    (normalize) ) )


;;;; explain
(defun explain (options format &rest args)
  (when (getf options :explain)
    (format t "; ~6T ~?~%" format args) ) )


;;;; file-datetime
(defun file-datetime (filename)
  (or (ignore-errors (file-write-date filename)) 0) )


;;;; make-subcontext
(defun make-subcontext (mod options)
  (labels (
    (normalize (sysmod)
      (let* ((configs (slot-value sysmod 'configs))
             (config  (getf options :config :debug))
             (options (append (getf configs config)
                              (getf configs :default)
                              options )) )
        (list*
            :system-module sysmod
            :default-output-pathname
                (truename (getf options :default-output-pathname))
            :default-source-pathname
                (truename (getf options :default-source-pathname))
            options ) ) )
    )
    (normalize (slot-value mod 'system-module)) ) )


;;;; message
(defun message (options format &rest args)
  (when (getf options :verbose)
    (format t "; ~?~%" format args)
    (force-output) ) )


;;;; result
(defun result (result1 result2)
    (declare (values result))
    (declare (type result result1 result2))
  (cond
    ((eq result1 :failed) result1)
    ((eq result2 :failed) result2)
    (t (or result1 result2)) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Module Operation
;;;

;;;; clean-action lisp-module
(defmethod clean-action ((mod lisp-module) options)
  (labels (
    ;; do-clean
    (do-clean (filename)
      (cond
        ((eql (file-datetime filename) 0)
          nil )
        ((progn
            (message options "delete ~S" filename)
            (ignore-errors (delete-file filename)) )
          :succeeded )) )
    )
    ;;
    (clean-action (slot-value mod 'depend) options)
    (do-clean (output-filename mod options)) ) )


;;;; clean-action module
(defmethod clean-action ((mod module) options)
  (clean-action (slot-value mod 'depend) options) )

;;;; clean-action parallel-module
(defmethod clean-action ((mod parallel-module) options)
  (call-next-method)
  (dolist (mod1 (slot-value mod 'members))
    (clean-action mod1 options) ) )


;;;; compile-action lisp-module
(defmethod compile-action ((mod lisp-module) options)
    (declare (values result))
  (labels (
    ;; compile-aux
    (compile-aux (srcfile outfile)
      (ensure-directories-exist
        (make-pathname :directory (pathname-directory outfile)) )
      (multiple-value-bind (out warning-p failure-p)
          (compile-file srcfile :output-file outfile)
          (declare (ignore out warning-p))
        (not failure-p) ) )

    ;; do-compile
    (do-compile ()
      (let ((filename (source-filename mod options)))
        (message options "Compile ~S" (file-namestring filename))
        (cond
          ((not (getf options :execute))
            :succeeded )
          ((compile-aux filename (output-filename mod options))
            :succeeded )
          (t
            (message options "Compilation failed: ~S" filename)
            :failed )) ) )

    ;; need-compile-p
    ;; FIXME 2007-04-09: If we has millsecond clock, we can use
    ;; greater than instead of >=.
    (need-compile-p (src)
      (or
          (when (getf options :force)
              (explain options "Force recompile ~S"
                (slot-value mod 'filename) )
              :force )
          (when (> (file-datetime src)
                   (file-datetime (output-filename mod options)) )
            (explain options "Source is newer than FASL file: ~S"
            (slot-value mod 'filename) )
            :newer )) )
    )
  (let ((src (source-filename mod options)))
    (if (eql (file-datetime src) 0)
        (progn
          (message options "No source file: ~S" src)
          :failed )
      (ecase (load-action (slot-value mod 'depend) options)
        ((:failed)
          :failed )
        ((nil)
          (cond
            ((not (getf (slot-value mod 'options) :compile t))
              (explain options "We don't need to compile ~S."
                (slot-value mod 'filename) ) )
            ((need-compile-p src)
              (do-compile) )
            (t
              (explain options "~S is up-to-date."
                (slot-value mod 'filename) ) )) )
        ((:succeeded)
          (if (not (getf (slot-value mod 'options) :compile t))
              (explain options "We don't need to compile ~S."
                (slot-value mod 'filename) )
            (do-compile) ) ))) ) ) )


;;;; compile-action module
(defmethod compile-action ((mod module) options)
  (load-action (slot-value mod 'depend) options) )


;;;; compile-action parallel-module
(defmethod compile-action ((mod parallel-module) options)
  (labels (
    (compile-aux (options result)
      (dolist (mod1 (slot-value mod 'members) result)
        (setq result (result (compile-action mod1 options) result)) ) )
    )
    ;;
   (ecase (call-next-method)
     ((:failed) :failed)
     ((:succeeded)
       (explain options "We need to recompile members.")
       (compile-aux (list* :force t options) :succeeded) )
     ((nil) (compile-aux options nil)) ) ) )


;;;; compile-action subsystem-module
(defmethod compile-action ((mod subsystem-module) options)
  (labels (
    (compile-aux (options result)
      (result 
        (compile-action
          (slot-value mod 'system-module)
          (make-subcontext mod options) )
        result ) )
    )
    ;;
    (ecase (call-next-method)
      ((:failed) :failed)
      ((:succeeded) (compile-aux (list* :force t options) :succeeded))
      ((nil) (compile-aux options nil)) ) ) )


;;;; compile-action system-module
(defmethod compile-action ((mod system-module) options)
  (compile-action (slot-value mod 'depend) options) )


;;;; load-action lisp-module
(defmethod load-action ((mod lisp-module) options)
  (labels (
    ;; load-aux
    (load-aux (filename)
      (ignore-errors (load filename :if-does-not-exist nil)) )

    ;; load-filename
    (load-filename ()
      (if (getf (slot-value mod 'options) :compile t)
          (output-filename mod options)
        (source-filename mod options) ) )

    ;; do-load
    (do-load (succeeded)
      (let ((filename (load-filename)))
        (message options "Load ~S" (file-namestring filename))
        (cond
          ((not (getf options :execute))
            succeeded )
          ((load-aux filename)
            (setf (slot-value mod 'core-filename) filename)
            (setf (slot-value mod 'core-datetime) (get-universal-time))
            succeeded )
          (t
            (message options "Failed to load ~S" filename)
            (setf (slot-value mod 'core-datetime) 0)
            :failed )) ) )

    ;; need-load-p
    (need-load-p ()
      (let ((filename (load-filename)))
        (or (when (getf options :force)
              (message options "force load ~S"
                (slot-value mod 'filename) )
              :force )
            (when (null (slot-value mod 'core-filename))
              (explain options "not loaded so far ~S"
                (slot-value mod 'filename) )
              :not-loaded-so-far )
            (when (string/= (namestring (slot-value mod 'core-filename))
                            (namestring filename) )
              (explain options "different config is loaded ~S"
                (slot-value mod 'filename) )
              :another )
            (when  (> (file-datetime filename)
                      (slot-value mod 'core-datetime) )
              (explain options "~S is changed."
                  (slot-value mod 'filename) )
              :changed )) ) )
    )
  (ecase (compile-action mod options)
    ((:failed)
      :failed )
    ((nil)
      (if (need-load-p)
          (do-load nil)
        (explain options "~S is already loaded."
            (slot-value mod 'filename) )) )
    ((:succeeded)
      (do-load :succeeded) )) ) )


;;;; load-action module
(defmethod load-action ((mod module) options)
  (load-action (slot-value mod 'depend) options) )


;;;; load-action parallel-module
(defmethod load-action ((mod parallel-module) options)
  (labels (
    (load-aux (options result)
      (dolist (mod1 (slot-value mod 'members) result)
        (setq result (result (load-action mod1 options) result)) ) )
    )
    ;;
   (ecase (call-next-method)
     ((:failed) :failed)
     ((:succeeded) (load-aux (list* :force t options) :succeeded))
     ((nil) (load-aux options nil)) ) ) )


;;;; load-action subsystem-module
(defmethod load-action ((mod subsystem-module) options)
  (labels (
    (load-aux (options result)
      (result
        (load-action 
          (slot-value mod 'system-module)
          (make-subcontext mod options) )
        result ) )
    )
    ;;
    (ecase (call-next-method)
      ((:failed) :failed)
      ((:succeeded) (load-aux (list* :force t options) :succeeded))
      ((nil) (load-aux options nil)) ) ) )


;;;; map-module-action module
(defmethod map-module-action (operation (mod module))
  (map-module-action operation (slot-value mod 'depend))
  (funcall operation mod) )


;;;; map-module-action parallel-module
(defmethod map-module-action (operation (mod parallel-module))
  (map-module-action operation (slot-value mod 'depend))
  (dolist (mod1 (slot-value mod 'members))
    (map-module-action operation mod1) )
  (funcall operation mod) )


;;;; map-module-action subsystem-module
(defmethod map-module-action (operation (mod subsystem-module))
  (map-module-action operation (slot-value mod 'depend))
  (map-module-action operation (slot-value mod 'system-module))
  (funcall operation mod) )


(defmethod source-filename ((mod lisp-module) options)
  (merge-pathnames
    (merge-pathnames
        (translate-logical-pathname (slot-value mod 'filename))
        (make-pathname :type "lisp") )
    (getf options :default-source-pathname) ) )


(defmethod output-filename ((mod lisp-module) options)
  (merge-pathnames (compile-file-pathname
                      (translate-logical-pathname (slot-value mod 'filename)) )
                   (getf options :default-output-pathname) ) )
