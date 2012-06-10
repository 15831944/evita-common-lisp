;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;;
;;; Defsystem - System Construction Utility.
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d24-defsys.lisp#3 $
;;;
;;; Description:
;;;  Implements defsystem macro and functions.
;;;
;
(in-package :cl-user)

(defpackage :defsystem
  (:nicknames :ds)
  (:use :common-lisp)
  (:export
    #:defsystem
    #:compile-system
    #:load-system
    #:map-system
    #:clean-system
    #:concatenate-system
    #:touch-system
    #:find-system
    #:undefsystem
    #:*defaul-file-type* )

  #+clisp
  (:shadow
    cl:find-package
    cl:make-pathname
    cl:merge-pathnames ) )


(in-package :defsystem)


;;;; CLISP hacks.
;;;

#+clisp
;; find-package doesn't accept package object as argument.
(defun find-package (package)
    (if (packagep package)
        package
      (cl:find-package package)) )

#+clisp
;; make-pathname doesn't take host from defaults.
(defun make-pathname (&rest args &key defaults &allow-other-keys)
  (when defaults
    (setf (getf args :host) (pathname-host defaults)) )
  (apply #'cl:make-pathname args) )

#+clisp
;; (merge-pathnames "p-defs"
;;                  (make-pathname :type "lisp"
;;                                 :defaults "xcompiler:source;" ))
;; returns (:absolute) instead of (:absolute "source")
;;
(defun merge-pathnames (pathname &optional default version)
  (setq pathname (pathname pathname))
  (cl:merge-pathnames pathname default version) )

;;;;
;;;; Types
;;;;


;;;; Module structure.
;;;
;;; Description:
;;;  Base class for defsystem modules.
;;;
;;; Indicators of Plist:
;;;     :compile-satisfies-load
;;;     :concatenate-system-ignore
;;;     :force-depend-recompile
;;;
(defstruct module
  (name             nil :type (or symbol string))
  (pretty-name      ""  :type string)
  (filename         nil :type (or null string))
  (depends         '()  :type list)
  (state            nil :type (member nil :compiled :loaded))
  (source-date      0   :type integer)
  (output-date      0   :type integer)
  (image-date       0   :type integer)
  (plist            nil :type list) )


(defmethod print-object ((o module) s)
  (print-unreadable-object (o s :type t)
    (prin1 (module-pretty-name o) s) ) )


;;;; module-gorup
;
(defstruct (module-group (:include module))
  (modules          '() :type list) )


;;;; system
;
(defstruct (system (:include module-group))
  (source-pathname  nil :type (or pathname null)) )


(declaim (ftype (function (module) pathname)
    get-module-source-pathname
    get-module-output-pathname ) )

(declaim (ftype (function (system t t) ext:unspecified)
    update-state-system ) )

(declaim (ftype (function (symbol list) (values t t))
    operate-modules ) )

(declaim (ftype (function (module) t)
    output-newer-than-image-p
    source-newer-than-output-p ) )

(declaim (ftype (function ((or symbol string system)) system)
    ensure-system ) )

(declaim (ftype (function (symbol &optional t) (or system null))
    find-system ) )

(declaim (ftype (function () t)
  report-compilation ) )

;;;; *systems*
;;;
;;; Description:
;;;  List of systems.
;
(defvar *systems* '())


;;;; *warned-modules*
;;;
;;; Description:
;;;  List of modules which has compilation warnings.
;
(defvar *failed-modules* '())
(defvar *warned-modules* '())


;;;; Tracks Module Load Time variable.
;;;
;;; Description:
;;;  Hash table for mapping module output pathname to load time.
;;;
(defvar *image-date-table*
  (make-hash-table :test 'equal) )

(defvar *output-pathname-type*
  (pathname-type (compile-file-pathname "foo")) )

;;;; Flags For Operation. 
;;;
(defvar *no-warn*)
(defvar *silent*)
(defvar *simulate*)
(defvar *explain* nil)
(defvar *group-stack* '())


;;;; Module Options
;;;
(defvar *module-source-pathname* nil)
(defvar *module-output-pathname* nil)


(defun getf-pathname (options key default)
  (let ((pathname (getf options key default)))
    (when (null pathname)
      (error "Module must have ~S option." key) )
    (when (and default (not (eq pathname default)))
      (setq pathname (merge-pathnames pathname default)) )
    pathname ) )



(defmacro with-module-options ((module) &body body)
  (let ((module. (gensym))
        (options. (gensym)) )
    `(let* ((,module. ,module)
            (,options. (module-plist ,module.)) )

      (let ((*group-stack*
              (if (not (module-name ,module.))
                  *group-stack*
                (cons ,module. *group-stack*) ) )
            (*package*
              (ensure-package (getf ,options. :default-package *package*)) )
            (*module-source-pathname*
              (getf-pathname ,options.
                    :default-pathname
                    *module-source-pathname* ) )
            (*module-output-pathname*
              (getf-pathname ,options.
                    :default-output-pathname
                    *module-output-pathname* ) ) )
         ,@body ) ) ) )


;;;; Ensure Package.
;;;
(defun ensure-package (package)
  (if (packagep package)
      package
    (let ((name package))
      (setq package (find-package name))
      (unless package
        (error "No such package: ~A" (string name)) )
      package )) )


;;;; Explain What Defsystem Does.
;;;
(defun explain (control &rest args)
  (when *explain*
    (format t ";*~8T ~?~%" control args) ) )


;;;; File Load Date.
;;;
(defun file-load-date (pathname)
  (setq pathname (pathname pathname))
  (gethash pathname *image-date-table*) )


;;;; file-save-date
;;;
;;; Description:
;;;  Returns file-wite-date of specified pathname or nil.
;
(defun file-save-date (pathname)
  (ignore-errors (file-write-date pathname)) )

;;;
;;; Module Actions
;;;


;;;; Compile Module.
;;;
;;; Returns:
;;;   unspecified.
;;;
(defun compile-module (module)
  (let ((source (get-module-source-pathname module))
        (output (get-module-output-pathname module)) )

    (when *explain*
      (format t ";= compile-file ~S to ~S~%" source output) )

    (remhash output *image-date-table*)
    (setf (module-image-date module) 0)

    (unless *simulate*
      (ensure-directories-exist output)
      (multiple-value-bind (output warnings-p failure-p)
          (let ((*compile-verbose* nil)
                (*compile-print*   t) )
            (compile-file source :output-file output) )

        (cond
          (failure-p
            (push module *failed-modules*) )
          (warnings-p
            (push module *warned-modules*) ))

        (unless failure-p
          (setf (module-output-date module) (or (file-save-date output) 0)) )

        failure-p )) ) )


;;;; Load Module Into Memory.
;;;
;;; Returns:
;;;   unspecified.
;;;
(defun load-module (module)
  (let ((source (get-module-output-pathname module)))
    (when *explain*
      (format t ";= load ~S~%" source) )

    (setf (module-image-date module) (get-universal-time))

    (setf (module-output-date module)
          (or (file-save-date source) (get-universal-time)) )

    (unless *simulate*
      (let ((*load-verbose* nil)
            (*load-print*   nil)
            (xc::*environment* nil) )
        (load source) )

      (setf (gethash (get-module-output-pathname module) *image-date-table*)
            (get-universal-time) )) ) )


;;;; Get Module Output Filename.
;;;
(defun get-module-output-pathname (module)
  (merge-pathnames (pathname-name (module-filename module))
                   (make-pathname :type *output-pathname-type*
                                  :defaults *module-output-pathname* )) )


;;;; Get Module Source Filename.
;;;
(defun get-module-source-pathname (module)
  (merge-pathnames (module-filename module)
                   (make-pathname :type "lisp"
                                  :defaults *module-source-pathname* )) )


;;;; Operate On Module.
;;;
(defun operate-module (action module)
  (when (system-p module)
    (let ((pathname (system-source-pathname module)))
      (unless pathname
        (explain "Try to load system ~A." (module-pretty-name module))
        (load (format nil "SYS:SYSTEM;~A.SYSTEM" (module-name module)))
        (setq pathname (system-source-pathname module))
        (unless pathname
          (format t "; There is no definition for system ~A."
                  (module-pretty-name module) )
          (return-from operate-module (values nil t)) ))

      (when (and pathname
                (> (or (file-save-date pathname) 0)
                   (or (file-load-date pathname) 0) ))
        (explain "Load system from ~S" pathname)
        (load pathname :verbose nil :print nil)
        (update-state-system module nil nil) ) ))

  (ecase action
    ((:load)
      (when (eq (module-state module) :loaded)
        (explain "  Module ~S has already been loaded."
                 (module-pretty-name module) )
        (return-from operate-module nil) )

      (explain "Compile ~S for loading"
               (module-pretty-name module) )

      (multiple-value-bind (action-p failure-p)
          (operate-module :compile module)

        (when failure-p
          (return-from operate-module (values nil t)) )

        (etypecase module
          (module-group
            (let ((modules (module-group-modules module)))
              (with-module-options (module)
                (when (operate-modules :load modules)
                  (setq action-p t) )) ) )

          (module
            (when (output-newer-than-image-p module)
              (when (or *simulate* (not *silent*))
                  (format t ";;; ~{ ~*~} Load [~A] ~S~%"
                          *group-stack*
                          (module-pretty-name (first *group-stack*))
                          (module-pretty-name module) ))

                  (load-module module)
                  #+nil (setq action-p t) ) ))

          (setf (module-state module) :loaded)

          (values action-p nil) ) )

    ((:compile)
      (when (or (eq (module-state module) :loaded)
                (eq (module-state module) :compiled) )
        (explain "  Module ~S has already been ~(~A~)."
                 (module-pretty-name module)
                 (module-state module) )
        (return-from operate-module nil) )

      (multiple-value-bind (action-p failure-p)
          (when (module-depends module)
            (explain "Load~{ ~S~^,~#[~; and~]~} for compiling ~S."
                     (mapcar #'module-pretty-name (module-depends module))
                     (module-pretty-name module) )
            (operate-modules :load (module-depends module)) )

        (when failure-p
          (return-from operate-module (values nil t)) )

        (etypecase module
          (module-group
            ;; Compile group
            ;;
            (explain "Compile module ~S." (module-pretty-name module))
            (let ((modules (module-group-modules module)))
              (let ((depend-date
                      (reduce #'max (module-depends module)
                              :key #'module-output-date
                              :initial-value 0 ) ))
                (dolist (module modules)
                  (when (< (module-output-date module) depend-date)
                    (explain " Module ~S is older than dependees."
                             (module-pretty-name module) )
                    (setf (module-output-date module) 0) ) ) )

              (with-module-options (module)
                (when (operate-modules :compile modules)
                  (setq action-p t) ))

              (setf (module-output-date module)
                    (reduce #'max modules
                            :key #'module-output-date
                            :initial-value 0 )) ) )

          (module
            ;; Compile module
            ;;
            (dolist (depend (module-depends module))
              (unless (module-state depend)
                (explain "Can't compile module ~S, because of depedent module ~S isn't loaded."
                         (module-pretty-name module)
                         (module-pretty-name depend) )
                (return-from operate-module nil) )

              (unless action-p
                (when (> (module-output-date depend)
                         (module-output-date module) )
                  (explain "Module ~S will be compiled because dependent ~S is newer."
                           (module-pretty-name module)
                           (module-pretty-name depend) )
                  (setq action-p t) )) )

            (unless action-p
              (setq action-p (source-newer-than-output-p module))
              (when action-p
                (explain "Module ~S will be compiled because source is newer than output."
                         (module-pretty-name module) )))

            (when action-p
              (when (or *simulate* (not *silent*))
                (format t ";;; ~{ ~*~} Compile [~A] ~S~%"
                        *group-stack*
                        (module-pretty-name (first *group-stack*))
                        (module-pretty-name module) ))

                (compile-module module) ) ))

           (setf (module-state module) :compiled)

           (values action-p nil) ) )) )


;;;; Operate On Modules.
;;;
(defun operate-modules (action modules)
  (let ((action-p  nil)
        (failure-p nil) )
    (dolist (module modules)
      (multiple-value-bind (action1-p failure1-p)
          (operate-module action module)
        (when action1-p
         (setq action-p t) )
        (when failure1-p
         (setq failure-p t) ) ) )
    (values action-p failure-p) ) )


;;;; Is Output Newer Than Image?
;;;
(defun output-newer-than-image-p (module)
  (> (module-output-date module) (module-image-date module)) )


;;;; Is Source Newer Than Output?
;;;
(defun source-newer-than-output-p (module)
  (> (module-source-date module) (module-output-date module)) )

;;;
;;; System and Module
;;;


;;;; Compile System.
;;;
;;; Returns:
;;;   t   - If any actions a necessary to bring system up-to-date.
;;;   nil - If no actions are necessary.
;;;
(defun compile-system (system &key
                      (explain   nil)
                      (recompile nil)
                      (no-warn   nil)
                      (reload    nil)
                      (silent    nil)
                      (simulate  nil) )
  (setq system (ensure-system system))

  (update-state-system system recompile reload)

  (let ((*explain*        explain)
        (*no-warn*        no-warn)
        (*silent*         silent)
        (*simulate*       simulate)
        (*failed-modules* '())
        (*warned-modules* '()) )

      (multiple-value-bind (action-p failure-p)
          (with-compilation-unit ()
            (operate-module :compile system) )

        (report-compilation)

        (cond
          (failure-p
            (format t "; Failed to compile system ~S.~%"
                    (module-pretty-name system) ) )

          ((not action-p)
            (format t "; System ~S has already been compiled.~%"
                    (module-pretty-name system) ) )

          (*warned-modules*
            (format t "; System ~S is compiled with ~D warning~:P.~%"
                    (module-pretty-name system)
                    (length *warned-modules*) ) )
          (t
            (format t "; System ~S is successfully compiled.~%"
                    (module-pretty-name system) ) )) ) ) )


;;;; Ensure System.
;;;
(defun ensure-system (thing)
    (declare (values system))
  (labels (
    ;; global-load
    (global-load (filename)
      (let ((xc::*environment* nil))
        (load filename) ) )

    ;; try-load
    (try-load (filename)
      (and (probe-file filename) (global-load filename)) )
    )
    ;;
    ;; ensure-system
    ;;
    (typecase thing
      (symbol
        (ensure-system (symbol-name thing)) )
      (string
        (let* ((name (intern thing :keyword))
               (system (find-system name nil)) )
          (cond
            ((null system)
              (or (try-load (format nil "~A.system" name))
                  (try-load (format nil "SYS:SYSTEM;~A.SYSTEM" name)) )
              (setq system (find-system name t)) )
            ((system-source-pathname system)
              (setf (module-source-date system)
                (or (file-save-date (system-source-pathname system)) 0) ) ))
          system ) )
      (system thing)
      (otherwise
        (error "System name must be symbol or string: ~S" thing) )) ) )


;;;; Describe System.
;;;
;;; BUGBUG: NYI: describe-object
;;;
(defun describe-system (name)
  (let ((system (find-system name t))
        (module-level* '()) )
    (dolist (module (system-modules system))
      (let ((level 1))
        (dolist (depend (module-depends module))
          (setq level (max level (1+ (getf module-level* depend 0)))) )

        (setq module-level* (list* module level module-level*))

        (write-char #\;)
        (dotimes (i level)
          (write-char #\Space) )

      (format t "module ~A ~%" (module-pretty-name module)) ) )
   nil ) )


;;;; Find Module From System.
;;;
(defun find-module (name system)
  (declare (type system system))

  (etypecase name
    (symbol
      (find name (system-modules system) :key 'module-name) )

    (string
      (find name (system-modules system) :key 'module-filename
                                         :test #'string-equal ))) )

;;;; Find System.
;;;
(defun find-system (name &optional error-p)
  (declare (type symbol name))

  (let ((system (find name *systems* :key #'module-name)))
    (when (and (not system) error-p)
      (error "There is no system called ~S." name) )
    system ) )


;;;; Load System.
;;;
;;; Returns:
;;;   t   - If any actions a necessary to bring system up-to-date.
;;;   nil - If no actions are necessary.
;;;
(defun load-system (system &key
                    (explain  nil)
                    (reload   nil)
                    (no-warn  nil)
                    (silent   nil)
                    (simulate nil) )
  (setq system (ensure-system system))

  (update-state-system system reload reload)

  (let ((*explain*          explain)
        (*no-warn*          no-warn)
        (*silent*           silent)
        (*simulate*         simulate)
        (*failed-modules*   '())
        (*warned-modules*   '()) )
    (multiple-value-bind (action-p failure-p)
        (with-compilation-unit ()
          (operate-module :load system) )

      (report-compilation)

      (cond
        (failure-p
          (format t "; Failed to load system ~S.~%"
                  (module-pretty-name system) ) )

        ((not action-p)
          (format t "; System ~S is already loaded.~%"
                  (module-pretty-name system) ) )

        (*warned-modules*
          (format t "; System ~S is compiled with ~D warning~:P.~%"
                  (module-pretty-name system)
                  (length *warned-modules*) ) )
        (t
          (format t "; System ~S is successfully loaded.~%"
                  (module-pretty-name system) ) )) ) ) )


;;;; Reinitialize System.
;;;
;;; Description:
;;;  Resets state of system and its sub-modules and sub-systems.
;;;
(defun update-state-system (system recompile reload)
  (labels (
    ;; update-state-module
    (update-state-module (module)
      (setf (module-state module) nil)

      (setf (module-image-date  module)
            (if reload
                0
                (or (file-load-date (get-module-output-pathname module))
                    0 )))

      (setf (module-output-date module)
            (if (or recompile reload)
                0
                (or (file-save-date (get-module-output-pathname module))
                     0 )))

      (setf (module-source-date module)
            (or (file-save-date (get-module-source-pathname module))
                (progn
                  (format *error-output*
                        "; No such file ~S~%"
                        (get-module-source-pathname module) )
                  0 ))) )

    ;; update-state-group
    (update-state-group (group)
      (with-module-options (group)
        (setf (module-state group) nil)
        (setf (module-output-date group) 0)
        (dolist (module (system-modules group))
          (if (module-group-p module)
              (update-state-group module)
            (update-state-module module) ) ) ) )
    )
    ;;
    ;; reinitailize-system
    ;;
    (with-module-options (system)
      (update-state-group system) ) ) )


;;;;; report-compilation
;;;
;;; Called by:
;;;   compile-system
;;;   load-system
;
(defun report-compilation ()
  (when *failed-modules*
    (format t ";;; Failed to compile ~D module~:P:~{~%;;;~10T~A~}~%"
        (length *failed-modules*)
        (mapcar #'module-pretty-name *failed-modules*) ) )

  (when *warned-modules*
    (format t ";;; ~D module has compilation warnings~:P:~{~%;;;~10T~A~}~%"
        (length *warned-modules*)
        (mapcar #'module-pretty-name *warned-modules*) ) ) )

;;;;
;;;; Defsystem Macro Loader.
;;;;

;;;; Parse Module Specification function.
;;;
;;; Returns:
;;;     module
;;;
;;; Called by:
;;;     %defsystem
;;;
;;;
(defun parse-module (spec depends system)
  (when (and spec (symbolp spec))
    (let ((module (find-module spec system)))
      (when (eq module system)
        (error "System ~S contains itself." (system-pretty-name system)) )
      (unless module
        (setq module (find-system spec nil))
        (unless module
          (setq module (make-system :name spec
                                    :pretty-name (string spec) ))
          (push module *systems*) ))
      (push module (system-modules system))
      (setf (module-depends module) depends)
      (return-from parse-module (list module)) ))

  (when (stringp spec)
    (setq spec `(:module nil ,spec)) )

  (unless (consp spec)
    (error "Invalid module specification: ~S" spec) )

  (case (first spec)
    ((:module)
      (destructuring-bind (name element &rest options)
          (rest spec)

        (when (find-module (or name element) system)
          (error "Module ~S is specified more than once." (or name element)) )

        (typecase element
          (cons
            (let ((module (make-module-group :modules element
                                             :pretty-name (string name)
                                             :depends depends
                                             :plist options ) ) )
              (push module (system-modules system))
              (list module) ) )

          (string
            (let ((module (make-module :name name
                                       :pretty-name element
                                       :depends  depends
                                       :filename    element ) ) )
              (push module (system-modules system))
              (list module) ) )

          (symbol
            (let ((module (make-module :name name
                                       :pretty-name (string element)
                                       :depends depends
                                       :plist (list :system
                                                    (find-system element t)
                                                    options )) ) )
              (push module (system-modules system))
              (list module) ) )

          (otherwise
            (error "Bad :module element: ~S" element) )) ) )

    ((:serial)
      (when (null (rest spec))
        (error ":serial must have modules.") )

      (let ((siblings '()))
        (dolist (spec (rest spec))
          (let ((modules (parse-module spec (append siblings depends) system)))
            (setq siblings (nconc modules siblings)) ) )
        siblings ) )

    ((:parallel)
      (when (null (rest spec))
        (error ":parallel must have modules.") )

      (let ((siblings '()))
        (dolist (spec (rest spec))
          (let ((modules (parse-module spec depends system)))
            (setq siblings (nconc modules siblings)) ) )
        siblings ) )

    (otherwise
      (error "Unknown module specification: ~S" (first spec)) )) )


;;;; Load Defsystem function.
;;;
(defun %defsystem (name options specs)
  (when *load-verbose*
    (format t "~&;;; Load system ~S~%" name) )

  (let ((system (find-system name nil)))
    (unless system
      (setq system (make-system :name name))
      (push system *systems*) )

    (let ((pretty-name (getf options :pretty-name)))
      (if pretty-name
          (remf options :pretty-name)
        (setq pretty-name (string-capitalize name)) )
      (setf (module-pretty-name system) pretty-name) )

    (when *load-pathname*
      (setf (getf options :source-file) *load-pathname*) )

    (setf (system-plist system) options)

    ;; parse module specifications
    (setf (system-modules system) '())

    (dolist (spec specs)
      (parse-module spec '() system) )

    ;; Topological sort modules
    (let ((modules (copy-list (system-modules system))))
      (setf (system-modules system)
            (sort (system-modules system)
                  #'(lambda (x y)
                      (let ((x-p (find x (module-depends y)))
                            (y-p (find y (module-depends x))) )
                        (cond
                          ((and x-p y-p)
                            (error "Module ~S and ~S are mutaly depended."
                                   (module-pretty-name x)
                                   (module-pretty-name y) ) )

                          ((and (not x-p) (not y-p))
                           (> (position x modules) (position y modules)) )

                          (t
                            x-p )) ) ))) )

    (setf (system-source-pathname system) *load-truename*)

    (setf (system-source-date system)
       (if *load-truename*
           (file-write-date *load-truename*)
         (get-universal-time) ) )

    (setf (gethash (system-source-pathname system) *image-date-table*)
          (get-universal-time) )

    name ) )


;;;; Defsystem macro.
;;;
;
(defmacro defsystem (name options &rest modules)
  `(%defsystem ',name ',options ',modules) )


#|
(defsystem :defsystem
   (:default-output-pathname "defsystem:output;"
    :default-pathanme        "defsystem:source;" )
  "defsys" )
|#

#|
(defsystem :foo
           (:default-pathname         "foo:src"
            :default-compile-pathname "foo:bin" )
  (:serial
    "defs"
    (:parallel
      ("macros" :force-depend-recompile t)
      "macro-util" )
    (:parallel "funcs1" "funcs2" "main") ) )

(defsystem :foo
  (:default-output-pathname "./bin/")
  (:serial
    "defsys"
    (:parallel "allegro" "clisp")
    "port" ) )
|#
