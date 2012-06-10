;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;;
;;; Defsystem - System Construction Utility.
;;; lisp/devel/d24-ds-fns.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel3/d50-ds-defs.lisp#1 $
;;;
;;; Description:
;;;  Implements defsystem macro and functions.
;;;
;
(in-package :cl-user)

(defpackage :defsystem
  (:nicknames :ds)
  (:use :common-lisp :extension)
  (:export
    #:clean-system
    #:compile-system
    #:defsystem
    #:find-system
    #:load-system
    #:map-system
    #:link-system
    #:touch-system
    #:undefsystem
    #:*defaul-file-type* ) )

(in-package :defsystem)

(deftype result () '(member nil :failed :succeeded))


;;;; module
(defclass module ()
  ((name            :type symbol :initform nil :initarg :name)
   (depend          :type (or module null) :initform nil :initarg :depend)
   (options         :type list :initform '() :initarg :options) )
  (:metaclass structure-class) )


;;;; loadable-module
(defclass loadable-module (module)
  ((core-datetime   :type unsigned-byte :initform 0)
   (core-filename   :type (or pathname null) :initform nil) )
  (:metaclass structure-class) )


;;;; lisp-module
(defclass lisp-module (loadable-module)
  ((filename        :type pathname :initarg :filename))
  (:metaclass structure-class) )


;;;; parallel-module
(defclass parallel-module (module)
  ((members         :type list :initform '() :initarg :members))
 (:metaclass structure-class) )


;;;; system-module
;;; filename
;;;  Pathname of system definition file or NIL if system is defined by
;;;  interactive session.
(defclass system-module (loadable-module)
  ((configs     :type list :initform '()))
  (:metaclass structure-class) )


;;;; subsystem-module
(defclass subsystem-module (module)
  ((system-module :type system-module :initarg :system-module))
 (:metaclass structure-class) )


(defvar *system-modules* (make-hash-table :test 'eq))

(declaim (ftype (function ((or symbol string) &optional t t)
                          (or system-module null) )
    ds:find-system ) )

(declaim (ftype (function ((or system-module null) (or symbol string)
                            &optional t t)
                          (or system-module null) )
    (setf ds:find-system) ) )

(declaim (ftype (function ((or function symbol) system-module) t)
    ds:map-system ) )


(declaim (ftype (function (pathname) unsigned-byte) file-datetime))

(declaim (ftype (function (system-module list) list)
    make-context ) )

(declaim (ftype (function (subsystem-module list) list)
    make-subcontext ) )

(declaim (ftype (function (list string &rest t) unspecified) explain message))
(declaim (ftype (function (function list) result) process-modules))
(declaim (ftype (function (result result) result) result))
(declaim (ftype (function (symbol list list) symbol) %defsystem))
(declaim (ftype (function (system-module) t) load-sysdef))
(declaim (ftype (function (function system-module) result) process-system))


(defgeneric clean-action (module options)
    (:method (module options) (declare (ignore module options)) nil) )

(defgeneric compile-action (module options)
    (:method (module options) (declare (ignore module options)) nil) )

(defgeneric load-action (module options)
    (:method (module options) (declare (ignore module options)) nil) )

(defgeneric map-module-action (operation module)
    (:method (op module) (declare (ignore op module)) nil) )

(defgeneric output-filename (module options))
(defgeneric source-filename (module options))

;;;; defsystem macro
(defmacro defsystem (name (&rest option*) &rest module*)
  `(%defsystem ',name ',option* ',module*) )
