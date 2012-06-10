;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 25 Environment - CMDL
;;; dev/d25-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-defs.lisp#2 $
;;;
;;; Description:
;;;  This file contains macros for command-loop command:
;;;
;;;  Macro:
;;;     defcommand
;;;
;;;  Functions:
;;;     add-command
;;;     find-command
;;;
;
(in-package :devel)

;;;; *commands*
;;;
;;; Description:
;;;  Command table.
;
(defvar *commands* (make-hash-table :test #'eq))


;;;; Variables updated by CMDL
;
(ext:deftlv -   nil)
(ext:deftlv *   nil)
(ext:deftlv **  nil)
(ext:deftlv *** nil)
(ext:deftlv +   nil)
(ext:deftlv ++  nil)
(ext:deftlv +++ nil)
(ext:deftlv /   nil)
(ext:deftlv //  nil)
(ext:deftlv /// nil)


;;;; command
;
(defstruct (command (:predicate commandp))
  (names    '() :type list)
  (help     nil :type (or null string))
  (reqs     '() :type list)
  (opts     '() :type list)
  (rests    '() :type list)
  (function nil :type function) )


;;;; disassemble-funciton
;
(defgeneric disassemble-function (fn)
  (declare (values ext:unspecified)) )


;;;; defcommand
;;;
;;; Syntax:
;;;     defcommand name (lambda-list) [doc-string] decl* [aliases] form*
;;;     aliases ::= :alias {string | (string+)
;;;
;;; Description:
;;;  Parses defcommand lambda-list.
;;;
;;;
;
(defmacro defcommand (name lambda-list &body body)
  (let ((requireds   '())
        (optionals   '())
        (rests       '())
        (keywords    '())
        (auxiliaries '())
        (names       '())
        (aliases     '())
        (decl*       '())
        (doc-string  nil)
        (form*       '())
        scan
        function )
  (labels (
    (check-name (name)
      (unless (and name (symbolp name))
        (error "Invalid parameter name: ~S" name) )
      (when (member name names :test #'eq)
        (error "Parameter ~S is appeared more than once." name) )
      (push name names)
      name )

    (check-spec (spec)
      (unless (eql (si::safe-list-length spec) 2)
        (error "Invalid parameter specifier: ~S" spec) )
      (unless (member (first spec) '(eval quote string) :test #'eq)
        (error "Invalid parameter quailifier: ~S" (first spec)) )
      (check-name (second spec)) )
    )
    ;;
    ;; parse-defcommand-lambda-list
    ;;
    (multiple-value-setq (scan requireds optionals rests keywords auxiliaries)
        (xc::analyze-lambda-list lambda-list) )

    (when scan
      (error "Invalid lambda-list: ~S" lambda-list) )

    (multiple-value-setq (decl* form* doc-string) (xc::analyze-body body t))

    (when (eq :alias (first form*))
      (pop form*)
      (let ((alias (pop form*)))
        (setq aliases (if (consp alias) alias (list alias))) ) )

    (setq lambda-list '())

    ;; requireds
    ;;
    (loop for scan on requireds
          for spec = (car scan)
          if (consp spec) do
            (push (check-spec spec) lambda-list)
          else do
            (push (check-name spec) lambda-list)
            (setf (car scan) (list 'eval spec))
          end )

    ;; optional
    ;;
    (when optionals
      (push (pop optionals) lambda-list)
      (loop for scan on optionals
            for spec = (car scan)
            if (not (consp spec)) do
              (push (check-name spec) lambda-list)
              (setf (car scan) (list 'eval spec))

            ;; (eval name) or (quote name)
            else if (and (eql (si::safe-list-length spec) 2)
                         (or (eq (car spec) 'eval)
                             (eq (car spec) 'quote)
                             (eq (car spec) 'string) )) do
              (push (check-name (second spec)) lambda-list)

            ;; ((qualifier name) initform svar)
            else if (consp (first spec)) do
              (check-spec (first spec))
              (push (list* (second (first spec)) (cdr spec)) lambda-list)
              (setf (car scan) (first spec))

            ;; (name initform svar)
            else do
              (check-name (first spec))
              (push spec lambda-list)
              (setf (car scan) (list 'eval (car spec)))
            end ) )

    (when rests
      (push '&rest lambda-list)
      (push (second rests) lambda-list)
      (setq rests (list (second rests))) )

    (when (and keywords (null rests))
      (setq rests (list '&key)) )

    (setq lambda-list (nreconc rests       lambda-list))
    (setq lambda-list (nreconc keywords    lambda-list))
    (setq lambda-list (nreconc auxiliaries lambda-list))

    (setq lambda-list (nreverse lambda-list))

    (setq function
      `#'(lambda ,lambda-list
            (declare (ext:lambda-name (command ,name)))
            ,@decl*
            (block ,name ,@form*) ))

    `(progn
       (eval-when (:compile-toplevel)
         (xc::compile-notice 'defcommand ',name) )
       (%defcommand '(,name ,@aliases)
                    '(,requireds ,optionals ,rests ,keywords)
                    ,doc-string
                    ,function )) ) ) )


;;;; %defcommand
;
(defun %defcommand (names argspec doc-string function)
  (add-command names argspec doc-string function) )


;;;; add-command
;
(defun add-command (names args doc-string function)
  (let ((command nil))
    (unless (consp names) (setq names (list names)))

    (loop for scan on names
          for name = (first scan)
          unless (keywordp name)
             do (setf (first scan) (intern (symbol-name name)
                                           #.(symbol-package :key))) )

    ;; Does command already exist?
    (dolist (name names)
      (let ((present (find-command name)))
        (cond
          ((not present))
          ((not command)
            (setq command present) )
          ((eq present command)
            )
          (t
            (error 'command-already-exists
                   :name name
                   :command command ))) ) )

    ;; Unregister previous names.
    (if (not command)
        (setq command (make-command :function function))
      (dolist (name (command-names command))
        (remhash name *commands*) ))

    (setf (command-names    command) names)
    (setf (command-reqs     command) (first  args))
    (setf (command-opts     command) (second args))
    (setf (command-rests    command) (third  args))
    (setf (command-function command) function)
    (setf (command-help     command) doc-string)

    ;; Register
    (dolist (name names)
      (setf (gethash name *commands*) command) )

    (first names) ) )


;;;; find-command
;
(defun find-command (name &optional error-p)
  (when (commandp name)
    (return-from find-command name) )
  (unless (keywordp name)
    (setq name (intern (string-upcase (string name))
                       #.(symbol-package :key) )) )
  (let ((command (gethash name *commands*)))
    (when (and (not command) error-p)
      (error 'undefined-command :name name) )
    command ) )


;;;; find-internal-function
;;;
;;; For: disassemble, documentation, step, and trace.
;
(defgeneric find-internal-function (fname fn)
    (declare (type function fn))
    (declare (values (or function null))) )
