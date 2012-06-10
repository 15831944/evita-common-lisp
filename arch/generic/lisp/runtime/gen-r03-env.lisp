;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 3 Evaluation and Compilation - proclaim
;;; devel3/d03-proclaim.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d03-proclaim.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     proclaim
;
(in-package :si)

;;;; find-declaration
(defun find-declaration (name &optional (error-p t) env)
  (let ((env (toplevel-environment env)))
    (with-latch ((ref environment latch env) :shared)
      (let ((alist (gethash/eq name (ref environment others env))))
        (cond
          ((assoc 'declaration alist))
          ((not error-p) nil)
          (t (error "There is no such declaration ~S." name)) ) ) ) ) )


;;;; (setf find-declaration)
(defun (setf find-declaration) (val name &optional error-p env)
    (declare (ignore error-p))
  (let ((env (toplevel-environment env)))
    (with-latch ((ref environment latch env))
      (let* ((htb (ref environment others env))
             (alist (gethash/eq name htb)) )
        (unless (assoc 'declaration alist)
          (setf (gethash/eq name htb) val) )
        val ) ) ) )


;;;; fun-info
(defun fun-info (fname key &optional env)
  (let ((env (or env *environment*))
        (name
          (etypecase fname
            (symbol fname)
            (setf-cell fname)
            (function-name (intern-setf-cell (second fname))) ) ))
    (loop
      (check-type env environment)
      (with-latch ((ref environment latch env) :shared)
        (let ((entry (gethash/eq name (ref environment functions env))))
          (when entry (return (values (cdr (assoc key (cdr entry))) t))) )
        (setq env (ref environment outer env)) )
      (when (null env) (return (values nil nil))) ) ) )


;;;; var-info
(defun var-info (name key &optional env)
  (check-type name symbol)
  (let ((env (or env *environment*)))
    (loop
      (check-type env environment)
      (with-latch ((ref environment latch env) :shared)
        (let ((entry (gethash/eq name (ref environment variables env))))
          (when entry (return (values (cdr (assoc key (cdr entry))) t))) )
        (setq env (ref environment outer env)) )
      (when (null env) (return (values nil nil))) ) ) )


(labels (
  (set-info (env htb name kind key val)
    (with-latch ((ref environment latch env))
      (let ((entry (gethash/eq name htb)))
        (cond
          ((null entry)
            (when val
              (setf (gethash/eq name htb) `(,kind (,key . ,val))) ) )
          ((or val (eq kind :constant))
            (when kind (setf (car entry) kind))
            (let ((cons (assoc key (cdr entry))))
              (if cons
                  (setf (cdr cons) val)
                (setf (cdr entry) (acons key val (cdr entry))) ) ) )
          (t
            (when (eq key kind) (setf (car entry) nil))
            (let ((cons (assoc key (cdr entry))))
              (when cons (setf (cdr cons) nil)) ) ))
        val ) ) )
    )
    ;;;; (setf fun-info)
    (defun (setf fun-info) (val fname key &optional env)
      (let ((env   (or env *environment*))
            (name
              (etypecase fname
                (symbol fname)
                (setf-cell fname)
                (function-name (intern-setf-cell (second fname))) ) )
            (kind
              (case key
                ((:macro) key)
                ((:special-operator) key)
                ((:compiler-macro) nil)
                (otherwise :function) ) ))
        (set-info
            env (ref environment functions env)
            name kind key val ) ) )

    ;;;; (setf var-info)
    (defun (setf var-info) (val name key &optional env)
        (check-type name symbol)
      (let ((env  (or env *environment*))
            (kind
              (case key
                ((:special) key)
                ((:constant) key)
                ((:symbol-macro) key)
                (otherwise nil) ) ))
        (set-info
            env (ref environment variables env)
            name kind key val ) ) )
  ) ; labels


;;;; runtime-environment-p
(defun runtime-environment-p (env)
  (check-type env environment)
  (null (ref environment outer env)) )


;;;; toplevel-environment
(defun toplevel-environment (env)
  (let ((env (or env *environment*)))
    (loop
      (check-type env environment)
      (when (toplevel-environment-p env) (return env))
      (setq env (ref environment outer env)) ) ) )


;;;; toplevel-environment-p
(defun toplevel-environment-p (env)
  (check-type env environment)
  (ref environment types env) )
