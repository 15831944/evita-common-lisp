;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; eval - devel - 3 Evaluation and Compilation
;;; devel/d03-eval.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d03-eval.lisp#3 $
;;;
;;; Description:
;;;  This file contains functions for development or use compiler
;;;  functionality:
;;;     compile                     3.8.3
;;;     compiler-macro-function     3.8.8
;;;     constantp                   3.8.30
;;;     eval                        3.8.4   (native)
;;;     macroexpand                 3.8.12
;;;     macroexpand-1               3.8.12
;;;     macro-function              3.8.11
;;;     proclaim                    3.8.16  m00-loader.lisp
;;;     special-operator-p          3.8.29
;
(in-package :xc)

;;;; 3.8.3 compile
;;;
;;; Syntax:
;;;   compile fname &optional definition => function, warnings-p, failure-p
;;;
;;; BUGBUG: We must specify what we should do when compile-form fails.
;;; See 3.2.5 Exceptional Situations in the Compiler.
;
(defun cl:compile (name &optional definition)
  (unless (or (null name) (si::function-name-p name))
    (error "Bad function name: ~S" name) )
  (multiple-value-bind (fn warnings-p failure-p)
      (cond
        ((functionp definition)
          (values definition nil nil) )

        ((and (consp definition)
              (eq 'lambda (first definition)) )
          (multiple-value-bind (fn warnings-p failure-p)
              (let ((*situation* 'compile)
                    ;; We need to compile function in EVM for CLOS
                    ;; discriminator. See implementation of FIN.
                    (*target*    +eval-target+) )
                (compile-form definition) )
            (when failure-p
              (error "Compilation failed.") )
            (values (funcall fn) warnings-p failure-p) ) )

        ((and name (null definition))
          (unless (fboundp name)
            (error 'undefined-function :name name) )
          (values nil nil nil) )

        (t
          (error
            "Expect lambda expression or function: ~S" definition ) ))
    (when (and name definition fn)
      (if (and (fboundp name) (macro-function name))
          (setf (macro-function name) fn)
        (setf (fdefinition name) fn) ))
    (values (or name fn) warnings-p failure-p) ) )


;;;; 3.8.30 constantp
;;;
;;; Syntax:
;;;   constantp form &optional env => boolean
;;;
;;; BUGBUG: NYI: constant function & macro expand
;;;
(defun cl:constantp (form &optional env)
  (values (constant-value-p form env)) )


;;;; 3.8.4 eval
;;;
;;; Note: We process eval-when here instead of using compiler for supporting
;;; defstruct macro with typep with structure-class.
;;;
;
(defun cl:eval (form)
  (labels (
    ;; check-form
    (check-form (form)
        (declare (values ext:sequence-index))
      (or (si::proper-list-p form) (malformed-form form)) )

    ;; eval-cons
    ;;  Note: we MUST process eval-when here.
    (eval-cons (form)
      (let ((operator (first form)))
        (cond
          ((symbolp operator)
            (eval-compound-form form) )
          ((and (consp operator) (eq (car operator) 'lambda))
            (eval-lambda-form form) )
          (t
            (error "Invalid compound form: ~S" form) )) ) )

    ;; eval-compound-form
    (eval-compound-form (form)
      (let ((operator (first form)))
        (multiple-value-bind (kind local-p alist)
            (xc::function-information operator *environment*)
          (when local-p
            (error "Can't use local function ~S" operator) )
          (ecase kind
            ((nil :function)
              (apply operator (mapcar #'eval (rest form))) )
            ((:macro)
              (let ((expander (cdr (assoc kind alist))))
                (eval (funcall expander form nil)) ) )
            ((:special-operator)
              (case operator
                ((quote)     (eval-special/quote form))
                ((function)  (eval-special/function form))
                ((progn)     (eval-forms form (rest form)))
                ((eval-when) (eval-special/eval-when form))
                ((if)        (eval-special/if form))
                ((setq)      (eval-special/setq form))
                ((load-time-value)
                  (eval-special/load-time-value form) )
                (otherwise
                  (let ((fn (compile-form form)))
                    (when (null fn)
                      (error "Compilation failed.") )
                    (funcall fn) ) )) )) ) ) )

    ;; eval-forms
    (eval-forms (form forms)
      (loop
        (when (null forms) (return nil))
        (unless (consp forms) (malformed-form form))
        (let ((form-1 (pop forms)))
          (when (null forms) (return (eval form-1)))
          (eval form-1) )) )

    ;; eval-lambda-form
    (eval-lambda-form (form)
      (let ((fn (compile nil (car form))))
        (apply fn (mapcar #'eval (rest form))) ) )

    ;; eval-special/eval-when
    ;;  Evaluates special form (eval-when (<situration>*) <form>*).
    (eval-special/eval-when (form)
      (unless (>= (check-form form) 2) (syntax-error form))
      (let ((situation* (second form)))
        (when (or (member :execute situation*)
                  (member 'eval    situation*)
                (when (eq xc::*situation* 'load)
                  (or (member :load-toplevel situation*)
                      (member 'load          situation*) ))
                (when (eq :compile-time-too *processing-mode*)
                  (or (member :compile-toplevel situation*)
                      (member 'compile          situation*) )))
          (eval-forms form (cddr form)) ) ) )

    ;; eval-special/function
    ;;  Evaluates special form (function <fname>).
    (eval-special/function (form)
      (unless (= (check-form form) 2) (syntax-error form))
      (let ((fname (second form)))
        (typecase fname
          ((or symbol (cons (eql setf) (cons symbol null)))
            (multiple-value-bind (kind local-p)
                (xc::function-information fname *environment*)
              (when local-p
                (error "Can't use local function ~S" fname) )
              (ecase kind
                ((nil :function)
                  (fdefinition fname) )
                ((:macro)
                  (error "~S is macro." fname) )
                ((:special-operator)
                  (error "~S is special-operator." fname) )) ) )
          ((cons (eql lambda) cons)
            (funcall (compile-form form)) )
          (t
            (error "Invalid function name: ~S~%" fname) )) ) )

    ;; eval-special/if
    ;;  Evaluates special form (if <test> <then> [<else>]).
    (eval-special/if (form)
      (unless (<= 3 (check-form form) 4) (syntax-error form))
      (if (eval (second form))
          (eval (third form))
        (eval (fourth form)) ) )

    ;; eval-special/load-time-value
    ;;  Evaluates special form (load-time-value form [read-only-p])
    (eval-special/load-time-value (form)
      (case (check-form form)
        ((2))
        ((3)
          (unless (or (eq (third form) nil) (eq (third form) t))
            (syntax-error form) ) )
        (otherwise (syntax-error form)) )
      (values (eval (second form))) )

    ;; eval-special/quote
    (eval-special/quote (form)
      (unless (= (check-form form) 2) (syntax-error form))
      (second form) )

    ;; eval-special/setq
    (eval-special/setq (form)
      (let ((runner (rest form))
            (value  nil) )
        (loop
          (when (null runner) (return value))
          (unless (consp runner) (malformed-form form))

          (let ((name (pop runner)))
            (unless (and name (symbolp name))
              (error "Invalid variable name: ~S" name))
            (when (null runner) (error "Missing value form for ~S." name))
            (unless (consp runner) (malformed-form form))

            (setq value (pop runner))

            (multiple-value-bind (kind local-p alist)
                (xc::variable-information name *environment*)
                (declare (ignore local-p))
              (ecase kind
                ((:special nil)
                  (setq value (eval value))
                  (set name value) )
                ((:constant)
                  (error "Can't alter constant ~S." name) )
                ((:symbol-macro)
                  (let ((expansion (cdr (assoc kind alist))))
                    (setq value (eval `(setf ,expansion ,value))) ) )) ) )) ) )

    ;; eval-symbol
    (eval-symbol (symbol)
        (declare (type symbol symbol))
        (declare (values t))
      (multiple-value-bind (kind local-p alist)
          (xc::variable-information symbol *environment*)
        (when local-p
          (error "Can't use local variable ~S." symbol) )
        (case kind
          ((:symbol-macro) (values (eval (cdr (assoc :symbol-macro alist)))))
          ((:constant)     (cdr (assoc :constant alist)))
          (t (symbol-value symbol)) ) ) )

    ;; malformed-form
    (malformed-form (form)
      (error "Malformed compound form: ~S" form) )

    ;; syntax-error
    (syntax-error (form)
      (error "Syntax error: ~S" form) )
    )
    ;;
    ;; eval
    ;;
    (cond
      ((symbolp form) (eval-symbol form))
      ((atom form) form)
      (t (eval-cons form)) ) ) )


;;;; 3.8.12 macroexpand
;;;
;;; Syntax:
;;;     macroexpand form &optional env => expansion, expanded-p
;;;
;
(defun cl:macroexpand (form &optional env)
  (let ((expanded-so-far-p nil))
    (loop
      (multiple-value-bind (expansion expanded-p)
          (macroexpand-1 form env)
        (unless expanded-p
          (return (values form expanded-so-far-p)) )
        (setq expanded-so-far-p t)
        (setq form expansion) )) ) )


;;;; 3.8.12 macroexpand-1
;;;
;;; Syntax:
;;;     macroexpand-1 form &optional env => expansion, expanded-p
;;;
;
(defun cl:macroexpand-1 (form &optional env)
  (cond
    ((consp form)
      (let ((expander 
              (and (symbolp (car form)) (macro-function (car form) env)) ))
        (if (null expander)
            (values form nil)
          (values (funcall *macroexpand-hook* expander form env) t) ) ) )

    ((symbolp form)
      (multiple-value-bind (kind local-p alist)
          (variable-information form env)
        (declare (ignore local-p))
        (if (eq :symbol-macro kind)
            (values (cdr (assoc :symbol-macro alist)) t)
          (values form nil) ) ) )
    (t
      (values form nil) )) )


;;;; 3.8.11 macro-function
;;;
;;; Syntax:
;;;     macro-function symbol &optional env => macro-function or nil
;;;     (setf (macro-function symbol &optional env) new-function)
;;;
;;; BUGBUG: Not support env
;;;
;
(defun cl:macro-function (symbol &optional env)
    (declare (type symbol symbol))
    (declare (values (or null function)))
  (cdr (assoc :macro (nth-value 2 (function-information symbol env)))) )


;;;; 3.8.29 special-operator-p
;;;
;;; Syntax:
;;;     special-operator-p symbol => generalized-boolean
;
(defun cl:special-operator-p (symbol)
    (declare (type symbol symbol))
  (eq :special-operator (function-information symbol nil)) )
