;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 5 Data and Control Flow
;;; dev/d05-control.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d05-control.lisp#5 $
;;;
;;; Description:
;;;  This file contains functions for development:
;;;     intern-setf-cell
;;;
;;;     (setf fdefinition)              5.3.3
;;;     function-lambda-expression      5.3.9
;;;     get-setf-expansion              5.3.63
;
(in-package :si)

;;;; 5.3.9 function-lambda-expression
;
(defun cl:function-lambda-expression (fn)
    (declare (type function fn))
    (declare (values list t t))
  (values nil (typep fn 'si:closure) (ext:function-name fn)) )


;;;; 5.3.63 get-setf-expansion
;;; Description:
;;;  Check place as follows:
;;;   symbol    => symbol-macro or variable
;;;    cons     => local-function, local-macro
;;;             => setf-expansion (by defset or define-setf-expansion)
;;;             => (setf function)
;;;
;;; BUGBUG: PERF: We should optimize for variable references, e.g. (foo x y)
;
(defun cl:get-setf-expansion (place &optional env)
    (declare (values list list list t t))
  (labels (
    ;; expand-symbol
    (expand-symbol (symbol)
        (declare (values list list list t t))
      (multiple-value-bind (kind local-p alist)
          (xc::variable-information symbol env)
          (declare (ignore local-p))
        (ecase kind
          ((:lexical :special nil)
            (let ((new-var (gensym "v")))
              (values '()
                      '()
                      (list new-var)
                      `(setq ,symbol ,new-var) symbol) ) )
          ((:symbol-macro)
            (get-setf-expansion (cdr (assoc :symbol-macro alist)) env) )
          ((:constant)
            (c::macro-error "Can't alter constant ~S." symbol) )) ) )

    ;; expand-cons
    (expand-cons (place)
        (declare (values list list list t t))
      (loop
        (multiple-value-bind (kind local-p alist)
            (xc::function-information (first place) env)

          (unless local-p
            (multiple-value-bind (kind local-p alist)
                (xc::function-information `(setf ,(first place)) env)
                (declare (ignore local-p))

              (case kind
                ((:macro)
                  (return (expand-cons/setf-expansion place alist env)) )
                ((:function)
                  (return (expand-cons/function place)) )) ))

          (ecase kind
            ((:function nil)
              (return (expand-cons/function place)) )

            ((:macro)
              (let ((expander (cdr (assoc :macro alist))))
                (setq place
                  (funcall *macroexpand-hook* expander place env) ) ) )

            ((:special-operator)
              (invalid-setf-place place) )) )) )

    ;; expand-cons/function
    ;; BUGBUG: Since loop expander uses get-setf-expansion, we can not
    ;; use loop macro at genesis.
    #+nil
    (expand-cons/function (place)
      (loop
        for arg in (rest place)
          unless (constantp arg env)
            collect arg into subform*
            and do (setq arg (gensym "a"))
            and collect arg into var-arg*
          end

          collect arg into arg*
        finally
          (let ((var-newval (gensym "v"))
                (reader     (first place)) )
            (return (values var-arg*
                            subform*
                            (list var-newval)
                            `(funcall #'(setf ,reader) ,var-newval ,@arg*)
                            `(,reader ,@arg*) )) )) )

    ;; expand-cons/function
    (expand-cons/function (place)
        (declare (values list list list t t))
      (let ((subform* '())
            (var-arg* '())
            (arg*     '()) )
        (dolist (arg (rest place))
          (when (need-save-p arg)
            (push arg subform*)
            (setq arg (gensym "a"))
            (push arg var-arg*) )
          (push arg arg*) )

        (setq subform* (nreverse subform*))
        (setq var-arg* (nreverse var-arg*))
        (setq arg*     (nreverse arg*))

        (let ((var-newval (gensym "v"))
              (reader     (first place)) )
          (values var-arg*
                  subform*
                  (list var-newval)
                  `(funcall #'(setf ,reader) ,var-newval ,@arg*)
                  `(,reader ,@arg*) ) ) ) )

    ;; expand-cons/setf-expansion
    (expand-cons/setf-expansion (place alist env)
        (declare (values list list list t t))
      (funcall (the (function (cons environment) (values list list list t t))
                    (cdr (assq :macro alist)) )
                place env ) )

    ;; invalid-setf-place
    (invalid-setf-place (place)
      (xc::macro-error "Invalid setf place: ~S" place) )

    ;; need-save-p
    (need-save-p (form)
      (typecase form
        (cons
          (not (constantp form env)) )
        (symbol
          (multiple-value-bind (kind local-p alist)
              (xc::variable-information form env)
              (declare (ignore local-p alist))
            (ecase kind
              ((:lexical) nil)
              ((:symbol-macro) t)
              ((:constant) nil)
              ((:special) t)
              ((nil) t) ) ) )) )
    )
    ;;
    (typecase place
      (symbol (expand-symbol place))
      (cons   (expand-cons   place))
      (otherwise (invalid-setf-place place)) ) ) )
