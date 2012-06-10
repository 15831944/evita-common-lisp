;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - Type System - 4.4.20 and
;;; lisp/typesys/tyo-04-and.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r04-type.lisp#2 $
;;;
;;; Description:
;;;  This fils contains declarations for type system.
;
(in-package :si)

;;;; find-type
(defun find-type (name &optional (errorp t) env)
    (declare (type symbol name))
    (declare (type (or environment null) env))
  (labels (
    (find-type-aux ()
      (let ((env (toplevel-environment env)))
        (loop
          (with-latch ((ref environment latch env) :shared)
            (let ((datum (gethash/eq name (ref environment types env))))
              (when datum (return datum))
              (setq env (ref environment outer env)) ) )
          (unless env (return nil)) ) ) )
    )
    (or (find-type-aux) (when errorp (error "No such type: ~S" name))) ) )


;;;; (setf find-type)
(defun (setf find-type) (expander name &optional errorp env)
    (declare (type symbol name))
    (declare (type (or environment null) env))
    (declare (ignore errorp))
  (let ((env (toplevel-environment env)))
    (with-latch ((ref environment latch env))
      (setf (gethash/eq name (ref environment types env)) expander) ) ) )


;; integer<=
(defun integer<= (x y)
    (declare (type (or integer (eql -) (eql +)) x y))
  (cond
    ((eq x '-) t)
    ((eq y '+) t)
    ((eq x '+) nil)
    ((eq y '-) nil)
    ((<= x y)) ) )


;; integer-lower
(defun integer-lower (ty)
    (declare (values (or integer (eql -))))
  (if (not (consp ty))
      '-
    (ecase (length ty)
      ((0 1) '-)
      ((2 3) (if (eq (second ty) '*) '- (second ty))) )) )


;; integer-upper
(defun integer-upper (ty)
    (declare (values (or integer (eql +))))
  (if (not (consp ty))
      '+
    (ecase (length ty)
      ((0 1 2) '+)
      ((3) (if (eq (third ty) '*) '+ (third ty))) )) )


;;;; parse-type
(defun parse-type (ty env)
    (declare (optimize (speed 3) (safety 0)))
    (declare (values t t))
  (labels (
    ;; parse-coons
    (parse-cons (ty)
        (declare (values t t))
      (let ((name (first ty)))
        (let ((expander (find-type name nil env)))
          (cond
            ((functionp expander)
              (parse-type (funcall expander ty env) env) )
            ((null expander)
              (case name
                ((cl:and)       (values :and ty))
                ((cl:complex)   (values :complex ty))
                ((cl:cons)      (parse/cons ty))
                ((cl:eql)       (values :eql ty))
                ((cl:integer)   (values :integer ty))
                ((cl:member)    (values :member ty))
                ((cl:not)       (values :not ty))
                ((cl:or)        (values :or ty))
                ((cl:satisfies) (values :satisfies ty))
                ((cl:values)
                  (error "Can't use ~S type specifier here." name) )
                (otherwise
                  (let ((class (find-class name nil env)))
                    (if class
                        (values :class class)
                      (values :undefined name) ) ) )) )
            (t (parse-type expander env)) ) ) ) )

    ;; parse-symbol
    (parse-symbol (name)
        (declare (type symbol name))
        (declare (values symbol t))
      (let ((expander (find-type name nil env)))
        (cond
          ((functionp expander)
            (parse-type (funcall expander (list name) env) env) )
          ((null expander)
            (let ((class (find-class ty nil env)))
              (if class 
                  (values :class class)
                (values :undefined name) ) ) )
          (t (parse-type expander env)) ) ) )

    ;; parse/cons
    (parse/cons (ty)
        (declare (values symbol t))
      (case (length ty)
        ((1) (values :class (find-class 'cons)))
        ((2)
            (if (eq (second ty) '*)
                (values :class (find-class 'cons))
              (values :cons `(cons ,(second ty) t)) ) )
        ((3)
            (cond
              ((and (eq (second ty) '*) (eq (third ty) '*))
                (values :class (find-class 'cons)) )
              ((eq (second ty) '*)
                (values :cons `(cons t ,(third ty))) )
              ((eq (third ty) '*)
                (values :cons `(cons ,(second ty) t)) )
              (t
                (values :cons ty) )) )
        (otherwise (error "Invalid type specifier: ~S" ty)) ) )
    )
    ;;
    (cond
      ((null ty)    (values nil nil))
      ((symbolp ty) (parse-symbol ty))
      ((classp ty)  (values :class ty))
      ((consp ty)   (parse-cons ty))
      (t (error "Invalid type specifier: ~S" ty)) ) ) )


;;;; subtypep
(defun cl:subtypep (ty1 ty2 &optional env)
    (declare (values t t))
  (labels (
    ;; subtypep/*/and
    (subtypep/*/and (ty1 ty2)
       (declare (values t t))
      (dolist (ty11 ty1 (values t nil))
        (multiple-value-bind (subtypep validp)
            (subtypep ty11 ty2 env)
            (declare (ignore subtypep))
          (unless validp (return (values nil nil))) ) ) )

    ;; subtypep/*/not
    (subtypep/*/not (ty1 ty2)
       (declare (values t t))
      (multiple-value-bind (subtypep validp)
          (subtypep ty1 (second ty2) env)
        (if (not validp)
            (values nil nil)
          (values (not subtypep) t) ) ) )

    ;; subtypep/*/or
    (subtypep/*/or (ty1 ty2)
       (declare (values t t))
      (let ((total-validp t))
        (dolist (ty11 ty1 (values nil total-validp))
          (multiple-value-bind (subtypep validp)
              (subtypep ty11 ty2 env)
            (when subtypep (return (values t t)))
            (unless validp (setq total-validp nil)) ) ) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; and
    ;;

    ;; subtypep/and/*
    (subtypep/and/* (ty1 ty2)
        (declare (values t t))
      (multiple-value-bind (kind2 ty2) (parse-type ty2 env)
        (case kind2
          ((:and)       (subtypep/*/and ty1 ty2))
          ((:or)        (subtypep/*/or ty1 ty2))
          ((:not)       (subtypep/*/not ty1 ty2))
          ((nil)        (values nil t))
          ((:satisfies) (values nil nil))
          (otherwise    (subtypep/and/- ty1 ty2)) ) ) )

    ;; subtypep/and/-
    (subtypep/and/- (ty1 ty2)
        (declare (values t t))
      (dolist (ty11 (rest ty1) (values nil t))
        (multiple-value-bind (subtypep validp)
            (subtypep ty11 ty2 env)
          (unless subtypep (return (values subtypep validp))) ) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; class
    ;;

    ;; subtypep/class/*
    (subtypep/class/* (class1 ty2)
        (declare (values t t))
        (declare (type class class1))
      (multiple-value-bind (kind2 ty2) (parse-type ty2 env)
        (case kind2
          ((:class)     (values (subclassp class1 ty2) t))
          ((:and)       (subtypep/*/and ty1 ty2))
          ((:complex)   (values nil t))
          ((:cons)      (values nil t))
          ((:integer)   (values nil t))
          ((:or)        (subtypep/*/or  ty1 ty2))
          ((:not)       (subtypep/*/not ty1 ty2))
          ((nil)        (values nil t))
          ((:satisfies) (values nil nil))
          (otherwise (values nil nil)) ) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; complex
    ;;
    (subtypep/complex/* (ty1 ty2)
      (multiple-value-bind (kind2 ty2) (parse-type ty2 env)
        (case kind2
          ((:and)       (subtypep/*/and ty1 ty2))
          ((:class)     (values (eq (find-class 'complex) ty2) t))
          ((:complex)   (subtypep/complex/complex ty1 ty2))
          ((:cons)      (values nil t))
          ((:integer)   (values nil t))
          ((nil)        (values nil t))
          ((:not)       (subtypep/*/not ty1 ty2))
          ((:or)        (subtypep/*/or  ty1 ty2))
          ((:satisfies) (values nil nil))
          (otherwise    (values nil nil)) ) ) )

    (subtypep/complex/complex (ty1 ty2)
      (subtypep (second ty1) (second ty2) env) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; cons
    ;;

    ;; subtypep/cons/*
    (subtypep/cons/* (cons1 ty2)
      (multiple-value-bind (kind2 ty2) (parse-type ty2 env)
        (case kind2
          ((:and)       (subtypep/*/and ty1 ty2))
          ((:class)     (values (eq (find-class 'cons) ty2) t))
          ((:complex)   (values nil t))
          ((:cons)
            (multiple-value-bind (subtypep valid)
                (subtypep (second cons1) (second ty2))
              (if (not subtypep)
                  (values subtypep valid)
                (subtypep (third cons1) (third ty2)) ) ) )
          ((:integer)   (values nil t))
          ((nil)        (values nil t))
          ((:not)       (subtypep/*/not ty1 ty2))
          ((:or)        (subtypep/*/or  ty1 ty2))
          ((:satisfies) (values nil nil))
          (otherwise    (values nil nil)) ) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; integer
    ;;
    (subtypep/integer/* (ty1 ty2)
        (declare (values t t))
      (multiple-value-bind (kind2 ty2) (parse-type ty2 env)
        (case kind2
          ((:and)       (subtypep/*/and ty1 ty2))
          ((:class)
            (values (subclassp (find-class 'integer) ty2) t) )
          ((:complex)   (values nil t))
          ((:cons)      (values nil t))
          ((:integer)
            (values (and (integer<= (integer-lower ty2)
                                    (integer-lower ty1) )
                         (integer<= (integer-upper ty1)
                                    (integer-upper ty2) ))
                     t ) )
          ((nil)        (values nil t))
          ((:not)       (subtypep/*/not ty1 ty2))
          ((:or)        (subtypep/*/or  ty1 ty2))
          ((:satisfies) (values nil nil))
          (otherwise    (values nil nil)) ) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; not
    ;;

    ;; subtypep/not/*
    ;;  (subtypep (not A) B) = (not (subtypep B A))
    (subtypep/not/* (ty1 ty2)
       (declare (values t t))
      (multiple-value-bind (kind2 ty2) (parse-type ty2 env)
        (case kind2
          ((nil)        (values nil t))
          ((:not)       (subtypep/not/not ty1 ty2))
          ((:satisfies) (values nil nil))
          (otherwise    (subtypep/not/- ty1 ty2)) ) ) )

    ;; subtypep/not/-
    (subtypep/not/- (ty1 ty2)
        (declare (values t t))
        (declare (ignore ty1 ty2))
      ;; give up...
      (values nil nil) )

    ;; subtypep/not/not
    (subtypep/not/not (ty1 ty2)
        (declare (values t t))
      (multiple-value-bind (subtypep validp)
          (subtypep (second ty1) (second ty2))
        (cond
          ((not validp) (values nil nil))
          (subtypep     (values t t))
          (t            (values nil nil)) ) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; or
    ;;

    ;; subtypep/or/*
    (subtypep/or/* (ty1 ty2)
        (declare (values t t))
      (multiple-value-bind (kind2 ty2) (parse-type ty2 env)
        (case kind2
          ((:and)       (subtypep/*/and ty1 ty2))
          ((nil)        (values nil t))
          ((:not)       (subtypep/*/not ty1 ty2))
          ((:or)        (subtypep/or/or ty1 ty2))
          ((:satisfies) (values nil nil))
          (otherwise    (subtypep/or/- ty1 ty2)) ) ) )

    ;; subtypep/or/-
    (subtypep/or/- (ty1 ty2)
        (declare (values t t))
      (dolist (ty11 (second ty1) (values t t))
        (multiple-value-bind (subtypep validp)
            (subtypep ty11 ty2 env)
          (unless subtypep (return (values nil validp))) ) ) )

    ;; subtypep/or/or
    (subtypep/or/or (ty1 ty2)
        (declare (values t t))
      (subtypep/*/or ty1 ty2) )
    )
    ;;
    (if (equal ty1 ty2)
        (values t t)
      (multiple-value-bind (kind1 ty1) (parse-type ty1 env)
        (ecase kind1
          ((:and)       (subtypep/and/*     ty1 ty2))
          ((:class)     (subtypep/class/*   ty1 ty2))
          ((:complex)   (subtypep/complex/* ty1 ty2))
          ((:cons)      (subtypep/cons/*    ty1 ty2))
          ((:integer)   (subtypep/integer/* ty1 ty2))
          ((:or)        (subtypep/or/*      ty1 ty2))
          ((:not)       (subtypep/not/*     ty1 ty2))
          ((:satisfies) (values nil nil))
          ((:undefined) (values nil nil)) ) )) ) )


;;;; typep
(defun cl:typep (x ty2 &optional env)
    (declare (optimize (speed 3) (safety 0)))
 ;(format t "; typep ~S ~S~%" x ty2)
  (labels (
    ;; typep/and
    (typep/and (x ty2)
      (dolist (ty22 (rest ty2) t)
        (unless (typep x ty22 env) (return nil)) ) )

    ;; typep/complex
    (typep/complex (x ty2)
      (and (complexp x) (typep (realpart x) (second ty2))) )

    ;; typep/cons
    (typep/cons (x ty2)
      (and (typep (car x) (second ty2)) (typep (cdr x) (third ty2))) )

    ;; typep/eql
    (typep/eql (x ty2)
      (eql x (second ty2)) )

    ;; typep/integer
    (typep/integer (x ty2)
      (and (integerp x)
           (integer<= (integer-lower ty2) x)
           (integer<= x (integer-upper ty2)) ) )

    ;; typep/member
    (typep/member(x ty2)
      (member x (rest ty2)) )

    ;; typep/not
    (typep/not (x ty2)
      (not (typep x (second ty2))) )

    ;; typep/or
    (typep/or (x ty2)
      (dolist (ty22 (rest ty2) nil)
        (when (typep x ty22 env) (return t)) ) )

    ;; typep/satisfies
    (typep/satisfies (x ty2)
      (funcall (second ty2) x) )
    )
    ;;
    (multiple-value-bind (kind2 ty2) (parse-type ty2 env)
      (ecase kind2
        ((:and)       (typep/and x ty2))
        ((:class)     (subclassp (class-of x) ty2))
        ((:complex)   (typep/complex x ty2))
        ((:cons)      (typep/cons x ty2))
        ((:eql)       (typep/eql x ty2))
        ((:integer)   (typep/integer x ty2))
        ((:member)    (typep/member x ty2))
        ((:not)       (typep/not x ty2))
        ((:or)        (typep/or  x ty2))
        ((:satisfies) (typep/satisfies x ty2))
        ((nil)        nil)
        ((:undefined) (error "Undefined type specifier ~S." ty2)) ) ) ) )
