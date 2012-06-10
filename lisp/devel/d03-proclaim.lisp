;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 3 Evaluation and Compilation - proclaim
;;; runtime/r03-proclaim.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d03-proclaim.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     proclaim
;
(in-package :si)

(deftlv c::*compilation-speed-quality* 3)
(deftlv c::*debug-quality* 3)
(deftlv c::*safety-quality* 3)
(deftlv c::*space-quality* 0)
(deftlv c::*speed-quality* 0)


;;;; find-declaration
;
(defun find-declaration (name &optional (error-p t) env)
    (declare (values t))
  (when (null env) (setq env *environment*))
  (loop
    while env do
      (let ((frob (gethash name (.env-others env))))
        (when (assoc 'declaration frob) (return t)) )
      (setq env (.env-outer env))
    finally
      (when error-p (error "There is no such declaration ~S." name)) ) )


;;;; 3.8.16 proclaim
;;;
;;; Syntax:
;;;     proclaim declaration-specifier => implementation-dependent
;
(defun cl:proclaim (spec)
  (macrolet (
    (do-all-tokens ((var token*-form) &rest decl*-form*)
      `(let* ((.token* ,token*-form)
              (.runner ,token*-form) )
         (block nil
           (tagbody
             .loop
               (when (null .runner) (return))
               (unless (consp .runner)
                 (warn "Declration must be a proper-list: ~S" .token*)
                 (return) )
               (let ((,var (pop .runner))) ,@decl*-form*)
               (go .loop) )) ) )
    )
  (labels (
    ;; add
    (add (htb name kind key val)
      (let ((frob (gethash name htb)))
        (cond
          ((null frob)
            (setf (gethash name htb) `(,kind . ((,key . ,val)))) )
          ((member (first frob) '(:special-operator :macro :symbol-macro))
            (style-warning "Ignore declaration for ~A: ~S"
                (first frob)
                name ) )
          (t
            (let ((cons (assoc key (rest frob) :test #'eq)))
              (if (null cons)
                  (setf (rest frob) (acons key val (rest frob)))
                 (setf (cdr cons) val) )
              (when kind (setf (first frob) kind)) ) )) ) )

    ;; can-not-use
    (can-not-use (spec)
      (error "Can't use ~S here." spec) )

    ;; style-warning
    (style-warning (control &rest args)
      (signal 'c::simple-style-warning
        :format-control control
        :format-arguments args ) )

    ;; process-both
    (process-both (name* env kind key val)
      (let ((funtab  (si::.env-functions env))
            (wrttab (si::.env-writers    env))
            (vartab (si::.env-variables  env)) )
        (do-all-tokens (name name*)
          (typecase name
            (symbol
              (add vartab name nil key val) )
            ((cons (eql function) (cons symbol null))
              (add funtab (second name) kind key val) )
            ((cons (eql function)
                   (cons (cons (eql setf) (cons symbol null)) null) )
              (add wrttab (second (second name)) kind key val) )) ) ) )

    ;; process-fun*
    (process-fun* (name* env kind key val)
      (let ((funtab (si::.env-functions env))
            (wrttab (si::.env-writers   env)) )
        (do-all-tokens (fname name*)
          (typecase fname
            (symbol
              (add funtab fname kind key val) )
            (function-name
              (add wrttab (second fname) kind key val) )
            (otherwise
              (error "~S isn't function name." fname) )) ) ) )

    ;; process-otherwise
    (process-otherwise (spec env)
      (let ((decl-id (first spec)))
        (cond
          ((not (symbolp decl-id))
            (error "Declaration identifier must be a symbol instead of ~S."
                decl-id ) )
          ((find-declaration (first spec) nil env))
          (t
            (let ((tyo (parse-type (first spec) env)))
              (process-var* (rest spec) env nil 'type tyo) ) )) ) )

    ;; process-var*
    (process-var* (name* env kind key val)
      (let ((vartab (si::.env-variables  env)) )
        (do-all-tokens (name name*)
          (typecase name
            (symbol
              (add vartab name kind key val) )
            (otherwise
              (error "Invalid variable name: ~S" name) )) ) ) )

    ;; process/declration
    (process/declaration (spec env)
      (let ((htb (si::.env-others env)))
        (do-all-tokens (name spec)
          (unless (symbolp name)
            (error "Declration name must be a symbol instead of ~S." name) )
          (let ((frob (gethash name htb)))
            (when (null (assoc 'declration frob))
              (setq frob (acons 'declration t frob))
              (setf (gethash name htb) frob) ) ) ) ) )

    ;; process/dynamic-extent - 3.8.20
    (process/dynamic-extent (spec env)
      (process-both (rest spec) env nil 'dynamic-extent t) )

    ;; process/ftype - 3.8.23
    #+nil
    (process/ftype (spec env)
      (let* ((ftype
               (if (consp (rest spec))
                   (second spec)
                 (error "Syntax error: ~S" spec) ) )
             (tyo (parse-type ftype)) )
        (unless (tyo-subtypep tyo (find-class 'function))
          (error "~S isn't subtypep of ~S." ftype 'function) )
        (process-fun* (cddr spec) env :function 'ftype tyo) ) )

    ;; For lazy type evaluation.
    (process/ftype (spec env)
      (let ((ftype
             (if (consp (rest spec))
                 (second spec)
               (error "Syntax error: ~S" spec) )) )
        (unless (or (symbolp ftype) (consp ftype))
          (error "Invalid type specifier: ~S~%" ftype) )
        (process-fun* (cddr spec) env :function 'ftype ftype) ) )

    ;; process/ignore - 3.8.19
    (process/ignore (spec env)
      (process-both (rest spec) env nil 'ignore (first spec)) )

    ;; process/inline - 3.8.20
    (process/inline (spec env)
      (process-fun* (rest spec) env nil 'inline (first spec)) )

    ;; process/lambda-name
    (process/lambda-name (spec env)
        (declare (ignore env))
      (can-not-use spec) )

    ;; process/optimize - 3.8.25
    (process/optimize (spec env)
        (declare (ignore env))
      (do-all-tokens (quality (rest spec))
        (typecase quality
          (symbol
            (process/optimize-aux quality 3) )
          ((cons symbol)
            (process/optimize-aux (first quality) 3) )
          ((cons symbol (cons (integer 0 3) null))
            (process/optimize-aux (first quality) (second quality)) )
          (otherwise
            (error "Invalid optimize quality: ~S" quality) )) ) )

    ;; process/optimize-aux
    (process/optimize-aux (key val)
      (case key
        ((cl:compilation-speed)
          (setq c::*compilation-speed-quality* val) )
        ((cl:debug)
          (setq c::*debug-quality* val) )
        ((cl:safety)
          (setq c::*safety-quality* val) )
        ((cl:space)
          (setq c::*space-quality* val) )
        ((cl:speed)
          (setq c::*speed-quality* val) )
        (otherwise
          (style-warning "Ignore unknown optimize quality ~S." key) )) )

    ;; process/special
    (process/special (spec env)
      (process-var* (rest spec) env :special :special t) )

    ;; process/type
    #+nil
    (process/type (spec env)
      (let* ((type
                (if (not (consp spec))
                    (error "Syntax error: ~S" spec)
                  (second spec) ) )
             (tyo (parse-type type)) )
        (when (eq tyo <nil>)
          (error "Can't use nil type.") )
        (process-var* (cddr spec) env nil 'type tyo) ) )

    (process/type (spec env)
      (let ((type
              (if (not (consp spec))
                  (error "Syntax error: ~S" spec)
                (second spec) ) ))
        (unless (or (symbolp type) (consp type))
          (error "Invalid type specifier: ~S" type) )
        (process-var* (cddr spec) env nil 'type type) ) )

    ;; process/type-predicate
    (process/type-predicate (spec env)
      (let ((typespec (second spec)))
        (process-fun* (cddr spec) env :function 'type-predicate typespec) ) )

    ;; process/values
    (process/values (spec env)
        (declare (ignore env))
      (can-not-use spec) )
    )
    (unless (consp spec)
      (error "Invalid declaration specifier: ~S" spec) )
    (let ((env (or c::*environment* *environment*)))
        (case (first spec)
          ((declaration)    (process/declaration spec env))
          ((dynamic-extent) (process/dynamic-extent spec env))
          ((ftype)          (process/ftype spec env))
          ((ignore)         (process/ignore spec env))
          ((ignorable)      (process/ignore spec env))
          ((inline)         (process/inline spec env))
          ((notinline)      (process/inline spec env))
          ((optimize)       (process/optimize spec env))
          ((special)        (process/special spec env))
          ((type)           (process/type spec env))
          ((values)         (process/values spec env))
          ((ext:lambda-name)
            (process/lambda-name spec env) )
          ((ext:type-predicate)
            (process/type-predicate spec env))
          (otherwise        (process-otherwise spec env)) )
        nil ) ) ) ) 
