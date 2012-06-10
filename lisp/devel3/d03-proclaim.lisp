;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 3 Evaluation and Compilation - proclaim
;;; devel3/d03-proclaim.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel3/d03-proclaim.lisp#5 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     proclaim
;
(in-package :si)

;;;; 3.8.16 proclaim
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
    (add (env htb name kind key val)
      (let* ((kname (if (consp name) (intern-setf-cell (second name)) name))
             (kind
               (with-latch ((ref environment latch env))
                 (add-aux htb kname kind key val) ) ))
        (when kind
          (c::style-warn "Ignore declaration for ~A: ~S" kind name) ) ) )

    ;; add-aux
    (add-aux (htb kname kind key val)
      (let ((frob (gethash/eq kname htb)))
        (cond
          ((null frob)
            (setf (gethash/eq kname htb) `(,kind . ((,key . ,val))))
            nil )
          ((member (first frob) '(:special-operator :macro :symbol-macro))
            (first frob) )
          (t
            (let ((cons (assoc key (rest frob) :test #'eq)))
              (if (null cons)
                  (setf (rest frob) (acons key val (rest frob)))
                 (setf (cdr cons) val) )
              (when kind (setf (first frob) kind))
              nil ) )) ) )

    ;; can-not-use
    (can-not-use (spec)
      (error "Can't use ~S here." spec) )

    ;; invalid-typespec
    (invalid-typespec (typespec)
      (error "Invalid type-specifier ~S" typespec) )

    ;; process-both
    (process-both (name* env kind key val)
      (let ((funtab (ref environment functions env))
            (vartab (ref environment variables env)) )
        (do-all-tokens (name name*)
          (typecase name
            (symbol
              (add env vartab name nil key val) )
            ((cons (eql function) (cons symbol null))
              (add env funtab (second name) kind key val) )
            ((cons (eql function)
                   (cons (cons (eql setf) (cons symbol null)) null) )
              (add env funtab (second name) kind key val ) )) ) ) )

    ;; process-fun*
    (process-fun* (name* env kind key val)
      (let ((funtab (ref environment functions env)))
        (do-all-tokens (fname name*)
          (typecase fname
            (symbol
              (add env funtab fname kind key val) )
            (function-name
              (add env funtab fname kind key val) )
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
      (let ((vartab (ref environment variables  env)) )
        (do-all-tokens (name name*)
          (typecase name
            (symbol
              (add env vartab name kind key val) )
            (otherwise
              (error "Invalid variable name: ~S" name) )) ) ) )

    ;; process/declration
    (process/declaration (spec env)
      (let ((htb (ref environment others env)))
        (do-all-tokens (name spec)
          (unless (symbolp name)
            (error "Declration name must be a symbol instead of ~S." name) )
          (let ((frob (gethash/eq name htb)))
            (when (null (assoc 'declration frob))
              (setq frob (acons 'declration t frob))
              (setf (gethash/eq name htb) frob) ) ) ) ) )

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
        (unless (validate-ftype ftype)
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
      (do-all-tokens (quality (rest spec))
        (typecase quality
          (symbol
            (process/optimize-aux env quality 3) )
          ((cons symbol (cons (integer 0 3) null))
            (process/optimize-aux env (first quality) (second quality)) )
          ((cons symbol null)
            (process/optimize-aux env (first quality) 3) )
          (otherwise
            (error "Invalid optimize quality: ~S" quality) )) ) )

    ;; process/optimize-aux
    (process/optimize-aux (env key val)
      (case key
        ((cl:compilation-speed cl:debug cl:safety cl:space cl:speed)
          (set-optimize env key val) )
        (otherwise
          (c::style-warn "Ignore unknown optimize quality ~S." key) )) )

    ;; set-optimize
    (set-optimize (env key val)
      (with-latch ((ref environment latch env))
        (let ((cons (assoc key c::*optimization*)))
          (if cons
              (setf (cdr cons) val)
            (push (cons key val) c::*optimization*) ) )) )

    ;; process/special
    (process/special (spec env)
      (process-var* (rest spec) env :special :special t) )

    ;; process/type
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

    ;; undefined-type
    (undefined-type (typespec)
      (warn 'simple-style-warning
            :format-arguments (list typespec)
            :format-control   "Undefined type-specifier ~S" ) )

    ;; validate-ftype
    (validate-ftype (typespec)
      (typecase typespec
        (null nil)
        (symbol
          (or (find-type typespec nil) (undefined-type typespec) :lazy) )
        (cons
          (cond
            ((eq (first typespec) 'function)
              (eql (length typespec) 3) )
            ((find-type (first typespec) nil))
            (t (undefined-type typespec) :lazy) ) )
        (otherwise
          (invalid-typespec typespec) )) )
    )
    (unless (consp spec)
      (error "Invalid declaration specifier: ~S" spec) )
    (let ((env (toplevel-environment *environment*)))
        (case (first spec)
          ((cl:declaration)     (process/declaration spec env))
          ((cl:dynamic-extent)  (process/dynamic-extent spec env))
          ((cl:ftype)           (process/ftype spec env))
          ((cl:ignore)          (process/ignore spec env))
          ((cl:ignorable)       (process/ignore spec env))
          ((cl:inline)          (process/inline spec env))
          ((cl:notinline)       (process/inline spec env))
          ((cl:optimize)        (process/optimize spec env))
          ((cl:special)         (process/special spec env))
          ((cl:type)            (process/type spec env))
          ((cl:values)          (process/values spec env))
          ((ext:lambda-name)    (process/lambda-name spec env) )
          ((ext:type-predicate) (process/type-predicate spec env))
          (otherwise            (process-otherwise spec env)) )
        nil ) ) ) )
