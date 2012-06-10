;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 6 Iteration - Loop Facility
;;; macro/m06-loop.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m06-loop.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     loop            6.2.4
;;;     loop-finish     6.2.5
;;;
;;; Note: Following evcl specific functions are used in this file:
;;;     one-argument-form-p
;;;     proper-list-p
;;;
;;; BUGBUG: NYI: compile-time-constant-optimization: Until we have
;;; eval-with-environment, we optimize only for global constant.
;;;
;;; BUGBUG: NYI: ext:use-loop-finish-in-finally-clause declation.
;;; Until Evita Common Lisp support ext:use-loop-finish-in-finally-clause
;;; declaration, Loop facility doesn't warn when you use loop-finish form in
;;; finally clause. It will infinite loop unless you write termination form in
;;; finally clause.
;;;
;;; Note: MIT Loop doesn't use flag variable for maximize and minimize when
;;; loop parser knows type of form.
;;;
;;; Note: We use gensym'ed tag name instead of next-loop and end-loop tag in
;;; MIT Loop.
;;;
;;; Iteration frame work
;;;  FOR clauses are represented by iteration frame:
;;;    variable-bindings with declaration.
;;;    head
;;;    around
;;;    prologue
;;;    body
;;;    tail
;;;
;;;  Head part initializes variables for first iteration. This inclueds
;;;  termination testing.
;;;
;;; Head and Tail parts also have:
;;;  pre-step-end-test
;;;    This is an end-test which determines if it is safe to step to the next
;;;    value of the iteration variable.
;;;
;;;  steps => (psetq ,@steps)
;;;    Variables that get stepped. This is internally manipulated as a list of
;;;    the form (var1 val1 var2 val2 ...); all of those variables are stepped
;;;    in parallel, meaning that all of the vals are evaluated before any of
;;;    the names are set.
;;;
;;;  post-step-end-test
;;;    Sometimes you can't see if you are done until you step to the next
;;;    value; that is, the end-test is a function of the stepped-to value.
;;;
;;;  pseudo-steps => (setq ,@pseudo-steps)
;;;    Other things that need to be stepped. This is typically used for
;;;    internal variables that are more convenienty stepped here., or to set
;;;    up iteration variables that are functions of some internal variable(s)
;;;    actually driving the iteration. This is a list like steps, but the
;;;    variables in it do not get stepped in parallel.
;;;
;;;
;;; Note:
;;;  There are number of differences between ANSI-CL and CLtL2 about
;;;  loop facility.
;;;
;;;  1. finally clause
;;;    We can't use "finally return expr" in ANSI-CL.
;;;     ANSI:   finally {compound-form}+
;;;     CLtL2:  finally [do | doing] {expr*} -- p.745
;;;     CLtL2:  finally return expr
;;;
;;;  2. repeat clause
;;;   We can't put REPEAT clause before FOR clause.
;;;     ANSI:   variable-clause::= with-clause | initial-final | for-as-clause
;;;     CLtL2:  variables ::= with | initial-final | for-as | repeat -- p.714
;;
;
(in-package :xc)

;;;; parse-loop
;;;
;;; Description:
;;;  Parses "extended" loop form.
;;;
;;; Notation:
;;;  dvar   A cons or symbol represents destructuing pattern.
;;;  svar   A symbol represents simple variable.
;;;  var-   A prefix for internally generated variable.
;;;  typ-   A type specifier.
;;;
;;; Local Variables:
;;;  names - a list
;;;   List of variable names introduced by "for" and "with" clauses. This
;;;   list is used for checking duplicated variables.
;
(defun parse-loop (source env)
  (let ((src-scan        source)
        (src-last        nil)               ; for error message
        (src-clause      nil)               ; for error message
        (token           nil)
        (raw-token       nil)
        (eof-marker      '(eof-marker))
        (unspecified     '(unspecified))

        (loop-name       nil)
        (finish-form     `(go ,(gensym "end-loop"))) ; save cons

        (names           '())   ; list of variable names.

        (frames          '())
        (vars            '())
        (destructs       '())

        (prologue*       '())

        (around*         '())   ; for hash-table and package iterator
        (head-pre-test*  '())
        (head-psetq*     '())
        (head-post-test* '())
        (head-setq*      '())

        (initially*      '())

        (body-form*      '())

        (tail-pre-test*  '())
        (tail-psetq*     '())
        (tail-post-test* '())
        (tail-setq*      '())

        (finally*        '())
        (loop-value-owner nil); for always and never
        (loop-value       nil)

        (body-binding*   '())   ; for accumulation and it.
        (body-decl*      '())
        (acc-vars        '())   ; alist of (name . last)
        (var-acc         nil)
        (var-last        nil)   ; for collect
        (var-first       nil)   ; for min/max
        (typ-acc         nil)
        (var-it?         nil)
        (var-it          (gensym "it"))
        (var-repeat      nil) )
    (labels (
      ;; bind-one-time-init-dvar
      ;;
      ;; Description:
      ;;  Binds variables that are initialized one time only.
      ;;
      ;;  When initform is global constant or list form, dvars are initialized
      ;;  in let instead of destructuing form.
      ;;
      (bind-one-time-init-dvar (dvar type initform)
        (labels (
          ;; bind-dvar
          (bind-dvar (dvar type initval decl)
            (typecase dvar
              (null)
              (cons
                (bind-dvar.dvar 'car dvar type initval)
                (bind-dvar.dvar 'cdr dvar type initval) )
              (otherwise
                (bind-simple-var dvar type initval decl) )) )

          ;; bind-dvar.dvar
          (bind-dvar.dvar (reader dvar.dvar type initval)
              (declare (type (member car cdr) reader))
              (declare (type cons dvar.dvar))
            (let ((dvar (funcall reader dvar.dvar)))
              (when dvar
                 (unless (eq unspecified type)
                   (when (consp type)
                     (setq type (funcall reader type)) ))

                 (unless (eq unspecified initval)
                   (setq initval (funcall reader initval)) )

                 (bind-dvar dvar type initval 'ignorable) ) ) )
          )
          ;;
          ;; bind-one-time-init-dvar
          ;;
          (cond
            ((or (not (consp dvar)) (eq unspecified initform))
              (bind-dvar dvar type initform nil) )

            ((constantp initform)
              (bind-dvar dvar type (eval initform) nil) )

            ((let ((nvars (si::proper-list-p dvar)))
              (and nvars
                   (every #'(lambda (x) (and x (symbolp x))) dvar)
                   (eql (si::proper-list-p initform) (1+ nvars))
                   (eq (first initform) 'list) ) )
              (bind-dvar dvar type (rest initform) nil) )

            (t
              (when (consp dvar)
                (let ((var (gensym "struct")))
                   (bind-simple-var var 'list initform)
                   (push (cons dvar var) destructs) ))
              (bind-dvar dvar type unspecified nil) )) ) )

      ;; bind-arith-svar
      ;;
      ;; Description:
      ;;  Bind numeric simple variable.
      ;;
      (bind-arith-svar (svar type initform)
        (when (consp svar)
          (loop-error
            "Can't use destructuring with arithmetic iteration: ~S"
            svar ))
        (when (eq unspecified type) (setq type 'real))
        (bind-psetq-svar svar type initform) )


      ;; bind-psetq-dvar
      ;;
      (bind-psetq-dvar (dvar type initform)
        (labels (
          (bind-dvar (dvar type decl)
            (if (not (consp dvar))
                (bind-simple-var dvar type unspecified decl)
              (progn
                (bind-dvar.dvar 'car dvar type)
                (bind-dvar.dvar 'cdr dvar type) )) )

          (bind-dvar.dvar (reader dvar.dvar type)
              (declare (type (member car cdr) reader))
              (declare (type cons dvar.dvar))
            (let ((dvar (funcall reader dvar.dvar)))
              (when dvar
                 (unless (eq unspecified type)
                   (when (consp type)
                     (setq type (funcall reader type)) ))
                 (bind-dvar dvar type 'ignorable) ) ) )
          )
          ;;
          ;; bind-psetq-dvar
          ;;
          (when (null dvar)
            (setq dvar (gensym)) )
          (typecase dvar
            (cons
              (let ((svar (gensym "struct")))
                (bind-psetq-dvar svar 'list initform)
                (setq initform svar)
                (push (cons dvar svar) destructs)
                (bind-dvar dvar type nil)
                svar ) )
            (otherwise
              (bind-psetq-svar dvar type initform)
              dvar )) ) )

      ;; bind-psetq-svar
      ;;
      (bind-psetq-svar (svar type initform)
          (declare (type symbol svar))
        (unless (eq unspecified initform)
          (unless (constantp initform env)
            (push svar     head-psetq*)
            (push initform head-psetq*)
            (setq initform unspecified) ))
        (bind-simple-var svar type initform) )

      ;; bind-setq-dvar
      ;;
      (bind-setq-dvar (dvar type initform)
        (labels (
          (bind-dvar (dvar type decl)
            (typecase dvar
              (null)
              (cons
                (bind-dvar.dvar 'car dvar type)
                (bind-dvar.dvar 'cdr dvar type) )
              (otherwise
                (bind-simple-var dvar type unspecified decl) )) )

          (bind-dvar.dvar (reader dvar.dvar type)
              (declare (type (member car cdr) reader))
              (declare (type cons dvar.dvar))
            (let ((dvar (funcall reader dvar.dvar)))
              (when dvar
                 (unless (eq unspecified type)
                   (when (consp type)
                     (setq type (funcall reader type)) ))
                 (bind-dvar dvar type 'ignorable) ) ) )
          )
          ;;
          ;; bind-setq-dvar
          ;;
          (when (null dvar) (setq dvar (gensym)))
          (typecase dvar
            (cons
              (let ((svar (gensym "struct")))
                (bind-setq-dvar svar 'list initform)
                (setq initform svar)
                (push (cons dvar svar) destructs)
                (bind-dvar dvar type nil)
                svar ) )
            (otherwise
              (bind-setq-svar dvar type initform 'ignorable)
              dvar )) ) )

      ;; bind-setq-svar
      ;;
      (bind-setq-svar (svar type initform &optional decl)
          (declare (type symbol svar))
        (unless (eq unspecified initform)
          (unless (constantp initform env)
            (push svar     head-setq*)
            (push initform head-setq*)
            (setq initform unspecified) ))
        (bind-simple-var svar type initform decl) )

      ;; bind-simple-var
      ;;
      (bind-simple-var (var type initform &optional decl)
        (check-var var)
        (push (list var type initform decl) vars) )

      ;; check-loop-value
      (check-loop-value (owner)
        (cond
          ((null loop-value-owner)
            (setq loop-value-owner owner) )
          (var-acc
            (loop-error
              "Can't use ~A clause with ~A clause."
              loop-value-owner
              owner ) )
          (t
            ;; Second always, nerver or thereis
            (loop-warn
              "Loop value is already provided by ~A clause."
              loop-value-owner ) )) )

      ;; check-var
      ;;
      (check-var (var)
        (unless (symbolp var)
          (loop-error "Variable name must be a symbol: ~S" var) )

        (when (keywordp var)
          (loop-error "Can't use keyword as variable: ~S" var) )

        (when (constantp var env)
          (loop-error "Can't use constant name as variable: ~S" var) )

        (when (string= "IT" (symbol-name var))
          (style-warn "We recommend not to use ~S in ~S." var 'loop) )

        (when (member var names :test #'eq)
          (loop-error "LOOP variable ~S is already used." var) )

        (push var names) )

      ;; convert-to-keyword
      ;;
      (convert-to-keyword (token)
        ;; BUGBUG: REVIEW: case sensitive lisp
        (cond
          ((not token)           token)
          ((not (symbolp token)) token)
          ((keywordp token)      token)
          (t (intern (symbol-name token) #.(symbol-package :key))) ) )

      ;; end-frame
      ;;
      (end-frame ()
        (push (list (nreverse vars)                 ; 0
                    around*                         ; 1
                    (nreverse head-pre-test*)       ; 2
                    (nreverse head-psetq*)          ; 3
                    (nreverse head-post-test*)      ; 4
                    (nreverse head-setq*)           ; 5
                    (nreverse tail-pre-test*)       ; 6
                    (nreverse tail-psetq*)          ; 7
                    (nreverse tail-post-test*)      ; 8
                    (nreverse tail-setq*)           ; 9
                    (nreverse destructs) )          ; 10
              frames )
        (setq vars            '())
        (setq destructs       '())
        (setq head-pre-test*  '())
        (setq head-psetq*     '())
        (setq head-post-test* '())
        (setq head-setq*      '())
        (setq tail-pre-test*  '())
        (setq tail-psetq*     '())
        (setq tail-post-test* '())
        (setq tail-setq*      '())
        (setq around*         '()) )

      ;; expect
      ;;
      (expect (raw-token keywords)
        (unless (member (convert-to-keyword token) keywords :test #'eq)
          (loop-error "Expect ~{~#[~;or ~]~A~^, ~} instead of ~A."
                      keywords
                      raw-token )) )

      ;; get-form
      ;;
      (get-form ()
        (when (null src-scan) (loop-error "Missing form"))
        (setq src-last src-scan)
        (pop src-scan) )

      ;; get-maybe-type
      ;;
      (get-maybe-type (type eof-error?)
        (cond
          ((member raw-token '(fixnum float nil t) :test #'eq)
            (setq type raw-token)
            (get-token eof-error?) )

          ((consp raw-token)
            (setq type raw-token)
            (get-token eof-error?) )

          ((eq :of-type token)
            (when (null src-scan) (loop-error "Missing type specifier"))
            (setq src-last src-scan)
            (setq type (pop src-scan))
            (get-token eof-error?) ) )
        type )

      ;; get-token
      ;;
      (get-token (eof-error-p)
        (if (null src-scan)
            (progn
              (when eof-error-p
                (loop-error "Unexpected end of LOOP clause.") )
              (setq raw-token nil)
              (setq token eof-marker) )
          (progn
            (setq src-last src-scan)
            (setq raw-token (pop src-scan))
            (setq token raw-token)
            (cond
              ((consp token)
                token )
              ((keywordp token)
                token )
              ((and token (symbolp token))
                (setq token (convert-to-keyword token)) )
              (t
               (loop-error "Expected LOOP keyword instead of: ~S"
                           token ) )) )) )

      ;; get-type
      ;;
      (get-type ()
        (when (null src-scan) (loop-error "Missing type specifier"))
        (get-token t)
        (get-maybe-type unspecified nil) )

      ;; get-var
      ;;
      (get-var ()
        (when (null src-scan) (loop-error "Missing variable."))
        (setq src-last src-scan)
        (pop src-scan) )

      ;; invalid-type-spec
      ;;
      #|
      (invalid-type-spec (var type)
        (loop-error "Invalid type specifier for LOOP variable ~S: ~S"
                    var type ) )
      |#

      ;; invalid-var-spec
      ;;
      #|
      (invalid-var-spec (var)
        (loop-error "Invalid LOOP variable specifier: ~S" var) )
      |#

      ;; loop-error
      ;;
      ;; BUGBUG: loop-error must signal as program-error
      ;;
      (loop-error (control &rest args)
        (macro-error "~@<LOOP: ~?~%Context: ~{~A~^ ~}~:>"
               control
               args
               (remove-if (complement #'symbolp)
                          (subseq source 0 (position src-clause source)) )) )

      ;; loop-warn
      (loop-warn (control &rest args)
        (style-warn "~@<LOOP: ~?~%Context: ~{~A~^ ~}~:>"
               control
               args
               (remove-if (complement #'symbolp)
                          (subseq source 0 (position src-clause source)) )) )

      ;; make-frame-form
      ;;
      (make-frame-form (frames form)
        (dolist (frame frames form)
          ;; Collect variable binidngs and declarations
          ;;
          (let* ((binding* '())
                 (decl*    '()) )
            (dolist (spec (first frame))
              (let ((var  (first  spec))
                    (type (second spec))
                    (init (third  spec))
                    (decl (fourth spec)) )
                (push `(,var ,(make-initform type init)) binding*)

                (unless (or (eq 't type) (eq unspecified type))
                  (push `(declare (type ,type ,var)) decl*) )

                (when decl
                  (push `(declare (,decl ,var)) decl*) ) ) )

            (setq binding* (nreverse binding*))
            (setq decl*    (nreverse decl*))

            ;; Process around
            ;;
            (dolist (around (second frame))
              (setq form (append around (list form))) )

            (setq form `(let ,binding* ,@decl* ,form)) ) ) ) 

      ;; make-destruct-form
      ;;
      (make-destruct-form (dvar.svar)
        (let ((binding*   '())
              (decl*      '())
              (setq-form* '()) )
          (labels (
            ;; destruct-dvar
            (destruct-dvar (dvar form)
                (declare (values ext:unspecified))
              (if (not (consp dvar))
                  (push `(setq ,dvar ,form) setq-form*)
                (progn
                  (destruct-dvar.dvar 'car dvar form)
                  (destruct-dvar.dvar 'cdr dvar form) )) )

            ;; destruct-dvar.dvar
            (destruct-dvar.dvar (reader dvar.dvar form)
                (declare (type symbol reader))
                (declare (type cons   dvar.dvar))
                (declare (type (or cons symbol) form))
                (declare (values ext:unspecified))
              (let ((dvar (funcall reader dvar.dvar)))
                (when dvar
                  (if (not (consp dvar))
                      (setq form `(,reader ,form))
                    (let ((var (gensym (symbol-name reader))))
                      (push `(declare (type list ,var)) decl*)
                      (push (list var `(,reader ,form)) binding*)
                      (setq form var) ))
                  (destruct-dvar dvar form) ) ) )
            )
            ;;
            ;; make-destruct-form
            ;;
            (destruct-dvar (car dvar.svar) (cdr dvar.svar))
            (setq binding*   (nreverse binding*))
            (setq setq-form* (nreverse setq-form*))
            `(let* ,binding* ,@decl* ,@setq-form*) ) ) )

      ;; make-funcall-form
      ;;
      (make-funcall-form (fn &rest arg*)
        (cond
          ((and (xc::one-argument-form-p fn 'function)
                (symbolp (second fn)) )
            `(,(second fn) ,@arg*) )
          ((constantp fn env)
            `(funcall ,fn ,@arg*) )
          (t
            (let ((var-fn (gensym)))
              (bind-psetq-svar var-fn '(or symbol function) fn)
              `(funcall ,var-fn ,@arg*) ) )) )

      ;; make-initform
      ;;
      (make-initform (type initform)
        (cond
          ((eq unspecified initform)
            (cond
              ((eq unspecified type)           '(ext:unspecified))
              ((subtypep type 'double-float)   0d0)
              ((subtypep type 'single-float)   0f0)
              ((subtypep type 'short-float)    0s0)
              ((subtypep type 'long-float)     0l0)
              ((subtypep type 'float)          0.0)
              ((subtypep type 'number)         0)
              (t                               '(ext:unspecified))) )
          ((realp initform)
            (cond
              ((null type) initform)
              ((eq unspecified type) initform)
              ((subtypep type 'double-float) (float initform 0d0))
              ((subtypep type 'float) (float initform 0f0))
              (t initform) ) )
          (t
            initform )) )

      ;; make-psetq-form
      ;;
      (make-psetq-form (psetq*)
        (cond
          ((null psetq*)         nil)
          ((null (cddr psetq*)) `(setq  ,@psetq*))
          (t                    `(psetq ,@psetq*)) ) )

      ;; make-setq-form
      ;;
      (make-setq-form (setq*)
        `(setq ,@setq*) )

      ;; make-test-form
      ;;
      (make-test-form (test*)
        (cond
          ((null (rest test*))
            `(when ,(first test*) ,finish-form) )
          (t
           `(when (or ,@test*) ,finish-form) )) )

      ;; mixed-accumulation
      ;;
      (mixed-accumulation ()
        (loop-error "Can't mix numeric and list LOOP value accumulation up.") )

      ;; multiple-phrase
      ;;
      (multiple-phrase (word1 word2)
        (loop-error "Can't use ~A with ~A." word2 word1) )

      ;; parse-accumulation
      ;;
      (parse-accumulation (name type initform typ-aux)
          (declare (type ext:type-specifier type))
          (declare (type t initform))
          (declare (type ext:type-specifier typ-aux))
          (declare (values t symbol symbol))

        (let ((form       (get-form))
              (var-simple nil)
              (var-aux    nil) )

          (when (eq (convert-to-keyword form) :it)
            (use-var-it)
            (setq form var-it) )

          (get-token nil)

          (if (eq :into token)
            ;; Accumlate into specified variable.
            ;;
            (progn
              (setq var-simple (get-form))
              (get-token nil)
              (unless (eq 'list type)
                (setq type (get-maybe-type type nil)) )

              (let ((acc.aux (assoc var-simple acc-vars :test #'eq)))
                (if acc.aux
                    (setq var-aux (cdr acc.aux))
                  (progn
                    (check-var var-simple)
                    (push (cons var-simple nil) acc-vars)
                    (setq initform (make-initform type initform))
                    (push `(,var-simple ,initform) body-binding*)
                    (setq acc.aux (list var-simple))
                    (push acc.aux acc-vars) ))

                (when (and typ-aux (not var-aux))
                  (setq var-aux (gensym (symbol-name var-simple)))
                  (push var-aux body-binding*)
                  (push `(declare (type ,typ-aux ,var-aux)) body-decl*)
                  (setf (cdr acc.aux) var-aux) ) ))

            ;; Accumlate into internal variable.
            ;;
            (progn
              (if (eq 'list type)
                  (when (and typ-acc (not (eq 'list typ-acc)))
                    (mixed-accumulation) )
                (progn
                  (setq type (get-maybe-type type nil))

                  (when typ-acc
                    (when (eq 'list typ-acc)
                      (mixed-accumulation) )

                    (when (subtypep type typ-acc)
                      (setq type typ-acc) ))) )

              (when (null var-acc)
                (setq var-acc (gensym "acc"))

                (check-loop-value name)

                ;; Auxiliary variable for list accumulator
                (when (eq 'list type)
                  (setq var-last (gensym "last"))
                  (push `(,var-last ,var-acc) body-binding*)
                  (push `(declare (type list ,var-last)) body-decl*)
                  (setq initform '(list nil)) )

                ;; Note: type declaration for var-acc will be generated
                ;; later.
                (setq initform (make-initform type initform))
                (push `(,var-acc ,initform) body-binding*) )

              (setq typ-acc    type)
              (setq var-simple var-acc)
              (setq var-aux    var-last) ))

          (values form var-simple var-aux) ) )

      ;; parse-acc-append
      ;;
      (parse-acc-append ()
        (multiple-value-bind (val-form var-simple var-last)
            (parse-accumulation :append 'list '() 'list)
          (if (eq var-simple var-acc)
              (push `(let ((,var-it (copy-list ,val-form)))
                       (when ,var-it
                         (setf (cdr ,var-last) ,var-it)
                         (setq ,var-last (last ,var-it)) ))
                      body-form* )
            (push `(let ((,var-it (copy-list ,val-form)))
                      (when ,var-it
                        (if ,var-last
                            (setf (cdr ,var-last) ,var-it)
                          (setq ,var-simple ,var-it) )
                        (setq ,var-last (last ,var-it))) )
                    body-form* )) ) )

      ;; parse-acc-collect
      ;;
      (parse-acc-collect ()
        (multiple-value-bind (val-form var-simple var-last)
            (parse-accumulation :collect 'list '() 'list)
          (if (eq var-simple var-acc)
              (push `(setq ,var-last (setf (cdr ,var-last) (list ,val-form)))
                    body-form* )
            (push `(let ((,var-it (list ,val-form)))
                     (if ,var-last
                         (setq ,var-last (setf (cdr ,var-last) ,var-it))
                       (setq ,var-last (setq ,var-simple ,var-it))) )
                  body-form* ) )) )

      ;; parse-acc-count
      ;;
      (parse-acc-count ()
        (multiple-value-bind (val-form var-simple)
            (parse-accumulation :count '(integer 0) 0 nil)
          (push `(when ,val-form (incf ,var-simple)) body-form*) ) )

      ;; parse-acc-max
      ;;
      (parse-acc-max ()
        (multiple-value-bind (val-form var-simple)
            (parse-accumulation :max 'real 0 nil)
          (unless var-first
            (setq var-first (gensym "first-p")) )
          (let ((var-form  (gensym "max")))
            (push `(let ((,var-form ,val-form))
                     (if (not ,var-first)
                         (setq ,var-simple (max ,var-form ,var-simple))
                       (progn
                         (setq ,var-first nil)
                         (setq ,var-simple ,var-form) )) )
                   body-form* ) ) ) )

      ;; parse-acc-min
      ;;
      (parse-acc-min ()
        (multiple-value-bind (val-form var-simple)
            (parse-accumulation :min 'real 0 nil)
          (unless var-first
            (setq var-first (gensym "first-p")) )
          (let ((var-form  (gensym "min")))
            (push `(let ((,var-form ,val-form))
                     (if (not ,var-first)
                         (setq ,var-simple (min ,var-form ,var-simple))
                       (progn
                         (setq ,var-first nil)
                         (setq ,var-simple ,var-form) )) )
                   body-form* ) ) ) )

      ;; parse-acc-nconc
      ;;
      (parse-acc-nconc ()
        (multiple-value-bind (val-form var-simple var-last)
            (parse-accumulation :nconc 'list '() 'list)
          (if (eq var-simple var-acc)
              (push `(let ((,var-it ,val-form))
                       (when ,var-it
                         (setf (cdr ,var-last) ,var-it)
                         (setq ,var-last (last ,var-it)) ))
                      body-form* )
            (push `(let ((,var-it ,val-form))
                     (when ,var-it
                       (if ,var-last
                           (setf (cdr ,var-last) ,var-it)
                         (setq ,var-simple ,var-it) )
                       (setq ,var-last (last ,var-it))) )
                    body-form* ) )) )

      ;; parse-acc-sum
      ;;
      (parse-acc-sum ()
        (multiple-value-bind (val-form var-simple)
            (parse-accumulation :sum 'number 0 nil)
          (push `(setq ,var-simple (+ ,var-simple ,val-form)) body-form*) ) )

      ;; parse-conditional
      ;;    "when" form clause {"and" clause}*
      ;;        ["else" clause {"and" clause}*]
      ;;        ["end"]
      ;;
      (parse-conditional (test-op)
          (declare (type (member when unless) test-op))
        (let ((saved-body-form* body-form*)
              (test-form (get-form))
              (then-form* '())
              (else-form* '()) )
          (setq body-form* '())

          (unless var-it?
            (setq var-it? t)
            (push var-it body-binding*) )

          (get-token t)
          (parse-selectable)
          (loop
            (case token
              ((:and)
                (get-token t)
                (parse-selectable) )
              ((:end)
                (setq then-form* (nreverse body-form*))
                (get-token nil)
                (return) )
              ((:else)
                (setq then-form* (nreverse body-form*))
                (setq body-form* '())
                (get-token t)
                (parse-selectable)
                (loop
                  (case token
                    ((:and)
                      (get-token t)
                      (parse-selectable) )
                    ((:end)
                      (get-token nil)
                      (return) )
                    (otherwise
                      (return) )))
                (setq else-form* (nreverse body-form*))
                (return) )
              (otherwise
                (setq then-form* (nreverse body-form*))
                (return) )))

          ;; Emit code
          ;;
          (setq body-form* saved-body-form*)
          (push `(setq ,var-it ,test-form) body-form*)
          (setq test-form var-it)

          (let ((if-form (if (not else-form*)
                              `(,test-op ,test-form ,@then-form*)
                            (progn
                               (if (rest then-form*)
                                   (setq then-form* `(progn ,@then-form*))
                                 (setq then-form* (first then-form*)) )

                               (if (rest else-form*)
                                   (setq else-form* `(progn ,@else-form*))
                                 (setq else-form* (first else-form*)) )

                               (when (eq 'unless test-op)
                                 (setq test-form `(not ,test-form)) )

                              `(if ,test-form ,then-form* ,else-form*) ))) )
            (push if-form body-form*) ) ) )

      ;; parse-doing
      ;;
      (parse-doing ()
        (let ((nforms 0))
          (loop
            (get-token nil)
            (when (or (eq eof-marker token) (symbolp token))
              (when (zerop nforms)
                (loop-error "Missing form.") )
              (return) )

            (push raw-token body-form*)
            (incf nforms) )) )

      ;; parse-finally
      ;;
      (parse-finally ()
        (let ((nforms 0))
          (loop
            (get-token nil)
            (when (or (eq eof-marker token) (symbolp token))
              (when (zerop nforms)
                (loop-error "Missing form.") )
              (return) )

            (push token finally*)
            (incf nforms) ) ) )

      ;; parse-for
      ;;
      (parse-for ()
        (loop
          (let ((var  (get-var))
                (type (get-type)) )
            (case token
              ((:from :downfrom :upfrom
                :to :downto :upto :below :above
                :by )
                         (parse-for-arith  var type))
              ((:in)     (parse-for-in     var type))
              ((:on)     (parse-for-on     var type))
              ((:=)      (parse-for-=      var type))
              ((:across) (parse-for-across var type))
              ((:being)  (parse-for-being  var type))
              (otherwise
                (loop-error "Unknow keyword ~A in ~A clause."
                            token
                            :for ) ))

            (unless (eq :and token)
              (end-frame)
              (return) ) )) )

      ;; parse-for-=
      ;;    for var [type] = form1 [then form2]
      ;;
      ;; Note: We can't optimize for x = 1. Because of, variable x maybe
      ;; modified in loop body.
      ;;
      (parse-for-= (dvar type)
        (let* ((form (get-form))
               (svar (bind-psetq-dvar dvar type form)) ) 
          (get-token nil)
          (when (eq :then token)
            (setq form (get-form))
            (get-token nil) )
          (step-psetq-svar svar form) ) )

      ;; parse-for-across
      ;;    for var [type] across vector
      ;;
      (parse-for-across (dvar type)
        (let* ((var-vec      (gensym "vec_"))
               (var-idx      (gensym "idx_"))
               (var-len      (gensym "len_"))
               (vector-form  (get-form))
               (reader-form `(row-major-aref ,var-vec ,var-idx)) )
          (get-token nil)

          (bind-simple-var var-vec 'vector     vector-form)
          (bind-simple-var var-len 'ext:sequence-index 0)
          (bind-psetq-svar var-idx 'ext:sequence-index 0)

          (push `(setq ,var-len (length ,var-vec)) prologue*)

          (push `(>= ,var-idx ,var-len) head-post-test*)
          (push (first head-post-test*) tail-post-test*)

          (step-psetq-svar var-idx `(1+ ,var-idx))

          (let ((var-elt (bind-setq-dvar dvar type reader-form)))
            (step-setq-svar var-elt reader-form) ) ) )

      ;; parse-for-arith
      ;;
      ;; Note: To keep evaluation order, each processing "from" and "to"
      ;; binds variable for form.
      ;;
      (parse-for-arith (var type)
        (let (step-fn from to test-fn by from-form to-form by-form)
          (loop
            (case token
              ((:from)
                (when from (multiple-phrase from token))
                (setq from token)

                (setq from-form (get-form))
                (unless (constantp from-form env)
                  (let ((var (gensym)))
                    (bind-simple-var var 'real from-form)
                    (setq from-form var) )) )

              ((:downfrom)
                (when from (multiple-phrase from token))
                (setq from token)

                (setq step-fn '-)

                (setq from-form (get-form))
                (unless (constantp from-form env)
                  (let ((var (gensym)))
                    (bind-simple-var var 'real from-form)
                    (setq from-form var) )) )

              ((:upfrom)
                (when from (multiple-phrase from token))
                (setq from token)

                (setq step-fn '+)

                (setq from-form (get-form))
                (unless (constantp from-form env)
                  (let ((var (gensym)))
                    (bind-simple-var var 'real from-form)
                    (setq from-form var) )) )

              ((:to)
                (when to (multiple-phrase to token))
                (setq to token)

                (setq to-form (get-form))
                (unless (constantp to-form env)
                  (let ((var (gensym)))
                    (bind-simple-var var 'real to-form)
                    (setq to-form var) )) )

              ((:downto)
                (when to (multiple-phrase to token))
                (setq to token)

                (setq step-fn '-)
                (setq test-fn '<)

                (setq to-form (get-form))
                (unless (constantp to-form env)
                  (let ((var (gensym)))
                    (bind-simple-var var 'real to-form)
                    (setq to-form var) )) )

              ((:upto)
                (when to (multiple-phrase to token))
                (setq to token)

                (setq step-fn '+)
                (setq test-fn '>)

                (setq to-form (get-form))
                (unless (constantp to-form env)
                  (let ((var (gensym)))
                    (bind-simple-var var 'real to-form)
                    (setq to-form var) )) )

              ((:below)
                (when to (multiple-phrase to token))
                (setq to token)

                (setq step-fn '+)
                (setq test-fn '>=)

                (setq to-form (get-form))
                (unless (constantp to-form env)
                  (let ((var (gensym)))
                    (bind-simple-var var 'real to-form)
                    (setq to-form var) )) )

              ((:above)
                (when to (multiple-phrase to token))
                (setq to token)

                (setq step-fn '-)
                (setq test-fn '<=)

                (setq to-form (get-form))
                (unless (constantp to-form env)
                  (let ((var (gensym)))
                    (bind-simple-var var 'real to-form)
                    (setq to-form var) )) )

              ((:by)
                (when by (multiple-phrase to token))
                (setq by token)

                (setq by-form (get-form))
                (unless (constantp by-form env)
                  (let ((var (gensym)))
                    (bind-simple-var var 'real by-form)
                    (setq by-form var) )) )

              (otherwise (return)) )
          (get-token nil) )

          ;; Set default values
          ;;
          (unless by (setq by-form 1))

          (cond
            ((not test-fn)
              (unless from (setq from-form 0))
              (when to (setq test-fn (if (eq step-fn '-) '< '>)))
              (unless step-fn (setq step-fn '+)) )

            ((not from)
              (setq from-form 0) ))

          ;; Emit iteration frame
          ;;
          (when (null var) (setq var (gensym)))
          (bind-arith-svar var type from-form)
          (when test-fn
            (push `(,test-fn ,var ,to-form) head-post-test*)
            (push (first head-post-test*)   tail-post-test*) )
          (step-psetq-svar var `(,step-fn ,var ,by-form)) ) )

      ;; parse-for-being
      ;;    "for" var [type] "being" {"each" | "the"}
      ;;
      (parse-for-being (var type)
        (get-token t)
        (expect raw-token '(:each :the))
        (case (get-token t)
          ((:external-symbol :external-symbols)
            (parse-for-being-pkg var type '(:external)) )

          ((:hash-key :hash-keys)
            (parse-for-being-htb var type :key) )

          ((:hash-value :hash-values)
            (parse-for-being-htb var type :value) )

          ((:present-symbol :present-symbols)
            (parse-for-being-pkg var type '(:internal :external)) )

          ((:symbol :symbols)
            (parse-for-being-pkg var type '(:internal :external :inherited)) )

          (otherwise
            (expect raw-token
                    '(:hash-key   :hash-keys
                      :hash-value :hash-values
                      :symbol     :symbols ) ))) )

      ;; prase-for-being-htb
      ;;
      ;; Arguments:
      ;;  var   loop variable
      ;;  type  type of loop variable
      ;;  kind  :key or :value
      ;;
      ;; Syntax:
      ;;    {"in" | "of"} hash-table ["using" ("hash-value" var)]
      ;;
      (parse-for-being-htb (var type kind)
        (get-token t)
        (expect raw-token '(:in :of))
        (let ((htb-form (get-form))
              (var-htb  (gensym "htb"))
              (fn-next  (gensym "next"))
              (var-more (gensym "more"))
              (var-key  (if (eq :key   kind) var  nil))
              (typ-key  (if (eq :key   kind) type 't))
              (var-val  (if (eq :value kind) var  nil))
              (typ-val  (if (eq :value kind) type 't)) )
          (get-token nil)

          ;; USING clause.
          ;;
          (when (eq :using token)
            (let* ((form  (get-form))
                   (using (and (eql (si::proper-list-p form) 2)
                               (convert-to-keyword (first form)) )) )
              (cond
                ((and (eq :key kind)   (eq :hash-value using))
                  (setq var-val (second form)) )

                ((and (eq :value kind) (eq :hash-key using))
                  (setq var-key (second form)) )

                (t
                  (loop-error "~A clause must be (~A var)."
                              :using
                              (if (eq :key kind) :hash-value :hash-key) )))
              (get-token nil) ))

          ;; Loop path of hash-table.
          ;;
          (bind-simple-var var-htb  'hash-table htb-form)
          (bind-simple-var var-more 't nil)

          (if var-key
              (setq var-key (bind-setq-dvar var-key typ-key unspecified))
            (progn
              (setq var-key (gensym "key"))
              (bind-simple-var var-key 't nil) ))

          (when var-val
            (setq var-val (bind-setq-dvar var-val typ-val unspecified)) )

          (push `(with-hash-table-iterator (,fn-next ,var-htb)) around*)

          (push `(not (multiple-value-setq (,var-more
                                            ,var-key
                                            ,@(and var-val (list var-val)))
                       (,fn-next) ))
                tail-post-test* )

          (push (first tail-post-test*) head-post-test*) ) )

      ;; parse-for-being-pkg
      ;;
      ;; Arguments:
      ;;  var   loop variable
      ;;  type  type of loop variable
      ;;  kind  a list of :internal, :external, and :inherited. For
      ;;        with-package-iterator.
      ;;
      ;; Syntax:
      ;;    [{"in" | "of"} package]
      ;;
      (parse-for-being-pkg (var type kind)
        (let ((fn-next  (gensym "next"))
              (pkg-form '*package*) )
          (get-token nil)
          (when (or (eq :in token) (eq :of token))
            (setq pkg-form (get-form))
            (get-token nil) )

          (when (consp var)
            (loop-error
                "Can't use destructuring for package loop: ~S"
                var ))

          (if (eq unspecified type)
              (setq type 'symbol)
            (multiple-value-bind (subtypep validp) (subtypep 'symbol type)
              (when (and validp (not subtypep))
                (loop-error
                    "Type ~S must be super type of ~S."
                    type
                    'symbol ))))

          ;; Loop path of package.
          ;;
          (push `(with-package-iterator (,fn-next (list ,pkg-form) ,@kind))
                around* )

          (if (null var)
              (push `(not (,fn-next)) tail-post-test*)
            (let ((var-more (gensym "more")))
              (bind-simple-var var-more 't nil)
              (bind-simple-var var type nil)
              (push `(not (multiple-value-setq (,var-more ,var) (,fn-next)))
                    tail-post-test* )))
          (push (first tail-post-test*) head-post-test*) ) )

      ;; parse-for-in
      ;;    for var [type] in form [by step-fn]
      ;;
      (parse-for-in (dvar type)
        (let* ((svar        (gensym "list_"))
               (reader-form `(car ,svar))
               (step-fn     '#'cdr)
               (list-form   (get-form)) )
          (get-token nil)
          (when (eq :by token)
            (setq step-fn (get-form))
            (get-token nil) )

          (bind-psetq-svar svar 'list list-form)

          (push `(endp ,svar) head-post-test*)
          (push (first head-post-test*) tail-post-test*)

          (step-psetq-svar svar (make-funcall-form step-fn svar))

          (let ((var-elt (bind-setq-dvar dvar type reader-form)))
            (step-setq-svar var-elt reader-form) ) ) )

      ;; parse-for-on
      ;;
      (parse-for-on (dvar type)
        (let ((step-fn   '#'cdr)
              (list-form (get-form)) )
          (get-token nil)
          (when (eq :by token)
            (setq step-fn (get-form))
            (get-token nil) )

          (let ((svar (bind-psetq-dvar dvar type list-form)))
            (push `(atom ,svar) head-post-test*)
            (push (first head-post-test*) tail-post-test*)
            (step-psetq-svar svar (make-funcall-form step-fn svar)) ) ) )

      ;; parse-initially
      ;;
      (parse-initially ()
        (let ((nforms 0))
          (loop
            (let ((token (get-token nil)))
              (when (or (eq eof-marker token) (symbolp token))
                (when (zerop nforms)
                  (loop-error "Missing form for LOOP clause ~A" :initially) )
                (return token) )
              (push token initially*)
              (incf nforms) ) ) ) )

      ;; parse-return
      ;;
      (parse-return ()
        (let ((form (get-form)))
          (when (eq :it (convert-to-keyword form))
            (use-var-it)
            (setq form var-it) )
          (push `(return-from ,loop-name ,form) body-form*) )
        (get-token nil) )

      ;; parse-selectable
      ;;
      ;; Called by:
      ;;   parse-conditional
      ;;
      (parse-selectable ()
        (case token
          ;; unconditional
          ;;
          ((:do :doing) (parse-doing))
          ((:return)    (parse-return))

          ;; conditional
          ;;
          ((:if :when) (parse-conditional 'when))
          ((:unless)   (parse-conditional 'unless))

          ;; accumulation
          ;;
          ((:collect :collecting)  (parse-acc-collect))
          ((:append :appending)    (parse-acc-append))
          ((:nconc :nconcing)      (parse-acc-nconc))
          ((:count :counting)      (parse-acc-count))
          ((:sum :summing)         (parse-acc-sum))
          ((:maximize :maximizing) (parse-acc-max))
          ((:minimize :minimizing) (parse-acc-min)) ) )

      ;; parse-with
      ;;    with dvar [= form] {and name [= form]}*
      (parse-with ()
        ;; Collect variables
        (loop
          (let* ((var      (get-var))
                 (type     (get-type))
                 (initform unspecified) )
            (case token
              (:and)
              ((:=)
                (setq initform (get-form))
                (get-token nil) ))

            ;; We arrange initform is evaluated for anonymous variable,
            ;; since initform can have side-effect.
            (when (null var) (setq var (gensym)))

            (bind-one-time-init-dvar var type initform)
            (unless (eq :and token) (return)) ))

        ;; Make destrucuring forms
        (dolist (dvar.form destructs)
          (push (make-destruct-form dvar.form) around*) )
        (setq destructs nil)
        (end-frame) )

      ;; step-psetq-svar
      ;;
      (step-psetq-svar (svar step-form)
          (declare (type symbol svar))
        (push svar tail-psetq*)
        (push step-form tail-psetq*) )

      ;; step-setq-svar
      ;;
      (step-setq-svar (svar step-form)
          (declare (type symbol svar))
        (push svar tail-setq*)
        (push step-form tail-setq*) )

      ;; use-var-it
      ;;
      (use-var-it ()
        (unless var-it?
          (loop-error "Variable for ~A isn't initialized." 'it) ) )
      )
      ;;
      ;; parse-loop
      ;;
      (get-token nil)
      (setq src-clause src-last)

      ;; name-clause ::= 'named' name
      ;;
      (when (eq :named token)
        (get-token t)
        (setq loop-name raw-token)
        (get-token nil) )

      ;; variable-clause
      ;;
      (loop
        (setq src-clause src-last)
        (case token
          ((:with)      (parse-with))
          ((:initially) (parse-initially))
          ((:finally)   (parse-finally))
          ((:for :as)   (parse-for))
          (otherwise    (return)) ))

      ;; main-clause
      ;;
      (loop
        (setq src-clause src-last)
        (case token
          ;; unconditional
          ;;
          ((:do :doing) (parse-doing))
          ((:return)    (parse-return))

          ;; accumulation
          ;;
          ((:collect :collecting)  (parse-acc-collect))
          ((:append :appending)    (parse-acc-append))
          ((:nconc :nconcing)      (parse-acc-nconc))
          ((:count :counting)      (parse-acc-count))
          ((:sum :summing)         (parse-acc-sum))
          ((:maximize :maximizing) (parse-acc-max))
          ((:minimize :minimizing) (parse-acc-min))

          ;; conditional
          ;;
          ((:if :when) (parse-conditional 'when))
          ((:unless)   (parse-conditional 'unless))

          ;; termination-test
          ;;
          ((:repeat)
            (when var-repeat
              (loop-error "Only one LOOP clause ~A is allowed." token) )
            (setq var-repeat (gensym))
            (bind-psetq-svar var-repeat 'integer (get-form))
            (push `(<= ,var-repeat 0) head-post-test*)
            (push (first head-post-test*) tail-post-test*)
            (step-psetq-svar var-repeat `(1- ,var-repeat))
            (end-frame)
            (get-token nil) )

          ((:while)
            (push `(unless ,(get-form) ,finish-form) body-form*)
            (get-token nil) )

          ((:until)
            (push `(when ,(get-form) ,finish-form) body-form*)
            (get-token nil) )

          ((:always)
            (check-loop-value :always)
            (push `(unless ,(get-form)
                     (return-from ,loop-name nil) )
                  body-form* )
            (setq loop-value t)
            (get-token nil) )

          ((:never)
            (check-loop-value :never)
            (push `(when ,(get-form) (return-from ,loop-name nil)) body-form*)
            (setq loop-value t)
            (get-token nil) )

          ((:thereis)
            (check-loop-value :thereis)
            (let ((var (gensym)))
              (push `(let ((,var ,(get-form)))
                       (when ,var (return-from ,loop-name ,var)))
                  body-form* )
              (get-token nil) ) )

          ;; initial-final
          ;;
          ((:initially) (parse-initially))
          ((:finally)   (parse-finally))

          ;; Others
          ;;
          ((:with :for :as)
            (loop-error "~S clause must be appeared start of ~S form."
                        token
                        'loop ) )

          ((:and)
            (loop-error "~S clause must be used in ~S or ~S clause."
                        token
                        :for
                        :with ) )

          (otherwise
            (when (eq eof-marker token)
              (return) )

            (if (symbolp token)
                (loop-error "Unknown LOOP keyword: ~A" token)
              (loop-error "Missing LOOP keyword before: ~S" token) ) ) ))

      ;; Accumulation
      ;;
      (when var-first
        (push `(,var-first t) body-binding*) )

      ;; Emit loop code
      ;;
      (let ((tag-end-loop   (second finish-form))
            (tag-next-loop  (gensym "next-loop"))
            (head-form*     '())
            (tail-form*     '())
            (merge?         t)
            (merge-to-head? t) )

        ;; Build head-form and tail-form from inner to outer.
        ;;
        ;; x y z w a b c N <body> x q y z w a b c
        ;;  => x (go T) N a b c <body> x T y z w
        ;;
        (dolist (frame frames)
          (pop frame)                            ; 0 vars
          (pop frame)                            ; 1 around*
          (let ((head-pre-test*  (pop frame))    ; 2
                (head-psetq*     (pop frame))    ; 3
                (head-post-test* (pop frame))    ; 4
                (head-setq*      (pop frame))    ; 5
                (tail-pre-test*  (pop frame))    ; 6
                (tail-psetq*     (pop frame))    ; 7
                (tail-post-test* (pop frame))    ; 8
                (tail-setq*      (pop frame))    ; 9
                (destructs       (pop frame)) )  ; 10

            (when merge?
              (block merge
                (when destructs
                  (if merge-to-head?
                     (dolist (destruct destructs)
                       (push (make-destruct-form destruct) head-form*) )
                   (dolist (destruct destructs)
                     (push (make-destruct-form destruct) tail-form*) ))
                  (setq destructs nil) )

                (unless (equal head-setq* tail-setq*)
                  (setq merge? nil)
                  (return-from merge) )

                (when head-setq*
                  (if merge-to-head?
                      (push (make-setq-form head-setq*) head-form*)
                    (push (make-setq-form head-setq*) tail-form*) )
                  (setq head-setq* nil)
                  (setq tail-setq* nil) )

                (unless (equal head-post-test* tail-post-test*)
                  (setq merge? nil)
                  (return-from merge) )

                (when head-post-test*
                  (setq merge-to-head? nil)
                  (push (make-test-form head-post-test*) tail-form*)
                  (setq head-post-test* nil)
                  (setq tail-post-test* nil) )

                (unless (equal head-psetq* tail-psetq*)
                  (setq merge? nil)
                  (return-from merge) )

                (when head-psetq*
                  (if merge-to-head?
                      (push (make-psetq-form head-psetq*) head-form*)
                    (push (make-psetq-form head-psetq*) tail-form*) )
                  (setq head-psetq* nil)
                  (setq tail-psetq* nil) )

                (unless (equal head-pre-test* tail-pre-test*)
                  (setq merge? nil)
                  (return-from merge) )

                (when head-pre-test*
                  (setq merge-to-head? nil)
                  (push (make-test-form head-pre-test*) tail-form*)
                  (setq head-pre-test* nil)
                  (setq tail-pre-test* nil) ))

              (unless merge?
                (push tag-next-loop head-form*)
                (when tail-form*
                  (let ((tag-tail (gensym "loop-test")))
                    (push tag-tail tail-form*)
                    (push `(go ,tag-tail) head-form*) ))))

            (when destructs
              (dolist (destruct destructs)
                (let ((form (make-destruct-form destruct)))
                  (push form head-form*)
                  (push form tail-form*) ) ))

            ;; process head and tail
            ;;
            (when head-setq*
              (push (make-setq-form  head-setq*) head-form*) )

            (when head-post-test*
              (push (make-test-form  head-post-test*) head-form*) )

            (when head-psetq*
              (push (make-psetq-form head-psetq*) head-form*) )

            (when head-pre-test*
              (push (make-test-form  head-pre-test*) head-form*) )

            (when tail-setq*
              (push (make-setq-form  tail-setq*) tail-form*) )

            (when tail-post-test*
              (push (make-test-form  tail-post-test*) tail-form*) )

            (when tail-psetq*
              (push (make-psetq-form tail-psetq*) tail-form*) )

            (when tail-pre-test*
              (push (make-test-form  tail-pre-test*) tail-form*) ) ) )

        (when merge?
          (push tag-next-loop head-form*)
          (when tail-form*
            (let ((tag-tail (gensym "loop-test")))
              (push tag-tail tail-form*)
              (push `(go ,tag-tail) head-form*) )))

        (when (null head-form*)
          (push tag-next-loop head-form*) )

        ;; Body Form
        ;;
        (let (body-form)
          (setq prologue*  (nreverse prologue*))
          (setq initially* (nreverse initially*))
          (setq body-form* (nreverse body-form*))

          (when var-acc
            (if (not var-last)
                (push `(return-from ,loop-name ,var-acc) finally*)
              (push `(return-from ,loop-name (cdr ,var-acc)) finally*) ))

          (cond
            (finally*
              (setq finally* (nreverse finally*)) )
            (loop-value
              (setq finally* `((return-from ,loop-name ,loop-value)))) )

          (setq body-form
            `(macrolet ((cl:loop-finish () ',finish-form))
               (tagbody
                   ,@prologue*
                   ,@initially*
                   ,@head-form*
                   ,@body-form*
                   ,@tail-form*
                   (go ,tag-next-loop)
                ,tag-end-loop
                  ,@finally* )))

          (when body-binding*
            (when typ-acc
              (push `(declare (type ,typ-acc ,var-acc)) body-decl*) )
            (setq body-form `(let* ,body-binding* ,@body-decl* ,body-form)) )

          ;; Build result form
          ;;
          ;; Processing frames from inner to outer.
          ;;
          `(macrolet ((cl:loop-finish () (invalid-loop-finish)))
             (block ,loop-name
               ,(make-frame-form frames body-form) )) ) ) ) ) )


;;;; 6.2.4 loop (simple form)
;;;
;;; Syntax:
;;;   (loop {form}*) => {result}*
;;;
;;; Expansion:
;;;   (block nil (tagbody #:loop form... (go #:loop)))
;
(defmacro cl:loop (&rest form* &environment env)
  (cond
    ((null form*)
      (error "Infinite loop") )
    ((some #'atom form*)
      (parse-loop form* env) )
    (t
      (let ((tag-loop (gensym "loop")))
        `(block nil (tagbody ,tag-loop ,@form* (go ,tag-loop))) ) )) )


;;;; 6.2.5 loop-finish
;;;
;;; Description:
;;;  Catches loop-finish macro outside of loop form. This message is more
;;;  helpful than MIT Loop's.
;;;
;;; Note:
;;;  MIT Loop: >Error: SI::END-LOOP unseen go tag.
;;;  CLISP:    >Error: (loop-finish) is not possible here.
;;;
;
(defmacro cl:loop-finish ()
  (error 'invalid-local-macro-call
         :name   'cl:loop-finish
         :parent 'cl:loop ) )


;;;; invalid-loop-finish
;;;
;;; Description:
;;;  Singals error for using loop-finish in variable-clause.
;
(defun invalid-loop-finish ()
  (error "~S macro can be used only ~S main-clause."
         'cl:loop-finish
         'cl:loop ) )
