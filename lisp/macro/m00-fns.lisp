;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evck - macro - Functions for Defining Macros
;;; macro/m00-fns.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m00-fns.lisp#6 $
;;;
;;;
;;; Description:
;;;  This file contains implementation for functions used by macro-expanders.
;;;
;;;     analyze-body
;;;     analyze-lambda-list
;;;     compiler-eval
;;;     constant-value-p
;;;     error-lambda-list-mismatched
;;;     make-ignorable-form
;;;     macro-internal-error
;;;     make-keyword-parser
;;;     make-vars
;;;     parse-destructuring-bind
;;;     parse-lambda-list
;;;     parse-macro
;;;     parse-macro-aux
;;;     parse-macro-lambda-list
;;;     program-error
;;;     style-warn
;;;     syntax-error
;;;     type-and
;;;
;;; Note:
;;;  Genesis also defines analyze-body.
;;;
;
(in-package :xc)

;;;; Analyze Body
;;;
;;; Returns:
;;;  decls - List of declarations.
;;;  body  - List of forms.
;;;  doc   - Documentation or nil.
;
(defun analyze-body (body &optional allow-doc-p)
  (let ((decls '())
        (doc nil) )
    (loop
      (unless (consp body) (return))
      (unless (and (consp (car body)) (eq 'declare (caar body))) (return))
      (push (pop body) decls) )

    (setq decls (nreverse decls))

    (when (and allow-doc-p (consp body) (stringp (car body)))
      (setq doc (pop body)) )

    (values decls body doc) ) )


;;;; Analyze Lambda List
;;;
;;; Syntax:
;;;     analyze-lambda-list list &optional body-p env-p
;;;         => scan,
;;;            requireds, optionals, rests, keywords,
;;;            axuiliarries
;;;            env
;;;
;;; Arguments and Values:
;;;    body-p           Allows &body in lambda-list
;;;    env-p            Allows &env in lambda-list.
;;;    rest             Next scan point
;;;    requireds        List of element.
;;;    optionals        List of element followed by &optional.
;;;    rests            List of element followed by &rest.
;;;    keywords         List of element followed by &key or &allow-other-keys.
;;;    auxiliaries      List of element followed by &aux.
;;;    env              Name of environment variable
;;;
;;; Description:
;;;  Analyzes ordinary and macro lambda list and returns each element as
;;;  values. Parameter names aren't checked in this function but others are
;;;  checked. Caller should check parameter names.
;;;
;;; keywords       ::= (keyword-symbol keyword-spec*)
;;; keyword-symbol ::= &key | &allow-other-key
;;; keyword-spec   ::= keyname var svar initform initform-p
;;;
;;; Example:
;;; (analyze-lambda-list
;;;     '(a b &optional c d &rest e &key f g &allow-other-keys &aux h i) nil )
;;; =>
;;; Value[0]=NIL
;;; Value[1]=(:REQUIRED A B)
;;; Value[2]=(&OPTIONAL C D)
;;; Value[3]=(&REST E)
;;; Value[4]=(&ALLOW-OTHER-KEYS (:F F NIL NIL NIL) (:G G NIL NIL NIL))
;;; Value[5]=(&AUX H I)
;;; Value[6]=NIL
;;;
;
(defun analyze-lambda-list (list &optional body-p env-p)
    (declare (type t list))
    (declare (values list list list list list list symbol))
  (let ((requireds '())
        (optionals '())
        (rests '())
        (keywords '())
        (auxiliaries '())
        (env nil)
        (scan list)
        (elt nil) )

   (labels (
     (check-env ()
       (when env-p
         (block nil
           (unless (consp scan) (setq elt nil) (return))
           (setq elt (car scan))

           (when (eq '&environment elt)
             (when env
               (macro-error "~S is appread more than once." elt) )

             (pop scan)

             (unless (consp scan)
               (macro-error "Missing variable name after ~S." elt) )

             (setq env (pop scan))

             (unless (symbolp env)
               (macro-error "Parameter ~S must be a symbol." env) )))) )
      )
      ;;
      ;; body of labels
      ;;
      (check-env)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; required
      ;;
      (loop
        (unless (consp scan) (setq elt nil) (return))
        (setq elt (car scan))

        (when (member elt lambda-list-keywords :test #'eq) (return))
        (setq scan (cdr scan))
        (push elt requireds) )

      (check-env)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; &optional
      ;;
      (when (eq '&optional elt)
        (push elt optionals)
        (pop scan)

        (loop
          (unless (consp scan) (setq elt nil) (return))
          (setq elt (car scan))

          (when (member elt lambda-list-keywords :test #'eq) (return))

          (let (var initform svar)
              (declare (ignorable var initform))
            (cond
              ((symbolp elt)
                (setq var elt)
                (setq elt (list elt)) )

              ((and (consp elt) (null (rest elt)))
                (setq var (first elt)) )

              ((<= 2 (si::safe-list-length elt) 3)
                (setq var      (first elt))
                (setq initform (second elt))
                (setq svar     (third elt)) )

              (t
                (macro-error "Invalid optional parameter: ~S" elt) ))

            (when (and svar (not (symbolp svar)))
              (macro-error "Supplied-p parameter ~S must be a symbol." elt) ) )

          (pop scan)
          (push elt optionals) ))

      (check-env)

      ;; Dotted lambda-list
      (when (and body-p (not (null scan)) (symbolp scan))
        (push '&rest rests)
        (push scan rests)
        (setq scan nil)
        (setq elt  nil) )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; &rest
      ;;
      (when (or (eq '&rest elt) (and body-p (eq '&body elt)))
        (push elt rests)
        (pop scan)

        (block nil
          (unless (consp scan) (setq elt nil) (return))

          (setq elt (car scan))

          (when (member elt lambda-list-keywords :test #'eq) (return))

          (push elt rests)

          (pop scan)
          (unless (consp scan) (setq elt nil) (return))
          (setq elt (car scan)) )

        (when (null (rest rests))
          (macro-error "Missing parameter name after ~S." '&rest) ))

      (check-env)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; &key
      ;;
      (when (eq '&key elt)
        (push elt keywords)
        (pop scan)

        (let ((keynames '()))
          (loop
            (unless (consp scan) (setq elt nil) (return))
            (setq elt (car scan))

            (when (eq '&allow-other-keys elt)
              (setf (car (last keywords)) '&allow-other-keys)
              (pop scan)
              (unless (consp scan) (setq elt nil) (return))
              (setq elt (car scan))
              (return) )

            (when (member elt lambda-list-keywords :test #'eq) (return))

            (pop scan)

            (let (keyname var initform svar initform-p)
              (cond
                ((symbolp elt)
                  (setq var elt) )

                ((<= 1 (si::safe-list-length elt) 3)
                 (setq var        (first  elt))
                 (setq initform   (second elt))
                 (setq svar       (third  elt))
                 (setq initform-p (>= (length elt) 2)) )

                (t
                  (macro-error "Invalid keyword parameter: ~S" elt) ))

              (cond
                ((symbolp var)
                  (setq keyname (intern (symbol-name var)
                                        #.(symbol-package :key) )) )

                ((= 2 (si::safe-list-length var))
                  (setq keyname (first  var))
                  (setq var     (second var)) )

                (t
                  (macro-error "Invalid keyword parameter: ~S" elt) ))

              (when (member keyname keynames :test #'eq)
                (style-warn
                  "Keyword ~S is appeared more than once." keyname ))

              (push keyname keynames)

              (when (and svar (not (symbolp svar)))
                (macro-error
                  "Supplied-p parameter ~S must be a symbol." elt ))

              (push (list keyname var svar initform initform-p) keywords) )) ))

      (check-env)

      ;; &aux
      (when (eq '&aux elt)
        (push elt auxiliaries)
        (pop scan)

        (loop
          (unless (consp scan) (setq elt nil) (return))
          (setq elt (car scan))

          (when (member elt lambda-list-keywords :test #'eq) (return))

          (cond
            ((symbolp elt)
              (setq elt (list elt)) )
            ((and (<= 1 (si::safe-list-length elt) 2)
                  (symbolp (first elt)) ))
            (t
              (macro-error
                "Invalid auxilially parameter: ~S" elt ) ))

          (pop scan)
          (push elt auxiliaries) ))

      (check-env)

      ;; result of parsing lambda list
      (values scan
             (nreverse requireds)
             (nreverse optionals)
             (nreverse rests)
             (nreverse keywords)
             (nreverse auxiliaries)
             env ) ) ) )


;;;; constant-value-p
;;;   => constant-p value
;;;
;;; Called by:
;;;   assoc
;;;   constantp
;;;   dotimes
;;;   eql
;;;   make-array
;;;   member
;
(defun constant-value-p (form env)
    (declare (values t t))
  (cond
    ((null form) (values t nil))
    ((keywordp form) (values t form))
    ((symbolp form)
      (multiple-value-bind (kind local-p alist)
          (variable-information form env)
          (declare (ignore local-p))
      (if (eq  kind :constant)
          (values t (cdr (assoc :constant alist)))
        (values nil nil) ) ) )
    ((consp form)
      (if (eq 'quote (car form))
          (values t (cadr form))
        (values nil nil) ) )
    (t (values t form)) ) )


;;;; check-syntax
(defun check-syntax (option min max syntax)
  (let ((len (si::safe-list-length option)))
    (if (<= min len max)
        len
      (syntax-error syntax option) ) ) )


;;;; Compiler Eval
;;;
;;; Syntax:
;;;     compiler-eval form env => result*
;;;
;;; Called by:
;;;     parse-macro
;;;
;;; BUGBUG: Stab
;
(defun compiler-eval (form env)
    (declare (ignore env))
  (eval form) )


;;;; Error Lambda List Mismatched
;;;
;;; Description:
;;;  Reports destructuring mismatching. This is default value of fn-mismatched
;;;  parameter of emit-destuct-bind
;;;
;
(defun error-lambda-list-mismatched (lambda-list source scan)
  (error "Lambda mismatched ~S: ~S ~S" lambda-list scan source) )


;;;; Emit Ignorable Form
;;;
;;; Returns
;;;  (let ((#:ignorable ,form))
;;;    (declare (ignorable #:ignorable))
;;;    #:ignorable )
;
(defun make-ignorable-form (form)
  (let ((var-ignorable '#:|ignorable|))
    `(let ((,var-ignorable ,form))
       ,var-ignorable ) ) )


(defun param-info-keyword    (param) (first  param))
(defun param-info-name       (param) (second param))
(defun param-info-supplied   (param) (third  param))
(defun param-info-initform   (param) (fourth param))
(defun param-info-initform-p (param) (fifth  param))


;;;; make-keyword-arguments-parser
;;; Syntax:
;;;   make-keyword-arguments-parser var-rest keys allow-other-keys-p
;;;     => binding* decl*
;;;
;;; Called by:
;;;     defsetf
;
(defun make-keyword-arguments-parser (var-rest keys allow-other-keys-p
                                      &aux tmps )
    (declare (type symbol var-rest))
    (declare (type list param))
    (declare (values list list))
  (labels (
    ;; collect-keywords
    (collect-keywords ()
        (declare (values list))
      (let* ((head (list 0))
             (tail head) )
        (dolist (param keys (rest head))
          (setq tail (setf (rest tail) (list (first param)))) ) ) )

    ;; make-check-parser
    (make-check-parser ()
        (declare (values form))
     `(let ((allow-other-keys nil))
         ;; Scan for :allow-other-keys
        (let ((runner ,var-rest) key val)
          (loop
            (unless (consp runner) (return))
            (setq key (pop runner))

            (unless (consp runner) (return))
            (setq val (pop runner))

            (when (eq key :allow-other-keys)
              (setq allow-other-keys val)
              (return) )) )

        (let ((runner ,var-rest) key)
          (loop
            (unless (consp runner)
              (unless allow-other-keys
                (when runner
                  ,(make-rest-error 'dotted-rest-argument) ))
              (return) )

            (setq key (pop runner))

            (unless (consp runner)
              (unless allow-other-keys
                (if runner
                    ,(make-rest-error 'dotted-rest-argument)
                  ,(make-rest-error 'odd-number-of-keyword-arguments) ))
              (return) )

            (setq val (pop runner))

            (cond
              ,.(mapcar #'make-test-clause tmps)
              ((not allow-other-keys)
                (si::unrecognized-keyword-argument
                  key
                  ',(collect-keywords) ) ))) ) ) )

    ;; make-non-checked-parser-form
    (make-non-check-parser ()
        (declare (values form))
        (assert keys)
     `(let ((runner ,var-rest) key val)
        (loop
          (unless (consp runner) (return))
          (setq key (pop runner))

          (unless (consp runner) (return))
          (setq val (pop runner))

          (cond ,@(mapcar #'make-test-clause tmps)) ) ) )

    ;; make-rest-error
    (make-rest-error (symbol)
        (declare (type symbol symbol))
        (declare (values form))
      `(error ',symbol :arguments ,var-rest) )

    ;; make-test-clause
    (make-test-clause (frob)
        (declare (type list frob))
        (declare (values list))
      (let ((key  (first frob))
            (ktmp (second frob))
            (stmp (third  frob)) )
        `((eq key ',key)
            (unless ,stmp (setq ,ktmp val) (setq ,stmp t)) ) ) )
    )
    ;;
    ;; make-keyword-arguments-parser
    ;;
    (let ((pre-binding*  '())
          (post-binding* '()) )

      (dolist (param keys)
          (declare (type param-info param))

        (let* ((key  (param-info-keyword  param))
               (kvar (param-info-name     param))
               (svar (param-info-supplied param))
               (ktmp (make-symbol (symbol-name kvar)))
               (stmp (make-symbol (format nil "~A-P" (symbol-name kvar)))) )

        (unless (symbolp key)
          (error 'invalid-keyword :keyword key) )

        (unless (symbolp kvar)
          (error 'invalid-variable :name kvar) )

        (unless (symbolp svar)
          (error 'invalid-variable :name svar) )

        (push ktmp pre-binding*)
        (push stmp pre-binding*)

        (push (list key ktmp stmp) tmps)

        (push `(,kvar (if ,stmp ,ktmp ,(param-info-initform param)))
              post-binding* )

        (when svar
          (push `(,svar ,stmp) post-binding*)) ) )

      (setq tmps (nreverse tmps))
      (setq pre-binding*  (nreverse pre-binding*))
      (setq post-binding* (nreverse post-binding*))

      (multiple-value-bind (dummy parser)
          (cond
              ((not allow-other-keys-p)
                (values (gensym) (make-check-parser)) )
              ((null keys)
                (values nil nil) )
              (t
                (values (gensym) (make-non-check-parser)) ))
        (if (null dummy)
            (values nil nil)
          (values `(,.pre-binding* (,dummy ,parser) ,.post-binding*)
                  `((declare (ignore ,dummy))) )) ) ) ) )


;;;; Make temporary variables for Macro
;;;
;;; Called by:
;;;     expander made by defsetf
;
(defun make-vars (form*)
  (mapcar #'(lambda (x) (declare (ignore x)) (gensym "val_")) form*) )


;;;; Parse Destructuring Bind
;;;
;;; Arguments and Values:
;;;     program     - A form which does destructuring.
;;;     var-env     - A symbol followed by &environment or nil.
;;;     names       - A list of variables.
;;;
;;; Called by:
;;;     define-compiler-macro
;;;     define-setf-expander
;;;     defmacro
;
#|
(a (&whole b b-1 b-2) &optional (c (init-c) c-p) d &key (e (init-e) e-p) f)

scanner cur src pat
  (check cur src pat)
  (pop cur)

check cur src pat
  (unless (consp cur)
    (error-mismatch pat src) )


(let* ((a (scanner scan))
       (#:src-b (scanner scan))
       (#:pat-b '(b-1 b-2))
       (#:cur-b #:src-b)
       (b (scanner #:cur-b #:pat-b  #:src-b))
       (b-1 (scanner #:cur-b #:pat-b  #:src-b))
       (#:c-p scan)
       (c (if #:c-p (scanner scan) (init-c)))
       (c-p #:c-p)
       (#:scan scan)
       (#:suppress (check-allow-other-keys #:scan nil))
       #:e #:e-p #:f #:f-p
       (#:dummy 
          (while #:scan
            (let ((#:key (keyscanner #:scan)) #:val)
               (unless #:scan (return))
                 (setq #:val (keyscanner #:scan #:suppress))
               (case #:key
                 (e (unless #:e-p (setq #:e val #:e-p t)))
                 (otherwise (unless #:suppress (ERROR))) ) ) ) )
       (e (if #:e-p #:e (init-e)))
       (e-p #:e-p)
       (f (if #:f-p #:f nil)) )
     ... declare ...
     ... form ... )
|#
(defun parse-destructuring-bind
    (lambda-list whole src body env-p
     &optional (fn-mismatched   'si::destructuring-mismatch)
               (initform-default nil) )
    (declare (values form symbol))
    (declare (type list lambda-list))
    (declare (type symbol whole))
    (declare (type list body))
  (let ((names            '())
        (binding*         '())
        (decl*            '())
        (fn-check-not-end '#:|check-not-end|)
        (fn-check-end     '#:|check-end|) )

    (labels (
      ;; add-name
      ;;    collects names for checking duplicated names.
      (add-name (name)
        (when (or (null name) (not (symbolp name)))
          (macro-error
            "Parameter name must be a symbol instead of ~S."
            name ) )

        (when (member name lambda-list-keywords :test #'eq)
          (style-warn
            "We recommend not to use lambda-list-keyword ~S as parameter."
            name ) )

        (when (member name names :test #'eq)
          (style-warn
            "Parameter ~S is appeared more than once."
            name ) )

        (push name names)
        name )

      ;;; emit-scanner
      (emit-scanner (var-cur var-src var-pat)
        `(progn
            (,fn-check-not-end ,var-cur ,var-src ,var-pat)
            (pop ,var-cur) ) )

      ;;; process auxiliary parameters
      (process-&aux (auxiliaries)
        (dolist (spec (rest auxiliaries))
          (if (symbolp spec)
              (push (add-name spec) binding*)
            (progn
              (add-name (first spec))
              (push spec binding*) )) ) )

      ;; process keywords parameters
      ;; BUGBUG: We should separate strict parser and loose parser.
      ;; BUGBUG: Strict parser must check invalid keyword.
      (process-&key (specs var-cur)
        (when specs
          (let ((var-suppress (gensym "suppress"))
                (var-key      (gensym "key"))
                (var-val      (gensym "val"))
                (var-scan     (gensym "scan"))
                (var-process  (gensym "process"))
                (suppress-p   (if (eq '&allow-other-keys (first specs)) t nil))
                (var.svar*    '()) )

            (push `(,var-suppress
                    (si::check-allow-other-keys ,var-cur ,suppress-p) )
                  binding* )

            (pop specs)

            (dolist (spec specs)
              (let ((var  (gensym "kvar"))
                    (svar (gensym "svar")) )
                (push var  binding*)
                (push svar binding*)
                (push (cons var svar) var.svar*) ) )

            (setq var.svar* (nreverse var.svar*))

            (push `(,var-scan ,var-cur) binding*)

            (push `(,var-process
                (loop
                  (unless (consp ,var-scan) (return))
                  (let ((,var-key (pop ,var-scan)) ,var-val)
                    (unless (consp ,var-scan) (return))
                    (setq ,var-val (pop ,var-scan))
                    (cond ,.(mapcar #'(lambda (pattern var.svar)
                      `((eq ,(first pattern) ,var-key)
                         (unless ,(cdr var.svar)
                           (setq ,(car var.svar) ,var-val
                                  ,(cdr var.svar) t) ) ) )
                       specs var.svar* )
                     (t
                       (unless ,var-suppress
                         (si::unrecognized-keyword-argument
                            ,var-key
                            ',(mapcar #'first specs) )) )) )))
                  binding* )
            (push `(declare (ignorable ,var-process)) decl*)

            (dolist (spec specs)
              (let ((var.svar (pop var.svar*)))
                (process-var-or-pat (second spec)
                             `(if ,(cdr var.svar)
                                  ,(car var.svar)
                                ,(if (fifth spec)
                                     (fourth spec)
                                   initform-default ))
                             '&key )

                (when (third spec)
                  (add-name (third spec))
                  (push `(,(third spec) ,(cdr var.svar))
                        binding* )) ) ) )) )

      ;; process optional parameters
      (process-&optional (specs var-cur var-src var-pat)
        (dolist (spec (rest specs))
          (multiple-value-bind (var initform svar)
              (cond
                ((symbolp spec)
                  (values spec initform-default nil) )
                ((consp spec)
                  (values (first spec)
                          (if (rest spec)
                              (second spec)
                            initform-default )
                          (third spec) ) )
                (t
                  (macro-error "Bad optional parameter: ~S" spec) ))

            (let ((var-svar nil))
              (when svar
                (setq var-svar (gensym "svar"))
                (push `(,var-svar (not (null ,var-cur))) binding*) )

              (process-var-or-pat var
                          `(if (null ,var-cur)
                                ,initform
                             ,(emit-scanner var-cur var-src var-pat) )
                          '&optional )
              (when svar
                (add-name svar)
                (push `(,svar ,var-svar) binding*) ) ) ) ) )

      ;; process required parameters
      (process-required (vars var-cur var-src var-pat)
        (dolist (var vars)
          (process-var-or-pat var
                       (emit-scanner var-cur var-src var-pat)
                       '&required ) ) )

      ;; process rest parameters
      (process-&rest (vars var-cur)
        (when vars
          (process-var-or-pat (second vars) var-cur '&rest) ) )

      ;; process-var-or-pat
      (process-var-or-pat (var src-form context)
          (declare (ignore context))
        (cond
          ((symbolp var)
           (add-name var)
           (push `(,var ,src-form) binding*) )

          ((consp var)
            (process-pat var src-form src-form nil) )

          (t
            (macro-error "Expect variable or pattern instead of ~S." var) )) )

      ;; process-pat
      (process-pat (pattern whole src env-p)
        (let ((scan pattern)
              (var-src (gensym "src"))
              (var-cur (gensym "cur"))
              (var-pat (gensym "pat")) )

          (push `(,var-src ,src)      binding*)
          (push `(,var-cur ,var-src)  binding*)
          (push `(,var-pat ',pattern) binding*)
          (push `(declare (ignorable ,var-pat)) decl*)

          (when (and (consp scan) (eq '&whole (first scan)))
            (pop scan)
            (unless (consp scan)
              (macro-error
                  "~S must be appeared before any other parameter or lambda list keywords."
                  '&whole ))
            (push `(,(add-name (pop scan)) ,(if (eq src whole) var-src whole))
                  binding* ))

          (multiple-value-bind (extra
                                requireds optionals rests keywords
                                auxiliaries
                                var-env )
              (analyze-lambda-list scan t env-p)

           (when extra
              (macro-error "Can't use lambda-list-keyword ~S after ~S."
                (first extra)
                (ldiff pattern extra) ))

           (process-required  requireds var-cur var-src var-pat)
           (process-&optional optionals var-cur var-src var-pat)
           (process-&rest     rests     var-cur)
           (process-&key      keywords  var-cur)
           (process-&aux      auxiliaries)

           (when (and (null rests) (null keywords))
             (let ((var-end (gensym "end")))
               (push `(,var-end (,fn-check-end ,var-cur ,var-src ,var-pat))
                     binding* )
               (push `(declare (ignorable ,var-end)) decl*) ))
           var-env ) ) )
       )
       ;;
       (let (var-env program)
         (setq var-env (process-pat lambda-list whole src env-p))
         (setq program
           `(labels (
              (,fn-check-end (cur src pat)
                (when cur
                  (,fn-mismatched cur src pat) ))

              (,fn-check-not-end (cur src pat)
                (unless (consp cur)
                  (,fn-mismatched cur src pat) ))
              )
              (declare (ignorable #',fn-check-end))
              (declare (ignorable #',fn-check-not-end))
              ;;
              (let* ,(nreverse binding*)
                ,@(nreverse decl*)
                ,@body ) ))
           (values program var-env) ) ) ) )


;;;; Parse Ordinary Lambda List
;;;
;;; Returns:
;;;    requireds        List of element.
;;;    optionals        List of element followed by &optional.
;;;    rests            List of element followed by &rest.
;;;    keywords         List of element followed by &key or &allow-other-keys.
;;;    auxiliaries      List of element followed by &aux.
;;;
;;; Steps:
;;;  1 Check parse error
;;;  2 Check name duplication
;;;  3 Check &optional specifiers
;;;  4 Check &key specifiers
;;;
;
(defun parse-lambda-list (list)
  (let ((vars '()))
  (labels (
    ;; check-var
    ;;   check parameter name.
    (check-var (var)
      (unless (symbolp var)
        (macro-error "Parameter ~S must be a symbol." var) )
      (when (member var lambda-list-keywords :test #'eq)
        (style-warn
            "We recommend not to use lambda-list-keyowrd ~S as parameter." var ))
      (when (member var vars :test #'eq)
        (style-warn "Parameter ~S is appeared more than once." var) )
      (when (eq :constant (c::variable-information var nil))
        (macro-error
            "Can't use constant ~S as parameter." var ))
      (push var vars) )
    )
    ;;
    (multiple-value-bind (scan requireds optionals rests keywords auxiliaries)
       (analyze-lambda-list list)

      ;; Check parse error
      (cond
        ((null scan))
        ((consp scan)
          (macro-error "Can't use lambda-list-keyword ~S." (car scan)) )
        (t
          (macro-error "Dotted-lambda-list isn't allowed: ~S." list) ))

      ;; required parameters
      (dolist (var requireds)
        (check-var var) )

      ;; optional parameters
      (dolist (spec (rest optionals))
        (cond
          ((symbolp spec)                  ; var
            (check-var spec) )
          ((= 1 (si::safe-list-length spec))   ; (var)
            (check-var (first spec)) )
          ((= 2 (si::safe-list-length spec))   ; (var initform)
            (check-var (first spec)) )
          ((= 3 (si::safe-list-length spec))   ; (var initform svar)
            (check-var (first spec))
            (check-var (third spec)) )
          (t
            (macro-error
              "Optional parameter ~S must be symbol or (symbol [initform [supplied-p])."
              spec ) )) )

          ;; rest parameter
          (dolist (var (rest rests))
            (check-var var) )

          ;; keyword parameters
          (dolist (spec (rest keywords))
            (check-var (second spec)) ; key var svar initform initform-p
            (when (third spec)
              (check-var (third spec)) ) )

          ;; auxiliary parameters
          (dolist (spec (rest auxiliaries))
            (cond
              ((symbolp spec)                     ; var
                (check-var spec) )
              ((= 1 (si::safe-list-length spec))    ; (var initform)
                (check-var (first spec)) )
              ((= 2 (si::safe-list-length spec))    ; (var initform)
                (check-var (first spec)) )
              (t
                (macro-error
                  "Auxiliary parameter ~S must be symbol or (symbol [initform])."
                  spec ) )) )

          ;; return values
          (values requireds
                  optionals
                  rests
                  keywords
                  auxiliaries ) ) ) ) )


;;;; Returns Macro Parser Lambda Expression
;;;
;;; Returns:
;;;     lambda-expr
;;;     doc-string
;;;
;;; Called by:
;;;     defmacro
;;;     parse-macro
;
(defun parse-macro-aux (name lambda-list body)
  (let ((var-form        '#:|form|)
        (var-env         '#:|env|)
        (fn-syntax-error '#:|syntax-error|) )
    (multiple-value-bind (decl* form* doc-string) (analyze-body body t)
    (multiple-value-bind (program var-env-user)
        (parse-destructuring-bind lambda-list
                                  var-form
                                  `(rest ,var-form)
                                  (append decl* form*)
                                  t
                                  fn-syntax-error )
      (values
        `#'(lambda (,var-form ,(or var-env-user var-env))
            (declare (ext:lambda-name (macro-function ,name)))
            ,@(unless var-env-user `((declare (ignore ,var-env))))
            ,@(when doc-string (list doc-string))
            (labels (
                (,fn-syntax-error (cur src pat)
                    (syntax-error '(,name ,@lambda-list)
                                  ,var-form
                                  cur
                                  src
                                  pat ) ) )
                (declare (ignorable #',fn-syntax-error))
                (block ,name ,program) ) )
        doc-string ) ) ) ) )


;;;; Parse Macro
;;;
;;; Called by:
;;;     macrolet
;;;
;;; Note: Defined in CLtL2
;
(defun parse-macro (name lambda-list body &optional env)
  (compiler-eval (parse-macro-aux name lambda-list body) env) )


;;;; Singal Program Error
;;;
;;; BUGBUG: Using program-error as function isn't conform.
;
(defun macro-error (ctrl-string &rest args)
  (error 'simple-program-error
         :format-control ctrl-string
         :format-arguments args ) )


(defun compiler-type-error (expected-type datum ctrl-string &rest args)
  (error 'si::simple-type-error
         :expected-type     expected-type
         :datum             datum
         :format-control    ctrl-string
         :format-arguments  args ) )


;;;; style-warn
;
(defun style-warn (ctrl-string &rest args)
    (declare (values null))
  (warn 'simple-style-warning
         :format-control ctrl-string
         :format-arguments args ) )


;;;; Syntax Error
;;;
;;; Called by:
;;;     define-compiler-macro
;;;     defmacro
;
(defun syntax-error (syntax form &optional cur src pat)
    (declare (ignore cur src pat))
  (error 'syntax-error :syntax syntax :form form) )


;;;; type-and
;
(defun type-and (type1 type2 &optional env)
  (let ((surep t))
    (block nil
      (multiple-value-bind (subtypep validp) (safe-subtypep type1 type2 env)
        (when subtypep (return type1))
        (unless validp (setq surep nil)) )

      (multiple-value-bind (subtypep validp) (safe-subtypep type2 type1 env)
        (when subtypep (return type2))
        (unless validp (setq surep nil)) )

      (unless surep `(and ,type1 ,type2)) ) ) )


;;;; undefined-type
;
(defun undefined-type (name env)
  (warn 'undefined-type-so-far :name name)
  (si::intern-class name env) )
