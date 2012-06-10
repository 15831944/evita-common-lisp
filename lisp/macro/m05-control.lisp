;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 5 Data and Control Flow
;;; macro/m05-control.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m05-control.lisp#4 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     and                     5.3.41
;;;     case                    5.3.46
;;;     ccase                   5.3.46
;;;     cond                    5.3.42
;;;     ctypecase               5.3.47
;;;     defconstant             5.3.15
;;;     define-modify-macro     5.3.60
;;;     define-setf-expander    5.3.62
;;;     defparameter            5.3.16
;;;     defsetf                 5.3.61
;;;     defun                   5.3.2
;;;     defvar                  5.3.16
;;;     destructuring-bind      5.3.17
;;;     ecase                   5.3.46
;;;     etypecase               5.3.47
;;;     multiple-value-bind     5.3.48  parser
;;;     multiple-value-list     5.3.50
;;;     multiple-value-setq     5.3.52  parser
;;;     nth-value               5.3.56
;;;     or                      5.3.44
;;;     prog                    5.3.57
;;;     prog*                   5.3.57
;;;     prog1                   5.3.58
;;;     prog2                   5.3.58
;;;     psetf                   5.3.64
;;;     psetq                   5.3.21
;;;     return                  5.3.26
;;;     rotatef                 5.3.66
;;;     setf                    5.3.64
;;;     shiftf                  5.3.65
;;;     typecase                5.3.47
;;;     unless                  5.3.45
;;;     when                    5.3.45
;
(in-package :xc)

;;;; 5.3.41 and
;;;
;;; Syntax:
;;;     and {form}* => {result}*
;;;
;;; Expansion:
;;;     (and)           => t
;;;     (and e1)        => e1
;;;     (and e1 e2 ...) => (if e1 (and e2 ..) nil)
;;;     (and t e1 ...) => (and e1 ...)
;;;     (and nil e1 ...) => nil
;
(defmacro cl:and (&rest form*)
  (cond
   ((null form*) t)
   ((null (cdr form*)) (car form*))
   (t `(if ,(car form*) (and ,@(cdr form*)))) ) )


;;;; 5.3.46 case
;;; Example:
;;;     (case key
;;;       ((x1 x2 x3) action_1)         ; (memv key '(x1 x2 x3))
;;;       (y1 action_2)                 ; not standard(test by eql?)
;;;       ...
;;;       (otherwise action_n-1) )
;;;       =>
;;;     (let ((#:CASEk key))
;;;        (cond
;;;         ((member #:CASEk '(x1 x2 x3)) action_1)
;;;         ((eql #:CASEk y) action_2)
;;;          ...
;;;         (t action...) ) )
;;;
;;;; 5.3.46 ccase
;;;; 5.3.47 ctypecase
;;;; 5.3.46 ecase
;;;; 5.3.47 etypecase
;;;; 5.3.47 typecase
;;; Expansion:
;;;  (let ((#:keyform keyform))
;;;    (cond
;;;     ((typep #:keyform 'type) ...)
;;;     ...
;;;     (t ...) ) )
;;;
;;; BUGBUG: NYI: check type name
;;; BUGBUG: NYI: check predecessor clause doesn't hide successors.
;;;
;
(labels (
  ;; collect-case-keys
  ;;  Collects keys in clauses for using error message.
  (collect-case-keys (caller clause*)
    (let ((keys '()))
      (dolist (clause clause* (nreverse keys))
        (when (consp clause)
          (let ((key (car clause)))
            (if (listp key)
                (dolist (key key) (push key keys))
              (progn
                (when (or (eq 't key) (eq 'otherwise key))
                  (style-warn "Should write ~S to (~S) in ~S." key caller) )
                (pushnew key keys) )) )) ) ) )

  ;; collect-typcase-types
  ;;  Collects types in clauses for using error message.
  (collect-typecase-types (caller clause*)
    (let ((types '()))
      (dolist (clause clause* (nreverse types))
        (when (consp clause)
          (let ((type (first clause)))
            (when (or (eq 't type) (eq 'otherwise type))
              (style-warn "You should not use type ~S in ~S." type caller) )
            (pushnew type types :test #'equal) )) ) ) )

  ;; expand-case
  (expand-case (test var keyform clause*)
    (let ((keys '())
          (cond-clauses '()) )
    (labels (
      ;; appeared-p
      (appeared-p (key)
        (if (eq test 'eql)
            (member key keys)
          (member key keys :test 'equal) ) )

      ;; emit-cond-clause
      (emit-cond-clause (key form*)
        (let ((test-form (make-test-form key)))
          (if test-form
              (push `(,test-form ,@form*) cond-clauses)
            (style-warn "Ignore: ~S" form*) ) ) )

      ;; make-test-form
      (make-test-form (key)
        (if (appeared-p key)
            (progn
              (style-warn "Case ~S is appeared more than once." key)
              nil )
          (progn
            (push key keys)
            `(,test ,var ',key) )) )
    )
    ;;
    (loop
      (when (null clause*)
        (setq cond-clauses (nreverse cond-clauses))
        (return `(let ((,var ,keyform)) (cond ,.cond-clauses))) )

      (multiple-value-bind (key form*)
          (let ((clause (pop clause*)))
            (unless (plusp (si::safe-list-length clause))
              (error 'invalid-clause
                   :operator 'case
                   :clause   clause ))
            (values (first clause)
                    (or (rest clause) (list (make-ignorable-form nil))) ) )
        (cond
          ((null key)
            (style-warn "No keys, do you mean (nil)?") )
          ((or (eq 't key) (eq 'otherwise key))
             (if (null clause*)
                 (push `(t ,@form*) cond-clauses)
               (progn
                 (style-warn "Otherwise clause should be the last clause.")
                 (emit-cond-clause key form*) )) )
          ((not (consp key))
            (emit-cond-clause key form*) )
          ((null (rest key))
            (emit-cond-clause (first key) form*) )
          ((eq test 'typep)
            (emit-cond-clause key form*) )
          (t
            (let ((or-forms '()))
              (dolist (key key)
                (let ((test-form (make-test-form key)))
                  (when test-form (push test-form or-forms)) ) )
              (setq or-forms (nreverse or-forms))
              (if (null or-forms)
                  (style-warn "Ignore: ~S" form*)
                (push `((or ,.or-forms) ,@form*) cond-clauses) ) ) )) )) ) ) )

  ;; expand-ccase
  ;;    ccase           ::= CCASE or TYPECASE
  ;;    case            ::= CASE or TYPECASE
  ;;    keyplace        ::= keyplace of CCASE or TYPECASE form.
  ;;    clause*         ::= clause* of CCASE or TYPECASE form.
  ;;    keys            ::= list of keys or types.
  ;;    expected-types  ::= (member ...) or (or ...)
  ;;
  (expand-ccase (ccase case keyplace clause* keys expected-type)
    (let ((var-value  '#:|value|)
          (blk-ccase  (gensym (symbol-name ccase)))
          (tag-loop   (gensym (symbol-name ccase))) )
    `(block ,blk-ccase
       (tagbody
         ,tag-loop
           (return-from ,blk-ccase
             (,case ,keyplace
               ,@clause*
               (otherwise
                 (restart-case
                     (error (make-condition 'si::case-failure
                                            :name          ',ccase
                                            :datum         ,keyplace
                                            :expected-type ',expected-type
                                            :possibilities ',keys ))
                   (store-value (,var-value)
                     :report
                       (lambda (stream)
                           (declare (ext:lambda-name
                                      (,ccase stroe-value :report) ))
                         (format stream "Supply a new value of ~S."
                                 ',keyplace ) )
                     (setf ,keyplace ,var-value)
                     (go ,tag-loop) )) ))))) ) )
  )
  ;;

  ;; case
  (defmacro cl:case (keyform &rest clause*)
    (let ((var (gensym "case")))
      (expand-case 'eql var keyform clause*) ) )

  ;; ccase
  (defmacro cl:ccase (keyplace &rest clause*)
    (let ((keys (collect-case-keys 'ccase clause*)))
      (expand-ccase 'ccase 'case keyplace clause*
        keys `(member ,@keys) ) ) )

  ;; ctypecase
  (defmacro cl:ctypecase (keyplace &rest clause*)
    (let ((types (collect-typecase-types 'ctypecase clause*)))
      (expand-ccase 'ctypecase 'typecase keyplace clause*
        types `(or ,@types) ) ) )

  ;; ecase
  (defmacro cl:ecase (keyform &rest clause*)
    (let ((var (gensym "ecase"))
          (keys (collect-case-keys 'ecase clause*)) )
      (setq clause* (append clause*
          `((otherwise
               (error 'si::case-failure
                      :name 'ecase
                      :datum ,var
                      :expected-type '(member ,@keys)
                      :possibilities ',keys )))))
      (expand-case 'eql var keyform clause*) ) )

  ;; etypecase
  (defmacro cl:etypecase (keyform &rest clause*)
    (let ((var (gensym "ecase"))
          (types (collect-typecase-types 'etypecase clause*)) )
      (setq clause* (append clause*
          `((otherwise
               (error 'si::case-failure
                      :name 'etypecase
                      :datum ,var
                      :expected-type '(or ,@types)
                      :possibilities ',types )))))
      (expand-case 'typep var keyform (append clause*)) ) )

  ;; typecase
  (defmacro cl:typecase (keyform &rest clause*)
    (let ((var (gensym "typecase")))
      (expand-case 'typep var keyform clause*) ) )
 ) ; labels


;;;; 5.3.42 cond
;;;
;;; Syntax:
;;;     cond {clause}* => {result}*
;;;     clause ::= (test-form {form*})
;;;
;;; Expansion:
;;;     (cond)  => nil
;;;     (cond (test forms) clause...) =>
;;;         `(if ,test (progn ,@forms) (cond ,@clauses...))
;;;     (cond (test) clause...) =>
;;;         `(let ((#:cond ,test)) (if #:cond #:cond (cond ,@clauses...)))
;
(defmacro cl:cond (&rest clauses)
  (if (null clauses)
      (make-ignorable-form nil)
    (let ((clause (car clauses)))
      (unless (plusp (si::safe-list-length clause))
        (error 'invalid-clause
               :operator 'cond
               :clause   clause ) )

     (setq clauses (cdr clauses))

     (let ((test  (car clause))
           (forms (cdr clause)) )
      (cond
        ((and (eq 't test) (null clauses))
          `(progn ,@forms) )

       (forms
         `(if ,test (progn ,@forms) (cond ,@clauses)) )

       (t
         (let ((var '#:|cond|))
           `(let ((,var ,test))
              (if ,var
                  ,var
                (cond ,@clauses) ) ) )) ) )) ) )


;;;; 5.3.15 defconstant
;;;
;;; Syntax:
;;;     defconstant name initial-value [documentation]
;
(defmacro cl:defconstant (name init-form &optional  doc-string)
  `(progn
     (eval-when (:compile-toplevel)
       (%defconstant ',name ,init-form) )
     (si::%defconstant ',name ,init-form ,doc-string) ) )


;;;; 5.3.60 define-modify-macro
;;;
;;; Syntax:
;;;     define-modify-macro name lambda-list function [documentation] => name
;;;
;;; Expansion:
;;;   (defmacro name (reference . lambda-list)
;;;       documentation
;;;       `(setf ,reference (function ,reference ,arg1 ,arg2 ...)) )
;
(defmacro cl:define-modify-macro (name lambda-list function
                                  &optional doc-string )
  (multiple-value-bind (scan requireds optionals rests keywords auxiliaries)
      (analyze-lambda-list lambda-list)

    ;; Check parse error
    (cond
      ((null scan))
      ((consp scan)
        (macro-error "Can't use ~S here." (car scan) ) )
      (t
        (macro-error "Malformed lambda-list: ~S" lambda-list) ))

    ;; Check &key and &aux parameters
    (cond
      (keywords    (macro-error "Can't use ~S here." '&key))
      (auxiliaries (macro-error "Can't use ~S here." '&aux)) )

    ;; Build expansion
    (let ((var-ref (gensym "ref_"))
          (var-env (gensym "env_"))
          (args    '()) )

      ;; collect arguments
      (dolist (arg requireds)
        (push arg args) )

      (dolist (arg (rest optionals))
        (if (consp arg)
            (push (first arg) args)
          (push arg args) ) )

      (dolist (arg (rest rests))
        (push arg args) )

      (setq args (nreverse args))

      `(defmacro ,name (,var-ref ,@lambda-list &environment ,var-env)
        ,@(when doc-string (list doc-string))
        (multiple-value-bind (vars vals stores store-form access-form)
            (get-setf-expansion ,var-ref ,var-env)
          `(let* (,.(mapcar #'list vars vals)
                  (,(first stores)
                   ,,(if rests
                       `(list* ',function ,'access-form ,@args)
                       `(list  ',function ,'access-form ,@args) ) ) )
             ,store-form ) )) ) ) )


;;;; 5.3.62 define-setf-expander
;;;
;;; Syntax:
;;;     define-setf-expander access-fn lambda-list
;;;       [[{declaration}* | documentation]] {form}* => access-fn
;;;
;;; Expansion:
;;;  (%define-setf-expander access-fn 'lambda-list expander)
;;;
;
(defmacro cl:define-setf-expander (access-fn lambda-list &body body)
  (let ((var-form        '#:|form|)
        (temp-env        '#:|env|)
        (fn-syntax-error '#:|syntax-error|)
        decl* form* doc-string
        program var-env expander)
    (multiple-value-setq (decl* form* doc-string) (analyze-body body t))
    (multiple-value-setq (program var-env)
        (parse-destructuring-bind lambda-list
                                  var-form
                                  `(rest ,var-form)
                                  (append decl* form*)
                                  t
                                  fn-syntax-error ))
    (setq expander
      `#'(lambda (,var-form ,(or var-env temp-env))
               (declare (ext:lambda-name (:setf-expander ,access-fn)))
           ,.(unless var-env `((declare (ignore ,temp-env))))
           ,.(when doc-string (list doc-string))
           (labels (
             (,fn-syntax-error (cur src pat)
               (syntax-error '(,access-fn ,lambda-list)
                             ,var-form
                             cur
                             src
                             pat ) )
             )
             (block ,access-fn ,program) ) ))
    `(progn
       (eval-when (:compile-toplevel)
         (%define-setf-expander ',access-fn ',lambda-list ,expander) )
       (si::%define-setf-expander ',access-fn ',lambda-list ,expander) ) ) )


;;;; 5.3.16 defparameter
;;;
;;; Syntax:
;;;     defparameter name [initial-value [documentation]]
;
(defmacro cl:defparameter (name init-form &optional  doc-string)
  `(progn
     (eval-when (:compile-toplevel)
       (%defparameter ',name ',init-form) )
     (si::%defparameter ',name ,init-form ,doc-string) ) )


;;;; 5.3.61 defsetf
;;;
;;; Syntax: The "short form":
;;;     defsetf access-fn update-fn [documentation] => access-fn
;;;
;;; Syntax: The "long form":
;;;     defsetf access-fn lambda-list ({store-variable}*)
;;;       [[{declaration}* | documentation]] {form}* => access-fn
;;;
;;; See Also:
;;;     define-setf-expander
;;;
;;; Expansion:
;;;  (defsetf symbol-value set)
;;;  ==> (%defsetf 'symbol-value 'set nil nil)
;;;
;;;  (defsetf subseq (sequence start &optional end) (new-sequence)
;;;    `(progn (replace ,sequence ,new-sequence ...) ,new-sequence) )
;;;  ==> (%defsetf 'subseq expander (sequence start &optional end) nil)
;;;  expander = #'(lambda (form env)
;;;                 (let ((#:temp (mapcar ...))
;;;                       (new-sequence #:g2) )
;;;                   (values vars vals store-vars writer-form reader-form) ))
;;;  reader-form = (access-fn ...)
;;;  writer-form  = (destucturing-bind (name . second) ...)
;
(defmacro cl:defsetf (&whole form access-fn second &rest rest)
  (labels (
    ;; exapnd-long-form
    (expand-long-form ()
        (declare (values cons t))
      (let ((var-form   (gensym "form_"))
            (var-tmps   (gensym "tmps_"))
            (var-args   (gensym "args_"))
            (var-vals   (gensym "vals_"))
            (var-scan   (gensym "scan_"))
            (decl*-keyword  nil)
            (fn-lerror  (gensym "lerror_"))
            (fn-rcheck  (gensym "rcheck_"))
            (fn-ocheck  (gensym "ocheck_"))
            (stores     (pop rest))
            (names      '())
            decl* form* binding* doc-string
            scan reqs opts rests keys auxs env )

        (labels (
          ;; check-name
          (check-name (name)
              (declare (values symbol))
            (unless (symbolp name)
              (error 'invalid-variable-name :name name) )
            (when (member name names)
              (style-warn "Lambda-list contains duplicated name: ~S" name) )
            (push name names)
            name )

          ;; make-expander
          (make-expander (&aux (var-env (or env (gensym "env"))))
              (declare (values form))
            `#'(lambda (,var-form ,var-env)
                   (declare (ext:lambda-name (defsetf ,access-fn)))
                   ,.(unless env `((declare (ignore ,var-env))))
                 (labels (
                   (,fn-lerror ()
                     (error 'syntax-error
                            :syntax '(,access-fn ,@second)
                            :form ,var-form ) )

                   ,.(when opts
                       `((,fn-ocheck (scan)
                           (cond
                             ((null scan) nil)
                             ((consp scan) t)
                             (t (,fn-lerror)) ) )))

                   ,.(when reqs
                       `((,fn-rcheck (scan)
                          (unless (consp scan) (,fn-lerror)) )))
                   )
                   ;;
                   (let* ((,var-tmps  '())
                          (,var-args  '())
                          (,var-vals  '())
                          (,var-scan  (rest ,var-form))
                          ,@binding*
                          ,.(mapcar #'(lambda (var) (list var `(gensym)))
                                  stores ) )
                    ,.decl*-keyword

                    ;; Checks extra arguments
                    ,@(unless rests `((when ,var-scan (,fn-lerror))))

                    ;; Makes return values for get-setf-expansion
                    (setq ,var-tmps (nreverse ,var-tmps))
                    (setq ,var-args (nreverse ,var-args))
                    (setq ,var-vals (nreverse ,var-vals))
                    (values
                        ,var-tmps
                        ,var-vals
                        (list ,@stores)
                        (locally ,@decl* (block ,access-fn ,@form*) )
                        `(,',access-fn ,@,var-args) ) ) ) ) )

          ;; make-opt-parser
          (make-opt-parser (initform stmp)
              (declare (type form initform))
              (declare (type symbol stmp))
              (declare (values form))
            `(if (not ,stmp)
                 (let ((val ,initform))
                   (push val ,var-vals)
                   val )
               (let* ((val (pop ,var-scan))
                      (var val) )
                 (unless (constantp val)
                   (setq var (gensym "o"))
                   (push var ,var-tmps)
                   (push val ,var-vals) )
                 (push var ,var-args)
                 var )) )

          ;; make-req-parser
          (make-req-parser ()
              (declare (values form))
            `(let* ((val
                      (progn
                        (,fn-rcheck ,var-scan)
                        (pop ,var-scan) ) )
                    (var val) )
               (unless (constantp val)
                 (push val ,var-vals)
                 (setq var (gensym "r"))
                 (push var ,var-tmps) )
                 (push var ,var-args)
                var ) )

          ;; make-rest-parser
          (make-rest-parser ()
              (declare (values form))
            `(let ((vars '()))
               (dolist (val ,var-scan (nreverse vars))
                 (if (constantp val)
                     (progn
                       (push val ,var-args)
                       (push val vars) )
                   (let ((var (gensym)))
                     (push var vars)
                     (push val ,var-vals)
                     (push var ,var-args)
                     (push var ,var-tmps) )) ) ) )
          )
          ;;
          (unless (listp stores)
            (macro-error "Invalid ~S long form: ~S" 'defsetf form) )

          (multiple-value-setq (decl* form* doc-string)
            (analyze-body rest t) )

          (multiple-value-setq (scan reqs opts rests keys auxs env)
            (analyze-lambda-list second nil t) )

          (when scan
            (macro-error "Extra token in lambda list: ~S" scan) )

          (when auxs
            (macro-error "Can't use ~S in ~S lambda list."
                '&aux
                'defsetf ))

          ;; Required parameters
          (dolist (rvar reqs)
            (push `(,(check-name rvar) ,(make-req-parser)) binding*) )

          ;; Optional parameters
          (dolist (ovar-init-svar (rest opts))
            (let ((ovar     (first  ovar-init-svar))
                  (initform (second ovar-init-svar))
                  (svar     (third  ovar-init-svar))
                  (stmp     (gensym)) )
              (check-name ovar)
              (push `(,stmp (,fn-ocheck ,var-scan)) binding*)
              (push `(,ovar ,(make-opt-parser initform stmp)) binding*)
              (when svar
                (check-name svar)
                (push `(,svar ,stmp) binding*) ) ) )

          ;; Rest parameter
          (when (and keys (null rests))
            (setq rests (list '&rest (gensym "rest"))) )

          (when rests
            (let ((rvar (second rests)))
              (check-name rvar)
              (push `(,rvar ,(make-rest-parser)) binding*) ))

          ;; Keyword parameters
          (when keys
            (dolist (param (rest keys))
              (let ((kvar (param-info-name param))
                    (svar (param-info-supplied param)) )
                (check-name kvar)
                (when svar (check-name svar)) ) )

            (multiple-value-bind (bind* decl*)
                (make-keyword-arguments-parser
                    (second rest)
                    (rest keys)
                    (eq (first keys) '&allow-other-keys) )
              (setq binding* (nreconc bind* binding*))
              (setq decl*-keyword decl*) ))

          ;; Exapnder
          (setq binding* (nreverse binding*))
          (values (make-expander) doc-string) ) ) )

    ;; expand-short-form
    (expand-short-form ()
        (declare (values cons t))
      (unless (null (cddr rest))
        (syntax-error
            :syntax '(defsetf access-fn update-fn &optional documentation)
            :form form ))

      (values
        `#'(lambda (form env)
               (declare (ext:lambda-name (defsetf . ,access-fn)))
               (declare (ignore env))
              ,@rest
              (let ((var-store (gensym "new_"))
                    (vars      (make-vars (rest form))) )
                (values vars
                        (rest form)
                        (list var-store)
                        `(,',second ,@vars ,var-store)
                        `(,',access-fn ,.vars) ) ) )
        (first rest) ) )
    )
    ;;
    ;; defsetf
    ;;
    (unless (symbolp access-fn)
      (macro-error
        "Expect symbol for function or macro name: ~S" access-fn ))

    (multiple-value-bind (expander doc-string)
        (cond
          ((symbolp second) (expand-short-form))
          ((consp   second) (expand-long-form))
          (t
            (macro-error
              "Second argument of DEFSETF must be symbol or cons: ~S"
              second ) ))

      (unless (or (null doc-string) (stringp doc-string))
        (macro-error "Invalid documentation: ~S~%" doc-string) )

      `(progn
         (eval-when (:compile-toplevel)
           (%defsetf ',access-fn ',second ,expander) )
         (si::%defsetf ',access-fn ',second ,expander ,doc-string) ) ) ) )


;;;; 5.3.2 defun
(defmacro cl:defun (name lambda-list &body body)
  `(progn
     (eval-when (:compile-toplevel)
       (%defun ',name ',lambda-list) )
     (si::%defun ',name ',lambda-list
       (labels ((,name ,lambda-list ,@body)) #',name) )) )


;;;; 5.3.16 defvar
(defmacro cl:defvar (name &optional (initform nil initform-p) doc-string)
  `(progn
     (eval-when (:compile-toplevel)
       (%defvar ',name ',initform) )
     ,(if (not initform-p)
          `(si::%defvar ',name nil ,doc-string nil)
        `(if (boundp ',name)
           (si::%defvar ',name nil ,doc-string nil)
         (si::%defvar ',name ,initform ,doc-string t) ))) )


;;;; 5.3.17 destructuring-bind
;;;
;;; Syntax:
;;;     destructuring-bind lambda-list expression {declaration}* {form*}
;;;     => {result}*
;
(defmacro cl:destructuring-bind (lambda-list expression &body body)
  (let ((var-expr (gensym)))
    `(let ((,var-expr ,expression))
      ,(parse-destructuring-bind lambda-list var-expr var-expr body nil) ) ) )


;;;; 5.3.48 multiple-value-bind
;;;
;;; Syntax:
;;;     multiple-value-bind ({var}*) values-form {declaration}* {form}*
;;;     => {result}*
;;;
;;; Note: compiler has multiple-value-bind parser. This macro exists
;;; for satsifying ANSI-CL conformance.
;
(defmacro cl:multiple-value-bind ((&rest var*) values-form &body body)
  (let ((var-rest '#:|rest|))
  (multiple-value-bind (decl* form*) (analyze-body body nil)
    `(multiple-value-call
        #'(lambda (&optional ,@var* &rest ,var-rest)
              (declare (ignore ,var-rest))
              (declare (dynamic-extent ,var-rest))
              ,@decl*
            ,@form* )
        ,values-form ) ) ) )


;;;; 5.3.50 multiple-value-list
(defmacro cl:multiple-value-list (form)
  `(multiple-value-call #'list ,form) )


;;;; 5.3.52 multiple-value-setq
;;;
;;; Since (setf values) return multiple values, but multiple-value-setq
;;; returns primary value only.
;
(defmacro cl:multiple-value-setq ((&rest name*) form)
  (let ((val '#:val))
    (if (null name*)
        `(let ((,val ,form)) ,val)
      `(let ((,val (setf (values ,@name*) ,form))) ,val) ) ) )


;;;; 5.3.56 nth-value
;;; Note: Compiler processes nth-calue.
(defmacro cl:nth-value (n form)
  `(nth (the (integer 0 ,(1- multiple-values-limit)) ,n)
        (multiple-value-list ,form) ) )


;;;; 5.3.44 or
;;; Example:
;;;     (or)            => nil
;;;     (or e1)         => e1
;;;     (or e1 e2 ...)  => (let ((#:temp e1)) (if #:temp #:temp (or e2 ...)))
;;;
;
(defmacro cl:or (&rest form*)
  (cond
   ((null form*) nil)
   ((null (cdr form*)) (car form*))
   (t (let ((var-or '#:|or|))
        `(let ((,var-or ,(car form*)))
           (if ,var-or ,var-or (or ,@(cdr form*))) ) ) )) )


;;;;  5.3.57 prog, prog*
;;;
;;; Syntax:
;;;     prog ({var | (var [init-form])}*) {declaration}* {tag | statement}*
;;;         => {result}*
;;;     prog* ({var | (var [init-form])}*) {declaration}* {tag | statement}*
;;;         => {result}*
;
(defmacro cl:prog ((&rest binding*) &body body)
  (multiple-value-bind (decl* form*)
      (analyze-body body nil)
    `(block nil
       (let ,binding* ,@decl* (tagbody ,@form*)) ) ) )


;;;;  5.3.57 prog, prog*
;;;
;;; Syntax:
;;;     prog ({var | (var [init-form])}*) {declaration}* {tag | statement}*
;;;         => {result}*
;;;     prog* ({var | (var [init-form])}*) {declaration}* {tag | statement}*
;;;         => {result}*
;
(defmacro cl:prog* ((&rest binding*) &body body)
  (multiple-value-bind (decl* form*)
      (analyze-body body nil)
    `(block nil
       (let* ,binding* ,@decl* (tagbody ,@form*)) ) ) )


;;;; 5.3.58 prog1, prog2
(defmacro cl:prog1 (first-form &rest form*)
  (let ((var-first (gensym "first-form_")))
    `(let ((,var-first ,first-form))
       ,@form*
       ,var-first ) ) )


;;;; 5.3.58 prog1, prog2
(defmacro cl:prog2 (first-form second-form &rest form*)
  (let ((var-second (gensym "second-form_")))
    `(let (,var-second)
       ,first-form
       (setq ,var-second ,second-form)
       ,@form*
       ,var-second ) ) )


;;;; 5.3.64 psetf
;;;
;;; Syntax:
;;;     psetf {{place form}}* => nil
;;;
;;; Expansion:
;;;     (let (binding*)
;;;       (setq var-1 val-1) ...
;;;       (multiple-value-setq store-1 form-1)
;;;       ...
;;;       store-form-1
;;;       store-form-2
;;;       ...
;;;       nil )
;
(defmacro cl:psetf (&rest pair* &environment env)
  (let ((scan         pair*)
        (binding*     '())
        (args-form*   '())
        (writer-form* '()) )
    (loop
      (when (null scan) (return))
      (let (place form)
        (setq place (pop scan))
        (when (null pair*)
          (error 'missing-setf-value-form :place place) )

        (setq form (pop scan))

        (multiple-value-bind (vars vals stores writer-form)
            (get-setf-expansion place env)

          (when vars
            (setq binding* (append binding* vars))
            (push `(setq ,.(mapcan #'list vars vals)) args-form*) )

          (setq binding* (append binding* stores))

          (if (null (rest stores))
              (push `(setq ,(first stores) ,form) args-form*)
            (push `(multiple-value-setq ,stores ,form) args-form*) )

          (push writer-form writer-form*) ) ))

    (setq args-form*   (nreverse args-form*))
    (setq writer-form* (nreverse writer-form*))
    `(let ,binding*
       ,@args-form*
       ,@writer-form*
       ,(make-ignorable-form nil) ) ) )


;;;; 5.3.21 psetq
;;;
;;; Description:
;;;  Expands into psetf form for handling symbol-macro.
;
(defmacro cl:psetq (&whole form &rest pair*)
  (let ((runner pair*))
    (loop
      (when (endp runner)
        (return `(psetf ,@pair*)) )
      (let ((var (pop runner)))
        (unless (symbolp var)
          (macro-error "Not variable: ~S" var) )
        (cond
          ((null runner)
            (error 'missing-setf-value-form :place var) )
          ((not (consp runner))
            (macro-error "Malformed form: ~S" form) ))
        (pop runner) )) ) )


;;;; 5.3.26 return
;;;
;;; Syntax:
;;;     return [result] => |
;
(defmacro cl:return (&optional (result nil result-p))
  `(return-from nil ,.(and result-p (list result))) )


;;;; 5.3.66 rotatef
;;;
;;; Syntax:
;;;     roatef {place}* => nil
;;;
;;; Expansion:
;;;  (let (var-1 var-2 ... new-1 new-2 ...)
;;;    (setq var-1 val-1)
;;;    (setq var-2 val-2)
;;;    ...
;;;    (multiple-value-setq (new-n ...) (reader-form-1 var-1 ...))
;;;    (multiple-value-setq (new-2 ...) (reader-form-2 var-2 ...))
;;;    ...
;;;    (setq new-1 (reader-form-n var-n ...))
;;;    (store-form-1)
;;;    ...
;;;    (store-form-n) )
;
(defmacro cl:rotatef (&rest place* &environment env)
  (let ((binding*     '())
        (stores*      '())
        (args-form*   '())
        (reader-form* '())
        (writer-form* '()) )

     (dolist (place place*)
       (multiple-value-bind (vars vals stores writer-form reader-form)
           (get-setf-expansion place env)

         (when vars
           (setq binding* (append binding* vars))
           (push `(setq ,.(mapcan #'list vars vals)) args-form*) )

         (setq binding* (append binding* stores))

         (push stores      stores*)
         (push reader-form reader-form*)
         (push writer-form writer-form*) ) )

     (setq args-form*   (nreverse args-form*))
     (setq reader-form* (nreverse reader-form*))
     (setq writer-form* (nreverse writer-form*))

     (when stores*
       (setq stores* (cons (first stores*) (nreverse (rest stores*)))) )

     (let ((stores-form* '()))
       (dolist (stores stores*)
         (let ((reader-form (pop reader-form*)))
           (if (null (rest stores))
               (push `(setq ,(first stores) ,reader-form)
                     stores-form* )
             (push `(multiple-value-setq ,stores ,reader-form)
                    stores-form* )) ) )
       (setq stores-form* (nreverse stores-form*))

      `(let ,binding*
          ,@args-form*
          ,@stores-form*
          ,@writer-form*
          ,(make-ignorable-form nil) ) ) ) )


;;;; 5.3.64 setf
;;;
;;; Syntax:
;;;     setf {{place form}}* => nil
;;;
;;; Expansion:
;;;     (progn
;;;       (let (binding*-1)
;;;         (multiple-value-bind (store*-1)
;;;             form-1
;;;           store-form-1 )
;;;         ... ))
;;;
;;; Optimization:
;;;  let + multiple-value-bind => (setq x a)
;;;  let + multiple-value-bind => let + setq
;
(defmacro cl:setf (&rest pair* &environment env)
  (let ((scan  pair*)
        (form* '()) )
    (loop
      (when (null scan) (return))
      (let (place form)
        (setq place (pop scan))
        (when (null scan)
          (error 'missing-setf-value-form :place place) )

        (setq form (pop scan))

        (multiple-value-bind (vars vals stores writer-form)
            (get-setf-expansion place env)
          (cond
            ((null stores)
              (push `(progn ,@vals ,writer-form) form*) )
            ;; setq
            ((and (null vars)
                  (null vals)
                  (null (rest stores))
                  (eq (first writer-form) 'setq)
                  (eq (second writer-form) place)
                  (eq (first stores) (third writer-form))
                  (null (cdddr writer-form)) )
             (push `(setq ,place ,form) form*) )

            ;; single store
            ((null (rest stores))
             (push `(let (,.(mapcar #'list vars vals)
                          (,(first stores) ,form) )
                      ,writer-form )
                   form* ) )

            ;; multiple stores
            (t
              (push `(let ,(mapcar #'list vars vals)
                       (multiple-value-bind ,stores
                            ,form
                         ,writer-form ) )
                    form* ) )) ) ))
     (setq form* (nreverse form*))
     (if (null (rest form*))
         (first form*)
       `(progn ,@form*) ) ) )


;;;; 5.3.65 shift
;;;
;;; Syntax:
;;;     shift {place}+ new-value => old-value-1
;;;
;;; Note:
;;;  (shiftf place1 place2 ... placen newvalue) is roughly equivalent to
;;;     (let ((var1 place1) (var2 place2) ... (varn placen)
;;;           (var0 newvalue) )
;;;       (setf place1 var2)
;;;       (setf place2 var3)
;;;       ...
;;;       (setf placen var0)
;;;       var1 )
;;;
;;; Expansion:
;;;  (let (var-1 var-2 ... new-1 new-2 ...)
;;;    (setq var-1 val-1)
;;;    (setq var-2 val-2)
;;;    ...
;;;    (setq old-val-1 (reader-form-1 var-1 ...))
;;;    (multiple-value-setq (new-1 ...) (reader-form-2 var-2 ...))
;;;    ...
;;;    (store-form-1)
;;;    ...
;;;    (store-form-n)
;;;    old-val-1 )
;
(defmacro cl:shiftf (place new-value &rest place* &environment env)
  (setq place*    (list* place new-value place*))
  (setq place*    (reverse place*))
  (setq new-value (pop place*))
  (setq place*    (nreverse place*))

  (let* ((stores*      (list (list (gensym "old_"))))
         (binding*     (list (caar stores*)))
         (args-form*   '())
         (reader-form* '())
         (writer-form* '()) )

     (dolist (place place*)
       (multiple-value-bind (vars vals stores writer-form reader-form)
           (get-setf-expansion place env)

         (when vars
           (setq binding* (append binding* vars))
           (push `(setq ,.(mapcan #'list vars vals)) args-form*) )

         (setq binding* (append binding* stores))

         (push stores      stores*)
         (push reader-form reader-form*)
         (push writer-form writer-form*) ) )

     (push new-value reader-form*)

     (setq args-form*   (nreverse args-form*))
     (setq reader-form* (nreverse reader-form*))
     (setq stores*      (nreverse stores*))
     (setq writer-form* (nreverse writer-form*))

     (let ((stores-form* '()))
       (dolist (stores stores*)
         (let ((reader-form (pop reader-form*)))
           (if (null (rest stores))
               (push `(setq ,(first stores) ,reader-form)
                     stores-form* )
             (push `(multiple-value-setq ,stores ,reader-form)
                    stores-form* )) ) )
       (setq stores-form* (nreverse stores-form*))

      `(let ,binding*
          ,@args-form*
          ,@stores-form*
          ,@writer-form*
          ,(caar stores*) ) ) ) )


;;;; 5.3.45 when, unless
;;;
;;; Syntax:
;;;     when test-form {form}* => {results}*
;;;     unless test-form {form}* => {result}*
;;;
(defmacro cl:unless (test-form &rest form*)
  `(if (not ,test-form) (progn ,@form*)) )


;;;; 5.3.45 when, unless
;;;
;;; Syntax:
;;;     when test-form {form}* => {results}*
;;;     unless test-form {form}* => {result}*
;;;
(defmacro cl:when (test-form &rest form*)
  `(if ,test-form (progn ,@form*)) )
