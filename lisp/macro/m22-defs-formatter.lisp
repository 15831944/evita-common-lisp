;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 22 Printer - Declarations
;;; macro/m22-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m22-defs-formatter.lisp#4 $
;;;
;;; Description:
;;;  This file contains declarations for printer macros:
;;;     formatter-context   (structure)
;;;     formatter-error     (condition)
;;;     define-formatter    (macro)
;;;
;
(in-package :xc)

;;;; formatter-context
;;;
;;; Slots:
;;;  forms              - A list of compiled-format forms.
;;;
;;;  next-args          - A list of markers of next argument references.
;;;  prev-args          - A list of markers of ~:P. Introduce binding of arg.
;;;  argc-args          - A list of markers of ~#.
;;;
;;;  arg-ctrl-p         - True when arbitrary argument reference by
;;;                       control-flow, ~[...~] or ~{...~}.
;;;  arg-goto-p         - True when arbitrary argument reference by ~*.
;;;                       Introduce binding of all-args.
;;;
;;;  up-and-out-p       - True when ~^ is appeard.
;;;  up-up-and-out-p    - True when ~:^ is appeard.
;;;  sublist-loop-p     - True during prorcessing ~{...:}.
;;;  indirect-p         - True when ~? is appeard.
;;;
;;;  fill-p             - True during processing ~<...~:@> for inserting
;;;                       "~:_" after each group of blanks.
;;;  no-more-args-p     - True after processing ~@<...~:>.
;;;  pprint-p           - True during processing logical-block.
;;;
;;; Note: We use term "argc" instead of "nargs" here. Because of length
;;; of "argc" is as same as "next" and "prev".
;
(defstruct (formatter-context
             (:constructor  make-formatter-context ())
             (:copier       nil)
             (:predicate    nil) )
  (ctrl-string          ""  :type simple-string)
  (ctrl-start           0   :type ext:sequence-index)
  (ctrl-end             0   :type ext:sequence-index)
  (ctrl-index           0   :type ext:sequence-index)

  (forms                '() :type list)

  (next-args            '() :type list)
  (prev-args            '() :type list)
  (argc-args            '() :type list)

  (arg-ctrl-p           nil :type boolean)
  (arg-goto-p           nil :type boolean)

  (go-up-and-out-p      nil)
  (indirect-p           nil)

  (sublist-loop-p       nil)
  (go-up-up-and-out-p   nil)
  (up-up-and-out-p      nil)


  ;; pprint-logibal-block
  (fill-p               nil)
  (pprint-p             nil)
  (no-more-args-p       nil)

  (use-stream-p         nil) )


;;;; formatter-error
;
(define-condition formatter-error (program-error)
  ((complaint
        :initarg    :complaint
        :type       string )
   (arguments
        :initarg    :arguments
        :type       list )
   (context
        :initarg    :context
        :type       formatter-context ) )
  (:report
    (lambda (c s)
      (let* ((context     (slot-value c 'context))
             (ctrl-string (formatter-context-ctrl-string context))
             (ctrl-index  (formatter-context-ctrl-index  context)) )
        (setq ctrl-string
              (subseq ctrl-string
                      (formatter-context-ctrl-start context)
                      (formatter-context-ctrl-end   context) ))
        (format s "Formatter: ~?~%~A~%~VT^~%"
                (slot-value c 'complaint)
                (slot-value c 'arguments)
                ctrl-string
                (1- ctrl-index) ) ) )) )


;;;; formatter-context-let
;;;
;;; Syntax:
;;;   formatter-context-let (slot-spec*) form* => result*
;;;
;;;   slot-spec ::= slot-name | (slot-name value)
;
(defmacro formatter-context-let ((&rest slot-spec*) &body form*)
  (labels (
    (reader-name (slot-name)
      (intern (format nil "FORMATTER-CONTEXT-~A" slot-name)) )

    (save-name (slot-name)
      (intern (format nil "save-~A" slot-name)) )
    )
    ;;
    ;; formatter-context-let
    ;;
    (multiple-value-bind (bindings write-forms restore-forms)
        (loop
          for slot-spec in slot-spec*
          with save-name
          with reader-form
          with value-form
          do
            (cond
              ((symbolp slot-spec)
                (setq save-name   (save-name slot-spec))
                (setq reader-form `(,(reader-name slot-spec) context))
                (setq value-form  reader-form) )

              ((eql (si::safe-list-length slot-spec) 1)
                (setq slot-spec (first slot-spec))
                (setq save-name   (save-name slot-spec))
                (setq reader-form `(,(reader-name slot-spec) context))
                (setq value-form  reader-form) )


              ((eql (si::safe-list-length slot-spec) 2)
                (setq save-name   (save-name (first slot-spec)))
                (setq reader-form `(,(reader-name (first slot-spec)) context))
                (setq value-form  (second slot-spec)) )

              (t
                (error 'simple-program-error
                       :format-control  "Invalid slot-spec: ~S"
                       :format-arguments (list slot-spec) )))
          collect `(,save-name         ,reader-form) into bindings
          collect `(setf ,reader-form  ,value-form)  into write-forms
          collect `(setf ,reader-form  ,save-name)   into restore-forms
          finally (return (values bindings write-forms restore-forms)) )
    `(let ,bindings
       ,@write-forms
       (multiple-value-prog1
           (progn ,@form*)
         ,@restore-forms ) ) ) ) )


;;;; define-formatter
;;;
;;; Syntax:
;;;   define-formatter char (param-spec*) {decl}* {form}*
;;;
;;; Local Macros:
;;;   add-form          - Adds form to expansion. Form may not include
;;                        reference to stream argument.
;;;   add-stream-form   - Adds form, which referes stream argument to
;;                        expansion.
;
(defmacro define-formatter (char (&rest modifier*) (&rest param-spec*)
                            &body body )
  (let ((fn-formatter   (intern (format nil "~~~:C formatter" char)))
        (minparams      0)
        (maxparams      0)
        (bindings       '())
        (syntax         "") )

    (unless (equal '(*) param-spec*)
      (dolist (param-spec param-spec*)
        (cond
          ((symbolp param-spec)
            (push `(,param-spec (pop param-scan)) bindings)
            (incf minparams) )

          ((eql (si::safe-list-length param-spec) 2)
            (push `(,(first param-spec) (or (pop param-scan)
                                            ,(second param-spec) ))
                  bindings ) )
          (t
            (macro-error"Invalid parameter specifier: ~S"
                param-spec ) ))
        (incf maxparams) ))

    (when bindings
      (setq bindings (nreverse bindings))
      (push '(param-scan params) bindings) )

    (setq syntax
       (format nil "~~~{~(~A~)~^,~}~:C" (mapcar #'car bindings) char) )

    `(defun ,fn-formatter (context modifier params)
         (declare (type formatter-context context))
         (declare (type (unsigned-byte 2) modifier))
         (declare (type list              list))
         (declare (ignorable modifier))
         (declare (dynamic-extent params))
       (macrolet (
         ;; add-form
         ;;
         (add-form (form)
           `(push ,form (formatter-context-forms context)) )

         ;; add-stream-form
         ;;
         (add-stream-form (form)
            `(progn
               (setf (formatter-context-use-stream-p context) t)
               (add-form ,form) ) )

         ;; check-param-type
         ;;
         (check-param-type (var type)
           (unless (symbolp var)
             (macro-error "~S accepts only variable name: ~S"
                    'check-param-type var ))
           (ecase type
             (integer
               `(when (characterp ,var)
                  (formatter-error-param-type-error
                    context ',var ,var ',type )) )
             (character
               `(when (integerp ,var)
                  (formatter-error-param-type-error
                    context ',var ,var ',type )) )) )

          ;; next-arg
          ;;
          (next-arg ()
            '(formatter-next-arg context) )

          ;; prev-arg
          ;;
          (prev-arg ()
            '(let* ((args   (formatter-context-next-args context))
                    (marker (list args)) )
               (when (null args)
                 (formatter-error-no-prev-arg) )
               (push marker (formatter-context-prev-args context))
               marker ) )
         )
         ;;
         ;; formatter
         ;;

         ;; Check modifier
         ;;
         ,@(cond
             ((equal '(*) modifier*)
               nil )

             ((null modifier*)
               `((unless (= 0 modifier)
                 (formatter-error-invalid-modifier context ,char 0) )) )

             ((equal '(:colon) modifier*)
              `((unless (<= 0 modifier 1)
                  (formatter-error-invalid-modifier context ,char 1) )) )

             ((equal '(:at-sign) modifier*)
              `((unless (or (= 0 modifier) (= 2 modifier))
                  (formatter-error-invalid-modifier context ,char 2) )) )

            ((equal '(:colon :at-sign) modifier*)
              `((unless (<= 0 modifier 2)
                  (formatter-error-invalid-modifier context ,char 3) )) )
            (t
              (macro-error "Invalid modifier specification: ~S"
                     modifier* ) ))

         ;; Check number of parameters
         ;;
         ,@(unless (equal '(*) param-spec*)
            `((let ((paramc (length params)))
                (cond
                  ((< paramc ,minparams)
                    (formatter-error-too-few-params context ,syntax) )
                  ((> paramc ,maxparams)
                   (formatter-error-too-many-params context ,syntax) )) )))

         (block formatter
           (let* ,bindings ,@body) ) ) ) ) )

(deftype formatter-state () '(member :none :digit :sign :param))

(declaim (ftype (function (formatter-context sequence-index sequence-index)
    (values character list sequence-index formatter-state) )
  formatter-parse-param ) )

(declaim (ftype (function ((integer 0 15) t) (values t t))
  formatter-parse-loop-frame ) )

(declaim (ftype (function
    (formatter-context character sequence-index)
    unspecified )
  formatter-skip-to-close ) )

(declaim (ftype (function (formatter-context list) list)
  formatter-subst-args ) )
