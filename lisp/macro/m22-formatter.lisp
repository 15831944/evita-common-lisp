;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 22 Printer - Formatter
;;; macro/m22-formatter.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m22-formatter.lisp#3 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     formatter-error-clause-count
;;;     formatter-error-ctrl-end
;;;     formatter-error-expect-digit
;;;     formatter-error-invalid-modifier
;;;     formatter-error-no-prev-arg
;;;     formatter-error-no-close
;;;     formatter-error-no-close
;;;     formatter-error-pp-malformed
;;;     formatter-error-param-type
;;;     formatter-error-semicolon-brace
;;;     formatter-error-semicolon-paren
;;;     formatter-error-too-many-params
;;;     formatter-error-too-many-modifiers
;;;     formatter-error-too-many-params
;;;     formatter-next-arg
;;;     formatter-parse
;;;     formatter-parse-1                       ~[...~]
;;;     formatter-parse-2                       ~{...~}
;;;     formatter-parse-3                       ~<...~>
;;;     formatter-parse-loop-d
;;;     formatter-parse-loop-frame
;;;     formatter-parse-loop-i
;;;     formatter-parse-param
;;;     formatter-parse-logical-block           ~< ... ~:>
;;;     formatter-skip-caluse
;;;     formatter-skip-to-close
;;;     formatter-slashed-symbol                ~/.../
;;;     formatter-subst-args
;;;
;;; Public Macros:
;;;     formatter                       22.4.2
;;;
;;; BUGBUG: NYI: Optimize ~@[consequente]
;;; BUGBUG: NYI: Remove nargs checking just after ~^.
;
(in-package :xc)


;;;; formatter-error-clause-count
;
(defun formatter-error-clause-count (context expect)
  (error 'formatter-error
         :context   context
         :complaint "Must be ~R clause~:P."
         :arguments (list expect) ) )


;;;; formatter-error-ctrl-end
;
(defun formatter-error-unexpected-eof (context end)
  (setf (formatter-context-ctrl-index context) end)
  (error 'formatter-error
         :context   context
         :complaint "Unexpected end of control string." ) )


;;;; formatter-error-expect-digit
;
(defun formatter-error-expect-digit (context index)
  (setf (formatter-context-ctrl-index context) index)
  (error 'formatter-error
         :context   context
         :complaint "Expect digit character." ) )


;;;; formatter-error-invalid-modifier
;
(defun formatter-error-invalid-modifier (context char mask)
  (let ((complaint
          (ecase mask
            (0 "~~~:C doesn't accept modifiers.")
            (1 "~~~:C accepts only colon(:) modifier.")
            (2 "~~~:C accepts only at-sign(@) modifier.")
            (3 "~~~:C doesn't accept combination of colon(:) and at-sign(@).")) ))
    (error 'formatter-error
           :context   context
           :complaint complaint
           :arguments (list char) ) ) )


;;;; formatter-error-no-close
;
(defun formatter-error-no-close (context open-index)
  (setf (formatter-context-ctrl-index context) open-index)
  (error 'formatter-error
         :context   context
         :complaint "No matching close directive." ) )


;;;; formatter-error-no-prev-arg
;
(defun formatter-error-no-prev-arg (context open-index)
  (setf (formatter-context-ctrl-index context) open-index)
  (error 'formatter-error
         :context   context
         :complaint "No previous argument." ) )


;;;; formatter-error-param-type-error
;
(defun formatter-error-param-type-error (context name value type)
  (error 'formatter-error
         :context   context
         :complaint "Parameter ~A(~S) must be type of ~S."
         :arguments (list name value type) ) )


;;;; formatter-error-pp-malformed
;
(defun formatter-error-pp-malformed (context)
  (error 'formatter-error
         :context   context
         :complaint "~~<...~~:> requires at most three clauses." ) )


;;;; formatter-error-semicolon-brace
;
(defun formatter-error-semicolon-brace (context)
  (error 'formatter-error
         :context   context
         :complaint "Can't use ~~; inside ~~{...~~}." ) )


;;;; formatter-error-semicolon-paren
;
(defun formatter-error-semicolon-paren (context)
  (error 'formatter-error
         :context   context
         :complaint "Can't use ~~; inside ~~(...~~)." ) )


;;;; formatter-error-too-few-params
;
(defun formatter-error-too-few-params (context syntax)
  (error 'formatter-error
         :context   context
         :complaint "Too few parametes for ~A."
         :arguments (list syntax) ) )


;;;; formatter-error-too-many-modifiers
;
(defun formatter-error-too-many-modifiers (context ctrl-index name char)
  (setf (formatter-context-ctrl-index context) ctrl-index)
  (error 'formatter-error
         :context   context
         :complaint "Directive syntax erorr: Two ~(~A~)(~C) modifiers."
         :arguments (list name char) ) )


;;;; formatter-error-too-many-params
;
(defun formatter-error-too-many-params (context syntax)
  (error 'formatter-error
         :context   context
         :complaint "Too many parametes for ~A."
         :arguments (list syntax) ) )


;;;; formatter-next-arg
;;;
;;; Called by:
;;;  define-formatter next-arg
;
(defun formatter-next-arg (context)
  (when (formatter-context-no-more-args-p context)
    (error 'formatter-error
           :context   context
           :complaint "No arguments left after ~~@<...~~:>." ) )

  (let ((marker (list nil)))
    (push marker (formatter-context-next-args context))
    marker ) )


;;;; formatter-insert-fill
;;;
;;; For: ~<...~:@>
;;;
;;; Description:
;;;  Inserts fill style conditional newline after each group of blanks.
;
(defun formatter-insert-fill (context string start end)
    (declare (type formatter-context context))
    (declare (type simple-string     string))
    (declare (type ext:sequence-index    start))
    (declare (type ext:sequence-index    end))
  (loop
    with prev    = start
    with nblanks = 0
    for  index from start below end
    do
     (cond
       ((si::whitespace-char-p (schar string index))
         (incf nblanks) )
       ((plusp nblanks)
         (setf (formatter-context-use-stream-p context) t)

         (cond
           ((= (- index prev) 1)
             (push `(write-char ,(schar string prev) stream)
                   (formatter-context-forms context) ) )

           ((= (- index prev) nblanks)
             (push `(si::write-chars #\Space ,nblanks stream)
                   (formatter-context-forms context) ) )
           (t
             (push `(write-string ,(subseq string prev index) stream)
                    (formatter-context-forms context) ) ))

         (push '(pprint-newline :fill stream)
               (formatter-context-forms context) )
         (setq nblanks 0)
         (setq prev    index) ) )
    finally
      (when (> index prev)
         (setf (formatter-context-use-stream-p context) t)

         (cond
           ((= (- index prev) 1)
             (push `(write-char ,(schar string prev) stream)
                   (formatter-context-forms context) ) )

           ((= (- index prev) nblanks)
             (push `(si::write-chars #\Space ,nblanks stream)
                   (formatter-context-forms context) ) )
           (t
             (push `(write-string ,(subseq string prev index) stream)
                    (formatter-context-forms context) ) ))

         (when (plusp nblanks)
           (push '(pprint-newline :fill stream)
                 (formatter-context-forms context) )))) )


;;;; formatter-parse
;;;
;;; Note: Compiler macro write-string turns write-string with one character
;;; string into write-char.
;
(defun formatter-parse (context ctrl-start ctrl-end)
    (declare (type formatter-context context))
    (declare (type ext:sequence-index    ctrl-start))
    (declare (type ext:sequence-index    ctrl-end))
  (let ((ctrl-prev  ctrl-start)
        (ctrl-index ctrl-start)
        (ctrl-str   (formatter-context-ctrl-string context))
        (fill-p     (formatter-context-fill-p context)) )
      (declare (type simple-string     ctrl-str))
      (declare (type ext:sequence-index    ctrl-index))
      (declare (type ext:sequence-index    ctrl-prev))
    (block outer
      (loop
        ;; Search tilde(~)
        (loop
          (when (= ctrl-end ctrl-index) (return-from outer))
          (when (char= #\~ (schar ctrl-str ctrl-index)) (return))
          (incf ctrl-index) )

        ;; Found tilde(~)
        (when (> ctrl-index ctrl-prev)
          (setf (formatter-context-use-stream-p context) t)
          (if fill-p
              (formatter-insert-fill context ctrl-str ctrl-prev ctrl-index)
            (push `(write-string ,(subseq ctrl-str ctrl-prev ctrl-index)
                                 stream )
                  (formatter-context-forms context) )) )

        (incf ctrl-index)
        (when (= ctrl-end ctrl-index)
          (formatter-error-unexpected-eof context ctrl-end) )

        (setf (formatter-context-ctrl-index context) ctrl-index)

        (let ((char    #\=)
              (params  '())
              (modifier 0) )
            (declare (type character     char))
            (declare (type list          params))
            (declare (type (integer 0 3) modifier))

          ;; Parse parameters
          (loop with last = nil do
            (multiple-value-bind (stop-char param ctrl-next state)
                (formatter-parse-param context ctrl-index ctrl-end)

              (setq ctrl-index ctrl-next)
              (setq char stop-char)

              (unless (eq :none state)
                (if last
                    (setq last (setf (cdr last) (list param)))
                  (setq last (list param) params last) ))

              (unless (char= #\, char)
                (return) ) ))

          ;; Parse modifiers
          (loop
            (case char
              (#\:
                (when (logbitp 0 modifier)
                  (formatter-error-too-many-modifiers
                      context ctrl-index :colon #\: ))
                (setq modifier (logior 1 modifier)) )
              (#\@
                (when (logbitp 1 modifier)
                  (formatter-error-too-many-modifiers
                      context ctrl-index :at-sign #\@) )
                 (setq modifier (logior 2 modifier)) )
              (otherwise (return)) )

            (when (= ctrl-end ctrl-index)
              (formatter-error-unexpected-eof context ctrl-end) )
            (setq char (schar ctrl-str ctrl-index))
            (incf ctrl-index) )

          ;; Dispatch to directive handlers
          ;;
          (setf (formatter-context-ctrl-index context) ctrl-index)

          (let ((handler
                   (and (< (char-code char) 128)
                        (intern (format nil "~~~:C formatter" char)
                                 #.(find-package :xc) ))) )
              (unless (fboundp handler)
                (error 'formatter-error
                       :context   context
                       :complaint "Unknown directive ~~~C."
                       :arguments (list char) ))

              (funcall handler context modifier params)

              (setq ctrl-index (formatter-context-ctrl-index context)) ) )

        ;; Setup for rest of control string
        ;;
        (setq ctrl-prev ctrl-index) ))

    ;; Emit trailing string
    ;;
    (when (> ctrl-index ctrl-prev)
      (setf (formatter-context-use-stream-p context) t)
      (if fill-p
          (formatter-insert-fill context ctrl-str ctrl-prev ctrl-index)
        (push `(write-string ,(subseq ctrl-str ctrl-prev ctrl-index) stream)
              (formatter-context-forms context) ))) ) )


;;;; formatter-parse-1
;;;
;;; Syntax:
;;;   formatter-parse-1 context ctrl-start ctrl-end
;;;     => forms
;;;
;;; Called by:
;;;  formatter-parse-2
;;;
;;; Description:
;;;  Parses control-string between ctrl-start and ctrl-end then returns
;;;  forms.
;
(defun formatter-parse-1 (context ctrl-start ctrl-end)
  (formatter-context-let ((ctrl-index ctrl-start)
                          (forms      '()) )
    (formatter-parse context ctrl-start ctrl-end)
    (nreverse (formatter-context-forms context)) ) )


;;;; formatter-parse-2
;;;
;;; Syntax:
;;;   formatter-parse-2 context ctrl-start ctrl-end form-arg
;;;     => bindings, forms
;;;
;;; Called by:
;;;  formatter-parse-3
;;;  formatter-parse-loop-d (inserts forms before/after returned form)
;;;
;;; Arguments and Values:
;;;  bindings   - a list.
;;;  forms      - a list.
;;;
;;; Description:
;;;  Returns form, which consumes list of argument.
;
(defun formatter-parse-2 (context ctrl-start ctrl-end form-arg)
  (formatter-context-let ((next-args  '())
                          (prev-args  '())
                          (argc-args  '())
                          (arg-ctrl-p  nil)
                          (arg-goto-p  nil) )
    (let ((forms
            (formatter-parse-1 context ctrl-start ctrl-end) )
          (bindings
             (and (formatter-context-prev-args context) '(arg prev-arg)) ))

      (when (formatter-context-arg-goto-p context)
        (push '(all-args args) bindings) )

      (setq forms (formatter-subst-args context forms))

      (unless (eq 'args form-arg)
        (push `(args ,form-arg) bindings) )

      (values bindings forms) ) ) )


;;;; formatter-parse-3
;;;
;;; Syntax:
;;;   formatter-parse-3 context ctrl-start ctrl-end form-arg
;;;     => form
;;;
;;; Arguments and Values:
;;;  form   - a list == (let* ((args ,form-arg)) ...)
;;;
;;; Description:
;;;  Returns form, which consumes list of argument.
;
(defun formatter-parse-3 (context ctrl-start ctrl-end form-arg)
  (multiple-value-bind (bindings forms)
      (formatter-parse-2 context ctrl-start ctrl-end form-arg)
   (if (null bindings)
       `(progn ,@forms)
     `(let* ,bindings ,@forms) ) ) )


;;;; formatter-parse-logical-block
;;;
;;; Variations:
;;;     ~< prefix ~; body ~; suffix ~>
;;;     ~:<     = prefix="(" suffix=")"
;;;     ~@:>    = insert ~:_ (pprint-newline :fill)
;;;     ~< per-line-prefix ~:; body ... ~:>
;;;
;;; Description:
;;;  Compiles ~<...~:>.
;
(defun formatter-parse-logical-block (context
                                      form-arg modifier clauses
                                      per-line-prefix-p fill-p )
  (multiple-value-bind (prefix body-clause suffix)
      (case (length clauses)
        (1  ; ~< body ~:>
           (if (logbitp 0 modifier) 
               (values "(" (first clauses) ")")
             (values "" (first clauses) "") ) )
        (2  ; ~< prefix ~; body ~:>
          (values (subseq (formatter-context-ctrl-string context)
                          (car (first clauses))
                          (cdr (first clauses)) )
                  (second clauses)
                  (if (logbitp 0 modifier) ")" "") ) )
        (3  ; ~< prefix ~; body ~; suffix ~:>
          (values (subseq (formatter-context-ctrl-string context)
                          (car (first clauses))
                          (cdr (first clauses)) )
                  (second clauses)
                  (subseq (formatter-context-ctrl-string context)
                           (car (third clauses))
                           (cdr (third clauses)) )) )
        (otherwise
          (formatter-error-pp-malformed context) ))

    (let ((per-line-prefix nil)
          (form
            (formatter-context-let ((fill-p          fill-p)
                                    (go-up-and-out-p nil)
                                    (indirect-p      nil)
                                    (pprint-p        t) )
              (let ((form
                      (formatter-parse-3 context
                                        (car body-clause)
                                        (cdr body-clause)
                                        'logical-block-arg ) ))

                (when (formatter-context-go-up-and-out-p context)
                  (setq form `(tagbody ,form up-and-out)) )

                (when (formatter-context-indirect-p context)
                  (setq form `(catch 'up-and-out ,form)) )

                form ) ) ))

      (when (logbitp 1 modifier)   ; ~@<...~:>
        (setf (formatter-context-no-more-args-p context) t) )

      ;; Cook keyword arguments.
      ;;
      (cond
        ((string= "" prefix)
          (setq prefix nil) )
        (per-line-prefix-p
          (setq per-line-prefix `(:per-line-prefix ,prefix))
          (setq prefix nil) )
        (t
          (setq prefix `(:prefix ,prefix)) ))

      (if (string= "" suffix)
          (setq suffix nil)
        (setq suffix `(:suffix ,suffix)) )

      ;; Make pprint-logical-block form
      ;;
      (setq form
        `(let ((logical-block-arg ,form-arg))
           (pprint-logical-block (stream logical-block-arg
                                  ,@prefix
                                  ,@suffix
                                  ,@per-line-prefix )
             ,form )
           ,@(and (logbitp 1 modifier) `((setq args nil))) ))
      form ) ) )


;;;; formatter-parse-loop-d
;;;
;;; Syntax:
;;;  formatter-parse-loop-d context modifer mode form-n
;;;                         ctrl-start ctrl-end => form
;;;
;;; Arguments and Values:
;;;  context    - a formatter-context.
;;;  mode       - 0 .. 7
;;;
;;; Description:
;;;  Parses direct-loop.
;
(defun formatter-parse-loop-d (context mode form-n ctrl-start ctrl-end)
    (declare (type formatter-context context))
    (declare (type (integer 0 7)     mode))
  (ecase (logand mode 3)
    (0  ; ~{str~}
      (let (bindings-1 exit-forms
            bindings-2 forms
            form
            (form-arg (formatter-next-arg context)) )

       ;; Iteration test
       (multiple-value-bind (bindings test-form)
           (formatter-parse-loop-frame mode form-n)
         (setq bindings-1 bindings)
         (setq exit-forms
           `((when ,test-form (go up-and-out))) ) )

        (formatter-context-let ((go-up-and-out-p nil)
                                (indirect-p      nil)
                                (no-more-args-p  nil)
                                (pprint-p        nil) )
          ;; Iteration body
          (multiple-value-setq (bindings-2 forms)
             (formatter-parse-2 context ctrl-start ctrl-end form-arg) )

          ;; Iteration form
          (setq form
            `(let* (,@bindings-1 ,@bindings-2)
               (tagbody
                 next-loop
                   ,@(unless (logbitp 3 mode) exit-forms)
                   ,@forms
                   ,@(when (logbitp 3 mode) exit-forms)
                   (go next-loop)
                 up-and-out ) ))

          (when (formatter-context-indirect-p context)
            (setq form `(catch 'up-and-out ,form)) )

          form ) ) )

    (1  ; ~:{str~}
      (let (bindings-1 test-form
            bindings-2 forms
            form
            (form-arg (formatter-next-arg context)) )

        (formatter-context-let ((go-up-and-out-p    nil)
                                (go-up-up-and-out-p nil)
                                (indirect-p         nil)
                                (no-more-args-p     nil)
                                (pprint-p           nil)
                                (up-up-and-out-p    t) )

          ;; Iteration test
          (multiple-value-setq (bindings-1 test-form)
             (formatter-parse-loop-frame mode form-n) )

          ;; Iteration body
          (multiple-value-setq (bindings-2 forms)
             (formatter-parse-2 context ctrl-start ctrl-end
                                '(pop sublists) ))

          (setq form `(let* ,bindings-2 ,@forms))

          (when (formatter-context-indirect-p context)
            (setq form `(catch 'up-and-out ,form)) )

          ;; Iteration form
          (setq form
            `(let ((sublists ,form-arg) ,@bindings-1)
               (tagbody
                 next-loop
                   ,@(unless (logbitp 3 mode)
                       `((when ,test-form  (go up-up-and-out))) )
                   ,form
                 up-and-out
                   ,@(when (logbitp 3 mode)
                       `((when ,test-form (go up-up-and-out))) )
                   (go next-loop)
                 up-up-and-out ) ))

          (when (formatter-context-indirect-p context)
            (setq form `(catch 'up-up-and-out ,form)) )

          form ) ) )

    (2  ; ~@{str~}
      (setf (formatter-context-arg-ctrl-p context) t)

      (let (bindings-1 exit-forms form)

        ;; Iteration test
        (multiple-value-bind (bindings test-form)
            (formatter-parse-loop-frame mode form-n)
          (setq bindings-1 bindings)
          (setq exit-forms `((when ,test-form (go up-and-out)))) )

        (formatter-context-let ((go-up-and-out-p nil)
                                (indirect-p      nil)
                                (no-more-args-p  nil)
                                (pprint-p        nil) )
          ;; Iteration form
          (setq form
            `(let* ,bindings-1
               (tagbody
                 next-loop
                   ,@(unless (logbitp 3 mode) exit-forms)
                   ,@(formatter-parse-1 context ctrl-start ctrl-end)
                   ,@(when   (logbitp 3 mode) exit-forms)
                   (go next-loop)
                 up-and-out ) ))

          (when (formatter-context-indirect-p context)
            (setq form `(catch 'up-and-out ,form)) )

          form ) ) )

    (3  ; ~:@{str~}
      (setf (formatter-context-arg-ctrl-p context) t)

      (let (bindings-1 exit-forms
            bindings-2 forms
            form
            (form-arg
              (if (formatter-context-pprint-p context)
                  '(progn (pprint-pop) (pop args) (setq sublists args))
                '(prog1 (pop args) (setq sublists args)) )) )

        ;; Iteration test
        (multiple-value-bind (bindings test-form)
            (formatter-parse-loop-frame mode form-n)
          (setq bindings-1 bindings)
          (setq exit-forms `((when ,test-form (go up-up-and-out)))) )

        (formatter-context-let ((go-up-and-out-p    nil)
                                (go-up-up-and-out-p nil)
                                (indirect-p         nil)
                                (no-more-args-p     nil)
                                (pprint-p           nil)
                                (up-up-and-out-p    t) )

          ;; Iteration body
          (multiple-value-setq (bindings-2 forms)
            (formatter-parse-2 context ctrl-start ctrl-end form-arg) )

          (setq form `(let* ,bindings-2 ,@forms))

          (when (formatter-context-indirect-p context)
            (setq form `(catch 'up-and-out ,form)) )

          ;; Iteration form
          (setq form
            `(let ((sublists args) ,@bindings-1)
               (tagbody
                 next-loop
                   ,@(unless (logbitp 3 mode) exit-forms)
                   ,form
                 up-and-out
                   ,@(when (logbitp 3 mode) exit-forms)
                   (go next-loop)
                 up-up-and-out ) ))

          (when (formatter-context-indirect-p context)
            (setq form `(catch 'up-up-and-out ,form)) )

          form ) ) )) )


;;;; formatter-parse-loop-frame
;;;
;;; Syntax:
;;;   formatter-parse-loop-frame mode
;;;     => bindings, test-form
;;;
;;; Arguments and Values:
;;;   mode  - Encoded flags. An integer 0 thorugh 15.
;;;             bit 0 - colon
;;;             bit 1 - at-sign
;;;             bit 2 - n
;;;             bit 3 - at least once or pre-test.
;
(defun formatter-parse-loop-frame (mode form-n)
    (declare (type (integer 0 15) mode))
  (ecase (logand mode 7)
    ;; ~{...~}
    ;;
    (0  ;#b000  ; (null params)
      (values nil '(null args)) )

    (4  ; #b100  ; params
      (values `((n ,form-n)) '(or (null args) (not (plusp (decf n))))) )

    ;; ~:{...~}
    ;;
    (1  ; #b001  ; (null params)
      (values nil '(null sublists)) )

    (5  ; #b101  ; params
      (values `((n ,form-n)) '(or (null sublists) (not (plusp (decf n))))) )

    ;; ~@{...~}
    ;;
    (2  ; #b010  ; (null params)
      (values nil '(null args)) )

    (6  ; #b110  ; params
      (values `((n ,form-n)) '(or (null args) (not (plusp (decf n))))) )

    ;; ~:@{...~}
    ;;
    (3  ; #b011  ; (null params)
      (values nil '(null sublists)) )

    (7  ; #b111  ; params
      (values `((n ,form-n)) '(or (null sublists) (not (plusp (decf n))))) )) )


;;;; formatter-parse-loop-i
;;;
;;; Syntax:
;;;  formatter-parse-loop-i context modifer mode form-n form-ctrl-str
;;;    => form
;
(defun formatter-parse-loop-i (context mode form-n form-ctrl-str )
  (ecase (logand mode 3)
    (0  ; ~{str~}
      (multiple-value-bind (bindings test-form)
          (formatter-parse-loop-frame mode form-n)
        `(let (,@bindings
               (ctrl-str ,form-ctrl-str)
               (args     ,(formatter-next-arg))
               (up-up-and-out-p
                 ,(formatter-context-up-up-and-out-p context) ))
           (loop
             ,@(unless (logbitp 3 mode)
               `((when ,test-form (return))) )

             (setq args
                (si::format-main
                  stream ctrl-str args up-up-and-out-p sublists ))

            ,@(when (logbitp 3 mode)
              `((when ,test-form (return))) )) ) ) )

    (1  ; ~:{str~}
      (multiple-value-bind (bindings test-form)
          (formatter-parse-loop-frame mode form-n)
        `(let (,@bindings
               (ctrl-str ,form-ctrl-str)
               (sublists ,(formatter-next-arg)) )
           (catch 'up-up-and-out
             (loop
               ,@(unless (logbitp 3 mode)
                   `((when ,test-form (return))) )

               (si::format-main stream ctrl-str (pop sublists) t sublists)

               ,@(when (logbitp 3 mode)
                  `((when ,test-form (return))) ))) ) ) )

    (2  ; ~@{str~}
      (multiple-value-bind (bindings test-form)
          (formatter-parse-loop-frame mode form-n)
        `(let (,@bindings
               (ctrl-str ,form-ctrl-str)
               (up-up-and-out-p
                 ,(formatter-context-up-up-and-out-p context) ))
           (loop
              ,@(unless (logbitp 3 mode)
                `((when ,test-form (return))))

              (setq args
                (si::format-main
                  stream ctrl-str args up-up-and-out-p sublists ))

             ,@(when (logbitp 3 mode)
                    `((when ,test-form (return))) )) ) ) )

    (3  ; ~:@{str~}
      (multiple-value-bind (bindings test-form)
          (formatter-parse-loop-frame mode form-n)
        `(let (,@bindings
               (ctrl-str ,form-ctrl-str)
               (sublists args) )
           (catch 'up-up-and-out
             (loop
               ,@(unless (logbitp 3 mode)
                      `((when ,test-form (return))) )

               (setq args (rest sublists))
               (si::format-main stream ctrl-str (first sublists) t args)
               (setq sublists args)

              ,@(when (logbitp 3 mode)
                 `((when ,test-form (return))) ))) ) ) )) )


;;;; formatter-parse-param
;;;
;;; Syntax:
;;;   formatter-parse-param context ctrl-start ctrl-end
;;;     => stop-char, param, ctrl-index, state
;;;
;
(defun formatter-parse-param (context ctrl-start ctrl-end)
  (let ((ctrl-string (formatter-context-ctrl-string context))
        (ctrl-index  ctrl-start)
        (state       :none)
        (param       nil)
        (sign        0)
        (char        #\=) )
    (loop
      (when (= ctrl-end ctrl-index)
        (formatter-error-unexpected-eof context ctrl-end) )

      (setq char (schar ctrl-string ctrl-index))
      (incf ctrl-index)

      (ecase state
        (:none
          (case char
            ((#\V #\v)
              (setq param (formatter-next-arg context))
              (setq state :param) )

            (#\#
              (let ((marker (list (formatter-context-next-args context))))
                (push marker (formatter-context-argc-args context))
                (setq param marker)
                (setq state :param) ) )

            (#\'
              (when (= ctrl-end ctrl-index)
                 (formatter-error-unexpected-eof context ctrl-end) )
              (setq param (schar ctrl-string ctrl-index))
              (incf ctrl-index)
              (setq state :param) )

            (#\+
              (setq sign  1)
              (setq param 0)
              (setq state :sign) )

            (#\-
              (setq sign  -1)
              (setq param 0)
              (setq state :sign) )

            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
              (setq state :digit)
              (setq param (digit-char-p char 10)) )

            (#\,
              (setq state :comma)
              (return) )

            (otherwise
              (return) )) )

        (:digit
          (let ((digit (digit-char-p char 10)))
            (unless digit (return))
            (setq param (+ digit (* param 10))) ) )

        (:param
          (return) )

        (:sign
          (setq param (digit-char-p char 10))
          (when (null param)
            (the nil (formatter-error-expect-digit context ctrl-index)) )
          (setq param (* sign param))
          (setq state :digit) )))
    (values char param ctrl-index state) ) )


;;;; formatter-skip-clause
;;;
;;; Syntax:
;;;  formatter-skip-clause context delimiter default-p
;;;   => tilde, stop-char, modifier
;;;
;;; Arguments and Values:
;;;   tilde - an index of control points to tilde.
;;;
;;; Side-Effects:
;;;   formatter-context-ctrl-index points to start of next clause.
;
(defun formatter-skip-clause (context close-char open-index)
    (declare (type formatter-context context))
    (declare (type character      close-char))
  (let* ((control    (formatter-context-ctrl-string context))
         (index      (formatter-context-ctrl-index  context))
         (end        (formatter-context-ctrl-end    context))
         (tilde      0) )
    (loop
      ;; Skip until tilde(~).
      (loop
        (when (= end index) (formatter-error-no-close context open-index))
        (when (char= #\~ (schar control index)) (return))
        (incf index) )

      (setq tilde index)

      (incf index)
      (when (= end index) (formatter-error-no-close context open-index))

      (let ((char (schar control index))
            (modifier 0) )
        ;; Parse parameters
        ;;
        (let ((state :none))
          (loop
            (ecase state
              (:none
                (case char
                  ((#\V #\v) (setq state :param))
                  (#\#       (setq state :param))
                  (#\'
                    (incf index)
                    (when (= end index)
                      (formatter-error-no-close context open-index) )
                    (setq state :param) )
                  (#\,)
                  (#\+ (setq state :sign))
                  (#\- (setq state :sign))
                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                    (setq state :digit) )
                  (otherwise (return))) )
              (:digit
                (cond
                  ((char= #\, char)
                    (setq state :none) )
                  ((not (digit-char-p char 10))
                    (return) )) )
              (:sign
                (unless (digit-char-p char 10)
                  (formatter-error-expect-digit context index) )
                (setq state :digit) )
              (:param
                (unless (char= #\, char) (return))
                (setq state :none) ))

            (incf index)
            (when (= end index)
              (formatter-error-no-close context open-index) )
            (setq char (schar control index)) ) )

        ;; Parse modifiers
        ;;
        (loop
          (case char
            (#\: (setq modifier (logior 1 modifier)))
            (#\@ (setq modifier (logior 2 modifier)))
            (otherwise (return)) )
          (incf index)
          (when (= end index)
            (formatter-error-no-close context open-index) )
          (setq char (schar control index)) )

        ;; Dispatch
        ;;
        (incf index)
        (setf (formatter-context-ctrl-index context) index)

        (when (char= close-char char)
          (return (values tilde char modifier)) )

        (case char
          (#\u0028
            (formatter-skip-to-close context #\u0029 index)
            (setf index (formatter-context-ctrl-index context)) )
          (#\<
            (formatter-skip-to-close context #\> index)
            (setf index (formatter-context-ctrl-index context)) )
          (#\u005B
            (formatter-skip-to-close context #\u005D index)
            (setf index (formatter-context-ctrl-index context)) )
          (#\u007B
            (formatter-skip-to-close context #\u007D index)
            (setf index (formatter-context-ctrl-index context)) )
          (#\;
            (when (char= #\u0029 close-char)
               (formatter-error-semicolon-paren context) )
            (when (char= #\u007D close-char)
               (formatter-error-semicolon-brace context) )
            (return (values tilde char modifier)) )) )) ) )


;;;; formatter-skip-to-close
;;;
;;; Description:
;;;  Skips control string until close-char.
;
(defun formatter-skip-to-close (context close-char open-index)
  (loop
    (multiple-value-bind (tilde stop-char)
        (formatter-skip-clause context close-char open-index)
        (declare (ignore tilde))
      (when (char= stop-char close-char) (return)) )) )


;;;; formatter-slashed-symbol
;;;
;;; Note: We always intern symbol for forward reference.
;
(defun formatter-slashed-symbol (context)
  (let* ((start    (formatter-context-ctrl-index context))
         (ctrl-str (formatter-context-ctrl-string context))
         (end
           (position #\/ ctrl-str
                     :start (formatter-context-ctrl-index context)
                     :end   (formatter-context-ctrl-end   context) )) )
    (unless end
      (error 'formatter-error
             :context   context
             :complaint "Missing '/'." ))

    (setf (formatter-context-ctrl-index context) (1+ end))

    (multiple-value-bind (symbol-name package-name)
        (let ((colon (position #\: ctrl-str :start start :end end)))
               (cond
                 ((not colon)
                   (values (subseq ctrl-str start end) "CL-USER") )

                 ((= colon end)
                   (error 'formatter-error
                          :context   context
                          :complaint "Missing symbol name." ) )
                 (t
                   (incf colon)
                   (let ((ncolons 1))
                     (when (char= #\: (schar ctrl-str colon))
                       (incf colon)
                       (incf ncolons) )
                     (values (subseq ctrl-str colon end)
                             (subseq ctrl-str start (- colon ncolons)) ) )) ) )

      (let ((package (find-package package-name)))
        (unless package
          (error 'formatter-error
                 :context   context
                 :complaint "There is no such package: ~S"
                 :arguments (list package-name) ))

        (setq symbol-name (nstring-upcase symbol-name))

        (intern symbol-name package) ) ) ) )


;;;; formatter-subst-args
;;;
;;; Description:
;;;  Substitutes argument references.
;;;
;;; Note: We use variable "args" for checking number of arguments left.
;;;
;
(defun formatter-subst-args (context forms)
    (declare (type formatter-context context))
    (declare (type list              forms))
  (loop
    for  marker in (formatter-context-next-args context)
    with form-arg =
      (cond
        ((and (formatter-context-prev-args context)
              (formatter-context-pprint-p  context) )
          '(progn
             (pprint-pop)
             (when (endp args) (si::compiled-format-error-no-args))
             (setq arg (pop args)) ) )

        ((formatter-context-pprint-p context)
          '(progn
             (pprint-pop)
             (when (endp args) (si::compiled-format-error-no-args))
             (pop args) ) )

        ((formatter-context-prev-args context)
          '(progn
             (when (endp args) (si::compiled-format-error-no-args))
             (setq arg (pop args)) ) )

        (t
          '(progn
             (when (endp args) (si::compiled-format-error-no-args))
             (pop args) ) ))

    do (nsubst form-arg marker forms) )

  (loop
    for  marker in (formatter-context-prev-args context)
    do (nsubst 'arg marker forms) )

  (loop
    for  marker in (formatter-context-argc-args context)
    do (nsubst '(length args) marker forms) )

  forms )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Formatters
;;;;


;;;; 22.3.9.3  Tilde Newline: Ignored Newline
;;;
;;; ~newline    = ignores newline and any following non-newline whitespace.
;;; ~:newline   = ignores newline.
;;; ~@newline   = emits newline, and ignores following any whitespace.
;
(define-formatter #\Newline (:colon :at-sign) ()
  (let ((ctrl-string (formatter-context-ctrl-string context))
        (ctrl-end    (formatter-context-ctrl-end    context))
        (ctrl-index  (formatter-context-ctrl-index  context)) )
      (declare (type simple-string  ctrl-string))
      (declare (type ext:sequence-index ctrl-end))
      (declare (type ext:sequence-index ctrl-index))
  (ecase modifier
    (0  ; ~newline
      (loop when (= ctrl-end ctrl-index) do (return)
            do (let ((char (schar ctrl-string ctrl-index)))
                 (when (or (char= #\Newline char)
                           (not (si::whitespace-char-p char)) )
                   (return) ) )
               (incf ctrl-index) )
      (setf (formatter-context-ctrl-index context) ctrl-index) )
    (1  ; ~:newline
     )
    (2  ; ~@newline
      (add-stream-form '(write-char #\Newline stream))
      (loop when (= ctrl-end ctrl-index) do (return)
            unless (si::whitespace-char-p (schar ctrl-string ctrl-index))
                do (return)
            do (incf ctrl-index) )
      (setf (formatter-context-ctrl-index context) ctrl-index) )) ) )


;;;; 22.3.3.4  Tilde Dollarsign: Monetary Floating-Point
;
(define-formatter #\$ (*) ((d 2) (n 1) (w 0) (padchar #\Space))
  (check-param-type d       integer)
  (check-param-type n       integer)
  (check-param-type w       integer)
  (check-param-type padchar character)

  (setf (formatter-context-use-stream-p context) t)
  (add-stream-form
    `(si::format-float-$ stream ,(next-arg) ,modifier
                         ,d ,n ,w ,padchar )) )


;;;; 22.3.1.2 Tilde Percent: Newline
;;;; 22.3.1.4 Tilde Vertical-Bar: Page
;;;; 22.3.1.5 Tilde Tilde Tilde
;
(macrolet (
  (define-formatter-basic (char out-char)
    `(define-formatter ,char () ((n 1))
       (check-param-type n integer)
       (cond
          ((not (integerp n))
            (add-stream-form `(si::write-chars ,,out-char ,n stream)) )
          ((= 1 n)
            ,(if (char= #\Newline out-char)
                 '(add-stream-form '(terpri stream))
              `(add-stream-form '(write-char ,out-char stream)) ) )
          ((plusp n)
            (add-stream-form `(si::write-chars ,,out-char ,n stream)) )) ) ) 
  )
  (define-formatter-basic #\% #\Newline)
  (define-formatter-basic #\| #\Page)
  (define-formatter-basic #\~ #\~)
 ) ; macrolet


;;;; 22.3.1.3 Tilde Ampersan: Fresh-Line
;
(define-formatter #\& () ((n 1))
  (check-param-type n integer)
  (cond
    ((not (integerp n))
      (add-stream-form `(si::format-fresh-line stream ,n)) )
    ((= 1 n)
      (add-stream-form `(fresh-line stream)) )
    ((plusp n)
      (add-stream-form `(si::format-fresh-line stream ,n)) )) )


;;;; 22.3.8.1  Tilde Left-Paren: Case Conversion
;;;
;;; Variations:
;;;   ~(str~)     - downcase
;;;   ~:(str~)    - capitalize all words
;;;   ~@(str~)    - capitalize just the first word
;;;   ~:@(str~)   - upcase
;;;
;;; Note:
;;;  When case conversions appear nested, the outer most conversion dominates.
;;;  In other words, inner conversions doesn't affect the final output.
;;;
;;; BUGBUG: ~( ~^ ~)
;
(define-formatter #\u0028 (*) ()
  (let* ((clause-start
           (formatter-context-ctrl-index context) )
         (clause-end
           (formatter-skip-clause context #\u0029 clause-start) )
         (form (formatter-parse-1 context clause-start clause-end))
         (fn-conv
           (ecase modifier
             (0 'nstring-downcase)
             (1 'nstring-capitalize)
             (2 'nstring-capitalize-1)
             (3 'nstring-upcase) ) ))

    (setq form
      (if (or (formatter-context-indirect-p context)
              (formatter-context-go-up-and-out-p context) )
          `(let ((target stream)
                 (stream (make-string-output-stream)) )
             (unwind-protect
                ,form
               (write-string (,fn-conv (get-output-stream-string stream))
                             target )) )
        `(,fn-conv (with-output-to-string (stream) ,form)) ))

    (add-stream-form form) ) )


;;;; 22.3.8.2  Tilde Right-Paren: End of Case Conversion
;;;; 22.3.6.3  Tilde Greater-Than-Sign: End of Justification
;;;; 22.3.7.3  Tilde Right-Bracket: End of Conditional Expression
;;;; 22.3.7.5  Tilde Right-Brace: End of Iteration
;
(labels (
  (error-mismatched (context char)
    (error 'formatter-error
           :context   context
           :complaint "Mismatched ~~~C."
           :arguments (list char) ) )
  )
  ;;
  (define-formatter #\u0029 () () (error-mismatched context #\u0029))
  (define-formatter #\>     () () (error-mismatched context #\>))
  (define-formatter #\u005D () () (error-mismatched context #\u005D))
  (define-formatter #\u007D () () (error-mismatched context #\u007D))
 ) ; lables


;;;; 22.3.7.1 Tilde Asterisk: Go-To
;;;
;;; Variations:
;;;   ~n*     skip next args
;;;   ~n:*    backup previous args
;;;   ~n@*    go to nth arg
;;;
;;; Note:
;;;  We could optimize ~* when control-string doesn't include control-flow,
;;;  e.g. "~A ~:* ~D".
;
(define-formatter #\* (:colon :at-sign) ((n (if (eql modifier 2) 0 1)))
  (setf (formatter-context-arg-goto-p context) t)

  (check-param-type n integer)

  (add-form
    (ecase modifier
      ((0)  ; ~* = next
        `(loop repeat ,n do
           (when (null args) (si::compiled-format-error-no-args)
           (pop args) )) )
      ((1)  ; ~:* = back
        `(let ((nth
                 (loop for scan on all-args
                       and nth = 0 then (1+ nth)
                       until (eq args scan)
                       finally (return nth) ) ))
          (decf nth ,n)
          (if (minusp nth)
              (setq args nil)
            (setq args (nthcdr nth all-args)) )
          (when (null args) (si::compiled-format-error-no-args)) ) )
      ((2)  ; ~@* = absolute
        `(progn
           (setq args (nthcdr ,n all-args))
           (when (null args) (si::compiled-format-error-no-args)) ) ))) )


;;;; 22.3.5.4 Tilde Slash: Call Function
;
(define-formatter #\/ (*) (*)
  (let ((symbol (formatter-slashed-symbol context)))
    (add-stream-form
      `(,symbol stream
                ,(next-arg)
                ,(logbitp 0 modifier)   ; colon
                ,(logbitp 1 modifier)   ; at-sign
                ,@params )) ) )


;;;; 22.3.9.1  Tilde Semicolon: Clause Separator
;
(define-formatter #\; () ()
  (error 'formatter-error
         :context   context
         :complaint "Use ~~; out side of bracket." ) )


;;;; 22.3.6.2 Tilde Less-Than-Sign: Justification
;;;
;;; Variations:
;;;   ~< left ~; ... ~>
;;;   ~:<left ~; ... ~>         Insert spaces before the first text.
;;;   ~@<left ~; ... ~>         Insert spaces after the last text.
;;;   ~< overflow ~n,w:; ...>   Overflow text
;;;
;;;; 22.3.5.2 Tilde Less-Than-Sign: Logical Block
;
(define-formatter #\< (*) ((mincol 0)
                           (colinc 1)
                           (minpad 0)
                           (padchar #\Space) )
  (check-param-type mincol  integer)
  (check-param-type colinc  integer)
  (check-param-type minpad  integer)
  (check-param-type padchar character)

  (let ((clause1-modifier 0)
        (clauses          '())  ; list of (cons start end)
        (close-modifier   0) )
      (declare (type (integer 0 3)  clause1-modifier))
      (declare (type list           clauses))
      (declare (type (integer 0 3)  close-modifier))

    ;; Looking for Right angle bracket
    ;;
    (let* ((clause1-start (formatter-context-ctrl-index context))
           (clause-start  clause1-start) )
      (loop
        (multiple-value-bind (tilde stop-char modifier)
            (formatter-skip-clause context #\> clause1-start)

          (push (cons clause-start tilde) clauses)

          (when (char= #\> stop-char)
            (setq close-modifier modifier)
            (return) )

          (when (null (rest clauses))
            (setq clause1-modifier modifier) )

          (setq clause-start (formatter-context-ctrl-index context)) ))
      (setq clauses (nreverse clauses)) )

    ;; Process segments
    ;;
    (if (logbitp 0 close-modifier)
        (progn
          ;; ~< prefix ~; body ~; suffix ~:>
          ;;
          (when params
            (formatter-error-too-many-params context) )

          (when (logbitp 0 clause1-modifier)
            (setf (formatter-context-ctrl-index context)
                  (car (second clauses)) )
            (formatter-error-invalid-modifier context #\; 2) )

          (add-stream-form
            (formatter-parse-logical-block
              context
              (if (logbitp 1 modifier) 'args (next-arg))
              modifier
              clauses
              (logbitp 1 clause1-modifier)       ; ~< prefix ~@; ... ~:>
              (logbitp 1 close-modifier) )) )    ; ~< ... ~:@>
      (progn
        ;; ~< left ~; text ... ~; right ~>
        ;; ~< overflow ~:; left ~; text ... ~; right ~>
        (check-param-type mincol  integer)
        (check-param-type colinc  integer)
        (check-param-type minpad  integer)
        (check-param-type padchar character)

        (when (logbitp 1 clause1-modifier)
          (formatter-error-invalid-modifier context #\; 1) )

        ;; Process clauses == Forms for producing strings.
        ;;
        (let ((overflow-p (logbitp 0 clause1-modifier))
              (spare      0)
              (width      0)
              (bindings  '())
              forms )

          (formatter-context-let ((go-up-and-out-p nil)
                                  (indirect-p nil) )
            (setq forms
              (loop
                for (ctrl-start . ctrl-end) in clauses
                for first-p = overflow-p then nil
                collect
                  (let ((forms (formatter-parse-1 context
                                                  ctrl-start ctrl-end ) ))
                    `(with-output-to-string (stream) ,@forms) )
                when first-p do
                  (let ((nparams 0)
                        (ctrl-start (1+ (cdr (first  clauses))))
                        (ctrl-end   (car (second clauses))) )
                    (loop
                      (multiple-value-bind (stop-char param ctrl-index state)
                            (formatter-parse-param context ctrl-start ctrl-end)
                        (unless (eq :none state)
                          (incf nparams)
                          (case nparams
                            (1 (setq spare param))
                            (2 (setq width param))
                            (otherwise
                              (setf (formatter-context-ctrl-index context)
                                    ctrl-index )
                              (formatter-error-too-many-params context) )))
                        (unless (char= #\, stop-char) (return))
                        (setq ctrl-start ctrl-index) )) )))

            ;; Make segments collect forms
            (cond
              ((null forms))
              ((null (rest forms))
                (setq bindings `((segments (list ,(pop forms))))) )
              (t
                (setq bindings
                    `((segments (list ,(pop forms)))
                      (last segments) ))
                (loop for scan on (rest forms) do
                  (setf (first scan)
                    `(setq last (setf (cdr last) (list ,(first scan)))) ))))

            ;; "~^" in ~< ... ~>
            (when (formatter-context-go-up-and-out-p context)
              (setq forms `((tagbody ,@forms up-and-out))) )

            (when (formatter-context-indirect-p context)
              (setq forms `((catch 'up-and-out ,@forms))) )

            (add-stream-form
              `(let* ,bindings
                  ,@forms
                  (si::format-justify
                    stream
                    ,(if overflow-p '(rest  segments) 'segments)
                    ,(if overflow-p '(first segments) nil)
                    ,spare ,width
                    ,modifier ,mincol ,colinc ,minpad ,padchar ) )) ) ))) ) )


;;;; 22.3.7.6  Tilde Question-Mark: Recursive Processing
;;;
;;; Variations:
;;;   ~?    two arguments for cotnrol-string and arguments list.
;;;   ~@?   one arguments for control-string.
;;;
;;; Note: Indirect control-string can have ~^ and ~:^
;
(define-formatter #\? (:at-sign) ()
  (setf (formatter-context-indirect-p context) t)

  (let ((sublist-args '()))

    (when (formatter-context-sublist-loop-p context)
      (setq sublist-args (list t 'sublists)) )

    (ecase modifier
      (0  ; ~?
        (add-stream-form
          `(si::format-main stream ,(next-arg) ,(next-arg) ,@sublist-args) ) )

      (2  ; ~@?
        (add-stream-form
          `(setq args (si::format-main stream ,(next-arg) args ,@sublist-args)) )

        (setf (formatter-context-arg-ctrl-p context) t) )) ) )


;;;; 22.3.4.1 Tilde A: Aesthetic
;;;
;;; ~:A     nil => ()
;;; ~@A     right align
;
(define-formatter #\A (*) ((mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  (check-param-type mincol  integer)
  (check-param-type colinc  integer)
  (check-param-type minpad  integer)
  (check-param-type padchar character)

  (let ((var-arg (next-arg)))
    (if (and (not (logbitp 0 modifier)) (integerp mincol) (<= mincol 1))
        (add-stream-form `(princ ,var-arg stream))
      (add-stream-form
         `(si::format-princ stream ,var-arg
                           ,modifier ,mincol ,colinc ,minpad ,padchar ))) ) )


;;;; 22.3.2.3 Tilde B: Binary
;;;; 22.3.2.2 Tilde D: Decimal
;;;; 22.3.2.4 Tilde O: Octal
;;;; 22.3.2.5 Tilde X: Hexadecimal
;
(macrolet (
  (define-formatter-radix-control (char base)
    `(define-formatter ,char (*) ((mincol         0)
                                  (padchar        #\Space)
                                  (commachar      #\,)
                                  (comma-interval 3) )
       (check-param-type mincol         integer)
       (check-param-type padchar        character)
       (check-param-type commachar      character)
       (check-param-type comma-interval integer)

       (add-stream-form
         `(si::format-integer stream ,(next-arg)
                              ,modifier ,',base ,mincol ,padchar
                              ,commachar ,comma-interval )) ) )
  )
  ;;
  (define-formatter-radix-control #\B  2)
  (define-formatter-radix-control #\D 10)
  (define-formatter-radix-control #\O  8)
  (define-formatter-radix-control #\X 16)
 ) ; macrolet


;;;; 22.3.1.1 Tilde C: Character
;
(define-formatter #\C (*) ()
  (add-stream-form `(si::format-character stream ,(next-arg) ,modifier)) )


;;;; 22.3.3.2  Tilde E: Exponential Floating-Point
;;;; 22.3.3.1  Tilde F: Fixed-Format Floating-Point
;;;; 22.3.3.3  Tilde G: General Floating-Point
;
(macrolet (
  (define-formatter-float (fmtchar)
    (let ((fn      (intern (format nil "FORMAT-FLOAT-~C" fmtchar)
                           (find-package :si) ) )
          (params  '((w nil) (d nil) (e nil) (k 0)
                     (ovfchar nil) (padchar #\Space)
                     (exptchar nil) ))
          (args    '(w d e k ovfchar padchar exptchar)) )

      (when (char= #\F fmtchar)
        (setq params (copy-list params))
        (setq params (delete 'e params        :key #'car))
        (setq params (delete 'exptchar params :key #'car))

        (setq args (copy-list args))
        (setq args (delete 'e args))
        (setq args (delete 'exptchar args)) )

      `(define-formatter ,fmtchar (*) ,params
          (check-param-type w       integer)
          (check-param-type d       integer)
          (check-param-type k       integer)
          (check-param-type ovfchar character)

          ,@(when (member 'e params :test #'eq)
             `((check-param-type e        integer)
               (check-param-type exptchar character) ))

          (add-stream-form
            `(,',fn stream ,(next-arg)
                    ,(logbitp 2 modifier)
                    ,@(list ,@args) )) ) ) )
  )
  ;;
  (define-formatter-float #\E)
  (define-formatter-float #\F)
  (define-formatter-float #\G)
 ) ;macrolet


;;;; 22.3.5.3 Tilde I: Indent
;;;
;;; ~I      pprint-indent :block
;;; ~:I     pprint-indnet :current
;
(define-formatter #\I (:colon) ((n 0))
  (add-stream-form
    `(pprint-indent ,(ecase modifier
                       (0 :block)       ; ~I
                       (1 :current) )   ; ~:I
                    ,n
                    stream )) )


;;;; 22.3.8.3 Tilde P: Plural
;;;
;;;  ~P     "s"
;;;  ~:P    ~:*, "s"
;;;  ~@P    "y" or "ies"
;;;  ~:@P   ~:*, "y" or "ies"
;
(define-formatter #\P (*) ()
  (ecase modifier
    (0  ; ~P
      (add-stream-form `(unless (eql 1 ,(next-arg)) (write-char #\s stream))) )
    (1  ; ~:P
      (add-stream-form `(unless (eql 1 ,(prev-arg)) (write-char #\s stream))) )
    (2  ; ~@P
      (add-stream-form
        `(if (eql 1 ,(next-arg))
             (write-char #\y stream)
           (write-string "ies" stream) ) ) )
    (3  ; ~:@P
      (add-stream-form
        `(if (eql 1 ,(prev-arg))
             (write-char #\y stream)
           (write-string "ies" stream) )) )) )



;;;; 22.3.2.1 Tilde R: Radix
;;;
;;; ~R      cardinal
;;; ~:R     ordinal
;;; ~@R     roman
;;; ~:@R    old-roman
;
(define-formatter #\R (*) ((base 10)
                           (mincol 0)
                           (padchar #\Space)
                           (commachar #\,)
                           (comma-interval 3) )
  (let ((arg (next-arg)))
    (if (null params)
        (add-stream-form
              (ecase modifier
                (0 `(si::format-cardinal  stream ,arg))     ; ~R
                (1 `(si::format-ordinal   stream ,arg))     ; ~:R
                (2 `(si::format-roman     stream ,arg))     ; ~@R
                (3 `(si::format-old-roman stream ,arg)) ))  ; ~:@R
      (progn
         (check-param-type base           integer)
         (check-param-type mincol         integer)
         (check-param-type padchar        character)
         (check-param-type commachar      character)
         (check-param-type comma-interval integer)

         (add-stream-form
           `(si::format-integer stream ,arg
                                ,modifier ,base ,mincol ,padchar
                                ,commachar ,comma-interval )))) ) )


;;;; 22.3.4.2 Tilde S: Aesthetic
;;;
;;; ~:S     nil => ()
;;; ~@S     right align
;
(define-formatter #\S (*) ((mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  (check-param-type mincol  integer)
  (check-param-type colinc  integer)
  (check-param-type minpad  integer)
  (check-param-type padchar character)

  (let ((arg (next-arg)))
    (if (and (not (logbitp 0 modifier)) (<= mincol 1))
        (add-stream-form `(princ ,arg stream))
      (add-stream-form
         `(si::format-prin1 stream ,arg
                        ,modifier ,mincol ,colinc ,minpad ,padchar ))) ) )


;;;; 22.3.6.1 Tilde T: Tabulate
;
(define-formatter #\T (*) ((n 1) (m 1))
  (ecase modifier
    (0  ; ~column,colincT
      (add-stream-form `(si::format-absolute-tab stream ,n ,m)) )
    (1  ; ~n,m:T
      (add-stream-form `(pprint-tab :section ,n ,m stream)) )
    (2  ; ~colrel,colinc@T
      (add-stream-form `(si::format-relative-tab stream ,n ,m)) )
    (3  ; ~n,m:@T
      (add-stream-form `(pprint-tab :section-relative ,n ,m stream)) )) )


;;;; 22.3.4.3 Tilde W: Write
;;;
;;; ~W      write-object
;;; ~:W     *print-pretty* <= t
;;; ~@W     *print-level* <= nil, *print-length* <= nil
;;; ~:@W    *print-level* <= nil, *print-length* <= nil, prtty-print* <= t
;
(define-formatter #\W (*) ()
  (let ((arg (next-arg)))
    (ecase modifier
      (0    ; ~W
        (add-stream-form `(si::write-object ,arg stream)) )
      (1    ; ~:W
        (add-stream-form
          `(let ((*print-pretty* t))
             (si::write-object ,arg stream) )) )
      (2    ; ~@W
        (add-stream-form
          `(let ((*print-level*  nil)
                 (*print-length* nil) )
              (si::write-object ,arg stream) )) )
      (3    ; ~:@W
        (add-stream-form
          `(let ((*print-level*  nil)
                 (*print-length* nil)
                 (*print-pretty* nil) )
              (si::write-object ,arg stream) )) )) ) )

;;;; 22.3.7.2  Tilde Left-Bracket: Conditional Expression
;;;
;;; Variations:
;;;   ~n[str0~;str1~;...~;strn~]
;;;   ~n[str0~;str1~;...~:;default~]
;;;   ~:[alternative~;consequent~]
;;;   ~@[consequent~]
;;;
;
(define-formatter #\u005B (:colon :at-sign) ((n 0))
  (ecase modifier
    (0  ; ~n[str0~;str1~;...~;strn~]
      (when (null params) (setq n (next-arg)))

      (let ((case-clauses  '())
            (args-so-far   (formatter-context-next-args context))
            (open-index    (formatter-context-ctrl-index context))
            (last-start    (formatter-context-ctrl-index context))
            (last-modifier 0)
            (clauses       '()) )

        ;; Collect clauses
        ;;
        (loop
          (multiple-value-bind (tilde stop-char modifier)
              (formatter-skip-clause context #\u005D open-index)
            (push (cons last-start tilde) clauses)

            (when (char= #\u005D stop-char)
              (unless (zerop modifier)
                (formatter-error-invalid-modifier context stop-char 0) )
              (return) )

            (when (logbitp 1 modifier)
              (formatter-error-invalid-modifier context stop-char 1) )

            (setq last-start   (formatter-context-ctrl-index context))
            (setq last-modifier modifier) ))
        (setq clauses (nreverse clauses))

        ;; Make case clauses
        ;;
        (setq case-clauses
          (loop for (ctrl-start . ctrl-end) in clauses
                for nth = 0 then (1+ nth)
             collect
               (cons nth (formatter-parse-1 context ctrl-start ctrl-end)) ))

        (when (logbitp 0 last-modifier)
          (setf (caar (last case-clauses)) 'otherwise) )

        (unless (eq args-so-far (formatter-context-next-args context))
          (setf (formatter-context-arg-ctrl-p context) t) )

        (add-form `(case ,n ,@case-clauses)) ) )

    (1 ; ~:[alternative~;consequent~]
      (let* ((alt-start (formatter-context-ctrl-index context))
             (alt-end
               (multiple-value-bind (tilde stop-char)
                   (formatter-skip-clause context #\u005D alt-start)
                 (when (char= #\u005D stop-char)
                   (formatter-error-clause-count context 2) )
                 tilde ) )
             (con-start (formatter-context-ctrl-index context))
             (con-end
               (multiple-value-bind (tilde stop-char)
                   (formatter-skip-clause context #\u005D alt-start)
                 (unless (char= #\u005D stop-char)
                   (formatter-error-clause-count context 2) )
                 tilde ) )

            ;; For parseing
            (test-form     (next-arg))
            (args-so-far   (formatter-context-next-args context)) )

        (add-form
          `(if (not ,test-form)
               (progn ,@(formatter-parse-1 context alt-start alt-end))
             (progn ,@(formatter-parse-1 context con-start con-end)) ))

        (unless (eq args-so-far (formatter-context-next-args context))
          (setf (formatter-context-arg-ctrl-p context) t) ) ) )

    (2  ; ~@[consequent~]
      (setf (formatter-context-arg-ctrl-p context) t)

      (let*  ((con-start (formatter-context-ctrl-index context))
              (con-end
                (multiple-value-bind (tilde stop-char)
                    (formatter-skip-clause context #\u005D con-start)
                  (unless (char= #\u005D stop-char)
                    (formatter-error-clause-count context 1) )
                  tilde ) ) )
        (add-form
          `(let ((save-args args))
              (when ,(next-arg)
                (setq args save-args)
                ,@(formatter-parse-1 context con-start con-end) ) )) ) )) )


;;;; 22.3.9.2  Tilde Circumflex: Escape Upward
;;;
;;; ~^      up-and-out
;;; ~:^     up-up-and-out
;
(define-formatter #\^ (:colon) ((param-1 0) (param-2 0) (param-3 0))
  (when (and (logbitp 0 modifier)
             (not (formatter-context-sublist-loop-p context)) )
    (error 'formatter-error
           :context   context
           :complaint "~~:^ is used out side of ~~:{...~~}." ))

  (if (and (formatter-context-pprint-p context)
           (null params)
           (zerop modifier) )
      (add-form '(pprint-exit-if-list-exhausted))
    (let ((form-test
            (ecase (length params)
              (0 (if (not (logbitp 0 modifier))
                     '(null args)
                   '(null sublists) ) )
              (1 `(zerop ,param-1))
              (2 `(= ,param-1 ,param-2))
              (3 `(<= ,param-1 ,param-2 ,param-3)) ) )
          (form-throw
            (if (logbitp 0 modifier)
                (progn
                  (setf (formatter-context-go-up-up-and-out-p context) t)
                    '(go up-up-and-out) )
            (progn
              (setf (formatter-context-go-up-and-out-p context) t)
              '(go up-and-out) )) ))
      (add-form `(when ,form-test ,form-throw)) )) )


;;;; 22.3.5.1. Tilde Underscore: Conditional Newline
;;;
;;;   ~_    pprint-newline :linear
;;;   ~:_   pprint-newline :fill
;;;   ~@_   pprint-newline :miser
;;;   ~:@_  pprint-newline :mandatory
;
(define-formatter #\_ (*) ()
  (add-stream-form
    `(pprint-newline ,(ecase modifier
                        (0 :linear)         ; ~_
                        (1 :fill)           ; ~:_
                        (2 :miser)          ; ~@_
                        (3 :mandatory) )    ; ~:@
                     stream )) )


;;;; 22.3.7.4  Tilde Left-Brace: Iteration
;;;
;;; Variations:
;;;
;;;   ~{str~}  = loop in list argument.
;;;   ~:{str~} = loop in sublist argument.
;;;   ~@{str~} = loop in rest of arguments.
;;;   ~:{str~} = loop in sublists of rest of arguments.
;;;
;;;   ~{~}     = loop indirect
;;;   ~{...~:} = loop at least once.
;;;
;
(define-formatter #\u007B (*) ((n nil))
  (let* ((ctrl-start (formatter-context-ctrl-index context))
         (ctrl-end   ctrl-start)
         (mode       modifier) )
           (declare (type ext:sequence-index ctrl-start))
           (declare (type ext:sequence-index ctrl-end))
           (declare (type (integer 0 15)  mode))

    ;; Looking for end position and checking colon modifier.
    ;;
    (multiple-value-bind (tilde stop-char last-modifier)
        (formatter-skip-clause context #\u007D ctrl-start)
        (declare (ignore stop-char))
      (setq ctrl-end tilde)

      (when params
        (setq mode (logior mode 4)) )

      (when (logbitp 0 last-modifier)
        (setq mode (logior mode 8)) )

      (if (eq ctrl-start ctrl-end)
          (progn
            ;; Indirect loop: ~{~}
            ;;
            (add-form
              (formatter-parse-loop-i context mode n (next-arg)) ))

        ;; Direct Loop
        ;;
        (add-form
          (formatter-parse-loop-d context mode n ctrl-start ctrl-end) )) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Macros
;;;;

;;;; 24.2.2 formatter
;;;
;;; Expansion:
;;;  #'(lambda (&optional arg-0 arg-2 .. (arg-n nil arg-ok-p) &rest args)
;;;      (catch 'up-and-out
;;;         ... )
;;;      args )
;
(defmacro cl:formatter (control-string)
    (check-type control-string string)
  (let ((context (make-formatter-context)))

    ;; Initialize context
    ;;
    (multiple-value-bind (string start end)
        (vector-data control-string)
      (setf (formatter-context-ctrl-string context) string)
      (setf (formatter-context-ctrl-start  context) start)
      (setf (formatter-context-ctrl-end    context) end)
      (setf (formatter-context-ctrl-index  context) start) )

    (formatter-parse context
                    (formatter-context-ctrl-start context)
                    (formatter-context-ctrl-end   context) )

    (setf (formatter-context-next-args context)
          (nreverse (formatter-context-next-args context)) )

    (setf (formatter-context-prev-args context)
          (nreverse (formatter-context-prev-args context)) )

    (setf (formatter-context-argc-args context)
          (nreverse (formatter-context-argc-args context)) )

    (setf (formatter-context-forms context)
          (nreverse (formatter-context-forms context)) )

    ;; Emit code
    ;;
    (let ((forms    (formatter-context-forms context))
          (bindings '())
          (reqs     '()) )

      (cond
        ((formatter-context-arg-goto-p context)
          ;; Formatter function accesses arguments in arbitrary order.
          (setq forms (formatter-subst-args context forms))

          (if (formatter-context-prev-args context)
              (setq bindings '((all-args args) arg))
            (setq bindings '((all-args args)))) )

        ((formatter-context-arg-ctrl-p context)
          ;; Formatter function consumes arbitrary number of arguments in
          ;; control-flow.
          (setq forms (formatter-subst-args context forms))

          (when (formatter-context-prev-args context)
            (setq bindings '(arg)) ) )

        (t
          ;; Formatter function consumes fixed number of arguments.
          (setq reqs
            (loop for  marker in (formatter-context-next-args context)
                  for  nth = 0 then (1+ nth)
                  with param = nil
              do
                (setq param (intern (format nil "ARG-~D" nth)
                                    #.(find-package :xc) ))

                (setf (car marker) param)
                (nsubst param marker forms)
              collect param ))

          (loop for marker in (formatter-context-prev-args context)
            do (nsubst (car marker) marker forms) )

          (loop for marker in (formatter-context-argc-args context)
            do (nsubst `(+ (length args) ,(length (car marker)))
                        marker
                        forms )) ))

      (when bindings
        (setq forms `((let* ,bindings ,@forms))) )

      (when reqs
        (push '&optional reqs)
        (setf (car (last reqs)) `(,(car (last reqs)) nil argc-ok-p))
        (push '(unless argc-ok-p (si::compiled-format-error-no-args)) forms) )

      ;; When control string has toplevel "~^".
      (when (formatter-context-go-up-and-out-p context)
        (setq forms `((tagbody ,@forms up-and-out))) )

      (unless (formatter-context-use-stream-p context)
        (push `(declare (ignore stream)) forms) )

      `#'(lambda (stream ,@reqs &rest args)
          ,@forms
          args ) ) ) )
