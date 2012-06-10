;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Runtime - 22 Printer - Pool
;;; runtime/r22-pool.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r22-defs-format.lisp#5 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;
;
(in-package :si)

; When we dont't want to use US English, we use 10^10.
;(defconstant most-positive-english-number +999999999)
;(defconstant most-negative-english-number -999999999)

(defconstant most-positive-english-number (1- (expt +10 63)))

;; BUGBUG: compiler must make most-positive-english-number available at
;; compilation time.
(defconstant most-negative-english-number (- most-positive-english-number))


;;;;*english-cardinal-expt-1000*
;;;
;;; Used by:
;;;   format-cardinal, format-oridnal
;;;
;;; Description:
;;;  Contains vector of US English of 10^(3*n).
;
(defconstant *english-cardinal-expt-1000*
  '#("thousand"                 ; 10^3
     "million"                  ; 10^6
     "billion"                  ; 10^9
     "trillion"                 ; 10^12
     "quadrillion"              ; 10^15
     "quintillion"              ; 10^18
     "sextillion"               ; 10^21
     "septillion"               ; 10^24
     "octillion"                ; 10^27
     "nonillion"                ; 10^30
     "decilinon"                ; 10^33
     "undecillion"              ; 10^36
     "duodecillion"             ; 10^39
     "tredecillion"             ; 10^42
     "quattuordecillion"        ; 10^45
     "quindecillion"            ; 10^48
     "sexdecillion"             ; 10^51
     "septendecillion"          ; 10^54
     "octodecillion"            ; 10^57
     "novemdecillion"           ; 10^60
     "vigintillion"             ; 10^63
     ) )


;;;; *english-cardinal-20-90*
;;;
;;; Used by:
;;;   format-cardinal, format-oridnal
;;;
;;; Description:
;;;  Contains vector of English word 20 through 90.
;
(defconstant *english-cardinal-20-90*
  '#(
     "twenty"   ; 20
     "thirty"   ; 30
     "forty"    ; 40
     "fifty"    ; 50
     "sixty"    ; 60
     "seventy"  ; 70
     "eighty"   ; 80
     "ninety"   ; 90
     ) )


;;;; *english-ordinal-20-90*
;;;
;;; Used by:
;;;   format-cardinal, format-oridnal
;
(defconstant *english-ordinal-20-90*
  '#(
     "twentieth"        ; 20
     "thirtieth"        ; 30
     "fortieth"         ; 40
     "fiftieth"         ; 50
     "sixtieth"         ; 60
     "seventieth"       ; 70
     "eightieth"        ; 80
     "ninetieth"        ; 90
     ) )


;;;; *english-cardinal-0-19*
;;;
;;; Used by:
;;;   format-cardinal, format-oridnal
;
(defconstant *english-cardinal-0-19*
 '#(
    "zero"              ; 0
    "one"               ; 1
    "two"               ; 2
    "three"             ; 3
    "four"              ; 4
    "five"              ; 5
    "six"               ; 6
    "seven"             ; 7
    "eight"             ; 8
    "nine"              ; 9
    "ten"               ; 10
    "eleven"            ; 11
    "twelve"            ; 12
    "thirteen"          ; 13
    "fourteen"          ; 14
    "fifteen"           ; 15
    "sixteen"           ; 16
    "seventeen"         ; 17
    "eighteen"          ; 18
    "nineteen"          ; 19
    ) )


;;;;; *english-ordinal-0-19*
;;;
;;; Used by:
;;;   format-cardinal, format-oridnal
;
(defconstant *english-ordinal-0-19*
 '#(
    "zeroth"            ; 0
    "first"             ; 1
    "second"            ; 2
    "third"             ; 3
    "fourth"            ; 4
    "fifth"             ; 5
    "sixth"             ; 6
    "seventh"           ; 7
    "eighth"            ; 8
    "ninth"             ; 9
    "tenth"             ; 10
    "eleventh"          ; 11
    "twelfth"           ; 12
    "thirteenth"        ; 13
    "fourteenth"        ; 14
    "fifteenth"         ; 15
    "sixteenth"         ; 16
    "seventeenth"       ; 17
    "eighteenth"        ; 18
    "nineteenth"        ; 19
    ) )


;;;; format-context
;;;
;;; Slots:
;;;  modifier   0=none, 1=colon, 2=at-sign, 3=colon and at-sign
;;;  out-scan   A list of format argument of ~:{...~}.
;;;
;
(defstruct (format-context
             (:print-object   print-structure-unreadably)
             (:constructor    nil)
             (:copier         nil)
             (:predicate      nil) )
  ;; format-control
  (control-string   ""  :type simple-string)
  (control-start    0   :type ext:sequence-index)
  (control-end      0   :type ext:sequence-index)
  (control-index    0   :type ext:sequence-index)
  (control-start-0  0   :type ext:sequence-index)
  (control-end-0    0   :type ext:sequence-index)

  ;; format-arguments
  (arg-list         '() :type list)
  (arg-scan         '() :type list)
  (pprint-pop       nil :type (or null function))

  ;; sublist iteration
  (outer-scan       '())
  (up-up-and-out-p  nil) ; generalize-boolean

  ;; directive
  (modifier         0     :type (integer 0 3))
  (params           '()   :type list) )


;;;; format-stream
;;;
;
(defclass format-stream (output-stream)
  ((case-conv  :initform nil
        :type (member nil :upcase :downcase
                      :cap-all-1 :cap-all-2
                      :cap-one ))
   (col      :initform 0   :type fixnum)

   ;; For simple padding (ABDORSX).
   (field-buffer :initform nil :type (or null simple-string))
   (field-nchars :initform 0   :type ext:sequence-index)
   (field-npads  :initform 0   :type fixnum)

   (stream   :initform nil :type (or stream null)) ) )


;;;; format-string
(define-pool format-string ()
  :constructor #'(lambda () (make-string 100)) )


;;;; format-context pool
(define-pool format-context ()
  :constructor #'(lambda () (make-structure 'format-context))
  :finalizer
    #'(lambda (context)
        (setf (ref format-context control-string   context) "")
        (setf (ref format-context arg-list         context) nil)
        (setf (ref format-context arg-scan         context) nil)
        (setf (ref format-context pprint-pop       context) nil)
        (setf (ref format-context outer-scan       context) nil)
        (setf (ref format-context params           context) nil) ) )


;;;; format-stream pool
;
(define-pool format-stream ()
  :constructor
    #'(lambda ()
        (make-instance 'format-stream) )

  :finalizer
    #'(lambda (stream &aux (st (ref instance storage stream)))
        (setf (ref format-stream flags st) STREAM-FLAG-OUTPUT)

        (when (ref format-stream field-buffer st)
          (free-pooled-format-string (ref format-stream field-buffer st))
          (setf (ref format-stream field-buffer st) nil) )

        (setf (ref format-stream stream       st) nil)
        (setf (ref format-stream case-conv    st) nil)
        (setf (ref format-stream col          st) 0)
        (setf (ref format-stream field-nchars st) 0)
        (setf (ref format-stream field-npads  st) 0) ) )


;;;; *format-dispatch-vector*
;;;
;;; Updated by:
;;;   define-format
;;;
;;; Referenced by:
;;;   format-interpreter
;;;
;;; Description:
;;;  Mapping of format directive character code to format directive
;;;  interpreter.
;
(defvar *format-dispatch-vector* (make-array 128 :initial-element nil))


(declaim (ftype (function () nil) compiled-format-error-no-args))

(declaim (ftype (function (stream integer integer) unspecified)
  format-absolute-tab ) )

(declaim (ftype (function (stream t (unsigned-byte 2)) unspecified)
  format-character ) )

(declaim (ftype (function (stream t (unsigned-byte 2)) unspecified)
  format-character ) )

(declaim (ftype (function (stream t) unspecified)
  format-cardinal format-ordinal ) )

(declaim (ftype (function
    (stream (unsigned-byte 2) integer integer integer character)
    unspecified )
  format-end-padding ) )

(declaim (ftype (function (format-context format-control &rest t) nil)
  format-error ) )

(declaim (ftype (function (format-context unsigned-byte) nil)
  format-error-clause-count ) )

(declaim (ftype (function (format-context) nil)
  format-error-control-end
  format-error-expect-digit
  format-error-invalid-modifier
  format-error-no-args
  format-error-pp-malformed
  format-error-semicolon-brace
  format-error-semicolon-paren
  format-error-too-few-params
  format-error-too-many-params ) )

(declaim (ftype (function (format-context sequence-index) nil)
  format-error-no-close ) )

(declaim (ftype (function
    (stream float (unsigned-byte 2) integer integer integer character)
    unspecified )
  format-float-D ) )

(declaim (ftype (function
    (stream float
        t                       ; at-sign-p
        fixnum                  ; w
        (or fixnum null)        ; d
        (or fixnum null)        ; e
        fixnum                  ; k
        (or character null)     ; ovfchar
        character               ; padchar
        (or character null) )   ; exptchar
    unspecified )
  format-float-E-aux
  format-float-G-aux ) )

(declaim (ftype (function
    (stream
     (or character null)    ; sign
     list                   ; digits
     (or fixnum null)       ; d
     fixnum                 ; k
     character              ; marker
     fixnum                 ; expt
     fixnum                 ; ewidth
     t                      ; leading-zero-p
     t  )                   ; trailing-zero-p
    unspecified )
  format-float-E-digits ) )

(declaim (ftype (function
    (stream
     float
     t                      ; at-sign-p
     fixnum fixnum fixnum   ; w d k
     character character )  ; ovfchar padchar
    t )
  format-float-F-aux ) )

(declaim (ftype (function
    (stream
     (or character null)    ; sign
     list                   ; digits
     fixnum fixnum          ; k d
     t )                    ; leading-zero-p
    unspecified )
  format-float-F-digits ) )

(declaim (ftype (function (stream fixnum) unspecified)
  format-fresh-line ) )

(declaim (ftype (function
    (string sequence-index sequence-index)
    simple-string )
  format-insert-fill ) )

(declaim (ftype (function
    (stream t
     (unsigned-byte 2)  ; modifier
     (integer 2 36)     ; base
     fixnum             ; mincol
     character          ; padchar
     character          ; commachar
     fixnum )           ; comma-interval
    unspecified )
  format-integer ) )

(declaim (ftype (function (stream format-context) unspecified)
  format-interpreter ) )

(declaim (ftype (function
    (stream format-context sequence-index sequence-index)
    unspecified )
  format-interpreter-1 ) )

(declaim (ftype (function
    (stream format-context simple-string sequence-index sequence-index)
    unspecified )
  format-interpreter-2 ) )

(declaim (ftype (function
    (stream format-context
     simple-string sequence-index sequence-index
     list )
    list )
  format-interpreter-3 ) )

(declaim (ftype (function
    (stream format-context
     simple-string sequence-index sequence-index
     list
     (or function null) )
    unspecified )
  format-interpreter-4 ) )

(declaim (ftype (function
    (stream
     list                       ; segments
     (or simple-string null)    ; prefix
     t                          ; spare
     fixnum                     ; line-width
     (unsigned-byte 2)          ; modifier
     fixnum                     ; mincol
     fixnum                     ; colinc
     fixnum                     ; minpad
     character )                ; padchar
     unspecified )
  format-justify ) )

(declaim (ftype (function
    (stream
     format-control
     list
     &optional t list )
    unspecified )
  format-main ) )

(declaim (ftype (function (stream integer) unspecified)
  format-roman format-old-romand ) )

(declaim (ftype (function
    (format-context sequence-index sequence-index)
    (values character t sequence-index) )
  format-parse-param ) )

(declaim (ftype (function
    (stream t (unsigned-byte 2) fixnum fixnum fixnum character)
    unspecified )
  format-prin1
  format-princ ) )

(declaim (ftype (function (stream fixnum fixnum) unspecified)
    format-relative-tag ) )

(declaim (ftype (function
    (format-context character sequence-index)
    (values sequence-index character (unsigned-byte 2)) )
  format-skip-clause ) )

(declaim (ftype (function
    (format-context character sequence-index)
    unspecified )
  format-skip-to-close ) )

(declaim (ftype (function (format-context) symbol)
  format-slashed-symbol ) )

(declaim (ftype (function
    (stream (unsigned-byte 2) fixnum fixnum fixnum character) unspecified)
  format-start-padding ) )

(declaim (ftype (function (format-stream stream) unspecified)
  format-start-stream ) )

(declaim (ftype (function
    (stream format-context sequence-index sequence-index) simple-string)
  format-to-string ) )

(declaim (ftype (function (character format-stream) unspecified)
  format-write-char ) )

;; BUGBUG: We will drop old-float-to-digits.
(declaim (ftype (function
    (float fixnum fixnum (member :absolute :normal :relative :variable) fixnum)
    (values list fixnum) )
  old-float-to-digits ) )


;;;; stream-line-column
(defmethod ext:stream-line-column ((stream format-stream))
    (declare (values ext:sequence-index))
  (ref format-stream col (ref instance storage stream)) )


;;;; stream-write-char
(defmethod ext:stream-write-char ((stream format-stream) char)
    (declare (type character char))
    (declare (values character))
  (format-write-char char stream) )


;;;; define-format
;;;
;;; Syntax:
;;;     define-format char (param-spec*) {decl}* {form}*
;;;
;;; Local Macros:
;;;     all-args
;;;     modifier
;;;     next-arg
;;;     params
;;;     rest-args
;
(defmacro define-format (char (&rest param-spec*) &body body)
  (let* ((fn-handler (intern (format nil "~~~:C directive handler" char)))
         (var-params (gensym "params_"))
         (minparams 0)
         (maxparams 0)
         (bindings '()) )

    (unless (equal '(*) param-spec*)
      (dolist (param-spec param-spec*)
        (cond
          ((symbolp param-spec)
            (push `(,param-spec (pop ,var-params)) bindings)
            (incf minparams) )

          ((eql (safe-list-length param-spec) 2)
            (push `(,(first param-spec) (or (pop ,var-params)
                                            ,(second param-spec) ))
                  bindings ) )
          (t
            (c::macro-error "Invalid parameter specifier: ~S"
                   param-spec ) ))
        (incf maxparams) ))

    (setq bindings (nreverse bindings))

    (when bindings
      (setq bindings `((,var-params (ref format-context params context))
                       ,@bindings )) )

    ;; Result form
    ;;
    `(progn
       (eval-when (:compile-toplevel)
         (c::compile-notice 'define-format ',char) )

       (labels (
           (,fn-handler (stream context)
               (declare (ignorable stream))
             (macrolet (
               ;; all-args
               (all-args ()
                 `(ref format-context arg-list context) )

               ;; modifier
               ;;
               (modifier (&rest accepts)
                 (cond
                   ((null accepts)
                     '(ref format-context modifier context) )

                   ((and (eq 'nil (first accepts)) (null (rest accepts)))
                     `(let ((modifier
                              (ref format-context modifier context) ))
                        (unless (zerop modifier)
                          (format-error-invalid-modifier context) )
                        ,(c::make-ignorable-form 'modifier) ) )
                   (t
                     `(let ((modifier
                              (ref format-context modifier context) ))
                          (declare (ignorable modifier))
                        (case modifier
                          (0)
                          ,.(mapcar #'(lambda (name)
                                        (ecase name
                                          (:colon         '(1))
                                          (:at-sign       '(2))
                                          (:colon-at-sign '(3)) ) )
                                    accepts )
                          (otherwise
                            (format-error-invalid-modifier context) ))
                         ,(c::make-ignorable-form 'modifier) ) )) )

               ;; next-arg
               ;;
               (next-arg ()
                 `(progn
                    (when (null (ref format-context arg-scan context))
                      (format-error-no-args context) )
                    (when (ref format-context pprint-pop context)
                      (funcall (ref format-context pprint-pop context)) )
                    (pop (ref format-context arg-scan context)) ) )

               ;; params
               ;;
               (params ()
                 `(ref format-context params context) )

               ;; rest-args
               ;;
               (rest-args ()
                 `(ref format-context arg-scan context) )
               )
               ;;
               ,@(cond
                   ((equal '(*) param-spec*) nil)
                   ((and (zerop minparams) (zerop maxparams))
                     `((when (ref format-context params context)
                       (format-error-too-many-params context) )) )

                   ((zerop minparams)
                     `((let ((nparams
                               (length
                                 (ref format-context params context) )))
                        (when (> nparams ,maxparams)
                           (format-error-too-many-params context) ))) )

                   (t
                     `((let ((nparams
                               (length
                                 (ref format-context params context))))
                         (when (< nparams ,minparams)
                           (format-error-too-few-params
                             context ) )
                         (when (> nparams ,maxparams)
                            (format-error-too-many-params
                              context ) ))) ))

               (let* ,bindings ,@body) ) )
            )
            ;;
            (setf (svref *format-dispatch-vector*
                        (char-code (char-upcase ,char)) )
                  #',fn-handler )

            (setf (svref *format-dispatch-vector*
                        (char-code (char-downcase ,char)) )
                  #',fn-handler )

            ,(c::make-ignorable-form `',fn-handler) )) ) )
