;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Runtime - 22 Printer - Format
;;; runtime/r22-format.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r22-format.lisp#5 $
;;;
;;; Description:
;;;  This file implements following functions:
;;;
;;; Compiled Format:
;;;     compiled-format-error-no-args
;;;
;;; Compiled and Interpret Format:
;;;     format-absolute-tab             ~@T
;;;     format-cardinal                 ~:R
;;;     format-character
;;;     format-end-padding                ~ABDORSX
;;;     format-error-control-end
;;;     format-error-expect-digit
;;;     format-error-invalid-modifier
;;;     format-error-no-args
;;;     format-error-no-close
;;;     format-error-pp-malformed
;;;     format-error-semicolon-brace
;;;     format-error-semicolon-paren
;;;     format-error-too-few-params
;;;     format-error-too-many-params
;;;     format-fresh-line               ~&
;;;     format-insert-fill
;;;     format-interpreter
;;;     format-interpreter-1
;;;     format-interpreter-2
;;;     format-interpreter-3
;;;     format-interpreter-4
;;;     format-justify
;;;     format-main
;;;     format-parse-param
;;;     format-old-roman                ~:@R
;;;     format-oridnal                  ~R
;;;     format-prin1                    ~S
;;;     format-princ                    ~A
;;;     format-relative-tab             ~T
;;;     format-roman                    ~@R
;;;     format-skip-clause              ~(, ~<, ~[, ~{
;;;     format-skip-to-close            ~(, ~<, ~[, ~{
;;;     format-slashed-symbol           ~/
;;;     format-start-padding              ~ABDORSX
;;;     format-start-stream             BUGBUG: Bad naming.
;;;     format-to-string                recursive interpreter
;;;     format-write-char
;;;
;;; Public Functions:
;;;     format
;;;
;;; Record of format:
;;;     format - process destination
;;;       format-main - establish format-stream and format-context
;;;         catch 'up-and-out
;;;           format-interpreter
;;;             *format-dispatch-vector*
;;;               directive handlers
;;;                 ~:{...~}    catch 'up-up-and-out, recursive-call
;;;                 ~:@{...~}   catch 'up-up-and-out, recursive-call
;;;                 ~<{...~>    catch 'up-and-out,    recursive-call
;;;                 ~?          recursive-call
;;;                 ~[...~]     recursive-call
;;;                 ~(...~)     recursive-call, filer
;;;                 ~mincolA    filter
;;;                 ~mincolB    filter
;;;                 ~mincolD    filter
;;;                 ~mincolO    filter
;;;                 ~mincolR    filter
;;;                 ~mincolS    filter
;;;                 ~mincolX    filter
;;;                 ~^          throw
;
(in-package :si)

;;;; compiled-format-error-no-args
;;;
;;; Description:
;;;  Reports no-args error in formatter function.
;
(defun compiled-format-error-no-args ()
  (error 'simple-printer-error
         :format-control "Insufficient arguments for compiled format." ) )


;;;; format-absolute-tab
;;; ~column,colincT
;
(defun format-absolute-tab (stream column colinc)
    (declare (type stream  stream))
    (declare (type integer column))
    (declare (type integer colinc))
  (let* ((current (stream-line-column stream))
         (nspaces
            (cond
              ((not current)  2)
              ((zerop colinc) (- column current))
              (t (- (* colinc (ceiling column colinc)) current)) ) ))
    (write-chars #\Space nspaces stream) ) )


;;;; format-character
;
(defun format-character (stream char modifier)
    (declare (type stream            stream))
    (declare (type (unsigned-byte 2) modifier))
  (if (not (characterp char))
      (let ((*print-escape* t))
        (write-object char stream) )
    (ecase modifier
      ((0)  ; ~C
        (stream-write-char stream char) )
      ((1)  ; ~:C
        (stream-write-string stream (char-name char)) )
      ((2)  ; ~@C
        (stream-write-string stream "#\\")
        (stream-write-string stream (char-name char)) )
      ((3)  ; ~:@C
        (stream-write-string stream (char-name char)) ))) )


;;;; format-cardinal    ~R
;;;  format-oridnal     ~:R
(labels (
  ;; format-english
  ;;
  (format-english (stream n ordinal-p)
    (cond
      ((not (and (integerp n)
                 (<= most-negative-english-number
                     n
                     most-positive-english-number )))
        (let ((*print-base* 10))
          (write-object n stream) ) )

      ((/= 0 n)
        (when (minusp n)
          (stream-write-string stream "minus ")
          (setq n (- n)) )
        (if (< n 1000)
            (format-english<1000 stream n ordinal-p)
          (format-english>1000 stream n ordinal-p 0) ) )

      (ordinal-p
        (stream-write-string stream (svref *english-ordinal-0-19*  0)) )

      (t
        (stream-write-string stream (svref *english-cardinal-0-19* 0)) )) )

    ;; format-english<1000
    ;;
    (format-english<1000 (stream n ordinal-p)
      (when (>= n 100)
        (multiple-value-bind (q r) (truncate n 100)
          (format-english<1000 stream q nil)
          (stream-write-char stream #\Space)
          (setq n r) )

        (stream-write-string
            stream
            (cond
              ((/= 0 n)  "hundred ")
              (ordinal-p "hundredth")
              (t         "hundred") )))

      (when (>= n 20)
        (multiple-value-bind (q r) (truncate n 10)
          (decf q 2)
          (setq n r)
          (cond
            ((/= 0 n)
              (stream-write-string stream (svref *english-cardinal-20-90* q))
              (stream-write-char stream #\-) )
            (ordinal-p
              (stream-write-string
                stream (svref *english-ordinal-20-90*  q) ) )
            (t
              (stream-write-string
                  stream (svref *english-cardinal-20-90* q) ) )) ))

      (cond
        ((= 0 n))
        (ordinal-p
          (stream-write-string
            stream (svref *english-ordinal-0-19*  n) ) )
        (t
          (stream-write-string
            stream (svref *english-cardinal-0-19* n) ) )) )

    ;; format-english>1000
    ;;
    (format-english>1000 (stream n ordinal-p nth)
      (multiple-value-bind (q r) (truncate n 1000)
        (if (< q 1000)
            (format-english<1000 stream q ordinal-p)
          (progn
             (format-english>1000 stream q ordinal-p (1+ nth))
             (when (zerop r)
               (return-from format-english>1000) )))
        (stream-write-char stream #\Space)
        (stream-write-string stream (svref *english-cardinal-expt-1000* nth))
        (cond
          ((/= 0 r)
            (stream-write-char stream #\Space)
            (format-english<1000 stream r ordinal-p) )
          (ordinal-p (stream-write-string stream "th")) ) ) )
  )
  ;;
  ;;
  (defun format-cardinal (stream integer)
    (format-english stream integer nil) )

  (defun format-ordinal (stream integer)
    (format-english stream integer t) )
 ) ; labels


;;;; format-end-padding
(defun format-end-padding (stream modifier mincol colinc minpad padchar)
    (declare (type format-stream stream))
    (declare (ignore mincol))

  (let* ((st     (ref instance storage stream))
         (npads  (ref format-stream field-npads  st))
         (buffer (ref format-stream field-buffer st)) )

    ;; Stop buffering for field padding.
    (setf (ref format-stream field-buffer st) nil)

    (unless (logbitp 1 modifier)
      (write-chars padchar minpad stream)
      (decf npads minpad) )

    (when (plusp npads)
      (write-chars padchar (* colinc (ceiling npads colinc)) stream)
      (when buffer
         (write-string buffer stream
                       :end (ref format-stream field-nchars st) )
         (free-pooled-format-string buffer) )) ) )


;;;; format-error
;
(defun format-error (context control &rest args)
  (error 'format-error
         :context context
         :complaint control
         :arguments args ) )


;;;; format-error-clause-count
(defun format-error-clause-count (context expect)
  (format-error context "Must be ~R clause~:P." expect ) )


;;;; format-error-control-end
(defun format-error-control-end (context)
  (setf (ref format-context control-index context)
        (ref format-context control-end   context) )
  (format-error context "Unexpected end of control string." ) )


;;;; format-error-expect-digit
(defun format-error-expect-digit (context)
  (format-error context "Expect digit character." ) )


;;;; format-error-invalid-modifier
(defun format-error-invalid-modifier (context)
  (format-error context "Invalid modifier combination." ) )


;;;; format-error-no-args
(defun format-error-no-args (context)
  (format-error context "Insufficient format arguments.") )


;;;; format-error-no-close
(defun format-error-no-close (context open-index)
  (setf (ref format-context control-index context) open-index)
  (format-error context "No matching close directive.") )


;;;; format-error-pp-malformed
(defun format-error-pp-malformed (context)
  (format-error context "~~<...~~:> requires at most three clauses.") )


;;;; format-error-semicolon-brace
(defun format-error-semicolon-brace (context)
  (format-error context "Can't use ~~; inside ~~{...~~}.") )


;;;; format-error-semicolon-paren
(defun format-error-semicolon-paren (context)
  (format-error context "Can't use ~~; inside ~~(...~~).") )


;;;; format-error-too-few-params
(defun format-error-too-few-params (context)
  (format-error context "Too few parametes.") )


;;;; format-error-too-many-params
(defun format-error-too-many-params (context)
  (format-error context "Too many parametes.") )


;;;; format-fresh-line
(defun format-fresh-line (stream n)
  (when (plusp n)
    (fresh-line stream)
    (loop repeat (1- n) do (stream-terpri stream)) ) )


;;;; format-insert-fill
;;;
;;; Called by:
;;;  ~<...~:>
;;;
;;; Description:
;;;  Inserts "~:_" after each groups of blanks.
;;;
;;; BUGBUG: NYI: Handle directives. When parameter hash '<space> and
;;; ~<newline>, current implementation inserts "~:_".
(defun format-insert-fill (ctrl-string ctrl-start ctrl-end)
  (with-output-to-string (stream)
    (loop
      for index from ctrl-start below ctrl-end
      for char = (schar ctrl-string index)
      with blank-p = nil
      finally
        (when blank-p (stream-write-string stream "~:_"))
      do
        (cond
          ((char= #\Space char)
            (setq blank-p t) )
          (blank-p
            (stream-write-string stream "~:_")
            (setq blank-p nil) ))
        (stream-write-char stream char) ) ) )


;;;; format-integer
;;;
;;; Modifier:
;;;   : = comma
;;;   @ = sign
;;;
;;; Binds:
;;;   *print-base*        base
;;;   *print-escape*      nil
;;;   *print-radix*       nil
;;;   *print-readably*    nil
;;;
;;; Note: format-integer will be appeared in function generated by formatter
;;; macro. So, we can not use format-context to pass argument.
;;;
;;; See:
;;;  print-bignum-aux, print-fixnum-aux, print-integer
;;;
;
(defun format-integer (stream arg
                       modifier base mincol padchar
                       commachar comma-interval )
    (declare (type integer   mincol))
    (declare (type character padchar))
    (declare (type character commachar))
    (declare (type integer   comma-interval))
  (labels (
    ;; format-bignum-comma
    ;;
    (format-bignum-comma (stream bignum base divisor ndigits
                          commachar comma-interval column )
      (multiple-value-bind (quotient remainder) (truncate bignum divisor)
        (incf column ndigits)

        (if (typep quotient 'fixnum)
            (format-fixnum-comma stream quotient base
                                 commachar comma-interval
                                 column )
          (format-bignum-comma stream quotient
                               base divisor ndigits
                               commachar comma-interval
                               column ))

        (decf column ndigits)

        ;; Print remainder in ndigits field with leading zero padding.
        ;;
        (loop for column-2  = column then (1+ column-2)
              and base^n    = base then (* base^n base)
              until (> base^n remainder)
              finally
                (loop for column-3 from (1- (+ column ndigits))
                                   above column-2 do
                  (when (zerop (mod column-3 comma-interval))
                    (stream-write-char stream commachar) )
                  (stream-write-char stream #\0) )

                (when (zerop (mod column-2 comma-interval))
                  (stream-write-char stream commachar) ))

        (format-fixnum-comma stream remainder base
                             commachar comma-interval
                             column ) ) )

    ;; format-fixnum-comma
    ;;
    (format-fixnum-comma (stream fixnum base
                          commachar comma-interval column )
      (multiple-value-bind (quotient remainder) (truncate fixnum base)
        (when (plusp quotient)
          (format-fixnum-comma stream quotient base
                             commachar comma-interval
                             (1+ column) )
          (when (zerop (mod column comma-interval))
            (stream-write-char stream commachar) ))
        (stream-write-char stream (digit-char remainder base)) ) )

    ;; format-integer-aux
    ;;
    (format-integer-aux (stream integer
                         modifier base commachar comma-interval )
      (cond
        ((minusp integer)
          (stream-write-char stream #\-)
          (setq integer (- integer)) )

        ((logbitp 1 modifier)
          (stream-write-char stream #\+) ))

      (etypecase integer
        (fixnum
          (if (not (logbitp 0 modifier))
              (print-fixnum-aux integer stream base)
            (format-fixnum-comma stream integer base
                                 commachar comma-interval 1 )) )
        (bignum
          (let ((divisor (aref *bignum-divisor-vector* base))
                (ndigits (aref *fixnum-ndigits-vector* base)) )
            (if (not (logbitp 0 modifier))
                (print-bignum-aux integer stream base divisor ndigits)
              (format-bignum-comma stream integer base
                                   divisor ndigits
                                   commachar comma-interval 1 )) ) )) )
    )
    ;;
    ;; format-integer
    ;;
    (cond
      ((not (integerp arg))
        (let ((*print-base*       base)
              (*print-escape*     nil)
              (*print-radix*      nil)
              (*print-readably*   nil) )
          (write-object arg stream) ) )

      ((not (if (logbitp 0 modifier) (>= mincol 3) (>= mincol 2)))
        (format-integer-aux stream arg
                            modifier base commachar comma-interval ) )

      ((typep stream 'format-stream)
        (format-start-padding stream 2 mincol 1 0 padchar)
        (format-integer-aux stream arg
                            modifier base commachar comma-interval )
        (format-end-padding stream 2 mincol 1 0 padchar) )

      (t
        (let ((string
                (with-output-to-string (stream)
                  (format-integer-aux stream arg
                                      modifier base
                                      commachar comma-interval ))) )
         (write-chars padchar (- mincol (length string)) stream)
         (stream-write-string stream string) ) )) ) )


;;;; format-interpreter
;;;
;;; BUGBUG: PERF: Should use stack-cons for collecting params.
(defun format-interpreter (stream context)
    (declare (type stream stream))
    (declare (type format-context context))
  (let* ((prev            (ref format-context control-start  context))
         (end             (ref format-context control-end    context))
         (ctrl-str        (ref format-context control-string context))
         (index           prev) )
    (block outer
      (loop
        ;; Search tilde(~)
        (loop
          (when (= end index) (return-from outer))
          (when (char= #\~ (schar ctrl-str index)) (return))
          (incf index) )

        ;; Found tilde(~)
        (when (> index prev)
          (write-string ctrl-str stream :start prev :end index) )

        (incf index)

        (when (= end index)
          (the nil (format-error-control-end context)) )

        (let ((char     (schar ctrl-str index))
              (modifier 0)
              (params   '()) )

          ;; Parse parameters
          ;;
          (let ((state :none)
                (param 0)
                (sign  0) )
            (loop
              (ecase state
                ((:none)
                  (case char
                    ((#\V #\v)
                      (when (null (ref format-context arg-scan context))
                        (format-error-no-args context) )
                      (push (pop (ref format-context arg-scan context))
                            params )
                      (setq state :param) )
                    ((#\#)
                      (push (length (ref format-context arg-scan context))
                            params )
                      (setq state :param) )
                    ((#\')
                      (incf index)
                      (when (= end index)
                        (the nil (format-error-control-end context)) )
                      (push (schar ctrl-str index)
                            params )
                      (setq state :param) )
                    ((#\,)
                      (push nil params) )
                    ((#\+)
                      (setq sign  1)
                      (setq param 0)
                      (setq state :sign) )
                    ((#\-)
                      (setq sign  -1)
                      (setq param 0)
                      (setq state :sign) )
                    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                      (setq param (digit-char-p char 10))
                      (setq state :digit) )
                    (otherwise
                      (return) )) )
                ((:digit)
                  (if (char= #\, char)
                      (progn
                        (push param params)
                        (setq state :none) )
                    (let ((digit (digit-char-p char 10)))
                      (unless digit
                        (push param params)
                        (return) )
                      (setq param (+ digit (* param 10))) )) )
                ((:sign)
                  (setq param (digit-char-p char 10))
                  (unless param
                    (the nil (format-error-expect-digit context)) )
                  (setq param (* sign param))
                  (setq state :digit) )
                ((:param)
                  (unless (char= #\, char) (return))
                  (setq state :none) ))

              (incf index)
              (when (= end index)
                (the nil (format-error-control-end context)) )
              (setq char (schar ctrl-str index)) )

            (setq params (nreverse params)) )

          ;; Parse modifiers
          ;;
          (loop
            (case char
              (#\: (setq modifier (logior 1 modifier)))
              (#\@ (setq modifier (logior 2 modifier)))
              (otherwise (return)) )

            (incf index)
            (when (= end index)
              (the nil (format-error-control-end context)) )
            (setq char (schar ctrl-str index)) )

          (incf index)

          (setf (ref format-context control-index context) index)
          (setf (ref format-context modifier      context) modifier)
          (setf (ref format-context params        context) params)

          ;; dispatch to directive handlers
          ;;
          (let ((handler
                  (and (< (char-code char) 128)
                       (svref *format-dispatch-vector* (char-code char)))))
            (unless handler
              (format-error context "Unknown directive ~~~C." char) )
            (funcall handler stream context) ) )
        (setq index (ref format-context control-index context))
        (setq prev index) ))

    ;; Emit trailing string
    ;;
    (when (> index prev)
      (write-string ctrl-str stream :start prev :end index) ) ) )


;;;; format-interpreter-1
;;;
;;; For: ~[...~]
;
(defun format-interpreter-1 (stream context ctrl-start ctrl-end)
    (declare (values unspecified))
  (with-accessors ((ctxt-start format-context-control-start)
                   (ctxt-index format-context-control-index)
                   (ctxt-end   format-context-control-end) ) context
    (let ((save-start ctxt-start)
          (save-index ctxt-index)
          (save-end   ctxt-end) )

      (setf ctxt-start ctrl-start)
      (setf ctxt-end   ctrl-end)

      (unwind-protect
          (format-interpreter stream context)
        (setf ctxt-start save-start)
        (setf ctxt-index save-index)
        (setf ctxt-end   save-end) ) )) )


;;;; format-interpreter-2
;;;
;;; For: ~? and ~{...~}
;
(defun format-interpreter-2 (stream context
                                       ctrl-string ctrl-start ctrl-end)
    (declare (values unspecified))
  (with-accessors ((ctxt-string  format-context-control-string)
                   (ctxt-start-0 format-context-control-start-0)
                   (ctxt-end-0   format-context-control-end-0)

                   (ctxt-start   format-context-control-start)
                   (ctxt-index   format-context-control-index)
                   (ctxt-end     format-context-control-end) ) context

    (let ((save-string  ctxt-string)
          (save-start-0 ctxt-start-0)
          (save-end-0   ctxt-end-0)

          (save-start   ctxt-start)
          (save-index   ctxt-index)
          (save-end     ctxt-end) )

        (setq ctxt-string  ctrl-string)
        (setq ctxt-start-0 ctrl-start)
        (setq ctxt-end-0   ctrl-end)

        (setf ctxt-start   ctrl-start)
        (setf ctxt-end     ctrl-end)

        (unwind-protect
            (format-interpreter stream context)
          (setq ctxt-string  save-string)
          (setq ctxt-start-0 save-start-0)
          (setq ctxt-end-0   save-end-0)

          (setf ctxt-start save-start)
          (setf ctxt-index save-index)
          (setf ctxt-end   save-end) ) ) ) )


;;;; format-interpreter-3
;;;
;;; For: ~{...~}
;
(defun format-interpreter-3 (stream context
                             ctrl-string ctrl-start ctrl-end
                             args )
    (declare (values list))
  (with-accessors ((ctxt-string   format-context-control-string)
                   (ctxt-start-0  format-context-control-start-0)
                   (ctxt-end-0    format-context-control-end-0)
                   (ctxt-start    format-context-control-start)
                   (ctxt-index    format-context-control-index)
                   (ctxt-end      format-context-control-end)
                   (ctxt-arg-list format-context-arg-list)
                   (ctxt-arg-scan format-context-arg-scan) ) context

    (let ((save-string  ctxt-string)
          (save-start-0 ctxt-start-0)
          (save-end-0   ctxt-end-0)

          (save-start   ctxt-start)
          (save-index   ctxt-index)
          (save-end     ctxt-end)

          (save-arg-list ctxt-arg-list)
          (save-arg-scan ctxt-arg-scan) )

        (setq ctxt-string  ctrl-string)
        (setq ctxt-start-0 ctrl-start)
        (setq ctxt-end-0   ctrl-end)

        (setf ctxt-start   ctrl-start)
        (setf ctxt-end     ctrl-end)

        (setq ctxt-arg-list args)
        (setq ctxt-arg-scan args)

        (unwind-protect
            (progn
              (format-interpreter stream context)
              ctxt-arg-scan )
          (setq ctxt-string  save-string)
          (setq ctxt-start-0 save-start-0)
          (setq ctxt-end-0   save-end-0)

          (setf ctxt-start save-start)
          (setf ctxt-index save-index)
          (setf ctxt-end   save-end)

          (setq ctxt-arg-list save-arg-list)
          (setq ctxt-arg-scan save-arg-scan) ) ) ) )


;;;; format-interpreter-4
;;;
;;; For: ~<...~:>
;
(defun format-interpreter-4 (stream context
                                       ctrl-string ctrl-start ctrl-end
                                       args
                                       pprint-pop )
    (declare (values unspecified))
  (with-accessors ((ctxt-string     format-context-control-string)
                   (ctxt-start-0    format-context-control-start-0)
                   (ctxt-end-0      format-context-control-end-0)
                   (ctxt-start      format-context-control-start)
                   (ctxt-index      format-context-control-index)
                   (ctxt-end        format-context-control-end)
                   (ctxt-arg-list   format-context-arg-list)
                   (ctxt-arg-scan   format-context-arg-scan)
                   (ctxt-pprint-pop format-context-pprint-pop) ) context

    (let ((save-string     ctxt-string)
          (save-start-0    ctxt-start-0)
          (save-end-0      ctxt-end-0)

          (save-start      ctxt-start)
          (save-index      ctxt-index)
          (save-end        ctxt-end)

          (save-arg-list   ctxt-arg-list)
          (save-arg-scan   ctxt-arg-scan)
          (save-pprint-pop ctxt-pprint-pop) )

        (setq ctxt-string  ctrl-string)
        (setq ctxt-start-0 ctrl-start)
        (setq ctxt-end-0   ctrl-end)

        (setf ctxt-start   ctrl-start)
        (setf ctxt-end     ctrl-end)

        (setq ctxt-arg-list   args)
        (setq ctxt-arg-scan   args)
        (setq ctxt-pprint-pop pprint-pop)

        (unwind-protect
            (progn
              (format-interpreter stream context)
              ctxt-arg-scan )
          (setq ctxt-string  save-string)
          (setq ctxt-start-0 save-start-0)
          (setq ctxt-end-0   save-end-0)

          (setf ctxt-start save-start)
          (setf ctxt-index save-index)
          (setf ctxt-end   save-end)

          (setq ctxt-arg-list   save-arg-list)
          (setq ctxt-arg-scan   save-arg-scan)
          (setq ctxt-pprint-pop save-pprint-pop) ) ) ) )


;;;; format-justify
(defun format-justify (stream segments
                       prefix spare line-width
                       modifier mincol colinc minpad padchar )
    (declare (type format-stream           stream))
    (declare (type list                    segments))
    (declare (type (or null simple-string) prefix))

  (when (null segments)
    (when (and prefix (> spare line-width))
      (stream-write-string stream prefix) )

    (write-chars padchar mincol stream)
    (return-from format-justify) )

  (let* ((nsegments (length segments))
         (ngaps     (ecase modifier
                      (0 (if (= 1 nsegments) 1 (1- nsegments)))
                      (1 nsegments)
                      (2 nsegments)
                      (3 (1+ nsegments)) ))
         (nchars    (+ (loop for segment in segments
                             sum (length segment) )
                       (* minpad ngaps) ))
         (width     (if (< nchars mincol)
                         mincol
                      (+ (* colinc (ceiling (- nchars mincol) colinc))
                          mincol )))
         (npads     (- width nchars))
         (npads-1   0)
         (extra     0)
         (pos       (ref format-stream col (ref instance storage stream))) )

    (when (and prefix (> (+ pos width spare) line-width))
      (stream-write-string stream prefix) )

    (multiple-value-setq (npads-1 extra) (truncate npads ngaps))
    (incf npads-1)

    (ecase modifier
      ((0)  ; ~<...~>
        (if (null (rest segments))
            (progn
              (write-chars padchar npads stream)
              (stream-write-string stream (first segments)) )
          (progn
            (stream-write-string stream (pop segments))

            (when (zerop extra) (decf npads-1))
            (write-chars padchar npads-1 stream)
            (decf extra)

            (loop until (null (rest segments))
                  do (stream-write-string stream (pop segments))
                     (when (zerop extra) (decf npads-1))
                     (write-chars padchar npads-1 stream)
                     (decf extra) )
            (stream-write-string stream (first segments)) )) )

      ((1)  ; ~:<....~> = left
        (dolist (segment segments)
          (when (zerop extra) (decf npads-1))
          (write-chars padchar npads-1 stream)
          (decf extra)

          (stream-write-string stream segment) ) )

      ((2)  ; ~@<...~> = right
        (dolist (segment segments)
          (stream-write-string stream segment)

          (when (zerop extra) (decf npads-1))
          (write-chars padchar npads-1 stream)
          (decf extra) ) )

      ((3)  ; ~:@<...~> = left & right
        (when (zerop extra) (decf npads-1))
        (write-chars padchar npads-1 stream)
        (decf extra)

        (dolist (segment segments)
          (stream-write-string stream segment)

          (when (zerop extra) (decf npads-1))
          (write-chars padchar npads-1 stream)
          (decf extra) ) )) ) )


;;;; format-main
;;;
;;; format-main stream control-string args &optional up-up-and-out-p
;;;     => args
;;;
;;; Called by:
;;;  format
;;;  formatter(~?)
;;;
;;; BUGBUG: REVIEW: Can we defer makeing format-stream until we really need it?
;;; BUGBUG: PERF: Should use stack-instance for format-context and
;;; format-stream.
;
(defun format-main (stream control-string args
                    &optional up-up-and-out-p outer-args )
    (declare (type stream stream))
  (etypecase control-string
    (string
      (with-pool (context format-context)
        (setf (ref format-context arg-list context) args)
        (setf (ref format-context arg-scan context) args)

        (multiple-value-bind (ctrl-string ctrl-start ctrl-end)
            (string-data control-string)
          (setf (ref format-context control-string  context) ctrl-string)
          (setf (ref format-context control-start   context) ctrl-start)
          (setf (ref format-context control-start-0 context) ctrl-start)
          (setf (ref format-context control-end     context) ctrl-end)
          (setf (ref format-context control-end-0   context) ctrl-end) )

          (setf (ref format-context up-up-and-out-p context)
                up-up-and-out-p )

          (setf (ref format-context outer-scan context) outer-args)

        (if (typep stream '(or format-stream pp-stream))
            (catch 'up-and-out (format-interpreter stream context))
          (with-pool (stream-2 format-stream)
            (format-start-stream stream-2 stream)
            (catch 'up-and-out (format-interpreter stream-2 context)) )) ) )
    (function
      (apply control-string stream args) ))
  nil )


;;;; format-old-roman   ~:@
;;;; format-roman       ~@R
;
(labels (
  ;; format-roman-aux
  ;;
  (format-roman-aux (stream n old-p &aux digit)
    (unless (and (typep n 'fixnum)
                 (<= 1 n (if old-p 4999 3999)) )
      (let ((*print-base* 10))
        (write-object n stream)
        (return-from format-roman-aux) ))

    (when (>= n 1000)
      (multiple-value-setq (digit n) (truncate n 1000))
      (format-roman-digit stream #\M #\* #\* digit old-p) )

    (when (>= n 100)
      (multiple-value-setq (digit n) (truncate n 100))
      (format-roman-digit stream #\C #\D #\M digit old-p) )

    (when (>= n 10)
      (multiple-value-setq (digit n) (truncate n 10))
      (format-roman-digit stream #\X #\L #\C digit old-p) )

    (format-roman-digit stream #\I #\V #\X n old-p) )

  ;; format-roman-digit
  ;;
  (format-roman-digit (stream one five ten digit old-p)
    (cond
      ((and (not old-p) (= 4 digit))
        ;; 4 => IV
        ;;
        (stream-write-char stream one)
        (stream-write-char stream five) )

      ((and (not old-p) (= 9 digit))
        ;; 9 => IX
        ;;
        (stream-write-char stream one)
        (stream-write-char stream ten) )

      (t
        (when (>= digit 5)
          (stream-write-char stream five)
          (decf digit 5) )
        (loop repeat digit
              do (stream-write-char stream one) ) )) )
  )
  ;;
  ;;
  (defun format-roman (stream n)
    (format-roman-aux stream n nil) )

  (defun format-old-roman (stream n)
    (format-roman-aux stream n t) )
 ) ; labels


;;;; format-parse-param
;;;
;;; Syntax:
;;;   format-parse-param context ctrl-start ctrl-end
;;;     => stop-char, param, ctrl-index
;;;
;;; Called by:
;;;  ~< ... ~:; ... ~>
;
(defun format-parse-param (context ctrl-start ctrl-end)
  (let ((ctrl-string (ref format-context control-string context))
        (ctrl-index  ctrl-start)
        (state       :none)
        (param       nil)
        (sign        0)
        (char        #\*) )
    (loop
      (when (= ctrl-end ctrl-index) (format-error-control-end context))
      (setq char (schar ctrl-string ctrl-index))
      (incf ctrl-index)

      (ecase state
        (:none
          (case char
            ((#\V #\v)
              (when (null (ref format-context arg-scan context))
                (format-error-no-args context) )
              (setq param (pop (ref format-context arg-scan context)))
              (setq state :param) )

            (#\#
              (setq param (length (ref format-context arg-scan context)))
              (setq state :param) )

            (#\'
              (incf ctrl-index)
              (when (= ctrl-end ctrl-index) (format-error-control-end context))
              (setq param (schar ctrl-string ctrl-index))
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
          (unless param (the nil (format-error-expect-digit context)))
          (setq param (* sign param))
          (setq state :digit) )))
    (values char param ctrl-index) ) )


;;;; format-prin1
;;;; format-princ
;
(labels (
  (format-print (stream object modifier mincol colinc minpad padchar)
    (cond
      ((not (plusp mincol))
        (if (and (null object) (logbitp 0 modifier))
            (stream-write-string stream "()")
          (write-object object stream) ) )

      ((typep stream 'format-stream)
        (format-start-padding stream modifier mincol colinc minpad padchar)
        (if (and (null object) (logbitp 0 modifier))
            (stream-write-string stream "()")
          (write-object object stream) )
        (format-end-padding stream modifier mincol colinc minpad padchar) )

      (t
        (let* ((string
                 (if (and (null object) (logbitp 0 modifier))
                     "()"
                   (with-output-to-string (stream)
                     (write-object object stream) )))
               (npads (- mincol (length string))) )

          (when (logbitp 1 modifier)    ; ~mincol@A
            (write-chars padchar minpad stream)
            (decf npads minpad)
            (setq npads (* colinc (ceiling npads colinc)))
            (write-chars padchar npads stream) )

          (stream-write-string stream string)

          (unless (logbitp 1 modifier)  ; ~mincolA
            (write-chars padchar minpad stream)
            (decf npads minpad)
            (setq npads (* colinc (ceiling npads colinc)))
            (write-chars padchar npads stream) ) ) )) )
    )
    ;;
   (defun format-prin1 (stream object modifier mincol colinc minpad padchar)
     (let ((*print-escape* t))
       (format-print stream object modifier mincol colinc minpad padchar) ) )

   (defun format-princ (stream object modifier mincol colinc minpad padchar)
     (let ((*print-escape*   nil)
           (*print-readably* nil) )
       (format-print stream object modifier mincol colinc minpad padchar) ) )
  ) ; labels


;;;; format-relative-tab
;;;
;;; ~colrel,colinc@T
;
(defun format-relative-tab (stream colrel colinc)
    (check-type stream stream)
    (check-type colrel integer)
    (check-type colinc integer)
  (write-chars #\Space colrel stream)
  (when (>= colinc 2)
    (let* ((current (stream-line-column stream))
           (nspaces (if (not current)
                        0
                      (- (* colinc (ceiling current colinc)) current) ) ))
      (write-chars #\Space nspaces stream) )) )


;;;; format-skip-clause
;;;
;;; Syntax:
;;;  format-skip-clause context delimiter default-p
;;;   => tilde, stop-char, modifier
;;;
;;; Arguments and Values:
;;;   tilde - an index of control points to tilde.
;;;
;;; Side-Effects:
;;;   format-context-control-index points to start of next clause.
;
(defun format-skip-clause (context close-char open-index)
    (declare (type format-context context))
    (declare (type character      close-char))
    (declare (type ext:sequence-index      open-index))
  (let* ((control    (ref format-context control-string context))
         (index      (ref format-context control-index  context))
         (end        (ref format-context control-end    context))
         (tilde      0) )
    (loop
      ;; Skip until tilde(~).
      (loop
        (when (= end index)
          (the nil (format-error-no-close context open-index)) )
        (when (char= #\~ (schar control index)) (return))
        (incf index) )

      (setq tilde index)

      (incf index)

      (when (= end index)
        (the nil (format-error-no-close context open-index)) )

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
                  ((#\#)     (setq state :param))
                  ((#\')
                    (incf index)
                    (when (= end index)
                      (format-error-no-close context open-index) )
                    (setq state :param) )
                  ((#\,))
                  ((#\+) (setq state :sign))
                  ((#\-) (setq state :sign))
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
                  (the nil (format-error-expect-digit context)) )
                (setq state :digit) )
              (:param
                (unless (char= #\, char) (return))
                (setq state :none) ))

            (incf index)
            (when (= end index)
              (format-error-no-close context open-index) )
            (setq char (schar control index)) ) )

        ;; Parse modifiers
        ;;
        (loop
          (case char
            ((#\:) (setq modifier (logior 1 modifier)))
            ((#\@) (setq modifier (logior 2 modifier)))
            (otherwise (return)) )
          (incf index)
          (when (= end index)
            (format-error-no-close context open-index) )
          (setq char (schar control index)) )

        ;; Dispatch
        ;;
        (incf index)
        (setf (ref format-context control-index context) index)

        (when (char= close-char char)
          (return (values tilde char modifier)) )

        (case char
          ((#\u0028)    ; "("
            (format-skip-to-close context #\u0029 index)
            (setf index (ref format-context control-index context)) )
          ((#\<)
            (format-skip-to-close context #\> index)
            (setf index (ref format-context control-index context)) )
          ((#\u005B)    ; "["
            (format-skip-to-close context #\u005D index)
            (setf index (ref format-context control-index context)) )
          ((#\u007B)    ; "{"
            (format-skip-to-close context #\u007D index)
            (setf index (ref format-context control-index context)) )
          ((#\;)
            (when (char= #\u0029 close-char)
               (format-error-semicolon-paren context) )
            (when (char= #\u007D close-char)
               (format-error-semicolon-brace context) )
            (return (values tilde char modifier)) )) )) ) )


;;;; format-skip-to-close
;;;
;;; Description:
;;;  Skips control string until close-char.
(defun format-skip-to-close (context close-char open-index)
  (loop
    (multiple-value-bind (tilde stop-char)
        (format-skip-clause context close-char open-index)
        (declare (ignore tilde))
      (when (char= stop-char close-char) (return)) )) )


;;;; format-slashed-symbol
(defun format-slashed-symbol (context)
    (declare (values symbol))
    (declare (type format-context context))
  (let* ((start    (ref format-context control-index context))
         (ctrl-str (ref format-context control-string context))
         (end
           (position #\/ ctrl-str
                     :start (ref format-context control-index context)
                     :end   (ref format-context control-end   context) )) )

    (unless end (format-error context "Missing '/'."))

    (setf (ref format-context control-index context) (1+ end))

    (multiple-value-bind (symbol-name package-name)
        (let ((colon (position #\: ctrl-str :start start :end end)))
               (cond
                 ((not colon)
                   (values (subseq ctrl-str start end) "CL-USER") )

                 ((= colon end)
                   (format-error context "Missing symbol name.") )
                 (t
                   (incf colon)
                   (let ((ncolons 1))
                     (when (char= #\: (schar ctrl-str colon))
                       (incf colon)
                       (incf ncolons) )
                     (values (subseq ctrl-str colon end)
                             (subseq ctrl-str start (- colon ncolons)) ) )) ) )

      (let ((package (find-package (nstring-upcase package-name))))
        (unless package
          (format-error context "There is no such package: ~S" package-name) )

        (setq symbol-name (nstring-upcase symbol-name))

        (multiple-value-bind (present status)
            (find-symbol symbol-name package)
          (unless status
            (format-error context
                   "There is no such symbol ~S in package ~S."
                   symbol-name package ))
          present ) ) ) ) )


;;;; format-start-padding
;;;
;;; Description:
;;;  Prepare for padding output.
;
(defun format-start-padding (stream modifier mincol colinc minpad padchar)
    (declare (type format-stream stream))
    (declare (ignore colinc))
    (check-type stream format-stream)

  (let ((st (ref instance storage stream)))
    (setf (ref format-stream field-nchars st) 0)

    (if (not (logbitp 1 modifier))
        (setf (ref format-stream field-nchars st) 0)
      (progn
        (write-chars padchar minpad stream)
        (decf mincol minpad)
        (let ((buffer (make-pooled-format-string)))
          ;; Allocate larger buffer.
          (when (< (length buffer) mincol)
            (free-pooled-format-string buffer)
            (setq buffer (make-string mincol)) )
          (setf (ref format-stream field-buffer st) buffer) )))

    (setf (ref format-stream field-npads st) mincol) ) )


;;;; format-start-stream
;;;
;;; Called by:
;;;   format-main
;;;
;;; Description:
;;;  Initialize format-stream with state of base-stream.
(defun format-start-stream (stream base-stream)
    ; (assert (not (eq stream base-stream)))
  (let ((st (ref instance storage stream)))
    (setf (ref format-stream stream st) base-stream)
    (setf (ref format-stream col st)
      (or (stream-line-column base-stream) 0) ) ) )


;;;; format-to-string
;;;
;;; Called by:
;;;   ~< ... ~>
;;;
;;; Description:
;;;  Executes format control string between ctrl-start and ctrl-end and
;;;  returns result string.
(defun format-to-string (stream context ctrl-start ctrl-end)
  (let* ((st (ref instance storage stream))
         (save-stream (ref format-stream stream st))
         (save-col    (ref format-stream col    st)) )
    (unwind-protect
        (with-output-to-string (stream-2)
            (setf (ref format-stream stream st) stream-2)
            (setf (ref format-stream col    st) 0)
            (format-interpreter-1 stream context ctrl-start ctrl-end) )
      (setf (ref format-stream stream st) save-stream)
      (setf (ref format-stream col    st) save-col) ) ) )


;;;; format-write-char
;;;
;;; Description:
;;;  Write chararacter on format-stream.
(defun format-write-char (char stream)
    (declare (type character     char))
    (declare (type format-stream stream))
  (let ((st (ref instance storage stream)))
  (labels (
    ;; convert
    (convert (char)
      (ecase (ref format-stream case-conv st)
        ((nil) char)
        ((:downcase) (char-downcase char))
        ((:upcase)   (char-upcase   char))
        ((:cap-all-1)
           (when (alphanumericp char)
             (setf (ref format-stream case-conv st) :cap-all-2) )
           (char-upcase char) )
        ((:cap-all-2)
          (unless (alphanumericp char)
           (setf (ref format-stream case-conv st) :cap-all-1) )
          (char-downcase char) )
        ((:cap-one)
          (when (alphanumericp char)
         (setf (ref format-stream case-conv st) :downcase) )
          (char-upcase char) )) )
    )
    ;;
    (let ((char (convert char))
           (buffer (ref format-stream field-buffer st))
           (npads  (ref format-stream field-npads  st)) )
      (setf (ref format-stream field-npads st) (decf npads))
      (if (not buffer)
          (progn
            (if (char/= #\Newline char)
                (incf (ref format-stream col st))
              (setf (ref format-stream col st) 0) )
            (stream-write-char (ref format-stream stream st) char) )
        (let ((nchars (ref format-stream field-nchars st)))
          (setf (schar buffer nchars) char)
          (setf (ref format-stream field-nchars st) (incf nchars))
          (when (zerop npads)
            (setf (ref format-stream field-buffer st) nil)
            (loop for index from 0 below nchars do
              (format-write-char (schar buffer index) stream) )
            (free-pooled-format-string buffer) ))) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Format Directives
;;;;

;;;; 22.3.9.3  Tilde Newline: Ignored Newline
;;;
;;; ~newline    = ignores newline and any following non-newline whitespace.
;;; ~:newline   = ignores newline.
;;; ~@newline   = newline, ignore following any whitespace.
(define-format #\Newline ()
  (let ((ctrl-string (ref format-context control-string context))
        (ctrl-end    (ref format-context control-end    context))
        (ctrl-index  (ref format-context control-index  context)) )
      (declare (type simple-string  ctrl-string))
      (declare (type ext:sequence-index ctrl-end))
      (declare (type ext:sequence-index ctrl-index))
  (ecase (modifier :colon :at-sign)
    ((0)    ; ~newline
      (loop when (= ctrl-end ctrl-index) do (return)
            do (let ((char (schar ctrl-string ctrl-index)))
                 (when (or (char= #\Newline char)
                           (not (whitespace-char-p char)) )
                   (return) ) )
               (incf ctrl-index) )
      (setf (ref format-context control-index context) ctrl-index) )
    ((1)    ; ~:newline
     )
    ((2)    ; ~@newline
      (stream-terpri stream)
      (loop when (= ctrl-end ctrl-index) do (return)
            unless (whitespace-char-p (schar ctrl-string ctrl-index))
                do (return)
            do (incf ctrl-index) )
      (setf (ref format-context control-index context) ctrl-index) )) ) )


;;;; 22.3.1.2 Tilde Percent: Newline
;;;; 22.3.1.4 Tilde Vertical-Bar: Page
;;;; 22.3.1.5 Tilde Tilde Tilde
(macrolet (
  (define-format-basic (char out-char)
    `(define-format ,char ((n 1))
       (modifier nil)
       (write-chars ,out-char n stream) ) )
  )
  ;;
  (define-format-basic #\% #\Newline)
  (define-format-basic #\| #\Page)
  (define-format-basic #\~ #\~)
 ) ; macrolet


;;;; 22.3.1.3 Tilde Ampersan: Fresh-Line
(define-format #\& ((n 1))
  (modifier nil)
  (format-fresh-line stream n) )


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
(define-format #\u0028 ()
  (let* ((clause-start (ref format-context control-index context))
         (clause-end   (format-skip-clause context #\u0029 clause-start))
         (st (ref instance storage stream)) )
    (if (typep stream 'format-stream)
        ;; Case conversion on format-stream
        ;;
        (if (ref format-stream case-conv st)
            (format-interpreter-1 stream context clause-start clause-end)
          (progn
            (setf (ref format-stream case-conv st)
                  (ecase (modifier)
                    (0 :downcase)
                    (1 :cap-all-1)
                    (2 :cap-one)
                    (3 :upcase) ))
             (unwind-protect
                 (format-interpreter-1 stream context
                                       clause-start clause-end )
               (setf (ref format-stream case-conv st) nil) )))

      ;; Case conversion on normal stream.
      ;;
      (let ((modifier (modifier))   ; save modifier for ~(...~)
            (string
              (with-output-to-string (stream)
                (format-interpreter-1 stream context
                                      clause-start clause-end ))))
        (ecase modifier
          (0 (setq string (nstring-downcase     string)))
          (1 (setq string (nstring-capitalize   string)))
          (2 (setf string (nstring-capitalize-1 string)))
          (3 (setq string (nstring-upcase string))) )

        (stream-write-string stream string) )) ) )


;;;; 22.3.8.2  Tilde Right-Paren: End of Case Conversion
;;;; 22.3.6.3  Tilde Greater-Than-Sign: End of Justification
;;;; 22.3.7.3  Tilde Right-Bracket: End of Conditional Expression
;;;; 22.3.7.5  Tilde Right-Brace: End of Iteration
;
(labels (
  (error-mismatched (context char)
    (format-error context "Mismatched ~~~C." char ) )
  )
  ;;
  (define-format #\u0029 () (error-mismatched context #\u0029))
  (define-format #\>     () (error-mismatched context #\>))
  (define-format #\u005D () (error-mismatched context #\u005D))
  (define-format #\u007D () (error-mismatched context #\u007D))
 ) ; lables


;;;; 22.3.7.1 Tilde Asterisk: Go-To
;;;
;;; Variations:
;;;   ~n*     skip next args
;;;   ~n:*    backup previous args
;;;   ~n@*    go to nth arg
(define-format #\* ((n (if (eql 2 (modifier)) 0 1)))
  (labels (
    (index-error (n min max+1)
      (format-error context
             "Index ~D must be [0, ~D]."
             n min (1- max+1) ) )
    )
    ;;
    ;; #\*
    ;;
    (ecase (modifier :colon :at-sign)
      (0    ; ~n*
        (unless (and (integerp n) (not (minusp n)))
          (index-error n 0 (length (rest-args))) )

        (loop for arg-scan on (rest-args)
              repeat n
              when (null arg-scan)
                do (index-error n 0 (length (rest-args)))
              finally
                (setf (ref format-context arg-scan context) arg-scan) ) )

      (1    ; ~:*
        (let ((nth 0))
          (loop for arg-scan on (ref format-context arg-list context)
                until (eq arg-scan (ref format-context arg-scan context))
                do (incf nth) )

          (unless (and (integerp n) (not (minusp n)))
            (index-error n 0 nth) )

          (loop for arg-scan on (ref format-context arg-list context)
                repeat (- nth n)
                when (null arg-scan)
                  do (index-error n 0 nth)
                finally
                  (setf (ref format-context arg-scan context) arg-scan) ) ) )

      (2    ; ~n@*
        (unless (and (integerp n) (not (minusp n)))
          (index-error n 0 (length (all-args))) )

        (loop for arg-scan on (all-args)
            repeat n
            when (null arg-scan)
              do (index-error n 0 (length (all-args)))
            finally
              (setf (ref format-context arg-scan context) arg-scan) ) )) ) )


;;;; 22.3.5.4 Tilde Slash: Call Function
(define-format #\/ (*)
  (let ((symbol (format-slashed-symbol context)))
    (apply symbol
           stream
           (next-arg)
           (logbitp 0 (modifier))   ; colon
           (logbitp 1 (modifier))   ; at-sign
           (ref format-context params context) ) ) )


;;;; 22.3.9.1  Tilde Semicolon: Clause Separator
(define-format #\; ()
  (format-error context "Use ~~; out side of bracket.") )


;;;; 22.3.6.2 Tilde Less-Than-Sign: Justification
;;;
;;; Variations:
;;;   ~< left ~; ... ~>
;;;   ~:<left ~; ... ~>         Insert spaces before the first text.
;;;   ~@<left ~; ... ~>         Insert spaces after the last text.
;;;   ~< overflow ~n,w:; ...>   Overflow text
;;;
;;;; 22.3.5.2 Tilde Less-Than-Sign: Logical Block
(define-format #\< ((mincol 0)
                    (colinc 1)
                    (minpad 0)
                    (padchar #\Space) )
  (let ((clause1-modifier 0)
        (clauses          '())
        (close-modifier   0) )
      (declare (type (unsigned-byte 2) clause1-modifier))
      (declare (type list clauses))

    ;; Looking for Right angle bracket
    ;;
    (let* ((clause1-start (ref format-context control-index context))
           (clause-start  clause1-start) )
      (loop
        (multiple-value-bind (tilde stop-char modifier)
            (format-skip-clause context #\> clause1-start)

          (push (cons clause-start tilde) clauses)

          (when (char= #\> stop-char)
            (setq close-modifier modifier)
            (return) )

          (when (null (rest clauses))
            (setq clause1-modifier modifier) )

          (setq clause-start (ref format-context control-index context)) ))
      (setq clauses (nreverse clauses)) )

    ;; Process segments
    ;;
    (cond
      ((logbitp 0 close-modifier)
        ;; ~< prefix ~; body ~; suffix ~:>
        ;;
        (when (params)
          (format-error-too-many-params context) )

        (unless (<= 1 (length clauses) 3)
          (format-error-pp-malformed context) )

        ;; Check modifier of the first ~;.
        (when (logbitp 0 clause1-modifier)
          (setf (ref format-context control-index context)
                (car (second clauses)) )
          (format-error-invalid-modifier context) )

        (let ((arg (if (not (logbitp 1 (modifier)))
                       (next-arg)
                     (prog1
                       (rest-args)
                       (setf (ref format-context arg-scan context) nil) )))
              (suffix (cond
                        ((= 3 (length clauses))
                          (subseq (ref format-context control-string context)
                                  (car (third clauses))
                                  (cdr (third clauses)) ) )

                        ((logbitp 0 (modifier))
                          ")" )
                        (t
                          "" )) )
              (prefix (cond
                        ((>= (length clauses) 2)
                          (let ((clause-1 (pop clauses)))
                            (subseq (ref format-context control-string context)
                                    (car clause-1)
                                    (cdr clause-1) ) ) )

                        ((logbitp 0 (modifier))
                          "(" )
                        (t
                          "" )) )
              (ctrl-string (ref format-context control-string context))
              (ctrl-start  (car (first clauses)))
              (ctrl-end    (cdr (first clauses))) )

          ;; ~< ... ~:@>
          ;;
          (when (logbitp 1 close-modifier)
            (setq ctrl-string (format-insert-fill ctrl-string
                                                  ctrl-start ctrl-end ))
            (setq ctrl-start 0)
            (setq ctrl-end   (length ctrl-string)) )

          ;; Call pretty-printer
          ;;
          (if (not (listp arg))
              ;; Simple optimization
              (write-object arg stream)
            (xc::pprint-logical-block-aux (stream stream
                                       arg
                                       prefix (logbitp 1 clause1-modifier)
                                       suffix )
              (catch 'up-and-out
                (format-interpreter-4
                  stream context
                  ctrl-string ctrl-start ctrl-end
                  arg
                  #'(lambda () (pprint-pop)) )) )) ) )

      ((logbitp 0 clause1-modifier)
        ;; ~< overflow ~:; left ~; text ... ~; right ~>
        ;;
        (when (logbitp 1 clause1-modifier)
          (format-error-invalid-modifier context) )

        (let ((segments '())
              (spare    0)
              (width    72) )

          (catch 'up-and-out
            (loop
              for scan on clauses
              for (ctrl-start . ctrl-end) = (first scan)
              do
                (push (format-to-string stream context ctrl-start ctrl-end)
                       segments )

                 ;; Parse the first "~;"
                 ;;
                 (when (eq clauses scan)
                   (setq ctrl-start (1+ ctrl-end))
                   (setq ctrl-end   (car (second clauses)))
                   (let ((nparams 0))
                     (loop
                       (multiple-value-bind (stop-char param ctrl-index)
                           (format-parse-param context ctrl-start ctrl-end)
                         (unless (= (1+ ctrl-start) ctrl-index)
                           (incf nparams)
                           (case nparams
                             (1 (setq spare param))
                             (2 (setq width param))
                             (otherwise
                               (setf (ref format-context control-index context)
                                     ctrl-index )
                               (format-error-too-many-params context) )))
                         (unless (char= #\, stop-char) (return))
                         (setq ctrl-start ctrl-index) )) ))))

          (setq segments (nreverse segments))

          (format-justify stream (rest segments)
                          (first segments) spare width
                          (modifier) mincol colinc minpad padchar ) ) )

      (t
        ;; ~< left ~; text ... ~; right ~>
        ;;
        (when (logbitp 1 clause1-modifier)
          (format-error-invalid-modifier context) )

        (let ((segments '()))
          (catch 'up-and-out
            (loop
              for (ctrl-start . ctrl-end) in clauses do
                (push (format-to-string stream context ctrl-start ctrl-end)
                       segments )))
          (setq segments (nreverse segments))

          (format-justify stream segments
                          nil 0 0
                          (modifier) mincol colinc minpad padchar ) ) )) ) )


;;;; 22.3.7.6  Tilde Question-Mark: Recursive Processing
(define-format #\? ()
  (multiple-value-bind (ctrl-string ctrl-start ctrl-end)
      (string-data (next-arg))
    (ecase (modifier :at-sign)
      ((0)  ; ~?
        (let ((args     (next-arg))
              (arg-list (ref format-context arg-list context))
              (arg-scan (ref format-context arg-scan context)) )
          (setf (ref format-context arg-list context) args)
          (setf (ref format-context arg-scan context) args)

          (format-interpreter-2
            stream context ctrl-string ctrl-start ctrl-end )

          (setf (ref format-context arg-list context) arg-list)
          (setf (ref format-context arg-scan context) arg-scan) ) )

      ((2)  ; ~@?
        (format-interpreter-2
          stream context ctrl-string ctrl-start ctrl-end ) )) ) )


;;;; 22.3.4.1 Tilde A: Aesthetic
;;;
;;; :   nil => ()
;;; @   right
;
(define-format #\A ((mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  (format-princ stream (next-arg) (modifier) mincol colinc minpad padchar) )


;;;; 22.3.2.3 Tilde B: Binary
;;;; 22.3.2.2 Tilde D: Decimal
;;;; 22.3.2.4 Tilde O: Octal
;;;; 22.3.2.5 Tilde X: Hexadecimal
;
(macrolet (
  (define-format-radix-control (char base)
    `(define-format ,char ((mincol         0)
                           (padchar        #\Space)
                           (commachar      #\,)
                           (comma-interval 3) )
       (format-integer stream
                       (next-arg)
                       (modifier)
                        ,base
                        mincol
                        padchar
                        commachar
                        comma-interval ) ) )
  )
  ;;
  (define-format-radix-control #\B  2)
  (define-format-radix-control #\D 10)
  (define-format-radix-control #\O  8)
  (define-format-radix-control #\X 16)
 ) ; macrolet


;;;; 22.3.1.1 Tilde C: Character
(define-format #\C ()
  (format-character stream (next-arg) (modifier)) )


;;;; 22.3.5.3 Tilde I: Indent
;;;
;;; ~I      pprint-indent :block
;;; ~:I     pprint-indnet :current
;
(define-format #\I ((n 0))
  (pprint-indent (ecase (modifier :colon)
                   (0 :block)       ; ~I
                   (1 :current) )   ; ~:I
                 n
                 stream ) )


;;;; 22.3.8.3 Tilde P: Plural
;;;
;;;  ~P     "s"
;;;  ~:P    ~:*, "s"
;;;  ~@P    "ies"
;;;  ~:@P   ~:*, "ies"
;
(define-format #\P ()
  (labels (
    (prev-arg ()
      (let (prev)
        (loop for scan on (all-args)
              with arg-scan = (ref format-context arg-scan context)
              until (eq arg-scan scan)
                 do (setq prev scan) )

        (when (null prev)
          (format-error context "No arguments for backup.") )
        (setf (ref format-context arg-scan context) (rest prev))
        (first prev) ) )
    )
    ;;
    (ecase (modifier)
      (0 (unless (eql (next-arg) 1) (stream-write-char stream #\s)))    ; ~P
      (1 (unless (eql (prev-arg) 1) (stream-write-char stream #\s)))    ; ~:P
      (2 (if (eql 1 (next-arg))                                         ; ~@P
             (stream-write-char stream #\y)
           (stream-write-string stream "ies") ) )
      (3 (if (eql (prev-arg) 1)                                         ; ~:@P
             (stream-write-char stream #\y)
           (stream-write-string stream "ies") )) ) ) )


;;;; 22.3.2.1 Tilde R: Radix
;;;
;;; ~R      cardinal
;;; ~:R     ordinal
;;; ~@R     roman
;;; ~:@R    old-roman
;
(define-format #\R ((base           10)
                    (mincol         0)
                    (padchar        #\Space)
                    (commachar      #\,)
                    (comma-interval 3) )
  (let ((modifier (modifier))
        (arg      (next-arg)) )
    (if (params)
        (format-integer stream
                        arg
                        modifier
                        base
                        mincol
                        padchar
                        commachar
                        comma-interval )
      (ecase modifier
        (0 (format-cardinal  stream arg))           ; ~R
        (1 (format-ordinal   stream arg))           ; ~:R
        (2 (format-roman     stream arg))           ; ~@R
        (3 (format-old-roman stream arg)) )) ) )    ; ~:@R


;;;; 22.3.4.1 Tilde S: Standard
;;;
;;; :   nil => ()
;;; @   right
;
(define-format #\S ((mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  (format-prin1 stream (next-arg) (modifier) mincol colinc minpad padchar) )


;;;; 22.3.6.1 Tilde T: Tabulate
;
(define-format #\T ((n 1) (m 1))
  (ecase (modifier)
    (0  ; ~column,colincT
      (format-absolute-tab stream n m) )
    (1  ; ~n,m:T
      (pprint-tab :section n m stream) )
    (2  ; ~colrel,colinc@T
      (format-relative-tab stream n m) )
    (3  ; ~n,m:@T
      (pprint-tab :section-relative n m stream) )) )


;;;; 22.3.4.3 Tilde W: Write
;;;
;;; ~W      write-object
;;; ~:W     *print-pretty* <= t
;;; ~@W     *print-level* <= nil, *print-length* <= nil
;;; ~:@W    *print-level* <= nil, *print-length* <= nil, prtty-print* <= t
;
(define-format #\W ()
  (ecase (modifier)
    ((0) ; ~W
      (write-object (next-arg) stream) )
    ((1) ; ~:W
      (let ((*print-pretty* t))
        (write-object (next-arg) stream) ) )
    ((2) ; ~@W
      (let ((*print-level*  nil)
            (*print-length* nil) )
        (write-object (next-arg) stream) ) )
    ((3) ; ~:@W
      (let ((*print-level*  nil)
            (*print-length* nil)
            (*print-pretty* t) )
        (write-object (next-arg) stream) ) )) )


;;;; 22.3.7.2  Tilde Left-Bracket: Conditional Expression
;;;
;;; Variations:
;;;   ~[str0~;str1~;...~;strn~]
;;;   ~:[alternative~;consequent~]
;;;   ~@[consequent~]
(define-format #\u005B ((n 0))
  (ecase (modifier :colon :at-sign)
    ((0) ; ~[str0~;str1~;...~;strn~]
      (let ((clause-start   0)
            (clause-end     0) )

        (when (null (params)) (setq n (next-arg)))

        ;; Looking for Right Bracket
        ;;
        (let ((open-index    (ref format-context control-index context))
              (last-start    (ref format-context control-index context))
              (last-modifier  0) )
          (loop
            (multiple-value-bind (tilde stop-char modifier)
                (format-skip-clause context #\u005D open-index)

              (when (zerop n)
                (setq clause-start last-start)
                (setq clause-end   tilde) )

              (decf n)

              (when (char= #\u005D stop-char)
                   (when (and (zerop clause-start) (logbitp 0 last-modifier))
                     (setq clause-start last-start)
                     (setq clause-end   tilde) )
                   (return) )

                 (setq last-start   (ref format-context control-index context))
                 (setq last-modifier modifier) )) )

        (when (plusp clause-start)
          (format-interpreter-1 stream context
                                       clause-start clause-end )) ) )

    ((1) ; ~:[alternative~;consequent~]
      (let* ((alt-start (ref format-context control-index context))
             (alt-end
               (multiple-value-bind (tilde stop-char)
                   (format-skip-clause context #\u005D alt-start)
                 (when (char= #\u005D stop-char)
                   (format-error-clause-count context 2) )
                 tilde ) )
             (con-start (ref format-context control-index context))
             (con-end
               (multiple-value-bind (tilde stop-char)
                   (format-skip-clause context #\u005D alt-start)
                 (unless (char= #\u005D stop-char)
                   (format-error-clause-count context 2) )
                 tilde ) ) )
        (if (next-arg)
            (format-interpreter-1 stream context
                                          con-start con-end )
          (format-interpreter-1 stream context
                                        alt-start alt-end ) ) ) )

    ((2) ; ~@[consequent~]
      (let*  ((con-start (ref format-context control-index context))
              (con-end
                (multiple-value-bind (tilde stop-char)
                    (format-skip-clause context #\u005D con-start)
                  (unless (char= #\u005D stop-char)
                    (format-error-clause-count context 1) )
                  tilde ) )
              (arg-scan (rest-args)) )

        (when (next-arg)
          (setf (ref format-context arg-scan context) arg-scan)
          (format-interpreter-1 stream context
                                        con-start con-end )) ) )) )


;;;; 22.3.9.2  Tilde Circumflex: Escape Upward
;;;
;;; ~^      up-and-out
;;; ~:^     up-up-and-out
(define-format #\^ ((param-1 0) (param-2 0) (param-3 0))
  (let* ((modifier (modifier :colon))
         (throw-p (ecase (length (params))
                     (0 (if (logbitp 0 modifier)
                            (null (ref format-context outer-scan context))
                          (null (ref format-context arg-scan context)) ) )
                     (1 (zerop param-1))
                     (2 (= param-1 param-2))
                     (3 (<= param-1 param-2 param-3)) ) ))
    (ecase modifier
      (0  ; ~^
        (when throw-p (throw 'up-and-out t)) )
      (1  ; ~:^
        (unless (ref format-context up-up-and-out-p context)
          (format-error context
                 "~~:^ is used out side of ~~:{...~~}." ))
        (when throw-p (throw 'up-up-and-out t)) )) ) )


;;;; 22.3.5.1. Tilde Underscore: Conditional Newline
;;;
;;;   ~_    pprint-newline :linear
;;;   ~:_   pprint-newline :fill
;;;   ~@_   pprint-newline :miser
;;;   ~:@_  pprint-newline :mandatory
(define-format #\_ ()
  (pprint-newline (ecase (modifier)
                    (0 :linear)         ; ~_
                    (1 :fill)           ; ~:_
                    (2 :miser)          ; ~@_
                    (3 :mandatory) )    ; ~:@
                  stream ) )


;;;; 22.3.7.4  Tilde Left-Brace: Iteration
;;;
;;; ~{str}  = loop in list argument.
;;; ~:{str} = loop in sublist argument.
;;; ~@{str} = loop in rest of arguments.
;;; ~:{str} = loop in sublists of rest of arguments.
;;;
;;; ~{...~:} = loop at least once.
(define-format #\u007B ((n nil))
  (let* ((ctrl-start   (ref format-context control-index context))
         (ctrl-end     ctrl-start)
         (ctrl-string  (ref format-context control-string context))
         (least-once-p  nil) )

    (multiple-value-bind (tilde stop-char modifier)
        (format-skip-clause context #\u007D ctrl-start)
        (declare (ignore stop-char))
      (setq ctrl-end tilde)

      (when (logbitp 0 modifier)
        (setq least-once-p t) ) )

    (when (eq ctrl-start ctrl-end)
       (multiple-value-setq (ctrl-string ctrl-start ctrl-end)
         (string-data (next-arg)) ))

    (ecase (modifier)
      (0    ; ~{...~}
        (let ((args (next-arg)))
          (catch 'up-and-out
            (loop
              (cond
                (least-once-p (setq least-once-p nil))
                ((null args)  (return))
                (n            (when (minusp (decf n)) (return))) )

              (setq args (format-interpreter-3
                          stream context
                          ctrl-string ctrl-start ctrl-end
                          args ))) ) ) )

      (1    ; ~:{...}
        (let ((save-up-up-p (ref format-context up-up-and-out-p context))
              (save-outer   (ref format-context outer-scan      context)) )

          (setf (ref format-context outer-scan context) (next-arg))

          (setf (ref format-context up-up-and-out-p context) t)

          (catch 'up-up-and-out
            (loop
              (cond
                (least-once-p (setq least-once-p nil))
                ((null (ref format-context outer-scan context)) (return))
                (n (when (minusp (decf n)) (return))) )

              (catch 'up-and-out
                (format-interpreter-3
                    stream context
                    ctrl-string ctrl-start ctrl-end
                    (pop (ref format-context outer-scan context)) ))))

          (setf (ref format-context outer-scan context)      save-outer)
          (setf (ref format-context up-up-and-out-p context) save-up-up-p) ) )

      (2    ; ~@{...~}
        (catch 'up-and-out
          (loop
            (cond
              (least-once-p (setq least-once-p nil))
              ((null (ref format-context arg-scan context)) (return))
              (n (when (minusp (decf n)) (return))) )

            (format-interpreter-2
              stream context
              ctrl-string ctrl-start ctrl-end ))) )

      (3    ; ~:@{...~}
        (let ((save-up-up-p (ref format-context up-up-and-out-p context))
              (save-outer   (ref format-context outer-scan      context)) )

          (setf (ref format-context outer-scan context) (rest-args))
          (setf (ref format-context up-up-and-out-p context) t)

          (catch 'up-up-and-out
            (loop
              (cond
                (least-once-p (setq least-once-p nil))
                ((null (ref format-context outer-scan context)) (return))
                (n (when (minusp (decf n)) (return))) )

              (catch 'up-and-out
                (format-interpreter-3
                    stream context
                    ctrl-string ctrl-start ctrl-end
                    (pop (ref format-context outer-scan context)) ))))

          (setf (ref format-context up-up-and-out-p context) save-up-up-p)

          (shiftf (ref format-context arg-scan   context)
                  (ref format-context outer-scan context)
                  save-outer ) ) )) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;

;;;; 22.4.31 format
;;;
;;; Note: args will be stored into format-context.
(defun cl:format (destination control-string &rest args)
    (declare (dynamic-extent args))
  (labels (
    (format-aux (stream control-string args)
      (if (or (not *print-pretty*) (typep stream 'pp-stream))
          (format-main stream control-string args)
        (let ((pp-stream (pprint-start stream)))
          (catch 'line-abbreviation
            (format-main pp-stream control-string args) )
          (pprint-end pp-stream) )) )
    )
    ;;
    ;; format
    ;;
    (etypecase destination
      ((eql t)
        (format-aux *standard-output* control-string  args)
        nil )

      (null
        (with-output-to-string (stream)
          (format-aux stream control-string args) ) )

      (stream
        (format-aux destination control-string args)
        nil )

      (string
        (with-output-to-string (stream destination)
          (format-aux stream control-string args) )
        destination )) ) )
