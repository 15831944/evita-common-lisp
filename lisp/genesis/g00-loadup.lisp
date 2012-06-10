;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - genesis - Load Script
;;; lisp/genesis/g00-loadup.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/genesis/g00-loadup.lisp#18 $
;;;
;;; Description:
;;;  This file contains function definitions for backquote macro.
;
(setq *image-save-time* (get-universal-time))
(setq gc nil)
(setq c::*optimize* '(
    (speed 3) (safety 2) (space 0) (debug 0) (compilation-speed 0) ) )

(setq cl:*load-verbose* t)
(setq cl:*load-print* #+debug-kernel t #-debug-kernel nil)

;; Turn off compiler debug settgins
(setq c6::*settings* nil)

;; Remember we are in genesis.
(setq cl:*features* (cons :genesis cl:*features*))


(setq *stack-overflow-handler*
  (lambda (fn)
    (format t "~&; Stack overflow in ~S~%" fn)
    (exit-process 1) ) )

#+64bit
(unless (eq (type-of +1152921504606846975) 'fixnum)
  (error "most-positive-fixnum") )

#+64bit
(unless (eq (type-of -1152921504606846976) 'fixnum)
  (error "most-negative-fixnum") )

(unless  (null    nil) (error "Nil must be a null."))
(unless  (atom    nil) (error "Nil must be a atom."))
(unless  (symbolp nil) (error "Nil must be a symbol."))
(unless  (listp   nil) (error "Nil must be a list."))
(when    (consp   nil) (error "Nil must be NOT a cons."))
(unless  (consp '(1 . 2)) (error "CONSP is wrong."))
(unless  (listp '(1 . 2)) (error "LISTP is wrong."))
(when    (atom  '(1 . 2)) (error "ATOM is wrong."))

(defun typep (x y) (declare (ignore x y)) t)

(defun *error (datum &rest args)
  (when (null *features*)
    (format t "~2%; Error in error!~2%") (exit-process 1) )
  (setq *features* nil)
  (if (null args)
      (format t "; Error: ~A~%" datum)
    (format t "; Error: ~S ~S~%" datum args) )
  (let ((nth 0))
    (dolist (frob (backtrace))
      (format t "; [~D] ~X ~X~%" (incf nth) (car frob) (cdr frob))) )
  (exit-process 1) )

(setf (symbol-function 'error) #'*error)

(defun stop ()
    (setf (symbol-function 'error) #'*error)
    ;(setq cl:*features* (delete :genesis cl:*features*))
    ;; define error and warn
    ;(load "../../lisp/runtime/r09-condition.lisp")
    (room t)
    (let ((*image-save-time* (get-universal-time)))
      (internal-save-image "_stop.image") )

    (exit-process 1) )

(defun warn (&rest x) (format t "; WARN: ~S~%" x))

(labels (
  (install (name expansion)
    (funcall #'(setf gethash/eq)
        expansion
        name
        (ref environment types *environment*) ) )
  )
  (install 'case-sensitivity-mode
    '(member :upcase :downcase :preserve :invert) )
  (install 'key-function 'function-designator)
  (install 'logical-host-designator '(or logical-host string))
  (install 'c::param-info 'list)
  (install 'pathname-host-designator '(or basic-host string))
  (install 'predicate 'function)
  (install 'readtable-designator '(or readtable null))
  (install 'unspecified t)

  (install 'mod
    (lambda (form env)
        (declare (ignore env))
      (list 'integer 0 (1- (second form))) ) )

  (install 'signed-byte
    (lambda (form env)
        (declare (lambda-name (deftype unsigned-byte)))
        (declare (ignore env))
      (let ((n (second form)))
        (if (not n)
            'integer
          (list 'integer (ash -1 (1- n)) (1- (ash 1 (1- n)))) ) ) ) )

  (install 'unsigned-byte
    (lambda (form env)
        (declare (lambda-name (deftype unsigned-byte)))
        (declare (ignore env))
      (list 'integer 0 (if (second form) (ash 1 (second form)) '*)) ) )
  ;(install 'windows-host-designator '(or windows-host string))
 ) ; labels

(load "../../lisp/genesis/g01-m02-backquote.lisp")
(load "../../lisp/macro/m02-backquote.lisp")
(load "../../lisp/genesis/g02-m00-fns.lisp")

(load "../../lisp/macro/m00-fns.lisp")

;; From here, we can use macrolet.
(load "../../lisp/genesis/g03-m03-eval.lisp")
(load "../../lisp/macro/m03-eval.lisp")
(load "../../lisp/runtime/r03-eval.lisp")

(defun c::macroexpand-hook (expander form env)
  (funcall expander form env) )

(setq cl:*macroexpand-hook* #'c::macroexpand-hook)

;; Change initial value of TLV *macroexpand-hook* to #'c::macroexpand-hook
(let* ((htb (ref environment variables *environment*))
       (alist  (rest (gethash/eq 'cl:*macroexpand-hook* htb)))
       (tlvrec (cdr (assq 'tlv alist))) )
  (unless tlvrec
    (error "~S doesn't have variable information of ~S."
        *environment* '*macroexpand-hook* ))
  (funcall #'(setf %ref) #'c::macroexpand-hook 'tlv-record 'value tlvrec) )

;; From here, we can use define-compiler-macro
(load "../../lisp/genesis/g04-m05-control.lisp")

(load "../../lisp/macro/m05-control.lisp")
(load "../../lisp/devel/d05-control.lisp")

;;; misc
(load "../../lisp/runtime/r00-fns.lisp")

;; for defconstant -> class-description
(load "../../arch/generic/lisp/runtime/gen-r07-object.lisp")

;;; for defun
(load "../../lisp/runtime3/r00-loader.lisp")

(load "../../lisp/macro/m05-place.lisp")
(load "../../lisp/macro/m06-do.lisp")
(load "../../lisp/macro/m06-loop.lisp")


;;; for loop
;(load "../../lisp/cmacro/n00-util.lisp")

(load "../../lisp/genesis/g05-runtime.lisp")

;;; for loop
(load "../../arch/generic/lisp/runtime/gen-r03-env.lisp")
(load "../../arch/generic/lisp/runtime/gen-r04-type.lisp")

;;; for package
(load "../../arch/generic/lisp/runtime/gen-r49-marker.lisp")
(load "../../lisp/genesis/g06-runtime.lisp")

(setf (find-type 'mod)
    (lambda (ty env)
        ;(declare (lambda-name (type-expander mod)))
        (declare (ignore env))
      `(integer 0 ,(1- (second ty))) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load macros
;;;

;;; We don't need defstruct and defclass until runtime functions are
;;; loades
;(load "../../lisp/macro/m07-object.lisp")
;(load "../../lisp/macro/m08-struct.lisp")

(load "../../lisp/macro/m04-type.lisp")
(load "../../lisp/macro/m09-condition.lisp")

; contains null vector in backquote
(load "../../lisp/macro/m11-package.lisp") 
(load "../../lisp/macro/m12-number.lisp")
(load "../../lisp/macro/m14-cons.lisp")
(load "../../lisp/macro/m17-sequence.lisp")
(load "../../lisp/macro/m18-hash-table.lisp")
(load "../../lisp/macro/m21-stream.lisp")
(load "../../lisp/macro/m22-printer.lisp")
(load "../../lisp/macro/m22-pprint.lisp")
(load "../../lisp/macro/m23-reader.lisp")
(load "../../lisp/macro/m24-syscon.lisp")
(load "../../lisp/macro/m25-env.lisp")
;(load "../../lisp/macro/m49-internal.lisp")
(load "../../lisp/macro/m50-lock.lisp")
(load "../../lisp/macro/m50-misc.lisp")
(load "../../lisp/macro/m50-pool.lisp")
;(load "../../lisp/macro/m50-tlv.lisp")

;; FIXME 2007-11-11 yosi@msn.com Where is good place for without-gc?
(defmacro without-garbage-collection (&body form*)
  `(locally ,.form*) )

(load "../../arch/generic/lisp/macro/gen-m11-package.lisp") 

;gc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Runtime Functions
;;;
(load "../../arch/generic/lisp/runtime/gen-r05-control.lisp")
(load "../../arch/generic/lisp/runtime/gen-r09-condition.lisp")
(load "../../arch/generic/lisp/runtime/gen-r10-symbol.lisp")

(load "../../arch/generic/lisp/runtime/gen-r12-float.lisp")
(load "../../arch/generic/lisp/runtime/gen-r12-integer.lisp")
(load "../../arch/generic/lisp/runtime/gen-r12-number.lisp")
(load "../../arch/generic/lisp/runtime/gen-r12-real.lisp")

(load "../../arch/generic/lisp/libm/float32/gen-float32-decode.lisp")
(load "../../arch/generic/lisp/libm/float64/gen-float64-decode.lisp")


(load "../../arch/generic/lisp/runtime/gen-r13-char.lisp")
(load "../../arch/generic/lisp/runtime/gen-r15-array.lisp")
(load "../../arch/generic/lisp/runtime/gen-r15-bitvec.lisp") ; not essential
(load "../../arch/generic/lisp/runtime/gen-r16-string.lisp")
(load "../../arch/generic/lisp/runtime/gen-r17-vector.lisp")
(load "../../arch/generic/lisp/runtime/gen-r49-cell.lisp")
(load "../../arch/generic/lisp/runtime/gen-r50-lock.lisp")

;(load "../../../lisp/runtime/r03-eval.lisp") ; already loaded
;(load "../../../lisp/runtime/r04-type.lisp") ; already loaded
(load "../../lisp/runtime3/r04-coerce.lisp")
(load "../../lisp/runtime/r05-control.lisp")
;(load "../../lisp/runtime/r07-object.lisp")
;(load "../../lisp/runtime/r08-struct.lisp")
(load "../../lisp/runtime/r09-condition.lisp")
(load "../../lisp/runtime/r10-symbol.lisp") ; need string-output-stream
(load "../../lisp/runtime/r11-package.lisp")
(load "../../lisp/runtime3/r12-number.lisp")
(load "../../lisp/runtime3/r12-integer.lisp")

;(load "../../lisp/runtime/r12-integer.lisp")
;(load "../../lisp/runtime/r12-number.lisp")
;(load "../../lisp/runtime/r12-math.lisp")
(load "../../lisp/runtime/r12-byte.lisp")

(load "../../lisp/runtime/r13-char.lisp")
(load "../../lisp/runtime/r14-cons.lisp")
(load "../../lisp/runtime/r14-set.lisp")
(load "../../lisp/runtime/r15-array.lisp")
(load "../../lisp/runtime/r17-fns.lisp")
(load "../../lisp/runtime/r16-string.lisp")
(load "../../lisp/runtime/r17-list.lisp")
(load "../../lisp/runtime/r17-vector.lisp")

(load "../../lisp/runtime/r17-sequence.lisp")

(load "../../arch/generic/lisp/runtime/gen-r18-hash-table.lisp")
(load "../../lisp/runtime/r18-sxhash.lisp")


;; make-pathname
;(load "../../lisp/runtime/r24-module.lisp")
;(load "../../lisp/runtime/r25-env.lisp")
(load "../../lisp/runtime/r25-time.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load CLOS
;;;

;;; make accessors defined in defclass form.
(load "../../lisp/genesis/g07-clos-slot.lisp")
(load "../../lisp/clos/o10-defclass.lisp")
(load "../../lisp/runtime/stream/r21-stream-defs.lisp")
(load "../../lisp/ffi/ffi-defs.lisp")

;; BUGBUG: We should move declaration of debug-output-stream to another
;; place.
(defclass debug-output-stream (platform-stream)
  ((flags :initform STREAM-FLAG-OUTPUT)) )

(load "../../lisp/runtime/r19-defs.lisp")
(load "../../lisp/runtime/r19-defs-logical.lisp")
(load "../../lisp/runtime/r19-defs-windows.lisp")

;;; accessors are ready to use.

(load "../../lisp/macro/m07-object.lisp")
(load "../../lisp/macro/m08-struct.lisp")

(load "../../lisp/runtime3/r07-object.lisp")

(load "../../lisp/clos/o04-emf.lisp")
(load "../../lisp/clos/o05-dfun.lisp")
(load "../../lisp/clos/o06-cdfun.lisp")


;;; g08-clos-method.lisp defines stab version of
;;;     make-instance
(load "../../lisp/genesis/g08-clos-method.lisp")

(load "../../lisp/clos/o11-method.lisp")
(load "../../lisp/clos/o12-class.lisp")
(load "../../lisp/clos/o13-slot.lisp")


(load "../../lisp/genesis/g09-clos-gf.lisp")

;; Install discriminator except for GF function-name, since it is used in
;; make-discriminator/dispatch.
(labels (
  (try (x)
    (let ((gf (and (fboundp x) (fdefinition x))))
      (when (typep gf 'generic-function)
        (when *load-print* (format t "; Install discriminator to ~S~%" gf))
        (let ((dfun (compute-discriminating-function gf)))
          (when *load-print* (format t ";   discriminator is ~S~%" dfun))
          (set-funcallable-instance-function gf dfun) )) ) )
  )
  (let ((setf-name (list 'setf 0)) (*load-print* *load-verbose*))
   (do-all-symbols (x)
     (unless (eq x 'function-name)
       (try x)
       (setf (second setf-name) x)
       (try setf-name) ) ) ) )

(load "../../lisp/clos/o14-gf.lisp")
(load "../../lisp/clos/o19-forward-referenced-class.lisp")
(load "../../lisp/clos/o20-struct.lisp")
(load "../../lisp/clos/o21-built-in-class.lisp")

(load "../../arch/generic/lisp/runtime/gen-r07-clos.lisp")
(load "../../arch/generic/lisp/runtime/gen-r49-classd.lisp")
(load "../../arch/generic/lisp/runtime/gen-r08-struct.lisp")

;;; From here, we can use defstruct.
(load "../../lisp/devel3/d03-proclaim.lisp")

;;; 19 Filenames
(load "../../lisp/runtime/r19-filename.lisp")
(load "../../lisp/runtime/r19-logical.lisp")
(load "../../lisp/runtime/r19-windows.lisp")

;; save-image uses namestring.
(load "../../lisp/runtime3/r49-image.lisp")

;;; 21 Streams
(load "../../lisp/runtime/stream/r21-broadcast-stream.lisp")
(load "../../lisp/runtime/stream/r21-concatenated-stream.lisp")
(load "../../lisp/runtime/stream/r21-echo-stream.lisp")
(load "../../lisp/runtime/stream/r21-file-stream.lisp")
(load "../../lisp/runtime/stream/r21-file-stream.lisp")
(load "../../lisp/runtime/stream/r21-string-input-stream.lisp")
(load "../../lisp/runtime/stream/r21-string-output-stream.lisp")
(load "../../lisp/runtime/stream/r21-synonym-stream.lisp")
(load "../../lisp/runtime/stream/r21-two-way-stream.lisp")

(load "../../lisp/runtime/stream/r21-stream-method.lisp")
(load "../../lisp/runtime/stream/r21-stream-api.lisp")

;; Set descriminator for stream generic function.
;; Check function name is (:DISCRIMINATOR).

(load "../../lisp/runtime3/r50-finalization.lisp")
(load "../../lisp/runtime3/r50-weak.lisp")

(defmethod function-name ((fun native-code-function))
  (ref native-code-function name fun) )

#+nil
(labels (
  ;; install-discriminator
  (install-discriminator (name)
    (let ((gf (and (fboundp name) (fdefinition name))))
      (when (typep gf 'standard-generic-function)
        (format t "; Install discriminator for ~S~%" name)
        (clos:set-funcallable-instance-function
              gf
              (compute-discriminator gf) )) ) )
  )
  (format t "~%")
  (dolist (name '(
        ;realize-instance
        ;unrealize-instance

        ; 19 Filenames
        internal-directory-namestring
        internal-enough-namestring
        internal-file-namestring
        internal-namestring
        make-pathname-using-host
        parse-device-component
        parse-directory-component
        parse-name-component
        parse-namestring-using-host
        parse-type-component
        parse-version-component
        pathname-equal-using-host
        pathname-match-p-using-host
        translate-component-using-host
        translate-directory-using-host
        translate-pathname-using-host
        truename-using-host
        wild-pathname-p-using-host

        ;; 21 Streams
        stream-advance-to-column
        stream-clear-input
        stream-clear-output
        stream-finish-output
        stream-force-output
        stream-fresh-line
        stream-line-number
        stream-line-column
        stream-listen
        stream-output-width
        stream-peek-char
        stream-read-byte
        stream-read-bytes
        stream-read-char
        stream-read-char-no-hang
        stream-read-line
        stream-start-line-p
        stream-terpri
        stream-unread-char
        stream-write-byte
        stream-write-bytes
        stream-write-char
        stream-write-string
        stream-yes-or-no-p
        stream-y-or-n-p
   ) )
   (install-discriminator name) ) )


;;; install discriminators
(defun install-discriminators ()
  (labels (
    (install (gf)
      (when (need-install-p gf)
        (when *load-verbose* (format t "; Install discriminator for ~S~%" gf))
        (set-funcallable-instance-function
            gf
            (compute-discriminator gf) )) )

    (need-install-p (gf)
      (let ((dfun (funcallable-instance-function gf)))
        (and (equal (function-name dfun) '(:discriminator))
             (notany #'(lambda (method)
                          (let ((class (class-of method)))
                            (or (eq (find-class 'standard-reader-method)
                                     class )
                                (eq (find-class 'standard-writer-method)
                                     class )) ) )
                    (generic-function-methods gf) )) ) )

    (process-class (class)
      (dolist (method (specializer-direct-methods class))
        (install (method-generic-function method)) )
      (dolist (subclass (class-direct-subclasses class))
        (process-class subclass) ) )
    )
    (process-class (find-class 't)) ) )

(install-discriminators)

;;; Reader

;;; for reader
(setf (symbol-function 'make-structure)
    #'make-instance )

(load "../../lisp/runtime/r23-defs.lisp")
(load "../../lisp/runtime/r23-fns.lisp")
(load "../../lisp/runtime/r23-macrochar.lisp")

;; for set-macro-character
(load "../../lisp/runtime/r23-readtable.lisp")
(load "../../lisp/runtime/r23-reader.lisp")

;; for open => ensure-physical-pathname
(load "../../lisp/runtime/r20-file.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printer
;;;

;;; for format-stream
(load "../../lisp/macro3/m50-ref.lisp")

;; Formatter uses defstruct.
(load "../../lisp/macro/m22-defs-formatter.lisp")
(load "../../lisp/macro/m22-formatter.lisp")

(loop for base from 2 to 36  do
  (loop for power-1 = -1 then (1+ power-1)
        and new-divisor = base then (* new-divisor base)
        and divisor = 1 then new-divisor
        while (typep new-divisor 'fixnum)
        do (setf (aref *bignum-divisor-vector* base) divisor)
           (setf (aref *fixnum-ndigits-vector* base) (1+ power-1)) ))

(load "../../lisp/runtime/r22-defs-printer.lisp")

(load "../../lisp/runtime/r22-printer-api.lisp")
(load "../../lisp/runtime/r22-printer.lisp")
(load "../../lisp/runtime/r22-print-object.lisp")

(load "../../arch/generic/lisp/runtime/gen-r22-print-object.lisp")


;; for r22-defs-format.lisp
(defun expt (base power) (iexpt base power))


(load "../../lisp/runtime/r22-defs-format.lisp")
(load "../../lisp/runtime/r22-defs-pprint.lisp")
(load "../../lisp/runtime/r22-pprint.lisp")

(install-discriminators)

(load "../../lisp/runtime/r22-format.lisp")
(load "../../lisp/runtime/r22-float.lisp")

;; Install pretty printers
(progn
  (set-pprint-dispatch 'array         'pprint-array)
  (set-pprint-dispatch 'cons          'pprint-fill)
  (set-pprint-dispatch 'vector        'pprint-vector)

  ;; Make current entires as initial entry.
  (dolist (entry (pp-dispatch-table-entries *standard-pprint-dispatch*))
    (setf (second entry) nil) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign Function Interface
;;;
(load "../../lisp/ffi/ffi-clos.lisp")
(load "../../lisp/ffi/ffi-defs2.lisp")
(load "../../lisp/ffi/ffi-fns.lisp")


;(defun abs (x) (if (minusp x) (- x) x))
(load "../../arch/generic/lisp/libm/gen-float-complex.lisp")
(load "../../arch/generic/lisp/runtime/gen-r12-random.lisp")

;; load machine-type, machine-version
#+(or x86 x64)
(load "../../arch/x86x64/lisp/runtime/x86x64-r25-env.lisp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Developer Support
;;;
(load "../../lisp/devel/d00-defpackage.lisp")
(load "../../lisp/devel/d01-deprecated.lisp")

;; c::*environment* vs. si::*environment*
(load "../../lisp/devel3/d03-eval.lisp")

(load "../../lisp/devel/d25-apropos.lisp")
(load "../../lisp/devel/d25-describe.lisp")
;(load "../../lisp/devel/d25-documentation.lisp")
(load "../../lisp/devel/d25-describe.lisp")
(load "../../lisp/devel/d25-dribble.lisp")
;(load "../lisp/devel/d25-ed.lisp")     since ed depends on platform.
;(load "../lisp/devel/d25-inspect.lisp")
(load "../../lisp/devel/d25-step.lisp")
;(load "../lisp/devel/d25-time.lisp")
;(load "../lisp/devel/d25-trace.lisp")


(load "../../lisp/devel3/d25-cmdl.lisp")
(load "../../lisp/devel3/d25-inspect.lisp")

(let ((host (make-windows-host "dummy")))
  (setq *pathname-hosts* (list host))
  (setq *default-pathname-defaults*
    (make-pathname-using-host host nil nil nil nil nil) ) )

#+nil
(let ((host (make-logical-host "SYS")))
  (push host *pathname-hosts*) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; compile-file
;;;
(load "../../lisp/devel/d24-defs.lisp")
(load "../../lisp/devel/d24-defs-fasd.lisp")
(load "../../lisp/devel/d24-defs-fasl.lisp")
(load "../../lisp/devel/d24-fasd.lisp")
(load "../../lisp/devel/d24-fasl.lisp")
(load "../../lisp/devel/d24-unfasl.lisp")

(load "../../lisp/devel3/d24-loader.lisp")
(load "../../lisp/devel3/d24-compile-file.lisp")


#+(or x86 x64)
(progn
  (load "../../arch/x86x64/lisp/runtime/x86x64-r03-defs.lisp")
  (load "../../arch/x86x64/lisp/runtime/x86x64-r03-funobj.lisp")

  (load "../../arch/x86x64/lisp/devel/x86x64-d00-macros.lisp")
  (load "../../arch/x86x64/lisp/devel/x86x64-d00-fns.lisp")

  (load "../../arch/x86x64/lisp/devel/x86x64-d24-fasd.lisp")
  (load "../../arch/x86x64/lisp/devel/x86x64-d24-fasl.lisp")
 ) ; x86 x64


(load "../../lisp/devel3/d24-load.lisp")

;; From here, load uses pretty printer for printing value.

(setf (symbol-function 'apply)
    (lambda (fn param &rest param*)
        (declare (lambda-name cl:apply))
      (cond
        ((null param*)
          (apply fn param) )
        ((null (rest param*))
          (apply fn (cons param (first param*))) )
        (t
          (let ((last2 (last param* 2)))
            (when last2 (setf (cdr last2) (second last2)))
            (apply fn (cons param param*)) ) )) ))

(defun cl:special-operator-p (name &optional env)
    (check-type name symbol)
  (eq (c::function-information name env) :special-operator) )


;;;; call-macro-expander
;;; Compiler calls this function when it needs to expand macro-form.
(defun c::call-macro-expander (fn form env)
  (handler-case (values (funcall fn form env) nil)
    (error (c) (values nil c)) ) )


;;; import defsystem facility
(load "../../lisp/devel3/d50-ds-defs")
(load "../../lisp/devel3/d50-ds-fns")

;; FIXME 2007-06-03 We should move defommand "cs" and "lds" to another
;; file.
(devel:defcommand cs
        (&optional (name (devel::get-arg :system "Comple system")))
    "Compile system"
  (when name
    (devel::remember-arg :system name)
    (ds:compile-system name) ) )


(devel:defcommand lds
        (&optional (name (devel::get-arg :system "Load system")))
    "Load system"
  (when name
    (devel::remember-arg :system name)
    (ds:load-system name) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; libm
;;;
(dolist (bits '(32 64))
  (dolist (name '(
        "decode"
        "float"
        "scale"
        "sign"

        "fceiling" "ffloor" "fround" "ftruncate"
        "mod" "rem"

        "rational" "rationalize"

        ;; libm
        "acos"  "asin"  "atan" "atan2"
        "acosh" "asinh" "atanh"
        "cos"   "sin"   "tan"
        "cosh"  "sinh"  "tanh"

        "kcos" "ksin" "ktan"
        "rem-pio2" #+nil "krem-pio2"

        "log" "log1p"
        "exp" "expm1"

        "expt"
        "hypot"
        ) )
    (load (format nil "../../arch/generic/lisp/libm/float~D/gen-float~D-~A"
        bits bits name )) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Windows
;;;
#+win32
#+x86
(progn
  (load "../../platform/win/lisp/runtime/pl-win-r00-ffi")
  (load "../../platform/win/lisp/runtime/pl-win-r25-getcmdl")
  (load "../../platform/win/lisp/runtime/pl-win-r25-getenv")
 )

;;; For regex
(load "../../lisp/runtime/r50-string-search")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Epilogue
;;;
(load "../../lisp/devel3/d99-toplevel.lisp")


;;; 25 Environment

(defun thread-error-hook (&rest x) (apply 'error x))

(defun cl:lisp-implementation-type () (values "Evita Common Lisp"))

#+win32
(defun cl:lisp-implementation-version ()
  (values
    #.(with-open-file (s "../../build.txt")
        (string-trim " " (read-line s)) )) )


#+win32
(let ((NetBIOS                      0)
      (DnsHostname                  1)
      (DnsDomain                    2)
      (DnsFullyQualified            3)
      (PhysicalNetBIOS              4)
      (PhysicalDnsHostname          5)
      (PhysicalDnsDomain            6)
      (PhysicalDnsFullyQualified    7) )
    (declare (ignorable NetBIOS DnsHostname DnsDomain DnsFullyQualified
        PhysicalNetBIOS PhysicalDnsHostname PhysicalDnsDomain
        PhysicalDnsFullyQualified ))
  (defun cl:long-site-name () (.get-computer-name PhysicalDnsFullyQualified))
  (defun cl:short-site-name () (.get-computer-name DnsHostname))  

  (defun cl:machine-instance () (.get-computer-name NetBIOS))

  (defun cl:software-type () (values "Windows"))
 ) ; progn


(setq cl:*features* (delete :genesis cl:*features*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Test Cases
;;
(setq *load-verbose* t)

(defun parse-sequence-type (seqty)
    (declare (values (member list vector) type-specifier
                     (or sequence-index null) ))
  (labels (
    ;; get-elty
    (get-elty (ty runner)
        (declare (ignore ty))
      (cond
        ((null runner) 't)
        ((eq (first runner) '*) 't)
        (t (first runner)) ) )

    ;; get-length
    (get-length (ty runner)
      (cond
        ((null runner) nil)
        ((eq (first runner) '*) nil)
        ((integerp (first runner)) (first runner))
        (t (invalid ty)) ) )

    ;; invalid
    (invalid (ty)
      (error "Invalid sequence-type: ~S" ty) )

    ;; parse-cons
    (parse-cons (ty)
      (case (first ty)
        ((vector)
          (values 'vector (get-elty ty (rest ty)) (get-length ty (cddr ty))) )
        ((string)
          (values 'vector 'character (get-length ty (rest ty))) )
        ((bit-vector)
          (values 'vector 'bit (get-length ty (rest ty))) )
        (otherwise
          (invalid ty) )) )

    ;; parse-symb
    (parse-symb (name)
      (case name
        ((list)   (values 'list t nil))
        ((vector) (values 'vector t nil))
        ((string) (values 'vector 'characer nil))
        ((bit-vector) (values 'vector 'bit nil))
        (otherwise (invalid name)) ) )
    )
    ;;
    (typecase seqty
      (symbol (parse-symb seqty))
      (cons   (parse-cons seqty))
      (otherwise (invalid seqty)) ) ) )

(dolist (x '(stop gc command-loop))
  (unintern x) )


(setq *print-array*
    (lambda ()
        (let ((*image-save-time* (get-universal-time))
              (*finalizations* nil) )
          (setq *print-array* nil)
          (internal-save-image "_untested.image") )) )

(setq *debug-output* nil)

(.collect-garbage :age 4)
(.collect-garbage :age 4)

(funcall *print-array*)

;;; Check static-object
(labels (
  (check (name)
    (let ((cell (find-value-cell name)))
      (unless (static-object-p cell)
        (format t "; must be static ~S~%" cell) ) )

    (let ((cell (find-setf-cell name)))
      (unless (static-object-p cell)
        (format t "; must be static ~S~%" cell) ) )

    (let ((class (find-class name nil)))
      (unless (static-object-p class)
        (format t "; must be static ~S~%" class) ) ) )
  )
  (dolist (pkg (list-all-packages))
    (do-external-symbols (x pkg) (check x)) ) )

(format t "~2%; ~D second takes generating image.~5%"
    (- (get-universal-time) *image-save-time*) )

;;;; EOF
