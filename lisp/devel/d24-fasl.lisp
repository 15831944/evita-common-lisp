;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 24 System Construction - FASL
;;; dev/d24-unfasl.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d24-fasl.lisp#7 $
;;;
;;; Description:
;;;  This file contains FASL loader.
;;;
;;; See Also: sys:src;d24-fasd.lisp
;;;
;;; BUGBUG: PERF: Make fasl-op-function not to be a closure.
;;; BUGBUG: PERF: Should use %make-array :element-type-code instead of
;;; make-array :element-type.
;
(in-package :devel)

;;;; fasl-state
;;;
;;; Slots:
;;;   fn
;;;     A function to process this state.
;;;   index
;;;     An index used as counter or index of slot in object.
;;;   end
;;;     An index. When index equals to end, object is constructed.
;;;   object
;;;     A object being constructed by this state.
;;;   tail
;;;     A nil or cons which is last cons of list being constructed.
;;;   marker
;;;     A nil or cons which car is label number and cdr is labeld object.
;
(defstruct (fasl-state
             (:constructor make-fasl-state (fn index end object)) )
  (fn       nil :type function)
  (index    0   :type ext:sequence-index)
  (end      0   :type ext:sequence-index)
  (object   t   :type t)
  (tail     nil :type list)
  (marker   nil :type list) )


;;;; fasl-load
;
(defun fasl-load (filespec  &key (verbose *load-verbose*)
                                 (print   *load-print*)
                                 (if-does-not-exist :error)
                                 (external-format :default) )
  (let ((stack   '())
        (labels  '())
        (nlabels 0) )
    (labels (
      ;; clear
      ;;    Clear object stack.
      (clear ()
        (when stack
          (fasl-error "; Following commands aren't completed: ~S" stack) )
        (setq stack '())
        (setq labels '())
        (setq nlabels 0) )

      ;; fasl-error
      ;;    Singals FASL error.
      (fasl-error (ctrl &rest args)
          (declare (dynamic-extent args))
        (error "FASL Error: ~?" ctrl args) )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; FASL Operators
      ;;

      ;; fasl-op-array
      (fasl-op-array (state operand)
          (declare (type fasl-state state))
          (declare (type t operand))
        (let ((array (fasl-state-object state))
              (index (fasl-state-index  state)) )
          (setf (row-major-aref array index) operand) ) )

      ;; fasl-op-class
      (fasl-op-class (state class-name)
          (declare (type fasl-state state))
          (declare (type symbol class-name))
        (setf (fasl-state-object state) (si::intern-class class-name)) )

      ;; fasl-op-classd
      (fasl-op-classd (state class-name)
          (declare (type fasl-state state))
          (declare (type symbol class-name))
        (setf (fasl-state-object state) (find-classd class-name)) )

      ;; fasl-op-complex
      (fasl-op-complex (state operand)
          (declare (type fasl-state state))
          (declare (type t operand))
        (let ((index (fasl-state-index  state)))
          (ecase index
            ((0) (setf (fasl-state-object state) operand))
            ((1) (complex (fasl-state-object state) operand)) ) ) )

      ;; fasl-op-defun
      (fasl-op-defun (state name-ll-fn)
          (declare (type fasl-state state))
          (declare (type list name-ll-fn))
        (let ((name (first  name-ll-fn))
              (ll   (second name-ll-fn))
              (fn   (third  name-ll-fn)) )
          (setf (fasl-state-object state) (si::%defun name ll fn)) ) )

      ;; fasl-op-funcall
      (fasl-op-funcall (state operand)
          (declare (type fasl-state state))
          (declare (type t operand))
        (setf (fasl-state-object state) (funcall operand)) )

      ;; fasl-op-funcall-1
      (fasl-op-funcall-1 (state operand)
          (declare (type fasl-state state))
          (declare (type t operand))
        (ecase (fasl-state-index state)
          ((1)
            (setf (fasl-state-object state) operand) )
          ((2)
             (let ((object (fasl-state-object state)))
                (funcall operand object) ) )) )

      ;; fasl-op-in-package
      (fasl-op-in-package (state operand)
          (declare (type fasl-state state))
        (setf (fasl-state-object state) (si::%in-package operand)) )

      ;; fasl-op-list
      (fasl-op-list (state operand)
          (declare (type fasl-state state))
          (declare (type t operand))
        (let ((head (fasl-state-object state))
              (tail (fasl-state-tail   state)) )
          (if (null tail)
              (progn
                (setf (car head) operand)
                (setf (fasl-state-tail state) head) )
            (let ((cons (list operand)))
              (setf (cdr tail) cons)
              (setf (fasl-state-tail state) cons) )) ) )

      ;; fasl-op-list*
      (fasl-op-list* (state operand)
          (declare (type fasl-state state))
          (declare (type t operand))
        (let ((head  (fasl-state-object state))
              (tail  (fasl-state-tail   state))
              (index (fasl-state-index  state))
              (end   (fasl-state-end    state)) )
          (cond
            ((null tail)
              (setf (car head) operand)
              (setf (fasl-state-tail state) head) )
            ((= (1+ index) end)
              (setf (cdr tail) operand) )
            (t
              (setq operand (list operand))
              (setf (cdr tail) operand)
              (setf (fasl-state-tail state) operand) )) ) )

      ;; fasl-op-marker
      (fasl-op-marker (state name)
          (declare (type fasl-state state))
          (declare (type symbol name))
        (let ((marker (funcall name)))
          (setf (fasl-state-object state) marker)

          (when (fasl-state-marker state)
            (setf (cdr (fasl-state-marker state)) marker) ) ) )

      ;; fasl-op-ratio
      (fasl-op-ratio (state operand)
          (declare (type fasl-state state))
          (declare (type t operand))
        (let ((index (fasl-state-index  state)))
          (ecase index
            ((0) (setf (fasl-state-object state) operand))
            ((1) (/ (fasl-state-object state) operand)) ) ) )

      ;; fasl-op-setlabel
      (fasl-op-setlabel (state operand)
          (declare (type fasl-state state))
          (declare (type t operand))
        (mark-object operand)
        (setf (fasl-state-object state) operand) )

      ;; fasl-op-setf-cell
      (fasl-op-setf-cell (state name)
          (declare (type fasl-state state))
          (declare (type symbol name))
        (let ((cell (si::intern-setf-cell name)))
          (setf (fasl-state-object state) cell)

          (when (fasl-state-marker state)
            (setf (cdr (fasl-state-marker state)) cell) ) ) )

      ;; fasl-op-svef
      (fasl-op-svref (state operand)
          (declare (type fasl-state state))
          (declare (type t operand))
        (let ((object (fasl-state-object state))
              (index  (fasl-state-index  state)) )
          (setf (svref object index) operand) ) )

      ;; fasl-op-value-cell
      (fasl-op-value-cell (state name)
          (declare (type fasl-state state))
          (declare (type symbol name))
        (let ((cell (si::intern-value-cell name)))
          (setf (fasl-state-object state) cell)

          (when (fasl-state-marker state)
            (setf (cdr (fasl-state-marker state)) cell) ) ) )

      ;; find-classd
      (find-classd (class-name)
          (declare (type symbol class-name))
          (declare (values si:class-description))
        (let ((class (find-class class-name)))
          (slot-value class 'si::instance-description) ) )

      ;; load-1
      (load-1 (in byte)
        (load-2 in byte) )

      ;; load-2
      (load-2 (in byte &aux (mark-p (logbitp 7 byte)))
        (case (ldb (byte 7 0) byte)
          ((#.FASL-OP-CLASS)
            (push-command #'fasl-op-class mark-p) )

          ((#.FASL-OP-CLASSD)
            (push-command #'fasl-op-classd mark-p) )

          ((#.FASL-OP-CLEAR)
            (clear) )

          ((#.FASL-OP-DEFUN)
            (push-command #'fasl-op-defun) )

          ((#.FASL-OP-FUNCALL)
            (push-command #'fasl-op-funcall) )

          ((#.FASL-OP-FUNCALL-1)
            (push-command #'fasl-op-funcall-1 nil nil 1 3) )

          ((#.FASL-OP-IN-PACKAGE)
            (push-command #'fasl-op-in-package) )

          ((#.FASL-OP-REFLABEL)
            (let* ((label (read-uint in))
                   (label.object (assoc label labels)) )
              (unless label.object
                (fasl-error "No such label ~D." label) )
              (process-operand (cdr label.object) nil) ) )

          ((#.FASL-OP-SETLABEL)
            (push-command #'fasl-op-setlabel) )

          ((#.FASL-OP-START)
            (clear) )

          ((#.FASL-OP-POSINT)
            (let ((int (read-uint in)))
              (process-operand int mark-p) ) )

          ((#.FASL-OP-NEGINT)
            (let ((int (- (read-uint in))))
              (process-operand int mark-p) ) )

          ((#.FASL-OP-COMPLEX)
            (push-command #'fasl-op-complex mark-p 0 0 2) )

          ((#.FASL-OP-RATIO)
            (push-command #'fasl-op-ratio mark-p 0 0 2) )

          ((#.FASL-OP-SINGLE-FLOAT)
            (let ((hx (read-uint in)))
              (process-operand (si::encode-float32 hx) mark-p) ) )

          ((#.FASL-OP-DOUBLE-FLOAT)
            (let ((hx (read-uint in))
                  (lx (read-uint in)) )
              (process-operand (si::encode-float64 hx lx) mark-p) ) )

          ((#.FASL-OP-LIST)
            (push-command #'fasl-op-list  mark-p (list nil) 0 (read-uint in)) )

          ((#.FASL-OP-LIST*)
            (push-command #'fasl-op-list* mark-p (list nil) 0 (read-uint in)) )

          ((#.FASL-OP-CONS)
            (push-command #'fasl-op-list* mark-p (list nil) 0 2) )

          ((#.FASL-OP-NIL)
            (process-operand nil nil) )

          ((#.FASL-OP-HOME)
            (let ((name (read-string in)))
              (process-operand (intern name) mark-p) ) )

          ((#.FASL-OP-KEYWORD)
            (let ((name  (read-string in))
                  (pname #.(symbol-package :key)) )
              (process-operand (intern name pname) mark-p ) ) )

          ((#.FASL-OP-SYMBOL)
            (let ((name (read-string in))
                  (pname (read-string in)) )
              (process-operand (intern name pname) mark-p) ) )

          ((#.FASL-OP-GENSYM)
             (let ((symbol (make-symbol (read-string in))))
               (process-operand symbol mark-p) ) )

          ((#.FASL-OP-PACKAGE)
            (process-operand (find-package (read-string in)) nil) )

          (#.FASL-OP-CHAR
            (process-operand (code-char (read-uint in)) nil) )

          (#.FASL-OP-MARKER
            (push-command #'fasl-op-marker) )

          ((#.FASL-OP-VALUE-CELL)
            (push-command #'fasl-op-value-cell) )

          ((#.FASL-OP-SETF-CELL)
            (push-command #'fasl-op-setf-cell) )

          #+evm ((#.FASL-OP-BYTE-CODE-FUNCTION)
            (multiple-value-bind (command cookie start end)
                (fasl-start-byte-code-function (read-uint in))
              (push-command command mark-p cookie start end) ) )

          ((#.FASL-OP-FUNOBJ)
            (multiple-value-bind (loader cookie start end)
                (fasl-start-funobj (read-uint in))
              (push-command loader mark-p cookie start end) ) )

          ((#.FASL-OP-SIMPLE-STRING)
            (let ((string (read-string in)))
              (process-operand string mark-p) ) )

          ((#.FASL-OP-SIMPLE-VECTOR)
            (let* ((nelts (read-uint in))
                   (vec   (make-array nelts)) )
              (push-command #'fasl-op-svref mark-p vec 0 nelts) ) )

          ((#.FASL-OP-SIMPLE-BIT-VECTOR)
            (read-and-process-vector in mark-p 'bit) )

          ((#.FASL-OP-SBYTE8-VECTOR)
            (read-and-process-vector in mark-p '(signed-byte 8)) )

          ((#.FASL-OP-SBYTE16-VECTOR)
            (read-and-process-vector in mark-p '(signed-byte 16)) )

          ((#.FASL-OP-SBYTE32-VECTOR)
            (read-and-process-vector in mark-p '(signed-byte 32)) )

          ((#.FASL-OP-UBYTE8-VECTOR)
            (read-and-process-vector in mark-p '(unsigned-byte 8)) )

          ((#.FASL-OP-UBYTE16-VECTOR)
            (read-and-process-vector in mark-p '(unsigned-byte 16)) )

          ((#.FASL-OP-UBYTE32-VECTOR)
            (read-and-process-vector in mark-p '(unsigned-byte 32)) )

          ((#.FASL-OP-SINGLE-FLOAT-VECTOR)
            (let* ((nelts  (read-uint in))
                   (vector (make-array nelts :element-type 'single-float)) )
              (dotimes (index nelts)
                (setf (row-major-aref vector index)
                  (let ((hx (read-uint in)) )
                      (si::encode-float32 hx) )) )
                (process-operand vector mark-p) ) )

          ((#.FASL-OP-DOUBLE-FLOAT-VECTOR)
            (let* ((nelts  (read-uint in))
                   (vector (make-array nelts :element-type 'double-float)) )
              (dotimes (index nelts)
                (setf (row-major-aref vector index)
                  (let ((hx (read-uint in))
                        (lx (read-uint in)) )
                      (si::encode-float64 hx lx) )) )
                (process-operand vector mark-p) ) )

          ; array rank dimension... object...
          ((#.FASL-OP-ARRAY)
            (let* ((rank       (read-uint in))
                   (dimensions (loop repeat rank collect (read-uint in)))
                   (ar         (make-array dimensions)) )
              (push-command #'fasl-op-array mark-p
                            ar
                            0 (array-total-size ar) ) ) )

          ((#.FASL-OP-PATHNAME)
            (let ((string (read-string in)))
              (process-operand (pathname string) mark-p) ) )

          (otherwise
            (fasl-error "Unknown FASL-OP: #x~X~%" byte) )) )

      ;; load-print
      ;;  Prints values of top-level form.
      (load-print (value)
        (when print (format t "~:W~%" value)) )

      ;; mark-object
      ;;  Marks object with label.
      (mark-object (object)
        (push (cons (incf nlabels) object) labels) )

      ;; process-operand
      (process-operand (operand mark-p)
        (when mark-p
          (mark-object operand) )

        (when (null stack)
          (load-print operand)
          (return-from process-operand) )

        (let ((state (first stack)))
          (funcall (fasl-state-fn state) state operand)

          (incf (fasl-state-index state))

          (when (= (fasl-state-index state) (fasl-state-end state))
            (pop stack)
            (process-operand (fasl-state-object state) nil) ) ) )

      ;; push-command
      (push-command (fn &optional mark-p object (start 0) (end 1))
          (declare (type function fn))
        (if (= start end)
            (process-operand object mark-p)
          (let ((state (make-fasl-state fn start end object)))
            (when mark-p
              (mark-object object)
              (setf (fasl-state-marker state) (first labels)) )
            (push state stack) )) )

      ;; read-uint
      ;;   Reads unsigned integer and returns it.
      (read-uint (in &optional (uint 0))
        (loop for byte = (read-byte in) do
          (setq uint (ash uint 7))
          (when (<= byte #x7F)
            (return (logior uint byte)) )
          (setq uint (logior uint (ldb (byte 7 0) byte))) ) )

      ;; read-string
      ;;  Reads string and returns it.
      (read-string (in)
        (let* ((nchars (read-uint in))
               (string (make-string nchars)) )
          (dotimes (index nchars)
            (let ((code (read-uint in)))
              (setf (schar string index) (code-char code)) ) )
          string ) )

      ;; read-and-process-vector
      ;;  Reads vector and process it.
      (read-and-process-vector (in mark-p type)
        (let* ((nelts  (read-uint in))
               (vector (make-array nelts :element-type type)) )
          (assert (not (simple-vector-p vector)))
          (loop
            for index from 0 below nelts do
              (setf (row-major-aref vector index) (read-uint in)) )

          (process-operand vector mark-p) ) )

      ;; load-main
      (load-main (in)
        (let ((*load-verbose*   verbose)
              (*load-print*     print)
              (*load-pathname*  filespec)
              (*load-truename*  (truename filespec))
              (*load-level*     (1+ *load-level*))
              (*package*        *package*)
              (*readtable*      *readtable*)
              (xc::*situation*  'load) )
          (loop
            for byte = (read-byte in nil)
            while byte do (load-1 in byte)
            initially
              (when verbose
                (fresh-line)
                (write-char #\;)
                (loop repeat *load-level* do (write-char #\Space))
                (format t "Fast loading   ~S...~%" filespec) )
            finally
              (when stack
                (fasl-error "~&; Stack isn't empty;  ~:W" stack) )
              (when verbose
                (fresh-line)
                (write-char #\;)
                (loop repeat *load-level* do (write-char #\Space))
                (format t "Finish loading ~S~%" filespec) )) ) )
      )
      ;;
      ;; fasl-load
      ;;
      (let ((filespec
              (let ((filespec (pathname filespec)))
                (merge-pathnames filespec
                         (make-pathname :host (pathname-host filespec)
                                        :type "FASL"
                                        :case :common )) ) ))

        (with-open-file (in filespec :element-type '(unsigned-byte 8)
                                     :if-does-not-exist if-does-not-exist
                                     :external-format   external-format )
          (when in (load-main in) t) ) ) ) ) )
