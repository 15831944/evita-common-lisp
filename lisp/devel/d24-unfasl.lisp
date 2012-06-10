;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 24 System Construction - UNFASL
;;; dev/d24-unfasl.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d24-unfasl.lisp#7 $
;;;
;;; Description:
;;;  This file contains functions for print contents of FASL file.
;;;
;;; See Also: sys:src;d24-fasd.lisp
;
(in-package :devel)


;;;; unfasl-print
;
(defun unfasl-print (filespec &optional (*standard-output* *standard-output*))
  (let ((stack  '())
        (labels '())
        (nlabels 0) )
  (labels (
    (clear ()
      (when stack
        (format t "; Broken FASL file. Following commands aren't completed: ~S" stack) )
      (setq stack '())
      (setq labels '())
      (setq nlabels 0) )

    ;; indent
    (indent (ctrl &rest args)
        (declare (dynamic-extent args))
      (format t "~@<;~@; ~2D~A ~?~:>~%"
        (length stack)
        (indent-aux (length stack))
        ctrl args ) )

    ;; indent-aux
    #+nil
    (indent-aux (level)
        (declare (type fixnum level))
        (declare (values string))
      (with-output-to-string (s)
        (loop while (plusp level) do
          (let ((mod (mod level 5)))
            (write-string (subseq " |  |  |  |  | "
                            0 (+ 3 (* (ash mod -1) 2) (ash mod -1)) )
                          s )
            (decf level 5) ) ) ) )

    (indent-aux (level)
        (declare (type fixnum level))
        (declare (values string))
      (with-output-to-string (s)
        (loop while (plusp level) do
          (write-string " |  " s)
          (decf level) ) ) )

    ;; mark-object
    (mark-object (object)
        (declare (values unspecified))
      (push (cons (incf nlabels) object) labels) )

    ;; process-operand
    (process-operand (operand mark-p)
      (when mark-p (mark-object operand))

      (when (null stack)
        (indent "top-level: ~S" operand)
        (return-from process-operand) )

      (let* ((command (first stack))
             (index   (second command))
             (end     (third  command)) )
        (indent "~S[~D] = ~:W~@[ ... label ~D~]"
                (first command) index
                operand
                (and mark-p nlabels) )
        (incf index)
        (setf (second command) index)

        (when (= index end)
          (pop stack)
          (when (eq (first command) 'setlabel)
            (mark-object operand) )
          (process-operand command nil) ) ) )

    ;; push-command
    ;;  Pushes command specified by name onto stack
    (push-command (name &optional mark-p (start 0) (end 1))
        (declare (values unspecified))
      (let ((command (list name start end)))
        (when mark-p (mark-object command))
        (indent "~S ~D ~D~@[ ... label ~D~]"
            name start end
            (and mark-p nlabels) )
        (if (= start end)
              (process-operand command nil)
          (push command stack) ) ) )

    ;; read-uint
    ;;  Reads an unsigned integer.
    (read-uint (in &optional (uint 0))
        (declare (values unsigned-byte))
      (loop for byte = (read-byte in) do
        (setq uint (ash uint 7))
        (when (<= byte #x7F)
          (return (logior uint byte)) )
        (setq uint (logior uint (ldb (byte 7 0) byte))) ) )

    ;; read-string
    ;;  Reads a string.
    (read-string (in)
        (declare (values simple-string))
      (let* ((nchars (read-uint in))
             (string (make-string nchars)) )
        (dotimes (index nchars)
          (setf (schar string index) (code-char (read-uint in))) )
        string ) )

    ;; read-vector
    (read-vector (in mark-p type)
      (let* ((nelts  (read-uint in))
             (vector (make-array nelts :element-type type)) )
        (loop
          for index below nelts do
            (setf (elt vector index) (read-uint in)) )
        (process-operand vector mark-p) ) )
    )
    ;;
    ;; unfasl-print
    ;;
    (setq filespec (pathname filespec))
    (setq filespec
      (merge-pathnames filespec
                       (compile-file-pathname (pathname-name filespec)) ))

    (with-open-file (in filespec :element-type '(unsigned-byte 8))
      (loop
        for byte = (read-byte in nil)
        for mark-p = (and byte (logbitp 7 byte))
        finally
          (when stack
            (format t "; Unprorcessed operands are on the stack;  ~:W"
              stack ) )
          (return filespec)
        while byte do
          (case (ldb (byte 7 0) byte)
            ((#.FASL-OP-CLASS)
              (push-command 'class mark-p) )

            ((#.FASL-OP-CLASSD)
              (push-command 'si:class-description mark-p) )

            ((#.FASL-OP-CLEAR)
              (clear)
              (indent "clear") )

            ((#.FASL-OP-DEFUN)
              (push-command 'defun) )

            ((#.FASL-OP-FUNCALL)
              (push-command 'funcall) )

            ((#.FASL-OP-FUNCALL-1)
              (push-command 'funcall-1 nil 1 3) )

            ((#.FASL-OP-IN-PACKAGE)
              (push-command 'in-package) )

            ((#.FASL-OP-REFLABEL)
              (let* ((label (read-uint in))
                     (label.object (assoc label labels :test #'eq)) )
                (unless label.object
                  (setq label.object
                    (cons label
                         (format nil "; ERROR: No such label ~D." label) )))
                (process-operand
                  (list 'reflabel label (cdr label.object))
                  nil ) ) )

            ((#.FASL-OP-SETLABEL)
              (push-command 'setlabel) )

            ((#.FASL-OP-START)
              (clear)
              (indent "start") )

            ((#.FASL-OP-POSINT)
              (let ((int (read-uint in)))
                (process-operand int mark-p) ) )

            ((#.FASL-OP-NEGINT)
              (let ((int (- (read-uint in))))
                (process-operand int mark-p) ) )

            ((#.FASL-OP-COMPLEX)
              (push-command 'complex mark-p 1 3) )

            ((#.FASL-OP-RATIO)
              (push-command 'ratio mark-p 1 3) )

            ((#.FASL-OP-SINGLE-FLOAT)
              (let ((hx (read-uint in)))
                (process-operand (si::encode-float32 hx) mark-p) ) )

            ((#.FASL-OP-DOUBLE-FLOAT)
              (let ((hx (read-uint in))
                    (lx (read-uint in)) )
                (process-operand (si::encode-float64 hx lx) mark-p) ) )

            ((#.FASL-OP-LIST)
              (push-command 'list mark-p 0 (read-uint in)) )

            ((#.FASL-OP-LIST*)
              (push-command 'list* mark-p 0 (read-uint in)) )

            ((#.FASL-OP-CONS)
              (push-command 'cons mark-p 0 2) )

            ((#.FASL-OP-NIL)
              (process-operand nil nil) )

            ((#.FASL-OP-HOME)
              (let ((name (read-string in)))
                (process-operand (intern name) mark-p) ) )

            ((#.FASL-OP-KEYWORD)
              (let ((name (read-string in)))
                (process-operand (intern name #.(symbol-package :key))
                                 mark-p ) ) )

            ((#.FASL-OP-SYMBOL)
              (let ((name (read-string in))
                    (pname (read-string in)) )
                (process-operand (format nil "~A::~A" pname name)
                                 mark-p ) ) )

            ((#.FASL-OP-GENSYM)
               (let ((symbol (make-symbol (read-string in))))
                 (process-operand symbol mark-p) ) )

            ((#.FASL-OP-PACKAGE)
              (process-operand (find-package (read-string in)) nil) )

            ((#.FASL-OP-CHAR)
              (process-operand (code-char (read-uint in)) nil) )

            ((#.FASL-OP-MARKER)
              (push-command 'marker mark-p 0 1) )

            ((#.FASL-OP-SETF-CELL)
              (push-command 'setf mark-p 0 1) )

            ((#.FASL-OP-VALUE-CELL)
              (push-command 'value mark-p 0 1) )

            #+evm ((#.FASL-OP-BYTE-CODE-FUNCTION)
              (push-command 'si:byte-code-function mark-p 2 (read-uint in)) )

            ((#.FASL-OP-FUNOBJ)
              (read-uint in)
              (push-command 'function mark-p 1 3) )

            ((#.FASL-OP-SIMPLE-STRING)
              (let ((string  (read-string in)))
                (process-operand string mark-p) ) )

            ((#.FASL-OP-SIMPLE-VECTOR)
              (push-command 'simple-vector mark-p 0 (read-uint in)) )

            ((#.FASL-OP-SIMPLE-BIT-VECTOR)
              (read-vector in mark-p 'bit) )

            ((#.FASL-OP-SBYTE8-VECTOR)
              (read-vector in mark-p '(signed-byte 8)) )

            ((#.FASL-OP-SBYTE16-VECTOR)
              (read-vector in mark-p '(signed-byte 16)) )

            ((#.FASL-OP-SBYTE32-VECTOR)
              (read-vector in mark-p '(signed-byte 32)) )

            ((#.FASL-OP-UBYTE8-VECTOR)
              (read-vector in mark-p '(unsigned-byte 8)) )

            ((#.FASL-OP-UBYTE16-VECTOR)
              (read-vector in mark-p '(unsigned-byte 16)) )

            ((#.FASL-OP-UBYTE32-VECTOR)
              (read-vector in mark-p '(unsigned-byte 32)) )

            ((#.FASL-OP-SINGLE-FLOAT-VECTOR)
              (let* ((nelts  (read-uint in))
                     (vector (make-array nelts :element-type 'single-float)) )
                (dotimes (index nelts)
                  (setf (elt vector index)
                    (let ((hx (read-uint in)) )
                      (si::encode-float32 hx) )) )
                (process-operand vector mark-p) ) )

            ((#.FASL-OP-DOUBLE-FLOAT-VECTOR)
              (let* ((nelts  (read-uint in))
                     (vector (make-array nelts :element-type 'double-float)) )
                (dotimes (index nelts)
                  (setf (elt vector index)
                    (let ((hx (read-uint in))
                          (lx (read-uint in)) )
                      (si::encode-float64 hx lx) )) )
                (process-operand vector mark-p) ) )


            ; array rank dimension... object...
            ((#.FASL-OP-ARRAY)
              (let* ((rank (read-uint in))
                     (dimensions (loop repeat rank collect (read-uint in))) )
                (push-command 'array mark-p 0 (reduce #'* dimensions)) ) )

            ((#.FASL-OP-PATHNAME)
              (let ((string  (read-string in)))
                (process-operand (pathname string) mark-p) ) )

            (otherwise
              (format t "; Unknown FASL-OP: #x~X~%" byte)
              (return) ))) ) ) ) )
