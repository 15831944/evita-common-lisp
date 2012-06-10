;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - devel - load
;;; lisp/devel/d24-load.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel3/d25-cmdl.lisp#10 $
;;;
;;; Description:
;;;  This file constains implemenation of
;;;     command-loop
;
(in-package :devel)

(deftlv *cmdl-io* nil)
(deftlv *cmdl-condition* nil)
(deftlv *cmdl-level* 0)
(deftlv *cmdl-vars* nil)

(defstruct command
  (name         nil :type symbol)
  (document     ""  :type simple-string)
  (function     nil :type function) )

(defvar *command-table*    (make-hash-table :test 'eq))
(defvar *command-lasts*    (make-hash-table :test 'equal))
(defvar *aliases*          (make-hash-table :test 'eq))

(declaim (ftype (function (symbol (or string null) function) keyword)
    %defcommand ))

(declaim (ftype (function () unspecified) command-loop))
(declaim (ftype (function (symbol) (or command null)) find-command))

(declaim (ftype (function (&optional stream) (values t t))
  read-arg ))

(declaim (ftype (function (&optional stream) (or simple-string nil))
  read-shell-arg ))

(declaim (ftype (function (t &optional t) unspecified) remember-value))
(declaim (ftype (function (list &optional t) unspecified) remember-values))
(declaim (ftype (function () unspecified) show-restarts))

(declaim (ftype (function ((unsigned-byte 32)) nil) si::exit-process))

;;;; defcommand
(defmacro defcommand (name lambda-list &body decl*-form*)
  (multiple-value-bind (decl* form* doc-string)
      (c::analyze-body decl*-form* t)
    (let ((name (intern (symbol-name name) :keyword)))
     `(%defcommand ,name ,(or doc-string "")
        (lambda ,lambda-list (declare (lambda-name (:command ,name)))
            ,@decl* ,@form* ) ) ) ) )


;;;; %defcommand
(defun %defcommand (name document function)
  (let ((cmd (si::gethash/eq name *command-table*)))
    (if cmd
        (setf (command-document cmd) document
              (command-function cmd) function )
      (let ((cmd (make-command :name name
                    :document document
                    :function function ) ))
        (setf (si::gethash/eq name *command-table*) cmd) ))
    name ) )


;;;; command-loop
(defun command-loop ()
  (let ((history 1)
        (eof '(eof)) )
  (labels (
    ;; emit-prompt
    (emit-prompt ()
      (format t "~&~A[~D]"
        (package-pretty-name *package*)
        history )
      (loop repeat *cmdl-level* do (write-char #\>))
      (write-char #\Space)
      (force-output) )

    ;; eval-form
    ;; eval-form
    (eval-form (form)
      (setq - form)
      (let ((values (multiple-value-list (eval-with-restart form))))
        (shiftf +++ ++ + form)
        (remember-values values) ) )

    ;; eval-line
    (eval-line ()
      (let ((form (read *cmdl-io* nil eof)))
        (unless (eq form eof)
          (if (symbolp form)
              (eval-symbol form)
            (eval-form form) )) ) )

    ;; eval-symbol
    (eval-symbol (sym)
      (let ((expansion
                (si::gethash/eq (intern (symbol-name sym) :keyword)
                                *aliases* ) ))
        (if expansion
            (with-input-from-string (s expansion)
              (let ((*cmdl-io* (make-concatenated-stream s *cmdl-io*)))
                (eval-line) ) )
          (let ((cmd (find-command sym)))
            (if cmd
                (execute-command cmd (get-args))
              (progn
                (skip-extra)
                (if (boundp sym)
                    (eval-form sym)
                  (format t "; Variable ~S is unbound.~%" sym) ))) )) ) )

    ;; eval-with-restart
    #+nil
    (eval-with-restart (form)
      (if (eql *cmdl-level* 1)
          (eval form)
        (let ((level *cmdl-level*))
          (restart-case (eval form)
            (abort ()
              :report (format nil "Return to command-loop ~D." level)
              (format t "~&; Back to command-loop ~D.~%" level) )) )) )

    (eval-with-restart (form)
        (let ((level *cmdl-level*))
          (restart-case (eval form)
            (abort ()
              :report (format nil "Return to command-loop ~D." level)
              (format t "~&; Back to command-loop ~D.~%" level) )) ) )

    ;; execute-command
    (execute-command (cmd args)
      (eval-with-restart `(apply ,(command-function cmd) ',args) ) )

    ;; get-args
    (get-args ()
      (with-collector (collect)
        (loop
          (multiple-value-bind (arg got) (read-arg)
            (unless got (return))
            (collect arg) )) ) )

    ;; main-loop
    (main-loop ()
      (loop
        (let ((form (read-command *cmdl-io*)))
          (when (eq form eof)
            (read-char *cmdl-io* nil nil)   ; read newline
            (format t "; EOF~%")
            (return) )
          (if (symbolp form)
              (eval-symbol form)
            (progn
              (skip-extra)
              (eval-form form) ))
          (incf history) )) )

    ;; read-command
    (read-command (stream)
      (emit-prompt)
      (loop
        (let ((ch (read-char stream nil eof)))
          (cond
            ((eq ch eof) (return eof))
            ((char= ch #\Newline) (emit-prompt))
            ((char= ch #\Space))
            ((char= ch #\Tab))
            ((char= ch #\Page))
            (t
              (unread-char ch stream)
              (return (read-preserving-whitespace stream nil eof)) )) )) )

    ;; package-pretty-name
    (package-pretty-name (pkg)
        (declare (values simple-string))
        (declare (type package pkg))
      (or (first (package-nicknames pkg))
          (package-name pkg) ) )

    ;; skip-extra
    (skip-extra ()
      (loop
        (let ((ch (read-char *cmdl-io* nil #\Newline)))
          (case ch
            ((#\Newline) (return))
            ((#\Space #\Tab #\Page #\u0029))
            (otherwise (unread-char ch *cmdl-io*) (return)) ) )) )
    )
    ;;
    (let* ((*cmdl-level* (1+ *cmdl-level*))
           (*cmdl-io* *terminal-io*)
           (*print-pretty* nil) )
      (main-loop)
      (abort) ) ) ) )


;;;; find-command
(defun find-command (name)
  (let ((key (intern (symbol-name name) :keyword)))
    (values (si::gethash/eq key *command-table*)) ) )


;;;; remember-value
(defun remember-value (value &optional (print t))
  (remember-values (list value) print) )


;;;; remember-values
(defun remember-values (values &optional (print t))
    (declare (type list values))
  (labels (
    (print-values ()
      (when print
        ;; Note: If we use non-interactive stream, column position of
        ;; input stream isn't start of line. Since the last output is
        ;; prompt text.
        (when (interactive-stream-p *cmdl-io*) (fresh-line))
        (if (null (rest values))
            (let ((value (first values)))
              (format t "; ~S: ~S~%" (type-of value) value) )
          (let ((nth 0))
            (dolist (value values)
              (format t "; [~D] ~S: ~S~%" nth (type-of value) value)
              (incf nth) ) ))) )
    )
    ;;
    (cond
      ((null values)
        (when print (format t "~&; No values.~%")) )
      ((equal / values)
        (print-values) )
      (t
        (shiftf *** ** * (first values))
        (shiftf /// // / values)
        (print-values) )) ) )


;;;; get-arg
(defun get-arg (key prompt)
  (labels (
    ;; prompt-for
    (prompt-for ()
      (format *cmdl-io* "~A: " prompt)
      (force-output *cmdl-io*)
      (read-arg) )
    )
    (multiple-value-bind (last present)
        (if key
            (gethash key *command-lasts*)
          (values nil nil) )
      (if present
          (values last t)
        (multiple-value-bind (arg got) (prompt-for)
          (when (and got key)
            (setf (gethash key *command-lasts*) arg) )
          (values arg got) )) ) ) )


;;;; remember-arg
(defun remember-arg (key arg)
  (setf (gethash key *command-lasts* key) arg) )


;;;; read-arg
(defun read-arg (&optional (in *cmdl-io*))
  (labels (
    ;; read-aux
    (read-aux ()
      (let ((ch (peek-char nil in nil #\Newline)))
        (if (char= ch #\Newline)
            (read-char in nil #\Newline)
          (let ((arg (read-preserving-whitespace in nil #\Newline)))
            (unless (eql arg #\Newline)
              ;; Read unmatched right parenthesis.
              (loop
                (skip-whitespaces)
                (let ((ch (peek-char nil in nil #\Newline)))
                  (unless (char= ch #\u0029) (return))
                  (read-char in) )))
            arg )) ) )

    ;; skip-whitespaces
    (skip-whitespaces ()
      (loop
        (let ((ch (peek-char nil in nil #\Newline)))
          (unless (or (char= ch #\Space) (char= ch #\Tab)) (return))
          (read-char in) )) )
    )
    ;;
    (skip-whitespaces)
    (let ((arg (read-aux)))
      (if (eql arg #\Newline)
          (values nil nil)
        (values arg t) ) ) ) )


;;;; read-shell-arg
(defun read-shell-arg (&optional (in *cmdl-io*))
  (labels (
    ;; read-delimited
    (read-delimited (delim)
      (with-output-to-string (s)
        (loop
          (let ((ch (read-char in nil delim)))
            (when (char= ch delim) (return))
            (write-char ch s) )) ) )

    ;; read-word
    (read-word ()
      (with-output-to-string (s)
        (loop
          (let ((ch (read-char in nil)))
            (when (null ch) (return))
            (when (or (char= ch #\Space) (char= ch #\Tab)) (return))
            (when (char= ch #\Newline) (unread-char ch) (return))
            (write-char ch s) )) ) )

    ;; skip-whilespaces
    (skip-whitespaces ()
      (loop
        (let ((ch (read-char in nil)))
          (when (null ch) (return-from read-shell-arg nil))
          (when (char= ch #\Newline) (return-from read-shell-arg nil))
          (unless (or (char= ch #\Space) (char= ch #\Tab))
            (unread-char ch in)
            (return) ) )) )
    )
    ;;
    (skip-whitespaces)
    (let ((ch (read-char in)))
      (if (or (char= ch #\u0022) (char= ch #\u0027))
          (read-delimited ch)
        (progn
          (unread-char ch in)
          (read-word) )) ) ) )


;;;; command alias
#+nil
(defcommand alias (&optional name expansion)
  (labels (
    ;; install
    (install ()
      (setf (si::gethash/eq name *aliases*) expansion) )

    ;; show-all
    (show-all ()
      (loop
        for name being each hash-key of *aliases*
          collect name into names
        finally
          (loop for name in (sort names #'string<)
            do (show-one name) )) )

   ;; show-one
   (show-one (name)
     (let ((expansion (si::gethash/eq name *aliases*)))
       (if expansion
           (format t "; ~S ~S~%" name expansion)
         (format t "; No such alias: ~S" name) ) ) )
    )
    ;;
    (cond
      (expansion (install))
      (name (show-one name))
      (t (show-all)) ) ) )

;;;; command abort
(defcommand abort ()
    "Invoke ABORT restart."
  (let ((found nil))
    (dolist (r (compute-restarts nil))
      (when (eq (restart-name r) 'abort)
        (when found (invoke-restart r))
        (setq found r) ) ) ) )


;;;; command backtrace
(defcommand bt (&optional full)
    "Print stack frames."
  (loop
    for (addr type . rest) in (si::backtrace)
    for nth = 0 then (1+ nth) do
    (case type
      ((:|Fun|)
        (format t "; [~D] ~X ~S +~4,'0X~%"
            nth addr (function-name (first rest)) (second rest) ) )
      (otherwise
        (when full
          (format t "; [~D] ~X ~S~{ ~S~}~%" nth addr type (cdr rest)) )))) )


;;;; command cd
(defcommand cd (&optional dirname)
    "Change current directory."
  (setf (si::.current-directory) dirname)
  (format t "; ~:W~%" (truename ".")) )


;;;; command cf
(defcommand cf (&optional (filename (get-arg :load "Compile")))
    "Compile file"
  (when filename
    (remember-arg :load filename)
    (compile-file filename) ) )


;;;; command cm1
(defcommand cm1 (&optional (form *))
    "Expand compiler-macro."
  (labels (
    ;; compiler-macroexpand-1
    (compiler-macroexpand-1 (form)
      (let* ((expander
              (and (consp form) (compiler-macro-function (first form))) )
             (expansion (and expander (funcall expander form nil))) )
        (or expansion form) ) )
    )
    ;;
    (let ((expansion (compiler-macroexpand-1 form)))
      (unless (eq expansion form)
        (format t "~&~:W~2%" expansion)
        (remember-value expansion nil) ) ) ) )


;;;; command cont
(defcommand cont (&optional (nth (get-arg nil "Restart")))
    "Invoke restart."
  (let ((nth (if (null nth) 0 nth))
        (restarts (compute-restarts)) )
    (if (and (integerp nth) (<= 0 nth (1- (length restarts))))
        (invoke-restart (nth nth restarts))
      (format t "; Expect [0, ~D] rather than ~S."
          (1- (length restarts))
          nth )) ) )


;;;; command des
(defcommand des (&optional (obj *))
  (describe obj) )


;;;; command f
(defcommand f (n)
    "Dump contents of frame."
  (let ((frame (and (typep n 'sequence-index) (nth n (si::backtrace)))))
    (case (second frame)
      ((:|Fun|)
         (let ((frame (si::get-frame (first frame))))
           (format t "; [~D] ~X ~S +~X~%"
              n (first frame) (second frame) (third frame) )
           (setq *cmdl-vars* (cdddr frame))
           (loop for (name ofs datum) in (cdddr frame) do
             (format t ";  +~D ~S = ~10T~:W~%" ofs name datum) )
           (remember-value (second frame) nil) ) )) ) )


;;;; fspec-p
;;; BUGBUG: We must move fspec-p to another file.
(defun fspec-p (x)
  (when x
    (or (functionp x)
        (si::function-name-p x)
        (and (consp x)
             (member (first x)
                    '(macro-function compiler-macro-function
                      labels flet
                      method ))))) )


;;;; command disassemble
(defcommand dis (&optional (fname (if (fspec-p *) * (get-arg nil "Function"))))
    "Disassemble function."
  (when fname (disassemble fname)) )


;;;; command echo
(defcommand echo (&rest arg*)
    "Echo arguments."
  (let ((delim ""))
    (dolist (arg arg*)
      (format t "~A~A" delim arg)
      (setq delim " ") ) ) )

;;;; command exit
(defcommand exit (&optional (n 0))
  (si::exit-process n) )


;;;; command gc
(defcommand gc ()
  (multiple-value-bind (before after) (si::.collect-garbage)
    (format t "; ~:D => ~:D~%" before after) ) )


;;;; command help
(defcommand help ()
    "Print all commands."
  (labels (
    ;; list-commands
    (list-commands ()
      (loop
        for cmd being each hash-value of *command-table*
          collect cmd into commands
        finally
          (return (sort commands #'string< :key 'command-name)) ) )
    )
    ;;
    (dolist (cmd (list-commands))
      (format t "; ~A ~20T~A~%"
        (command-name cmd)
        (command-document cmd)) ) ) )


;;;; cmmand i
(defcommand i (&optional (arg *))
    "Inspect object."
  (remember-value (inspect arg)) )


;;;; command ld
(defcommand ld (&optional (filename (get-arg :load "Load")))
    "Load file"
  (when filename
    (multiple-value-bind (succeeded condition)
        (ignore-errors (load filename :if-does-not-exist nil))
      (cond
        (succeeded
          (remember-arg :load filename) )
        ((null condition)
          (format t "; No such file: ~S~%" filename) )
        (t
          (remember-arg :load filename)
          (format t "; Failed to load ~S: ~A~%" filename condition) )) )) )

;;;; command ls
(defcommand ls ()
  (let ((cwd (truename ".")))
    (dolist (path (directory (make-pathname :name :wild)))
      (format t "; ~A~%" (enough-namestring path cwd)) ) ) )


;;;; command m1
(defcommand m1 (&optional (form *))
    "Expand macro."
  (let ((expansion (macroexpand-1 form)))
    (unless (eq expansion form)
      (format t "~&~:W~2%" expansion)
      (remember-value expansion nil) ) ) )


;;;; command package
(defcommand pkg (&optional (name (get-arg nil "Package")))
    "Change current package."
  (let ((package
          (typecase name
            (symbol (find-package name))
            (string (find-package name))
            (package name) ) ))
    (if package
       (setq *package* package)
      (format t "; No such package ~S.~%" name) ) ) )


;;;; command pp
(defcommand pp (&optional (obj *))
    "Pretty-print object."
 (format t "~&~:W~2%" obj) )


;;;; command pwd
(defcommand pwd ()
    "Print current working directory."
 (format t "~&~:W~2%" (truename ".")) )


;;;; command rs
(defcommand rs ()
    "Show available restarts."
  (show-restarts) )


;;;; command toplevel
(defcommand toplevel ()
    "Return to toplevel."
  (let ((restart (first (last (compute-restarts)))))
    (when restart (invoke-restart restart)) ) )


;;;; command var
(defcommand var (name)
    "Display specified variable in current function frame."
  (let ((frob (or (member name *cmdl-vars* :key 'first)
                  (member name *cmdl-vars* :key 'second) ) ))
    (when frob
      (remember-value (third (first frob)) t) ) ) )

;;;; debugger
(defun debugger (cond)
  (let ((*print-pretty* nil))
    (format t "; Error: ~A~%" cond)
    (let ((*cmdl-condition* cond))
      (command-loop) ) ) )


(defun show-restarts ()
  (format t "~&;;; Restart actions:~%")
  (loop for restart in (compute-restarts)
        for index = 0 then (1+ index)
        do (format t ";  cont ~2D - ~A~%" index restart) )
  (format t ";~%")
  (format t ";   Type 'cont' followed by number, e.g. 'cont 0' to invoke the first restart.~%")
  (format t ";   Type 'abort' to exit from debugger.~2%")
  (values) )


;;;; command who-call
(defcommand who-call (fname)
    "Show callers"
  (let* ((key (if (symbolp fname) fname (si::intern-setf-cell (second fname))))
         (vec (cdr (gethash key si::*caller-table*)))
         (n 0) )
      (declare (type sequence-index n))
    (when vec
      (dotimes (i (length vec))
        (let ((caller (svref vec i)))
          (when (functionp caller)
            (format t ";  ~D ~S~%" (incf n) 
                (or (function-name caller) caller) )) ) ))
    (when (zerop n) (format t ";  No callers.~%")) ) )
