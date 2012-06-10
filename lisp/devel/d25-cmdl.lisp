;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 25 Environment - CMDL
;;;; dev/d25-cmdl.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-cmdl.lisp#2 $
;;;
;;; Description:
;;;  This file contains command-loop.
;;;
;;;  Public Functions:
;;;     command-loop
;;;     execute-command
;;;     prompt-for
;;;     remember-value
;;;     remember-values
;;;
;;;  CMDL Commands:
;;;     abort
;;;     backtrace
;;;     compile-file
;;;     continue
;;;     describe
;;;     disassemble
;;;     down
;;;     exit
;;;     frame
;;;     gc
;;;     help
;;;     inspect
;;;     load
;;;     loc
;;;     ls
;;;     macroepxand
;;;     macroexpand-1
;;;     package
;;;     pprint
;;;     pwd
;;;     restarts
;;;     up
;;;
;;;  Internal Functions:
;;;     command-loop
;;;     debugger
;;;     show-restarts
;;;
;
(in-package :cmdl)


(ext:deftlv *cmdl-level*)

;;;; Debugger
;
(ext:deftlv *condition*)
(ext:deftlv *restarts*)
(ext:deftlv *frame-index* 7)

;;;; *abort*
;;;
;;; Description:
;;;  Contains "abort" restart object of the inner most command-loop. Command
;;;  abort use this.
;
(ext:deftlv *abort*)
(ext:deftlv *last-abort*)

;;;; Input History
;
(ext:deftlv *input-history-head*)
(ext:deftlv *input-history-last*)
(ext:deftlv *input-history-count*)
(ext:deftlv *input-history-limit* 30)

;;;; Compile and Load
;
(ext:deftlv *source-file-types* '("LISP" "CL" "L" "LSP"))
(ext:deftlv *last-pathname*)

(ext:deftlv *cmdl-standard-input*)
(ext:deftlv *cmdl-standard-output*)


;;;; command-loop
;
(defun command-loop ()
  (labels (
    ;; prompt-char
    ;;  Prints prompt and read one character.
    (prompt-char (level)
      (loop
        ;(fresh-line)
        (clear-input)
        (format t "~A[~D]"
            (si::package-nickname *package*)
            *input-history-count* )
        (loop repeat (1+ level) do (write-char #\>))
        (write-char #\Space)
        (force-output)
      (loop
        (let ((char (read-char nil nil)))
          (unless char (return-from prompt-char nil))
          (when (char= #\Newline char) (return))
          (unless (si::whitespace-char-p char)
            ;; BUGBUG: Until we have console-stream, we use terpri to reset
            ;; column.
            (fresh-line)
            (unread-char char)
            (incf *input-history-count*)
            (return-from prompt-char char) ) ))) )

    ;; process
    (process (char)
      (cond
        ((char= #\, char)
          (read-char)   ; discard comma(,)
          (read-eval-print) )

        ((char= #\: char)
          (read-char)   ; discard colon(:)
          (let* ((name    (read-word))
                 (command (find-command name)) )
            (if command
                (process-command command)
              (progn
                (clear-input)
                (format t "; No such command: ~A~%" name) )) ) )

        ;; BUGBUG: What '1' is? We should use symbolc name instead of
        ;; numberic value.
        ((eql 1 (si::get-char-syntax char *readtable*))
          (let* ((string (read-word))
                 (thing  (or (find-command string)
                             (ignore-errors (read-from-string string)) )) )
            (cond
              ((commandp thing)
                (process-command thing) )

              ((symbolp thing)
                (if (boundp thing)
                    (remember-value (symbol-value thing) t)
                  (progn
                    (clear-input)
                    (format t "; No such command or variable: ~A~%"
                            string ) )) )
              (t
                (clear-input)
                (remember-value thing t) )) ) )

        (t
          (read-eval-print) )) )

    ;; process-command
    (process-command (command)
      (let* ((last (list nil))
             (head last) )
          (labels (
            (add-arg (spec)
              (block nil
                (let ((char (read-char nil nil)))
                  (unless char (return nil))
                  (unread-char char)
                  (when (char= #\Newline char) (return nil))
                  (let ((arg (read-arg spec)))
                    (when (and (not arg) (eq 'string spec))
                      (return nil) )
                    (setq last (setf (cdr last) (list arg))) )
                  t )) )

            (read-arg (spec)
              (ecase (first spec)
                (eval   (eval (read-preserving-whitespace)))
                (quote  (read-preserving-whitespace))
                (string (read-word)) ) )
            )
            ;;
            ;; process-command
            ;;
            (dolist (spec (command-reqs command))
              (unless (add-arg spec) (return)) )

            (dolist (spec (command-opts command))
              (unless (add-arg spec) (return)) )

            (let ((spec (command-rests command)))
              (unless spec (setq spec '(string rest)))
              (loop while (add-arg spec)) )
            (apply #'execute-command command (rest head)) ) ) )


    ;; read-eval-print
    ;;
    (read-eval-print ()
      (let ((form (read)))
        (remember-input form)
        (remember-values (multiple-value-list (eval form)) t) ) )

    ;; read-word
    ;;
    (read-word ()
      (loop for char = (read-char nil nil)
            unless char return nil
            if (not (si::whitespace-char-p char))
              collect char into word
            else do
              (when (or word (char= #\Newline char))
                (unread-char char)
                (return (coerce word 'string)) )) )
    )
    ;;
    ;; command-loop
    ;;
    (let* ((level               *cmdl-level*)
           (*cmdl-level*        (1+ level))
           (*frame-index*       7)
           (*package*           *package*)
           (*readtable*         *readtable*)
           (*abort*             *last-abort*)
           (*standard-input*    *cmdl-standard-input*)
           (*standard-output*   *cmdl-standard-output*) )
      (loop
        (with-simple-restart (abort "Return to command level ~D." level)
          (let ((*last-abort* (find-restart 'abort)))
            (loop
              (let ((char (prompt-char level)))
                (cond
                   (char
                     (process char) )
                    ((>= level 1)
                      (return-from command-loop) )
                    (t
                      (unless (interactive-stream-p *standard-input*)
                        (return-from command-loop) ) )) )) ) )
          (format t "; Re-enter command level ~D.~2%" level) ) ) ) )


;;;; execute-command
;
(defun cmdl:execute-command (name &rest args)
    (declare (dynamic-extent args))
  (let* ((last    (last args))
         (head    (if last
                      (cons nil args)
                    (setq last (list nil)) )) )
    (labels (
      (add-arg (spec)
        (setq last (setf (cdr last) (list (read-arg spec)))) ) 

      (read-arg (spec)
        (multiple-value-bind (value got-p)
            (prompt-for (format nil "~A=" (second spec)) (first spec))
          (unless got-p (return-from execute-command))
          value ) )

    (show-usage (command)
      ;; BUGBUG: Should use format directives
      (format t "; Usage: ~A" (first (command-names command)))
      (dolist (spec (command-reqs command))
        (format t " ~A" (second spec)) )
      (when (command-opts command)
        (format t " [")
        (loop
          for first? = t then nil
          for spec in (command-opts command)
            unless first? do (write-char #\Space)
              do (princ (second spec)) )
       (format t "]") )
       (when (command-reqs command) (write-string "..."))
       (terpri) )
      )
      ;;
      ;; execute-command
      ;;
      (let ((command (find-command name t))
            (runner    args) )

        ;; required arguments
        (dolist (spec (command-reqs command))
          (if runner (pop runner) (add-arg spec)) )

        (dolist (spec (command-opts command))
          (pop runner) )

        (setq args (cdr head))

        (if (or (null runner) (command-rests command))
            (apply (command-function command) args)
          (show-usage command) ) ) ) ) )


;;;; get-last-pathname
;
(defun get-last-pathname (prompt)
  (or *last-pathname*
      (prompt-for prompt 'string) ) )


;;;; prompt-for
;;;
;;; BUGBUG: NYI: should use read-preserving-whitespace instead of read.
;
(defun cmdl:prompt-for (prompt &optional (kind 'quote))
  (labels (
    ;; prompt-read-char
    (prompt-read-char (prompt)
      (write-string prompt)
      (force-output)
      (loop
        (let ((char (read-char)))
          (unless (si::whitespace-char-p char)
            (return-from prompt-read-char char) )
          (when (char= #\Newline char)
            (return-from prompt-for (values nil nil)) ) )) )
    )
    ;;
    (clear-input)
    (let ((char (prompt-read-char prompt)))
      (unread-char char)
      (ecase kind
        (eval   (values (eval (read)) t))
        (quote  (values (read) t))
        (string (values (read-line) t)) ) ) ) )


;;;; remember-input
;
(defun remember-input (form)
  (labels (
    (safe-equal (x y &optional (depth 0))
      (and (<= depth 10)
           (cond
             ((eql x y) t)
             ((and (consp x) (consp y))
               (incf depth)
               (loop
                 (unless (safe-equal (car x) (car y) depth)
                   (return nil) )
                 (setq x (cdr x))
                 (setq y (cdr y))
                 (unless (and (consp x) (consp y))
                   (return (safe-equal x y depth)) )) )
             (t nil) )) )
    )
    ;;
    ;; remember-input
    ;;
    (unless (safe-equal form (first *input-history-last*))
      (when (>= (length *input-history-head*) *input-history-limit*)
        (pop *input-history-head*) )
      (setq *input-history-last*
            (setf (cdr *input-history-last*) (list form) )))
    (shiftf +++ ++ + - form)
    form ) )


;;;; remember-value
;
(defun cmdl:remember-value (value &optional print-p)
  (remember-values (list value) print-p) )


;;;; remember-values
;
(defun cmdl:remember-values (values &optional print-p)
    (declare (dynamic-extent values))
  (when (and values (not (eq #.(si::unbound-marker) (first values))))
    (shiftf *** ** * (first values))
    (shiftf /// // / values) )
  (when print-p
    (cond
      ((null values) (fresh-line))
      ((null (rest values))
        (format t "~&~W~%" (first values)) )
      (t
        (loop for value in values
              for nth = 0 then (1+ nth)
              do (format t "~&; Values[~D]=~S~%" nth value) ) )))
  (values-list values) )


;;;; show-restarts
;
(defun show-restarts ()
  (when (null *restarts*)
    (format t "; There is no avaialble restart.~%")
    (return-from show-restarts) )

  (format t "~&;;; Restart actions:~%")
  (loop for restart in *restarts*
        for index = 0 then (1+ index)
        do (format t ";  cont ~2D - ~A~%" index restart) )
  (format t ";~%")
  (format t ";   Type 'cont' followed by number, e.g. 'cont 0' to invoke the first restart.~%")
  (format t ";   Type 'abort' to exit from debugger.~2%")
  (values) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Commands
;;;;


;;;; abort
;
(defcommand abort ()
  "Exit from current command loop."
  (if (>= *cmdl-level* 2)
      (invoke-restart-interactively *abort*)
    (progn
      (format t "~%; You are in toplevel command-loop.~%"
      (format t "; Type 'exit' to exit from lisp.~%") ) )) )


;;;; chdir
;;;
;;; Usage:
;;;   chdir [directory]
;
(defcommand chdir (&optional (string directory))
  "Change or show current directory."
  :alias cd
  (when directory
    (setf (si::.current-directory) directory) )
  (format t "; Current directory is ~A.~%" (si::.current-directory)) )


;;;; compile-file
;
(defcommand compile-file (&optional ((string filename)
                                            (get-last-pathname "Compile: ") ))
  "Compile file."
  :alias cf

  (setq *last-pathname* filename)

  (multiple-value-bind (object warning? failure?)
      (compile-file filename :verbose t)
      (declare (ignore object))
    (cond
      (failure?
        (format t "; Failed to compile.~%") )
      (warning?
        (format t "; ~D warnings.~%" warning?) )) ) )


;;;; compiler-macroexpand-1
;
(defcommand compiler-macroexpand-1 (&optional ('form *))
  "Expand compiler macro."
  :alias cm1
  (when (consp form)
    (let ((expander (compiler-macro-function (first form))))
      (if (not expander)
          (format t "; ~S isn't compiler macro name.~%" (first form))
        (let ((expansion (funcall expander form nil)))
          (if (not expansion)
            (format t "; No expansion~%")
          (let ((*print-pretty* t))
            (remember-value expansion t) )) )) )) )


;;;; condition
;
(defcommand condition ()
  "Show the last signaled condition."
  :alias cond
  (if (not *condition*)
      (format t "; No condition has been signaled.~%")
    (progn
      (format t "; ~A~%" *condition*)
      (remember-value *condition* t) )) )


;;;; continue
;
(defcommand continue (nth)
  "Continue with specified restart point."
  :alias cont
  (let ((restart (nth nth *restarts*)))
    (cond
      ((not *restarts*)
        (format t "; There is no restart.~%") )
      ((null restart)
        (format t "; No such restarts: ~D~%" nth)
        (format t "; Use restart command to list avaialble restarts.~%") )
      (t
        (format t "; Continue: ~A~%" restart)
        (invoke-restart-interactively restart) )) ) )


;;;; describe
;
(defcommand describe (&optional (object *))
  "Describe object."
  :alias des
  (describe object)
  (remember-value object) )


;;;; disassemble
;
(defcommand disassemble (&optional ('fspec *))
  "Disassemble function"
  :alias dis
  (let ((fn (parse-fspec fspec nil)))
    (if fn
        (progn
          (disassemble fn)
          (remember-value fn) )
      (format t "; No such function ~S.~%" fspec) ) ) )


;;;; exit
;
(defcommand exit ()
  "Exit from lisp."
  :alias (bye quit)
  (si::.exit-lisp 0) )

;;;; gc
;
(defcommand gc ()
  "Invoke garbage collector."
  (ext:gc) )


;;;; help
;
(defcommand help (&optional ((string name)))
  "Show help message about command loop."
  :alias ?
  (if (not name)
      (loop for command being each hash-value of *commands*
            collect command into commands
            initially
              (format t "; Available commands:~%")
            finally
              (setq commands
                (sort commands #'string<
                      :key #'(lambda (c) (first (command-names c))) ))
              (setq commands (delete-duplicates commands :test #'eq))
              (loop for command in commands
                    do (format t ";   ~A~25T~A~%"
                               (first (command-names command))
                               (command-help command) )))
    (let ((command (find-command name)))
      (unless command
        ;; BUGBUG: NYI: show matched commands
        (format t "; No such command: ~A" name)
        (return-from help) )
      (format t "; Syntax:~%;    ~A ~A ~A ~A~%"
              (first (command-names command))
              (command-reqs command)
              (command-opts command)
              (command-rests command) )
      (format t "; Aliases:~%;   ~A~%" (rest (command-names command)))
      (format t "; Description:~%;  ~A~2%" (command-help command)) )) )


;;;; inspect
;
(defcommand inspect (&optional (object *))
  "Inspect object."
  :alias ins
  (inspect object) )


;;;; load
;
(defcommand load (&optional ((string filename)
                                    (get-last-pathname "Load: " )))
  "Load source file or object file into lisp."
  :alias ld
  (labels (
    (check-file (pathname)
      (or (ignore-errors (file-write-date pathname)) 0) )

    (make-typed-filename (pathname type)
      (make-pathname :type     type
                     :case     :common
                     :defaults pathname ) )
    )
    ;;
    ;; load
    ;;
    (let* ((pathname (pathname filename))
           (type     (pathname-type filename :case :common)) )
      (when (or (null type) (string/= "FASL" type))
        (let* ((fasl-name (make-typed-filename pathname "FASL"))
               (fasl-time (check-file fasl-name))
               (lisp-name nil)
               (lisp-time 0) )

          (dolist (type (if type (list type) *source-file-types*))
            (let ((test-name (make-typed-filename pathname type)))
              (when (setq lisp-time (check-file test-name))
                (setq lisp-name test-name)
                (return) ) ) )

          (when (and (zerop fasl-time) (null lisp-name))
            (format t "; No such file to load: ~A~%" filename)
            (return-from load) )

          (setq pathname (if (> fasl-time lisp-time) fasl-name lisp-name))
          (setq filename (namestring
                            (make-pathname :type :unspecific
                                           :defaults pathname ))) ))

      (with-open-file (in pathname :if-does-not-exist nil)
        (if in
            (progn
              (setq *last-pathname* filename)
              (load in :verbose t) )
          (format t "; No such file to load: ~A~%" pathname) ) ) ) ) )


;;;; ls
;
(defcommand ls (&optional ((string arg) "*"))
  "List contents of directory."
  (loop
    with pathnames = (directory arg)
    with names = (make-array (length pathnames))
    with max =
      (loop
         with defaults = (make-pathname :name nil :type nil
                                        :defaults (truename arg) )
         for pathname in pathnames
         for index = 0 then (1+ index)
         do (setf (svref names index) (enough-namestring pathname defaults))
         maximize (length (svref names index)) )
    with col-size = (* 8 (ceiling (+ 1 max) 8))
    with ncols    = (truncate 78 col-size)
    with nrows    = (ceiling (length names) ncols)
    for row from 0 below nrows do
      (loop
        for col from 0 below ncols
        for index = row then (+ index nrows)
        do
          (when (>= index (length names)) (return))
          (format t "~VA" col-size (svref names index)) )
      (terpri) ) )


;;;; macroexpand
;
(defcommand macroexpand (&optional ('form *))
  "Expand macro using macroexpand."
  :alias me
  (when (consp form)
    (let ((expander (macro-function (first form))))
      (if (not expander)
          (format t "; ~S isn't macro name.~%" (first form))
      (multiple-value-bind (expansion expanded-p)
          (macroexpand form)
        (if (not expanded-p)
            (format t "; No expansion~%")
          (let ((*print-pretty* t))
            (remember-value expansion t) )) )) )) )


;;;; macroexpand-1
;
(defcommand macroexpand-1 (&optional ('form *))
  "Expand macro using macroexpand-1."
  :alias m1
  (when (consp form)
    (let ((expander (macro-function (first form))))
      (if (not expander)
          (format t "; ~S isn't macro name.~%" (first form))
      (multiple-value-bind (expansion expanded-p)
          (macroexpand-1 form)
        (if (not expanded-p)
            (format t "; No expansion~%")
          (let ((*print-pretty* t))
            (remember-value expansion t) )) )) )) )


;;;; package
;;;
;;; Usage:
;;;   package [directory]
;
(defcommand package (&optional 'package)
  "Change or show current package."
  :alias pkg
  (when package
     (unless (find-package package)
        (format t "; No such package: ~S" package) )
      (si::%in-package package) )
  (format t "; Current package is ~A.~%" *package*) )


;;;; pprint
;
(defcommand pprint (&optional (object * object-p))
  "Pretty print."
  :alias pp
  (pprint object)
  (terpri)
  (when object-p
    (remember-value object) ) )


;;;; pwd
;
(defcommand pwd ()
  "Print working directory."
  (format t "; Current working directory is ~A.~%" (si::.current-directory)) )


;;;; restarts
;
(defcommand restarts ()
  "Show available restats."
  :alias rs

  (show-restarts) )
