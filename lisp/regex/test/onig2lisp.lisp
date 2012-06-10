;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: EXTENSION; Base: 10 -*-
;;;;
;;;; regex - test case generator for Onigruma
;;; lisp/regex/test/onig2lisp.lisp
;;;
;;; This file is NOT part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: /proj/evcl/lisp/regex/test/onig2lisp.lisp 21 2006-07-13 15:01:00 yosi $
;;;
;;; Description:
;;;  This file contains test harness for regex facility.
;;;
;;; See Also:
;;;     For Onigrma:
;;;         http://www.geocities.jp/kosako1/oniguruma/
;;;
;;;     For Ruby's Regex
;;;         http://www.rubycentral.com/book/ref_c_regexp.html
;
(in-package :cl-user)

;;;; Oniguruma Regex specific notations
;;;     (?<name> ...)       Named capture
;;;     \g<name>            Named capture call
;;;     \g<nth>             Indexed capture call
;;;     \k<name>            Named back reference
;;;     \C-x                Control character
;;;     \M-x                Meta character (x | 0x80)
;;;     \M-\C-x             Control meta
;;;     (?@...), (?@<name>...)  Capture history (disabled by default)
;;;     POSIX bracket       CC && CC, [range && range]
;;;
;;;  POSIX bracket ([:xxxxx:], negate [:^xxxxx:])
;;;      alnum    alphabet or digit char
;;;      alpha    alphabet
;;;      ascii    code value: [0 - 127]
;;;      blank    \t, \x20
;;;      cntrl
;;;      digit    0-9
;;;      graph
;;;      lower
;;;      print
;;;      punct
;;;      space    \t, \n, \v, \f, \r, \x20
;;;      upper
;;;      xdigit   0-9, a-f, A-F


;;; Test case format:
;;;   Cases are written as function calls as follows:
;;;     x(pattern, subject, start, end, [nth=0])    -- match test
;;;         Apply pattern to subject.
;;;         Expect to match and nth capture contains subject string from
;;;         start to end.
;;;
;;;     i(pattern, subject, start, end, [nth=0]) -- ignored case
;;;         Ignored test case.
;;;
;;;     n(pattern, subject) -- negative test
;;;         Apply pattern to subject and expect to NOT match.
;;;
;;;     r(pattern, subject, expect, [pos=nil])  -- rindex test
;;;         Calls rindex(pattern, pos) method of string object of subject.
;;;         Expect result equals expect. If pos is specified and is not nil,
;;;         scanning starts from pos.
;;;


;;;; onig-to-lisp
;;;
;
(defun onig-to-lisp (input &optional output (prefix "onig"))
  (when (null output)
    (setq output (merge-pathnames (make-pathname :type "retest") input)) )
  (with-open-file (in input)
  (with-open-file (out output :direction :output :if-exists :supersede)
  (let ((line-count 0)
        (token-buf  nil)
        (case-count 0)
        (var-s      "")     ; for s = <<EOS ... EOS
        (strbuf
          (make-array 1000
            :element-type 'character
            :fill-pointer 0
            :adjustable t ) ) )
  (labels (
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Source Buffer
    ;;

    ;; get-char
    (get-char (&optional (eof-char #\Newline))
      (let ((char (read-char in nil eof-char)))
        (when (eql char #\Newline)
          (incf line-count) )
        char ) )

    ;; unget-char
    (unget-char (char)
      (when (eql char #\Newline) (decf line-count))
      (unread-char char in) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Lexical Analyzer
    ;;
    ;; get-token
    (get-token ()
      (if token-buf
          (shiftf token-buf nil)
        (progn (get-token-aux)) ) )

    ;; get-token-aux
    (get-token-aux ()
      (let ((char (get-char nil)))
        (cond
          ((null char) nil)
          ((or (char= char #\u0022) (char= char #\u0027))
            (get-token/string char) )
          ((char= char #\/)     (get-token/regex))
          ((char= char #\#)     (get-token/comment) (get-token-aux))
          ((alpha-char-p char)  (unget-char char)   (get-token/name))
          ((digit-char-p char)  (unget-char char)   (get-token/number))
          ((space-char-p char)  (get-token-aux))
          (t (get-token/operator char)) ) ) )

    ;; get-token/comment
    (get-token/comment ()
      (loop
        for char = (get-char)
        until (char= char #\Newline) ) )

    ;; get-token/here-doc
    (get-token/here-doc ()
      (loop
        with marker =
          (loop
            initially (setf (fill-pointer strbuf) 0)
            for char = (get-char)
            until (eql char #\Newline)
            do (vector-push char strbuf)
            finally
              (when (zerop (length strbuf))
                (error "No marker after <<") )
              (return
                (prog1 (copy-seq strbuf)
                  (setf (fill-pointer strbuf) 0) )) )
        with start = 0
        for char = (get-char) do
          (vector-push-extend char strbuf)
          (when (eql char #\Newline)
            (when (string= strbuf marker
                    :start1 start
                    :end1 (1- (fill-pointer strbuf)) )
              (decf (fill-pointer strbuf) (+ (length marker) 1))
              (loop-finish) )
            (setq start (fill-pointer strbuf)) )
        finally
          (return (copy-seq strbuf)) ) )

    ;; get-token/name
    (get-token/name ()
      (loop
        initially
           (setf (fill-pointer strbuf) 0)
        for char = (get-char)
        while (name-char-p char) do
          (vector-push-extend (char-upcase char) strbuf)
        finally
          (unget-char char)
          (assert (plusp (length strbuf)))
          (return (intern strbuf :cl-user)) ) )

    ;; get-token/operator
    (get-token/operator (char)
      (case char
        ((#\u0028) :open)
        ((#\u0029) :close)
        ((#\u005B) :open2)
        ((#\u005D) :close2)
        ((#\.) :dot)
        ((#\,) :comma)
        ((#\?) :question)
        ((#\*)         ; * *= *<name>
          (setq char (get-char))
          (cond
            ((first-name-char-p char)
              (unget-char char)
              (get-token/name) )
            ((char= char #\=) '*=)
            (t (unget-char char) '*)) )
        ((#\$)
          (setq char (get-char))
          (unless (first-name-char-p char)
            (parse-error "Invalid $ operator") )
          (unget-char char)
          `(variable ,(get-token/name)) )
        ((#\+)
          (setq char (get-char))
          (case char
            ((#\+) :++)
            ((#\=) :+=)
            (otherwise (unget-char char) :+) ) )
        ((#\&)
          (setq char (get-char))
          (case char
            ((#\&) :&&)
            ((#\=) :&=)
            (otherwise (unget-char char) :&) ) )
        ((#\%)
          (setq char (get-char))
          (case char
            ((#\=) :%=)
            (otherwise (unget-char char) :%) ) )
        ((#\-)
          (setq char (get-char))
          (case char
            ((#\-) :--)
            ((#\=) :-=)
            (otherwise (unget-char char) :-) ) )
        ((#\<)
          (setq char (get-char))
          (case char
            ((#\=) :<=)
            ((#\<) (get-token/here-doc))
            (otherwise (unget-char char) :<) ) )
        ((#\=)
          (setq char (get-char))
          (case char
            ((#\=) :==)
            (otherwise (unget-char char) :=) ) )
        ((#\>)
          (setq char (get-char))
          (case char
            ((#\=) :>=)
            (otherwise (unget-char char) :>) ) )
        ((#\!)
          (setq char (get-char))
          (case char
            ((#\=) :!=)
            (otherwise (unget-char char) :!) ) )
        (otherwise
          (parse-error "Unknown operator: ~S" char) )) )

    ;; get-token/number
    (get-token/number ()
      (loop
        with num = 0
        for char = (get-char)
        for digit = (digit-char-p char)
        while digit do
          (setq num (+ (* num 10) digit))
        finally
          (unget-char char)
          (return num) ) )

    ;; get-token/regex
    (get-token/regex ()
      (loop
        with pattern =
          (prog1
              (get-token/string #\/)
            (setf (fill-pointer strbuf) 0) )
        for char = (get-char) do
          (case char
            ((#\n))
            ((#\e))
            ((#\u))

            ((#\i) (vector-push char strbuf))
            ((#\s) (vector-push char strbuf))
            ((#\m) (vector-push char strbuf))
            ((#\x) (vector-push char strbuf))
            (otherwise (loop-finish)) )
        finally
          (unget-char char)
          (if (zerop (length strbuf))
              (return `(regex ,pattern))
            (return `(regex ,pattern ,(copy-seq strbuf))) )) )

    ;; get-token/string
    (get-token/string (delimiter)
      (loop
        initially
          (setf (fill-pointer strbuf) 0)
        with state = :normal
        with acc = 0
        with count = 0
        for char = (get-char delimiter) do
          (case state
            ((:normal)
              (cond
                ((eql char #\\) (setq state :escape))
                ((eql char delimiter) (loop-finish))
                (t (vector-push-extend char strbuf)) ) )
            ((:escape)
              (cond
                ((eql char delimiter)
                  (vector-push-extend char strbuf)
                  (setq state :normal) )
                ((eql char #\n)
                  (vector-push-extend #\Newline strbuf)
                  (setq state :normal) )
                ((eql char #\x)
                  (if (eql delimiter #\/)
                      (progn
                        (vector-push-extend #\\ strbuf)
                        (vector-push-extend #\x strbuf)
                        (setq state :normal) )
                    (progn
                      (setq acc 0)
                      (setq count 0)
                      (setq state :hex) )) )
                ((and (not (eql delimiter #\/)) (digit-char-p char 8))
                  (unget-char char)
                  (setq acc 0)
                  (setq count 0)
                  (setq state :octal) )
                (t
                  (vector-push-extend #\\ strbuf)
                  (vector-push-extend char strbuf)
                  (setq state :normal) )) )
            ((:hex)
              (let ((digit (digit-char-p char 16)))
                (unless digit (parse-error "Expect xdigit: ~S" char))
                (setq acc (logior (ash acc 4) digit))
                (incf count)
                (when (eql count 2)
                  (vector-push-extend (code-char acc) strbuf)
                  (setq state :normal) ) ) )
            ((:octal)
              (let ((digit (digit-char-p char 8)))
                (unless digit (parse-error "Expect digit: ~S" char))
                (setq acc (logior (ash acc 3) digit))
                (incf count)
                (when (eql count 3)
                  (vector-push-extend (code-char acc) strbuf)
                  (setq state :normal) ) ) ))
        finally
          (assert (eq state :normal))
          (return (copy-seq strbuf)) ) )

    ;; first-name-char-p
    (first-name-char-p (char)
      (or (name-char-p char) (char= char #\$)) )

    ;; name-char-p
    (name-char-p (char)
      (or (alphanumericp char) (char= char #\_)) )

    ;; space-char-p
    (space-char-p (char)
      (member char '(#\Space #\Tab #\Newline #\Return)) )

    ;; unget-token
    (unget-token (token)
        (assert (null token-buf))
      (when token
        (setq token-buf token) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Parser
    ;;
    (operator-p (token)
      (keywordp token) )

    (reserved-p (name)
      (find name '(if else end)) )

    (parse-error (control &rest args)
      (error "Syntax error: ~A(~D): ~?" input line-count control args) )

    ;; parse-expr
    (parse-expr ()
      (progn (parse-expr-aux nil) ) )

    ;; parse-expr-aux
    (parse-expr-aux (close)
      (loop
        with expr-1 = (parse-expr-factor)
        for token = (get-token) do
          (when (or (eq token close) (eq token :comma))
            (unget-token token)
            (loop-finish) )

          (unless (operator-p token)
            (unget-token token)
            (loop-finish) )

          (case token
            ((:close)
              (parse-error "Umatched close paren.") )
            ((:close2)
              (parse-error "Umatched close bracket.") ))

          (let ((op token)
                (expr-2 (parse-expr-factor)) )
            (setq expr-1 `(,op ,expr-1 ,expr-2)) )
        finally
          (return expr-1) ) )

    ;; parse-expr-factor
    (parse-expr-factor ()
      (let ((token (get-token)))
        (cond
          ((eq token :open)
            (parse-subexpr :close) )
          ((eq token :open2)
            `(vector ,(parse-list :close2)) )
          ((not (symbolp token))
            token )
          ((keywordp token)
            token )
          ((reserved-p token)
            token )
          (t
            (let ((name token))
              (loop
                (setq token (get-token))
                (case token
                  ((:open)
                   (setq name `(funcall ,(1+ line-count)
                                    ,name ,@(parse-list :close ))) )
                  ((:open2)
                    (setq name `(aref ,name ,@(parse-list :close2))) )
                  ((:dot)
                    (setq name `(slot-value ,name ,(get-token))) )
                  ((:question)
                    (return `(:question ,name)) )
                  (otherwise
                    (unget-token token)
                    (return name) ))) ) )) ) )

    ;; parse-subexpr
    (parse-subexpr (close)
      (prog1
          (parse-expr-aux close)
        (let ((token (get-token)))
          (assert (eq token close)) )) )

    ;; parse-list
    (parse-list (close)
      (let ((token (get-token)))
        (if (eq token close)
            nil
          (loop
            initially (unget-token token)
            collect (parse-expr-aux close)
            do (setq token (get-token))
            do (when (eq token close) (loop-finish))
            do (case token
                ((:comma))
                (otherwise
                  (parse-error "Expect comma or paren: ~S" token) ) ))) ) )

    ;; parse-stmt
    (parse-stmt ()
      (let ((token (get-token)))
        (case token
          ((nil) nil)
          ((def) (parse-stmt/def))
          ((if)  (parse-stmt/if))
          (otherwise
            (unget-token token)
            (let ((expr (parse-expr)))
              (setq token (get-token))
              (if (not (eq token 'if))
                  (progn (unget-token token) expr)
                `(if ,(parse-expr) ,expr) ) ) )) ) )

    ;; parse-stmt/def
    ;;  def <name> paramdef* statement* end
    (parse-stmt/def ()
      (loop
        with name = (get-token)
        with paramdefs =
          (progn
            (unless (eq (get-token) :open)
              (parse-error "Expect open paren") )
            (parse-list :close) )
        for token = (get-token)
        until (eq token 'end) do
          (unget-token token)
          collect (parse-stmt) into stmts
        finally
          (return `(def ,name ,paramdefs ,@stmts)) ) )

    ;; parse-stmt/if
    (parse-stmt/if ()
      (let ((cond (parse-expr))
            (then (parse-stmt))
            (token (get-token))
            (else nil) )
        (when (eq token 'else)
          (setq else (parse-stmt))
          (setq token (get-token)) )
        (unless (eq token 'end) (parse-error "Expect end"))
        `(if ,cond ,then ,@(and else (list else))) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Process
    ;;

    (unsupported-pattern-expr (expr)
      (parse-error "Pattern expression ~S isn't supported." expr) )

    ;; process-pattern
    ;;  (regex string modifiers)
    ;;  (funcall lid (slot-value regexp new) string)
    (process-pattern (expr)
      (cond
        ((not (consp expr)) (unsupported-pattern expr))
        ((eq (first expr) 'regex) (second expr))
        ((and (eq (first expr) 'funcall)
              (equal (third expr) '(slot-value regexp new)) )
          (fourth expr) )
        (t (unsupported-pattern-expr expr)) ) )

    ;; process-string
    ;;  string
    ;;  s
    (process-string (expr)
      (cond
        ((stringp expr) expr)
        ((and (eq expr 's) (stringp var-s)) var-s)
        (t
          (parse-error "String expression ~S isn't supported." expr) )) )

    ;; process-stmt
    (process-stmt (stmt)
      (case (and (consp stmt) (first stmt))
        ((funcall) (process-stmt/funcall stmt))
        ((:=)      (process-stmt/assign stmt))
        ((nil))
        (otherwise
          (dolist (stmt (rest stmt))
            (process-stmt stmt) ) )) )

    ;; process-stmt/assign
    (process-stmt/assign (stmt)
      (destructuring-bind (name value) (rest stmt)
        (when (eq name 's)
          (setq var-s value) ) ) )

    ;; process-stmt/funcall
    (process-stmt/funcall (stmt)
      (destructuring-bind (lid name &rest args) (rest stmt)
        (case name
          ((i) (process-stmt/funcall-x lid args))
          ((n) (process-stmt/funcall-n lid args))
          ((r) (process-stmt/funcall-r lid args))
          ((x) (process-stmt/funcall-x lid args)) ) ) )

    ;; process-stmt/funcall-n
    (process-stmt/funcall-n (lid args)
      (destructuring-bind (pattern string) args
        (setq pattern (process-pattern pattern))
        (when (null pattern) (return-from process-stmt/funcall-n))

        (setq string  (process-string string))
        (when (null string) (return-from process-stmt/funcall-n))

        (emit-case lid pattern string '(:single-line nil :unicode t) nil) ) )

    ;; process-stmt/funcall-r
    (process-stmt/funcall-r (lid args)
      (destructuring-bind (pattern string bexpect &optional bend) args
        (setq pattern (process-pattern pattern))
        (when (null pattern) (return-from process-stmt/funcall-r))

        (setq string  (process-string string))
        (when (null string) (return-from process-stmt/funcall-r))

        (let* ((expect (map-bpos-to-cpos string bexpect))
               (end
                 (if (null bend)
                     (length string)
                   (map-bpos-to-cpos string bend) ) )
               (gid (format nil "~A/~A" prefix lid))
               (form
                 `(test-case/r
                    ,gid
                    ,(escape pattern)
                    ,(escape string)
                    ,end
                    ,expect ) ))
        (format out ";;;; ~A: ~A~%" gid (escape pattern))
        (format out ";;;    ~S.rindex(~S~@[, ~D~]) == ~D~%"
            (escape string)
            (escape pattern)
            end
            expect )
        (format out ";~%")
        (format out "~:W~2%" form) ) ) )

    ;; process-stmt/funcall-x
    (process-stmt/funcall-x (lid args)
      (destructuring-bind (pattern string bstart bend &optional (nth 0)) args
        (setq pattern (process-pattern pattern))
        (when (null pattern) (return-from process-stmt/funcall-x))

        (setq string  (process-string string))
        (when (null string) (return-from process-stmt/funcall-x))

        (let* ((mstart (map-bpos-to-cpos string bstart))
               (mend   (map-bpos-to-cpos string bend))
               (options '(:single-line nil :unicode t))
               (expect
                 (loop
                   with expect = (list (subseq string mstart mend))
                   repeat nth do (push ''* expect)
                   finally (return expect) ) ))
          (emit-case lid pattern string options expect) ) ) )

    ;; emit-case
    (emit-case (lid pattern string options expect)
      (incf case-count)
      (let* ((gid (format nil "~A/~A" prefix lid))
             (form
                `(test-case
                    ,gid
                    ,(escape pattern)
                    ,(escape string)
                    ',options
                    (list ,@(mapcar #'escape expect)) ) ))
        (format out ";;;; ~A: ~A~%" gid (escape pattern))
        (format out ";;;~%;;;    Matched~%;;;~%;~%")
        (format out "~:W~2%" form) ) )

    ;; escape
    ;;  Escape non-ASCII with \uXXXX
    (escape (frob)
      (cond
        ((not (stringp frob)) frob)
        ((null (position-if #'need-escape-p frob)) frob)
        (t `(backslash ,(escape-string frob))) ) )

    ;; escape-string
    (escape-string (string)
      (with-output-to-string (s)
        (loop for char across string do
           (cond
             ((not (need-escape-p char))
               (write-char char s) )
             ((eql char #\Newline)
               (write-string "\\n" s) )
             (t
               (format s "\\u~4,'0X" (char-code char)) ))) ) )

    ;; need-escape-p
    (need-escape-p (char)
      (not (<= #x20 (char-code char) #x7F)) )

    ;; map-bpos-to-cpos
    (map-bpos-to-cpos (string bpos)
      (loop
        with bcount = 0
        for pos below (length string)
        for char = (char string pos) do
          (when (>= bcount bpos) (loop-finish))
          (if (>= (char-code char) #x1000)
              (incf bcount 2)
            (incf bcount) )
        finally (return pos) ) )
    )
    ;;
    (loop
      for stmt = (parse-stmt)
      while stmt do
        (process-stmt stmt)
      finally (return (values case-count line-count)) ) ) ) ) ) )


#|
(onig-to-lisp "/src/oniguruma-2004-06-17/test.rb")
|#
