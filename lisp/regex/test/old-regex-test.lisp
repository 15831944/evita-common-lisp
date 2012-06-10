;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: EXTENSION; Base: 10 -*-
;;;;
;;;; regex - execute
;;; lisp/regex/test/regx-test.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: /proj/evcl/lisp/regex/test/old-regex-test.lisp 18 2006-07-13 15:01:00 yosi $
;;;
;;; Description:
;;;  This file contains test harness for regex facility.
;
(in-package :cl-user)

;;;; *perl-test-files*
;;;
;;; Description:
;;;  Contains list of test case filenames.
;
(defparameter *perl-test-files* '(
    "regex-test-evita.txt"
    "regex-test-perl504.txt"
    "regex-test-perl584.txt"
 ) )


;;;; *perl-failed-cases*
;;;
;;; Description:
;;;  Contains alist of test-case-id and reason for Perl format test cases.
;
(defvar *perl-failed-cases*)


;;;; *pcre-failed-cases*
;;;
;;; Description:
;;;  Contains alist of test-case-id and reason for PCRE format test cases.
;
(defvar *pcre-failed-cases*)

;;;; *variables*
;;;
;;; Description:
;;;  Contains hash-table for variable name interpolation.
;
(defvar *variables* (make-hash-table :test 'equal))

;;; Variables used by regex-test-perl584.txt
(progn
  (setf (gethash "bang" *variables*) "!")

  (setf (gethash "nulnul" *variables*)
    (make-string 1 :initial-element (code-char 0)) )

  (setf (gethash "ffff" *variables*)
    (make-string 1 :initial-element (code-char #xFFFF)) )
  ) ; pron


;;;; compile-pattern
;
(defun compile-pattern (pattern)
    (declare (type simple-string pattern))
    (declare (values ext:regex))
  (labels (
    ;; extract-pattern
    (extract-pattern (pattern)
      (if (or (string= pattern "") (char/= (schar pattern 0) #\'))
          (values pattern '(:single-line nil))
        (loop
          with options = '(:single-line nil)
          for pos downfrom (1- (length pattern))
          for char = (schar pattern pos)
          until (char= char #\') do
            (case char
              ((#\i)
                (setq options (list* :ignore-case t options)) )
              ((#\m)
                (setq options (list* :multiple-line t options)) )
              ((#\s)
                (setq options (list* :single-line t options)) )
              ((#\x)
                (setq options (list* :extended-syntax t options)) )
              (otherwise
                (error "Invalid option ~C" char) ))
          finally
            (return
              (values (subseq pattern 1 pos) options) ))) )
    )
    ;;
    ;; compile-pattern
    ;;

    ;; For regex-test-perl584
    (let ((pos (search "${bang}" pattern)))
      (when pos
        (setq pattern
          (concatenate 'string
            (subseq pattern 0 pos)
            "!"
            (subseq pattern (+ pos 7)) ))) )

    (multiple-value-bind (pattern options) (extract-pattern pattern)
      (values (apply #'ext:compile-regex pattern options) pattern) ) ) )


;;;; expand-backslash
;
(defun expand-backslash (text)
    (declare (type string text))
    (declare (values string))
  (with-output-to-string (s)
    (loop
      with state = :normal
      with name-start = 0
      for pos below (length text)
      for char = (char text pos) do
        (ecase state
          ((:normal)
            (case char
              ((#\\)
                (setq state :escape) )
              ((#\$)
                (setq state :dollar) )
              (otherwise
                (write-char char s) )) )
          ((:dollar)
            (case char
              ((#\u007B)
                (setq name-start (1+ pos))
                (setq state :variable) )
              (otherwise
                (write-char #\$ s)
                (write-char char s)
                (setq state :normal) )) )
          ((:escape)
            (let ((expand (cdr (assoc char '(
                        #+nil (#\a . #\u0007)
                        #+nil (#\b . #\u0008)
                        (#\f . #\u000C)
                        (#\n . #\Newline)
                        (#\t . #\Tab)
                        (#\v . #\u000B) ) )) ))
                (when (null expand)
                  (write-char #\\ s)
                  (setq expand char) )
                (write-char expand s)
                (setq state :normal) ) )
          ((:variable)
            (when (eql char #\u007D)
              (let ((name (subseq text name-start pos)))
                (write-string (gethash name *variables* "") s) )
              (setq state :normal) ) ))
      finally
        (ecase state
          ((:normal))
          ((:dollar) (write-char #\$ s))
          ((:variable) (error "${name} doesn't closed."))
          ((:escape) (error "Expect character after backslash")) )) ) )


;;;; expand-dollar
;;;
;;; Perl's predefined variables related to regex matching.
;;;     $&      match
;;;     $`      prematch            n/a
;;;     $'      postmatch           n/a
;;;     $+      last-paren-match
;;;     ${name} variable
;;;     @-      starts offset of the last successful match. Refered as $-[n].
;;;     @+      ends offset of the last successful match.
;
(defun expand-dollar (match template)
    (declare (type ext:regex-match match))
    (declare (type string template))
    (declare (values string))
  (with-output-to-string (s)
    (loop
      with state  = :normal
      with nchars = 0
      with capture = 0
      with name = (make-array 10
                        :element-type 'character
                        :fill-pointer t
                        :adjustable t )
      with vector-fn = nil
      with vector-index of-type sequence-index = 0
      for pos below (length template) do
        (let ((char (char template pos)))
          (ecase state
            ((:normal)
              (case char
                ((#\$)
                  (write-string (subseq template (- pos nchars) pos) s)
                  (setq nchars 0)
                  (setq state :dollar) )
                ((#\@)
                  (write-string (subseq template (- pos nchars) pos) s)
                  (setq nchars 0)
                  (setq state :at-mark) )
                ((#\\)
                  (write-string (subseq template (- pos nchars) pos) s)
                  (setq nchars 0)
                  (setq state :escape) )
                (otherwise
                  (incf nchars) )) )
            ((:at-mark)
              (let ((fn
                        (case char
                          ((#\-) #'ext:match-start)
                          ((#\+) #'ext:match-end)
                          (otherwise (error "Expect + or -")) ) ))
                (loop
                  with count = (ext:match-group-count match)
                  for index below count do
                    (format s "~D~@[ ~]"
                        (funcall fn match index)
                        (/= (1+ index) count) ))
                (setq state :normal) ) )
            ((:dollar)
              (case char
                ((#\$)
                  (write-char char s)
                  (setq state :normal) )
                ((#\&)
                  (let ((string (ext:match-string match 0)))
                    (when string (write-string string s))
                    (setq state :normal) ) )
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                  (setq capture (digit-char-p char))
                  (setq state :capture) )
                ((#\-)
                  (setq vector-fn #'ext:match-start)
                  (setq state :vector) )
                ((#\+)
                  (setq vector-fn #'ext:match-end)
                  (setq state :vector) )
                ((#\u007B)
                  (setf (fill-pointer name) 0)
                  (setq state :variable) )
                (otherwise
                  (format t "Invalid $-directive: ~C" char)
                  (setq state :normal) )) )
            ((:capture)
              (let ((digit (digit-char-p char)))
                (if digit
                    (setq capture (+ (* capture 10) digit))
                  (progn
                    (decf pos)
                    (write-string
                      (or (ext:match-string match capture) "") s )
                    (setq state :normal) )) ) )
            ((:escape)
              (write-char char s)
              (setq state :normal) )
            ((:variable)
              (if (char/= char #\u007D)
                  (vector-push-extend char name)
                (let ((value (gethash name *variables* "")))
                  (write-string value s)
                  (setq state :normal) )) )
            ((:vector)
              (unless (char= char #\u005B) (error "Expect ["))
              (setq vector-index 0)
              (setq state :vector-open) )
            ((:vector-open)
              (if (char= char #\u005D)
                  (let ((val (funcall vector-fn match vector-index)))
                    (when val (princ val s))
                    (setq state :normal) )
                (let ((digit (digit-char-p char)))
                  (if (not digit)
                      (error "Expect ]")
                    (progn
                      (setq vector-index (* vector-index 10))
                      (setq vector-index (+ vector-index digit)) )) )) )
        ) )
      finally
        (ecase state
          ((:normal)
            (write-string (subseq template (- pos nchars) pos) s) )
          ((:dollar :escape :variable)
            (error "Unexpected EOF: ~S" template) )
          ((:capture)
            (write-string
              (or (ext:match-string match capture) "") s) ))) ) )


;;;; perl-test-all
;
(defun perl-test-all (&optional verbose-p)
  (dolist (file *perl-test-files*)
    (format t "; TEST: ~A~%" file)
    (perl-test-file file verbose-p) ) )


;;;; perl-test-file
;
(defun perl-test-file (file &optional verbose-p)
    (declare (type ext:pathname-designator file))
    (declare (values (or null unsigned-byte)))
  (labels (
    ;; process-match
    (process-match (case)
      (destructuring-bind (pattern text response
                           &optional template expect comment ) case
            (declare (ignore comment))
        ;; expand \n and \b in text
        (setq text   (expand-backslash text))
        (setq expect (expand-backslash expect))

        (cond
          ((string= response "b")  (setq response "y"))
          ((string= response "yB") (setq response "y")) )

        (let* ((regex
                 (handler-case (compile-pattern pattern)
                   (error (c) c) ) )
               (result
                 (if (not (typep regex 'ext:regex))
                     "c"
                   (let ((match
                            (handler-case (ext:eval-regex regex text)
                              (ext:regex-error (c)
                                c ) ) ))
                     (cond
                       ((typep match 'ext:regex-error) "e")
                       ((not (ext:matched-p match)) "n")
                       ((string/= response "y") "m")
                       ((string= (expand-dollar match template) expect) "y")
                       (t "x") ) )) ))
            (cond
              ((string= result response)
                :succeeded )
              ((string= result "c")
                (values :compile regex) )
              ((string= result "n")
                :match )
              ((string= result "m")
                :match )
              ((string= result "x")
                :expand )
              ((string= result "e")
                :eval )
              (t
                (CAN-NOT-HAPPEN) )) ) ) )

    ;; process-split
    (process-split (case)
      (destructuring-bind (pattern limit text expect &optional comment) case
          (declare (ignore comment))
        (setq pattern (compile-pattern pattern))
        (setq text (expand-backslash text))
        (setq expect (expand-backslash expect))
        (setq expect (subst nil "nil" (split #\| expect) :test 'equal))
        (let ((result (ext:regex-split pattern text (parse-integer limit))))
          (if (equal result expect)
              :succeeded
            :failed ) ) ) )
    )
    ;;
    (setq file
        (merge-pathnames
            file
            (pathname "sys:source;regex;test;*.txt") ))
    (with-open-file (in file)
      (loop
        with fn =
            (if (search "split" (namestring file) :test 'string-equal)
                #'process-split
                #'process-match )
        with op =
            (if (search "split" (namestring file) :test 'string-equal)
                'test-split
                'test-match )
        with case-count   = 0
        with fail-count   = 0
        with ignore-count = 0
        with line-count   = 0
        for line = (read-line in nil)
        while line do
          (incf line-count)
          (unless (or (char= (char line 0) #\#)
                      (string= line "") )
            (incf case-count)
            (let* ((case (split #\Tab line))
                   (form `(,op ,@case)) )
              (when verbose-p
                (format t "; ~3D: ~S~%" line-count form) )
              (multiple-value-bind (result cond) (funcall fn case)

                (unless (eq result :succeeded)
                  (when (assoc form *perl-failed-cases* :test #'equal)
                    (incf ignore-count)
                    (setq result :succeeded) ))

                (unless (eq result :succeeded)
                  (incf fail-count)
                  (format t "; ~3D: FAIL: ~A ~21T~S~@[  ; ~A~]~%"
                      line-count result form cond )
                  (when (>= fail-count 100)
                    (error "Too many failures.") )) ) ))
        finally
          (format t ";   FAIL: ~D/~D~@[, ignore ~D~]~%"
            fail-count case-count
            (and (plusp ignore-count) ignore-count) )
          (return (values fail-count case-count ignore-count)) ) ) ) )

;;;; split
;
(defun split (separator string)
    (declare (values list))
  (when (string= string "") (return-from split nil))
  (ext:with-collector (collect)
    (loop
      with nchars = 0
      for pos from 0 below (length string)
      for char = (char string pos) do
        (if (char/= char separator)
            (incf nchars)
            (progn
              (collect (subseq string (- pos nchars) pos))
              (setq nchars 0) ))
      finally
        (collect (subseq string (- pos nchars) pos)) ) ) )


;;;; test-match
;
(defun test-match (pattern text
                     &optional response template expect comment )
    (declare (ignore response expect comment))
  (setq text (expand-backslash text))
  (multiple-value-bind (regex pattern) (compile-pattern pattern)
    (format t "Parse tree:~%~:W~2%" (ext::parse-regex pattern))
    (describe regex)
    (let ((match
            (let ((ext::*regex-debug* t))
              (ext:eval-regex regex text)) ))
      (if (not (ext:matched-p match))
          (format t "; Not matched!~%")
        (progn
          (describe match)
          (values regex
                  (and template (expand-dollar match template))
                  (loop for nth below (ext:match-group-count match)
                    collect (ext:match-string match nth) ))) ) ) ) )


;;;; test-split
;
(defun test-split (pattern limit text &optional expect comment)
    (declare (ignore comment))
  (setq pattern (compile-pattern pattern))
  (when (stringp limit) (setq limit (parse-integer limit)))
  (setq text (expand-backslash text))
  (when (stringp expect)
    (setq expect (expand-backslash expect))
    (setq expect (subst nil "nil" (split #\| expect) :test #'equal)) )
  (values (ext:regex-split pattern text limit) expect) )


;;;; backslash
;
(defun backslash (string)
  (with-output-to-string (s)
    (loop
      with state = :normal
      with acc = 0
      with count = 0
      for char across string do
        (ecase state
          ((:normal)
            (if (char= char #\\)
                (setq state :backslash)
              (write-char char s) ) )
          ((:backslash)
            (case char
              ((#\n)
                (write-char #\Newline s)
                (setq state :normal) )
              ((#\t)
                (write-char #\Tab s)
                (setq state :normal) )
              ((#\u)
                (setq acc 0)
                (setq count 0)
                (setq state :unicode) )
              (otherwise
                (write-char #\\ s)
                (write-char char s)
                (setq state :normal) )) )
          ((:unicode)
            (let ((digit (digit-char-p char 16)))
              (unless digit (error "Bad \\u"))
              (setq acc (logior (ash acc 4) digit))
              (incf count)
              (when (eql count 4)
                (write-char (code-char acc) s)
                (setq state :normal) )) ))
      finally
        (unless (eq state :normal)
          (error "Invalid backslash: ~S~%" string) )) ) )


;;;; pcre-regex-test
;
(defun pcre-test (id pattern options string expect)
    (declare (type string pattern))
    (declare (type list options))
    (declare (type string string))
    (declare (type list matches))
  (let* ((form `(pcre-test ,id ,pattern ',options ,string ',expect))
         (regex
            (handler-case (apply #'ext:compile-regex pattern options)
              (ext:regex-parse-error (c)
                (return-from pcre-test (values form c)) ) ) )
         (match
           (handler-case (ext:eval-regex regex string)
             (error (c)
               (return-from pcre-test (values form c)) ) ) )
         (result
           (when (ext:matched-p match)
             (loop
               with elts = '()
               for nth below (ext:match-group-count match) do
                 (push (ext:match-string match nth) elts)
               finally
                 #+nil
                 (loop while (and elts (null (first elts))) do
                   (pop elts) )
                 (return (nreverse elts)) )) ))
    ;; Perl's @- and @+ don't set position for
    ;#+nil
    (when (mismatch expect result :test #'equal)
      (values result expect) )
    #+nil
    (unless  (equal expect result)
      (values result expect) ) ) )


;;;; pcre-regex-test/g
;
(defun pcre-test/g (id pattern options string expect rest)
    (declare (type string pattern))
    (declare (type list options))
    (declare (type string string))
    (declare (type list matches))
  (when (>= (length pattern) 100)
    (return-from pcre-test/g "too long pattern") )

  (let* ((form `(pcre-test/g ,id ,pattern ',options ,string ',expect))
         (regex
            (handler-case (apply #'ext:compile-regex pattern options)
              (ext:regex-parse-error (c)
                (return-from pcre-test/g (values form c)) ) ) )
         (match (ext:eval-regex regex string))
         (result
           (loop while (ext:matched-p match)
             collect (ext:match-string match)
             when (eql (ext:match-start match) (ext:match-end match))
               do (incf (slot-value match 'position))
             do (ext:next-match match) ) ))
    (unless (equal expect result)
      (values result expect) ) ) )


;;;; pcre-test-file
;
(defun pcre-test-file (file &key verbose stop-on-fail log)
  (labels (
    ;; know-failure-p
    (know-failure-p (form)
      (let ((test-case-id (second form)))
        (assoc test-case-id *pcre-failed-cases* :test #'string=) ) )

    ;; main
    (main (file log)
        (declare (type pathname file))
        (declare (type (or stream null) log))
      (with-open-file (in file)
        (loop
          with case-count = 0
          with fail-count = 0
          with ignore-count = 0
          with eof = '(eof)
          for form = (read in nil eof)
          until (eq form eof) do
            (incf case-count)
            (when verbose
              (format t "; ~D ~S~%" case-count form) )

            (multiple-value-bind (result expect) (eval form)
              (when (know-failure-p form)
                (incf ignore-count)
                (setq result nil) )
              (when result
                (when log
                  (when (typep expect 'error)
                    ;; (format log "~@<;; ~@;~:W~:>" error)
                    (pprint-logical-block (log nil :per-line-prefix ";; ")
                      (describe-object expect log) ))
                  (format log "~&~:W~2%" form) )

                (format t ";   FAIL:~%~:W~%" form)
                (format t ";     Result: ~S~%" result)
                (format t ";     Expect: ~S~%" expect)
                (when (typep expect 'error)
                  (describe expect) )
                (incf fail-count)
                (when stop-on-fail (loop-finish)) ) )
          finally
            (return (values fail-count ignore-count case-count)) ) ) )
    )
    ;;
    (setq file
        (merge-pathnames
            file
            (pathname "sys:source;regex;test;*.test") ))
    (if (null log)
        (main file nil)
      (with-open-file (out log :direction :output :if-exists :supersede)
        (main file out) )) ) )


;;;; pcre-test-all
;
(defun pcre-test-all (&optional verbose)
  (loop
    with total-case-count = 0
    with total-fail-count = 0
    with total-ignore-count = 0
    for file in '(
            "regex-test-pcre1"
            "regex-test-pcre2"
            "regex-test-pcre3"
            "regex-test-pcre4"
            "regex-test-pcre5" ) do
      (format t "; Try ~S~%" file)
      (multiple-value-bind (fail-count ignore-count case-count)
          (pcre-test-file file :verbose verbose)
        (format t ";  ~D/~D, skip=~D~%" fail-count case-count ignore-count)
        (incf total-case-count case-count)
        (incf total-ignore-count ignore-count)
        (incf total-fail-count fail-count) )
    finally (return
        (values total-fail-count total-ignore-count total-case-count) )) )


(when (find-package :ext)
  (import 'regex-test-all :ext)
  (import 'perl-test-file :ext)
  (import 'test-match :ext)
  (import 'test-split :ext)

  (defun ext::test ()
    (with-open-file (out
                            "fail.txt"
                            :direction :output
                            :if-exists :supersede )
      (let ((*standard-output*
              (make-broadcast-stream *standard-output* out) ))
        (perl-test-file "sys:source;regex;test;regex-test-perl584.txt") )) )
 ) ; ext


;;;; Failed Test Cases in Perl format.
;
(setq *perl-failed-cases* '(
    ((TEST-MATCH "a(?{})b" "cabd" "y" "$&" "ab")
        "486: (?{code}...) is not supported." )
    ((TEST-MATCH "a(?{\"\\{\"})b" "cabd" "y" "$&" "ab")
        "491: (?{code}...) is not supported." )
    ((TEST-MATCH "a(?{$bl=\"\\{\"}).b" "caxbd" "y" "$bl" "{")
        "493: (?{code}...) is not supported." )
    ((TEST-MATCH "(?(?{0})a|b)" "a" "n" "-" "-")
        "533: (?{code}...) is not supported." )
    ((TEST-MATCH "(?(?{0})b|a)" "a" "y" "$&" "a")
        "534: (?{code}...) is not supported." )
    ((TEST-MATCH "(?(?{1})b|a)" "a" "n" "-" "-")
        "535: (?{code}...) is not supported." )
    ((TEST-MATCH "(?(?{1})a|b)" "a" "y" "$&" "a")
        "536: (?{code}...) is not supported." )
    ((TEST-MATCH "(?{$a=2})a*aa(?{local$a=$a+1})k*c(?{$b=$a})" "yaaxxaaaacd" "y" "$b" "3")
        "556: (?{code}...) is not supported." )
    ((TEST-MATCH "(?{$a=2})(a(?{local$a=$a+1}))*aak*c(?{$b=$a})" "yaaxxaaaacd" "y" "$b" "4")
        "557: (?{code}...) is not supported." )
    ((TEST-MATCH "(?<=x+)y" "-" "c" "-" "Variable length lookbehind not implemented")
        "598: We support variable length lookbehind." )
    ((TEST-MATCH "a(?{$a=2;$b=3;($b)=$a})b" "yabz" "y" "$b" "2")
        "800: (?{code}...) is not supported." )
    ((TEST-MATCH "^(a(??{\"(?!)\"})|(a)(?{1}))b" "ab" "y" "$2" "a" "# [ID 20010811.006]")
        "897: (??{code}...) is not supported." )
    ((TEST-MATCH "(??{})" "x" "y" "-" "-")
        "932: (??{code}...) is not supported." )
 ) ) ; *perl-failed-casses*


;;;; Failed Test Cases in PCRE format.
;
(setq *pcre-failed-cases* '(
    ("testinput1/109"   "\\c{ = ; and \\c: = z are not allowed.")
    ("testinput1/244"   "Generator issue. We can't get all captures in Perl")
    ("testinput1/245"   "Generator issue. We can't get all captures in Perl")
    ("testinput1/246"   "Generator issue. We can't get all captures in Perl")
    ("testinput1/254"   "Generator issue. We can't get all captures in Perl")
    ("testinput1/255"   "Generator issue. We can't get all captures in Perl")
    ("testinput1/256"   "Generator issue. We can't get all captures in Perl")
    ("testinput1/279"   "Generator issue. We can't get all captures in Perl")
    ("testinput1/1445"  "Incomplete {m,n}.")
    ("testinput1/1448"  "Incomplete {m,n}.")
    ("testinput1/1451"  "{m,n} contains non-digit.")
    ("testinput1/1489"  "Invalid backslash notation \\81.")
    ("testinput1/1490"  "Invalid backslash notation \\81.")
    ("testinput1/1493"  "Invalid backslash notation \\91.")
    ("testinput1/1494"  "Invalid backslash notation \\91.")
    ("testinput1/1503"  "Invalid backslash notation \\g.")
    ("testinput1/1509"  "Generator issue. We can't get all captures in Perl")
    ("testinput1/1612"  "REVIEW: We should allow (?).")
    ("testinput1/1675"  "BUGBUG: ^[W-\\]46]")
    ;("testinput1/1882"  "BUGBUG: Don't factor caputre out if it contains alternation")
    ;("testinput1/1883"  "BUGBUG: Don't factor caputre out if it contains alternation")
    ("testinput1/1886"  "BUGBUG: m/.*/g")
    ("testinput1/1936"  "REVIEW: Invalid {n,m}. e.g. {ab}")
    ;("testinput1/1993"  "BUGBUG: Don't factor caputre out if it contains alternation")
    ;("testinput1/2025"  "BUGBUG: Don't factor caputre out if it contains alternation")
    ;("testinput1/2026"  "BUGBUG: Don't factor caputre out if it contains alternation")
    ("testinput1/2125"  "Generator issue. We can't get all captures in Perl")
    ;("testinput1/2335"  "BUGBUG: nested lookahead")
    ("testinput1/2723"  "Generator issue. we can't get all captures in Perl.")
    ("testinput1/2726"  "Generator issue. we can't get all captures in Perl.")
    ("testinput1/2728"  "Generator issue. we can't get all captures in Perl.")
    ("testinput1/2986"  "Generator issue. we can't get all captures in Perl.")

    ;; Generator removes backslash for single quote.
    #+nil ("testinput1/3619"  "Backslash \\'.")
    #+nil ("testinput1/3620"  "Backslash \\'.")
    #+nil ("testinput1/3621"  "Backslash \\'.")

    ("testinput1/3695"  "Backslash \\E.")
    ("testinput1/3713"  "Backslash \\Q without \\E.")
    ("testinput1/3808"  "Backslash \\Q in character class.")
    ("testinput1/3809"  "Backslash \\Q in character class.")
    ("testinput1/3810"  "Backslash \\Q in character class.")
    ("testinput1/3811"  "Backslash \\Q in character class.")
    ("testinput1/3812"  "Backslash \\Q in character class.")
    ("testinput1/3813"  "Backslash \\Q in character class.")
    ("testinput1/3814"  "Backslash \\Q in character class.")
    ("testinput1/3817"  "Backslash \\z and \\C are not supported in character class.")
    ("testinput1/3818"  "Backslash \\z and \\C are not supported in character class.")
    ("testinput1/3821"  "Backslash \\M are not supported.")

    ;; testinput2
    ("testinput2/312"   "REVIEW: Captures in negative lookaround are unbound on succession of negative lookaround.")
    ("testinput2/745"   "REVIEW: Evita makes capture unbound on failure.")
    ("testinput2/748"   "REVIEW: Evita makes capture unbound on failure.")
    ("testinput2/760"   "REVIEW: Evita makes capture unbound on failure.")
    ("testinput2/763"   "REVIEW: Evita makes capture unbound on failure.")
    ("testinput2/766"   "REVIEW: Evita makes capture unbound on failure.")
    ("testinput2/769"   "REVIEW: Evita makes capture unbound on failure.")
    #+nil ("testinput2/1205"  "REVIEW: Factor capture out from repetition into two parts, atomic and capture after repetition.")

    ;; testinput4
    ;("testinput4/68"    "BUGBUG: why?: lazy quantifier")
    ;("testinput4/69"    "BUGBUG: why?: lazy quantifier")
    ;("testinput4/70"    "BUGBUG: why?: lazy quantifier")
    ;("testinput4/89"    "BUGBUG: why?: lazy quantifier")
    ;("testinput4/90"    "BUGBUG: why?: lazy quantifier")
    ;("testinput4/91"    "BUGBUG: why?: lazy quantifier")
    ("testinput4/111"    "REGEX-NOT-SUPPORTED: \x{10000}")
    ("testinput4/131"   "OK: \C is not supported.")
    ("testinput4/134"   "OK: \C is not supported.")
    ("testinput4/137"   "OK: \C is not supported.")
    ("testinput4/140"   "OK: \C is not supported.")
    ("testinput4/143"   "OK: \C is not supported.")
    ("testinput4/144"   "OK: \C is not supported.")
    ("testinput4/145"   "OK: \C is not supported.")
    ("testinput4/146"   "OK: \C is not supported.")
    ("testinput4/147"   "OK: \C is not supported.")
    ("testinput4/150"   "OK: \C is not supported.")
    ("testinput4/151"   "OK: \C is not supported.")
    ("testinput4/152"   "OK: \C is not supported.")
    ("testinput4/153"   "OK: \C is not supported.")

    ("testinput4/242"   "BUGBUG: Unicode \w?")

    ("testinput4/468"   "OK: \C is not supported.")
    ("testinput4/469"   "OK: \C is not supported.")
    ("testinput4/472"   "OK: \C is not supported.")
    ("testinput4/473"   "OK: \C is not supported.")
    ("testinput4/474"   "OK: \C is not supported.")
    ("testinput4/475"   "OK: \C is not supported.")


    ;; testinput5
    ("testinput5/50"    "BUGBUG: unicode?")
    ("testinput5/54"    "BUGBUG: unicode?")

    ("testinput5/81"    "OK: \C is not supported.")
    ("testinput5/88"    "OK: \C is not supported.")
    ("testinput5/89"    "OK: \C is not supported.")

    ;; cl-ppcre
    ("testinput/103"    "\\c{=; and \\c:=z are not allowed.")
    ("testinput/1392"   "Incomplete {m,n}.")
    ("testinput/1395"   "Incomplete {m,n}.")
    ("testinput/1398"   "{m,n} contains non-digit.")
    ("testinput/1434"   "Invalid backslash notation \\81.")
    ("testinput/1435"   "Invalid backslash notation \\81.")
    ("testinput/1438"   "Invalid backslash notation \\91.")
    ("testinput/1439"   "Invalid backslash notation \\91.")
    ("testinput/1448"   "Invalid backslash notation \\g.")
    ("testinput/1551"   "REVIEW: We should allow (?).")
    ("testinput/1811"   "BUGBUG: m/.*/g")
    ("testinput/1860"   "REVIEW: Invalid {n,m}. e.g. {ab}")
    ("testinput/1930"   "REVIEW: Incomlete (?<)")
    ("testinput/1942"   "Backslash in cc: [\\x]")
    ("testinput/1943"   "Backslash in cc: [\\x]")
    ("testinput/1970"   "Incomplete backslash \\x")
    ("testinput/1971"   "Incomplete backslash \\x")
    ("testinput/1972"   "Incomplete backslash \\x")
    ("testinput/2025"   "Invalid range [---]")
    ("testinput/2026"   "Invalid range [---]")
    ("testinput/2037"   "Incomplete brace a{")
    ("testinput/2040"   "No m,n brace a{}")
    ("testinput/2043"   "Incomplete brace a{3")
    ("testinput/2046"   "Incomplete brace a{3, ")
    ("testinput/2049"   "Incomplete brace a{3, ")
    ("testinput/2050"   "Incomplete brace a{3, ")
    ("testinput/2051"   "Incomplete brace a{3, ")
    ("testinput/2059"   "Incomplete brace a{3, ")
    ("testinput/2060"   "Incomplete brace a{3, ")
    ("testinput/2061"   "Incomplete brace a{3, ")
    ("testinput/2069"   "Invalid \\x")
    ("testinput/2070"   "Invalid \\x")
    ("testinput/2073"   "Invalid \\x")
    ("testinput/2074"   "Invalid \\x")
    ("testinput/2077"   "Invalid \\x")
    ("testinput/2078"   "Invalid \\x")
    ("testinput/2079"   "Invalid \\x")
    ("testinput/2080"   "Invalid \\x")
    ("testinput/2083"   "REVIEW: \\x 3 w/ x")
    ("testinput/2084"   "REVIEW: \\x 3 w/ x")
    ("testinput/2085"   "REVIEW: \\x 3 w/ x")
    ("testinput/2086"   "REVIEW: \\x 3 w/ x")
    ("testinput/2089"   "Incomplete brace a{ ")
    ("testinput/2090"   "Incomplete brace a{ ")
    ("testinput/2091"   "Incomplete brace a{ ")
    ("testinput/2094"   "Incomplete brace a{ ")
    ("testinput/2095"   "Incomplete brace a{ ")
    ("testinput/2096"   "Incomplete brace a{ ")
    ("testinput/2115"   "Incomplete (?<)")
    ("testinput/2126"   "(?<=a) isn't repetable.")
    ("testinput/2127"   "(?<=a) isn't repetable.")
    ("testinput/2128"   "(?<=a) isn't repetable.")
    ("testinput/2131"   "(?<!a) isn't repetable.")
    ("testinput/2132"   "(?<!a) isn't repetable.")
    ("testinput/2133"   "(?<!a) isn't repetable.")
    ("testinput/2145"   "(?<=a) isn't repetable.")
    ("testinput/2146"   "(?<=a) isn't repetable.")
    ("testinput/2147"   "(?<=a) isn't repetable.")
    ("testinput/2150"   "(?<!a) isn't repetable.")
    ("testinput/2151"   "(?<!a) isn't repetable.")
    ("testinput/2152"   "(?<!a) isn't repetable.")
    ("testinput/2165"   "(?<=a) isn't repetable.")
    ("testinput/2166"   "(?<=a) isn't repetable.")
    ("testinput/2167"   "(?<=a) isn't repetable.")
    ("testinput/2170"   "(?<=a) isn't repetable.")
    ("testinput/2173"   "((?<=a)) isn't repetable.")
    ("testinput/2173"   "(?=a) isn't repetable.")
    ("testinput/2176"   "((?<=a)) isn't repetable.")
    ("testinput/3895"   "Standalone \\E")
    ("testinput/3896"   "Standalone \\E")
    ("testinput/3899"   "\\E in cc")
    ("testinput/3900"   "\\E in cc")
    ("testinput/3911"   "\\Q w/ \\E")
    ("testinput/3912"   "\\Q w/ \\E")
    ("testinput/3915"   "REVIEW: Nested \\Q...\\E")
    ("testinput/3916"   "REVIEW: Nested \\Q...\\E")
) )
