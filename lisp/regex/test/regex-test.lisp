;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: EXTENSION; Base: 10 -*-
;;;;
;;;; regex - execute
;;; lisp/regex/test/regx-test.lisp
;;;
;;; This file is NOT part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: /proj/evcl/lisp/regex/test/regex-test.lisp 35 2006-07-13 15:01:00 yosi $
;;;
;;; Description:
;;;  This file contains test harness for regex facility.
;
(in-package :cl-user)

;;;; *all-test-case-files*
;;;
;;; Description:
;;;  Contains list of test case filenames.
;
(defparameter *all-test-case-files* '(
    "evita.retest"          ; sanity testing
    "perl-evita.retest"
    "perl-504.retest"
    "perl-584.retest"
    "onig.retest"           ; has Unicode test cases
    "pcre-5.retest"
    "pcre-4.retest"
    "pcre-3.retest"
    "pcre-2.retest"
    "pcre-1.retest"         ; heavy
    "pcre-cl.retest"        ; heavy
 ) )


;;;; *options*
;;;
;;; Description:
;;;  Default options for si::compile-regex.
;
(defvar *options* nil)


;;;; *test-file*
;;;
;;; Description:
;;;  Bound by regex-test-file to file being tested.
;
(defvar *test-file* nil)


;;;; *failed-cases*
;;;
;;; Description:
;;;  Contains alist of test-case-id and reason for PCRE format test cases.
;
(defvar *failed-cases*)


;;;; backslash
;;;
;;; Description:
;;;  Expands "\n" and "\uXXXX" backslash notations in string and returns
;;;  new string even if string doesn't contains backslash.
;;;
;;;  Other backslash notations are left unchanged.
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


;;;; regex-test-all
;
(defun regex-test-all (&rest args)
  (loop
    with total-case-count = 0
    with total-fail-count = 0
    with total-ignore-count = 0
    for file in *all-test-case-files* do
      (format t "; Try ~S~%" file)
      (force-output)
      (multiple-value-bind (fail-count ignore-count case-count)
          (apply #'regex-test-file file args)
        (format t ";  ~D/~D, skip=~D~%" fail-count case-count ignore-count)
        (force-output)
        (incf total-case-count case-count)
        (incf total-ignore-count ignore-count)
        (incf total-fail-count fail-count) )
    finally (return
        (values total-fail-count total-ignore-count total-case-count) )) )


;;;; regex-test-file
;
(defun regex-test-file (file &key verbose stop-on-fail log)
    (declare (values fixnum fixnum fixnum))
  (let ((case-count   0)
        (fail-count   0)
        (ignore-count 0) )
  (labels (
    ;; know-failure-p
    (know-failure-p (form)
      (let ((id (second form)))
        (stringp (second (assoc id *failed-cases* :test #'string=))) ) )

    ;; main
    (main (file)
        (declare (type pathname file))
        (declare (type (or stream null) log))
      (let ((*test-file* file))
        (main-aux file) ) )

    ;; main-aux
    (main-aux (file)
        (declare (type pathname file))
        (declare (type (or stream null) log))
      (with-open-file (in file)
        (loop
          with eof = '(eof)
          for form = (read in nil eof)
          until (eq form eof) do
            (incf case-count)
            (when verbose
              (format t "; ~D ~:W~%" case-count form) )

            (multiple-value-bind (response result expect) (eval form)
              ;; Handles perl="yB" and perl-not-supported.
              (when response
                (let* ((id (second form))
                       (id.expect
                         (assoc id *failed-cases* :test #'string=) )
                       (expect (second id.expect)) )
                  (when (and id.expect (listp expect))
                    (when (equal result expect)
                      (setq response nil) )) ))
              (cond
                ((null response)
                  #+nil succeeded )
                ((know-failure-p form)
                  (incf ignore-count) )
                (t
                  (incf fail-count)
                  (report-failure form result expect)
                  (when stop-on-fail (loop-finish)) )) )) ) )

    ;; report-failure
    (report-failure (form result expect)
      (when log
        (when (typep expect 'error)
          ;; (format log "~@<;; ~@;~:W~:>" error)
          (pprint-logical-block (log nil :per-line-prefix ";; ")
            (describe-object expect log) ))
        (format log "~&; ~D: Expect=~S Result=~S~%" fail-count expect result)
        (format log "~:W~2%" form) )

      (format t ";   FAIL:~%~:W~%" form)
      (format t ";     Result: ~S~%" result)
      (format t ";     Expect: ~S~%" expect)
      (when (typep expect 'error)
        (describe expect) ) )
    )
    ;;
    (setq file
        (merge-pathnames
            file
            (pathname "sys:source;regex;test;*.retest") ))
    (typecase log
      (null   (main file))
      (stream (main file))
      (otherwise
        (setq log (pathname log))
        (setq log (merge-pathnames log
                    (make-pathname :host (pathname-host log) :type "lisp") ))
        (with-open-file (out log :direction :output :if-exists :supersede)
          (setq log out)
          (main file) ) ))

    (values fail-count ignore-count case-count) ) ) )


;;;; test-case
;
(defun test-case (id pattern string options expect)
    (declare (type string pattern))
    (declare (type string string))
    (declare (type list options))
    (declare (type (or list string (eql :compile)) expect))
    (declare (values t t t))
    (declare (ignore id))
  (labels (
    ;; match-p
    ;;  Shorter patterns matches subjects, since Perl's @- and @+ don't
    ;;  contains captures that not executed.
    (match-p (patterns subjects)
      (dolist (subject subjects t)
        (when (null patterns) (return t))
        (let ((pat (pop patterns)))
          (cond
            ((eq pat '*))
            ((equal pat subject))
            (t (return nil)) ) ) ) )
    )
    ;;
    (when (stringp expect) (setq expect (list expect)))
    (setq options (append options *options*))
    (let* ((regex
              (handler-case (apply #'si::compile-regex pattern options)
                (si::regex-parse-error (c)
                  (if (eq expect :compile)
                      (return-from test-case (values nil nil))
                    (return-from test-case (values :compile c)) ) ) ) )
           (match
             (if (null *test-file*)
                 (si::eval-regex regex string)
               (handler-case (si::eval-regex regex string)
                 (error (c)
                   (return-from test-case (values :execute c)) ) )) )
           (result
             (when (si::matched-p match)
               (loop
                 with elts = '()
                 for nth below (si::match-group-count match) do
                   (push (si::match-string match nth) elts)
                 finally
                   (return (nreverse elts)) )) ))
      (cond
        ((null expect)
          (values (and result :match) result expect) )
        ((consp expect)
          (if (and (consp result) (match-p expect result))
              (values nil nil nil)
            (values :match result expect) ) )
        ((eq expect :compile)
          (values :not-perl result expect) )
        (t
          (error "CAN'T HAPPEN!") )) ) ) )

;;;; test-case/g
;
(defun test-case/g (id pattern string options expect &optional rest)
    (declare (type string pattern))
    (declare (type list options))
    (declare (type string string))
    (declare (type list matches))
    (declare (ignore rest))
  (setq options (append options *options*))
  (let* ((form `(test-case/g ,id ,pattern ,string ',options ',expect))
         (regex
            (handler-case (apply #'si::compile-regex pattern options)
              (si::regex-parse-error (c)
                (return-from test-case/g (values :compile form c)) ) ) )
         (match (si::eval-regex regex string))
         (result
           (si::with-collector (collect)
             (si::do-match (match match)
               (collect (si::match-string match)) ) ) ))
    (unless (equal expect result)
      (values :match result expect) ) ) )


;;;; test-case/r
;;;
;;; Syntax:
;;;     (test-case/r id pattern string end expect) =>
;;;         failed-reason match-start expect
;;;
;;; Description:
;;;  Test case for backward scanning regex. In Ruby, this is "rindex test".
;
(defun test-case/r (id pattern string end expect)
    (declare (ignore id))
  (let* ((options (list* :from-end t :unicode t *options*))
         (regex
           (handler-case (apply #'si::compile-regex pattern options)
             (si::regex-parse-error (c)
               (return-from test-case/r (values :compile c)) ) ) )
         (match
            (handler-case (si::eval-regex regex string :end end)
              (error (c)
                (return-from test-case/r (values :execute c)) )) ))
      (unless (eql (si::match-start match) expect)
        (values :match (si::match-start match) expect) ) ) )


;;;; test-comp
;;;
;;; Description:
;;;  Test driver of regex compiler.
;
(defun test-comp (source &rest options)
  (loop while (stringp (first options)) do (pop options))
  (setq options (append options *options*))
  (let ((si::*regex-debug* t))
    (apply #'si::compile-regex source options) ) )


;;;; test-match
;
(defun test-match (pattern text &rest options)
  (labels (
    ;; show-match
    (show-match (match)
      (if (>= (length text) 50)
          (describe match)
        (progn
          (format t "   ~A~%" text)
          (loop
            for nth below (si::match-group-count match)
            for start = (si::match-start match nth)
            for end   = (si::match-end   match nth) do
              (format t "~2D " nth)
              (if (null start)
                  (format t " unbound~%")
                (progn
                  (format t "~VT^" (+ start 3))
                  (loop repeat (- end start 1) do (write-char #\^))
                  (format t "  [~D,~D]~%" start end) ))))) )
    )
    ;;
    ;; test-match
    ;;
    (setq text (backslash text))
    (format t "Parse tree:~%~:W~2%"
      (apply #'si::parse-regex pattern options) )

    (setq options (append options *options*))

    (multiple-value-bind (regex)
        (apply #'si::compile-regex pattern options)
      (describe regex)
      (let* ((match
               (let ((si::*regex-debug* t))
                 (si::eval-regex regex text) ) )
             (results
               (when (si::matched-p match)
                  (loop for nth below (si::match-group-count match)
                    collect (si::match-string match nth) )) ))
        (if (null results)
            (format t "; Not matched!~%")
          (show-match match) )
        (values match regex results) ) ) ) )



;;;; Failed Test Cases in PCRE format.
;
(setq *failed-cases* '(
    ;; perl-evita

    ;; Perl should fail on compilation, since this regex contains variable
    ;; lengthed lookbehind.
    ("perl-evita/120" ("Foo" "A ")) ; from .NET 

    ;; perl-584
    ;; @- = @LAST_MATCH_START 
    ("perl-584/486" "(?{code}...) is not supported.")
    ("perl-584/491" "(?{code}...) is not supported.")
    ("perl-584/493" "(?{code}...) is not supported.")
    ("perl-584/516" "Non-existing capture in condition: (?(nth)...)")
    ("perl-584/517" "Non-existing capture in condition: (?(nth)...)")
    ("perl-584/533" "(?{code}...) is not supported.")
    ("perl-584/534" "(?{code}...) is not supported.")
    ("perl-584/535" "(?{code}...) is not supported.")
    ("perl-584/536" "(?{code}...) is not supported.")
    ("perl-584/556" "(?{code}...) is not supported.")
    ("perl-584/557" "(?{code}...) is not supported.")
    ("perl-584/598" "We support variable length lookbehind.")
    ("perl-584/629" "Test case generator failuer.")
    ("perl-584/647" "Test case generator failuer.")
    ("perl-584/701" "Test case generator failuer.")
    ("perl-584/755" "Test case generator failuer.")
    ("perl-584/943" "Perl doesn't support non-fixed lookahead")
    ("perl-584/800" "(?{code}...) is not supported.")
    ("perl-584/860" ("aba" "a" "b"))            ; from .NET
    ("perl-584/861" ("aabbaa" "aa" "bb"))       ; from .NET
    ("perl-584/879" "(??{code}...) is not supported.")
    ("perl-584/891" ("abc" "ab"))     ; perl=yB, from .NET
    ("perl-584/897" ("abc" "ab"))     ; perl=yB, from .NET
    ("perl-584/908" ("abc" "ab"))     ; perl=yB, from .NET
    ("perl-584/914" ("abc" "ab"))     ; perl=yB, from .NET
    ("perl-584/924" "(??{code}...) is not supported.")

    ;; pcre-1
    ("pcre-1/109"   "\\c{ = ; and \\c: = z are not allowed.")
    ("pcre-1/244"   "Generator issue. We can't get all captures in Perl")
    ("pcre-1/245"   "Generator issue. We can't get all captures in Perl")
    ("pcre-1/246"   "Generator issue. We can't get all captures in Perl")
    ("pcre-1/254"   "Generator issue. We can't get all captures in Perl")
    ("pcre-1/255"   "Generator issue. We can't get all captures in Perl")
    ("pcre-1/256"   "Generator issue. We can't get all captures in Perl")
    ("pcre-1/279"   "Generator issue. We can't get all captures in Perl")
    ("pcre-1/1445"  "Incomplete {m,n}.")
    ("pcre-1/1448"  "Incomplete {m,n}.")
    ("pcre-1/1451"  "{m,n} contains non-digit.")
    ("pcre-1/1489"  "Invalid backslash notation \\81.")
    ("pcre-1/1490"  "Invalid backslash notation \\81.")
    ("pcre-1/1493"  "Invalid backslash notation \\91.")
    ("pcre-1/1494"  "Invalid backslash notation \\91.")
    ("pcre-1/1503"  "Invalid backslash notation \\g.")
    ("pcre-1/1509"  "Generator issue. We can't get all captures in Perl")
    ("pcre-1/1612"  "REVIEW: We should allow (?).")
    ("pcre-1/1675"  "BUGBUG: ^[W-\\]46]")
    ;("pcre-1/1882"  "BUGBUG: Don't factor caputre out if it contains alternation")
    ;("pcre-1/1883"  "BUGBUG: Don't factor caputre out if it contains alternation")
    ("pcre-1/1886"  "BUGBUG: m/.*/g")

    ; pcre-1/1898 enlagres cstack to 6400 when compiled to lisp.
    
    ("pcre-1/1936"  "REVIEW: Invalid {n,m}. e.g. {ab}")
    ;("pcre-1/1993"  "BUGBUG: Don't factor caputre out if it contains alternation")
    ;("pcre-1/2025"  "BUGBUG: Don't factor caputre out if it contains alternation")
    ;("pcre-1/2026"  "BUGBUG: Don't factor caputre out if it contains alternation")
    ("pcre-1/2125"  "Generator issue. We can't get all captures in Perl")
    ;("pcre-1/2335"  "BUGBUG: nested lookahead")
    ("pcre-1/2723"  "Generator issue. we can't get all captures in Perl.")
    ("pcre-1/2726"  "Generator issue. we can't get all captures in Perl.")
    ("pcre-1/2728"  "Generator issue. we can't get all captures in Perl.")
    ("pcre-1/2986"  "Generator issue. we can't get all captures in Perl.")
    ("pcre-1/3427"  "Non-existing capture in condition: (?(nth)...)")

    ;; Generator removes backslash for single quote.
    #+nil ("pcre-1/3619"  "Backslash \\'.")
    #+nil ("pcre-1/3620"  "Backslash \\'.")
    #+nil ("pcre-1/3621"  "Backslash \\'.")

    ("pcre-1/3695"  "Backslash \\E.")
    ("pcre-1/3713"  "Backslash \\Q without \\E.")
    ("pcre-1/3808"  "Backslash \\Q in character class.")
    ("pcre-1/3809"  "Backslash \\Q in character class.")
    ("pcre-1/3810"  "Backslash \\Q in character class.")
    ("pcre-1/3811"  "Backslash \\Q in character class.")
    ("pcre-1/3812"  "Backslash \\Q in character class.")
    ("pcre-1/3813"  "Backslash \\Q in character class.")
    ("pcre-1/3814"  "Backslash \\Q in character class.")
    ("pcre-1/3817"  "Backslash \\z and \\C are not supported in character class.")
    ("pcre-1/3818"  "Backslash \\z and \\C are not supported in character class.")
    ("pcre-1/3821"  "Backslash \\M are not supported.")

    ;; pcre-2

    ;; lookbehind: multiple fixed-length pattern
    ("pcre-2/189"   ("foo"))
    ("pcre-2/190"   ("foo"))
    ("pcre-2/191"   nil)
    ("pcre-2/192"   nil)
    ("pcre-2/193"   nil)

    ;; lookbehind: multiple fixed-length pattern
    ("pcre-2/285"   ("-cart"))
    ("pcre-2/286"   ("-cart"))
    ("pcre-2/287"   nil)
    ("pcre-2/288"   nil)
    ("pcre-2/289"   nil)

    ;; lookbehind: multiple fixed-length pattern
    ("pcre-2/294"   ("alphabetabcd" "abcd" nil)) ; PCRE doen't set $2.
    ("pcre-2/295"   ("endingxyz" nil "xyz"))
    ("pcre-2/298"   ("ZZ"))
    ("pcre-2/299"   ("ZZ"))
    ("pcre-2/300"   ("ZZ"))
    ("pcre-2/301"   ("ZZ"))
    ("pcre-2/302"   ("ZZ"))
    ("pcre-2/303"   ("ZZ"))
    ("pcre-2/304"   nil)
    ("pcre-2/305"   nil)
    ("pcre-2/306"   nil)
    ("pcre-2/307"   nil)
    ("pcre-2/308"   nil)

    ;; REVIEW: Captures in negative lookaround are unbound on succession of
    ;; negative lookaround. However, Perl keeps it.
    ("pcre-2/312"  ("bar" nil))

    ;; Capture in loop. Perl makes unbound. .NET pops off
    ("pcre-2/725" ("mainOmain" "main" "O"))
    ("pcre-2/730" ("aba" "a" "b"))
    ("pcre-2/733" ("aabbaa" "aa" "bb"))
    ("pcre-2/739" ("aabbaa" "aa" "bb"))
    ("pcre-2/742" ("aabbaa" "bb"))
    ("pcre-2/751" ("aabbaa" "bb"))
    ("pcre-2/754" ("aabbbaa" "bbb"))
    ("pcre-2/757" ("aabbbaa" "bbb"))

    ;; PCRE extension
    ("pcre-2/616"   "PCRE: (?R) construct")
    ("pcre-2/617"   "PCRE: (?R) construct")

    ("pcre-2/745"   "REVIEW: Evita makes capture unbound on failure.")
    ("pcre-2/748"   "REVIEW: Evita makes capture unbound on failure.")
    ("pcre-2/760"   "REVIEW: Evita makes capture unbound on failure.")
    ("pcre-2/763"   "REVIEW: Evita makes capture unbound on failure.")
    ("pcre-2/766"   "REVIEW: Evita makes capture unbound on failure.")
    ("pcre-2/769"   "REVIEW: Evita makes capture unbound on failure.")

    ;; Possessive quantifier
    ("pcre-2/820"   ("F"))
    ("pcre-2/821"   nil)

    ("pcre-2/832"   ("now is the time for all good men to come to the aid of the party" "party"))
    ("pcre-2/833"   nil)
    ("pcre-2/834"   nil)

    ("pcre-2/837"   ("12345a" "12345" "a"))
    ("pcre-2/838"   nil)
    ("pcre-2/839"   nil)

    ("pcre-2/842"   ("aaab"))
    ("pcre-2/845"   ("aaab" "aaab"))
    ("pcre-2/848"   ("aaab" "aaa"))

    ("pcre-2/851"   ("abc(ade)ufh()()x" "x"))
    ("pcre-2/854"   ("(abc)" "abc"))
    ("pcre-2/855"   ("(abc(def)xyz)" "xyz"))
    ("pcre-2/856"   nil)
    ("pcre-2/857"   nil)


    #+nil ("pcre-2/1205"  "REVIEW: Factor capture out from repetition into two parts, atomic and capture after repetition.")

    ;; pcre-4
    ("pcre-4/111"    "REGEX-NOT-SUPPORTED: \x{10000}")
    ("pcre-4/131"   "OK: \C is not supported.")
    ("pcre-4/134"   "OK: \C is not supported.")
    ("pcre-4/137"   "OK: \C is not supported.")
    ("pcre-4/140"   "OK: \C is not supported.")
    ("pcre-4/143"   "OK: \C is not supported.")
    ("pcre-4/144"   "OK: \C is not supported.")
    ("pcre-4/145"   "OK: \C is not supported.")
    ("pcre-4/146"   "OK: \C is not supported.")
    ("pcre-4/147"   "OK: \C is not supported.")
    ("pcre-4/150"   "OK: \C is not supported.")
    ("pcre-4/151"   "OK: \C is not supported.")
    ("pcre-4/152"   "OK: \C is not supported.")
    ("pcre-4/153"   "OK: \C is not supported.")

    ("pcre-4/242"   "BUGBUG: Unicode \w?")

    ("pcre-4/468"   "OK: \C is not supported.")
    ("pcre-4/469"   "OK: \C is not supported.")
    ("pcre-4/472"   "OK: \C is not supported.")
    ("pcre-4/473"   "OK: \C is not supported.")
    ("pcre-4/474"   "OK: \C is not supported.")
    ("pcre-4/475"   "OK: \C is not supported.")


    ;; pcre-5
    ("pcre-5/50"    "BUGBUG: unicode?")
    ("pcre-5/54"    "BUGBUG: unicode?")

    ("pcre-5/81"    "OK: \C is not supported.")
    ("pcre-5/88"    "OK: \C is not supported.")
    ("pcre-5/89"    "OK: \C is not supported.")

    ("pcre-5/182"   "\\Q in [...]")
    ("pcre-5/183"   "\\Q in [...]")

    ;; ppcre-cl
    ("pcre-cl/103"    "\\c{=; and \\c:=z are not allowed.")
    ("pcre-cl/1392"   "Incomplete {m,n}.")
    ("pcre-cl/1395"   "Incomplete {m,n}.")
    ("pcre-cl/1398"   "{m,n} contains non-digit.")
    ("pcre-cl/1434"   "Invalid backslash notation \\81.")
    ("pcre-cl/1435"   "Invalid backslash notation \\81.")
    ("pcre-cl/1438"   "Invalid backslash notation \\91.")
    ("pcre-cl/1439"   "Invalid backslash notation \\91.")
    ("pcre-cl/1448"   "Invalid backslash notation \\g.")
    ("pcre-cl/1551"   "REVIEW: We should allow (?).")
    ("pcre-cl/1811"   "BUGBUG: m/.*/g")
    ("pcre-cl/1860"   "REVIEW: Invalid {n,m}. e.g. {ab}")
    ; pcre-cl/1823 enlagres cstack to 6400 when compiled to lisp. (see also
    ; pcre-1/1898).
    ("pcre-cl/1930"   "REVIEW: Incomlete (?<)")
    ("pcre-cl/1942"   "Backslash in cc: [\\x]")
    ("pcre-cl/1943"   "Backslash in cc: [\\x]")
    ("pcre-cl/1946"   "Capture")
    ("pcre-cl/1970"   "Incomplete backslash \\x")
    ("pcre-cl/1971"   "Incomplete backslash \\x")
    ("pcre-cl/1972"   "Incomplete backslash \\x")
    ("pcre-cl/2025"   "Invalid range [---]")
    ("pcre-cl/2026"   "Invalid range [---]")
    ("pcre-cl/2037"   "Incomplete brace a{")
    ("pcre-cl/2040"   "No m,n brace a{}")
    ("pcre-cl/2043"   "Incomplete brace a{3")
    ("pcre-cl/2046"   "Incomplete brace a{3, ")
    ("pcre-cl/2049"   "Incomplete brace a{3, ")
    ("pcre-cl/2050"   "Incomplete brace a{3, ")
    ("pcre-cl/2051"   "Incomplete brace a{3, ")
    ("pcre-cl/2059"   "Incomplete brace a{3, ")
    ("pcre-cl/2060"   "Incomplete brace a{3, ")
    ("pcre-cl/2061"   "Incomplete brace a{3, ")
    ("pcre-cl/2069"   "Invalid \\x")
    ("pcre-cl/2070"   "Invalid \\x")
    ("pcre-cl/2073"   "Invalid \\x")
    ("pcre-cl/2074"   "Invalid \\x")
    ("pcre-cl/2077"   "Invalid \\x")
    ("pcre-cl/2078"   "Invalid \\x")
    ("pcre-cl/2079"   "Invalid \\x")
    ("pcre-cl/2080"   "Invalid \\x")
    ("pcre-cl/2083"   "REVIEW: \\x 3 w/ x")
    ("pcre-cl/2084"   "REVIEW: \\x 3 w/ x")
    ("pcre-cl/2085"   "REVIEW: \\x 3 w/ x")
    ("pcre-cl/2086"   "REVIEW: \\x 3 w/ x")
    ("pcre-cl/2089"   "Incomplete brace a{ ")
    ("pcre-cl/2090"   "Incomplete brace a{ ")
    ("pcre-cl/2091"   "Incomplete brace a{ ")
    ("pcre-cl/2094"   "Incomplete brace a{ ")
    ("pcre-cl/2095"   "Incomplete brace a{ ")
    ("pcre-cl/2096"   "Incomplete brace a{ ")
    ("pcre-cl/2103"   "BUGBUG: We allow {1}")
    ("pcre-cl/2115"   "Incomplete (?<)")
    ("pcre-cl/2126"   "(?<=a) isn't repetable.")
    ("pcre-cl/2127"   "(?<=a) isn't repetable.")
    ("pcre-cl/2128"   "(?<=a) isn't repetable.")
    ("pcre-cl/2131"   "(?<!a) isn't repetable.")
    ("pcre-cl/2132"   "(?<!a) isn't repetable.")
    ("pcre-cl/2133"   "(?<!a) isn't repetable.")
    ("pcre-cl/2145"   "(?<=a) isn't repetable.")
    ("pcre-cl/2146"   "(?<=a) isn't repetable.")
    ("pcre-cl/2147"   "(?<=a) isn't repetable.")
    ("pcre-cl/2150"   "(?<!a) isn't repetable.")
    ("pcre-cl/2151"   "(?<!a) isn't repetable.")
    ("pcre-cl/2152"   "(?<!a) isn't repetable.")
    ("pcre-cl/2165"   "(?<=a) isn't repetable.")
    ("pcre-cl/2166"   "(?<=a) isn't repetable.")
    ("pcre-cl/2167"   "(?<=a) isn't repetable.")
    ("pcre-cl/2170"   "(?<=a) isn't repetable.")
    ("pcre-cl/2173"   "((?<=a)) isn't repetable.")
    ("pcre-cl/2173"   "(?=a) isn't repetable.")
    ("pcre-cl/2176"   "((?<=a)) isn't repetable.")
    ("pcre-cl/3608"   "Unbound group reference in codition")
    ("pcre-cl/3895"   "Standalone \\E")
    ("pcre-cl/3896"   "Standalone \\E")
    ("pcre-cl/3899"   "\\E in cc")
    ("pcre-cl/3900"   "\\E in cc")
    ("pcre-cl/3911"   "\\Q w/ \\E")
    ("pcre-cl/3912"   "\\Q w/ \\E")
    ("pcre-cl/3915"   "REVIEW: Nested \\Q...\\E")
    ("pcre-cl/3916"   "REVIEW: Nested \\Q...\\E")

    ;; Onigruma
    ("onig/86"      "Meta character \\M-c")
    ("onig/89"      "Invalid backslash \\#")
    ("onig/122"     "Control character \\C-c")
    ("onig/159"     "Invalid backslash [\\^]")
    ("onig/205"     "POSIX bracket expression")
    ("onig/206"     "POSIX bracket expression")
    ("onig/344"     "Nested quantifier")
    ("onig/382"     "Omission of min in {min,max}")
    ("onig/472"     "Pattern call \\g<1>")
    ("onig/478"     "Pattern call \\g<name_2>")
    ("onig/480"     "Pattern call \\g<ab>")
    ("onig/481"     "Pattern call \\g<n>")
    ("onig/482"     "Pattern call \\g<n>")
    ("onig/483"     "Pattern call \\g<n>")
    ("onig/484"     "Pattern call \\g<n>")
    ("onig/485"     "Pattern call \\g<n>")
    ("onig/486"     "Pattern call \\g<n>")
    ("onig/488"     "Pattern call \\g<_9>")
    ("onig/489"     "Named capture: ?<_>: We don't allow punctuation")
    ("onig/497"     "Pattern call \\g<foo>")
    ("onig/498"     "Pattern call \\g<foo>")
    ("onig/499"     "Pattern call \\g<foo>")
    ("onig/500"     "Pattern call \\g<bar>")
    ("onig/501"     "Pattern call \\g<1>")
    ("onig/502"     "Pattern call \\g<_A>")
    ("onig/503"     "Pattern call \\g<pon>")
    ("onig/504"     "Pattern call \\g<m>")
    ("onig/505"     "Pattern call \\g<n>")
    ("onig/506"     "Pattern call \\g<n>")
    ("onig/521"     "Backward search semantic difference")
    ("onig/523"     "Backward search semantic difference")
    ("onig/532"     "Forward reference (ffy)\\1")
    ("onig/536"     "Forward reference (((.a)))\\3")
    ("onig/537"     "Forward reference (ac*?z)\\1")
    ("onig/727"     "Omission of min in {min,max}")
    ("onig/797"     "Pattern call \\g<name>")
    ("onig/798"     "Pattern call \\g<name>")
    ("onig/816"     "POSIX bracket expression")
    ("onig/829"     "Backward search semantic difference")
    ("onig/830"     "Backward search semantic difference")
    ("onig/832"     "Backward search semantic difference")
    ("onig/840"     "Forward reference xxx\\1")
    ("onig/844"     "Forward reference xxx\\3")
    ("onig/845"     "Forward reference xxx\\1")
    ("onig/866"     "UTF-8 test. Expected result is one UTF-8 character")
    ("onig/884"     "UTF-8 test. Expected result is one UTF-8 character")
    ("onig/887"     "UTF-8 test. Expected result is one UTF-8 character")
    ("onig/890"     "UTF-8 test. Expected result is one UTF-8 character")
) )


(when (interactive-stream-p *standard-input*)
  (format t "
; Test Harness For Regex Facility.
;
; Try followings:
;   (regex-test-all)        Does all test cases.
;   (regex-test-file case-name :log log-file :verbose)
;   (test-match regex string)
;
" ) )
