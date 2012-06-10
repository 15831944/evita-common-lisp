;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: EXTENSION; Base: 10 -*-
;;;;
;;;; regex - study
;;; lisp/regex/regx-study.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-study.lisp#3 $
;;;
;;; Description:
;;;  This file contains study function for regex parse tree.
;
(in-package :si)

;;; BUGBUG: For computing minimum length of whole regex, we should consider
;;; positive lookahead and lookbehind.For example,
;;;     /foo(?=bar)/    => 6 instead of 3
;;;     /(?<=foo)bar/   => 6 instead of 3

;;;; regex-study-length
;;;
;;; Syntax:
;;;     regex-study-length expr
;;;       => min-length, max-length, capture-p, deterministic-p
;;;
;;; Description:
;;;  Computes length of matched string of specified expression.
;;;  If deterministic-p is true, min-length and max-length must be equal.
;;;
(defun regex-study-length (expr)
    (declare (values fixnum fixnum t t))
    (declare (type regex-expr expr))
  (labels (
    ;; max+
    (max+ (max-1 max-2)
        (declare (type fixnum min-1 max-1))
        (declare (values fixnum))
      (cond
        ((eql max-1 regex-infinity) max-1)
        ((eql max-2 regex-infinity) max-2)
        (t (+ max-1 max-2)) ) )

    ;; max*
    (max* (max-1 max-2)
        (declare (type fixnum min-1 max-1))
        (declare (values fixnum))
      (cond
        ((eql max-1 0) 0)
        ((eql max-2 0) 0)
        ((eql max-1 regex-infinity) max-1)
        ((eql max-2 regex-infinity) max-2)
        (t (* max-1 max-2)) ) )

    ;; study
    (study (expr)
      (etypecase expr
        (character (values 1 1 nil t))
        (string    (values (length expr) (length expr) nil t))
        (fixnum    (values 1 1 nil t))
        (symbol
          (case expr
            ((:any :space :word) (values 1 1 nil t))
            (otherwise (values 0 0 nil t)) ) )
        (cons
          (ecase (first expr)
            ((=) (values 0 regex-infinity nil nil))
            ((and) ; subexpr+
              (pop expr)

              (multiple-value-bind (min max capture-p fixed-p)
                  (study (first expr))
                (dolist (subexpr (rest expr))
                  (multiple-value-bind (submin submax subcapture-p subfixed-p)
                      (study subexpr)
                    (when subcapture-p (setq capture-p t))
                    (unless subfixed-p (setq fixed-p nil))

                    (incf min submin)
                    (setq max (max+ max submax)) ) )

                (values min max capture-p fixed-p) ) )

            ((boundp) (values 0 0 nil nil))
            ((atom)   (study (second expr)))
            ((capture)
              (multiple-value-bind (min max capture-p fixed-p)
                  (study (third expr))
                  (declare (ignore capture-p))
                (values min max t fixed-p) ) )
            ((category) (values 1 1 nil t))
            ((equal) (values 0 regex-infinity nil nil))
            ((if)       ; cond then else
              (destructuring-bind (cond then else) (rest expr)
                (multiple-value-bind (min-0 max-0 cap-0-p) (study cond)
                    (declare (ignore min-0 max-0))
                (multiple-value-bind (min-1 max-1 cap-1-p) (study then)
                (multiple-value-bind (min-2 max-2 cap-2-p) (study else)
                  (values (min min-1 min-2)
                          (max max-1 max-2)
                          (or cap-0-p cap-1-p cap-2-p)
                          nil ) ) ) ) ) )
            ((max min)  ; min max subexpr
              (destructuring-bind (min max subexpr) (rest expr)
                (multiple-value-bind (submin submax capture-p fixed-p)
                    (study subexpr)
                  (unless (eql min max) (setq fixed-p nil))
                  (values (* min submin)
                          (max* max submax)
                          capture-p
                          fixed-p ) ) ) )
            ((not)
              (if (eq (second expr) :boundary)
                  (values 0 0 nil t)
                (values 1 1 nil t) ) )
            ((or)   ; subexpr+
              (pop expr)
              (multiple-value-bind (min max capture-p) (study (first expr))
                (dolist (subexpr (rest expr))
                  (multiple-value-bind (submin submax subcapture-p)
                      (study subexpr)
                    (setq min (min min submin))
                    (setq max (max max submax))
                    (when subcapture-p (setq capture-p t)) ) )
                (values min max capture-p nil) ) )
            ((:range) (values 1 1 nil t))
            ((reverse) (study (second expr)))
            ((string-equal)
              (let ((len (length (second expr))))
                (values len len t) ) )
            ((union)  (values 1 1 nil t))
            ((unless when)
              (multiple-value-bind (min max cap-p det-p) (study (second expr))
                  (declare (ignore min max))
                (values 0 0 cap-p det-p) ) )) )) )
    )
    ;;
    (study expr) ) )


;;;; regex-study-scan
;;; BUGBUG: NYI: Guess start point from the first lookbehind.
(defun regex-study-scan (expr from-end)
    (declare (type regex-expr expr))
    (declare (type t from-end))
    (declare (values fixnum regex-hint))
  (labels (
    ;; cc-size
    ;;  Guess size of character class.
    (cc-size (expr)
      (etypecase expr
        (character 1)
        (fixnum    1)
        (string    1)
        (cons
          (ecase (first expr)
            ((category) char-code-limit)
            ((not) char-code-limit)
            ((or)
              (loop for subexpr in (rest expr)
                maximize (cc-size subexpr) ) )
            ((:range)
              (destructuring-bind (min max) (rest expr)
                (if (characterp min)
                    (- (char-code max) (char-code min) -1)
                  (* (- max min -1) 2) ) ) )
            ((string-equal)
              (let* ((string (second expr))
                     (char   (char string 0)) )
                (if (both-case-p char) 2 1) ) )
            ((union)
              (loop
                with sum = 0
                for subexpr in (rest expr)
                for size = (cc-size subexpr) do
                  (cond
                    ((eql sum char-code-limit))
                    ((eql size char-code-limit) (setq sum char-code-limit))
                    (t (incf sum size)) )
                finally (return size) ))) )) )

    ;; extract-cc
    ;;  Extracts left-most character class.
    (extract-cc (expr)
 #+nil (format t "; extrac-cc: expr=~S~%" expr)
      (etypecase expr
        (character expr)
        (fixnum    expr)
        (string    (char expr 0))
        (symbol    nil)
        (cons
          (case (first expr)
            ((and) (extract-cc/and (rest expr)))
            ((capture)
              (extract-cc (third expr)) )
            ((max min)
              (destructuring-bind (min max subexpr) (rest expr)
                  (declare (ignore min max))
                (extract-cc subexpr) ) )
            ((or)
              (loop
                with cc-exprs = '()
                for subexpr in (rest expr)
                for cc-expr = (extract-cc subexpr) do
                  (when (null cc-expr) (return nil))
                  (push cc-expr cc-exprs)
                finally
                  (when cc-exprs (return `(union ,@cc-exprs))) ) )
            ((:range) expr)
            ((string-equal)
              (let ((char (char (second expr) 0)))
                (if (both-case-p char) (char-code char) char) ) )
            (otherwise nil) ) )) )

    ;; extract-cc/and
    ;;  Extract character class from AND subexprs. We need at least one fixed
    ;;  subexpr.
    (extract-cc/and (subexprs)
      (loop
        with cc-exprs   = '()
        with fixed-expr = nil
        for subexpr in subexprs
        for cc-expr = (extract-cc subexpr)
        while cc-expr do
          (push cc-expr cc-exprs)
          (multiple-value-bind (min max) (regex-study-length subexpr)
            (unless (eql min 0)
              (when (eql min max) (setq fixed-expr subexpr))
              (loop-finish)) )
        finally
#+nil (format t "; extract-cc: and: cc-exprs=~S~%" cc-exprs)
          (cond
            ((null fixed-expr) (return nil))
            ((null cc-exprs) (return nil))
            ((null (rest cc-exprs)) (return (first cc-exprs)))
            (t (return `(union ,@cc-exprs))) )) )

    ;; extract-char
    (extract-char (expr)
      (typecase expr
        (character expr)
        (fixnum    expr)
        (string    (char expr 0))
        (cons
          (case (first expr)
            ((string-equal)
              (let ((char (char (second expr) 0)))
                (if (both-case-p char)
                    (char-code char)
                 char ) ) )) )) )

    ;; extract-fixed
    ;;  Extracts left-most fixed subexpr.
    (extract-fixed (expr pos)
        (declare (values (or string cons character fixnum null)
                         (or sequence-index null)
                         t ) )
        (declare (type t expr))
 #+nil (format t "; extract-fixed: pos=~D ~S~%" pos expr)
      (etypecase expr
        (character (values expr pos t))
        (string    (values expr pos t))
        (fixnum    (values expr pos t))
        (symbol    (values nil  nil t))
        (cons
          (case (first expr)
            ((and)
              (extract-fixed/and (rest expr) pos) )

            ((capture)
              (extract-fixed (third expr) pos) )

            ((max min)
              (destructuring-bind (min max subexpr) (rest expr)
                  (declare (ignore max))
                (if (eql min 0)
                    (values nil nil nil)
                  (multiple-value-bind (fixed-expr fixed-posn)
                      (extract-fixed subexpr pos)
                    (values fixed-expr fixed-posn nil) )) ) )

            ((or)
              (extract-fixed/or (rest expr) pos) )

            ((:range)
              (values expr pos t) )

            ((string-equal)
              (values expr pos t) )

            (otherwise
              (values nil nil nil) )) )) )

    ;; extract-fixed/and
    ;;  Extracts left most restrictive fixed expression from (AND subexpr+).
    ;;  A string is expression is more restrictive than character set.
    ;;
    ;;  Example:
    ;;    Source: "[c-f]aa[x-z]"
    ;;    IR:     (and (:range #\c #\f) "aa" (:range #\x #\z))
    ;;    Result: expr="aa" pos=1
    (extract-fixed/and (subexprs pos)
      (loop
        with max-str-expr = nil
        with max-str-len  = 0
        with max-str-pos  = pos
        with min-cc-expr  = nil
        with min-cc-size  = char-code-limit
        with min-cc-pos   = pos
        for subexpr in subexprs do
          (multiple-value-bind (fixed-expr fixed-pos morep)
              (extract-fixed subexpr pos)
 #+nil (format t "; extract-fixed/and: pos=~D ~S fixed-pos=~D morep=~S~%"
    pos fixed-expr fixed-pos morep )
            (cond
              ((null fixed-expr)
                (loop-finish) )
              ((stringp fixed-expr)
                (let ((str-len (length fixed-expr)))
                  (when (< max-str-len str-len)
                    (setq max-str-len  str-len)
                    (setq max-str-pos  fixed-pos)
                    (setq max-str-expr fixed-expr)
                    (setq pos (+ fixed-pos str-len)) ) ) )
              ((and (consp fixed-expr)
                    (eq (first fixed-expr) 'string-equal) )
                (let ((str-len (length (second fixed-expr))))
                  (when (< max-str-len str-len)
                    (setq max-str-len  str-len)
                    (setq max-str-pos  fixed-pos)
                    (setq max-str-expr fixed-expr)
                    (setq pos (+ fixed-pos str-len)) ) ) )
              (t
                (let ((cc-size (cc-size fixed-expr)))
                  (when (> min-cc-size cc-size)
                    (setq min-cc-size cc-size)
                    (setq min-cc-expr fixed-expr)
                    (setq min-cc-pos  fixed-pos)
                    (setq pos (1+ fixed-pos)) ) ) ))
            (unless morep (loop-finish)) )
        finally
          (cond
            (max-str-expr
              (return (values max-str-expr max-str-pos)) )
            (min-cc-expr
              (return (values min-cc-expr min-cc-pos)) )
            (t
              (return (values (extract-cc `(and ,@subexprs)) pos)) ))) )

    ;; extract-fixed/or
    (extract-fixed/or (subexprs pos)
      (multiple-value-bind (fixed-1 pos-1)
          (extract-fixed (first subexprs) pos)
        (when (null fixed-1)
          (return-from extract-fixed/or (values nil nil nil)) )

        (loop
          with data = (list fixed-1)
          for subexpr in (rest subexprs) do
            (multiple-value-bind (fixed-2 pos-2)
                (extract-fixed subexpr pos)
              (unless (and fixed-2 (eql pos-1 pos-2))
                (return (values nil nil nil)) )
              (if (and (consp fixed-2) (eq (first fixed-2) 'or))
                  (setq data (append (rest fixed-2) data))
                (push fixed-2 data) ) )
          finally
            (return (values `(or ,@data) pos-1 nil)) ) ) )

    ;; study-anchor-at-end
    ;;  Just checks (sequence ... :EOS)
    (study-anchor-at-end (expr)
      (when (and (consp expr) (eq (first expr) 'and))
        (case (last expr)
          ((:eos :seol)
            (unless from-end REGEX-FLAG-SCAN-ONCE) )
          ((:bos)
            (when from-end REGEX-FLAG-SCAN-ONCE) ))) )

    ;; study-anchor-at-start
    (study-anchor-at-start (expr)
      (cond
        ((eq expr :bos)
          (logior REGEX-FLAG-SCAN-BOS.F REGEX-FLAG-SCAN-ONCE) )
        ((eq expr :eos)
          (logior REGEX-FLAG-SCAN-EOS.F REGEX-FLAG-SCAN-ONCE) )
        ((eq expr :pos)
          REGEX-FLAG-SCAN-POS.F )
        ((eq expr :mbol)
          (unless from-end REGEX-FLAG-SCAN-MBOL.F) )
        ((not (consp expr))
          nil )
        (t
          (case (first expr)
            ((and)
              (study-anchor-at-start (second expr)) )
            ((capture)
              (study-anchor-at-start (third  expr)) )
            ((or)
              ;; All branches start with same anchor.
              (loop
                with subexprs = (rest expr)
                with anchor-1 = (study-anchor-at-start (pop subexprs))
                for subexpr in subexprs
                for anchor-2 = (study-anchor-at-start subexpr) do
                  (when (null anchor-2) (return nil))
                  (unless (eql anchor-1 anchor-2) (return nil))
                finally (return anchor-1) ) )
            ((max min)  ; min max subexpr
              ;; This is rare case, e.g. "(?m:^foo(?s:.*))+"
              ;; It matches all consecutive lines start with "foo".
              (destructuring-bind (min max subexpr) (rest expr)
                  (declare (ignore max))
                (when (eql min 0) (study-anchor-at-start subexpr)) ) )
            ((reverse when)
              (study-anchor-at-start (second expr)) )) )) )

    ;; study-substr
    (study-substr (expr)
      (multiple-value-bind (fixed offset) (extract-fixed expr 0)
        (when fixed
          (multiple-value-bind (expr bump) (study-substr-aux fixed)
            (vector nil bump offset expr) )) ) )

    ;; study-substr-aux
    (study-substr-aux (expr)
        (declare (values t (or sequence-index null)))
 #+nil (format t "; study-substr-aux: expr=~S~%" expr)
      (etypecase expr
        (character
          (values expr 1) )
        (string
          (values expr (length expr)) )
        (fixnum
          (values expr 1) )
        (cons
          (ecase (first expr)
            ((or)
              (multiple-value-bind (prefix-expr length)
                  (study-substr/prefix (rest expr))
                (if prefix-expr
                    (values prefix-expr length)
                  (progn
                    (loop
                      for runner on (rest expr)
                      for subexpr = (first runner)
                      for char = (extract-char subexpr) do
                        (when char
                          (setf (first runner) char) ))
                    (let ((exprs (regex-study-union (rest expr))))
                      (assert (and exprs (null (rest exprs))))
                      (values (first exprs) 1) ))) ) )
            ((:range)
              (values expr 1) )
            ((string-equal)
              (let ((string (second expr)))
                (values expr (length string)) ) )
            ((union)
              (multiple-value-bind (positives negatives)
                  (regex-study-union (rest expr))
                (cond
                  ((and positives negatives)
                    (values `(union ,@positives (not (union ,negatives))) 1) )
                  (positives
                    (if (rest positives)
                        (values `(union ,@positives) 1)
                      (values (first positives) 1) ) )
                  (negatives
                    (if (null (rest negatives))
                        (values `(not ,(first negatives)) 1)
                      (values `(not (union ,@negatives)) 1) ) )) ) )) )) )

    ;; study-substr/prefix
    ;;  Returns common prefix from exprs.
    (study-substr/prefix (exprs)
      (loop
        with prefix of-type simple-string = ""
        with prefix-len = 0
        with ignore-case-p = nil
        for expr in exprs do
          (typecase expr
            (string
              (cond
                ((zerop prefix-len)
                  (setq prefix expr)
                  (setq prefix-len (length expr)) )
                (ignore-case-p
                  (return nil) )
                (t
                  (let ((pos (mismatch expr prefix)))
                    (when (eql pos 0) (return nil))
                    (when pos (setq prefix-len pos)) ) )) )
            (character
              (return nil) )
            (fixnum
              (return nil) )
            (cons
              (case (first expr)
                ((:range)
                  (return nil) )
                ((string-equal)
                  (let ((string (second expr)))
                    (cond
                      ((zerop prefix-len)
                        (setq prefix string)
                        (setq prefix-len (length string))
                        (setq ignore-case-p t) )
                      ((not ignore-case-p)
                        (return nil) )
                      (t
                        (let ((pos (mismatch string prefix :test 'char-equal)))
                          (when (eql pos 0) (return nil))
                          (when pos (setq prefix-len pos)) ) )) ) )) ))
        finally
          (assert (plusp prefix-len))
          (cond
            ((not (eql prefix-len 1))
              (let ((prefix (subseq prefix 0 prefix-len)))
                (if ignore-case-p
                    (return (values `(string-equal ,prefix) prefix-len))
                  (return (values prefix prefix-len)) ) ) )
            ((and ignore-case-p (both-case-p (schar prefix 0)))
              (return (char-code (schar prefix 0))) )
            (t
              (return (schar prefix 0)) ))) )
    )
    ;;
    (multiple-value-bind (flags hint)
        (let ((flags (study-anchor-at-start expr)))
          (if flags
              (values flags nil)
            (values 0 (study-substr expr)) ) )

      (let ((value (study-anchor-at-end expr)))
        (when value (setq flags (logior flags value))) )

      (when from-end
        (setq flags (logior flags REGEX-FLAG-SCAN-BACKWARD)) )

      (values flags hint) ) ) )


;;;; regex-study-union
;;;
;;; Syntax:
;;;     regex-study-union subexprs => positives, negatives
;;;
;;; Arguments and Values:
;;;     pos-chars   -- A null, character, (:range min, max), or
;;;                    (bit-vector op min bitvec).
;;;     pos-cats    -- A list of exprs.
;;;     neg-chars   -- A null, character, (:range min, max), or
;;;                    (bit-vector op min bitvec).
;;;     netg-cats    -- A list of exprs.
;;;
;;; Description:
;;;  Studies subexprs of union and returns positive characters, positive
;;;  categories, negative characters, and negative categories.
;
(defun regex-study-union (subexprs)
  (let ((bitvec (slot-value (regex-get-context) 'bitvec))
        (bv-min 0)
        (bv-max 0) )
    (declare (type simple-bit-vector bitvec))
    (declare (type (unsigned-byte 16) bv-min))
    (declare (type (unsigned-byte 16) bv-max))
  (labels (
    ;; bv-init
    (bv-init ()
      (bit-xor bitvec bitvec bitvec)
      (setq bv-min #xFFFF)
      (setq bv-max #x0000) )

    ;; bv-range
    (bv-range (min-char max-char)
        (declare (type character min-char max-char))
        (declare (values unspecified))
      (let ((min-code (char-code min-char))
            (max-code (char-code max-char)) )
        (setq bv-min (min bv-min min-code))
        (setq bv-max (max bv-max max-code))
        (loop
          for code from (char-code min-char) to (char-code max-char) do
            (setf (sbit bitvec code) 1) ) ) )

    ;; bv-range/i
    (bv-range/i (min-code max-code)
        (declare (type (unsigned-byte 16) min-code max-code))
        (declare (values unspecified))
      (loop
        for code from min-code to max-code
        for char = (code-char code)
        for downcase = (char-code (char-downcase char))
        for upcase   = (char-code (char-upcase   char)) do
          (if (< downcase upcase)
              (progn
                (setq bv-min (min bv-min downcase))
                (setq bv-max (max bv-max upcase)) )
              (progn
                (setq bv-min (min bv-min upcase))
                (setq bv-max (max bv-max downcase)) ))

          (setf (sbit bitvec downcase) 1)
          (setf (sbit bitvec upcase)   1) ) )

    ;; divide
    ;;  Returns list of positive cc and negative cc
    (divide (subexprs)
      (loop
        with positives = '()
        with negatives = ' ()
        for subexpr in subexprs do
          (etypecase subexpr
            (character  (push subexpr positives))
            (fixnum     (push subexpr positives))
            ((eql :any) (push subexpr positives))
            (cons
              (ecase (first subexpr)
                ((category) (push subexpr positives))
                ((not)
                  (multiple-value-bind (pos* neg*) (divide (rest subexpr))
                    (setq positives (nreconc neg* positives))
                    (setq negatives (nreconc pos* negatives)) ) )
                ((:range) (push subexpr positives))
                ((union)
                  (multiple-value-bind (pos* neg*) (divide (rest subexpr))
                    (setq positives (nreconc pos* positives))
                    (setq negatives (nreconc neg* negatives)) ) )) ))
        finally
          (setq positives (nreverse positives))
          (setq negatives (nreverse negatives))
          (return (values positives negatives)) ) )

    ;; study
    ;;  Returns chars and cats.
    (study (subexprs op)
      (loop
        initially
          (bv-init)
        finally
          (let ((chars (study-bit-vector op)))
            (setq cats (nreverse cats))
            (when chars (push chars cats))
            (return cats) )
        with cats = '()
        for subexpr in subexprs do
          (typecase subexpr
            (character
              (let ((code (char-code subexpr)))
                (setq bv-min (min bv-min code))
                (setq bv-max (max bv-max code))
                (setf (sbit bitvec code) 1) ) )
            (fixnum
              (let* ((char (code-char subexpr))
                     (downcase (char-code (char-downcase char)))
                     (upcase   (char-code (char-upcase   char))) )
                (setq bv-min (min bv-min downcase))
                (setq bv-max (max bv-max downcase))
                (setq bv-min (min bv-min upcase))
                (setq bv-max (max bv-max upcase))
                (setf (sbit bitvec downcase) 1)
                (setf (sbit bitvec upcase)   1) ) )
            ((cons (eql :range))
              (destructuring-bind (min max) (rest subexpr)
                (if (characterp min)
                    (bv-range min max)
                  (bv-range/i min max) ) ) )
            (otherwise (push subexpr cats)) )) )

    ;; study-bit-vector
    ;;  Returns one of following:
    ;;    nil                   -- if bit-vector contains no elements.
    ;;    character             -- if bit-vector contains only one element.
    ;;    (:range min max)      -- if bit-vector has only one range.
    ;;    (bit-vector min bits) -- otherwise
    (study-bit-vector (op)
        (declare (type (member = /=) op))
      (cond
        ((zerop bv-max)
          nil )
        ((eql bv-min bv-max)
          (code-char bv-min) )
        ((or (eql (1+ bv-min) bv-max)
             (eql (count 1 bitvec :start bv-min :end (1+ bv-max))
                  (- bv-max bv-min -1) ) )
          `(:range ,(code-char bv-min) ,(code-char bv-max)) )
        (t
          `(bit-vector ,op ,bv-min ,(subseq bitvec bv-min (1+ bv-max))) )) )
    )
    ;;
    (multiple-value-bind (positives negatives) (divide subexprs)
      (setq positives (study positives '=))
      (setq negatives (study negatives '/=))
      (values positives negatives) ) ) ) )


;;;; regex-study-2
(defun regex-study-union-2 (subexpr*)
  (let ((ci-min char-code-limit)
        (ci-max 0)
        (cis '())
        (cs-min char-code-limit)
        (cs-max 0)
        (css '())
        (result* '()) )
    (labels (
      (make-result ()
        (when cis
          (push `(charset eq
                    ,(code-char ci-min)
                    ,(code-char ci-max)
                    ,(coerce (nreverse cis) 'string) )
                result* ))
        (when css
          (push `(charset =
                    ,(code-char cs-min)
                    ,(code-char cs-max)
                    ,(coerce (nreverse css) 'string) )
                result* ))
        (nreverse result*) )
      )
    (dolist (subexpr subexpr* (make-result))
      (typecase subexpr
        (character
          (setq cs-min (min cs-min (char-code subexpr)))
          (setq cs-max (max cs-max (char-code subexpr)))
          (push subexpr css) )
        (fixnum
          (let* ((char (char-upcase (code-char subexpr)))
                 (code (char-code char)) )
            (setq ci-min (min ci-min code))
            (setq ci-max (max ci-max code))
            (push char cis) ) )
        (otherwise
          (push subexpr result*) )) ) ) ) )
