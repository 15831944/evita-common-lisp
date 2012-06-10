;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - byte-code compiler
;;; lisp/regex/regex-byte-comp.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-byte-comp.lisp#3 $
;;;
;;; Description:
;;;  This file contains regex byte-code compiler.
;
(in-package :si)

;;;; regex-byte-compile
;;;
;;; Description:
;;;  Compiles regular expression into Regex byte-code stream and
;;;  sets a scanner to regex object, then returns regex object.
;
(defun regex-byte-compile (source &rest keys)
    (declare (type string source))
    (declare (values regex))
    (declare (dynamic-extent keys))
  (multiple-value-bind (expr name-vector from-end modifiers)
      (apply #'parse-regex source keys)
      (declare (type t expr))
      (declare (type simple-vector name-vector))
      (declare (type t from-end))
  (let ((codevec (regex-byte-compile-matcher expr from-end)))

  (multiple-value-bind (flags scan-hint) (regex-study-scan expr from-end)
    (let ((scanner (regex-byte-compile-scanner flags scan-hint)))
      (if (null scanner)
          (setq scan-hint nil)
        (progn
          (setf (svref scan-hint 0) scanner)
          (if from-end
            (setq flags (logior flags REGEX-FLAG-SCAN-FUN.B))
            (setq flags (logior flags REGEX-FLAG-SCAN-FUN.F)) ))) )

    (make-instance 'regex
       :function
          (lambda (match)
              (declare (lambda-name byte-code-regex-matcher))
              (declare (type regex-match match))
              (declare (values t))
            (regex-execute-byte-code match codevec ) )
      :codevec       codevec
      :flags         flags
      :scan-hint     scan-hint
      :min-length    (regex-study-length expr)
      :name-vector   name-vector
      :source        source
      :modifiers     modifiers  ) ) ) ) )


;;;; regex-byte-compile-matcher
;;;
;;; Description:
;;;  Compiles regex parse tree as matcher and returns code-vector.
;;;
;;; Note: For readability, we don't encode backward-p, not equal and
;;; ignore-case varition of opcode.
;;;
;
(defun regex-byte-compile-matcher (expr from-end)
    (declare (type regex-expr expr))
    (declare (type t from-end))
    (declare (values simple-vector))
  (let* ((context        (regex-get-context))
         (codevec        (slot-value context 'codevec))
         (backward-p     from-end)
         (last-pc        0)
         (loop-depth     0) )
    (declare (type sequence-index loop-depth))

  (labels (
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Tree Walker
    ;;

    ;; compile-1
    ;;  Syntax:
    ;;      compile-1 expr min-rest => unspecified
    ;;  Arguments and Values:
    ;;      expr        -- A regex-parse-node.
    ;;      min-rest    -- An index. A minimum length of string expected
    ;;                     after expr.
    (compile-1 (expr min-rest)
        (declare (type sequence-index min-rest))
        (declare (values unspecified))

         ;(format t "; compile-1: min-rest=~D ~S~%" min-rest expr)
         (assert (>= min-rest 0))

      (etypecase expr
        (character (emit-char= '= expr))
        (string
          (if backward-p
              (emit-2 REGEX-OP-STRING=.B expr)
              (emit-2 REGEX-OP-STRING=.F expr) ) )
        (fixnum
          (let ((char (code-char expr)))
            (if (both-case-p char)
               (emit-char-equal '= char)
              (emit-char= '= char) ) ) )
        ((eql :void))
        (symbol (compile-symbol expr))
        (cons
          (ecase (first expr)
            ((=)    (compile/= (second expr)))
            ((and)  (compile/and (rest expr) min-rest))
            ((atom) (compile/atom expr min-rest))
            ((bit-vector)
              (destructuring-bind (which min-code bitvec) (rest expr)
                (compile/bit-vector which min-code bitvec) ) )

            ((capture)
              (destructuring-bind (nth subexpr) (rest expr)
                (compile/capture nth subexpr min-rest) ) )

            ((category) ; category
              (compile/category '= (second expr) (third expr)) )

            ((charset)
              (compile/charset
                (second expr) (third expr) (fourth expr) (fifth expr) ) )

            ((equal) (compile/equal (second expr)))

            ((if)
              (destructuring-bind (cond then else) (rest expr)
                (compile/if cond then else min-rest) ) )

            ((max)
              (destructuring-bind (min max subexpr) (rest expr)
                (compile/max min max subexpr min-rest) ) )

            ((min)
              (destructuring-bind (min max subexpr) (rest expr)
                (compile/min min max subexpr min-rest) ) )

            ((not)  ; subexpr
              (compile/not (second expr) min-rest) )

            ((or)
              (compile/or (rest expr) min-rest) )

            ((:range)   ; min-char max-char
              (compile/range '= (second expr) (third expr)) )

            ((reverse)
              (setq backward-p (not from-end))
              (compile-1 (second expr) min-rest)
              (setq backward-p from-end) )

            ((string-equal) ; string
              (let ((string (second expr)))
                (if backward-p
                    (emit-2 REGEX-OP-STRING-EQUAL.B string)
                    (emit-2 REGEX-OP-STRING-EQUAL.F string) ) ) )

            ((union) ; subexpr+
              (compile/union '= (rest expr) min-rest) )

            ((unless)
              (compile/unless expr min-rest) )

            ((when)
              (compile/when expr min-rest) )) )) )

    ;; compile/=
    ;;  See also: compile/equal
    (compile/= (nth)
      (if backward-p
          (emit-2 REGEX-OP-CAPTURE=.B nth)
          (emit-2 REGEX-OP-CAPTURE=.F nth) ) )

    ;; compile/and
    (compile/and (subexprs min-rest)
      (dolist (subexpr subexprs)
        (incf min-rest (compute-min-len subexpr)) )
      (dolist (subexpr subexprs)
        (decf min-rest (compute-min-len subexpr))
        (compile-1 subexpr min-rest) ) )

    ;; compile/atom
    (compile/atom (expr min-rest)
        (declare (ignore min-rest))
      (destructuring-bind (subexpr) (rest expr)
        (if (not (use-stack-p subexpr))
            (compile-1 subexpr 0)
          (progn
            (emit-1 REGEX-OP-SAVE-CXP)
            (compile-1 subexpr 0)
            (emit-1 REGEX-OP-RESTORE-CXP) )) ) )

    ;; compile/bit-vector
    (compile/bit-vector (which min-code bitvec)
        (declare (type (member = /=) which))
        (declare (type character-code min-code))
        (declare (type simple-bit-vector bitvec))
      (if backward-p
          (if (eq which '=)
              (emit-3 REGEX-OP-BIT-VECTOR=.B  min-code bitvec)
              (emit-3 REGEX-OP-BIT-VECTOR/=.B min-code bitvec) )
          (if (eq which '=)
              (emit-3 REGEX-OP-BIT-VECTOR=.F  min-code bitvec)
              (emit-3 REGEX-OP-BIT-VECTOR/=.F min-code bitvec) )) )

    ;; compile/capture
    (compile/capture (nth subexpr min-rest)
      (emit-1 REGEX-OP-PUSH-POS)
      (compile-1 subexpr min-rest)
      (if backward-p
          (emit-2 REGEX-OP-CAPTURE.B nth)
          (emit-2 REGEX-OP-CAPTURE.F nth) ) )

    ;; compile/category
    (compile/category (op min max)
      (if max
          (if (eq op '=)
              (if backward-p
                  (emit-3 REGEX-OP-CATEGORY<=.B min max)
                  (emit-3 REGEX-OP-CATEGORY<=.F min max) )
            (if backward-p
              (emit-3 REGEX-OP-CATEGORY>.B min max)
              (emit-3 REGEX-OP-CATEGORY>.F min max) ))
        (if (eq op '=)
            (if backward-p
                (emit-2 REGEX-OP-CATEGORY=.B min)
                (emit-2 REGEX-OP-CATEGORY=.F min) )
          (if backward-p
              (emit-2 REGEX-OP-CATEGORY/=.B min)
              (emit-2 REGEX-OP-CATEGORY/=.F min) ))) )

    ;; compile/charset
    (compile/charset (op min max string)
      (emit-4
        (ecase op
          ((=)  (if backward-p REGEX-OP-CHARSET=.B   REGEX-OP-CHARSET=.F))
          ((/=) (if backward-p REGEX-OP-CHARSET/=.B  REGEX-OP-CHARSET/=.F))
          ((eq) (if backward-p REGEX-OP-CHARSET-EQ.B REGEX-OP-CHARSET-EQ.F))
          ((ne) (if backward-p REGEX-OP-CHARSET-NE.B REGEX-OP-CHARSET-NE.F)) )
        min
        max
        string ) )

    ;; compile/equal
    ;;  See also: compile/=
    (compile/equal (nth)
      (if backward-p
          (emit-2 REGEX-OP-CAPTURE-EQUAL.B nth)
          (emit-2 REGEX-OP-CAPTURE-EQUAL.F nth) ) )

    ;; compile/if
    (compile/if (cond then else min-rest)
        (declare (values sequence-index))
      (ecase (first cond)
        ((boundp)
          (compile/if/capture cond then else min-rest) )
        ((unless when)
          (compile/if/lookaround cond then else min-rest) ) ) )

    ;; compile/if-capture
    ;;        CAPTURE-IF-NOT L001
    ;;        ... then ...
    ;;        GO L002
    ;;  L001  ... else ...
    ;;  L002  ...
    (compile/if/capture (cond then else min-rest)
      (let ((push-pc (+ (current-pc) 1)))
        (emit-3 REGEX-OP-CAPTURE-IF-NOT 'label (second cond))
        (if (eq else :void)
            (progn
              (compile-1 then min-rest)
              (patch-label push-pc) )
          (let ((branch-pc
                  (progn
                    (compile-1 then min-rest)
                    (emit-ref-label REGEX-OP-GO) ) ))
            (patch-label push-pc)
            (compile-1 else min-rest)
            (patch-label branch-pc) )) ) )

    ;; compile/if/lookaround
    ;;           SAVE-CXP
    ;;           OR L001
    ;;           ... look around ...
    ;;           RESTOR-CXP
    ;;           ... then ...
    ;;           GO L002
    ;;      L001 RESTOR-CXP
    ;;           ... else ...
    ;;      L002
    ;;
    (compile/if/lookaround (cond then else min-rest)
      (let ((push-pc 0))
        (emit-1 REGEX-OP-SAVE-CXP)
        (setq push-pc (emit-ref-label REGEX-OP-OR))
        (compile-1 cond min-rest)
        (emit-1 REGEX-OP-RESTORE-CXP)

        (if (eq else :void)
            (progn
              (compile-1 then min-rest)
              (patch-label push-pc) )
          (let ((branch-pc
                  (progn
                    (compile-1 then min-rest)
                    (emit-ref-label REGEX-OP-GO) ) ))
            (patch-label push-pc)
            (emit-1 REGEX-OP-RESTORE-CXP)   ; for OR
            (compile-1 else min-rest)
            (patch-label branch-pc) )) ) )

    ;; compile-loop
    ;;  Compiles greedy, lazzy and posessive (NYI) repeation.
    ;;
    ;;  Compiled form of repetition "r*" has following form:
    ;;           OR       L001          ; when min == 0
    ;;           PUSH-INT -min          ; push initial value of loop counter
    ;;      L002 PUSH-POS               ; push pos for null match check
    ;;           ... r ...
    ;;           NULL     L001          ; check null match
    ;;           REST     n             ; check rest length
    ;;           MAX      L002 max-min  ; check and increment loop counter
    ;;      L001 ...
    ;;
    ;; BUGBUG: We should count min-rest of WHEN and UNLESS, since
    ;; regex-study-length returns 0 for them.
    ;;
    (compile-loop (op min max subexpr min-rest)
      (let* ((min-sublen   (compute-min-len subexpr))
             (match-null-p (eql min-sublen 0))
             (counter-p    (or (>= min 2) (not (eql max regex-infinity))))
             (patch nil) )
        (when (eql min 0)
          (ecase op
            ((max)
              (setq patch (emit-ref-label-2 REGEX-OP-OR patch)) )
            ((min)
              (setq patch (emit-ref-label-2 REGEX-OP-PUSH patch)) )))

        (when counter-p
          (emit-2 REGEX-OP-PUSH-int (- min)) )

        (let ((loop-pc (current-pc)))
          (when match-null-p (emit-1 REGEX-OP-PUSH-POS))

          (incf loop-depth)
          (compile-1 subexpr min-rest)
          (decf loop-depth)

          (when match-null-p
            (if counter-p
                (setq patch (emit-ref-label-2 REGEX-OP-NULC patch))
              (setq patch (emit-ref-label-2 REGEX-OP-NULL patch)) ))

          (cond
            ((zerop min-rest))
            ((eql loop-depth 0)
              ;; We use LAST for the last outermost loop, otherwise
              ;; we failed (eval-regex "foo\\w*\\d{4}baz" "foobar1234baz").
              (unless (zerop last-pc)
                (setf (elt codevec last-pc)
                  (if (eql (elt codevec last-pc) REGEX-OP-LAST.B)
                      REGEX-OP-REST.B
                      REGEX-OP-REST.F )))
              (setq last-pc (current-pc))
              (if backward-p
                  (emit-2 REGEX-OP-LAST.B min-rest)
                  (emit-2 REGEX-OP-LAST.F min-rest) ) )
            (t
              (if backward-p
                  (emit-2 REGEX-OP-REST.B min-rest)
                  (emit-2 REGEX-OP-REST.F min-rest) ) ))

          (if (not counter-p)
              (ecase op
                ((max)
                  (emit-2 REGEX-OP-PUSH loop-pc) )
                ((min)
                  (emit-2 REGEX-OP-OR loop-pc) ))
            (ecase op
              ((max)
                (emit-3 REGEX-OP-MAX loop-pc (- max min)) )
              ((min)
                (emit-3 REGEX-OP-MIN loop-pc (- max min)) )))

          (patch-labels patch) ) ) )

    ;; compile/max
    (compile/max (min max subexpr min-rest)
      (multiple-value-setq (min max subexpr)
        (fold-nested-loop 'max min max subexpr) )

      (cond
        ((and (eql min 0) (eql max 0)) 0)
        ((and (eql min 0) (eql max 1)) (compile/max-0-1 subexpr min-rest))
        ((and (eql min max) (compile/max-fixed min subexpr min-rest)))
        ((compile/max-simple  min max subexpr min-rest))
        ((compile/max-capture min max subexpr min-rest))
        (t (compile-loop 'max min max subexpr min-rest)) ) )

    ;; compile/max-0-1
    ;;  For r? == r{0,1}
    (compile/max-0-1 (subexpr min-rest)
      (let ((push-pc (emit-ref-label REGEX-OP-OR)))
        (compile-1 subexpr min-rest)
        (patch-label push-pc) ) )

    ;; compile/max-capture
    ;;   Factor capturing out when r is deterministic.
    ;;      (r)* => (?:r*(r))?
    ;;      (r)+ => (?:r*(r))
    ;;  This xform exposes mergable nested loop.
    (compile/max-capture (min max subexpr min-rest)
      (unless (and (<= min 1) (eql max regex-infinity))
        (return-from compile/max-capture nil) )

      (unless (and (consp subexpr) (eq (first subexpr) 'capture))
        (return-from compile/max-capture nil) )

      (let ((subsubexpr (third subexpr)))
        (multiple-value-bind (submin submax cap-p det-p)
            (regex-study-length subsubexpr)
            (declare (ignore cap-p))
          (unless (and det-p (eql submin submax))
            (return-from compile/max-capture nil) )

          (let ((expr `(and (max 0 ,regex-infinity ,subsubexpr) ,subexpr)))
            (when (eql min 0) (setq expr `(max 0 1 ,expr)))
            (compile-1 expr min-rest)
            :done ) ) ) )

    ;; compile/max-fixed
    ;;  For a{4} and a{4}?
    (compile/max-fixed (n subexpr min-rest)
        ;; BUGBUG: NYI: We should use min-rest for compile/max-fixed.
        (declare (ignore min-rest))
      (cond
        ((eq subexpr :any)
          (when (<= n 10)
            (loop
              with opcode = (if backward-p REGEX-OP-ANY.B REGEX-OP-ANY.F)
              repeat n do (emit-1 opcode)
              finally (return :done) )) )
        ((characterp subexpr)
          (cond
            ((eql n 1)
              (if backward-p
                  (emit-2 REGEX-OP-CHAR=.B subexpr)
                  (emit-2 REGEX-OP-CHAR=.F subexpr) )
              :done )
            ((<= n 20)
              (let ((chars (make-string n :initial-element subexpr)))
                (if backward-p
                    (emit-2 REGEX-OP-STRING=.B chars)
                    (emit-2 REGEX-OP-STRING=.F chars) )
                :done ) )) )
        ((typep subexpr 'fixnum)
          (cond
            ((eql n 1)
              (if backward-p
                  (emit-2 REGEX-OP-CHAR-EQ.B (code-char subexpr))
                  (emit-2 REGEX-OP-CHAR-EQ.F (code-char subexpr)) )
              :done )
            ((<= n 20)
              (let ((chars (make-string n
                                :initial-element (code-char subexpr)) ))
                (if backward-p
                    (emit-2 REGEX-OP-STRING-EQUAL.B chars)
                    (emit-2 REGEX-OP-STRING-EQUAL.F chars) )
                :done ) )) ) ) )

    ;; compile/max-simple
    (compile/max-simple (min max subexpr min-rest)
      (cond
        ((not (eql max regex-infinity)) nil)

        ((eq subexpr :any)
          (case min
            ((0) (emit-2 REGEX-OP-OR (+ (current-pc) 4)))
            ((1))
            (otherwise
              (unless (compile/max-fixed (1- min) subexpr min-rest)
                (return-from compile/max-simple nil) ) ))
          (if backward-p
              (emit-2 REGEX-OP-REPEAT-ANY.B min-rest)
              (emit-2 REGEX-OP-REPEAT-ANY.F min-rest) )
          :done )

        ((characterp subexpr)
          (case min
            ((0) (emit-2 REGEX-OP-OR (+ (current-pc) 5)))
            ((1))
            (otherwise
              (unless (compile/max-fixed (1- min) subexpr min-rest)
                (return-from compile/max-simple nil) ) ))
          (if backward-p
              (emit-3 REGEX-OP-REPEAT-CHAR=.B min-rest subexpr)
              (emit-3 REGEX-OP-REPEAT-CHAR=.F min-rest subexpr) )
          :done )

        ((typep subexpr 'fixnum)
          (case min
            ((0) (emit-2 REGEX-OP-OR (+ (current-pc) 5)))
            ((1))
            (otherwise
              (unless (compile/max-fixed (1- min) subexpr min-rest)
                (return-from compile/max-simple nil) ) ))
          (setq subexpr (code-char subexpr))
          (if backward-p
              (emit-3 REGEX-OP-REPEAT-CHAR-EQ.B min-rest subexpr)
              (emit-3 REGEX-OP-REPEAT-CHAR-EQ.F min-rest subexpr) )
          :done )

        ((consp subexpr)
          (case (first subexpr)
            ((not)
              (compile/max-simple-not min subexpr min-rest))
            ((:range)
              (compile/max-simple-range min subexpr min-rest) )) )) )

    ;; compile/max-simple-not
    (compile/max-simple-not (min expr min-rest)
      (let ((subexpr (second expr)))
        (typecase subexpr
          (character
            (compile/max-simple-not/char= min expr min-rest) )
          (fixnum
            (compile/max-simple-not/char-equal min expr min-rest) )) ) )

    ;; compile/max-simple-not/char-equal
    ;;  For /[^x]{n,}/i
    (compile/max-simple-not/char-equal (min expr min-rest)
      (let ((operand (code-char (second expr))))
        (case min
          ((0) (emit-2 REGEX-OP-OR (+ (current-pc) 5)))
          ((1))
          (otherwise
            (unless (compile/max-fixed min expr min-rest)
              (return-from compile/max-simple-not/char-equal nil) ) ))

        (if backward-p
          (emit-3 REGEX-OP-REPEAT-CHAR-NE.B min-rest operand)
          (emit-3 REGEX-OP-REPEAT-CHAR-NE.F min-rest operand) )
        :done ) )

    ;; compile/max-simple-not/char=
    ;;  For /[^x]{n,}/
    (compile/max-simple-not/char= (min expr min-rest)
      (let ((operand (second expr)))
        (case min
          ((0) (emit-2 REGEX-OP-OR (+ (current-pc) 5)))
          ((1))
          (otherwise
            (unless (compile/max-fixed min expr min-rest)
              (return-from compile/max-simple-not/char= nil) ) ))

        (if backward-p
          (emit-3 REGEX-OP-REPEAT-CHAR/=.B min-rest operand)
          (emit-3 REGEX-OP-REPEAT-CHAR/=.F min-rest operand) )
        :done ) )

    ;; compile/max-simple-range
    (compile/max-simple-range (min expr min-rest)
      (etypecase (second expr)
        (character
          (compile/max-simple-range/cs min expr min-rest) )
        (fixnum
          nil ) ) )

    ;; compile/max-simple-range/cs
    ;;  For /[a-z]{n,}/
    (compile/max-simple-range/cs (min expr min-rest)
      (destructuring-bind (min-char max-char) (rest expr)
        (case min
          ((0) (emit-2 REGEX-OP-OR (+ (current-pc) 6)))
          ((1))
          (otherwise
            (unless (compile/max-fixed min expr min-rest)
             (return-from compile/max-simple-range/cs nil) ) ))

        (if backward-p
            (emit-4 REGEX-OP-REPEAT-CHAR<=.B
                      min-rest min-char max-char  )
            (emit-4 REGEX-OP-REPEAT-CHAR<=.F
                      min-rest min-char max-char  ))
        :done ) )

    ;; compile/min
    (compile/min (min max subexpr min-rest)
      (multiple-value-setq (min max subexpr)
        (fold-nested-loop 'min min max subexpr) )
      (cond
        ((and (eql min 0) (eql max 0))
          :done )
        ((and (eql min 0) (eql max 1))
          (compile/min-0-1 subexpr min-rest) )
        ((and (eql min max) (compile/max-fixed min subexpr min-rest)))
        (t
          (compile-loop 'min min max subexpr min-rest) )) )

    ;; compile/min-0-1
    (compile/min-0-1 (subexpr min-rest)
      (let ((push-pc (emit-ref-label REGEX-OP-PUSH)))
        (compile-1 subexpr min-rest)
        (patch-label push-pc) ) )

    ;; compile/not
    (compile/not (subexpr min-rest)
      (etypecase subexpr
        (character (emit-char= '/= subexpr))
        (fixnum    (emit-char-equal '/= (code-char subexpr)))
        (symbol
          (ecase subexpr
            ((:any)      (emit-2 REGEX-OP-FAIL 0))
            ((:boundary) (emit-1 REGEX-OP-BOUNDARY/=))
            ((:space)    (emit-1 REGEX-OP-SPACE/=.F))
            ((:word)     (emit-1 REGEX-OP-WORD/=.F)) ) )
        (cons
          (ecase (first subexpr)
            ((bit-vector)
              (destructuring-bind (op min-code bitvec) (rest subexpr)
                (ecase op
                  ((=)  (compile/bit-vector '/= min-code bitvec))
                  ((/=) (compile/bit-vector '=  min-code bitvec)) ) ) )
            ((category)
              (compile/category '/= (second subexpr) (third subexpr)) )
            ((charset)
              (compile/charset
                (cdr (assoc (second subexpr)
                            '((= . /=) (/= . =) (eq . ne) (ne .eq)) ))
                (third  subexpr)        ; min
                (fourth subexpr)        ; max
                (fifth  subexpr) ) )   ; string
            ((not)
              (compile-1 (second subexpr) min-rest) )
            ((:range)
              (compile/range '/= (second subexpr) (third subexpr)) )
            ((union)
              (compile/union '/= (rest subexpr) min-rest) )) )) )

    ;; compile/or
    ;;  (or)        => nothing
    ;;  (or expr)   => expr
    ;;  (or char* expr char*)   => (or (union char*) expr (union char*))
    (compile/or (subexprs min-rest)
      (cond
        ((null subexprs))
        ((null (rest subexprs))
          (compile-1 (first subexprs) min-rest) )
        (t
          (loop
            with place = nil
            with union = nil
            with prev  = nil
            with all-p = t
            for runner on subexprs
            for subexpr = (first runner) do
              (cond
                ((not (or (char-expr-p subexpr) (char-class-expr-p subexpr)))
                  (setq place nil)
                  (setq prev  runner)
                  (setq all-p nil) )
                ((null place)
                  (setq place runner)
                  (setq prev  runner)
                  (setq union subexpr) )
                (t
                  (setf (rest prev) (rest runner))
                  (setq union `(union ,subexpr ,union))
                  (setf (first place) union) ))
            finally
              (if (and all-p union)
                  (return (compile/union '= subexprs min-rest)) )
                  (return (compile/or-aux   subexprs min-rest)) ) )) )


    ;; compile/or-aux
    (compile/or-aux (subexprs min-rest)
      (loop
        with patch = nil
        for runner on subexprs
        for push-pc = (and (rest runner) (emit-ref-label REGEX-OP-OR))
        for subexpr = (first runner) do
          (compile-1 subexpr min-rest)
          (when push-pc
             ;; BUGBUG: NYI: emit pop if all altanatives are
             ;; distinct.
            (setq patch (emit-ref-label-2 REGEX-OP-GO patch))
            (patch-label push-pc) )
        finally
          (patch-labels patch) ) )

    ;; compile/range
    (compile/range (which min max)
      (cond
        ((eql min max)
          (if (characterp min)
              (emit-char= which min)
            (emit-char-equal which min) ) )
        ((characterp min)
          (if backward-p
              (if (eq which '=)
                  (emit-3 REGEX-OP-CHAR<=.B min max)
                  (emit-3 REGEX-OP-CHAR>.B  min max) )
              (if (eq which '=)
                  (emit-3 REGEX-OP-CHAR<=.F min max)
                  (emit-3 REGEX-OP-CHAR>.F  min max) )) )
        (t
          (let ((min (code-char min))
                (max (code-char max)) )
            (if backward-p
                (if (eq which '=)
                    (emit-3 REGEX-OP-CHAR-LE.B min max)
                    (emit-3 REGEX-OP-CHAR-GT.B min max) )
                (if (eq which '=)
                    (emit-3 REGEX-OP-CHAR-LE.F min max)
                    (emit-3 REGEX-OP-CHAR-GT.F min max) )) ) )) )

    ;; compile-symbol
    (compile-symbol (expr)
      (let ((opcode
          (if backward-p
              ;; Parser doesn't generate MEOL and SEOL for backward search.
              (ecase expr
                ((:any)      #.REGEX-OP-ANY.B)
                ((:bos)      #.REGEX-OP-BOS)
                ((:boundary) #.REGEX-OP-BOUNDARY=)
                ((:eos)      #.REGEX-OP-EOS)
                ((:mbol)     #.REGEX-OP-MBOL)
                ((:pos)      #.REGEX-OP-POS.B)
                ((:space)    #.REGEX-OP-SPACE=.B)
                ((:word)     #.REGEX-OP-WORD=.B) )
              (ecase expr
                ((:any)      #.REGEX-OP-ANY.F)
                ((:bos)      #.REGEX-OP-BOS)
                ((:boundary) #.REGEX-OP-BOUNDARY=)
                ((:eos)      #.REGEX-OP-EOS)
                ((:mbol)     #.REGEX-OP-MBOL)
                ((:meol)     #.REGEX-OP-MEOL.F)
                ((:pos)      #.REGEX-OP-POS.F)
                ((:seol)     #.REGEX-OP-SEOL.F)
                ((:space)    #.REGEX-OP-SPACE=.F)
                ((:word)     #.REGEX-OP-WORD=.F) )) ))
        (emit-1 opcode) ) )

    ;; compile/union
    ;;  positive(=)                 negative(/=)
    ;;      OR L_1                      not subexpr1
    ;;      ... subexpr1 ...            ANY.B
    ;;      GO L_end                    not subexpr2
    ;;    L_1:                          ANY.B
    ;;      OR L_2                      ...
    ;;      ... subexpr2 ...            not subexpr3
    ;;      GO L_end
    ;;      ...
    ;;    L_end:
    (compile/union (op subexpr* min-rest)
      (let ((subexpr* (regex-study-union-2 subexpr*)))
        (ecase op
          ((=)
            (cond
              ((null subexpr*))
              ((null (rest subexpr*))
                (compile-1 (first subexpr*) min-rest) )
              (t
                (compile/or-aux subexpr* min-rest) )) )
          ((/=)
            (cond
              ((null subexpr*))
              ((null (rest subexpr*))
                (compile-1 `(not ,(first subexpr*)) min-rest) )
              (t
                (loop while subexpr* do
                  (compile-1 `(not ,(pop subexpr*)) min-rest)
                  (when subexpr* (emit-1 REGEX-OP-ANY.B)) ) )) )) ) )

    ;; compile/unless
    (compile/unless (expr min-rest)
      (let ((subexpr (second expr)))
        (if (eq subexpr :void)
            (emit-2 REGEX-OP-FAIL 0)
          (progn
            (emit-1 REGEX-OP-SAVE-CXP)
            (let ((patch-pc (emit-ref-label REGEX-OP-OR)))
              (compile-1 subexpr min-rest)
              (emit-1 REGEX-OP-RESTORE-CXP)
              (emit-2 REGEX-OP-FAIL 0)
              (patch-label patch-pc)
              (emit-1 REGEX-OP-RESTORE-CXP) ))) ) )

    ;; compile/when
    (compile/when (expr min-rest)
      (let ((subexpr (second expr)))
        (emit-1 REGEX-OP-SAVE-CXP)
        (emit-1 REGEX-OP-SAVE-POS)
        (compile-1 subexpr min-rest)
        (emit-1 REGEX-OP-RESTORE-POS)
        (emit-1 REGEX-OP-RESTORE-CXP) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Emitters
    ;;

    ;; current-pc
    (current-pc ()
      (length codevec) )

    ;; emit-1
    (emit-1 (op)
      (vector-push-extend op codevec) )

    ;; emit-2
    (emit-2 (op operand)
      (vector-push-extend op codevec)
      (vector-push-extend operand codevec) )

    ;; emit-3
    (emit-3 (op operand-1 operand-2)
      (vector-push-extend op codevec)
      (vector-push-extend operand-1 codevec)
      (vector-push-extend operand-2 codevec) )

    ;; emit-4
    (emit-4 (op operand-1 operand-2 operand-3)
      (vector-push-extend op codevec)
      (vector-push-extend operand-1 codevec)
      (vector-push-extend operand-2 codevec)
      (vector-push-extend operand-3 codevec) )

    ;; emit-char=
    (emit-char= (op char)
        (declare (type (member = /=) op))
        (declare (type character char))
      (if backward-p
          (if (eq op '=)
              (emit-2 REGEX-OP-CHAR=.B  char)
              (emit-2 REGEX-OP-CHAR/=.B char) )
          (if (eq op '=)
              (emit-2 REGEX-OP-CHAR=.F  char)
              (emit-2 REGEX-OP-CHAR/=.F char) )) )

    ;; emit-char-equal
    (emit-char-equal (op char)
        (declare (type (member = /=) op))
        (declare (type character char))
      (if backward-p
          (if (eq op '=)
              (emit-2 REGEX-OP-CHAR-EQ.B     char)
              (emit-2 REGEX-OP-CHAR-NE.B char) )
          (if (eq op '=)
              (emit-2 REGEX-OP-CHAR-EQ.F     char)
              (emit-2 REGEX-OP-CHAR-NE.F char) )) )

    ;; emit-make-captures-unbound
    #+nil
    (emit-make-captures-unbound (expr)
      (when (consp expr)
        (ecase (first expr)
          ((atom unless when)
            (emit-make-captures-unbound (second expr)) )
          ((capture)    ; nth
            (emit-2 REGEX-OP-CAPTURE-MAKUNBOUND (second expr)) )
          ((max min)    ;  min max subexpr
            (emit-make-captures-unbound (third expr)) )
          ((not) nil)
          ((or progn)     ; subexpr+
            (dolist (subexpr (rest expr))
              (emit-make-captures-unbound subexpr) ) )
          ((union) nil) )) )

    ;; emit-ref-label
    (emit-ref-label (op)
      (vector-push-extend op codevec)
      (prog1
         (current-pc)
         (vector-push-extend 'label codevec) ) )

    ;; emit-ref-label-2
    (emit-ref-label-2 (op link)
      (vector-push-extend op codevec)
      (prog1
         (current-pc)
         (vector-push-extend link codevec) ) )

    ;; patch-label
    (patch-label (label-pc)
      (setf (elt codevec label-pc) (current-pc)) )

    ;; patch-labels
    (patch-labels (patch)
      (loop
        with next-pc = (current-pc)
        while patch do
          (let ((next (elt codevec patch)))
            (setf (elt codevec patch) next-pc)
            (setq patch next) )) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Utitlity funcitons
    ;;

    ;; char-class-expr-p
    (char-class-expr-p (expr)
      (and (consp expr) (member (first expr) '(not :range union))) )

    ;; char-expr-p
    (char-expr-p (expr)
      (or (characterp expr)
          (and (typep expr 'fixnum) (not (minusp expr))) ) )

    ;; compute-min-len
    (compute-min-len (expr)
        (declare (values sequence-index))
      (values (regex-study-length expr)) )


    ;; Fold nested loop:
    ;;  (max min-1 max-1 (max min-2 max-2 subexpr))
    ;;      where min-2 /= max-2
    ;;  ==>
    ;;  (max (* min-1 min-2) (* max-1 max-2) subexpr)
    ;;
    ;;  Example:
    ;;    (a{1,4})* => a+
    ;;    (a{4})*   => (a{4})*
    ;;
    (fold-nested-loop (op min max subexpr)
      (loop
        (unless (and (consp subexpr) (eq (first subexpr) op))
          (return (values min max subexpr)) )

        (destructuring-bind (submin submax subsubexpr) (rest subexpr)
          (when (eql submin submax)
            (return (values min max subexpr)) )

          (setq min (* min submin))
          (setq max (max* max submax))
          (setq subexpr subsubexpr) )) )

    ;; max*
    (max* (max-1 max-2)
        (declare (type sequence-index min-1 max-1))
        (declare (values fixnum))
      (cond
        ((eql max-1 0) 0)
        ((eql max-2 0) 0)
        ((eql max-1 regex-infinity) max-1)
        ((eql max-2 regex-infinity) max-2)
        (t (* max-1 max-2)) ) )

    ;; use-stack-p
    (use-stack-p (expr)
      (when (consp expr)
        (ecase (first expr)
          ((=) nil)
          ((and) (some #'use-stack-p (rest expr)) )
          ((atom) (use-stack-p (second expr)) )
          ((capture) t)
          ((category) nil)
          ((equal) nil)
          ((if) t)
          ((max min) t)
          ((not) t) ; BUGBUG: Should check subexpr
          ((or) (some #'use-stack-p (rest expr)))
          ((:range) nil)
          ((string-equal) nil)
          ((union) t ) ; BUGBUG: Should check subexpr
          ((unless) t)
          ((when) t) )) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Optimizer
    ;;

    ;; optimize
    (optimize ()
      (loop while (optimize-branch)) )

    ;; optimize-branch
    ;;  Transform
    ;;        GO label  =>    SUCCESS
    ;;        ...                 ...
    ;;      label:              label:
    ;;        SUCCESS         SUCCESS
    ;;
    (optimize-branch ()
      (loop
        with pc = 0
        with end-pc = (- (length codevec) 2)
        with optimized-p = nil
        while (< pc end-pc) do
          (let ((opcode (elt codevec pc)))
            (case opcode
              ((#.REGEX-OP-GO)
                (when (eql (elt codevec (+ pc 1)) end-pc)
                  (setf (elt codevec pc) REGEX-OP-SUCCESS)
                  (setq optimized-p t) ) ))
            (incf pc (regex-instruction-size opcode)) )
        finally (return optimized-p) ) )

    ;; update-labels
    #+nil
    (update-labels (ref-pc delta)
      (loop
        with pc = 0
        with end-pc = (- (length codevec) 2)
        while (< pc end-pc) do
          (let* ((opcode (elt codevec pc))
                 (info (svref *regex-instruction-vector* opcode))
                 (size     (svref info 0))
                 (operands (svref info 2)) )
            (when (eq (first operands) 'label)
              (let ((target-pc (elt codevec (+ pc 1))))
                (when (>= target-pc ref-pc)
                  (setf (elt codevec (+ pc 1)) (+ target-pc delta)) )))
            (incf pc size) )) )
    )
    ;;
    ;; regex-compile
    ;;
    (setf (fill-pointer codevec) 0)
    (compile-1 expr 0)
    (emit-2 REGEX-OP-SUCCESS 0)
    (optimize)
    (copy-seq codevec) ) ) )
