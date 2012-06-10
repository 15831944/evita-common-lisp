;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - Compiled RegEx Code Evaluator.
;;; lisp/regex/regx-eval.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-byte-exec.lisp#3 $
;;;
;;; Description:
;;;  This file contains regex execute engine.
;
(in-package :si)

;;;; regex-execute-byte-code
;;;
;;; BUGBUG: Since negative lookahead causes CHAR=.F at pos = string-end,
;;; CHAR=.F must check pos, e.g. "\\d+(?!\\.)"
;
(defun regex-execute-byte-code (match codevec)
    (declare (type regex-match match))
    (declare (values t))
  (multiple-value-bind (flags min-len scan-hint)
      (let ((regex (slot-value match 'regex)))
        (values (slot-value regex 'flags)
                (slot-value regex 'min-length)
                (slot-value regex 'scan-hint) ) )
  (let* ((string       (slot-value match 'string))
         (string-start (slot-value match 'start))
         (scan-start   (slot-value match 'position))
         (string-end   (slot-value match 'end))

         (start-vec    (slot-value match 'start-vector))
         (end-vec      (slot-value match 'end-vector))

         (context      (regex-get-context))
         (cstack       (slot-value context 'cstack))
         (vstack       (slot-value context 'vstack))

         (from-end          (plusp (logand flags REGEX-FLAG-SCAN-BACKWARD)))
         (scan-limit-pos    0)
         (loop-limit-pos    0)

         ;; Registers
         (pos           scan-start)
         (pc            0)
         (vsp           (length vstack))
         (csp           (length cstack))
         (cxp           csp) )
    (declare (type fixnum pos))
    (declare (type sequence-index scan-limit-pos))
    (declare (type sequence-index string-start string-end))
    (declare (type sequence-index pc))
    (declare (type sequence-index vsp))
    (declare (type sequence-index csp))
  (macrolet (
    ;; category=
    (category= ()
     '(eql (char-category ch) (svref codevec (+ pc 1))) )

    ;; category/=
    (category/= ()
     '(not (eql (char-category ch) (svref codevec (+ pc 1)))) )

    ;; category<=
    (category<= ()
     '(let ((min (svref codevec (+ pc 1)))
            (max (svref codevec (+ pc 2))) )
        (<= min (char-category ch) max) ) )

    ;; category>
    (category> ()
     '(let ((min (svref codevec (+ pc 1)))
            (max (svref codevec (+ pc 2))) )
        (not (<= min (char-category ch) max)) ) )

    ;; char-range<=
    (char-range<= ()
     '(let ((min (svref codevec (+ pc 1)))
            (max (svref codevec (+ pc 2))) )
        (char<= min ch max) ) )

    ;; char-range>
    (char-range> ()
     '(let ((min (svref codevec (+ pc 1)))
            (max (svref codevec (+ pc 2))) )
        (not (char<= min ch max)) ) )

    ;; char-range-LE
    (char-range-LE ()
     '(let ((min (svref codevec (+ pc 1)))
            (max (svref codevec (+ pc 2))) )
        (or (char<= min (char-upcase   ch) max)
            (char<= min (char-downcase ch) max) ) ) )

    ;; char-range-GT
    (char-range-GT ()
     '(let ((min (svref codevec (+ pc 1)))
            (max (svref codevec (+ pc 2))) )
        (and (not (char<= min (char-upcase   ch) max))
             (not (char<= min (char-downcase ch) max)) ) ) )

    ;; charset=
    (charset= ()
     '(let ((min (svref codevec (+ pc 1)))
            (max (svref codevec (+ pc 2)))
            (str (svref codevec (+ pc 3))) )
        (and (char<= min ch max) (find ch str)) ) )

    ;; charset/=
    (charset/= ()
     '(let ((min (svref codevec (+ pc 1)))
            (max (svref codevec (+ pc 2)))
            (str (svref codevec (+ pc 3))) )
        (not (and (char<= min ch max) (find ch str))) ) )

    ;; charset-eq
    (charset-eq ()
     '(let ((min (svref codevec (+ pc 1)))
            (max (svref codevec (+ pc 2)))
            (str (svref codevec (+ pc 3))) )
        (let ((ch (char-upcase ch)))
          (and (char<= min ch max) (find ch str)) ) ) )

    ;; charset-ne
    (charset-ne ()
     '(let ((min (svref codevec (+ pc 1)))
            (max (svref codevec (+ pc 2)))
            (str (svref codevec (+ pc 3))) )
        (let ((ch (char-upcase ch)))
          (not (and (char<= min ch max) (find ch str))) ) ) )

    ;; test-backward
    (test-backward (test-form &optional (opsize 2))
     `(progn
        (decf pos)
        (when (and (>= pos string-start)
                   (let ((ch (schar string pos))) ,test-form) )
          (incf pc ,opsize) ) ) )

    ;; test-forward
    (test-forward (test-form &optional (opsize 2))
     `(when (and (< pos string-end)
                 (let ((ch (schar string pos))) ,test-form) )
        (incf pos)
        (incf pc ,opsize) ) )

    ;; char-backward
    (char-backward (test-fn)
      `(test-backward (,test-fn (svref codevec (+ pc 1)) ch)) )

    ;; char-forward
    (char-forward (test-fn)
      `(test-forward (,test-fn (svref codevec (+ pc 1)) ch)) )

    ;; repeat-1-backward
    (repeat-1-backward (insn-size test-form)
     `(let* ((rest     (svref codevec (+ pc 1)))
             (max-pos  pos)
             (next-pos (+ string-start rest))
             (next-pc  (+ pc ,insn-size)) )
        (when (< next-pos pos)
          (loop while (> pos next-pos) do
            (let ((char (schar string (1- pos))))
              (unless ,test-form (return)) )
            (decf pos) )
          (unless (eql pos max-pos)
            (cstack-push-4 REGEX-CTRL-REPEAT.B next-pc pos max-pos)
            (setq pc next-pc) )) ) )

    ;; repeat-1-forward
    (repeat-1-forward (insn-size test-form)
     `(let* ((rest     (svref codevec (+ pc 1)))
             (min-pos  pos)
             (next-pos (- string-end rest))
             (next-pc  (+ pc ,insn-size)) )
        (when (> next-pos pos)
          (loop while (< pos next-pos) do
            (let ((char (schar string pos)))
              (unless ,test-form (return)) )
            (incf pos) )
          (unless (eql pos min-pos)
            (cstack-push-4 REGEX-CTRL-REPEAT.F next-pc pos min-pos)
            (setq pc next-pc) )) ) )
    )
  (labels (
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Execute byte-code-vector
    ;;

    ;; execute
    (execute (start)
        (declare (type sequence-index start))
        (declare (values (or (eql t) null)))
      (setq vsp (length vstack))
      (setq csp (- (length cstack) 2))  ; debug: -2: release: -1
        (assert (eql (svref cstack csp) REGEX-CTRL-FAIL))
      (setq cxp csp)
      (setq pc 0)
      (setq pos start)

      (REGEX-DEBUG "~%;~70~~%")

      (loop
        (when (eql (svref codevec pc) REGEX-OP-SUCCESS)
          (if from-end
              (progn
                (setf (svref start-vec 0) pos)
                (setf (svref end-vec   0) start)
                (when (eql pos start) (decf pos)) )
              (progn
                (setf (svref start-vec 0) start)
                (setf (svref end-vec   0) pos)
                (when (eql pos start) (incf pos)) ))

          (setf (slot-value match 'position) pos)

          (REGEX-DEBUG "; Match: pos=~D <~A> <~A> <~A>~%"
              start
              (let ((before (match-before match)))
                (subseq before (max (- (length before) 10) 0)) )
              (match-string match)
              (let ((after (match-after match)))
                (subseq after (min (length after) 10)) ))

          (return t) )
        (unless (execute-1)
          ;; We must reset captures for next scanning.
          ;; Note: When regex contains one of atomic grouping, if, or
          ;; lookaround, captures may not be restored.
          (fill start-vec nil)
          (fill end-vec   nil)
          (return nil) )) )

    ;; execute-1
    (execute-1 ()
      (loop
        (when (execute-1-aux) (return t))
        (loop

          (REGEX-DEBUG ";*** backtarck: csp=~D vsp=~D ~S opd[0]=~S~%"
              csp vsp
              (let ((op (svref cstack csp)))
                (svref #(capture continue fail
                         pop-int pop-pos
                         push-int push-pos
                         repeat.f repeat.b
                         save-csp sav-pos )
                  op ) )
              (svref cstack (1+ csp)) )

          (ecase (svref cstack csp)
            ((#.REGEX-CTRL-CAPTURE)    ; nth start end
              (let ((nth   (svref cstack (+ csp 1)))
                    (start (svref cstack (+ csp 2)))
                    (end   (svref cstack (+ csp 3))) )
                (setf (svref start-vec nth) start)
                (setf (svref end-vec   nth) end)
                (incf csp 4) ) )
            ((#.REGEX-CTRL-CONTINUE) ; next-pc pos
              (let ((next-pc  (svref cstack (+ csp 1)))
                    (last-pos (svref cstack (+ csp 2))) )
                (setq pc  next-pc)
                (setq pos last-pos)
                (incf csp 3)
                (return) ) )
            ((#.REGEX-CTRL-FAIL)
              (return-from execute-1 nil) )
            ((#.REGEX-CTRL-POP-INT #.REGEX-CTRL-POP-POS)
                (assert (< vsp (length vstack)))
              (incf vsp)
              (incf csp) )
            ((#.REGEX-CTRL-PUSH-INT) ; int
              (vstack-push-int (svref cstack (+ csp 1)))
              (incf csp 2) )
            ((#.REGEX-CTRL-PUSH-POS) ; pos
              (vstack-push-pos (svref cstack (+ csp 1)))
              (incf csp 2) )
            ((#.REGEX-CTRL-REPEAT.F) ; next-pc pos min-pos
              (let ((next-pc  (svref cstack (+ csp 1)))
                    (last-pos (svref cstack (+ csp 2)))
                    (min-pos  (svref cstack (+ csp 3))) )
                (REGEX-DEBUG "; BK: repeat.f: pos=~D/~D~%" pos min-pos)

                (decf last-pos)
                (if (eql last-pos min-pos)
                    (incf csp 4)
                  (progn
                    (setq pc  next-pc)
                    (setq pos last-pos)
                    (setf (svref cstack (+ csp 2)) pos)
                    (return) )) ) )
            ((#.REGEX-CTRL-REPEAT.B) ; next-pc pos max-pos
              (let ((next-pc  (svref cstack (+ csp 1)))
                    (last-pos (svref cstack (+ csp 2)))
                    (max-pos  (svref cstack (+ csp 3))) )
                (REGEX-DEBUG "; BK: repeat.b: pos=~D/~D~%" pos max-pos)
                (incf last-pos)
                (if (eql last-pos max-pos)
                    (incf csp 4)
                  (progn
                    (setq pc  next-pc)
                    (setq pos last-pos)
                    (setf (svref cstack (+ csp 2)) pos)
                    (return) )) ) )
            ((#.REGEX-CTRL-SAVE-CXP)   ; cxp vsp
              (setq vsp (svref cstack (+ csp 2)))
              (setq cxp (svref cstack (+ csp 1)))
              (incf csp 3) )
            ((#.REGEX-CTRL-SAVE-POS)   ; pos
              (setq pos (svref cstack (+ csp 1)))
              (incf csp 2) )))) )

    ;; execute-1-aux
    (execute-1-aux ()
      (REGEX-DEBUG "; <~A> <~A>~30T csp=~D vsp=~D pos=~D|~:C| ~3,'0D ~S~%"
          (subseq string (max (- pos 10) string-start) (max pos string-start))
          (subseq string (max pos string-start)  (min (+ pos 10) string-end))

          csp
          vsp
          pos
          (or (and (<= string-start pos (1- string-end)) (schar string pos))
              #\$ )
          pc
          (loop
            with opcode = (svref codevec pc)
            with info   = (svref *regex-instruction-vector* opcode)
            for i from (1+ pc) below (+ pc (svref info 0))
              collect (svref codevec i) into operands
            finally
              (return (cons (svref info 1) operands)) ))

      (ecase (svref codevec pc)
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; String Instructions
        ;;
        ((#.REGEX-OP-STRING=.F)
          (let* ((operand  (svref codevec (+ pc 1)))
                 (next-pos (+ pos (length operand))) )
            (when (and (<= next-pos string-end)
                       (string= string operand
                                :start1 pos
                                :end1   next-pos ))
              (setq pos next-pos)
              (incf pc 2) ) ) )

        ((#.REGEX-OP-STRING=.B)
          (let* ((operand  (svref codevec (+ pc 1)))
                 (next-pos (- pos (length operand))) )
            (when (and (>= next-pos string-start)
                       (string= string operand
                                :start1 next-pos
                                :end1   pos) )
              (setq pos next-pos)
              (incf pc 2) ) ) )

        ((#.REGEX-OP-STRING-EQUAL.F)
          (let* ((operand  (svref codevec (+ pc 1)))
                 (next-pos (+ pos (length operand))) )
            (when (and (<= next-pos string-end)
                       (string-equal string operand
                                :start1 pos
                                :end1   next-pos ))
              (setq pos next-pos)
              (incf pc 2) ) ) )

        ((#.REGEX-OP-STRING-EQUAL.B)
          (let* ((operand  (svref codevec (+ pc 1)))
                 (next-pos (- pos (length operand))) )
            (when (and (>= next-pos string-start)
                       (string-equal string operand
                                :start1 next-pos
                                :end1   pos ))
              (setq pos next-pos)
              (incf pc 2) ) ) )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Character Instructions
        ;;

        ;; CHAR-= ch
        ((#.REGEX-OP-CHAR=.B)   (char-backward char=))
        ((#.REGEX-OP-CHAR=.F)   (char-forward  char=))

        ((#.REGEX-OP-CHAR/=.B)  (char-backward char/=))
        ((#.REGEX-OP-CHAR/=.F)  (char-forward  char/=))

        ((#.REGEX-OP-CHAR-EQ.F) (char-forward  char-equal))
        ((#.REGEX-OP-CHAR-EQ.B) (char-backward char-equal))

        ((#.REGEX-OP-CHAR-NE.F) (char-forward  char-not-equal))
        ((#.REGEX-OP-CHAR-NE.B) (char-backward char-not-equal))

        ;; CHAR-RANGE min max
        ((#.REGEX-OP-CHAR<=.B)     (test-backward (char-range<=) 3))
        ((#.REGEX-OP-CHAR<=.F)     (test-forward  (char-range<=) 3))

        ((#.REGEX-OP-CHAR>.B)      (test-backward (char-range>) 3))
        ((#.REGEX-OP-CHAR>.F)      (test-forward  (char-range>) 3))

        ((#.REGEX-OP-CHAR-LE.B)    (test-backward (char-range-LE) 3))
        ((#.REGEX-OP-CHAR-LE.F)    (test-forward  (char-range-LE) 3))

        ((#.REGEX-OP-CHAR-GT.B)    (test-backward (char-range-GT) 3))
        ((#.REGEX-OP-CHAR-GT.F)    (test-forward  (char-range-GT) 3))

        ;; CHAARSET min max str
        ((#.REGEX-OP-CHARSET=.B)   (test-backward (charset=)   4))
        ((#.REGEX-OP-CHARSET=.F)   (test-forward  (charset=)   4))

        ((#.REGEX-OP-CHARSET/=.B)  (test-backward (charset/=)  4))
        ((#.REGEX-OP-CHARSET/=.F)  (test-forward  (charset/=)  4))

        ((#.REGEX-OP-CHARSET-EQ.B) (test-backward (charset-eq) 4))
        ((#.REGEX-OP-CHARSET-EQ.F) (test-forward  (charset-eq) 4))

        ((#.REGEX-OP-CHARSET-NE.B) (test-backward (charset-ne) 4))
        ((#.REGEX-OP-CHARSET-NE.F) (test-forward  (charset-ne) 4))

        ((#.REGEX-OP-ANY.F)
          (when (< pos string-end)
            (incf pos)
            (incf pc) ) )

        ((#.REGEX-OP-ANY.B)
          (decf pos)
          (when (>= pos string-start)
            (incf pc) ) )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Capture Instructions
        ;;
        ((#.REGEX-OP-CAPTURE.B) ; nth
          (let ((nth (svref codevec (+ pc 1)))
                (end (vstack-pop-pos)) )
            (cstack-push-2 REGEX-CTRL-PUSH-POS end)
            (cstack-push-4 REGEX-CTRL-CAPTURE
                nth (svref start-vec nth) (svref end-vec nth) )
            (setf (svref start-vec nth) pos)
            (setf (svref end-vec   nth) end)

        (REGEX-DEBUG "; set-capture[~D]=~S~%"
            nth (match-string match nth) )

            (incf pc 2) ) )

        ((#.REGEX-OP-CAPTURE.F) ; nth
          (let ((nth   (svref codevec (+ pc 1)))
                (start (vstack-pop-pos)) )
            (cstack-push-2 REGEX-CTRL-PUSH-POS start)
            (cstack-push-4 REGEX-CTRL-CAPTURE nth
              (svref start-vec nth) (svref end-vec nth) )
            (setf (svref start-vec nth) start)
            (setf (svref end-vec   nth)  pos)

            (REGEX-DEBUG "; set-capture[~D]=~S~%"
                nth (match-string match nth) )

            (incf pc 2) ) )

        ((#.REGEX-OP-CAPTURE-IF-NOT)   ; label nth
          (let ((branch-pc (svref codevec (+ pc 1)))
                (nth       (svref codevec (+ pc 2))) )
            (if (svref start-vec nth)
                (incf pc 3)
              (setq pc branch-pc) ) ) )

        ((#.REGEX-OP-CAPTURE=.B) ; nth
          (let* ((nth   (svref codevec (+ pc 1)))
                 (start (match-start match nth))
                 (end   (match-end   match nth)) )
            (when start
              (let ((next-pos (+ pos (- end start))))
                (when (and (>= next-pos string-start)
                           (string= string string
                                :start1 next-pos
                                :end1   pos
                                :start2 start
                                :end2   end ))
                  (setq pos next-pos)
                  (incf pc 2) ) )) ) )

        ((#.REGEX-OP-CAPTURE=.F) ; nth
          (let* ((nth   (svref codevec (+ pc 1)))
                 (start (match-start match nth))
                 (end   (match-end   match nth)) )
            (when start
              (let ((next-pos (+ pos (- end start))))
                (when (and (<= next-pos string-end)
                           (string= string string
                                :start1 pos
                                :end1   next-pos
                                :start2 start
                                :end2   end ))
                  (setq pos next-pos)
                  (incf pc 2) ) )) ) )

        ((#.REGEX-OP-CAPTURE-EQUAL.B) ; nth
          (let* ((nth   (svref codevec (+ pc 1)))
                 (start (match-start match nth))
                 (end   (match-end   match nth)) )
            (when start
              (let ((next-pos (+ pos (- end start))))
                (when (and (>= next-pos string-start)
                           (string-equal string string
                                :start1 next-pos
                                :end1   pos
                                :start2 start
                                :end2   end ))
                  (setq pos next-pos)
                  (incf pc 2) ) )) ) )

        ((#.REGEX-OP-CAPTURE-EQUAL.F) ; nth
          (let* ((nth   (svref codevec (+ pc 1)))
                 (start (match-start match nth))
                 (end   (match-end   match nth)) )
            (when start
              (let ((next-pos (+ pos (- end start))))
                (when (and (<= next-pos string-end)
                           (string-equal string string
                                :start1 pos
                                :end1   next-pos
                                :start2 start
                                :end2   end ))
                  (setq pos next-pos)
                  (incf pc 2) ) )) ) )

        ((#.REGEX-OP-CAPTURE-MAKUNBOUND) ; nth
          (let ((nth (svref codevec (+ pc 1))))

            (REGEX-DEBUG "; unbound-capture[~D] (was ~S)~%"
                nth (match-string match nth) )

            (cstack-push-4 REGEX-CTRL-CAPTURE
                nth (svref start-vec nth) (svref end-vec nth) )
            (setf (svref start-vec nth) nil)
            (setf (svref end-vec   nth) nil)
            (incf pc 2) ) )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Unicode Category instructions
        ;;
        ((#.REGEX-OP-CATEGORY=.B)
          (test-backward (category=)) )

        ((#.REGEX-OP-CATEGORY=.F)
          (test-forward (category=)) )

        ((#.REGEX-OP-CATEGORY/=.F)
          (test-forward (category/=)) )

        ((#.REGEX-OP-CATEGORY/=.B)
          (test-backward (category/=)) )

        ((#.REGEX-OP-CATEGORY<=.B)
          (test-backward (category<=) 3) )

        ((#.REGEX-OP-CATEGORY<=.F)
          (test-forward (category<=) 3) )

        ((#.REGEX-OP-CATEGORY>.B)
          (test-backward (category>) 3) )

        ((#.REGEX-OP-CATEGORY>.F)
          (test-forward (category>) 3) )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Repetation
        ;;
        ((#.REGEX-OP-GO) ; label
          (setq pc (svref codevec (+ pc 1))) )

        ((#.REGEX-OP-MAX) ; label max-min
          (let ((loop-pc (svref codevec (+ pc 1)))
                (max-min (svref codevec (+ pc 2)))
                (next-pc (+ pc 3))
                (counter (vstack-pop-int)) )
            (cstack-push-2 REGEX-CTRL-PUSH-INT counter)
            (incf counter)
            (cond
              ((eql counter max-min)
                (REGEX-DEBUG ";~20T matched: counter=~D~%" counter)
                (setq pc next-pc) )
              ((< counter 0)
                (REGEX-DEBUG ";~20T need more: counter=~D~%" counter)
                (cstack-push-1 REGEX-CTRL-POP-INT)
                (vstack-push-int counter)
                (setq pc loop-pc) )
              (t
                (REGEX-DEBUG ";~20T try more: counter=~D~%" counter)
                (CSTACK-PUSH-3 REGEX-CTRL-CONTINUE next-pc pos)
                (cstack-push-1 REGEX-CTRL-POP-INT)
                (vstack-push-int counter)
                (setq pc loop-pc) )) ) )

        ((#.REGEX-OP-MIN) ; label max-min
          (let ((loop-pc (svref codevec (+ pc 1)))
                (max-min (svref codevec (+ pc 2)))
                (next-pc (+ pc 3))
                (counter (vstack-pop-int)))
            (cstack-push-2 REGEX-CTRL-PUSH-INT counter)
            (incf counter)
            (cond
              ((eql counter max-min)
                (REGEX-DEBUG ";~20T matched: counter=~D~%" counter)
                (setq pc next-pc) )
              ((< counter 0)
                (REGEX-DEBUG ";~20T need more: counter=~D~%" counter)
                (cstack-push-1 REGEX-CTRL-POP-INT)
                (vstack-push-int counter)
                (setq pc loop-pc) )
              (t
                (assert (< counter max-min))
                (REGEX-DEBUG ";~20T fallthrough: counter=~D~%" counter)
                (CSTACK-PUSH-3 REGEX-CTRL-CONTINUE loop-pc pos)
                (cstack-push-2 REGEX-CTRL-PUSH-INT counter)
                (setq pc next-pc) )) ) )

        ((#.REGEX-OP-NULC) ; label
          (let ((exit-pc  (svref codevec (+ pc 1)))
                (next-pc (+ pc 2))
                (mark-pos (vstack-pop-pos)) )
            (cstack-push-2 REGEX-CTRL-PUSH-POS mark-pos)
            (if (not (eql pos mark-pos))
                (setq pc next-pc)
              (let ((counter (vstack-pop-int)))
                (cstack-push-2 REGEX-CTRL-PUSH-INT counter)
                (setq pc exit-pc) )) ) )

        ((#.REGEX-OP-NULL) ; label
          (let ((exit-pc  (svref codevec (+ pc 1)))
                (next-pc (+ pc 2))
                (mark-pos (vstack-pop-pos)) )
            (cstack-push-2 REGEX-CTRL-PUSH-POS mark-pos)
            (if (not (eql pos mark-pos))
                (setq pc next-pc)
              (setq pc exit-pc) ) ) )

        ((#.REGEX-OP-OR) ; label
          (let ((label-pc (svref codevec (+ pc 1))))
            (CSTACK-PUSH-3 REGEX-CTRL-CONTINUE label-pc pos)
            (incf pc 2) ) )

        ((#.REGEX-OP-PUSH) ; label
          (CSTACK-PUSH-3 REGEX-CTRL-CONTINUE (+ pc 2) pos)
          (setq pc (svref codevec (+ pc 1))) )

        ((#.REGEX-OP-PUSH-INT)  ; n
          (let ((val (svref codevec (+ pc 1))))
            (cstack-push-1 #.REGEX-CTRL-POP-INT)
            (vstack-push-int val)
            (incf pc 2) ) )

        ((#.REGEX-OP-PUSH-POS)
          (cstack-push-1 #.REGEX-CTRL-POP-POS)
          (vstack-push-pos pos)
          (incf pc) )

        ((#.REGEX-OP-LAST.F) ; min-rest
          (let* ((min-rest (svref codevec (+ pc 1)))
                 (next-pos (+ pos min-rest)) )
            (when (<= next-pos string-end)
              (cond
                ((>= next-pos loop-limit-pos)
 (REGEX-DEBUG "; We've already checked ~D/~D.~%~50~~%" next-pos loop-limit-pos)
                  nil )
                ((eql (1+ next-pos) loop-limit-pos)
 (REGEX-DEBUG "; We won't check after ~D/~D.~%~50~~%" next-pos loop-limit-pos)
                  (setq loop-limit-pos next-pos)
                  (incf pc 2) )
                (t 
                  (incf pc 2) ))) ) )

        ((#.REGEX-OP-LAST.B) ; min-rest
          (let* ((min-rest (svref codevec (+ pc 1)))
                 (next-pos (- pos min-rest)) )
            (when (>= next-pos string-start)
              (cond
                ((<= next-pos loop-limit-pos)
                  nil )
                ((eql (1- next-pos) loop-limit-pos)
                  (setq loop-limit-pos next-pos)
                  (incf pc 2) )
                (t 
                  (incf pc 2) ))) ) )

        ((#.REGEX-OP-REST.F) ; rest
          (let* ((rest     (svref codevec (+ pc 1)))
                 (next-pos (+ pos rest)) )
            (when (and (<= next-pos string-end) (< next-pos loop-limit-pos))
              (incf pc 2) ) ) )

        ((#.REGEX-OP-REST.B) ; rest
          (let* ((rest     (svref codevec (+ pc 1)))
                 (next-pos (- pos rest)) )
            (when (and (>= next-pos string-start) (> next-pos loop-limit-pos))
              (incf pc 2) ) ) )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Simple Repetation Instructions
        ;;
        ((#.REGEX-OP-REPEAT-ANY.F)   ; rest
          (let* ((rest (svref codevec (+ pc 1)))
                 (next-pos (- string-end rest))
                 (next-pc  (+ pc 2)) )
            (when (> next-pos pos)
              (let ((min-pos pos))
                (setq pos next-pos)
                (cstack-push-4 REGEX-CTRL-REPEAT.F next-pc pos min-pos)
                (setq pc next-pc) )) ) )

        ((#.REGEX-OP-REPEAT-ANY.B)   ; rest
          (let* ((rest (svref codevec (+ pc 1)))
                 (next-pos (+ string-start rest))
                 (next-pc  (+ pc 2)) )
            (when (< next-pos pos)
              (let ((max-pos pos))
                (setq pos next-pos)
                (cstack-push-4 REGEX-CTRL-REPEAT.B next-pc pos max-pos)
                (setq pc next-pc) )) ) )

        ((#.REGEX-OP-REPEAT-CHAR=.F)   ; rest char
          (let ((operand (svref codevec (+ pc 2))))
            (repeat-1-forward 3 (char= operand char)) ) )

        ((#.REGEX-OP-REPEAT-CHAR=.B)   ; rest char
          (let ((operand (svref codevec (+ pc 2))))
            (repeat-1-backward 3 (char= operand char)) ) )

        ((#.REGEX-OP-REPEAT-CHAR/=.F)   ; rest char
          (let ((operand (svref codevec (+ pc 2))))
            (repeat-1-forward 3 (char/= operand char)) ) )

        ((#.REGEX-OP-REPEAT-CHAR/=.B)   ; rest char
          (let ((operand (svref codevec (+ pc 2))))
            (repeat-1-backward 3 (char/= operand char)) ) )

        ((#.REGEX-OP-REPEAT-CHAR<=.F) ; rest min-char max-char
          (let ((min-char (svref codevec (+ pc 2)))
                (max-char (svref codevec (+ pc 3))) )
            (repeat-1-forward 4 (char<= min-char char max-char)) ) )

        ((#.REGEX-OP-REPEAT-CHAR<=.B) ; rest min-char max-char
          (let ((min-char (svref codevec (+ pc 2)))
                (max-char (svref codevec (+ pc 3))) )
            (repeat-1-backward 4 (char<= min-char char max-char)) ) )

        ((#.REGEX-OP-REPEAT-CHAR-EQ.F)   ; rest char
          (let ((operand (svref codevec (+ pc 2))))
            (repeat-1-forward 3 (char-equal operand char)) ) )

        ((#.REGEX-OP-REPEAT-CHAR-EQ.B)   ; rest char
          (let ((operand (svref codevec (+ pc 2))))
            (repeat-1-backward 3 (char-equal operand char)) ) )

        ((#.REGEX-OP-REPEAT-CHAR-NE.F)   ; rest char
          (let ((operand (svref codevec (+ pc 2))))
            (repeat-1-forward 3 (char-not-equal operand char)) ) )

        ((#.REGEX-OP-REPEAT-CHAR-NE.B)   ; rest char
          (let ((operand (svref codevec (+ pc 2))))
            (repeat-1-backward 3 (char-not-equal operand char)) ) )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Misc
        ;;
        ((#.REGEX-OP-FAIL)
          (REGEX-DEBUG "FAIL!~%")
          nil )

        ((#.REGEX-OP-RESTORE-CXP)
            (assert (eql (svref cstack cxp) REGEX-CTRL-SAVE-CXP))
          (setq vsp (svref cstack (+ cxp 2)))
          (setq csp (+ cxp 3))
          (setq cxp (svref cstack (+ cxp 1)))
          (incf pc) )

        ((#.REGEX-OP-RESTORE-POS)
          (setq pos (vstack-pop-pos))
          (cstack-push-2 REGEX-CTRL-SAVE-POS pos)
          (incf pc) )

        ((#.REGEX-OP-SAVE-CXP)
          (cstack-push-3 REGEX-CTRL-SAVE-CXP cxp vsp)
          (setq cxp csp)
          (incf pc) )

        ((#.REGEX-OP-SAVE-POS)
          (vstack-push-pos pos)
          (incf pc) )

        ((#.REGEX-OP-SUCCESS)
          (REGEX-DEBUG ";*** SUCCESS!~%")
          t )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Zero-Width Assertions
        ;;
        ((#.REGEX-OP-BOUNDARY=)
          (when (boundary-p pos) (incf pc)) )

        ((#.REGEX-OP-BOUNDARY/=)
          (unless (boundary-p pos) (incf pc)) )

        ((#.REGEX-OP-BOS)
          (when (eql pos string-start) (incf pc)) )

        ((#.REGEX-OP-EOS)
          (when (eql pos string-end) (incf pc)) )

        ((#.REGEX-OP-MBOL)
          ;; Matches
          ;;    o at start of string
          ;;    o after any newline
          (when (or (eql pos string-start)
                    (and (not (eql pos string-end))
                         (eql (schar string (1- pos)) #\Newline) ))
            (incf pc) ) )

        ((#.REGEX-OP-MEOL.F)
          ;; Matches before any newline
          (when (or (eql pos string-end)
                    (eql (schar string pos) #\Newline) )
            (incf pc) ) )

        ((#.REGEX-OP-POS.F)
          (when (eql pos scan-start) (incf pc)) )

        ((#.REGEX-OP-POS.B)
          (when (eql pos scan-start) (incf pc)) )

        ((#.REGEX-OP-SEOL.F)
          (when (or (eql pos string-end)
                    (and (eql (1+ pos) string-end)
                         (eql (schar string pos) #\Newline) ))
            (incf pc) ) )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; Deprecated Instructions
        ;;
        ((#.REGEX-OP-SPACE=.B)
          (test-backward (regex-space-char-p ch) 1) )

        ((#.REGEX-OP-SPACE=.F)
          (test-forward (regex-space-char-p ch) 1) )

        ((#.REGEX-OP-SPACE/=.B)
          (test-backward (not (regex-space-char-p ch)) 1) )

        ((#.REGEX-OP-SPACE/=.F)
          (test-forward (not (regex-space-char-p ch)) 1) )

        ((#.REGEX-OP-WORD=.B)
          (test-backward (regex-word-char-p ch) 1) )

        ((#.REGEX-OP-WORD=.F)
          (test-forward (regex-word-char-p ch) 1) )

        ((#.REGEX-OP-WORD/=.B)
          (test-backward (not (regex-word-char-p ch)) 1) )

        ((#.REGEX-OP-WORD/=.F)
          (test-forward (not (regex-word-char-p ch)) 1) )

          ) ) ; execute-1-aux

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Scanners
    ;;

    ;; scan-fun.b
    ;;
    ;; For pattern is /ABC...*/ and subject is "ABCDE..."
    ;;  string-start    A
    ;;                  B
    ;;  scan-min        C       string=.b "CBA"
    ;;                  D       any.b
    ;;  scan-limit-pos       E       any.b
    ;;                  ...
    ;;
    ;; BUGBUG: SCAN-FUN.B: We should pick bump up from bad-char-skip vector.
    (scan-fun.b ()
      (loop
        with scanner = (svref scan-hint 0)
        with pat-len = (svref scan-hint 1)
        with scan-min = (- scan-limit-pos pat-len)
        with pos = scan-start
        while (>= pos scan-min) do
        (let ((start (funcall scanner string scan-min pos)))
          (when (null start) (return nil))
          (when (execute start) (return t))
          (decf pos) )) )

    ;; scan-fun.f
    ;; BUGBUG: SCAN-FUN.F: We should pick bump up from bad-char-skip vector.
    ;; See pcre-2/300.
    (scan-fun.f ()
      (loop
        with scanner = (svref scan-hint 0)
        with scan-max = scan-limit-pos
        with pos = scan-start
        while (<= pos scan-max) do
        (let ((start (funcall scanner string pos string-end)))
          (when (null start) (return nil))
          (when (execute start) (return t))
          (incf pos) )) )

    ;; scan-mbol.f
    ;;  Tries at each start of line (= next of newline) or
    ;;  start of string.
    (scan-mbol.f ()
      (block scanner
        (let ((pos scan-start))
          (when (eql pos string-start)
            (when (execute pos) (return-from scanner t))
            (incf pos) )

          (loop for start from pos below string-end do
            (when (eql (schar string (1- start)) #\Newline)
              (when (execute start) (return-from scanner t)) ) ) ) ) )

    ;; scan-mbol.b
    ;;  1. Match at string-start
    ;;  2. Match at at end when string[end-1] = \n
    (scan-mbol.b ()
      (block scanner
        (when (eql scan-start string-start)
          (when (execute scan-start) (return-from scanner t)) )

        (loop for end from scan-start above string-start do
          (when (eql (schar string (1- end)) #\Newline)
            (when (execute end) (return-from scanner t)) ))) )

    ;; scan-meol.f
    ;;  1. Match at string-end
    ;;  2. Match at pos wher string[pos] = \n
    (scan-meol.f ()
      (block scanner
        (when (eql scan-start string-end)
          (when (execute scan-start) (return-from scanner t)) )

        (loop for pos from scan-start below string-end do
          (when (eql (schar string pos) #\Newline)
            (when (execute pos) (return-from scanner t)) ))) )

    ;; scan-meol.b
    ;;  1. Match at string-end
    ;;  2. Match at pos when string[pos] = \n
    (scan-meol.b ()
      (block scanner
        (when (eql scan-start string-end)
          (when (execute scan-start) (return-from scanner t)) )

        (loop for end from scan-start above string-start do
          (when (eql (schar string (1- end)) #\Newline)
            (when (execute end) (return-from scanner t)) ))) )

    ;; scan-seol.f
    ;;  1. Match at string-end
    ;;  2. Match at string-end - 1 when string(string-end - 1) = \n
    (scan-seol.f ()
      (cond
        ((execute string-end) t)
        ((eql (schar string (1- string-end)) #\Newline)
          (execute (1- string-end)) )) )

    ;; scan-seol.b
    ;;  1. Match at string-end
    (scan-seol.b ()
      (when (eql scan-start string-end)
        (execute scan-start) ) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Stack Operations
    ;;
    ;; cstack-push-1
    (cstack-push-1 (op)
      (cstack-take 1)
      (setf (svref cstack (+ csp 0)) op) )

    ;; cstack-push-2
    (cstack-push-2 (op arg-1)
      (cstack-take 2)
      (setf (svref cstack (+ csp 0)) op)
      (setf (svref cstack (+ csp 1)) arg-1) )

    ;; cstack-push-3
    (cstack-push-3 (op arg-1 arg-2)
      (cstack-take 3)
      (setf (svref cstack (+ csp 0)) op)
      (setf (svref cstack (+ csp 1)) arg-1)
      (setf (svref cstack (+ csp 2)) arg-2) )

    ;; cstack-push-4
    (cstack-push-4 (op arg-1 arg-2 arg-3)
      (cstack-take 4)
      (setf (svref cstack (+ csp 0)) op)
      (setf (svref cstack (+ csp 1)) arg-1)
      (setf (svref cstack (+ csp 2)) arg-2)
      (setf (svref cstack (+ csp 3)) arg-3) )

    ;; cstack-take
    (cstack-take (n)
      (when (< csp n)
        (let* ((len       (length cstack))
               (new-len   (+ len len))
               (new-cstack (make-array new-len))
               (new-csp   (+ len csp)) )
          (replace new-cstack cstack
            :start1 new-csp
            :start2 csp )
          (setf (slot-value context 'cstack) new-cstack)
          (setq cstack new-cstack)
          (setq csp new-csp) ))
      (decf csp n) )

    ;; vstack-pop
    (vstack-pop-int ()
      (prog1
          (svref vstack vsp)
        (incf vsp) ) )

    ;; vstack-pop-pos
    (vstack-pop-pos ()
      (prog1
          (svref vstack vsp)
          #+nil
          (second (svref vstack vsp))
        (incf vsp) ) )

    ;; vstack-push-int
    (vstack-push-int (v)
      ;; BUGBUG: We should enlarge vstack.
      (when (zerop vsp) (error 'regex-too-deep-recursion))
      (decf vsp)
      (setf (svref vstack vsp) v) )

    ;; vstack-push-pos
    (vstack-push-pos (v)
      ;; BUGBUG: We should enlarge vstack.
      (when (zerop vsp) (error 'regex-too-deep-recursion))
      (decf vsp)
      (setf (svref vstack vsp) v)
      #+nil (setf (svref vstack vsp) `(pos ,v)) )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Utility Functions
    ;;

    ;; boundary-p
    (boundary-p (pos)
      (cond
        ((eql pos string-start)
          (and (not (eql pos string-end))
               (regex-word-char-p (schar string pos)) ) )
        ((eql pos string-end)
          (regex-word-char-p (schar string (1- pos))) )
        ((regex-word-char-p (schar string (1- pos)))
          (not (regex-word-char-p (schar string pos))) )
        (t
          (regex-word-char-p (schar string pos)) )) )
    )
    ;;
    ;; regex-execute
    ;;
    (fill start-vec nil)
    (fill end-vec   nil)

 (REGEX-DEBUG "; string-start=~D string-end=~D min-len=~D~%"
    string-start string-end min-len )

    (unless (<= string-start pos string-end)
      (return-from regex-execute-byte-code nil) )

    (if from-end
        (unless (<= string-start (- pos min-len))
          (return-from regex-execute-byte-code nil) )
        (unless (<= (+ pos min-len) string-end)
          (return-from regex-execute-byte-code nil) ))

    (setf (svref cstack (- (length cstack) 1)) REGEX-CTRL-FAIL)
    (setf (svref cstack (- (length cstack) 2)) REGEX-CTRL-FAIL)

    (if from-end
        (progn
          (setq scan-limit-pos (+ string-start min-len))
          (setq loop-limit-pos (1- string-start)) )
        (progn
          (setq scan-limit-pos (- string-end min-len))
          (setq loop-limit-pos (1+ string-end)) ))

    (block match-attempt
      (ecase (logand flags REGEX-FLAG-SCAN-MASK)
        ((#.REGEX-FLAG-SCAN-ALL.F)
          (loop for pos from scan-start to scan-limit-pos do
            (when (execute pos) (return t)) ) )

        ((#.REGEX-FLAG-SCAN-ALL.B)
          (loop for pos from scan-start downto scan-limit-pos do
            (when (execute pos) (return t)) ) )

        ((#.REGEX-FLAG-SCAN-BOS.F)
          (when (eql scan-start string-start) (execute string-start)) )

        ((#.REGEX-FLAG-SCAN-BOS.B)
          (when (>= scan-start string-start) (execute string-start)) )

        ((#.REGEX-FLAG-SCAN-EOS.F)
          (when (<= scan-start string-end) (execute string-end)) )

        ((#.REGEX-FLAG-SCAN-EOS.B)
          (when (eql scan-start string-end) (execute string-end)) )

        ((#.REGEX-FLAG-SCAN-FUN.F)  (scan-fun.f))
        ((#.REGEX-FLAG-SCAN-FUN.B)  (scan-fun.b))

        ((#.REGEX-FLAG-SCAN-POS.F)
          (execute scan-start) )

        ((#.REGEX-FLAG-SCAN-POS.B)
          (execute scan-start) )

        ((#.REGEX-FLAG-SCAN-MBOL.F) (scan-mbol.f))
        ((#.REGEX-FLAG-SCAN-MBOL.B) (scan-mbol.b))

        ((#.REGEX-FLAG-SCAN-MEOL.F) (scan-meol.f))
        ((#.REGEX-FLAG-SCAN-MEOL.B) (scan-meol.b))

        ((#.REGEX-FLAG-SCAN-SEOL.F) (scan-seol.f))
        ((#.REGEX-FLAG-SCAN-SEOL.B) (scan-seol.b)) )) ) ) ) ) )
