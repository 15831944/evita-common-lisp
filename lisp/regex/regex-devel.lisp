;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - compile
;;; lisp/regex/regx-compile.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-devel.lisp#2 $
;;;
;;; Description:
;;;  This file contains regex compiler.
;
(in-package :si)

;;;; describe-object regex
;
(defmethod describe-object ((o regex) s)
    (declare (type stream s))
    (declare (values regex))
  (labels (
    ;; describe-flags
    (describe-flags (flags)
      (terpri s)

      (unless (zerop (logand flags REGEX-FLAG-SCAN-BACKWARD))
        (format s "It is compiled for backward searching.~%") )

      (format s "Scanner will try ~A.~%"
        (ecase (logand flags REGEX-FLAG-SCAN-MASK)
          ((#.REGEX-FLAG-SCAN-ALL.F) "every characters from start to end")
          ((#.REGEX-FLAG-SCAN-ALL.B) "every characters from end to start")
          ((#.REGEX-FLAG-SCAN-BOS.F)  "once at start of string")
          ((#.REGEX-FLAG-SCAN-BOS.B)  "once at start of string")
          ((#.REGEX-FLAG-SCAN-EOS.F)  "once at end of string")
          ((#.REGEX-FLAG-SCAN-EOS.B)  "once at end of string")
          ((#.REGEX-FLAG-SCAN-FUN.F) "every characters from start to end")
          ((#.REGEX-FLAG-SCAN-FUN.B) "every characters from end to start")
          ((#.REGEX-FLAG-SCAN-MBOL.F) "at every start line to end")
          ((#.REGEX-FLAG-SCAN-MBOL.B) "at every start line to start")
          ((#.REGEX-FLAG-SCAN-MEOL.F) "at every end line to start")
          ((#.REGEX-FLAG-SCAN-MEOL.B) "at every end line to start")
          ((#.REGEX-FLAG-SCAN-POS.F) "once at end of last match")
          ((#.REGEX-FLAG-SCAN-POS.B) "once at start of last match")
          ((#.REGEX-FLAG-SCAN-SEOL.F) "once at end of line")
          ((#.REGEX-FLAG-SCAN-SEOL.B) "once at end of line") ))

      (unless (zerop (logand flags REGEX-FLAG-SCAN-ONCE))
        (format s "Second match won't success.~%") ) )

    ;; describe-scan-hint
    (describe-scan-hint (scan-hint)
      (when (null scan-hint) (return-from describe-scan-hint))
      (format s "Scanner will use specialized scanner for~_ ~S at offset ~D.~%"
        (svref scan-hint 3)
        (svref scan-hint 2) ) )

    ;; disasm
    (disasm (codevec)
        (declare (type simple-vector codevec))
      (loop
        with labels = (make-label-list codevec)
        with end of-type sequence-index = (length codevec)
        with pc  of-type sequence-index = 0
        while (< pc end) do
          (if (not (eql (first labels) pc))
              (write-char #\Space)
            (progn
              (pop labels)
              (write-char #\L) ))
          (incf pc (disasm-1 codevec pc))
          (terpri) ) )

    ;; disasm-1
    (disasm-1 (codevec pc)
        (declare (type simple-vector codevec))
        (declare (type sequence-index pc))
        (declare (values sequence-index))
      (let* ((opcode   (svref codevec pc))
             (info     (svref *regex-instruction-vector* opcode))
             (size     (svref info 0))
             (mnemonic (svref info 1))
             (formats  (svref info 2)) )
        (format s "~3,'0D ~A" pc mnemonic)
        (disasm-operands codevec pc formats)
        size ) )

    ;; disasm-operands
    (disasm-operands (codevec pc formats)
        (declare (type simple-vector codevec))
        (declare (type sequence-index pc))
      (when (null formats) (return-from disasm-operands))

      (format t "~20T")

      (dolist (format formats)
        (incf pc)
        (let ((operand (svref codevec pc)))
          (case format
            ((-))
            ((bitvec)
              (let* ((min    (svref codevec (- pc 1)))
                     (bitvec operand)
                     (max    (+ min (length bitvec) -1)) )
                (format s " [~:C, ~:C] ~S"
                  (code-char min) (code-char max)
                  (loop for index below (length bitvec)
                    unless (zerop (sbit bitvec index))
                      collect (code-char (+ index min)) )) ) )
            ((char string)
              (format t " ~S" operand) )
            ((code-min))
            ((label)
              (format t " L~3,'0D" operand) )
            ((max)
              (format t " max=~D"
                (if (eql operand regex-infinity) '* operand) ) )
            (otherwise
              (format t " ~(~A~)=~S" format operand) )) ) ) )

    ;; make-label-list
    (make-label-list (codevec)
        (declare (type simple-vector codevec))
      (loop
        with end of-type sequence-index = (length codevec)
        with pc  of-type sequence-index = 0
        with labels = '()
        while (< pc end) do
          (let* ((opcode (svref codevec pc))
                 (info     (svref *regex-instruction-vector* opcode))
                 (size     (svref info 0))
                 ;(mnemonic (svref info 1))
                 (formats  (svref info 2)) )
            (when (eq (first formats) 'label)
              (let ((label (svref codevec (+ pc 1))))
                (pushnew label labels) ))
            (incf pc size) )
        finally
          (return (sort labels #'<)) ) )
    )
    ;;
    ;; describe-object
    ;;
    (if (plusp (length (slot-value o 'codevec)))
        (format s "~S is~_ byte-compiled form of regular expression.~%"
            o )
        (format s "~S is~_ compiled form of regular expression.~%"
            o ) )

    (multiple-value-bind (min max)
        (regex-study-length (parse-regex (slot-value o 'source)))
      (terpri s)
      (cond
        ((eql min max)
          (format s "It matches ~D character~:P." min) )
        ((eql max regex-infinity)
          (format s "It matches at least ~D character~:P." min) )
        (t
          (format s "It matches ~D to ~D characters." min max) ))
      (terpri s) )

    (let ((names (regex-group-names o)))
      (when names
        (format s "~%It has ~R capture~:P:~%" (length names))

        (loop
          for name in names
          for nth = 0 then (1+ nth) do
            (format s "  ~D: ~:[(anonymous)~;~A~]~%"
                nth
                (keywordp name)
                name ))) )

    (describe-flags (slot-value o 'flags))
    (describe-scan-hint (slot-value o 'scan-hint))

    (let ((codevec (slot-value o 'codevec)))
      (when codevec
        (format s "~%Compiled form:~%")
        (disasm codevec) ) )
    o ) )


;;;; print-object regex
;
(defmethod cl:print-object ((re regex) s)
    (declare (type stream s))
    (declare (values regex))
  (labels (
    ;; print-source
    (print-source (source mods s)
        (declare (type string source))
      (let ((escape-alist '(
        (#.(code-char #x07) . #\a)
        ; (#.(code-char #x08) . #\b)    since only in cc.
        (#.(code-char #x09) . #\t)
        (#.(code-char #x0A) . #\n)
        (#.(code-char #x0C) . #\v)
        (#.(code-char #x0D) . #\r) ) ))
      (multiple-value-bind (source start end) (string-data source)
        (loop
           initially (write-char #\/ s)
           finally   (write-char #\/ s)
           for pos from 0 below end
           for char = (schar source pos) do
             (when (> (- pos start) 20)
                (write-string " .." s)
                (loop-finish) )
             (cond
               ((eql char #\/) (write-string "[/]" s))
               ((let ((escape (cdr (assoc char escape-alist))))
                 (when escape
                   (write-char #\\ s)
                   (write-char escape s) ) ) )
               ((<= unicode:category-control-min
                    (char-category char)
                    unicode:category-control-max )
                 (format s "\\u~4,'0X" (char-code char)) )
               (t (write-char char s)) )) )

      (unless (zerop (logand mods REGEX-MOD-IGNORE-CASE))
        (write-char #\i s) )

      (unless (zerop (logand mods REGEX-MOD-MULTILINE))
        (write-char #\m s) )

      (unless (zerop (logand mods REGEX-MOD-SINGLE-LINE))
        (write-char #\s s) )

      (unless (zerop (logand mods REGEX-MOD-UNICODE))
        (write-char #\u s) )

      (unless (zerop (logand mods REGEX-MOD-IGNORE-SPACE))
        (write-char #\x s) ) ) )
    )
    ;;
    (print-unreadable-object (re s :type t :identity t)
      (let ((source
              (and (slot-boundp re 'source) (slot-value re 'source)) )
            (mods
              (and (slot-boundp re 'modifiers) (slot-value re 'modifiers)) ))
        (when (and (stringp source) (integerp mods))
          (print-source source mods s) ) )
      re ) ) )


;;;; print-object regex-match
;
(defmethod cl:print-object ((m regex-match) s)
    (declare (type stream s))
    (declare (values regex-match))
  (labels (
    (show-result (m s)
      (let ((matched (match-string m)))
        (cond
          ((null matched)
            (write-string "Not-Matched" s) )
          ((= (length matched) 0)
            (format s "Empty-Match at ~D/~D"
                (match-start m)
                (match-target-end m) ) )
          ((<= (length matched) 20)
            (format s "Matched \"~A\" at ~D/~D"
                matched
                (match-start m)
                (match-target-end m) ) )
          (t
            (format s "Matched \"~A ..\" at ~D/~D"
              (subseq matched 0 18)
              (match-start m)
              (match-target-end m) ) )) ) )
    )
    ;;
    (print-unreadable-object (m s :type t :identity t)
      (when (slot-boundp m 'string)
        (show-result m s) )
      m ) ) )
