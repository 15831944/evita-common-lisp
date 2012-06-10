;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: EXTENSION; Base: 10 -*-
;;;;
;;;; regex - Unicode Category Table
;;; lisp/regex/regx-unicode-category.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-unicode-category.lisp#1 $
;;;
;;; Description:
;;;  This file contains initialization form of *rege-xunicode-category-table*.
;
(in-package :si)

(setq *regex-unicode-category-table*
  (loop
    with htb  = (make-hash-table :test 'equal)
    with min  = 0
    with max  = 0
    with last = #\C
    for (name value) in `(
            ("Cn" ,unicode:category-unassigned)
            ("Cc" ,unicode:category-control)
            ("Cf" ,unicode:category-format)
            ("Co" ,unicode:category-private-use)
            ("Cs" ,unicode:category-surrogate)

            ("Ll" ,unicode:category-lowercase-letter)
            ("Lm" ,unicode:category-modifier-letter)
            ("Lo" ,unicode:category-other-letter)
            ("Lt" ,unicode:category-titlecase-letter)
            ("Lu" ,unicode:category-uppercase-letter)

            ("Mc" ,unicode:category-combining-spacing-mark)
            ("Me" ,unicode:category-enclosing-mark)
            ("Mn" ,unicode:category-non-spacing-mark)

            ("Nd" ,unicode:category-decimal-digit-number)
            ("Nl" ,unicode:category-letter-number)
            ("No" ,unicode:category-other-number)

            ("Pc" ,unicode:category-connector-punctuation)
            ("Pd" ,unicode:category-dash-punctuation)
            ("Pe" ,unicode:category-close-punctuation)
            ("Pf" ,unicode:category-final-punctuation)
            ("Pi" ,unicode:category-initial-punctuation)
            ("Po" ,unicode:category-open-punctuation)
            ("Ps" ,unicode:category-other-punctuation)

            ("Sc" ,unicode:category-currency-symbol)
            ("Sk" ,unicode:category-modifier-symbol)
            ("Sm" ,unicode:category-math-symbol)
            ("So" ,unicode:category-other-symbol)

            ("Zl" ,unicode:category-line-separator)
            ("Zp" ,unicode:category-paragraph-separator)
            ("Zs" ,unicode:category-space-separator) )
    for normalized = (string-upcase name) do
      (setf (gethash normalized htb) `(category ,value))
      (if (char= (schar name 0) last)
          (progn
            (setq min (min min value))
            (setq max (max max value)) )
        (progn
          (setf (gethash (string last) htb) `(category ,min ,max))
          (setq min value)
          (setq max value)
          (setq last (schar name 0)) ))
    finally
      (assert (and (eql (+ unicode:category-lowercase-letter 1)
                        unicode:category-titlecase-letter )
                   (eql (+ unicode:category-lowercase-letter 2)
                        unicode:category-uppercase-letter )))
      (setf (gethash "L&" htb)
        `(category ,unicode:category-lowercase-letter
                   ,unicode:category-uppercase-letter ))
      (return htb) ) )
