;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: EXTENSION; Base: 10 -*-
;;;;
;;;; regex - sample - grep
;;; lisp/regex/test/grep.lisp
;;;
;;; This file is NOT part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: /proj/evcl/lisp/regex/test/grep.lisp 18 2006-07-13 15:01:00 yosi $
;;;
;;; Description:
;;;  This file contains implementation of simple grep using Regex facility.
;
(in-package :cl-user)

(defun grep (pattern &rest names)
  (labels (
    ;; grep-aux
    (grep-aux (match name stream)
      (loop
        with line-count = 0
        with match-count = 0
        for line = (read-line stream nil)
        while line do
          (incf line-count)
          (when (ext:first-match match)
            (format t "~A(~D): ~A~%" name line-count line)
            (incf match-count) )
        finally
          (return match-count) ))
    )
    ;;
    (let ((match (ext:eval-regex pattern ""))
          (count   0) )
      (when (null names) (push "-" names))
      (dolist (name names count)
        (if (string= name "-")
            (incf count (grep-aux match name *standard-input*))
          (with-open-file (stream name :if-not-exists nil)
            (if (null stream)
                (format *standard-error* "grep: No such file: ~A" name)
              (incf count (grep-aux match name stream)) ) )) ) ) ) )
