;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;; evcl - devel - 25 Environment - Apropos
;;; devel/d25-apropos.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-apropos.lisp#3 $
;;;
;;; Description:
;;;  This file contains apropos functions.
;;;     apropos         25.2.5
;;;     apropos-list    25.2.5
;
(in-package :devel)

;;;; 25.2.5 apropos
(defun cl:apropos (string &optional package)
  (let* ((symbols
           (let ((symbols (apropos-list string package)))
             (when (null symbols)
               (format t "There is no symbol that contains ~S.~%" string)
               (return-from apropos (values)) )
             (sort symbols #'string< :key #'symbol-name) ) )
         (width
           (1+ (loop for symbol in symbols
                 maximize (length (prin1-to-string symbol)) )) )
         (terpri (when (> width 20) (setq width 4) 1)) )
    (let ((*print-pretty* t)
          (*print-circle* t) )
      (loop for symbol in symbols do
        (unless (or (boundp symbol) (fboundp symbol))
          (write-char #\Space) )

        (prin1 symbol)

        (when (boundp symbol)
          (format t "~V% ~VTvalue: ~W~%"
                  terpri width (symbol-value symbol )) )

        (cond
          ((macro-function symbol)
            (format t "~V%~VTMacro~%" terpri width) )

          ((fboundp symbol)
            (format t "~V%~VT~S~%"
                    terpri width (symbol-function symbol) ) ))

        (fresh-line) )

      (values) ) ) )


;;;; 25.2.5 apropos-list
(defun cl:apropos-list (string &optional package)
  (let ((packages
          (if (not package)
              (list-all-packages)
            (list (si::ensure-package package)) ) )
        (hash-table (make-hash-table :test #'eq))
        (string-u (string-upcase   string))
        (string-d (string-downcase string)) )
    (with-package-iterator (next packages :external :internal)
      (loop
        (multiple-value-bind (more? symbol) (next)
          (unless more? (return))
          (when (or (search string-u (symbol-name symbol))
                    (search string-d (symbol-name symbol)) )
            (setf (gethash symbol hash-table) t) ) )) )
    (loop for symbol being each hash-key of hash-table
      collect symbol ) ) )
