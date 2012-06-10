;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Compiler; Base: 10 -*-
;;;;
;;;; evcl - macro - 11 Packages
;;; macro/m11-packages.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m11-package.lisp#5 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     defpackage              11.2.19
;;;     do-all-symbols          11.2.20
;;;     do-external-symbols     11.2.20
;;;     do-symbols              11.2.20
;;;     in-package              11.2.16
;
(in-package :xc)

;;;; 11.2.19 defpackage
(defmacro cl:defpackage (name &rest option*)
  (labels (
    (check-disjoint-name (name caller names list-name)
      (when (member name names :test #'string=)
        (macro-error
               "Can't ~A ~A, because it is in ~A."
               caller name list-name )) )

    (check-duplicate-name (name caller names)
      (when (member name names :test #'string=)
        (macro-error
               "~A is already in ~S list."
               name caller )) )

    (check-existing-package (name package)
      (let ((existing-package (find-package name)))
        (unless existing-package
          (macro-error
                 "No such package: ~A"
                 name ))
        (when (eq existing-package package)
          (macro-error
                 "Can't use itself: ~S"
                 existing-package )) ) )

    (check-new-package (name package)
      (let ((new-package (find-package name)))
        (unless (or (null new-package) (eq new-package package))
          (style-warn
                 "Package ~A already exists."
                 name )) ) )
    )
    ;;
    ;; defpackage
    ;;
    (setq name (string name))
    (let ((new-package           (find-package name))
          (doc-string             nil)
          (exports                '())
          (imports                '())
          (import-froms           '())
          (interns                '())
          (names                  (list name))
          (shadows                '())
          (shadowing-imports      '())
          (shadowing-import-froms '())
          (size                   nil)
          (supplied-use-p         nil)
          (uses                   '()) )
      (dolist (option option*)
        (when (<= (si::safe-list-length option) 0)
          (macro-error
                 "Malformed ~S option: ~S"
                 'defpackage option ))

        (case (first option)
          ((:documentation)
            (unless (and (null (cddr option)) (stringp (second option)))
              (syntax-error
                     :syntax '(:documentation string)
                     :form option ))

            (when doc-string
              (macro-error
                     "Multiple ~S option: ~S"
                     'defpackage
                     :documentation ))

            (setq doc-string (second option)) )

          ((:export)
            (dolist (name (rest option))
              (setq name (string name))
              (check-duplicate-name name :export exports)
              (check-disjoint-name  name :intern interns :export)
              (push name exports) ) )

          ((:import-from)
            (unless (> (si::safe-list-length option) 2)
              (error 'syntax-error
                     :syntax '(:import-from package &rest symbol-name*)
                     :form option ))
            (let ((import-from (string (second option)))
                  (names       '()) )
              (check-existing-package import-from new-package)
              (dolist (name (cddr option))
                (setq name (string name))
                (check-duplicate-name name :import-from imports)
                (check-disjoint-name  name :import-from interns :intern)
                (check-disjoint-name  name :import-from shadows :shadow)
                (check-disjoint-name  name :import-from shadowing-imports
                                           :shadowing-import-from )
                (push name imports)
                (push name names) )
              (push (cons import-from names) import-froms) ) )

          ((:intern)
            (dolist (name (rest option))
              (setq name (string name))
              (check-duplicate-name name :intern interns)
              (check-disjoint-name  name :intern exports :intern)
              (check-disjoint-name  name :intern imports :import-from)
              (check-disjoint-name  name :intern shadows :shadow)
              (check-disjoint-name  name :intern shadowing-imports
                                         :shadowing-imports )
              (push name interns) ) )

          ((:nicknames)
            (dolist (name (rest option))
              (setq name (string name))
              (check-duplicate-name name :nicknames names)
              (check-new-package name new-package)
              (push name names) ) )

          ((:shadow)
            (dolist (name (rest option))
              (setq name (string name))
              (check-duplicate-name name :shadow shadows)
              (check-disjoint-name  name :shadow imports :import-from)
              (check-disjoint-name  name :shadow interns :intern)
              (check-disjoint-name  name :shadow shadows :shadow)
              (check-disjoint-name  name :shadow shadowing-imports
                                         :shadowing-imports )
              (push name shadows) ) )

          ((:shadowing-import-from)
            (unless (> (si::safe-list-length option) 2)
              (error 'syntax-error
                     :syntax '(:shadowing-import-from package
                                                      &rest symbol-name* )
                     :form option ))
            (let ((import-from (string (second option)))
                  (names       '()) )
              (check-existing-package import-from new-package)
              (dolist (name (cddr option))
                (setq name (string name))
                (check-duplicate-name name :shadowing-import-from
                                           shadowing-imports )
                (check-disjoint-name  name :shadowing-import-from
                                           imports :import-from )
                (check-disjoint-name  name :shadowing-import-from
                                           interns :intern )
                (check-disjoint-name  name :shadowing-import-from
                                           shadows :shadows )
                (push name shadowing-imports)
                (push name names) )
              (push (cons import-from names) shadowing-import-froms) ) )

          ((:size)
            (unless (and (null (cddr option)) (integerp (second option)))
              (error 'syntax-error
                     :syntax '(:size integer)
                     :form option ))

            (when size
              (macro-error
                     "~S option ~S appeared more than once."
                     'defpackage
                     :size ))

            (setq size (second option)) )

          ((:use)
            (setq supplied-use-p t)
            (dolist (name (rest option))
              (setq name (string name))
              (check-existing-package name new-package)
              (push name uses) ) )

          (otherwise
            (macro-error
                   "Unknown ~S option: ~S"
                   'defpackage
                   (first option) ) )) )
      ;;
      ;; Expand to %defpackage
      ;;
      (setq names (nreverse names))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (si::%defpackage ',names
                      ',doc-string
                      ',exports
                      ',import-froms
                      ',interns
                      ',shadows
                      ',shadowing-import-froms
                      ',size
                      ',supplied-use-p
                      ',uses ) ) ) ) )


(labels (
  ;; expand
  (expand (var result result-p body package-list symbol-type*)
    (let ((loop   '#:loop)
          (more   '#:more)
          (next   '#:next)
          (symb   '#:symb)
          (return (if result-p `(return ,result) '(return))) )
      (multiple-value-bind (decl* form*) (analyze-body body nil)
       `(block nil
          (with-package-iterator (,next ,package-list ,@symbol-type*)
            (tagbody
             ,loop
              (multiple-value-bind (,more ,symb) (,next)
                (unless ,more ,return)
                (let ((,var ,symb)) (declare (type symbol ,var)) ,@decl*
                  (tagbody ,@form*) ) )
              (go ,loop) ) )) ) ) )
  )
  ;; 11.2.20 do-all-symbols
  (defmacro cl:do-all-symbols ((var &optional (result nil result-p))
                                    &body body )
    (expand var result result-p body
        '(list-all-packages) '(:internal :external)) )

  ;; 11.2.20 do-external-symbols
  (defmacro cl:do-external-symbols
        ((var &optional (package '*package*) (result nil result-p))
            &body body )
    (expand var result result-p body
        `(list ,package) '(:external) ) )

  ;; 11.2.20 do-symbols
  (defmacro cl:do-symbols
        ((var &optional (package '*package*) (result nil result-p))
            &body body )
    (expand var result result-p body
        `(list ,package) '(:external :internal :inherited) ) )
 ) ; labels


;;;; 11.2.16 in-package
(defmacro in-package (package)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (si::%in-package ',package) ))
