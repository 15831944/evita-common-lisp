;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-10-cl-symbol.lisp#1 $
(in-package :tc-user)

;;; 10.2.3 symbolp
(deftest cl-10-03-001 () (symbolp 'x)   :boolean t)
(deftest cl-10-03-002 () (symbolp 'nil) :boolean t)
(deftest cl-10-03-003 () (symbolp '())  :boolean t)
(deftest cl-10-03-004 () (symbolp 0)    :boolean nil)
(deftest cl-10-03-005 () (symbolp 1)    :boolean nil)
(deftest cl-10-03-006 () (symbolp #\A)  :boolean nil)
(deftest cl-10-03-007 () (symbolp #())  :boolean nil)
(deftest cl-10-03-008 () (symbolp #*1)  :boolean nil)
(deftest cl-10-03-009 () (symbolp "")   :boolean nil)
(deftest cl-10-03-010 () (symbolp "a")  :boolean nil)


;;; 10.2.4 keywordp
(deftest cl-10-04-001 () (keywordp :keyword) :boolean t)
(deftest cl-10-04-002 () (keywordp 'keyword) nil)
(deftest cl-10-04-003 () (keywordp 1)        nil)

;;; 10.2.5 make-symbol
(deftest cl-10-05-001 () (symbolp (make-symbol "FOO")) :boolean t)


;;; 10.2.6 copy-symbol
;;;
;;; copy1 <- copy only name.
;;; copy2 <- copy all slots.
(deftest cl-10-06-001 ()
    (let ((symbol '#:symbol) copy1 copy2)
      (setf (symbol-value symbol)    'value)
      (setf (symbol-function symbol) #'car)
      (setf (symbol-plist symbol)    '(name kyoko age 20 sex female))

      (setq copy1 (copy-symbol symbol))
      (setq copy2 (copy-symbol symbol t))

      (values
        (list (boundp copy1) (fboundp copy1) (symbol-plist copy1))
        (list (and (boundp copy2) (symbol-value copy2))
              (and (boundp copy2) (symbol-function copy2))
              (symbol-plist copy2) )) )
  (values
    (nil nil nil)
    (value #.#'car (name kyoko age 20 sex female)) ) )


;;; 10.2.7 gensym
(deftest cl-10-07-001 () (symbolp (gensym)) :boolean t)
(deftest cl-10-07-002 () (symbolp (gensym "X")) :boolean t)
(deftest cl-10-07-003 () (symbolp (gensym 0)) :boolean t)
(deftest cl-10-07-004 () (symbolp (gensym "")) :boolean t)


;;; 10.2.8 *gensym-counter*
(deftest cl-01-08-001 () (integerp *gensym-counter*) :boolean t)


;;; 10.2.9 gentemp
(deftest cl-10-09-001 () (symbolp (gentemp)) :boolean t)
(deftest cl-10-09-002 () (symbolp (gentemp "x")) :boolean t)
(deftest cl-10-09-003 () (symbolp (gentemp "x" :cl-user)) :boolean t)
(deftest cl-10-09-004 () (symbolp (gentemp "")) :boolean t)


;;; 10.2.10 symbol-function
(deftest cl-10-10-001 () (symbol-function 'car) #.#'car)

;;; 10.2.11 symbol-name
(deftest cl-10-11-001 () (symbol-name 'car) "CAR")


;;; 10.2.12 symbol-package
(deftest cl-10-12-001 () (symbol-package 'car) #.(find-package :cl))


;;; 10.2.13 symbol-plist
(deftest cl-10-13-001 ()
    (let ((symbol '#:symbol) (plist '(1 2 3 4 5)))
      (setf (symbol-plist symbol) plist)
      (eq (symbol-plist symbol) plist) )
  :boolean t )


;;; 10.2.14 symbol-value
;;; 10.2.15 get
;;; 10.2.16 remprop

;;; 10.2.17 boundp
(deftest cl-10-17-001 () (boundp '*package*) :boolean t)
(deftest cl-10-17-001 () (boundp '*unbound-variable*) nil)

;;; 10.2.18 makunbound
;;; 10.2.19 set

