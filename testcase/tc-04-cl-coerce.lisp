;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-04-cl-coerce.lisp#1 $
(in-package :tc-user)

;;;; 4.4.24 coerce
;;; sequence
;;; character
;;; complex
;;; float
;;; function

(deftest cl-04-24-001 () (coerce nil        't) nil)
(deftest cl-04-24-002 () (coerce '(#\a #\b) 't) (#\a #\b))
(deftest cl-04-24-003 () (coerce #(a b)     't) #(a b))
(deftest cl-04-24-004 () (coerce #*0101     't) #*0101)
(deftest cl-04-24-005 () (coerce "ab"       't) "ab")
(deftest cl-04-24-006 () (coerce 1          't) 1)
(deftest cl-04-24-007 () (coerce 'a         't) a)
(deftest cl-04-24-008 () (coerce #\a        't) #\a)


(deftest cl-04-24-101 () (coerce nil        'list) nil)
(deftest cl-04-24-102 () (coerce '(#\a #\b) 'list) (#\a #\b))
(deftest cl-04-24-103 () (coerce #(a b)     'list) (a b))
(deftest cl-04-24-104 () (coerce #*0101     'list) (0 1 0 1))
(deftest cl-04-24-105 () (coerce "ab"       'list) (#\a #\b))
(deftest cl-04-24-106 () (coerce 1          'list) :error type-error)
(deftest cl-04-24-107 () (coerce 'a         'list) :error type-error)
(deftest cl-04-24-108 () (coerce #\a        'list) :error type-error)

(deftest cl-04-24-201 () (coerce nil        'string) "")
(deftest cl-04-24-202 () (coerce '(#\a #\b) 'string) "ab")
(deftest cl-04-24-203 () (coerce #(a b)     'string) :error type-error)
(deftest cl-04-24-204 () (coerce #*0101     'string) :error type-error)
(deftest cl-04-24-205 () (coerce "ab"       'string) "ab")
(deftest cl-04-24-206 () (coerce 1          'string) :error type-error)
(deftest cl-04-24-207 () (coerce 'a         'string) :error type-error)
(deftest cl-04-24-209 () (coerce #\a        'string) :error type-error)

(deftest cl-04-24-301 () (coerce nil        'vector) #())
(deftest cl-04-24-302 () (coerce '(#\a #\b) 'vector) #(#\a #\b))
(deftest cl-04-24-303 () (coerce #(a b)     'vector) #(a b))
(deftest cl-04-24-304 () (coerce #*0101     'vector) #*0101)
(deftest cl-04-24-305 () (coerce "ab"       'vector) "ab")
(deftest cl-04-24-306 () (coerce 1          'vector) :error type-error)
(deftest cl-04-24-307 () (coerce 'a         'vector) :error type-error)
(deftest cl-04-24-308 () (coerce #\a        'vector) :error type-error)

(deftest cl-04-24-401 () (coerce nil        'bit-vector) #*)
(deftest cl-04-24-402 () (coerce '(#\a #\b) 'bit-vector) :error type-error)
(deftest cl-04-24-403 () (coerce #(a b)     'bit-vector) :error type-error)
(deftest cl-04-24-404 () (coerce #*0101     'bit-vector) #*0101)
(deftest cl-04-24-405 () (coerce "ab"       'bit-vector) :error type-error)
(deftest cl-04-24-406 () (coerce 1          'bit-vector) :error type-error)
(deftest cl-04-24-407 () (coerce 'a         'bit-vector) :error type-error)
(deftest cl-04-24-408 () (coerce #\a        'bit-vector) :error type-error)

(deftest cl-04-24-401 () (coerce nil        'character) :error type-error)
(deftest cl-04-24-402 () (coerce '(#\a #\b) 'character) :error type-error)
(deftest cl-04-24-403 () (coerce #(a b)     'character) :error type-error)
(deftest cl-04-24-404 () (coerce #*0101     'character) :error type-error)
(deftest cl-04-24-405 () (coerce "ab"       'character) :error type-error)
(deftest cl-04-24-406 () (coerce 1          'character) :error type-error)
(deftest cl-04-24-407 () (coerce 'a         'character) #\A)
(deftest cl-04-24-408 () (coerce #\a        'character) #\a)
(deftest cl-04-24-409 () (coerce "a"        'character) #\a)


(deftest cl-04-24-901 () (coerce '(a b c) 'vector)  #(A B C))
(deftest cl-04-24-903 () (coerce 'a 'character)  #\A)
(deftest cl-04-24-904 () (coerce 4.56 'complex)  #C(4.56 0.0))
(deftest cl-04-24-905 () (coerce 4.5s0 'complex)  #C(4.5s0 0.0s0))
(deftest cl-04-24-906 () (coerce 7/2 'complex)  7/2)
(deftest cl-04-24-907 () (coerce 0 'short-float)  0.0s0)
(deftest cl-04-24-908 () (coerce 3.5L0 'float)  3.5L0)
(deftest cl-04-24-909 () (coerce 7/2 'float)  3.5)
(deftest cl-04-24-910 () (coerce (cons 1 2) t)  (1 . 2))

;; All the following forms should signal an error:
(deftest cl-04-24-911 () (coerce '(a b c) '(vector * 4)) :error type-error)
(deftest cl-04-24-912 () (coerce #(a b c) '(vector * 4)) :error type-error)
(deftest cl-04-24-913 () (coerce '(a b c) '(vector * 2)) :error type-error)
(deftest cl-04-24-914 () (coerce #(a b c) '(vector * 2)) :error type-error)
(deftest cl-04-24-915 () (coerce "foo" '(string 2)) :error type-error)
(deftest cl-04-24-916 () (coerce #(#\a #\b #\c) '(string 2)) :error type-error)
(deftest cl-04-24-917 () (coerce '(0 1) '(simple-bit-vector 3)) :error type-error)

(deftest cl-04-24-990 () (coerce 1 nil) :error type-error)
(deftest cl-04-24-991 () (coerce 'x nil) :error type-error)
(deftest cl-04-24-991 () (coerce nil nil) :error type-error)

