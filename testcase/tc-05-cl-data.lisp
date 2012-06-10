;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: TC-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-05-cl-data.lisp#1 $
(in-package #:tc-user)

;;;; 5.3.1 not
(deftest cl-05-301-001 () (not nil)               :boolean t)
(deftest cl-05-301-002 () (not '())               :boolean t)
(deftest cl-05-301-003 () (not (integerp 'sss))   :boolean t)
(deftest cl-05-301-004 () (not (integerp 1))      :boolean nil)
(deftest cl-05-301-005 () (not 3.7)               :boolean nil)
(deftest cl-05-301-006 () (not 'apple)            :boolean nil)

(deftest cl-05-301-007 ()
    (flet (
      (not-1 (x) (not x))
      (not-2 (x) (not (not x)))
      (not-3 (x) (not (not (not x))))
      )
      (list
        (not-1 1) (not-2 1) (not-3 1)
        (not-1 nil) (not-2 nil) (not-3 nil) ) )
  (nil t nil t nil t) )


;;; 5.3.4 fboundp
(deftest cl-05-304-001 () (fboundp 'car)            :boolean t)
(deftest cl-05-304-002 () (fboundp 'deftest)     :boolean t)
(deftest cl-05-304-003 () (fboundp 'not-exist)      :boolean nil)

(deftest cl-05-304-004 ()
    "define-setf-expander"
  (fboundp '(setf apply)) :boolean nil )

(deftest cl-05-304-005 ()
    "defsetf"
  (fboundp '(setf subseq)) :boolean nil )

;; special-operators
(deftest cl-05-304-101 () (fboundp 'block) :boolean t)
(deftest cl-05-304-102 () (fboundp 'catch) :boolean t)
(deftest cl-05-304-103 () (fboundp 'eval-when) :boolean t)
(deftest cl-05-304-104 () (fboundp 'flet) :boolean t)
(deftest cl-05-304-105 () (fboundp 'function) :boolean t)
(deftest cl-05-304-106 () (fboundp 'go) :boolean t)
(deftest cl-05-304-107 () (fboundp 'if) :boolean t)
(deftest cl-05-304-108 () (fboundp 'labels) :boolean t)
(deftest cl-05-304-109 () (fboundp 'let) :boolean t)
(deftest cl-05-304-110 () (fboundp 'let*) :boolean t)
(deftest cl-05-304-111 () (fboundp 'load-time-value) :boolean t)
(deftest cl-05-304-112 () (fboundp 'locally) :boolean t)
(deftest cl-05-304-113 () (fboundp 'macrolet) :boolean t)
(deftest cl-05-304-114 () (fboundp 'multiple-value-call) :boolean t)
(deftest cl-05-304-115 () (fboundp 'multiple-value-prog1) :boolean t)
(deftest cl-05-304-116 () (fboundp 'progn) :boolean t)
(deftest cl-05-304-117 () (fboundp 'progv) :boolean t)
(deftest cl-05-304-118 () (fboundp 'quote) :boolean t)
(deftest cl-05-304-119 () (fboundp 'return-from) :boolean t)
(deftest cl-05-304-120 () (fboundp 'setq) :boolean t)
(deftest cl-05-304-121 () (fboundp 'symbol-macrolet) :boolean t)
(deftest cl-05-304-122 () (fboundp 'tagbody) :boolean t)
(deftest cl-05-304-123 () (fboundp 'the) :boolean t)
(deftest cl-05-304-124 () (fboundp 'throw) :boolean t)
(deftest cl-05-304-125 () (fboundp 'unwind-protect) :boolean t)


;;; 5.3.33 eq
(deftest cl-05-102-001 ()
    (eq 'a 'b) :boolean nil )

(deftest cl-05-102-002 ()
    (eq 'a 'a) :boolean t )

(deftest cl-05-102-003 ()
    (eq 3 3) :boolean t )

(deftest cl-05-102-004 ()
    (eq 3 3.0) :boolean nil )

(deftest cl-05-102-005 ()
    (eq 3.0 3.0) :boolean nil )

(deftest cl-05-102-006 ()
    (eq #c(3 -4) #c(3 -4)) :boolean nil )

(deftest cl-05-102-007 ()
    (eq #c(3 -4.0) #c(3 -4)) :boolean nil )

(deftest cl-05-102-008 ()
    (eq (cons 'a 'b) (cons 'a 'c)) :boolean nil )

(deftest cl-05-102-009 ()
    (eq (cons 'a 'b) (cons 'a 'b)) :boolean nil )

(deftest cl-05-102-010 ()
    (eq '(a . b) '(a . b)) :boolean nil )

(deftest cl-05-102-011 ()
    (let ((x (cons 'a 'b))) (eq x x))  :boolean t)

(deftest cl-05-102-012 ()
    (let ((x '(a . b))) (eq x x)) :boolean t )

(deftest cl-05-102-013 ()
    (eq #\A #\A) :boolean t )

(deftest cl-05-102-014 ()
    (let ((x "Foo")) (eq x x)) :boolean t )

(deftest cl-05-102-015 ()
    (eq "Foo" "Foo") :boolean nil )

(deftest cl-05-102-016 ()
    (eq "Foo" (copy-seq "Foo")) :boolean nil )

(deftest cl-05-102-017 ()
    (eq "FOO" "foo") :boolean nil )

(deftest cl-05-102-018 ()
    (eq "string-seq" (copy-seq "string-seq")) :boolean nil )

(deftest cl-05-102-019 ()
    (let ((x 5)) (eq x x)) :boolean t )


;;; 5.3.34 eql
(deftest cl-05-334-001 () (eql 'a 'b) :boolean nil)
(deftest cl-05-334-002 () (eql 'a 'a) :boolean t)
(deftest cl-05-334-003 () (eql 3 3) :boolean t)
(deftest cl-05-334-004 () (eql 3 3.0) :boolean nil)
(deftest cl-05-334-005 () (eql 3.0 3.0) :boolean t)
(deftest cl-05-334-006 () (eql #c(3 -4) #c(3 -4)) :boolean t)
(deftest cl-05-334-007 () (eql #c(3 -4.0) #c(3 -4)) :boolean nil)
(deftest cl-05-334-008 () (eql (cons 'a 'b) (cons 'a 'c)) :boolean nil)
(deftest cl-05-334-009 () (eql (cons 'a 'b) (cons 'a 'b)) :boolean nil)
(deftest cl-05-334-010 () (eql '(a . b) '(a . b)) :boolean nil)
(deftest cl-05-334-011 () (let ((x (cons 'a 'b))) (eql x x)) :boolean t)
(deftest cl-05-334-012 () (let ((x '(a . b))) (eql x x)) :boolean t)
(deftest cl-05-334-013 () (eql #\A #\A) :boolean t)
(deftest cl-05-334-014 () (eql "Foo" "Foo") :boolean nil)
(deftest cl-05-334-015 () (eql "Foo" (copy-seq "Foo")) :boolean nil)
(deftest cl-05-334-016 () (eql "FOO" "foo") :boolean nil)
(deftest cl-05-334-017 () (eql +0.0 -0.0) :boolean nil)

;;; 5.3.35 equal
(deftest cl-05-335-001 () (equal 'a 'b) :boolean nil)
(deftest cl-05-335-002 () (equal 'a 'a) :boolean t)
(deftest cl-05-335-003 () (equal 3 3) :boolean t)
(deftest cl-05-335-004 () (equal 3 3.0) :boolean nil)
(deftest cl-05-335-005 () (equal 3.0 3.0) :boolean t)
(deftest cl-05-335-006 () (equal #c(3 -4) #c(3 -4)) :boolean t)
(deftest cl-05-335-007 () (equal #c(3 -4.0) #c(3 -4)) :boolean nil)
(deftest cl-05-335-008 () (equal (cons 'a 'b) (cons 'a 'c)) :boolean nil)
(deftest cl-05-335-009 () (equal (cons 'a 'b) (cons 'a 'b)) :boolean t)
(deftest cl-05-335-010 () (equal '(member t nil) '(member t)) :boolean nil)
(deftest cl-05-335-011 () (equal #\A #\A) :boolean t)
(deftest cl-05-335-012 () (equal #\A #\a) :boolean nil)
(deftest cl-05-335-013 () (equal "Foo" "Foo") :boolean t)
(deftest cl-05-335-014 () (equal "Foo" (copy-seq "Foo")) :boolean t)
(deftest cl-05-335-015 () (equal "FOO" "foo") :boolean nil)
(deftest cl-05-335-016 () (equal "This-string" "This-string") :boolean t)
(deftest cl-05-335-017 () (equal "This-string" "this-string") :boolean nil)

;; less than 32bit
(deftest cl-05-335-018 () (equal #*1010 #*1010) :boolean t)
(deftest cl-05-335-019 () (equal #*1010 #*1000) :boolean nil)

;; n mod 32 = 0
(deftest cl-05-335-020 () (equal #32*1010 #32*1010) :boolean t)
(deftest cl-05-335-021 () (equal #64*1010 #64*1010) :boolean t)

;; n mod 32 = 0
(deftest cl-05-335-022 () (equal #32*1010 #32*1110) :boolean nil)
(deftest cl-05-335-023 () (equal #64*1010 #64*1110) :boolean nil)

;; n mod 32 != 0
(deftest cl-05-335-024 () (equal #40*1010 #40*1010) :boolean t)
(deftest cl-05-335-025 () (equal #80*1010 #80*1010) :boolean t)

;; n mod 40 != 0
(deftest cl-05-335-026 () (equal #40*1010 #40*1110) :boolean nil)
(deftest cl-05-335-027 () (equal #80*1010 #80*1110) :boolean nil)

(deftest cl-05-335-028 ()
    (equal #*00000000000000100000000000000000 #32*0) :boolean nil )

;;; 5.3.36 equalp
(deftest cl-05-336-001 () (equalp 'a 'b) :boolean nil)
(deftest cl-05-336-002 () (equalp 'a 'a) :boolean t)
(deftest cl-05-336-003 () (equalp 3 3) :boolean t)
(deftest cl-05-336-004 () (equalp 3 3.0) :boolean t)
(deftest cl-05-336-005 () (equalp 3.0 3.0) :boolean t)
(deftest cl-05-336-006 () (equalp #c(3 -4) #c(3 -4)) :boolean t)
(deftest cl-05-336-007 () (equalp #c(3 -4.0) #c(3 -4)) :boolean t)
(deftest cl-05-336-008 () (equalp (cons 'a 'b) (cons 'a 'c)) :boolean nil)
(deftest cl-05-336-009 () (equalp (cons 'a 'b) (cons 'a 'b)) :boolean t)
(deftest cl-05-336-010 () (equalp #\A #\A) :boolean t)
(deftest cl-05-336-011 () (equalp #\A #\a) :boolean t)
(deftest cl-05-336-012 () (equalp "Foo" "Foo") :boolean t)
(deftest cl-05-336-013 () (equalp "Foo" (copy-seq "Foo")) :boolean t)
(deftest cl-05-336-014 () (equalp "FOO" "foo") :boolean t)
(deftest cl-05-336-015 () (equal (vector 1 2) (vector 1 2)) :boolean t)

(deftest cl-05-336-016 ()
    (let ((array1 (make-array 6 :element-type 'integer
                                 :initial-contents '(1 1 1 3 5 7))) 
          (array2 (make-array 8 :element-type 'integer
                                 :initial-contents '(1 1 1 3 5 7 2 6)
                                 :fill-pointer 6) )
          (vector1 (vector 1 1 1 3 5 7)) )
      (values (not (null (equalp array1 array2)))
              (not (null (equalp array1 vector1))) ) )
  (values t t) )


;;;; 5.3.40 every, notany, notevery, some
(deftest cl-05-340-001 () (every #'characterp "abc") :boolean t)
(deftest cl-05-340-002 () (some #'= '(1 2 3 4 5) '(5 4 3 2 1)) :boolean t

(deftest cl-05-340-003 ()
    (notevery #'< '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) :boolean nil )

(deftest cl-05-340-004 ()
    (notany #'> '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) :boolean t )

(deftest cl-05-340-005 () (some #'plusp '(-4 0 5 6)) :boolean t)
(deftest cl-05-340-006 () (some #'> '(-4 0 5 6) '(0 12 12 12)) :boolean nil)
(deftest cl-05-340-007 () (some #'> '(-4 0 5 6) '(3 3 3 3)) :boolean t)
(deftest cl-05-340-008 () (some #'> '(-4 0 5 6) '(3 3)) :boolean nil)

(deftest cl-05-340-009 () (some #'plusp #(-4 0 5 6)) :boolean t)
(deftest cl-05-340-010 () (some #'> #(-4 0 5 6) #(0 12 12 12)) :boolean nil)
(deftest cl-05-340-011 () (some #'> #(-4 0 5 6) #(3 3 3 3)) :boolean t)
(deftest cl-05-340-012 () (some #'> #(-4 0 5 6) #(3 3)) :boolean nil)

(deftest cl-05-340-013 () (every    #'both-case-p "abc") :boolean t
(deftest cl-05-340-014 () (some     #'both-case-p "abc") :boolean t
(deftest cl-05-340-015 () (notany   #'both-case-p "abc") :boolean nil
(deftest cl-05-340-016 () (notevery #'both-case-p "abc") :boolean nil

(deftest cl-05-340-017 () (every    #'both-case-p "a+1") :boolean nil
(deftest cl-05-340-018 () (some     #'both-case-p "a+1") :boolean t
(deftest cl-05-340-019 () (notany   #'both-case-p "a+1") :boolean nil
(deftest cl-05-340-020 () (notevery #'both-case-p "a+1") :boolean t

(deftest cl-05-340-021 () (every    #'both-case-p "123") :boolean nil
(deftest cl-05-340-022 () (some     #'both-case-p "123") :boolean nil
(deftest cl-05-340-023 () (notany   #'both-case-p "123") :boolean t
(deftest cl-05-340-024 () (notevery #'both-case-p "123") :boolean t

(deftest cl-05-340-025 () (every    #'both-case-p '(#\a #\b #\c)) :boolean t
(deftest cl-05-340-026 () (some     #'both-case-p '(#\a #\b #\c)) :boolean t
(deftest cl-05-340-027 () (notany   #'both-case-p '(#\a #\b #\c)) :boolean nil
(deftest cl-05-340-028 () (notevery #'both-case-p '(#\a #\b #\c)) :boolean nil

(deftest cl-05-340-029 () (every    #'both-case-p '(#\a #\+ #\1)) :boolean nil
(deftest cl-05-340-030 () (some     #'both-case-p '(#\a #\+ #\1)) :boolean t
(deftest cl-05-340-031 () (notany   #'both-case-p '(#\a #\+ #\1)) :boolean nil
(deftest cl-05-340-032 () (notevery #'both-case-p '(#\a #\+ #\1)) :boolean t

(deftest cl-05-340-033 () (every    #'both-case-p '(#\1 #\2 #\3)) :boolean nil
(deftest cl-05-340-034 () (some     #'both-case-p '(#\1 #\2 #\3)) :boolean nil
(deftest cl-05-340-035 () (notany   #'both-case-p '(#\1 #\2 #\3)) :boolean t
(deftest cl-05-340-036 () (notevery #'both-case-p '(#\1 #\2 #\3)) :boolean t


;;; 5.3.56 nth-value
(deftest cl-05-356-001 () (nth-value 0 (values 'a 'b)) a)
(deftest cl-05-356-002 () (nth-value 1 (values 'a 'b)) b)
(deftest cl-05-356-003 () (nth-value 2 (values 'a 'b)) :boolean nil)

(deftest cl-05-356-004 ()
    (let* ((x 83927472397238947423879243432432432)
           (y 32423489732)
           (a (nth-value 1 (floor x y)))
           (b (mod x y)) )
      (values a b (= a b)) )
 (3332987528 3332987528 t) )
