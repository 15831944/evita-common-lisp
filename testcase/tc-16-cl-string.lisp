;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-16-cl-string.lisp#1 $
(in-package :tc-user)

;;; 16.2.8 string-upcate
(deftest cl-16-08-001 () (string-upcase "abcde") "ABCDE")
(deftest cl-16-08-002 () (string-upcase "abcde" :start 2 :end 4) "abCDe")

(deftest cl-16-08-003 () (string-downcase "ABCDE") "abcde")
(deftest cl-16-08-004 () (string-downcase "ABCDE" :start 2 :end 4) "ABcdE")

(deftest cl-16-08-101 () (string-capitalize "ABCDE") "Abcde")
(deftest cl-16-08-102 () (string-capitalize "ABCDE" :start 2 :end 4) "ABCdE")

(deftest cl-16-08-201 ()
    (string-upcase "Dr. Livingston, I presume?")
  "DR. LIVINGSTON, I PRESUME?" )

(deftest cl-16-08-202 ()
    (string-upcase "Dr. Livingston, I presume?" :start 6 :end 10)
  "Dr. LiVINGston, I presume?" )

(deftest cl-16-08-203 ()
    (string-downcase "Dr. Livingston, I presume?")
  "dr. livingston, i presume?" )

(deftest cl-16-08-204 ()
    (string-capitalize "elm 13c arthur;fig don't")
  "Elm 13c Arthur;Fig Don'T" )

(deftest cl-16-08-205 ()
    (string-capitalize " hello ")
  " Hello " )

(deftest cl-16-08-206 ()
    (string-capitalize
        "occlUDeD cASEmenTs FOreSTAll iNADVertent DEFenestraTION" )
  "Occluded Casements Forestall Inadvertent Defenestration" )

(deftest cl-16-08-207 ()
    (string-capitalize 'kludgy-hash-search)
  "Kludgy-Hash-Search" )

(deftest cl-16-08-208 ()
    "not \"Don't!\""
    (string-capitalize "DON'T!")
  "Don'T!" )

(deftest cl-16-08-209 ()
    (string-capitalize "pipe 13a, foo16c")
  "Pipe 13a, Foo16c")

(deftest cl-16-08-301 ()
    (let ((str  (copy-seq "0123ABCD890a")))
      (values (nstring-downcase str :start 5 :end 7) str) )
  (values "0123AbcD890a" "0123AbcD890a") )


(deftest cl-16-08-401 ()
    (let ((s (make-array 10 :element-type 'character :fill-pointer 3)))
      (setf (char s 0) #\a)
      (setf (char s 1) #\b)
      (setf (char s 2) #\c)
      (nstring-upcase s) ) "ABC" )


(deftest cl-16-08-402 ()
    (let ((s (make-array 10 :element-type 'character :fill-pointer 3)))
      (setf (char s 0) #\A)
      (setf (char s 1) #\B)
      (setf (char s 2) #\C)
      (nstring-downcase s) ) "abc" )

;;; 16.2.9 string-trim
(deftest cl-16-09-001 ()
    (string-trim "abc" "abcaakaaakabcaaa")
  "kaaak" )

(deftest cl-16-09-002 ()
    (string-trim '(#\Space #\Tab #\Newline) " garbanzo beans
        ")
 "garbanzo beans" )

(deftest cl-16-09-003 ()
    (string-trim " (*)" " ( *three (silly) words* ) ")
 "three (silly) words" )

(deftest cl-16-09-101 ()
    (string-left-trim "abc" "labcabcabc")
  "labcabcabc" )

(deftest cl-16-09-002 ()
    (string-left-trim " (*)" " ( *three (silly) words* ) ")
  "three (silly) words* ) " )

(deftest cl-16-09-003 ()
    (string-right-trim " (*)" " ( *three (silly) words* ) ") 
  " ( *three (silly) words" )

(deftest cl-16-09-004 ()
    (let* ((base    (copy-seq "0123456789"))
           (string  (make-array 5 :element-type 'character
                                  :displaced-to base
                                  :displaced-index-offset 2 )) )
      (string-left-trim "234" string) )
  "56" )

(deftest cl-16-09-005 ()
    (let* ((base    (copy-seq "0123456789"))
           (string  (make-array 5 :element-type 'character
                                  :displaced-to base
                                  :displaced-index-offset 2 )) )
      (string-right-trim "56" string) )
 "234" )

(deftest cl-16-09-006 ()
    (let* ((base    (copy-seq "0123456789"))
           (string  (make-array 5 :element-type 'character
                                  :displaced-to base
                                  :displaced-index-offset 2 )) )
      (string-trim "2356" string) )
 "4" )

(defmacro string-cmp (op &rest args)
  `(bool (eql (bool (,op ,.args))
              (flet ((foo () (bool (,op ,.args)))) (foo)))) )

(deftest cl-16-09-101 () (string-cmp string= "foo" "foo") t)
(deftest cl-16-09-102 () (string-cmp string= "FOO" "foo") t)
(deftest cl-16-09-103 () (string-cmp string= "foo" "bar") t)

(deftest cl-16-09-104 () (string-cmp string/= "foo" "foo") t)
(deftest cl-16-09-105 () (string-cmp string/= "FOO" "foo") t)
(deftest cl-16-09-106 () (string-cmp string/= "foo" "bar") t)

(deftest cl-16-09-107 () (string-cmp string< "foo" "foo") t)
(deftest cl-16-09-108 () (string-cmp string< "FOO" "foo") t)
(deftest cl-16-09-109 () (string-cmp string< "foo" "bar") t)

(deftest cl-16-09-110 () (string-cmp string<= "foo" "foo") t)
(deftest cl-16-09-111 () (string-cmp string<= "FOO" "foo") t)
(deftest cl-16-09-112 () (string-cmp string<= "foo" "bar") t)

(deftest cl-16-09-113 () (string-cmp string> "foo" "foo") t)
(deftest cl-16-09-114 () (string-cmp string> "FOO" "foo") t)
(deftest cl-16-09-115 () (string-cmp string> "foo" "bar") t)

(deftest cl-16-09-116 () (string-cmp string>= "foo" "foo") t)
(deftest cl-16-09-117 () (string-cmp string>= "FOO" "foo") t)
(deftest cl-16-09-118 () (string-cmp string>= "foo" "bar") t)


(deftest cl-16-09-119 () (string-cmp string-equal "foo" "foo") t)
(deftest cl-16-09-120 () (string-cmp string-equal "FOO" "foo") t)
(deftest cl-16-09-121 () (string-cmp string-equal "foo" "bar") t)
