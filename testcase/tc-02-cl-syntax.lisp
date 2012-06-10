;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-02-cl-syntax.lisp#1 $
(in-package :tc-user)

;;; 2.2 Reader Algorithm

(deftest cl-02-02-001 ()
    (read-from-string "ab\\c")
    (values |ABc| 4) )

(deftest cl-02-02-002 ()
    (read-from-string "ab|c|")
    (values |ABc| 5) )

(deftest cl-02-02-003 ()
    (read-from-string "213.")
    (values 213 4) )


;;; 2.4.1  Left-Parenthesis 
;;; 2.4.2  Right-Parenthesis 
;;; 2.4.3  Single-Quote 
;;; 2.4.4  Semicolon 
;;; 2.4.5  Double-Quote 
;;; 2.4.6  Backquote 
;;; 2.4.7  Comma 
;;; 2.4.8  Sharpsign 


;;; 2.4.8.17 Sharpsign Plus
;;; 2.4.8.18 Sharpsign Minus

; When reader doesn't check number of values returned from macro reader,
; result has two elements.
(deftest cl-02-03-003 ()
    (read-from-string "(list #+ignore 'ignore #-ignore 'ok)")
    (values (list 'ok) 36) )

(deftest cl-02-03-004 ()
    (read-from-string "(list #+common-lisp \"Common Lisp\")")
    (values (list "Common Lisp") 34) )

(deftest cl-02-03-005 ()
    (read-from-string "(list #+(and common-lisp nil) 1 2)")
    (values (list 2) 34) )

(deftest cl-02-03-006 ()
    (read-from-string "(list #+(or common-lisp nil) 1 2)")
    (values (list 1 2) 33) )

(deftest cl-02-03-007 ()
    (read-from-string "(list #+(not common-lisp) 1 2)")
    (values (list 2) 30) )

; Good implementation should not add nil as feature.
(deftest cl-02-03-008 ()
    (read-from-string "(list #+(not nil) 1 2)")
    (values (list 1 2) 22) )




;;;; 2.4.8.12 Sharpsign A

#+nil
(deftest cl-02-04-001 ()
    (read-from-string "#0A object")
    #.(make-array nil :initial-element 'object) )

 #+nil
(deftest cl-02-04-002 ()
    (read-from-string "#1A(object)")
    #.(make-array 1 :initial-element 'object) )

#+nil
(deftest cl-02-04-003 ()
    (read-from-string "#2A((1 2 3) (4 5 6))")
    #.(make-array '(2 3) :initial-contents '((1 2 3) (4 5 6))) )

#+nil
(deftest cl-02-04-004 ()
    (read-from-string "#2A(nil)")
    #.(make-array '(1 0) :initial-contents '(nil)) )


;;; 2.4.8.10 Sharpsign B
(deftest cl-02-05-001 () (read-from-string "#b11010101") 213)
(deftest cl-02-05-002 () (read-from-string "#b+11010101")  213)
(deftest cl-02-05-003 () (read-from-string "#b-11010101") -213)
(deftest cl-02-05-004 () (read-from-string "#b1001/1011")  9/11)
(deftest cl-02-05-005 () (read-from-string "#b+1001/1011") 9/11)
(deftest cl-02-05-006 () (read-from-string "#b-1001/1011") -9/11)

;;; 2.4.8.11 Sharpsign C
(deftest cl-02-05-001 ()
    (read-from-string "#c(1 1)")
    #.(complex 1 1) )

(deftest cl-02-05-002 ()
    (read-from-string "#c(1 0)")
    1 )

(deftest cl-02-05-003 ()
    (read-from-string "#c(1.0 1.0)")
    #.(complex 1.0 1.0) )

(deftest cl-02-05-003 ()
    (read-from-string "#c(1.0 0.0)")
    #.(complex 1.0 0.0) )


;;; 2.4.8.13 Sharpsign O
(deftest cl-02-06-001 () (read-from-string "#o325") 213)
(deftest cl-02-06-002 () (read-from-string "#o+325") 213)
(deftest cl-02-06-003 () (read-from-string "#o-325") -213)
(deftest cl-02-06-004 () (read-from-string "#o123/456") 83/302)
(deftest cl-02-06-005 () (read-from-string "#o+123/456") 83/302)
(deftest cl-02-06-006 () (read-from-string "#o-123/456") -83/302)


;;; 2.4.8.14 Sharpsign R
(deftest cl-02-07-001 () (read-from-string "#2r11010101") 213)
(deftest cl-02-07-002 () (read-from-string "#8r325") 213)
(deftest cl-02-07-003 () (read-from-string "#16rD5") 213)
(deftest cl-02-07-004 () (read-from-string "#2r+11010101") 213)
(deftest cl-02-07-005 () (read-from-string "#8r+325") 213)
(deftest cl-02-07-006 () (read-from-string "#16r+D5") 213)
(deftest cl-02-07-007 () (read-from-string "#2r-11010101") -213)
(deftest cl-02-07-008 () (read-from-string "#8r-325") -213)
(deftest cl-02-07-009 () (read-from-string "#16r-D5") -213)
(deftest cl-02-07-010 () (read-from-string "#3r-21010") -192)
(deftest cl-02-07-011 () (read-from-string "#25r-7H") -192)

;;; 2.4.8.9  Sharpsing X
(deftest cl-02-08-001 () (read-from-string "#x1234") 4660)
(deftest cl-02-08-002 () (read-from-string "#xabcd") 43981)
(deftest cl-02-08-003 () (read-from-string "#x+1234") 4660)
(deftest cl-02-08-004 () (read-from-string "#x+abcd") 43981)
(deftest cl-02-08-005 () (read-from-string "#x-1234") -4660)
(deftest cl-02-08-006 () (read-from-string "#x-abcd") -43981)
(deftest cl-02-08-007 () (read-from-string "#x123456789abcdef") 81985529216486895)
(deftest cl-02-08-008 () (read-from-string "#x-123456789abcdef") -81985529216486895)
(deftest cl-02-08-009 () (read-from-string "#x123456789ABCDEF") 81985529216486895)
(deftest cl-02-08-010 () (read-from-string "#x-123456789ABCDEF") -81985529216486895)
(deftest cl-02-08-011 () (read-from-string "#xACCEDED") 181202413)

;;; 2.4.8.1 Sharpsign Backslash
(deftest cl-02-09-001 () (read-from-string "#\\Space") #.(code-char 32))
(deftest cl-02-09-002 () (read-from-string "#\\space") #.(code-char 32))
(deftest cl-02-09-003 () (read-from-string "#\\SpAcE") #.(code-char 32))
; Code value of #\Newline depends on implementation.
(deftest cl-02-09-004 () (read-from-string "#\\Newline") #.(code-char 10))
(deftest cl-02-09-005 () (read-from-string "#\\Return") #.(code-char 13))
(deftest cl-02-09-006 () (read-from-string "#\\Page") #.(code-char 12))
(deftest cl-02-09-007 () (read-from-string "#\\Tab") #.(code-char 9))
(deftest cl-02-09-008 () (read-from-string "#\\A") #.(code-char 65))
(deftest cl-02-09-009 () (read-from-string "#\\a") #.(code-char 97))
(deftest cl-02-09-010 () (read-from-string "#\\\\") #.(code-char 92))
(deftest cl-02-09-011 () (read-from-string "(#\\\\ 2)") (#.(code-char 92) 2))
;; evcl extension
(deftest cl-02-09-012 () (read-from-string "#\\control+a") #.(code-char 1))
(deftest cl-02-09-013 () (read-from-string "#\\control+Z") #.(code-char 26))
(deftest cl-02-09-014 () (read-from-string "#\\u1234") #.(code-char #x1234))
(deftest cl-02-09-015 () (read-from-string "#\\uabcd") #.(code-char #xABCD))
(deftest cl-02-09-016 () (read-from-string "#\\uAbcD") #.(code-char #xABCD))
