;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-05-evcl.lisp#1 $
(in-package #:tc-user)

(deftest si-05-340-001 () (si::some/1 #'plusp '(-4 0 5 6)) :boolean t
(deftest si-05-340-002 () (si::some/1 #'plusp #(-4 0 5 6)) :boolean t

;;; vector
(si::every/1    #'both-case-p "abc") :boolean t
(si::some/1     #'both-case-p "abc") :boolean t
(si::notany/1   #'both-case-p "abc") :boolean nil
(si::notevery/1 #'both-case-p "abc") :boolean nil

(si::every/1    #'both-case-p "a+1") :boolean nil
(si::some/1     #'both-case-p "a+1") :boolean t
(si::notany/1   #'both-case-p "a+1") :boolean nil
(si::notevery/1 #'both-case-p "a+1") :boolean t

(si::every/1    #'both-case-p "123") :boolean nil
(si::some/1     #'both-case-p "123") :boolean nil
(si::notany/1   #'both-case-p "123") :boolean t
(si::notevery/1 #'both-case-p "123") :boolean t

;;; list
(si::every/1    #'both-case-p '(#\a #\b #\c)) :boolean t
(si::some/1     #'both-case-p '(#\a #\b #\c)) :boolean t
(si::notany/1   #'both-case-p '(#\a #\b #\c)) :boolean nil
(si::notevery/1 #'both-case-p '(#\a #\b #\c)) :boolean nil

(si::every/1    #'both-case-p '(#\a #\+ #\1)) :boolean nil
(si::some/1     #'both-case-p '(#\a #\+ #\1)) :boolean t
(si::notany/1   #'both-case-p '(#\a #\+ #\1)) :boolean nil
(si::notevery/1 #'both-case-p '(#\a #\+ #\1)) :boolean t

(si::every/1    #'both-case-p '(#\1 #\2 #\3)) :boolean nil
(si::some/1     #'both-case-p '(#\1 #\2 #\3)) :boolean nil
(si::notany/1   #'both-case-p '(#\1 #\2 #\3)) :boolean t
(si::notevery/1 #'both-case-p '(#\1 #\2 #\3)) :boolean t


