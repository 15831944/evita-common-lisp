;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-12-cl-fixnum.lisp#1 $
(in-package :tc-user)

;;; 12.2.19 floor, ceiling, truncate, round

;;; (div 50 6)
(deftest cl-12-19-100 ()
  (list (multiple-value-list (ceiling +50 +6))     ; P/P=>P,P
        (multiple-value-list (ceiling -50 +6))     ; N/P=>N,N
        (multiple-value-list (ceiling +50 -6))     ; P/N=>P,N
        (multiple-value-list (ceiling -50 -6)) )   ; N/N=>P,N
  ((9 -4) (-8 -2) (-8 2) (9 4)) )

(deftest cl-12-19-101 ()
  (list (multiple-value-list (floor +50 +6))     ; P/P=>P,P
        (multiple-value-list (floor -50 +6))     ; N/P=>N,N
        (multiple-value-list (floor +50 -6))     ; P/N=>P,N
        (multiple-value-list (floor -50 -6)) )   ; N/N=>P,N
  ((8 2) (-9 4) (-9 -4) (8 -2)) )

(deftest cl-12-19-102 ()
  (list (multiple-value-list (round +50 +6))     ; P/P=>P,P
        (multiple-value-list (round -50 +6))     ; N/P=>N,N
        (multiple-value-list (round +50 -6))     ; P/N=>P,N
        (multiple-value-list (round -50 -6)) )   ; N/N=>P,N
  ((8 2) (-8 -2) (-8 2) (8 -2)) )

(deftest cl-12-19-103 ()
  (list (multiple-value-list (truncate +50 +6))     ; P/P=>P,P
        (multiple-value-list (truncate -50 +6))     ; N/P=>N,N
        (multiple-value-list (truncate +50 -6))     ; P/N=>P,N
        (multiple-value-list (truncate -50 -6)) )   ; N/N=>P,N
  ((8 2) (-8 -2) (-8 2) (8 -2)) )

;;; (div 3 2)
(deftest cl-12-19-104 ()
  (list (multiple-value-list (ceiling +3 +2))     ; P/P=>P,P
        (multiple-value-list (ceiling -3 +2))     ; N/P=>N,N
        (multiple-value-list (ceiling +3 -2))     ; P/N=>P,N
        (multiple-value-list (ceiling -3 -2)) )   ; N/N=>P,N
  ((2 -1) (-1 -1) (-1 1) (2 1)) )

(deftest cl-12-19-105 ()
  (list (multiple-value-list (floor +3 +2))     ; P/P=>P,P
        (multiple-value-list (floor -3 +2))     ; N/P=>N,N
        (multiple-value-list (floor +3 -2))     ; P/N=>P,N
        (multiple-value-list (floor -3 -2)) )   ; N/N=>P,N
  ((1 1) (-2 1) (-2 -1) (1 -1)) )

(deftest cl-12-19-102 ()
  (list (multiple-value-list (round +3 +2))     ; P/P=>P,P
        (multiple-value-list (round -3 +2))     ; N/P=>N,N
        (multiple-value-list (round +3 -2))     ; P/N=>P,N
        (multiple-value-list (round -3 -2)) )   ; N/N=>P,N
  ((2 -1) (-2 1) (-2 -1) (2 1)) )

(deftest cl-12-19-107 ()
  (list (multiple-value-list (truncate +3 +2))     ; P/P=>P,P
        (multiple-value-list (truncate -3 +2))     ; N/P=>N,N
        (multiple-value-list (truncate +3 -2))     ; P/N=>P,N
        (multiple-value-list (truncate -3 -2)) )   ; N/N=>P,N
  ((1 1) (-1 -1) (-1 1) (1 -1)) )

;;;; 12.2.57 integer-length
(deftest cl-12-57-100 () (integer-length 0)  0)
(deftest cl-12-57-101 () (integer-length 1)  1)
(deftest cl-12-57-102 () (integer-length 3)  2)
(deftest cl-12-57-103 () (integer-length 4)  3)
(deftest cl-12-57-104 () (integer-length 7)  3)
(deftest cl-12-57-105 () (integer-length -1)  0)
(deftest cl-12-57-106 () (integer-length -4)  2)
(deftest cl-12-57-107 () (integer-length -7)  3)
(deftest cl-12-57-108 () (integer-length -8)  3)
(deftest cl-12-57-109 () (integer-length (expt 2 9))  10)
(deftest cl-12-57-110 () (integer-length (1- (expt 2 9)))  9)
(deftest cl-12-57-111 () (integer-length (- (expt 2 9)))  9)
(deftest cl-12-57-112 () (integer-length (- (1+ (expt 2 9))))  10)

;;;; 12.2.74 float
(deftest cl-12-74-101 () (float 0) 0.0)
(deftest cl-12-74-102 () (float 1 .5) 1.0)

(deftest cl-12-74-103 ()
  (loop
    for k from (integer-length most-positive-fixnum) to 127
    for ix = (ash  1 k)
    for fx = (float ix)
    for iy = (ash -1 k)
    for fy = (float iy)
      unless (or (eql ix (- iy)) (eql fx (- fy))) collect ix )
  nil )

(deftest cl-12-74-111 () (float 0 0d0) 0.0d0)
(deftest cl-12-74-112 () (float 1 .5d0) 1.0d0)

(deftest cl-12-74-113 ()
  (loop
    for k from (integer-length most-positive-fixnum) to 127
    for ix = (ash  1 k)
    for fx = (float ix 0d0)
    for iy = (ash -1 k)
    for fy = (float iy 0d0)
      unless (or (eql ix (- iy)) (eql fx (- fy))) collect ix )
  nil )
