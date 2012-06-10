;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 25 Environment - Time
;;; runtime/r25-time.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r25-time.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following functions:
;;;     apropos                         25.2.5  dev
;;;     apropos-list                    25.2.5  dev
;;;     decode-universal-time           25.2.1
;;;     describe                        25.2.6  dev
;;;     describe-object                 25.2.6  dev
;;;     disassemble                     25.2.14 dev (native)
;;;     documentation                   25.2.15 NYI dev
;;;     dribble                         25.2.19 NYI
;;;     ed                              25.2.17 NYI
;;;     encode-universal-time           25.2.2 
;;;     get-internal-real-time          25.2.12 intrinsic
;;;     get-internal-run-time           25.2.13 intrinsic
;;;     get-decoded-time                25.2.3  intrinsic
;;;     get-universal-time              25.2.3
;;;     inspect                         25.2.18 dev
;;;     internal-time-units-per-second  25.2.11 boot
;;;     lisp-implementation-type        25.2.24 intrinsic
;;;     lisp-implementation-version     25.2.24 intrinsic
;;;     long-site-name                  25.2.25 intrinsic
;;;     machine-instance                25.2.26 intrinsic
;;;     machine-type                    25.2.27 intrinsic
;;;     machine-version                 25.2.28 intrinsic
;;;     room                            25.2.16 dev (native)
;;;     short-site-name                 25.2.25 intrinsic
;;;     sleep                           25.2.4  NYI
;;;     software-type                   25.2.29 intrinsic
;;;     software-version                25.2.29 intrinsic
;;;     step                            25.2.9  dev
;;;     time                            25.2.10 dev
;;;     trace                           25.2.8  dev
;;;     untrace                         25.2.8  dev
;;;     user-homedir-pathname           25.2.30 NYI
;
(in-package :si)
;;;; *days-before-month*
;;;
;;; Value:
;;;         1  2  3  4   5   6   7   8   9  10  11  12  13
;;;   #(NIL 0 31 59 90 120 151 181 212 243 273 304 334 365)
;
(defconstant *days-before-month*
  (let ((vec (make-array 14))
        (ndays 0)
        (month 1) )
                ; 1  2  3  4  5  6  7  8  9 10 11 12
    (dolist (n '(31 28 31 30 31 30 31 31 30 31 30 31))
      (setf (svref vec month) ndays)
      (incf month)
      (incf ndays n) )
    (assert (= 365 ndays))
    (setf (svref vec month) ndays)
    vec ) )


;;;; leap-year-p
;;;
;;; Description:
;;;  Returns t when specified year is leap year, otherwise returns nil.
;;;
;;; Definition:
;;;  Common Lisp leap year is:
;;;    - divisible by 4
;;;    - except that divisible by 100
;;;    - divisible by 400
;
(defun leap-year-p (y)
    (declare (type fixnum y))
  (and (zerop (mod y 4))
       (or (not (zerop (mod y 100))) (zerop (mod y 400))) ) )


;;;; leap-years-before
;;;
;;; Description:
;;;  Returns number of leap years between 1900 and specified year.
;
(defun leap-years-before (year)
  (let ((nyears (- year 1901)))
    (+ (- (truncate nyears 4) (truncate nyears 100))
       (truncate (+ nyears 300) 400) ) ) )


;;;; 25.2.1 decode-universal-time
;;;
;;; Returns nine values:
;;;     1. second
;;;     2. minute
;;;     3. hour
;;;     4. date (ay of month)
;;;     5. month
;;;     6. year
;;;     7. day of week
;;;     8. day-light-p
;;;     9. timezone
;
(defun cl:decode-universal-time (u &optional z)
    (declare (type (integer 0) u))
  (let ((d-p nil))
    (unless z
      (multiple-value-bind (s n h d m y dow daylight-p zone)
          (get-decoded-time)
          (declare (ignore s n h d m y dow))
        (setq d-p daylight-p)
        (setq z zone) ))
    (let (s n h d m y dow)
      (decf u (* z 3600))
      (multiple-value-setq (d u) (floor u #.(* 24 3600)))
      (setq dow (mod d 7))
      (multiple-value-setq (h u) (floor u 3600))
      (multiple-value-setq (n s) (floor u 60))
      (setq y (+ 1900 (floor d 366)))
      (loop
        (let ((ndays (- d (* (- y 1900) 365) (leap-years-before y))))
          (when (< ndays (if (leap-year-p y) 366 365))
            (setq d (1+ ndays))
            (return) )
          (incf y) ))
      (when (leap-year-p y)
        (when (= 60 d)
          (return-from decode-universal-time (values s n h 29 2 y dow d-p z)) )
        (when (> d 60) (decf d)) )
      (setq m 2)
      (loop
        (when (<= d (svref *days-before-month* m))
          (decf m)
          (decf d (svref *days-before-month* m))
          (return) )
        (incf m) )
      (values s n h d m y dow d-p z) ) ) )


;;;; 25.2.2 encode-universal-time
;
(defun cl:encode-universal-time (s n h d m y &optional z)
    (declare (type (mod 60) s))
    (declare (type (mod 60) n))
    (declare (type (mod 24) h))
    (declare (type (integer 1 31) d))
    (declare (type (integer 1 12) m))
    (declare (type (or (integer 0 99) (integer 1900)) y))
    (declare (type (or null rational) z))
    (declare (values (integer 0 *)))

  (when (< y 100)
    (let ((year (nth-value 5 (get-decoded-time))))
      (incf y (- year (mod year 100)))
      (cond
        ((<  (- y year) -50) (incf y 100))
        ((>= (- y year)  50) (decf y 100)) ) ))

  (when (null z)
    (multiple-value-bind (s n h d m y dow daylight-p zone)
        (get-decoded-time)
        (declare (ignore s n h d m y dow))
      (when daylight-p
        (if (plusp zone) (decf zone) (incf zone)) )
        (setq z zone) ))

  (let* ((ndays (+ (1- d)
                   (svref *days-before-month* m)
                   (if (> m 2)
                       (leap-years-before (1+ y))
                     (leap-years-before y) )
                   (* (- y 1900) 365) ))
         (nhours (+ h (* ndays 24))) )
    (+ s (* (+ n (* (+ nhours z) 60)) 60)) ) )


;;;; 25.2.3 get-universal-time
;
(defun cl:get-universal-time ()
  (multiple-value-bind (s n h d m y day daylight-p zone)
      (get-decoded-time)
      (declare (ignore day))
      (declare (ignore daylight-p))
    (encode-universal-time s n h d m y zone) ) )
