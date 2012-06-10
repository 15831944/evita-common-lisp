;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Developer - 25 Environment
;;; dev/r25-env.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-time.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     time            25.2.10
;
(in-package :si)

;;;; *class-vector*
;;;
;;; Description:
;;;  This simple vector contains class objects. Index is ClassD.m_nTypeCode.
;
(defvar si::*class-vector*)


;;;; Report times
;;;
;;; Syntax:
;;;     time-it function => {result}*
;;;
;;; Arguments and Values:
;;;     function    function which takes no arguments.
;;;
;;; Used by:
;;;     time
;
(defun time-it (func-0)
  (let ((count-vec (make-array 256 :initial-element 0))
        (size-vec  (make-array 256 :initial-element 0))
        real-1 user-1 syst-1
        real-2 user-2 syst-2
        gc-real-1 gc-user-1 gc-syst-1
        gc-real-2 gc-user-2 gc-syst-2
        gc-count-1
        gc-count-2
        gc-real gc-user gc-syst
        results )

    (let* ((count-addr (si::get-thread-object-statistics))
           (size-addr  (+ count-addr 256)) )
      (loop for i from 0 to 255 do
        (setf (svref count-vec i) (ubyte32 count-addr i))
        (setf (svref size-vec i)  (ubyte32 size-addr  i)) ) )

    (multiple-value-setq (gc-real-1 gc-user-1 gc-syst-1 gc-count-1)
        (get-gc-times) )

    (multiple-value-setq (real-1 user-1 syst-1) (get-thread-times))

    (setq results (multiple-value-list (funcall func-0)))

    (multiple-value-setq (real-2 user-2 syst-2) (get-thread-times))

    (multiple-value-setq (gc-real-2 gc-user-2 gc-syst-2 gc-count-2)
        (get-gc-times) )

    (setq gc-real (- gc-real-2 gc-real-1))
    (setq gc-user (- gc-user-2 gc-user-1))
    (setq gc-syst (- gc-syst-2 gc-syst-1))

    (let* ((count-addr (si::get-thread-object-statistics))
           (size-addr  (+ count-addr 256)) )
      (loop for i from 0 to 255 do
        (setf (svref count-vec i)
              (- (ubyte32 count-addr i) (svref count-vec i)) )

        (setf (svref size-vec i)
              (- (ubyte32 size-addr i)  (svref size-vec i)) )) )

    ;; Decrement conses made by multiple-value-list.

    (decf (svref count-vec T_Cons)
        (length results) )

    (decf (svref size-vec  T_Cons)
        (* (length results) |sizeof LispVal| |lengthof Cons|) )

    ;; Show times
    ;;
    (format t "~&")

    (format t "; CPU-time (non-gc) ~,2F sec user, ~,2F sec system~%"
            (/ (- user-2 user-1 gc-user) 1000d0)
            (/ (- syst-2 syst-1 gc-syst) 1000d0) )

    (format t "; CPU-time (gc)     ~,2F sec user, ~,2F sec system~%"
            (/ gc-user 1000d0)
            (/ gc-syst 1000d0) )

    (format t "; Elapsed Time      ~,2F sec real, ~,2F sec gc~%"
            (/ (- real-2 real-1) 1000d0)
            (/ gc-real 1000d0) )

    (when (/= gc-count-2 gc-count-1)
      (format t "; GC: ~D time~:P~%" (- gc-count-2 gc-count-1)) )

    (write-line "; ")

    (loop
       for i from 0 below (length count-vec)
       with total-items = 0
       with total-size  = 0
       unless (zerop (svref count-vec i))
         collect
           (progn
             (incf total-items (svref count-vec i))
             (incf total-size  (svref size-vec  i))
             (list (if (svref si::*class-vector* i)
                       (class-name (svref si::*class-vector* i))
                     i )
                   (svref count-vec i)
                   (svref size-vec  i) ))
         into name-count-size-list

      finally
        (when (null name-count-size-list) (return))

        (setq name-count-size-list
              (sort name-count-size-list #'> :key #'third) )

        (write-line "; Allocated objects:")
        (format t ";   ~30A ~12@A ~12@A~%"
                  "Type"
                  "Items"
                  "Byte" )
        (write-line ";   --------------------------------------------------------")
        (format t "~:{;   ~30S ~12:D ~12:D~%~}" name-count-size-list)
        (write-line ";   ========================================================")
        (format t ";   ~30A ~12:D ~12:D~%;~%"
                "Total"
                total-items
                total-size ))

    (values-list results) ) )
