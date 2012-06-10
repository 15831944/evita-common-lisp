;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - devel - 25 Environment - roomt
;;;; devel/d25-room.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-room.lisp#2 $
;;;
;;; Description:
;;;  This file countains apropos functions.
;;;     room    25.2.16
;
(in-package :devel)

;;;; room
;
(defun cl:room (&optional (detail :default))
    (declare (type (member nil t :default) detail))
    (declare (values))
  (labels (
    (detail (detail)
        (declare (values (integer 1 2)))
      (ecase detail
        ((nil) 1)
        ((:default) 1)
        ((t) 2) ) )

    ;; report-objects
    (report-objects (objects)
        (declare (type list objects))
        (declare (values ext:unspecified))
      (multiple-value-bind (total-count total-size)
          (loop
            with total-count of-type fixnum = 0
            with total-size  of-type fixnum = 0
            for (class count size) in objects do
              (incf total-count count)
              (incf total-size  size)
            finally
              (return (values total-count total-size)) )
      (format t "; Objects:~%")
      (format t ";        Byte            Count           Class~%")
      (format t "; ------------------+------------+------------------------~%")

      (loop for (class count size) in objects do
          (format t "; ~12:D ~4,1,2F% ~12:D ~S~%"
              size
              (/ size total-size)
              count
              (class-name class) ) )

      (format t "; ------------------+------------+------------------------~%")
      (format t "; ~12:D byte  ~12:D Total~%" total-size total-count)
      (format t ";~%") ) )
    )
    (multiple-value-bind (start end objects areas)
        (si::memory-statistics (detail detail))
        (declare (ignore areas))

      (format t "; Memory:~%;   [#x~X, #x~X], ~,2FMB~%;~%"
          (ash start 2)
          (ash end 2)
          (/ (- end start) #.(ash 1 18)) )

      (when detail
        (setq objects (sort objects #'> :key #'third))
        (report-objects objects) )

      (values) ) ) )
