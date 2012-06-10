;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 50 Extension - Finalization
;;; lisp/runtime/r50-finalization.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime3/r50-finalization.lisp#4 $
;;;
;;; Description:
;;;  This fils contains declarations for type system.
;
(in-package :si)

;;;; schedule-finalization
(defun ext:schedule-finalization (obj fun)
    (declare (values finalization))
    (declare (type function-designator fun))
  (labels (
    (make-finalization (obj fun)
      (let ((finrec (.allocate-weakobj #.(class-description 'finalization))))
        (setf (ref finalization object   finrec) obj)
        (setf (ref finalization function finrec) fun)
        (setf (ref finalization state    finrec) :scheduled)
        finrec ) )
    )
    ;;
    (let* ((finrec (make-finalization obj fun))
           (cons (list finrec)) )
      (with-latch (*finalizations-latch*)
        (setf (cdr cons) *finalizations*)
        (setq *finalizations* cons)
        finrec ) ) ) )


;;;; unschedule-finalization
(defun ext:unschedule-finalization (finrec)
    (declare (values t))
    (check-type finrec finalization)
  (setf (ref finalization state finrec) :unscheduled)
  (with-latch (*finalizations-latch*)
    (setq *finalizations* (delq finrec *finalizations*))
    finrec ) )
