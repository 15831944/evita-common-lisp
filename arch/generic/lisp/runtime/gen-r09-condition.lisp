;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - runtime - 9 Conditions
;;; lisp/runtime/r09-condition.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r09-condition.lisp#3 $
;;;
;
(in-package :si)

;;;; make-restart
(defun make-restart (&key (name (required))
                          (function (required))
                          test-function
                          report-function
                          interactive-function )
    (declare (values cl:restart))
    (declare (type (or function null string) report-function))
  (let ((x (.allocate-record #.(class-description 'cl:restart))))
    (setf (ref restart name x) name)
    (setf (ref restart function x) function)
    (setf (ref restart test-function x) test-function)
    (setf (ref restart report-function x)
        (if (stringp report-function)
            (lambda (s) (write-string report-function s))
          report-function ))
    (setf (ref restart interactive-function x) interactive-function)
    x ) )


;;;; cl:restart-name
(defun cl:restart-name (x)
    (check-type x cl:restart)
  (ref restart name x) )
