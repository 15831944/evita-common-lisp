;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 25 Environment
;;; runtime/r25-env.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r25-env.lisp#2 $
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

;;;; 25.2.4 sleep
;
(defun sleep (second)
  (error "NYI: sleep") )


;;;; 25.2.30 user-homedir-pathname
;
(defun user-homedir-pathname (&optional host)
    (declare (ignore host))
  (pathname (concatenate 'string (getenv "HOMEDRIVE") (getenv "HOMEPATH"))) )
