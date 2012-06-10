;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x86x64 - runtime - 3 Evaluation and Compilation
;;; arch/x86/lisp/runtime/x86-r03-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/runtime/x86x64-r03-defs.lisp#1 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

;;;; fundesc
(defclass fundesc (foreign-object)
  ((gcmap-offset    :type uint32)
   (annot-offset    :type uint32)
   (code-size       :type uint32)
   (frame           :type uint32) )
  (:metaclass foreign-class) )
