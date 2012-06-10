;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Dev - 24 System Construction - Defintions
;;; dev/d24-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d24-defs.lisp#2 $
;;;
;;; Description:
;;;  This file contains constants and special variables used by FASt Dump
;;; (FASD) and FASt Loader (FASL).
;;;
;;;  See d24-defs-fasd.lisp and d24-defs-fasl.lisp for format of FASD/FASL.
;
(in-package :devel)

;;;; *compile-print*
;
(ext:deftlv cl:*compile-print* t)

;;;; *compile-verbose*
;
(ext:deftlv cl:*compile-verbose* t)

(ext:deftlv *compile-nwarns* 0)
(ext:deftlv *compile-nfails* 0)
(ext:deftlv *compile-level*  0)
(ext:deftlv *undefined-functions* nil)


;;; For: eval-when
(ext:deftlv xc::*environment*)
(ext:deftlv xc::*situation*)
(ext:deftlv xc::*processing-mode*)
(ext:deftlv xc::*target*)


;;;; *load-level*
;;;
;;; Description:
;;;  Represents nesting level of load function.
;
(ext:deftlv *load-level* 0)
