;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 22 Printer
;;; macro/m22-printer.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m22-printer.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     print-unreadable-object         22.4.12
;
(in-package :xc)

;;;; 22.4.12 print-unreadable-object
(defmacro cl:print-unreadable-object ((object stream &key type identity)
                                      &rest form* )
  (if (null form*)
      `(si::print-unreadable-object-function
        ,object ,stream ,type ,identity nil )
    (let ((fn '#:|print-unreadable-object|))
      `(flet ((,fn (,object ,stream)
                  (declare (ignorable ,object ,stream))
                (progn ,@form*) ))
            (declare (dynamic-extent #',fn))
          (si::print-unreadable-object-function
            ,object ,stream ,type ,identity #',fn ) ) )) )
