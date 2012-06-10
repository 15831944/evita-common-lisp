;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 23 reader
;;; macro/m23-reader.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m23-reader.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     with-standard-io-syntax     23.2.12
;
(in-package :xc)


;;;; 23.2.12 with-standard-io-syntax
;;;
;;; Syntax:
;;;     with-standard-io-syntax {form}* => {result}*
;;;
;;; Note:
;;;  We use wrapper function instead of let to prevent re-compiling
;;;  when we add printer and reader control variables.
;;;
;
(defmacro cl:with-standard-io-syntax (&body body)
  `(si::funcall-with-standard-io-syntax
     #'(lambda ()
            (declare (ext:lambda-name (with-standard-io-syntax function)))
         ,@body )) )

