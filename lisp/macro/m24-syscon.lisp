;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 24 System Construction
;;; macro/m24-system.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m24-syscon.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     with-compilation-unit   24.2.4
;;;
;;; See devel;d24-defs.lisp
;
(in-package :xc)


;;;; 24.2.4 with-compilation-unit
;;;
;;; Syntax:
;;;     with-compilation-unit (option*) {form}* => {result}*
;;;
;;; Note:
;;;  We use wrapper function instead of let to prevent re-compiling
;;;  when we change compiler.
;;;
;
(defmacro cl:with-compilation-unit ((&rest option*) &rest form*)
  (unless (si::proper-list-p option*)
    (error 'si::not-proper-list :datum option*) )
  `(xc::funcall-with-compilation-unit
      ,(loop
         with scan = option*
         with key = nil
         with val = nil
         with constant-p = t
         finally
           (if constant-p
               (return `',option*)
             (return `(list ,@options)) )
         while scan
           do
             (setq key (pop scan))
             (unless (eq :override key)
               (style-warn "Unknown compilation-unit option: ~S" key))
           collect `',key into options
           if (consp scan)
             do
               (setq val (pop scan))
               (unless (constantp val)
                 (setq constant-p nil) )
             and collect val into options
           else if (null scan)
             do (error 'missing-value-for-key :key key)
           end )
      #'(lambda ()
            (declare (ext:lambda-name (with-compilation-unit . function)))
          (progn ,@form*) )) )
