;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM; Base: 10 -*-
;;;;
;;;; regex - macros
;;; lisp/regex/regx-macros.lisp
;;;
;;; This file is part of Evita Common Lisp,
;;;
;;; Copyright (C) 1996-2007 by Project Vogue,
;;; Written by Yoshifumi "VOGUE" INOUE, (yosi@msn,com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/regex/regex-macros.lisp#2 $
;;;
;;; Description:
;;;  This file contains regex compiler,
;
(in-package :si)

;;;; do-match
(defmacro do-match ((var match &optional (result nil result-p))
                        &body decl*-form* )
  (let ((loop       '#:loop)
        (end-loop   '#:end-loop) )
  `(let ((,var ,match))
     (declare (type regex-match var))
    (block nil
      (tagbody
          (unless (matched-p ,var) (go ,end-loop))
       ,loop
          (locally ,@decl*-form*)
          (when (next-match ,var) (go ,loop))
       ,end-loop
          (return ,@(and result-p (list result))) )) ) ) )
