;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 17 Sequences
;;; macro/m17-sequence.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m17-sequence.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     subseq          17.3.6
;
(in-package :xc)

;;;; 17.3.6 subseq
;
(defsetf cl:subseq (sequence start &optional end) (new-sequence)
  (let ((var-seq    (gensym "v"))
        (var-start  (gensym "s"))
        (var-end    (gensym "e"))
        (var-newseq (gensym "n")) )
    `(let ((,var-seq    ,sequence)
           (,var-start  ,start)
           (,var-end    ,end)
           (,var-newseq ,new-sequence) )
       (replace ,var-seq ,var-newseq :start1 ,var-start :end1 ,var-end)
       ,var-newseq ) ) )
