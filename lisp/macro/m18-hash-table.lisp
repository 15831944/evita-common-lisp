;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Macro - 18 Hash Tables
;;; macro/m18-hash-table.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m18-hash-table.lisp#4 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     with-hash-table-iterator    18.2.12
;
(in-package :xc)

;;;; 18.2.12 with-hash-table-iterator
(defmacro cl:with-hash-table-iterator ((name hash-table) &body body)
  (let ((var-hash-table (gensym "htb_"))
        (var-vector     (gensym "vec_"))
        (var-count      (gensym "count_"))
        (var-index      (gensym "index_"))
        (*key           '#:key)
        (*val           '#:val) )
    `(let* ((,var-hash-table    ,hash-table)
            (,var-count         (hash-table-count ,var-hash-table))
            (,var-vector        (ref hash-table vector ,var-hash-table))
            (,var-index         2) )
          (declare (type sequence-index ,var-count))
          (declare (type sequence-index ,var-index))
        (macrolet (
          (,name ()
            `(loop
               (when (eql ,',var-count 0)
                 (return (values ,(make-ignorable-form nil)
                                 ,(make-ignorable-form nil)
                                 ,(make-ignorable-form nil) )))
               (let ((,',*key (svref ,',var-vector ,',var-index)))
                 (incf ,',var-index 2)
                 (unless (or (eq #.(si::free-slot-marker)    ,',*key)
                             (eq #.(si::removed-slot-marker) ,',*key) )
                   (let ((,',*val (svref ,',var-vector (1- ,',var-index))))
                     (decf ,',var-count)
                     (return (values ,(make-ignorable-form t)
                                     ,',*key
                                     ,',*val )) )) )) )
           )
           ,@body ) ) ) )
