;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 50 Extensions - Miscellaneous
;;; macro/m50-misc.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m50-misc.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     defenum
;;;     with-collector
;
(in-package :xc)

;;;; defenum
;
(defmacro ext::defenum (name (&rest option*) &rest enum*)
    (declare (ignore option*))
  (loop
    with val = -1
    with cur-val = 0
    with doc-string = nil
    for enum in enum*
     if (symbolp enum) do
        (setq cur-val (incf val))
        (setq doc-string nil)
     else if (consp enum) do
       (destructuring-bind (name-1 &optional val-1 doc-1) enum
         (when (null val-1) (setq val-1 (incf val)))
         (unless (typep val-1 'fixnum)
           (error "Enumerated value(~S) for ~S must be an fixnum."
             val-1 name-1 ))
         (setq enum name-1)
         (setq cur-val val-1)
         (setq doc-string doc-1)
         (setq val val-1) )
    else do
      (error "Invalid enumeration: ~S~%" enum)
    end
    do (setq enum (intern (format nil "~A-~A" name enum)))
    minimize cur-val into min-val
    maximize cur-val into max-val
    collect `(defconstant ,enum ,cur-val ,@(and doc-string (list doc-string)))
      into form*
    unless (member cur-val vals)
      collect cur-val into vals
    finally
      (let ((typespec
              (if (eql (length vals) (- max-val min-val -1))
                  `(integer ,min-val ,max-val)
                `(member ,@vals) ) ))
        (return `(progn (deftype ,name () ',typespec) ,@form* ',name)) )) )


;;;; with-collector
;
(defmacro ext:with-collector ((collector) &body body)
  (unless (and collector (symbolp collector) (not (keywordp collector)))
    (error "Collector name must be a function-name: ~S" collector) )
  (let ((head (gensym "head_"))
        (tail (gensym "tail_")) )
  `(let* ((,head (list 0))
          (,tail ,head) )
       (declare (type cons ,head ,tail))
     (macrolet (
       (,collector (arg)
          `(setq ,',tail (setf (cdr ,',tail) (list ,arg))) )
        )
        ,@body )
     (cdr ,head) ) ) )
