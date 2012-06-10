;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 21 Streams
;;; macro/m21-stream.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m21-stream.lisp#8 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;; Public Macros:
;;;     with-input-from-string      21.2.51
;;;     with-open-file              21.2.31
;;;     with-open-stream            21.2.33
;;;     with-output-to-string       21.2.52
;
(in-package :xc)

;;;; 21.2.51 with-input-from-string
(defmacro cl:with-input-from-string ((var string &key index (start 0) end)
                                     &body body )
  (if (not index)
      `(let ((,var (make-string-input-stream ,string ,start ,end)))
         ,@body )
    `(let ((,var (make-string-input-stream ,string ,start ,end)))
        (multiple-value-prog1 (locally ,@body)
          (setf ,index (slot-value ,var 'si::index)) ) )) )


;;;; 21.2.31 with-open-file
(defmacro cl:with-open-file ((stream filespec &rest option*) &body body)
  (let ((abort (gensym "abort")))
    (multiple-value-bind (decl* form*) (analyze-body body nil)
     `(let ((,stream (open ,filespec ,@option*))
            (,abort t) )
          (declare (type (or stream null) ,stream))
          ,@decl*
        (unwind-protect
            (multiple-value-prog1 (progn ,@form*) (setq ,abort nil))
          (si::close/2 ,stream ,abort) ) ) ) ) )


;;;; 21.2.33 with-open-stream
(defmacro cl:with-open-stream ((var stream) &body body)
  (multiple-value-bind (decl* form*) (analyze-body body nil)
   `(let ((,var ,stream))
       (declare (type stream ,var))
       ,@decl*
      (unwind-protect (progn ,@form*) (close ,var)) ) ) )


;;;; 21.2.52 with-output-to-string
(defmacro cl:with-output-to-string
    ((var &optional string-form &rest args &key element-type) &body body)
        (declare (ignore element-type))
  (cond
    (string-form
      `(let* ((,var (make-instance 'si::string-output-stream
                        :string ,string-form
                        ,@args )) )
           ,@body ) )
    (args
      `(let* ((,var (make-instance 'si::string-output-stream ,@args)))
         ,@body
         (get-output-stream-string ,var) ) )
    (t
     `(with-pool (,var si::string-output-stream)
        ,@body
        (get-output-stream-string ,var) ) )) )
