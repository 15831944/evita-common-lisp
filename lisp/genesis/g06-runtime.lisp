;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - genesis - Functions needed for loading macros.
;;; lisp/genesis/g06-runtime.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/genesis/g06-runtime.lisp#7 $
;;;
;;; Description:
;;;  This file contains function definitions for backquote macro.
;;;
(in-package :si)

;; for in-package
(defun cl:coerce (x ty)
  (labels (
    ;; unsupport
    (unsupport ()
      (error "Unsupported coerce ~S ~S~%" x ty) )
    )
    ;;
    (when (equal (type-of x) ty)
      (return-from cl:coerce x) )
    (cond
      ((eq ty 'vector)
        (typecase x
          (list
            (let ((v (make-simple-vector (length x)))
                  (i 0) )
              (dolist (e x v)
                (setf (svref v i) e) (incf i) ) ) )
          (otherwise (unsupport)) ) )
      ((eq ty 'list)
        (typecase x
          (simple-vector
            (let* ((anchor (list 0)) (last anchor))
              (dotimes (i (length x) (cdr anchor))
                (setq last (setf (cdr last) (list (svref x i)))) ) ) )
          (otherwise (unsupport)) ) )
      ((eq ty 'string)
        (typecase x
          (symbol (symbol-name x))
          (otherwise (unsupport)) ) )
      (t (unsupport)) ) ) )


;; length/list -- for export
(defun length/list (x) (declare (type list x))
  (let ((n 0)) (declare (type sequence-index n))
    (loop (when (endp x) (return n)) (incf n) (setq x (cdr x))) ) )


;; for with-package-iterator in find-all-symbols
(defun make-simple-vector (length &optional (initval nil initval-p))
    (declare (type array-total-size length))
    (declare (type t initval))
    (declare (values simple-vector))
  (let ((vector
          (.allocate-vector #.(class-description 'simple-vector) length) ))
    (when initval-p
      (dotimes (i length)
        (setf (svref vector i) initval) ))
    vector ) )

;; for with-package-iterator in find-all-symbols
(defun cl:vector (&rest elts)
  (let ((v (make-simple-vector (length elts))) (i 0))
    (dolist (elt elts v) (setf (svref v i) elt) (incf i)) ) )


;;; for package
#+nil
(defun cl:string (x)
  (etypecase x
    (string x)
    (symbol (symbol-name x)) ) )
