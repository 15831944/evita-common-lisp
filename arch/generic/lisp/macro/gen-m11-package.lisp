;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Compiler; Base: 10 -*-
;;;;
;;;; evcl - macro - 11 Packages
;;; macro/m11-packages.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/macro/gen-m11-package.lisp#3 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     with-package-iterator   11.2.13
;
(in-package :xc)

;;;; 11.2.13 with-package-iterator
;;;
;;; Note: Since GC removes unreferenced symbol from package, we can't
;;; rely on total number of symbols in package.
(defmacro cl:with-package-iterator
    ((name package-list-form &rest symbol-type*) &body body)
  (let ((filter 0))
    (labels (
      (check (mask name)
        (if (zerop (logand filter mask))
            (setq filter (logior filter mask))
          (style-warn "Symbol-type ~S is appeared more than once."
              name ) ) )
      )
      ;;
      (dolist (symbol-type symbol-type*)
        (case symbol-type
          ((:external)  (check 1 symbol-type))
          ((:inherited) (check 2 symbol-type))
          ((:internal)  (check 4 symbol-type))
          (otherwise
            (macro-error "Invalid symbol-type ~S." symbol-type) )) )

      ;; Note: It isn't error that no symbol-types are specified.
      (when (zerop filter)
        (macro-error "At least one of ~S, ~S or ~S be specfied."
              :external
              :inherited
              :internal )) )
    (let ((var-package    (gensym "package_"))
          (var-task-list  (gensym "tasklist_"))
          (var-vector     (gensym "vector_"))
          (var-index      (gensym "index_"))
          (var-access     (gensym "access_")) )
    `(let ((,var-task-list
        ;; Build task-list
        (let* ((anchor (list 0))
               (last   anchor) )
            (declare (dynamic-extent anchor))
          (dolist (package (let ((packages ,package-list-form))
                             (if (listp packages) packages (list packages)) )
                  (cdr anchor) )
            (let ((package (si::ensure-package package)))
              ,@(when (logbitp 2 filter)
                  `((setq last (setf (cdr last) (list (vector
                        :internal
                        (ref package si::internal-table package)
                        package ))))))

              ,@(when (logbitp 0 filter)
                  `((setq last (setf (cdr last) (list (vector
                        :external
                        (ref package si::external-table package)
                        package ))))))


             ,@(when (logbitp 1 filter)
                `((dolist (package (package-use-list package))
                    (setq last (setf (cdr last) (list (vector
                            :inherited
                            (ref package si::external-table package)
                            package ))))))) ) ) ) ))

       ;; Process task-list
       (let ((,var-package  (find-package :cl))
             (,var-vector   '#(0))
             (,var-access   nil)
             (,var-index    0) )
           (declare (type package ,var-package))
           (declare (type simple-vector ,var-vector))
           (declare (type sequence-index ,var-index))
         (macrolet (
             (,name ()
               `(block next
                  (loop
                    ;; Loop while empty or removed slot.
                    (loop
                      (incf ,',var-index)
                      (when (eql (length ,',var-vector) ,',var-index)
                        (return) )
                      (let ((present (svref ,',var-vector ,',var-index)))
                        (when (symbolp present)
                          (return-from next
                            (values t
                                    present
                                    ,',var-access
                                    ,',var-package ))) ))

                    ;; FIXME 2007-03-08: Compiler signals "Use extra
                    ;; value warning."
                    (when (null ,',var-task-list)
                      (return-from next nil) )

                    (let ((task (pop ,',var-task-list)))
                        (declare (type simple-vector task))
                      (setq ,',var-access  (svref task 0))
                      (setq ,',var-vector  (svref task 1))
                      (setq ,',var-package (svref task 2))
                      (setq ,',var-index   0) ))) )
           ) ,@body ) ) ) ) ) )
