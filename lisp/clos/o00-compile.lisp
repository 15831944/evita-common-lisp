;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - BOOT - Load CLOS modules
;;; lisp/clos/o00-loadup.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o00-compile.lisp#2 $
;;;
;;; Description:
;;;  This file contains forms for creating slot accessors.
;;;
;
;(in-package :cl-user)
;(load "o01-defpackage")

(in-package :si)

(eval-when (:compile-toplevel)
  (error "This file must be load instead of compile-file.") )

(defvar *nwarnings* 0)


;;;; Printer's for debugging
;
#+nil
(progn
  (si::install-class-printer
    'built-in-class
    #'(lambda (object stream)
          (declare (ext:lambda-name (print-object built-in-class)))
        (print-unreadable-object (object stream :type t :identity t)
          (prin1 (si::.class-name object) stream) ) ))

  (si::install-class-printer
    'funcallable-standard-class
    #'(lambda (object stream)
          (declare (ext:lambda-name (print-object funcallable-standard-class)))
        (print-unreadable-object (object stream :type t :identity t)
          (prin1 (si::.class-name object) stream) ) ))

  (si::install-class-printer
    'standard-class
    #'(lambda (object stream)
          (declare (ext:lambda-name (print-object standard-class)))
        (print-unreadable-object (object stream :type t :identity t)
          (prin1 (si::.class-name object) stream) ) ))

  (si::install-class-printer
    'standard-generic-function
    #'(lambda (object stream)
          (declare (ext:lambda-name (print-object standard-generic-function)))
        (print-unreadable-object (object stream :type t :identity t)
          (prin1 (si::.generic-function-name object) stream) ) ))

  (si::install-class-printer
    'standard-method
    #'(lambda (o s)
          (declare (ext:lambda-name (print-object standard-method)))
        (print-unreadable-object (o s :type t :identity t)
          (let ((gf (slot-value o 'generic-function)))
            (prin1 (and gf (slot-value gf 'name)) s)
            (write-char #\Space s)
            (pprint-newline :linear s)
            (prin1 (mapcar
                      #'(lambda (specializer)
                          (typecase specializer
                            (class (class-name specializer))
                            (eql-specializer
                              `(eql ,(eql-specializer-object specializer)) )
                            (otherwise specializer) ) )
                      (slot-value o 'specializers) )
                   s ) )) ))
 ) ; progn


;;;; compile-and-load
;
#+genesis
(defun compile-and-load (basename)
  (format t "; Loading ~S~%" basename)
  (force-output)
  (load basename :print t) )

#-genesis
(defun compile-and-load (basename)
  (labels (
    (file-newer-p (file1 file2)
      (> (safe-file-write-date file1) (safe-file-write-date file2)) )

    (safe-file-write-date (filespec)
      (or (ignore-errors (file-write-date filespec)) 0) )
    )
    ;;
    ;; compile-and-load
    ;;
  (let ((srcname
          (merge-pathnames basename
                           (make-pathname
                                :host (pathname-host basename)
                                :type "LISP"
                                :case :common ) ) )
        (objname (compile-file-pathname basename)) )
    (when (file-newer-p srcname objname)
      (multiple-value-bind (fasl warnings-p failure-p)
         (compile-file srcname)
         (declare (ignore fasl))
      (when failure-p
        (format t "~2&Failed to compile: ~S ~S ~S~2%"
                srcname failure-p warnings-p )
        (break) )
      (incf *nwarnings* (or warnings-p 0)) ))
    (load objname :print t) ) ) )

(setq *nwarnings* 0)

(compile-and-load "../lisp/clos/o02-defs.lisp")
(compile-and-load "../lisp/clos/o03-boot.lisp")
(compile-and-load "../lisp/clos/o04-emf.lisp")
(compile-and-load "../lisp/clos/o05-dfun.lisp")
(compile-and-load "../lisp/clos/o06-cdfun.lisp")
;(compile-and-load "../lisp/clos/o10-defclass.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From here, we can make generic-function and method.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set param-info slot of generic-functions that created by BOOT CLOS.
;
(labels (
  (patch (class)
    (loop
      for method in (specializer-direct-methods class)
      for gf = (method-generic-function method) do
        (when (and (null (slot-value gf 'param-info))
                   #+nil (not (eq (slot-value gf 'name) 'class-name)) )
          (format t "; Set lambda-list ~S~%" (slot-value gf 'name))
          (force-output)
          (set-generic-function-lambda-list
            gf
            (method-lambda-list method)
            nil )))
    (dolist (subclass (class-direct-subclasses class))
      (patch subclass) ) )
  )
  ;;
  (patch (find-class 't))
 ) ;; labels


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load methods
;;;

(compile-and-load "../lisp/clos/o11-method.lisp")
(compile-and-load "../lisp/clos/o12-class.lisp")
(compile-and-load "../lisp/clos/o13-slot.lisp")
(compile-and-load "../lisp/clos/o20-struct.lisp")


;;; Convert early generic-function to real general-funciton.
(labels (
  (convert-to-gf (fname slambda-list)
    (when (typep (fdefinition fname) 'generic-function)
      (return-from convert-to-gf) )

    (let* ((lambda-list  (extract-lambda-list slambda-list))
           (specializers (extract-specializer-names slambda-list))
           (gf (make-instance
                 'standard-generic-function
                 :name fname ) )
           (mt (make-instance
                 'standard-method
                 :specializers (mapcar 'find-class specializers)
                 :lambda-list  lambda-list
                 :function     (fdefinition fname) ) ))
      (format t "; Convert to generic-function ~S~%" fname)
      (add-method gf mt)
      ;(set-funcallable-instance-function gf (compute-discriminator gf))
      (setf (fdefinition fname) gf) ) )
  )
  ;;
  (convert-to-gf 'make-method-lambda
                 '((gf standard-generic-function)
                   (mt standard-method)
                   form-lambda
                   env ))
  (convert-to-gf 'compute-discriminating-function
                 '((gf standard-generic-function)) )
 ) ; labels


;;;
;;; Enable Generic Functions
;;;
;;; Note: Since discriminator uses slot-value, slot-value must handle
;;  instances of standard-class, standard-generic-function and
;;; standard-method without calling slot-value-using-class.
;;;
(loop for (fname . gf) in *early-generic-function-alist* do
  (format t "; Fix early generic-function ~S~%" fname)

  ;; For compiler in make-discriminator/*
  (when (eq 'print-object fname)
    (set-funcallable-instance-function gf (compute-discriminator gf)) )

  (setf (fdefinition fname) gf) )


;;;
;;; Install class printers for debugging
;;;

#|
(dolist (name '(
    standard-direct-slot-definition
    standard-effective-slot-definition
    standard-generic-function
   ))
  (si::install-class-printer
    name
    #'(lambda (o s)
        (print-unreadable-object (o s :type t :identity t)
          (prin1 (slot-value o 'name) s) ) )) )


(si::install-class-printer
  'standard-method
  #'(lambda (o s)
      (print-unreadable-object (o s :type t :identity t)
        (let ((gf (slot-value o 'generic-function)))
          (prin1 (and gf (slot-value gf 'name)) s)
          (write-char #\Space s)
          (pprint-newline :linear s)
          (prin1 (mapcar
                    #'(lambda (specializer)
                        (typecase specializer
                          (class (class-name specializer))
                          (eql-specializer
                            `(eql ,(eql-specializer-object specializer)) )
                          (otherwise specializer) ) )
                    (slot-value o 'specializers) )
                 s ) )) ))
|#

(when (plusp *nwarnings*)
  (format t "; ~D warning~:P~%" *nwarnings*) )
