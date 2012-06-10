;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 07 Objects
;;; arch/generic/lisp/runtime/gen-r07-object.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r07-object.lisp#4 $
;;;
;;; Description:
;;;  This fils contains declarations for type system.
;
(in-package :si)

;;; REVIEW: offset of standard, structure, built-in, and funcallable are
;;; same are same. Can we optimize this? It is better to compile dose
;;; tail merge?
(macrolet (
  (class-ref (slot-name class)
   `(let* ((class ,class)
           (metaclass (class-of class))
           (storage   (ref instance storage class)) )
      (cond
        ((eq metaclass #.(find-class 'standard-class))
          (ref standard-class ,slot-name storage) )
        ((eq metaclass #.(find-class 'structure-class))
          (ref structure-class ,slot-name storage) )
        ((eq metaclass #.(find-class 'built-in-class))
          (ref built-in-class ,slot-name storage) )
        ((eq metaclass #.(find-class 'clos:funcallable-standard-class))
           (ref clos:funcallable-standard-class ,slot-name storage) )
        (t
          (slot-value class ',slot-name) )) ) )
  )

  ;;;; class-description
  (defun class-description (name)
    (class-ref instance-description (find-class name)) )

  ;;;; subclassp
  ;#+nil
  (defun subclassp (class1 class2)
      (declare (values t))
      (declare (type class class1 class2))
      ;; Note: Since subclassp is called by subtypep, we can't turn on type
      ;; checking.
      (declare (optimize (speed 3) (safety 0)))
    (if (eq class1 class2)
         t
      (let ((cpl1 (class-ref class-precedence-list class1)))
        (if cpl1
            (memq class2 cpl1)
          (dolist (super1 (class-ref direct-superclasses class1) nil)
            (when (subclassp super1 class2) (return t)) )) )) )
  ) ; macrolet


;;;; subclassp
#+nil
(defun subclassp (class1 class2)
    ;; BUGBUG: To avoid recursive call of subclassp.
    #+nil (declare (type class class1 class2))
    (declare (type t class1 class2))
    (declare (values t))
    (declare (optimize (speed 3) (safety 0) (space 0)))
  (macrolet (
    (*subclassp (cpl-form super-form)
     `(or (eq class1 class2)
          (let ((cpl1 ,cpl-form))
            (if cpl1
                (memq class2 cpl1)
              (dolist (super1 ,super-form nil)
                (when (subclassp super1 class2) (return t)) )) )) )
    )
   (let ((metaclass1 (class-of class1)))
     (if (or (eq #.(find-class 'standard-class) metaclass1)
             (eq #.(find-class 'clos:funcallable-standard-class) metaclass1)
             (eq #.(find-class 'built-in-class) metaclass1)
             (eq #.(find-class 'structure-class) metaclass1) )
           (*subclassp
            `(ref standard-class class-precedence-list class1)
            `(ref standard-class direct-superclasses   class1) )
        (*subclassp
            `(slot-value class1 'class-precedence-list)
            `(slot-value class1 'direct-superclasses) ) ) ) ) )


;;;; classp
(defun classp (x)
  (subclassp (class-of x) #.(find-class 'class)) )
