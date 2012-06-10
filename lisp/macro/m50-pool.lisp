;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 49 Internal
;;; macro/m49-internal.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m50-pool.lisp#3 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     define-pool
;;;     without-context-switch
;
(in-package :xc)

;;;; defne-pool
;;;
;;; BUGBUG: NYI: marcher, lambda-list
;
(defmacro ext:define-pool (name lambda-list
                       &key (constructor (required))
                            initializer
                            finalizer
                       &aux (var-obj '#:object) )
  (labels (
    ;; compute-make
    (compute-make ()
      (multiple-value-bind (runner reqs opts rests keys auxs)
          (analyze-lambda-list lambda-list)
        (when runner (macro-error "Invalid lambda-list: ~S" lambda-list))
        (when auxs (macro-error "Can't use &aux here."))
        (cond
          ((or rests keys)
            (let ((var-rest '#:rest))
              (check-opts opts)
              (check-keys keys)
              (values
                `(,@reqs &rest ,var-rest)
                (make-apply constructor `(,@reqs ,var-rest))
                (make-apply initializer `(,var-obj  ,@reqs ,var-rest)) ) ) )
          (opts
            (let ((arg* `(,@reqs ,@(mapcar #'first (rest opts)))))
              (values
                `(,@reqs ,@opts)
                (make-funcall constructor arg*)
                (make-funcall initializer `(,var-obj ,@arg*)) ) ) )
          (t
            (values
                reqs
                (make-funcall constructor reqs)
                (make-funcall initializer `(,var-obj ,@reqs)) ) )) ) )

    ;; check-opts
    (check-opts (opts)
      (dolist (opt (rest opts))
        (when (rest opt) (macro-error "Can't use initform here.")) ) )

    ;; check-keys
    (check-keys (keys)
      (dolist (key keys)
        (when (fifth key) (macro-error "Can't use init form here.")) ) )

    ;; make-apply
    (make-apply (fn arg*)
      (when fn (if (symbolp fn) `(apply ',fn ,@arg*) `(apply ,fn ,@arg*))) )

    ;; make-funcall
    (make-funcall (fn arg*)
      (when fn (if (symbolp fn) `(,fn ,@arg*) `(funcall ,fn ,@arg*))) )
    )
    ;;
    (let ((fn-free  (intern (format nil "FREE-POOLED-~A" name)))
          (fn-make  (intern (format nil "MAKE-POOLED-~A" name))) )

      (multiple-value-bind (make-ll cons-form init-form)
          (compute-make)

      `(progn
         (eval-when (:compile-toplevel)
           (compile-notice 'define-pool ',name) )

         ;; free-pooled-xxx
         (defun ,fn-free (,var-obj)
           ,@(when finalizer (list (make-funcall finalizer `(,var-obj))))
           (push ,var-obj (gethash ',name si::*object-pool*))
           nil )

         ;; make-pooled-xxx
         (defun ,fn-make ,make-ll
           ,(if (null init-form)
                `(or (pop (gethash ',name si::*object-pool*))
                     ,cons-form )
            `(let ((,var-obj (pop (gethash ',name si::*object-pool*))))
               (if ,var-obj
                   (progn ,init-form ,var-obj)
                 ,cons-form ) )) )

        ;; result
        ',name ) ) ) ) )


;;;; with-pool
(defmacro ext:with-pool ((var name) &body decl*-form*)
  (let ((make (intern (format nil "MAKE-POOLED-~A" name)
                      (symbol-package name) ) )
        (free (intern (format nil "FREE-POOLED-~A" name)
                      (symbol-package name) ) ))
    (multiple-value-bind (decl* form*) (analyze-body decl*-form* nil)
     `(let ((,var (,make))) (declare (type ,name ,var)) ,@decl*
        (unwind-protect (progn ,@form*) (,free ,var)) ) ) ) )
