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
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m49-internal.lisp#3 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     define-pool
;;;     without-context-switch
;
(in-package :xc)

;;;; without-context-switch
;
#+nil
(defmacro si::without-context-switch ((&key fast) &rest form*)
  (let ((var (gensym))
        (MASK #x18) )
    (if fast
        `(let ((,var (si:set-thread-event-mask 0 ,MASK)))
              (declare (type fixnum ,var))
            (multiple-value-prog1
                (progn ,@form*)
              (si:set-thread-event-mask ,var ,MASK) ) )
    `(let ((,var (si:set-thread-event-mask 0 ,MASK)))
         (declare (type fixnum ,var))
       (unwind-protect
          (progn ,@form*)
         (si:set-thread-event-mask ,var ,MASK) ) )) ) )


;;;; without-garbage-collection
;
#+nil
(defmacro si::without-garbage-collection ((&key fast) &rest form*)
  (let ((var (gensym))
        (MASK #x08) )
    (if fast
        `(let ((,var (si:set-thread-event-mask 0 ,MASK)))
              (declare (type fixnum ,var))
            (multiple-value-prog1
                (progn ,@form*)
              (si:set-thread-event-mask ,var ,MASK) ) )
    `(let ((,var (si:set-thread-event-mask 0 ,MASK)))
         (declare (type fixnum ,var))
       (unwind-protect
          (progn ,@form*)
         (si:set-thread-event-mask ,var ,MASK) ) )) ) )


;;;; without-interrupt
;
(defmacro si::without-interrupt ((&key fast) &rest form*)
  (let ((var (gensym))
        (MASK #x10) )
    (if fast
        `(let ((,var (si:set-thread-event-mask 0 ,MASK)))
              (declare (type fixnum ,var))
            (multiple-value-prog1
                (progn ,@form*)
              (si:set-thread-event-mask ,var ,MASK) ) )
    `(let ((,var (si:set-thread-event-mask 0 ,MASK)))
         (declare (type fixnum ,var))
       (unwind-protect
          (progn ,@form*)
         (si:set-thread-event-mask ,var ,MASK) ) )) ) )
