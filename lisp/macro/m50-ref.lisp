;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 50 Extensions - Miscellaneous
;;; macro/m50-ref.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m50-ref.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     defenum
;;;     with-collector
;
(in-package :xc)

;;;; ext:ref
(defmacro ext:ref (class-name slot-name x)
  (labels (
    ;; expand
    (expand (class accessor offset)
      (unless (clos:class-finalized-p class)
        (clos:finalize-inheritance class) )
      (expand-aux class accessor offset) )

    ;; expand-aux
    (expand-aux (class accessor offset)
      (let ((eslotd (find slot-name (clos:class-slots class)
                        :key  #'clos:slot-definition-name
                        :test #'eq )) )
        (when (null eslotd)
          (error "~S doesn't have slot ~S. eslotds=~S" class slot-name
            (clos:class-slots class) ))
        (let ((location (clos:slot-definition-location eslotd)))
          (etypecase location
            (fixnum `(,accessor ,x ,(+ location offset)))
            (cons   `(slot-value ,x ',slot-name)) ) ) ) )

    ;; expand-built-in
    (expand-built-in (class)
      (let ((accessor (map-to-accessor)))
        (if (clos:class-finalized-p class)
            (expand-aux class accessor 0)
          (let ((location (position slot-name (clos:class-direct-slots class)
                                :key #'clos:slot-definition-name
                                :test #'eq ) ))
            (when (null location)
              (error "~S doesn't have slot ~S dslotds=~S."
                class slot-name
                (clos:class-direct-slots class) ))
            `(,accessor ,x ,location) )) ) )

    ;; map-class-name
    (map-class-name (class-name)
      (or (cdr (assoc class-name '(
                          (si::classd . si:class-description)
                          (si::env    . si::environment)
                          ;(si::slotd  . clos:standard-direct-slot-definition)
                          (method     . standard-method) )))
           class-name ) )

    ;; map-to-accessor
    (map-to-accessor ()
      (or (cdr (assoc class-name
                '((symbol . si::symbol-ref)
                  (si::instance            . si::.instance-ref)
                  (si:native-code-function . si::function-ref) )))
          'si:record-ref ) )
    )
    ;;
    (let ((class (find-class (map-class-name class-name))))
      (etypecase class
        (structure-class
          (expand-aux class 'si::record-ref 1) )
        (clos:funcallable-standard-class
          (expand class 'si::record-ref 2) )
        (standard-class
          (expand class 'si::instance-ref 2) )
        (built-in-class
          (expand-built-in class) )) ) ) )
