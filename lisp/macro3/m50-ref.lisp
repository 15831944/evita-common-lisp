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
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro3/m50-ref.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     defenum
;;;     with-collector
;
(in-package :xc)

;;;; ext:ref
(defmacro ext:ref (class-name slot-name x &environment env)
  (labels (
    (map-class-name (class-name)
      (case class-name
        ((si::classd) 'si::class-description)
        (otherwise class-name) ) )
    )
    ;;
    (let ((class (find-class (map-class-name class-name) t env)))
      (when (and (typep class 'standard-class)
                 (not (clos:class-finalized-p class)) )
        (clos:finalize-inheritance class) )
      `(si::%ref ',class-name ',slot-name ,x) ) ) )
