;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Macro - 50 Extensions - Locks
;;; macro/m50-lock.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m50-lock.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     with-latch
;;;     with-mutex
;
(in-package :xc)

;;;; with-latch
;;;
;;; Syntax:
;;;   with-latch (latch mode) decl* form* => value*
;;;
;;; Arguments and Values:
;;;     latch   -- A form produces latch object.
;;;     mode    -- A keyword :shared or :exclusive.
;;;     decl*   -- declarations.
;;;     form*   -- forms evaluated in locked latch.
;
(defmacro ext:with-latch
        ((latch &optional (mode :exclusive)) &body decl*-form*)
  (let ((var-latch '.latch))
    `(let ((,var-latch ,latch))
         (declare (type ext:latch ,var-latch))
       (ext:lock-latch ,var-latch ,mode)
       (unwind-protect
           (locally ,@decl*-form*)
         (ext:unlock-latch ,var-latch) ) ) ) )


;;;; with-mutex
;;;
;;; Syntax:
;;;   with-mutex (mutex) decl* form* => value*
;;;
;;; Arguments and Values:
;;;     mutex   -- A form produces mutex object.
;;;     decl*   -- declarations.
;;;     form*   -- forms evaluated in locked mutex.
;
(defmacro ext:with-mutex ((mutex) &body decl*-form*)
  (let ((var-mutex '.mutex))
    `(let ((,var-mutex ,mutex))
         (declare (type ext:mutex ,var-mutex))
       (ext:lock-mutex ,var-mutex)
       (unwind-protect
           (locally ,@decl*-form*)
         (ext:unlock-mutex ,var-mutex) ) ) ) )
