;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 49 Internals - class description
;;; arch/generic/lisp/runtime/gen-r49-classd.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r49-classd.lisp#5 $
;;;
;;; Description:
;;;  This fils contains declarations for type system.
;
(in-package :si)

;;; Note: Only for make-class-description to retreive classd.m_format value.
(defun classd-format (classd)
    (declare (type class-description))
  (ref class-description format classd) )

;;; Note: Only for make-class-description to retreive classd.m_tag_code value.
(defun classd-tag-code (classd)
    (declare (type class-description))
  (ref class-description tag-code classd) )


;;;; make-class-description
(defun make-class-description (class slots)
    (declare (values class-description))
    (declare (type class class))
    (declare (type list slots)) ; list of effective-slot-definition
  (labels (
    ;; init
    (init (classd)
      (setf (ref classd class classd) class)
      (setf (ref classd hash-code classd) (sxhash (class-name class)))
      (setf (ref classd slots classd) slots)
      (setf (ref classd typespec classd) (class-name class))

      (let ((n (length slots))
            (format.instance
                #.(classd-format
                    (class-description 'standard-class)) )
            (format.structure
                #.(classd-format
                    (class-description 'structure-object) ) )
            (format.foreign
                #.(classd-format
                    (class-description 'foreign-object) ) )
            (format.function
                #.(classd-format
                    (class-description 'standard-generic-function) ) )
            (tag.instance
                #.(classd-tag-code
                    (class-description 'standard-class) ) )
            (tag.structure
                #.(classd-tag-code
                    (class-description 'structure-object) ) )
            (tag.function
                #.(classd-tag-code
                    (class-description 'standard-generic-function) )) )
        (etypecase class
          (standard-class
            ;; classd+storage+
            ;; classd+storaged+slot[0]...slot[n-1]
            (setf (ref classd format classd) format.instance)
            (setf (ref classd format-param classd) (+ n 4))
            (setf (ref classd format-misc  classd) n)
            (setf (ref classd tag-code classd) tag.instance) )
          (funcallable-standard-class
            ;; classd+storaged+slot[0]...slot[n-1]
            (setf (ref classd format classd) format.function)
            (setf (ref classd format-param classd) 4)
            (setf (ref classd format-misc  classd) n)
            (setf (ref classd tag-code classd) tag.function) )
          (structure-class
            ;; classd + slot[0]...slot[n-1]
            (setf (ref classd format classd) format.structure)
            (setf (ref classd format-param classd) (+ n 1))
            (setf (ref classd format-misc  classd) n)
            (setf (ref classd tag-code classd) tag.structure) )
          (foreign-class
            ;; classd + slot[0]...slot[n-1]
            (setf (ref classd format classd) format.foreign)
            (setf (ref classd format-param classd) (+ n 1))
            (setf (ref classd format-misc  classd) n)
            (setf (ref classd tag-code classd) 0) )) ) )
    )
    ;;
    (let ((classd (.allocate-record #.(class-description 'class-description))))
      (init classd)
      classd ) ) )
