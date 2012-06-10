;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Development - 1.8.1 Deprecated Function
;;; lisp/dev/d01-deprecated.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d01-deprecated.lisp#2 $
;;;
;;; Description:
;;; Description:
;;;  This file contains macros for runtime function:
;;;
;;;     assoc-if-not                14.2.36
;;;     count-if-not                17.3.10
;;;     delete-if-not               17.3.22
;;;     find-if-not                 17.3.14
;;;     gentemp                     10.2.9
;;;     member-if-not               14.2.33
;;;     nsubst-if-not               14.2.12
;;;     nsubstitute-if-not          17.3.19
;;;     position-if-not             17.3.15
;;;     provide                     NYI
;;;     rassoc-if-not               14.2.39
;;;     remove-if-not               17.3.22
;;;     require                     NYI
;;;     set                         boot
;;;     subst-if-not                14.2.12
;;;     substitute-if-not           17.3.19
;;;
;;;  :test-not argument functions:
;;;     adjoin
;;;     assoc
;;;     count
;;;     delete
;;;     deelte-duplicates
;;;     find
;;;     intersection
;;;     member
;;;     mismatch
;;;     nintersection
;;;     nset-difference
;;;     nset-exclusive-or
;;;     nsublist
;;;     nsubst
;;;     nsubstitute
;;;     nunion
;;;     position
;;;     rassoc
;;;     remove
;;;     remove-duplicates
;;;     search
;;;     set-difference
;;;     set-exclusive-or
;;;     sublis
;;;     subsetp
;;;     subst
;;;     substitute
;;;     tree-equal
;;;     union
;;;
;;; Deprecated variables:
;;;     *module*
;
(in-package :si)

;;;; define-test-not
;;;
;;; Note: This is complex backquote example. :-)
;
(macrolet (
  (define-test-not (name &rest arg*)
    (let ((name-not    (intern (format nil "~A-NOT" name)))
          (lambda-list (append arg* (list '&rest 'args))) )

      (unless (eq #.(symbol-package 'car) (symbol-package name-not))
        (error "Bad deprecated function name: ~S" name-not) )

      (unless (position 'test arg*)
        (error "Parameter TEST must be specified ~S:" name) )

      `(progn
         (defun ,name-not ,lambda-list
             (declare (dynamic-extent args))
           (apply #',name ,@(subst '(complement test) 'test arg*) args) )

         (define-compiler-macro ,name-not ,lambda-list
           `(,',name ,@(list
                        ,@(loop for arg in arg*
                                if (eq 'test arg)
                                  collect '`(complement ,test)
                                else
                                  collect `,arg ) )
                     ,@args ) )) ) )
  )
  ;;
  (define-test-not cl:assoc-if       test)
  ;(define-test-not cl:count-if       test)
  ;(define-test-not cl:delete-if      test)
  ;(define-test-not cl:find-if        test)
  (define-test-not cl:member-if      test)
  (define-test-not cl:nsubst-if      new test)
  ;(define-test-not cl:nsubstitute-if new test)
  ;(define-test-not cl:position-if    test)
  (define-test-not cl:rassoc-if      test)
  ;(define-test-not cl:remove-if      test)
  (define-test-not cl:subst-if       new test)
  ;(define-test-not cl:substitute-if  new test)
 ) ; macrolet


;;;; *gentemp-counter*
;;;
;;; Description:
;;;  Internal name generation counter, similar to *gensym-counter*.
;
(defvar *gentemp-counter* 0)

;;;; 10.2.9 gentemp
;
(defun cl:gentemp (&optional (prefix "T") (package *package*))
  (check-type prefix string)

  (loop
   (let ((name
           (with-output-to-string (stream)
             (funcall (formatter "~A~D") stream prefix *gentemp-counter*) ) ))
     (multiple-value-bind (symbol type)
         (intern name package)
       (unless type
         (return symbol) )
       (incf *gentemp-counter*) ) )) )
