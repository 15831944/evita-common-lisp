;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 25 Environemnt - Describe
;;; lisp/devel/d25-describe.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-describe.lisp#3 $
;;;
;;; Description:
;;;
;;; Public Functions:
;;;     describe        25.2.6
;;;     describe-object 25.2.7
;
(in-package :si)

;;;; 25.2.6 describe
;
(defun cl:describe (object &optional (stream *standard-output*))
  (let ((*print-circle*   t)
        (*print-readably* nil)
        (*print-length*   7)
        (*print-level*    7)
        (*print-pretty*   t) )
    (fresh-line stream)
    (describe-object object stream)
    (terpri stream) )
  (values) )


;;;; describe-object array
;
(defmethod cl:describe-object ((array array) stream)
  (let ((*print-array* nil))
    (format stream "~@<~W~:_ is a ~{~D~^x~} ~:[simple-array~;adjustable array~].~:>~%"
        array
        (array-dimensions array)
        (adjustable-array-p array) )
    (if (eq (array-element-type array) 't)
        (format stream " It is general array and can contain any objects.~%")
      (format stream " It is specialized for type ~S.~%"
          (array-element-type array) ))
    (multiple-value-bind (displacement offset) (array-displacement array)
      (when displacement
        (format stream " It is displaced to ~S at ~D.~%"
            displacement
            offset )) ) ) )


;;;; describe-object vector
;
(defmethod cl:describe-object ((vector vector) stream)
  (let ((*print-array* nil))
    (format stream "~@<~W~:_ is a vector with ~D element~:P.~:>~%"
        vector
        (array-total-size vector) )

    (if (eq (array-element-type vector) 't)
        (format stream " It is general vector and can contain any objects.~%")
      (format stream " It is specialized for type ~S.~%"
          (array-element-type vector) ))

    (multiple-value-bind (displacement offset) (array-displacement vector)
      (when displacement
        (format stream " It is displaced to ~S at ~D.~%"
            displacement
            offset )) )

    (when (adjustable-array-p vector)
      (format stream " It is adjustable.~%") )

    (when (array-has-fill-pointer-p vector)
      (format stream " It has fill-pointer ~D.~%"
          (fill-pointer vector) )) ) )


;;;; describe-object cons
;
(defmethod cl:describe-object ((object cons) stream)
  (format stream "~@<~W~:_ is an object of~:_ built-in-class ~(~S~).~:>~%"
          object
          (class-name (class-of object)) )
  (format stream "  CAR ~W~%" (car object))
  (format stream "  CDR ~W~%" (cdr object)) )


;;;; 25.2.7 describe-object standard-object
;
(defmethod cl:describe-object ((object standard-object) stream)
  (let* ((class (class-of object))
         (slots (remove-if
                  #'(lambda (slotd)
                      (not (eq :instance
                               (slot-definition-allocation slotd) )) )
                  (class-slots class) )) )
    (format stream "~@<~S is an ~3Iinstance of class ~S.~@:>~2%"
            object (class-name class) )
    (when slots
      (format stream " It has ~D instance slots:~%" (length slots))
      (loop
        for slotd in (class-slots class)
        for slot-name = (slot-definition-name slotd)
        with tab-width =
              (loop for slotd in (class-slots class)
                    for slot-name = (slot-definition-name slotd)
                    maximize (+ 3 (length (symbol-name slot-name))) )
        do
          (if (slot-boundp object slot-name)
              (format stream  "  ~A~VT~W~%"
                      slot-name
                      tab-width
                      (slot-value object slot-name) )
            (format stream "  ~A~VT.. Unbound ..~%"
                    slot-name
                    tab-width )))) ) )


;;;; 25.2.7 describe-object standard-object
;
(defmethod cl:describe-object ((object structure-object) stream)
  (let* ((class (class-of object))
         (slots (class-slots class)) )
    (format stream "~@<~S is an ~3Iinstance of class ~S.~@:>~2%"
            object (class-name class) )
    (when slots
      (format stream " It has ~D slots:~%" (length slots))
      (loop
        for slotd in (class-slots class)
        for slot-name = (slot-definition-name slotd)
        with tab-width =
              (loop for slotd in (class-slots class)
                    for slot-name = (slot-definition-name slotd)
                    maximize (+ 3 (length (symbol-name slot-name))) )
        do
          (if (slot-boundp object slot-name)
              (format stream  "  ~A~VT~W~%"
                      slot-name
                      tab-width
                      (slot-value object slot-name) )
            (format stream "  ~A~VT.. Unbound ..~%"
                    slot-name
                    tab-width )))) ) )


;;;; 25.2.7 describe-object integer
;
(defmethod cl:describe-object ((n integer) s)
  (labels (
    (prime-p (n)
      (if (<= n 1)
          nil
        (loop
          for test-divisor = 2 then (1+ test-divisor)
          when (> (* test-divisor test-divisor) n) return t
          when (zerop (rem n test-divisor)) return nil )) )
    )
    ;;
    ;; describe-n
    ;;
    (format s "~D is ~[an integer zero~;an negative-integer~;a prime number~;a positive-integer~] (~S). ~D bit~:P are used to represent it.~%"
            n
            (cond
              ((zerop n)   0)
              ((minusp n)  1)
              ((prime-p n) 2)
              (t                3) )
            (type-of n)
            (integer-length n) )

    (loop for (base . ctrl-str) in '(
            ( 2 . "#b~,,' ,4:B")
            ( 8 . "#o~,,' ,3:O")
            (10 . "~:D")
            (16 . "#x~,,' ,4:X") ) do
      (format s "  Base ~D~12T~@?~%" base ctrl-str n) )
    (when (<= 1 n 3999)
      (format s "  Roman~12T~@R~%" n) )
    (format s "  English~12T~R~%" n) ) )


;;;; 25.2.7 describe-object symbol
;
(defmethod cl:describe-object ((sym symbol) s)
  (let ((package (symbol-package sym)))
    (format s "~S is ~:[an uninterned symbol~;an ~(~:*~A~) symbol of package ~A~].~%"
          sym
          (and package (nth-value 1 (find-symbol (symbol-name sym) package)))
          (and package (package-name package)) ) )

  (if (not (boundp sym))
      (format s "  It is unbound.~%")
    (format s "  Its value is ~:W.~%" (symbol-value sym)) )

  (when (fboundp sym)
    (format s "  Its function binding is ~S.~%" (symbol-function sym)) )

  (let ((class (find-class sym nil)))
    (format s "~@[  It is class name of ~S.~%~]" class) )

  (let ((class (si::find-structure sym nil)))
    (format s "~@[  It is structure name of ~S.~%~]" class) )

  (when (si::find-type sym nil)
    (format s "  It is type specfier.~%") )

  (let ((plist (symbol-plist sym)))
    (when plist
      (format s "  Its property list has ~D indicator/value pair~:P:~%"
              (ash (or (si::proper-list-p plist) 0) -1) )
      (loop
        (unless (consp plist) (return))
        (let ((key (pop plist))
              (val (and (consp plist) (pop plist))) )
          (format s "    ~W~20T~W~%" key val) ))) ) )
