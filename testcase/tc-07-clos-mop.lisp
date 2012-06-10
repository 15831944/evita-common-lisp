;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-07-clos-mop.lisp#1 $
(in-package #:tc-user)

(deftest clos-07-90-001 ()
    (clos:extract-lambda-list '((p position))) (P) )

(deftest clos-07-90-002 ()
    (clos:extract-lambda-list '((p position) x y)) (P X Y) )

(deftest clos-07-90-003 ()
    (clos:extract-lambda-list '(a (b (eql x)) c &rest i)) (A B C &REST i) )


(deftest clos-07-91-001 ()
    (clos:extract-specializer-names '((p position))) (POSITION) )

(deftest clos-07-91-002 ()
    (clos:extract-specializer-names '((p position) x y)) (POSITION T T) )

(deftest clos-07-91-003 ()
    (clos:extract-specializer-names '(a (b (eql x)) c &rest i)) (T (EQL X) T) )

;;;; MOP compute-class-precedence-list
(deftest clos-07-92-001 ()
    (progn
      (setf (find-class 'a) nil)
      (setf (find-class 'b) nil)
      (setf (find-class 'c) nil)
      (setf (find-class 's) nil)
      (setf (find-class 'r) nil)
      (setf (find-class 'q) nil)

      (defclass a () ())
      (defclass b () ())
      (defclass c () ())
      (defclass s (a b) ())
      (defclass r (a c) ())
      (defclass q (s r) ())

      (mapcar #'class-name
              (clos:compute-class-precedence-list (find-class 'q)) ))
 (q s r a c b standard-object t) )

;;;; 7.7.8 change-class
;;; from FORWARD-REFERENCED-CLASS apple => STANDARD-CLASS.
(deftest clos-07-92-002 ()
    (progn
      (setf (find-class 'pie)      nil)
      (setf (find-class 'apple)    nil)
      (setf (find-class 'fruit)    nil)
      (setf (find-class 'cinnamon) nil)
      (setf (find-class 'spice)    nil)

      (defclass pie (apple cinnamon) ())
      (defclass apple (fruit) ())
      (defclass cinnamon (spice) ())
      (defclass fruit (food) ())
      (defclass spice (food) ())
      (defclass food () ())
      (mapcar #'class-name
              (clos:compute-class-precedence-list (find-class 'pie)) ))
  (pie apple fruit cinnamon spice food standard-object t) )

; (defclass error-class (fruit apple) ())
; inconsistent-class-precedence-list
