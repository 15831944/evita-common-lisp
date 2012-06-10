;;; CLOS micro-benchmark
;;;
;;; KICZALES, G. and RODRIGUEZ, L. "Efficient method dispatch in PCL",
;;; Proceedings of Conference on LISP and Functional Programming, p. 99--105,
;;;  Nice, France, June 1990.
;
(in-package :cl-user)

(defun fun (x) 0)

(defclass c1 ()
  (( x :initform 0
       :accessor accessor1
       :accessor accessor2
       :accessor accessor3 )) )


(defclass c2 (c1)
  () )


(defclass c3 (c1)
  () )


(defmethod g1 ((f c1)) 0)
(defmethod g2 ((f c1)) 0)
(defmethod g2 ((f c2)) 0)

(defvar *outer-times* 3)
(defvar *inner-times* 100000)

(eval-when (:compile-toplevel :execute)

(defmacro test (&body body)
  `(let ((i1 (make-instance 'c1))
         (i2 (make-instance 'c2))
         (i3 (make-instance 'c3)) )
     (loop repeat *outer-times* do
       (time (loop repeat *inner-times* do ,@body)) ) ) )
) ; eval-when

(defun fun-test () (test (fun i1)))
(defun accessor1-test () (test (accessor1 i1)))

(defun accessor2-test ()
  (test (accessor2 i2) (accessor2 i2)) )

(defun accessor3-test ()
  (test (accessor3 i1) (accessor2 i2) (accessor3 i3)) )


(defun g1-test () (test (g1 i1)))
(defun g2-test () (test (g2 i2) (g2 i2)))

; evcl-2.0.0813.1-i686.8.3/731MHz
;   fun         0.03
;   accessor1   0.35
;   accessor2   0.78
;   accessor3   1.08
;   g1          0.39
;   g2          1.22

