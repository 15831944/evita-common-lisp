;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: TC-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-18-cl-hash-table.lisp#1 $
(in-package :tc-user)

;;;; 18.2.3 hash-table-p
(deftest cl-18-02-03-001 () (bool (hash-table-p 1)) nil)
(deftest cl-18-02-03-002 () (bool (hash-table-p nil)) nil)
(deftest cl-18-02-03-003 () (bool (hash-table-p 'x)) nil)
(deftest cl-18-02-03-004 () (bool (hash-table-p (make-hash-table))) t)

;;;; 18.2.4 hash-table-count
(deftest cl-18-02-04-001 ()
  (hash-table-count (make-hash-table))
  0 )

(deftest cl-18-02-04-002 (:depend-to (cl-18-02-09-001))
  (let ((htb (make-hash-table)))
    (setf (gethash 1 htb) 'one)
    (hash-table-count htb) )
  1 )

(deftest cl-18-02-04-003 (:depend-to (cl-18-02-09-001))
  (let ((htb (make-hash-table)))
    (setf (gethash 1 htb) 'one)
    (setf (gethash 2 htb) 'two)
    (hash-table-count htb) )
  2 )

(deftest cl-18-02-04-004 (:depend-to (cl-18-02-09-001))
  (let ((htb (make-hash-table)))
    (setf (gethash 1 htb) 'one)
    (setf (gethash 1 htb) 'two)
    (hash-table-count htb) )
  1 )

;;;; 18.2.5 hash-table-rehash-size
(deftest cl-18-02-05-001 ()
  (let ((htb (make-hash-table)))
    (bool (typep (hash-table-rehash-size htb)
                '(or (integer 1 *) (float (1.0) *)) )) )
  t )

;;;; 18.2.6 hash-table-rehash-threshold
(deftest cl-18-02-06-001 ()
  (let ((htb (make-hash-table)))
    (bool (typep (hash-table-rehash-threshold htb) '(real 0 1))) )
  t )

;;;; 18.2.7 hash-table-size
(deftest cl-18-02-07-001 ()
  (let ((htb (make-hash-table)))
    (bool (typep (hash-table-size htb) '(integer 0 *))) )
  t )

;;;; 18.2.8 hash-table-test
(deftest cl-18-02-08-001 ()
  (let ((htb (make-hash-table)))
    (bool (eq (hash-table-test htb) 'eql)) )
  t )

;;;; 18.2.9 gethash
(deftest cl-18-02-09-001 ()
  (let ((htb (make-hash-table)))
    (setf (gethash 1 htb) 'one)
    (multiple-value-bind (v p) (gethash 1 htb) (values v (bool p))) )
  (values one t) )

(deftest cl-18-02-09-002 ()
  (let ((htb (make-hash-table)))
    (setf (gethash 1 htb) 'one)
    (multiple-value-bind (v p) (gethash 2 htb) (values v (bool p))) )
  (values nil nil) )

(deftest cl-18-02-09-003 ()
  (let ((htb (make-hash-table)))
    (setf (gethash 1 htb) 'one)
    (multiple-value-bind (v p) (gethash 2 htb 'two) (values v (bool p))) )
  (values two nil) )

;;;; 18.2.10 remhash
(deftest cl-18-02-10-001 (:depend-to (cl-18-02-09-001))
  (let ((htb (make-hash-table)))
    (setf (gethash 1 htb) "C")
    (bool (remhash 1 htb)) )
  t )

(deftest cl-18-02-10-002 (:depend-to (cl-18-02-09-001))
  (let ((htb (make-hash-table)))
    (setf (gethash 1 htb) "C")
    (bool (remhash 2 htb)) )
  nil )


;;; 18.2.11 maphash
(deftest cl-18-02-11-001 (:depend-to (cl-18-02-09-001))
    "Basic work"
  (let ((htb (make-hash-table)))
    (setf (gethash 'one htb) 1)
    (setf (gethash 'two htb) 2)
    (let ((sum 0))
      (maphash (lambda (k v) (declare (ignore k)) (incf sum v)) htb)
      sum ) )
  3 )

(deftest cl-18-02-11-002 ()
    "Check value of maphash"
  (let ((htb (make-hash-table)))
    (maphash 'contantly htb) )
  nil )


;;;; 18.2.12 with-hash-table-iterator
(deftest cl-18-02-12-001 ()
  (let ((htb (make-hash-table)))
    (with-hash-table-iterator (next htb)
      (next) ) )
  (values nil nil nil) )

(deftest cl-18-02-12-002 (:depend-to (cl-18-02-09-001))
  (let ((htb (make-hash-table)))
    (setf (gethash 'one htb) 1)
    (with-hash-table-iterator (next htb)
      (next) ) )
  (values t one 1) )

;;; 18.2.13 clrhash
(deftest cl-18-02-13-001 (:depend-to (cl-18-02-09-001))
  (let ((htb (make-hash-table)))
    (setf (gethash 'one htb) 1)
    (clrhash htb)
    (hash-table-count htb) )
  0 )

(deftest cl-18-02-13-002 (:depend-to (cl-18-02-09-001))
  (let ((htb (make-hash-table)))
    (setf (gethash 'one htb) 1)
    (bool (eq (clrhash htb) htb)) )
  t )

(deftest cl-18-02-13-003 (:depend-to (cl-18-02-09-001))
  (let ((htb (make-hash-table)))
    (setf (gethash 'one htb) 1)
    (clrhash htb)
    (gethash 'one htb) )
  (values nil nil) )


;;;; 18.2.14 sxhash
(defun cl-18-02-14-sxhash (x)
  (bool (eql (sxhash x) (sxhash x))) )

(deftest cl-18-02-14-001 () (cl-18-02-14-sxhash 0) t)
(deftest cl-18-02-14-002 () (cl-18-02-14-sxhash 1) t)
(deftest cl-18-02-14-003 () (cl-18-02-14-sxhash (1+ most-positive-fixnum)) t)
(deftest cl-18-02-14-004 () (cl-18-02-14-sxhash (ash 1 200)) t)
(deftest cl-18-02-14-005 () (cl-18-02-14-sxhash 1.23f0) t)
(deftest cl-18-02-14-006 () (cl-18-02-14-sxhash 1.23d0) t)
(deftest cl-18-02-14-007 () (cl-18-02-14-sxhash nil) t)
(deftest cl-18-02-14-008 () (cl-18-02-14-sxhash '(1 2)) t)
(deftest cl-18-02-14-009 () (cl-18-02-14-sxhash #\a) t)
(deftest cl-18-02-14-010 () (cl-18-02-14-sxhash #()) t)
(deftest cl-18-02-14-011 () (cl-18-02-14-sxhash #(1)) t)
(deftest cl-18-02-14-012 () (cl-18-02-14-sxhash "a") t)
(deftest cl-18-02-14-013 () (cl-18-02-14-sxhash #*) t)
(deftest cl-18-02-14-014 () (cl-18-02-14-sxhash #*101011) t)
(deftest cl-18-02-14-015 () (cl-18-02-14-sxhash 'foo) t)
(deftest cl-18-02-14-016 () (cl-18-02-14-sxhash #p"foo.lisp") t)
(deftest cl-18-02-14-017 () (cl-18-02-14-sxhash #'car) t)

(deftest cl-18-02-14-018 ()
  (bool (eql (sxhash (list 'a 'b)) (sxhash (list 'a 'b))))
  t )

(deftest cl-18-02-14-019 ()
  (bool (eql (sxhash (make-string 2 :initial-element #\a))
             (sxhash (make-string 2 :initial-element #\a)) ))
  t )
