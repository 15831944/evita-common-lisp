;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-03-cl-decl.lisp#1 $
(in-package :tc-user)


;;; 3.8.22 inline/notinline

;; inline + decompse mv CALL
(deftest cl-03-01-001 ()
    (flet ((foo (x) (single-value x)))
      (if (foo nil) 1 2) )
  2 )

;; inline + decompse mv CALL
(deftest cl-03-01-002 ()
    (flet ((foo (x) (single-value x)))
      (multiple-value-bind (x y z) (foo 1) (list x y z)) )
  (1 nil nil) )

;; inline + decompose mv MERGE
(deftest cl-03-01-003 ()
    (flet (
      (foo (x)
        (catch nil
          (when x (return-from foo (multiple-values 4 5 6)))
        (multiple-values 7 8 9) ) )
      )
     (multiple-value-bind (x y z) (foo nil) (list x y z)) )
  (values 7 8 9) )


;; "d24-load": load -- owner of frames must be updated after inline expansion.
(deftest cl-03-01-003 ()
    (let ((*some-variable* 1234))
        (declare (special *some-variable*))
      (flet (
        (foo ()
          (let ((*some-variable* (1+ *some-variable*)))
            (single-value *some-variable*) ) )
        )
        (setq *some-variable* 0)
        (list (foo) *some-variable*) ) )
  (1 0) )


;; inline + non-local return-from (1:1)
(deftest cl-03-01-004 ()
    (flet (
      (foo (x)
        (flet ((bar (y) (incf x y) (return-from foo x)))
          (when x (bar 3))
        1234 ) )
      )
     (list (foo nil) (foo 5)) )
  (1234 8) )

;; inline + non-local return-from (n:1)
(deftest cl-03-01-005 ()
    "See oftc-03-01-006 for notinline version." 
    (flet (
      (foo (x)
        (flet (
          (bar () (return-from foo 1234))
          (baz () (return-from foo 5678))
          )
          (if x (bar) (baz)) ) )
      )
     (list (foo t) (foo nil)) )
  (1234 5678) )

(deftest cl-03-01-006 ()
    "See oftc-03-01-005 for inline version." 
    (flet (
      (foo (x)
        (flet (
          (bar () (return-from foo 1234))
          (baz () (return-from foo 5678))
          )
           (declare (notinline bar baz))
         (if x (bar) (baz)) ) )
      )
      (list (foo t) (foo nil)) )
  (1234 5678) )


;; inline + non-local return-from (block owner isn't caller)
(deftest cl-03-01-007 ()
    (block outer
      (labels (
        (foo () (return-from outer 7))
        (bar () (return-from outer 8))
        (baz (x) (if x (foo) (bar)))
        )
        (list (baz nil) (baz t)) ))
  8 )

;; inline + non-local return-from (block owner isn't caller)
(deftest cl-03-01-008 ()
    (block outer
      (flet (
        (foo ()
          (flet ((bar () (return-from outer 7))) (bar)) )
        )
        (foo) ) )
  7 )


;; inline + non-local go
(deftest cl-03-01-009 ()
    (let ((x 1234))
      (tagbody
        (flet ((foo () (setq x 7890) (go exit))) (foo))
        (setq x 4567)
        exit )
      x )
  7890 )

(deftest cl-03-01-010 ()
    (let ((x 1234))
      (tagbody
        (flet ((foo () (setq x 7890) (go exit))) (declare (notinline foo))
          (foo) )
        (setq x 4567)
       exit )
      x )
   7890 )
