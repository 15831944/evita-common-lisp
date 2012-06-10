;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-03-cl-eval.lisp#1 $
(in-package :tc-user)

;;;; 3.3.4  Declaration Scope
(deftest cl-03-02-001 ()
    (let ((x 0))                    ; special x = 0
         (declare (special x))
       (let ((x 1))                 ; lexical x = 1
         (let ((y x))               ; lexical x = 1
             (declare (special x))
           (values x y) ) ) )       ; special x = 0
  (values 0 1) )

(deftest cl-03-02-002 ()
    (let ((x 0))                    ; special x = 0
        (declare (special x))
      (let ((x 1))                  ; lexical x = 1
        (let ((y x)                 ; lexical x = 1
              (x 5) )               ; specail x = 5
          (declare (special x))
          (values x y) )))
  (values 5 1) )

(deftest cl-03-02-003 ()
    (let ((x 0))                    ; special x = 0
      (declare (special x))
      (let ((x 1))                  ; lexical x = 1
        (let* ((y x)                ; lexical x = 1
               (x 5))               ; special x = 5
            (declare (special x))
          (values x y) ) ) )
  (values 5 1) )

(deftest cl-03-02-003 ()
    (let ((x2q 1))
      (declare (special x2q))
      (let ((x2q 2))
        (+ x2q  ; lexical x2q = 2
           (locally (declare (special x2q)) x2q)) ) )   ; special x2q = 1
  3 )

(deftest cl-03-02-004 ()
    (let ((x4q 1))
        (declare (special x4q))
     (let ((x4q 2))
       (dotimes (i x4q x4q) (declare (special x4q)))) )
  1 )


;; non-local r/o
(deftest cl-03-03-001 ()
    (let ((x (cons 5 6))) (flet ((foo () x)) (foo))) (5 . 6) )

;; non-local r/w: BIND_IND
(deftest cl-03-03-002 ()
    (let ((x 0)) (flet ((foo () (setq x 1) nil)) (foo) x)) 1 )

;; non-local r/w: BIND_IND+STORE_IND
(deftest cl-03-03-003 ()
    (let ((x 0))
      (flet ((foo () (setq x (1+ x)) nil)) (foo) (setq x (1+ x)) (foo))
      x )
  3 )


;; non-local r/w and r/o
(deftest cl-03-03-004 ()
    (let ((x 0))
      (flet ((foo () (setq x 1) nil) (bar () x)) (foo) (bar)) )
  1 )

;; Closure has a nested function "bar".
(deftest cl-03-03-005 ()
    (flet (
      (foo (x)
        (setq x (1+ x))
        (lambda ()
          (flet ((bar () x))
              (declare (notinline bar))
            (bar) )) )
      )
      (declare (notinline foo))
      (funcall (foo 0)) )
  1 )



;; "g09-runtime": make-array:
;; ERROR: size and fill-pointer are coalesced.
;; OBSERVATION: There is no reference to arg[0](= size).
(deftest cl-03-03-006 ()
    (flet ((foo (x &key y) (values (if (eq t y) x y))))
      (list (foo 1) (foo 2 :y t) (foo 3 :y 100)) )
  (nil 2 100) )

;; "d24-fasl": push-command: non-local + optional w/ initform
(deftest cl-03-03-007 ()
    (let ((a (list 'a)))
      (flet ((foo (x &optional (y 2)) (append a (list x y))))
        (list (foo 1) (foo 1 3)) ) )
  ((a 1 2) (a 1 3)) )

;;; 3.4.1.6 Examples of Ordinary Lambda Lists

;; inline + decompose mv (MOVE v53 <- v12)
(defmacro test-lambda (lambda-expr &rest args)
  `(flet (
     (test ,(second lambda-expr)
       ,@(cddr lambda-expr) )
    )
    (let ((expr-result (,lambda-expr ,@args))
          (form-result (test ,@args)) )
      (if (equal expr-result form-result)
          expr-result
        (values expr-result form-result) ) ) ) )


;; Here are some examples involving optional parameters and rest parameters
(deftest cl-03-04-001 ()
    (test-lambda (lambda (a b) (+ a (* b 3))) 4 5)
  19 )

(deftest cl-03-04-002 ()
    (test-lambda (lambda (a &optional (b 2)) (+ a (* b 3))) 4 5)
  19 )

(deftest cl-03-04-003 ()
    (test-lambda (lambda (a &optional (b 2)) (+ a (* b 3))) 4)
  10 )

(deftest cl-03-04-004 ()
    (test-lambda (lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)))
  (2 nil 3 nil nil) )

(deftest cl-03-04-005 ()
    (test-lambda (lambda (&optional (a 2 b) (c 3 d) &rest x)
        (list a (bool b) c (bool d) x)) 6 )
  (6 t 3 nil nil) )

(deftest cl-03-04-006 ()
    (test-lambda (lambda (&optional (a 2 b) (c 3 d) &rest x)
        (list a (bool b) c (bool d) x)) 6 3 )
  (6 t 3 t nil) )

(deftest cl-03-04-007 ()
    (test-lambda (lambda (&optional (a 2 b) (c 3 d) &rest x)
        (list a (bool b) c (bool d) x)) 6 3 8 )
  (6 t 3 t (8)) )

(deftest cl-03-04-008 ()
    (test-lambda (lambda (&optional (a 2 b) (c 3 d) &rest x)
            (list a (bool b )c (bool d) x))
        6 3 8 9 10 11 )
  (6 t 3 t (8 9 10 11)) )

;; Here are some examples involving keyword parameters
(deftest cl-03-04-009 ()
    (test-lambda (lambda (a b &key c d) (list a b c d)) 1 2)
  (1 2 nil nil) )

(deftest cl-03-04-009 ()
    (test-lambda (lambda (a b &key c d) (list a b c d)) 1 2 :c 6)
  (1 2 6 nil) )

(deftest cl-03-04-010 ()
    (test-lambda (lambda (a b &key c d) (list a b c d)) 1 2 :d 8)
  (1 2 nil 8) )

(deftest cl-03-04-011 ()
    (test-lambda (lambda (a b &key c d) (list a b c d)) 1 2 :c 6 :d 8)
  (1 2 6 8) )

(deftest cl-03-04-012 ()
    (test-lambda (lambda (a b &key c d) (list a b c d)) 1 2 :d 8 :c 6)
  (1 2 6 8) )

(deftest cl-03-04-013 ()
    (test-lambda (lambda (a b &key c d) (list a b c d)) :a 1 :d 8 :c 6)
  (:a 1 6 8) )

(deftest cl-03-04-014 ()
    (test-lambda (lambda (a b &key c d) (list a b c d)) :a :b :c :d)
  (:a :b :d nil) )

(deftest cl-03-04-015 ()
    (test-lambda (lambda (a b &key ((:sea c)) d) (list a b c d)) 1 2 :sea 6)
  (1 2 6 nil) )

;; BLD#1818: Due to bug of compiler-macro of funcall, we fail to compile
;; following test case.
;((lambda (a b &key ((c c)) d) (list a b c d)) 1 2 'c 6)  => (1 2 6 nil)

;; Here are some examples involving optional parameters, rest parameters, and
;; keyword parameters together:
(deftest cl-03-04-016 ()
    (test-lambda (lambda (a &optional (b 3) &rest x &key c (d a))
      (list a b c d x)) 1)
  (1 3 nil 1 ()) )

(deftest cl-03-04-017 ()
    (test-lambda (lambda (a &optional (b 3) &rest x &key c (d a))
      (list a b c d x)) 1 2)
  (1 2 nil 1 ()) )

(deftest cl-03-04-018 ()
    (test-lambda (lambda (a &optional (b 3) &rest x &key c (d a))
      (list a b c d x)) :c 7)
  (:c 7 nil :c ()) )

(deftest cl-03-04-019 ()
    (test-lambda (lambda (a &optional (b 3) &rest x &key c (d a))
      (list a b c d x)) 1 6 :c 7)
  (1 6 7 1 (:c 7)) )

(deftest cl-03-04-020 ()
    (test-lambda (lambda (a &optional (b 3) &rest x &key c (d a))
      (list a b c d x)) 1 6 :d 8)
  (1 6 nil 8 (:d 8)) )

(deftest cl-03-04-021 ()
    (test-lambda (lambda (a &optional (b 3) &rest x &key c (d a))
      (list a b c d x) ) 1 6 :d 8 :c 9 :d 10)
  (1 6 9 8 (:d 8 :c 9 :d 10)) )

;; Simplify-CFG: BRANCH-diamond-MERGE
(deftest cl-03-04-022 ()
    (test-lambda (lambda (&optional (x 1234 s)) (unless s (setq x 5678)) x))
  5678 )

(deftest cl-03-04-023 ()
    (test-lambda (lambda (&optional (x 1234 s)) (unless s (setq x 5678)) x)
        7890 )
  7890 )

;;; call closure
(deftest cl-03-04-024 ()
    "UpVarPass::insert_cvector"
    (let ((x 1))
      (flet ((bar () (incf x)))
        (ignore-arguments #'bar)
        (bar) ) )
  2 )


;;; doc-string

(deftest cl-03-05-001 ()
    (progn (defun lambda.1 () "foo") (lambda.1))
  "foo" )

(deftest cl-03-05-002 ()
    (let ((fn (lambda () "foo"))) (funcall fn))
  "foo" )

(deftest cl-03-05-003 () (flet ((fn () "foo")) (fn)) "foo")

(deftest cl-03-05-004 ()
    (flet ((fn () "foo")) (declare (notinline fn)) (fn))
  "foo" )

(deftest cl-03-05-005 () (funcall (lambda () "foo")) "foo")

(deftest cl-03-05-006 ()
    (progn (defun lambda.1 () "foo" "bar") (lambda.1))
  "bar" )

(deftest cl-03-05-007 ()
    (let ((fn (lambda () "foo" "bar"))) (funcall fn)) "bar" )

(deftest cl-03-05-008 () (flet ((fn () "foo" "bar")) (fn)) "bar")

(deftest cl-03-05-009 () 
    (flet ((fn () "foo" "bar")) (declare (notinline fn)) (fn)) "bar" )

(deftest cl-03-05-010 () (funcall (lambda () "foo" "bar")) "bar")
