;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-05-cl-control.lisp#1 $
(in-package :tc-user)

;;; 5.3.22 block
(deftest cl-05-22-001 () (block empty) nil)

(deftest cl-05-22-002 ()
    (block whocares (values 1 2) (values 3 4))
  :values (3  4) )

(deftest cl-05-22-003 ()
    (let ((x 1)) 
      (block stop (setq x 2) (return-from stop) (setq x 3))
      x )
  2 )

(deftest cl-05-22-004 ()
    (block early (return-from early (values 1 2)) (values 3 4))
 (values 1 2) )

(deftest cl-05-22-005 ()
    (block outer (block inner (return-from outer 1)) 2)
  1 )

(deftest cl-05-22-006 ()
    (block twin (block twin (return-from twin 1)) 2)
  2 )

;; Contrast behavior of this example with corresponding example of CATCH.
(deftest cl-05-22-007 ()
    (block b
      (flet ((b1 () (return-from b 1)))
        (block b (b1) (print 'unreachable))
       2 ))
  1 )

;;; nonlocal-exit but nerver invoked
(deftest cl-05-22-008 ()
    (block a (flet ((bar () (return-from a 1234))) 5678))
  5678 )

;; LOAD_NLX+MERGE of mvtag (windows parse-namestring-using-host)
(deftest cl-05-22-009 ()
    (flet (
      (foo (x)
        (flet ((exit () (return-from foo (values 'exit 7))))
          (when x (exit))
          (when (eq x 1) (return-from foo (values 'one 5)))
          (values 'normal 8) ) )
      )
      (list
        (multiple-value-list (foo nil))
        (multiple-value-list (foo t)) ) )
  ((normal 8) (exit 7)) )

;; pprint-array: BIND_VCL+LOAD_VCL
(deftest cl-05-22-010 ()
    "UpVarPass::insert_cvector"
    (let ((x (ignore-arguments 'a)))
      (flet ((foo () (setq x 'b) nil))
        (ignore-arguments 1 x)
        (ignore-arguments #'foo)
        (foo)
        x ) )
  b )

(deftest cl-05-22-011 ()
    (flet (
      (test-it (a)
        (let ((x 1))
          (block d
            (labels (
              (exit () (setq x 'exit) (return-from d))
              (process (y) (if y 'normal (exit)))
              )
              (setq x (process a))
              (when a (ignore-arguments)) ))
          x ) )
      )
      (list (test-it nil) (test-it t)) )
  (exit normal) )



;;; 5.3.23 catch
(deftest cl-05-23-001 ()
    (catch '-tag 1 2 (throw '-tag 3) 4)  3 )

(deftest cl-05-23-001 ()
    (catch '-tag 1 2 3 4)  4 )

(defun throw-back (tag) (throw tag t))

(deftest cl-05-23-002 ()
    (catch '-tag (throw-back '-tag) 2) t )

;; Contrast behavior of this example with corresponding example of BLOCK.
(deftest cl-05-23-003 ()
    (catch 'c
      (flet ((c1 () (throw 'c 1)))
        (catch 'c (c1) (print 'reachable))
         2 ))
  2 )

(deftest cl-05-23-004 ()
   (catch 'foo
     (flet ((foo () (throw 'foo (values 'a 'b 'c 'd 'e))))
        (declare (notinline foo))
      (foo) ) )
  (values a b c d e) )


;;; 5.3.17 destructuring-bind
(deftest cl-05-03-001 ()
    (labels ((iota (n) (loop for i from 1 to n collect i)))       ;helper
       (destructuring-bind ((a &optional (b 'bee)) one two three)
           `((alpha) ,@(iota 3))
         (list a b three two one) ) )
  (alpha bee 3 2 1) )


;;; 5.3.6 flet
;;; 5.3.6 labels
#|
;; We drop function ext:function-name.
(labels (
  (foo ()
    (flet ((foo () 'foo-1))
      #'foo ) )
  )
  (ext:function-name #'foo) )
 foo
|#

#|
;;; x86::assemble-aux: We must preserve order of edge after coalescing bblocks.
(defun foo (form*)
   (when (null *code-buffer*) (setq *code-buffer* (make-list 3)))
   (dolist (x form*) (parse-form x)) )
 123
|#

;;; 5.3.27 tagbody
(deftest cl-05-27-001 ()
     (let (val)
        (tagbody
          (setq val 1)
          (go point-a)
          (incf val 16)
         point-c
          (incf val 04)
          (go point-b)
          (incf val 32)
         point-a
          (incf val 02)
          (go point-c)
          (incf val 64)
         point-b
          (incf val 08))
        val)
  15 )

;; f2 and lambda can be inlined.
(deftest cl-05-27-002 ()
    (labels (
     (f1 (flag)
       (let ((n 1))
         (tagbody 
           (setq n (f2 flag #'(lambda () (go out))))
          out
           (return-from f1 n))) )
     (f2 (flag escape)
       (if flag (funcall escape) 2) )
      )
      (list (f1 nil) (f1 t)) )
  (2 1) )

; framework of pprint-logical-block
#+nil
(defun foo ()
  (block nil
    (flet ((exit () (return)))
      (tagbody 1
        (when (bar) (return))
        (when (bar) (exit))
        (go 1) ) ))
    nil )


;;; 5.3.48 multiple-value-bind

;;;; mv-bind
;;; Force compiler see multiple-value-bind, since multiple-value-bind is
;;; macro.
(defmacro mv-bind (vars values-form &body decl*-form*)
  `(funcall
      (lambda ()
        (multiple-value-bind ,vars ,values-form
           (declare (ignorable ,@vars))
           ,@decl*-form* ))) )

(deftest cl-05-48-001 ()
    (multiple-value-bind (x y) (values 7 5) (list x y)) (7 5) )

(deftest cl-05-48-002 ()
    (multiple-value-bind (x y) (values 7 5) x)  7 )

(deftest cl-05-48-003 ()
    (multiple-value-bind (x y) (values 7 5) y)  5 )

(deftest cl-05-48-004 ()
    (multiple-value-bind (x y z) (values 7 5) (list x y z))  (7 5 nil) )

(deftest cl-05-48-005 ()
    (multiple-value-bind (x y z) (values 7 5) z)  nil )

(deftest cl-05-48-006 ()
    (multiple-value-bind (x y) (multiple-values 7 5) (list x y))  (7 5) )

(deftest cl-05-48-007 ()
    (multiple-value-bind (x y) (multiple-values 7 5) x)  7 )

(deftest cl-05-48-008 ()
    (multiple-value-bind (x y) (multiple-values 7 5) y)  5 )

(deftest cl-05-48-009 ()
    (multiple-value-bind (x y z) (multiple-values 7 5) (list x y z))  (7 5 nil) )

(deftest cl-05-48-010 ()
    (multiple-value-bind (x y z) (multiple-values 7 5) z)  nil )

(deftest cl-05-48-101 ()
    (mv-bind (x y) (values 7 5) (list x y)) (7 5) )

(deftest cl-05-48-102 ()
    (mv-bind (x y) (values 7 5) x)  7 )

(deftest cl-05-48-103 ()
    (mv-bind (x y) (values 7 5) y)  5 )

(deftest cl-05-48-104 ()
    (mv-bind (x y z) (values 7 5) (list x y z))  (7 5 nil) )

(deftest cl-05-48-105 ()
    (mv-bind (x y z) (values 7 5) z)  nil )

(deftest cl-05-48-106 ()
    (mv-bind (x y) (multiple-values 7 5) (list x y))  (7 5) )

(deftest cl-05-48-107 ()
    (mv-bind (x y) (multiple-values 7 5) x)  7 )

(deftest cl-05-48-108 ()
    (mv-bind (x y) (multiple-values 7 5) y)  5 )

(deftest cl-05-48-109 ()
    (mv-bind (x y z) (multiple-values 7 5) (list x y z))  (7 5 nil) )

(deftest cl-05-48-110 ()
    (mv-bind (x y z) (multiple-values 7 5) z)  nil )

;; OPT-FUNCALL
(deftest cl-05-48-111 ()
    (labels ((bar () 1))
        (declare (notinline bar))
      (multiple-value-bind (x y) (bar)
        (list x y) ) )
  (1 nil) )


;;; 5.3.49 multiple-value-call

(deftest cl-05-49-001 ()
    (multiple-value-call #'list)  nil )

(deftest cl-05-49-002 ()
    (let ((x 1))
      (flet ((bar (&rest y) (cons x y)) (baz () (values 2 3 4 5)))
        (multiple-value-call #'bar (baz)) ) )
 (1 2 3 4 5) )

(deftest cl-05-49-003 ()
    (let ((x 1))
      (flet ((bar (&rest y) (cons x y)))
        (multiple-value-call #'bar (values 2 3 4 5)) ) )
     (1 2 3 4 5) )


;;;; 5.3.50 multiple-value-list

;; inline expansion and mv-call
(deftest cl-05-50-001 ()
    (labels ((foo () (multiple-values 4 5 6)))
      (multiple-value-list (foo)) )
  (4 5 6) )

;;; 5.3.51 multiple-value-prog1
(deftest cl-05-09-001 ()
    (multiple-value-prog1 (values 7 8 9) (ignore-arguments))
  (values 7 8 9) )

(deftest cl-05-09-002 ()
    (multiple-value-prog1 (multiple-values 7 8 9) (ignore-arguments))
  (values 7 8 9) )


;;;; 5.3.56 nth-value
(deftest cl-05-56-001 ()
    (labels ((foo (x) (nth-value x (multiple-values 'a 'b 'c 'd))))
      (declare (notinline foo))
      (list (foo 0) (foo 1) (foo 2) (foo 3) (foo 4)) )
  (a b c d nil) )

(deftest cl-05-56-002 ()
    (nth-value (single-value 0) (multiple-values 1 2 3)) 1 )

(deftest cl-05-56-003 ()
    (nth-value (single-value 1) (multiple-values 1 2 3)) 2 )

(deftest cl-05-56-004 ()
    (nth-value (single-value 2) (multiple-values 1 2 3)) 3 )

(deftest cl-05-56-005 ()
    (nth-value (single-value 3) (multiple-values 1 2 3)) nil )

(deftest cl-05-56-006 ()
    (nth-value (single-value 4) (multiple-values 1 2 3)) nil )

(deftest cl-05-56-007 ()
    (nth-value (single-value (1- multiple-values-limit))
               (multiple-values 1 2 3))
    nil )

(deftest cl-05-56-008 ()
    (nth-value (single-value 0) (multiple-values)) nil )

(deftest cl-05-56-009 ()
    (nth-value (single-value 1) (multiple-values)) nil )


(deftest cl-05-56-101 ()
    (nth-value (single-value -1) (multiple-values 1 2 3))
  :error type-error )

(deftest cl-05-56-102 ()
    (funcall (lambda () (nth-value (single-value -1) (multiple-values 1 2 3))))
  :error type-error )


(deftest cl-05-56-103 ()
    (nth-value (single-value multiple-values-limit) (multiple-values 1 2 3))
  :error type-error )


;;; 5.3.66 rotatef

;; SSA-to-CFG: swap problem
(deftest cl-05-66-001 ()
    (flet ((foo (start end p) (when p (rotatef start end)) (cons start end)))
      (list (foo 1 2 nil) (foo 1 2 t)) )
 ((1 . 2) (2 . 1)) )

;;;; 5.3.29 unwind-protect

;;; mv-ret  void-ret
(deftest cl-05-29-001 ()
    (let ((x 1))
      (labels ((bar () (unwind-protect (setq x 2) (setq x 3))))
          (declare (notinline bar))
        (bar)
        x ) )
  3 )
