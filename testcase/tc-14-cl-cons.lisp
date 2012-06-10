;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-14-cl-cons.lisp#1 $
(in-package :tc-user)

;;;; 14.2.44 adjoin
(deftest cl-14-44-001 ()
    (let ((slist '()))  ;   NIL 
        (multiple-value-call #'values
            (adjoin 'a slist)                            ;    (A) 
            slist                                        ;    NIL 
           (setq slist (adjoin '(test-item 1) slist))    ; ((TEST-ITEM 1)) 
           (adjoin '(test-item 1) slist)                 ; ((TEST-ITEM 1) (TEST-ITEM 1)) 
           (adjoin '(test-item 1) slist :test 'equal)    ; ((TEST-ITEM 1)) 
           (adjoin '(new-test-item 1) slist :key #'cadr) ; ((TEST-ITEM 1)) 
           (adjoin '(new-test-item 1) slist)             ; ((NEW-TEST-ITEM 1) (TEST-ITEM 1))
           ) )
  (values
      (a)
      nil
      ((test-item 1))
      ((test-item 1) (test-item 1))
      ((test-item 1))
      ((test-item 1))
      ((new-test-item 1) (test-item 1)) ) )


;;; 14.2.36 assoc
(deftest cl-14-36-001 ()
    (let ((values `((x . 100) (y . 200) (z . 50))))
      (values (copy-list (assoc 'y values))
              (rplacd (assoc 'y values) 201)
              (assoc 'y values) ) )
 (values (Y . 200) (Y . 201) (Y . 201)) )


(deftest cl-14-36-002 ()
    (let ((alist `((1 . "one")(2 . "two")(3 . "three"))))
      (values (assoc 2 alist)
              (assoc-if #'evenp alist)
              (assoc-if (complement #'(lambda(x) (< x 3))) alist) ) )
  (values (2 . "two")   (2 . "two")   (3 . "three")) )


(deftest cl-14-36-003 ()
    (let ((alist '(("one" . 1) ("two" . 2))))
      (values (assoc "one" alist)
              (assoc "one" alist :test #'equalp)  
              (assoc "two" alist :key #'(lambda(x) (char x 2))) 
              (assoc #\o alist :key #'(lambda(x) (char x 2))) ) )
  (values NIL ("one" . 1) NIL ("two" . 2)) )

(deftest cl-14-36-004 ()
    (assoc 'r '((a . b) (c . d) (r . x) (s . y) (r . z)))
  (R . X) )

(deftest cl-14-36-005 ()
    (assoc 'goo '((foo . bar) (zoo . goo)))
  nil )

(deftest cl-14-36-006 ()
    (assoc '2 '((1 a b c) (2 b c d) (-7 x y z)))
  (2 B C D) )

(deftest cl-14-36-007 ()
    (let ((alist '(("one" . 1) ("2" . 2) ("three" . 3))))
      (assoc-if (complement #'alpha-char-p) alist
               :key #'(lambda (x) (char x 0)))  )
  ("2" . 2) )

;;; 14.2.28 butlast
(deftest cl-14-28-001 () (butlast (list 'a 'b 'c 'd) 0) (a b c d))
(deftest cl-14-28-002 () (butlast (list 'a 'b 'c 'd) 1) (a b c))
(deftest cl-14-28-003 () (butlast (list 'a 'b 'c 'd) 2) (a b))
(deftest cl-14-28-004 () (butlast (list 'a 'b 'c 'd) 3) (a))
(deftest cl-14-28-005 () (butlast (list 'a 'b 'c 'd) 4) nil)
(deftest cl-14-28-006 () (butlast (list 'a 'b 'c 'd) 5) nil)
(deftest cl-14-28-007 () (butlast (list 'a))            nil)
(deftest cl-14-28-008 () (butlast '())                  nil)

(deftest cl-14-28-009 () (butlast (list* 'a 'b 'c) 0)   (a b))
(deftest cl-14-28-010 () (butlast (list* 'a 'b 'c) 1)   (a))
(deftest cl-14-28-011 () (butlast (list* 'a 'b 'c) 2)   nil)


;;; 14.2.37 copy-alist
(deftest cl-14-37-001 ()
    (copy-alist '((1 . one) (2 . two) (3. three)))
  ((1 . one) (2 . two) (3. three)) )

;;; 14.2.14 copy-list
(deftest cl-14-14-001 () (copy-list '(1 2 3 4))   (1 2 3 4))
(deftest cl-14-14-002 () (copy-list '(1 2 3 . 4)) (1 2 3 . 4))


;;; 14.2.10 copy-tree
(deftest cl-14-10-001 ()
    (let* ((object (list (cons 1 "one") (cons 2 (list 'a 'b 'c))))
           (object-too object)
           (copy-as-tree  (copy-tree  object)) )
      (values (bool (eq object object-too))
              (bool (eq copy-as-tree object))
              (bool (eql copy-as-tree object))
              (bool (equal copy-as-tree object)) ) )
  (values t nil nil t) )


;;; 14.2.41 getf
(deftest cl-14-41-001 () (getf '() 'prop1)               nil)
(deftest cl-14-41-002 () (getf '() 'prop1 7)             7)
(deftest cl-14-41-003 () (getf '(prop1 val1) 'prop1)     val1)
(deftest cl-14-41-004 () (getf '(prop1 val1) 'prop1 7)   val1)


;;; 14.2.40 get-properties
(deftest cl-14-40-001 ()
    (get-properties '(prop1 val1) '(prop1 prop2))
  (values prop1 val1 (prop1 val1)) )

(deftest cl-14-40-002 ()
    (get-properties '(prop1 val1 prop2 val2) '(prop1 prop2))
  (values prop1 val1 (prop1 val1 prop2 val2)) )


;;; 14.2.16 list-length
(deftest cl-14-16-001 () (list-length nil)       0)
(deftest cl-14-16-002 () (list-length '(1))      1)
(deftest cl-14-16-003 () (list-length '(1 2 3))  3)
(deftest cl-14-16-004 () (list-length '#1=(1 2 3 . #1#)) nil)


;;; 14.2.29 last
(deftest cl-14-29-001 () (last nil)                NIL)
(deftest cl-14-29-002 () (last '(1 2 3))           (3))
(deftest cl-14-29-003 () (last '(1 2 . 3))         (2 . 3))
(deftest cl-14-29-004 () (last (list 'a 'b 'c 'd))  (D))

(deftest cl-14-29-005 () (last '(a b c))            (C))

(deftest cl-14-29-005 () (last '(a b c) 0) nil)
(deftest cl-14-29-006 () (last '(a b c) 1) (C))
(deftest cl-14-29-007 () (last '(a b c) 2) (B C))
(deftest cl-14-29-008 () (last '(a b c) 3) (A B C))
(deftest cl-14-29-009 () (last '(a b c) 4) (A B C))

(deftest cl-14-29-010 () (last '(a . b) 0) B)
(deftest cl-14-29-011 () (last '(a . b) 1) (A . B))
(deftest cl-14-29-012 () (last '(a . b) 2) (A . B))


;;; 14.2.21 first, second, ..., ninth, tenth
(deftest cl-14-21-001 () (first   '(1 2 3 4 5 6 7 8 9 0))  1)
(deftest cl-14-21-002 () (second  '(1 2 3 4 5 6 7 8 9 0))  2)
(deftest cl-14-21-003 () (third   '(1 2 3 4 5 6 7 8 9 0))  3)
(deftest cl-14-21-004 () (fourth  '(1 2 3 4 5 6 7 8 9 0))  4)
(deftest cl-14-21-005 () (fifth   '(1 2 3 4 5 6 7 8 9 0))  5)
(deftest cl-14-21-006 () (sixth   '(1 2 3 4 5 6 7 8 9 0))  6)
(deftest cl-14-21-007 () (seventh '(1 2 3 4 5 6 7 8 9 0))  7)
(deftest cl-14-21-008 () (eighth  '(1 2 3 4 5 6 7 8 9 0))  8)
(deftest cl-14-21-009 () (ninth   '(1 2 3 4 5 6 7 8 9 0))  9)
(deftest cl-14-21-010 () (tenth   '(1 2 3 4 5 6 7 8 9 0))  0)


;;; 14.2.30 ldiff
(deftest cl-14-30-001 ()
    (let* ((list '(a b c))
           (vector (vector list (cddr list)
                           (copy-list list) '(f g h) '() 'd 'x))
           (results '()) )
      (dotimes (i (length vector))
        (push (ldiff list (svref vector i)) results) )
      (values-list (nreverse results)) )
  (values nil (a b) (a b c) (a b c) (a b c) (a b c) (a b c)) )


(deftest cl-14-30-002 ()
    (let* ((list '(a b c . d))
           (vector (vector list (cddr list) (copy-list list)
                            '(f g h) '() 'd 'x))
           (results '()) )
      (dotimes (i (length vector))
        (push (ldiff list (svref vector i)) results) )
      (values-list (nreverse results)) )
  (values nil (a b) (a b c . d) (a b c . d) (a b  c . d) (a b c) (a b c . d)) )


;;; 14.2.34 mapcan
(deftest cl-14-34-001 ()
    (mapcan #'(lambda (x y) (if (null x) nil (list x y)))
              '(nil nil nil d e)
              '(1 2 3 4 5 6))
  (D 4 E 5) )

(deftest cl-14-34-002 ()
    (mapcan #'(lambda (x) (and (numberp x) (list x)))
            '(a 1 b c 3 4 d 5))
  (1 3 4 5) )


;;; 14.2.34 mapcar
(deftest cl-14-34-101 () (mapcar #'car '((1 a) (2 b) (3 c))) (1 2 3))
(deftest cl-14-34-102 () (mapcar #'abs '(3 -4 2 -5 -6))      (3 4 2 5 6))

(deftest cl-14-34-103 () (mapcar #'cons '(a b c) '(1 2 3))
    ((A . 1) (B . 2) (C . 3)) )


;;; 14.2.34 mapcon
(deftest cl-14-34-204 ()
    (mapcon #'list '(1 2 3 4))  ((1 2 3 4) (2 3 4) (3 4) (4)) )


;;; 14.2.34 maplist
(deftest cl-14-34-305 ()
    (maplist #'append '(1 2 3 4) '(1 2) '(1 2 3)) 
  ((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)) )

(deftest cl-14-34-306 ()
    (maplist #'(lambda (x) (cons 'foo x)) '(a b c d))
  ((FOO A B C D) (FOO B C D) (FOO C D) (FOO D)) )

(deftest cl-14-34-307 ()
    (maplist #'(lambda (x) (if (member (car x) (cdr x)) 0 1)) '(a b a c d b c))
  (0 0 1 0 1 1 1) )

;;; 14.2.33 member
(deftest cl-14-33-001 () (member 2 '(1 2 3)) (2 3))

(deftest cl-14-33-002 ()
    (member 2 '((1 . 2) (3 . 4)) :test (complement #'=) :key #'cdr)
  ((3 . 4)) )

(deftest cl-14-33-003 () (member 'e '(a b c d)) nil)

(deftest cl-14-33-004 ()
  (member-if #'listp '(a b nil c d))  (NIL C D) )

(deftest cl-14-33-005 ()
    (member-if #'numberp '(a #\Space 5/3 foo))  (5/3 FOO) )

(deftest cl-14-33-006 ()
    (member-if (complement #'zerop) 
                 '(3 6 9 11 . 12)
                 :key #'(lambda (x) (mod x 3)))
  (11 . 12) )


;;; 14.2.28 nbutlast
(deftest cl-14-28-001 () (nbutlast (list 'a 'b 'c 'd) 0)        (a b c d))
(deftest cl-14-28-002 () (nbutlast (list 'a 'b 'c 'd) 1)        (a b c))
(deftest cl-14-28-003 () (nbutlast (list 'a 'b 'c 'd) 2)        (a b))
(deftest cl-14-28-004 () (nbutlast (list 'a 'b 'c 'd) 3)        (a))
(deftest cl-14-28-005 () (nbutlast (list 'a 'b 'c 'd) 4)        nil)
(deftest cl-14-28-006 () (nbutlast (list 'a 'b 'c 'd) 5)        nil)
(deftest cl-14-28-007 () (nbutlast (list 'a))                   nil)
(deftest cl-14-28-008 () (nbutlast '())                         nil)

(deftest cl-14-28-009 () (nbutlast (list* 'a 'b 'c) 0)          (a b))
(deftest cl-14-28-010 () (nbutlast (list* 'a 'b 'c) 1)          (a))
(deftest cl-14-28-011 () (nbutlast (list* 'a 'b 'c) 2)          nil)


;;; 14.2.25 nconc
(deftest cl-14-25-001 () (nconc) nil)
(deftest cl-14-25-002 () (nconc `(a b c) `(d e f)) (a b c d e f))
(deftest cl-14-25-003 () (nconc `(a b c) nil `(d e f)) (a b c d e f))
(deftest cl-14-25-004 () (nconc `(a b c) `(d e f) nil) (a b c d e f))
(deftest cl-14-25-005 () (nconc nil `(a b c) nil `(d e f)) (a b c d e f))
(deftest cl-14-25-006 () (nconc nil `(a b c) nil `(d e f) nil) (a b c d e f))
(deftest cl-14-25-007 () (nconc nil `(1) `(2) `(3) `(4)) (1 2 3 4))


;;; 14.2.43 intersection
(deftest cl-14-43-001 ()
    (intersection (list 1 1 2 3 4 'a 'b 'c "A" "B" "C" "d")
                  (list 1 4 5 'b 'c 'd "a" "B" "c" "D") )
  (C B 4 1 1) )

(deftest cl-14-43-002 ()
    (intersection (list 1 1 2 3 4 'a 'b 'c "A" "B" "C" "d")
                  (list 1 4 5 'b 'c 'd "a" "B" "c" "D")
                  :test #'equal )
  ("B" C B 4 1 1) )

(deftest cl-14-43-003 ()
    (intersection (list 1 1 2 3 4 'a 'b 'c "A" "B" "C" "d")
                  (list 1 4 5 'b 'c 'd "a" "B" "c" "D")
                  :test #'equalp )
  ("d" "C" "B" "A" C B 4 1 1) )


(deftest cl-14-43-004 ()
    (nintersection (list 1 1 2 3 4 'a 'b 'c "A" "B" "C" "d")
                   (list 1 4 5 'b 'c 'd "a" "B" "c" "D") )
  (1 1 4 B C) )


(deftest cl-14-43-005 ()
    (nintersection (copy-list '((1 . 2) (2 . 3) (3 . 4) (4 . 5)))
                   (copy-list '((1 . 3) (2 . 4) (3 . 6) (4 . 8)))
                   :key #'cdr )
  ((2 . 3) (3 . 4)) )

;;; 14.2.47 nset-exclusive
(deftest cl-14-47-001 ()
    (nset-exclusive-or `(1 "a" "b") `(1 "A" "b"))
  ("a" "b" "A" "b") )

(deftest cl-14-47-002 ()
    (nset-exclusive-or `(1 "a" "b") `(1 "A" "b") :test #'equal)
  ("a" "A") )

(deftest cl-14-47-003 ()
    (nset-exclusive-or `(1 "a" "b") `(1 "A" "b") :test 'equalp)
  nil )


;;; 14.2.11 nsublis
(deftest cl-14-11-001 ()
    (let ((tree1 `(1 (1 2) ((1 2 3)) (((1 2 3 4))))))
      (nsublis '((t . 'temp))
               tree1
               :key #'(lambda (x) (or (atom x) (< (list-length x) 3)))) )
 ((quote temp) (quote temp) quote temp) )


;;; 14.2.22 nth
(deftest cl-14-22-001 () (nth 0 '(1 2 3 4 5 6 7 8 9 0))  1)
(deftest cl-14-22-002 () (nth 1 '(1 2 3 4 5 6 7 8 9 0))  2)
(deftest cl-14-22-003 () (nth 2 '(1 2 3 4 5 6 7 8 9 0))  3)
(deftest cl-14-22-004 () (nth 3 '(1 2 3 4 5 6 7 8 9 0))  4)
(deftest cl-14-22-005 () (nth 4 '(1 2 3 4 5 6 7 8 9 0))  5)
(deftest cl-14-22-006 () (nth 5 '(1 2 3 4 5 6 7 8 9 0))  6)
(deftest cl-14-22-007 () (nth 6 '(1 2 3 4 5 6 7 8 9 0))  7)
(deftest cl-14-22-008 () (nth 7 '(1 2 3 4 5 6 7 8 9 0))  8)
(deftest cl-14-22-009 () (nth 8 '(1 2 3 4 5 6 7 8 9 0))  9)
(deftest cl-14-22-010 () (nth 9 '(1 2 3 4 5 6 7 8 9 0))  0)
(deftest cl-14-22-011 () (nth 10 '(1 2 3 4 5 6 7 8 9 0)) nil)

(deftest cl-14-22-101 () (nth -1 '(1 2 3 4 5 6 7 8 9 0)) :error type-error)
(deftest cl-14-22-102 () (nth -1 nil) :error type-error)


;;; 14.2.31 nthcdr
(deftest cl-14-31-001 () (nthcdr 0 '(1 2 3 4 5 6 7 8 9 0))
  (1 2 3 4 5 6 7 8 9 0) )

(deftest cl-14-31-002 () (nthcdr 1 '(1 2 3 4 5 6 7 8 9 0))
  (2 3 4 5 6 7 8 9 0) )

(deftest cl-14-31-003 () (nthcdr 2 '(1 2 3 4 5 6 7 8 9 0))  (3 4 5 6 7 8 9 0))
(deftest cl-14-31-004 () (nthcdr 3 '(1 2 3 4 5 6 7 8 9 0))  (4 5 6 7 8 9 0))
(deftest cl-14-31-005 () (nthcdr 4 '(1 2 3 4 5 6 7 8 9 0))  (5 6 7 8 9 0))
(deftest cl-14-31-006 () (nthcdr 5 '(1 2 3 4 5 6 7 8 9 0))  (6 7 8 9 0))
(deftest cl-14-31-007 () (nthcdr 6 '(1 2 3 4 5 6 7 8 9 0))  (7 8 9 0))
(deftest cl-14-31-008 () (nthcdr 7 '(1 2 3 4 5 6 7 8 9 0))  (8 9 0))
(deftest cl-14-31-009 () (nthcdr 8 '(1 2 3 4 5 6 7 8 9 0))  (9 0))
(deftest cl-14-31-010 () (nthcdr 9 '(1 2 3 4 5 6 7 8 9 0))  (0))
(deftest cl-14-31-011 () (nthcdr 10 '(1 2 3 4 5 6 7 8 9 0)) nil)
(deftest cl-14-31-012 () (nthcdr 0 '())      NIL)
(deftest cl-14-31-013 () (nthcdr 3 '())      NIL)
(deftest cl-14-31-014 () (nthcdr 0 '(a b c)) (A B C))
(deftest cl-14-31-015 () (nthcdr 2 '(a b c)) (C))
(deftest cl-14-31-016 () (nthcdr 4 '(a b c)) ())
(deftest cl-14-31-017 () (nthcdr 1 '(0 . 1)) 1)


;;; 14.2.38 pairlis
(deftest cl-14-38-001 ()
    (let ((keys '(1 2 3))
          (data '("one" "two" "three"))
          (alist '((4 . "four"))) )
     (values (pairlis keys data)
             (pairlis keys data alist)
             alist ) )
 (values
    ((3 . "three") (2 . "two") (1 . "one"))
    ((3 . "three") (2 . "two") (1 . "one") (4 . "four"))
    ((4 . "four")) ) )

;;;; 14.2.39 rassoc
(deftest cl-14-39-001 ()
    (let ((alist '((1 . "one") (2 . "two") (3 . 3))))
      (values (rassoc 3 alist)
              (rassoc "two" alist)
              (rassoc "two" alist :test 'equal)
              (rassoc 1 alist :key #'(lambda (x) (if (numberp x) (/ x 3))))
              (rassoc 'a '((a . b) (b . c) (c . a) (z . a)))
              (rassoc-if #'stringp alist)
              (rassoc-if (complement #'vectorp) alist) ) )
 (values (3 . 3) NIL (2 . "two") (3 . 3) (C . A) (1 . "one") (3 . 3)) )


;;; 14.2.27 revappend
(deftest cl-14-27-001 () (revappend '(1 2 3) '())        (3 2 1))
(deftest cl-14-27-002 () (revappend '(1 2 3) '(a . b))   (3 2 1 A . B))
(deftest cl-14-27-003 () (revappend '() '(a b c))        (A B C))
(deftest cl-14-27-004 () (revappend '(1 2 3) 'a)         (3 2 1 . A))
(deftest cl-14-27-005 () (revappend '() 'a)              A) 


;;; 14.2.8 rplaca, rplacd
(deftest cl-14-08-001 ()
    (let ((some-list (list* 'one 'two 'three 'four)))
      (values
        (copy-list (rplaca some-list 'uno))     ; (uno two three . four)
        (copy-list some-list)                   ; (uno two three . four)
        (rplacd (last some-list) (list 'IV))    ; (three IV)
        some-list ) )                           ; (thrre IV)
  (values
    (uno two three . four)
    (uno two three . four)
    (three IV)
    (uno two three IV) ) )


;;; 14.2.47 set-exclusive
(deftest cl-14-27-001 ()
    (set-exclusive-or '(1 "a" "b") '(1 "A" "b"))
 ("b" "A" "b" "a") )

(deftest cl-14-27-002 ()
    (set-exclusive-or '(1 "a" "b") '(1 "A" "b") :test #'equal)
  ("A" "a") )

(deftest cl-14-27-003 ()
    (set-exclusive-or '(1 "a" "b") '(1 "A" "b") :test 'equalp)
  NIL )


;;; 14.2.11 sublis
(deftest cl-14-11-001 ()
    (sublis '((x . 100) (z . zprime))
            '(plus x (minus g z x p) 4 . x) )
 (PLUS 100 (MINUS G ZPRIME 100 P) 4 . 100) )

(deftest cl-14-11-002 ()
    (sublis '(((+ x y) . (- x y)) ((- x y) . (+ x y)))
            '(* (/ (+ x y) (+ x p)) (- x y))
            :test #'equal )
  (* (/ (- X Y) (+ X P)) (+ X Y)) )

;; Note: ANSI spec uses stringp without wrapper, it may not work if
;; stringp returns non-nil value otherthan t.
(deftest cl-14-11-003 ()
    (let ((tree1 '(1 (1 2) ((1 2 3)) (((1 2 3 4))))))
      (values (sublis '((3 . "three")) tree1) 
              (sublis '((t . "string"))
                      (sublis '((1 . "") (4 . 44)) tree1)
                      :key (lambda (x) (not (null (stringp x)))) )
              tree1 ) )
 (values
    (1 (1 2) ((1 2 "three")) (((1 2 "three" 4))))
    ("string" ("string" 2) (("string" 2 3)) ((("string" 2 3 44))))
    (1 (1 2) ((1 2 3)) (((1 2 3 4)))) ) )

(deftest cl-14-11-004 ()
    (let ((tree2 '("one" ("one" "two") (("one" "Two" "three")))))
      (values (sublis '(("two" . 2)) tree2) 
              tree2   
             (sublis '(("two" . 2)) tree2 :test 'equal) ) ) 
 (values
    ("one" ("one" "two") (("one" "Two" "three"))) 
    ("one" ("one" "two") (("one" "Two" "three")))
    ("one" ("one" 2) (("one" "Two" "three"))) ) )

;;; 14.2.48 subsetp
(deftest cl-14-48-001 () (subsetp '(1) '(1 "a" (1 2))) :boolean t)
(deftest cl-14-48-002 () (subsetp '((1 2)) '(1 "a" (1 2))) nil)

(deftest cl-14-48-003 ()
    (subsetp '((1 2)) '(1 "a" (1 2)) :test 'equal)
  :boolean t)

(deftest cl-14-48-004 ()
    (subsetp '(1 "A") '(1 "a" (1 2)) :test #'equalp)
  :boolean t )

(deftest cl-14-48-005 () (subsetp '((1) (2)) '((1) (2)))
  nil )

(deftest cl-14-48-006 () (subsetp '((1) (2)) '((1) (2)) :key #'car)
  :boolean t )



;;; 14.2.12 subst
(deftest cl-14-12-001 () 
    (let ((tree1 '(1 (1 2) (1 2 3) (1 2 3 4))))
      (values (subst "two" 2 tree1) (subst "five" 5 tree1)) )
  (values
    (1 (1 "two") (1 "two" 3) (1 "two" 3 4))
    (1 (1 2) (1 2 3) (1 2 3 4)) ) )

(deftest cl-14-12-002 () 
    (subst 'tempest 'hurricane '(shakespeare wrote (the hurricane)))
 (SHAKESPEARE WROTE (THE TEMPEST)) )

(deftest cl-14-12-003 () 
    (subst 'foo 'nil '(shakespeare wrote (twelfth night)))
  (SHAKESPEARE WROTE (TWELFTH NIGHT . FOO) . FOO) )

(deftest cl-14-12-004 () 
    (subst '(a . cons) '(old . pair)
           '((old . spice) ((old . shoes) old . pair) (old . pair))
           :test #'equal)
  ((OLD . SPICE) ((OLD . SHOES) A . CONS) (A . CONS)) )


;;; 14.2.12 subst-if
;;;
;;; Note: ANSI-CL says (1 x) for subst-if-not. It should be bug.
;;;
(deftest cl-14-12-101 () 
    (let ((tree1 '(1 (1 2) (1 2 3) (1 2 3 4))))
      (values (subst-if 5 #'listp tree1)                        ; 5
              (subst-if '(x) (complement #'consp) tree1) ) )    ; (1 x)
 (values 5 ((X) ((X) (X) X) ((X) (X) (X) X) ((X) (X) (X) (X) X) X)) )


;;; 14.2.30 tailp
(deftest cl-14-30-001 () 
    (let* ((list '(a b c))
           (vector (vector list (cddr list) (copy-list list)
                        '(f g h) '() 'd 'x))
           (results '()) )
      (dotimes (i (length vector))
        (push (tailp (svref vector i) list) results) )
      (values-list (nreverse results)) )
  (values t t nil nil t nil nil) )


(deftest cl-14-30-002 () 
    (let* ((list '(a b c . d))
           (vector (vector list (cddr list) (copy-list list)
                            '(f g h) '() 'd 'x))
           (results '()) )
      (dotimes (i (length vector))
        (push (tailp (svref vector i) list) results) )
      (values-list (nreverse results)) )
  (values t t nil nil nil t nil) )


;;; 14.2.13 tree-equal
(deftest cl-14-13-001 () 
    (let ((tree1 '(1 (1 2)))
          (tree2 '(1 (1 2))))
     (values
        (tree-equal tree1 tree2)        ;  true
        (eql tree1 tree2) ) )           ; false
  (values t nil) )

(deftest cl-14-30-002 () 
    (let ((tree1 '('a ('b 'c)))
          (tree2 '('a ('b 'c))) )
     (tree-equal tree1 tree2 :test 'eq) )
  :boolean t )



;;; 14.2.49 union
(deftest cl-14-49-001 () 
    (sort (union '(a b c) '(f a d e)) #'string<)
  (a b c d e f) )

(deftest cl-14-49-002 () 
    (sort
      (mapcar #'car (union '((x 5) (y 6)) '((z 2) (x 4)) :key #'car))
      #'string< )
  (x y z) )

(deftest cl-14-49-003 () 
    (sort (nunion '(a b c) '(f a d e)) #'string<)
  (a b c d e f) )

(deftest cl-14-49-004 () 
    (sort
      (mapcar #'car (nunion '((x 5) (y 6)) '((z 2) (x 4)) :key #'car))
      #'string< )
  (x y z) )
