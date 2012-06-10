;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-17-cl-sequence.lisp#1 $
(in-package :tc-user)

;;;; 17.3.1  sequence
(deftest cl-17-01-001 () (typep '()    'sequence) :boolean t)
(deftest cl-17-01-002 () (typep '(a)   'sequence) :boolean t)
(deftest cl-17-01-003 () (typep '(a b) 'sequence) :boolean t)

(deftest cl-17-01-101 () (typep #()    'sequence) :boolean t)
(deftest cl-17-01-102 () (typep #(a)   'sequence) :boolean t)
(deftest cl-17-01-103 () (typep #(a b) 'sequence) :boolean t)

(deftest cl-17-01-201 () (typep ""    'sequence) :boolean t)
(deftest cl-17-01-202 () (typep "a"   'sequence) :boolean t)
(deftest cl-17-01-203 () (typep "a b" 'sequence) :boolean t)

(deftest cl-17-01-301 () (typep #*   'sequence) :boolean t)
(deftest cl-17-01-302 () (typep #*0  'sequence) :boolean t)
(deftest cl-17-01-303 () (typep #*1  'sequence) :boolean t)
(deftest cl-17-01-304 () (typep #*01 'sequence) :boolean t)
(deftest cl-17-01-305 () (typep #*10 'sequence) :boolean t)

(deftest cl-17-01-401 () (typep 1  'sequence) nil)
(deftest cl-17-01-402 () (typep 1f0  'sequence) nil)
(deftest cl-17-01-403 () (typep 1d0  'sequence) nil)
(deftest cl-17-01-404 () (typep 1/2  'sequence) nil)
(deftest cl-17-01-405 () (typep (1+ most-positive-fixnum) 'sequence) nil)
(deftest cl-17-01-406 () (typep 'a 'sequence) nil)


;;;; 17.3.2  copy-seq
;;;; 17.3.3  elt

;;;; 17.3.4 fill
(deftest cl-17-04-001 ()
    (labels (
      (test (expect seq item &rest args)
        (append (test-aux expect (concatenate 'list seq) item args)
                (test-aux expect (concatenate 'list seq) item args) ) )

      (test-aux (expect seq item args)
        (let ((result (apply #'fill seq item args)))
          (unless (sequence-equal (coerce expect (type-of seq)) result)
            `((fill ,seq ,item ,@args)) ) ) )
      )
      ;;
      (test nil nil nil)
      (test "----"                "****" #\-)
      (test "012ee"               "1234" #\e :start 3)
      (test '(a b lose lose lose) '(a b c d e) 'lose :start 2)
      (test '(a b lose lose e)    '(a b c d e) 'lose :start 2 :end 4)
     ) ; labels
  nil )


;;;; 17.3.5 make-sequence
(deftest cl-17-05-001 ()
    (make-sequence 'list 0) nil )

(deftest cl-17-05-002 ()
    (make-sequence 'string 26 :initial-element #\.)
  ".........................." )

(deftest cl-17-05-003 ()
    (make-sequence 'bit-vector 3 :initial-element 0)
  #*000 )

(deftest cl-17-05-004 ()
    (let ((s (make-sequence '(vector double-float) 2 :initial-element 1d0)))
      (values (array-element-type s) (length s) (elt s 0) (aref s 1)) )
  (values double-float 2 1d0 1d0) )

(deftest cl-17-05-005 ()
    (make-sequence 'list 3) (nil nil nil))

(deftest cl-17-05-006 ()
    (make-sequence '(vector bit) 5 :initial-element 0) #*00000)

; Note: CLISP does
; (make-sequence 'array 5 :initial-element t)  #(t t t t t)

; (make-sequence '(vector * 2) 3) should signal an error
; (make-sequence '(vector * 4) 3) should signal an error


;;; 17.3.6 subseq
(deftest cl-17-06-001 () (subseq "Foobar" 3 5) "ba")
(deftest cl-17-06-002 () (subseq '(a b c) 1)   (b c))
(deftest cl-17-06-003 () (subseq "012345" 2)   "2345")
(deftest cl-17-06-004 () (subseq "012345" 3 5) "34")

(deftest cl-17-06-005 ()
    (let ((s (copy-seq "012345")))
      (values (setf (subseq s 0 2) "A") s) )
  (values "A" "A12345") )

(deftest cl-17-06-006 ()
    (let ((s (copy-seq "012345")))
      (values (setf (subseq s 4) "abc") s) )
  (values "abc" "0123ab") )

(deftest cl-17-06-007 ()
    (let ((s (copy-seq "Foobar")))
      (setf (subseq s 3 5) "le")
      s )
 "Fooler" )

(deftest cl-17-06-008 ()
    (subseq #*000000000000000000000000000000000100000 0 34)
  ; 1234567890123456789012345678901234
  #*0000000000000000000000000000000001 )

(deftest cl-17-06-009 ()
    (subseq #*000000000000000000000000000000000100000 1 34)
     ; 123456789012345678901234567890123
  #*000000000000000000000000000000001 )


(deftest cl-17-06-010 ()
    (subseq #*000000000000000000000000000000000100000 2 34)
  ; 12345678901234567890123456789012
  #*00000000000000000000000000000001 )

(deftest cl-17-06-011 ()
    (subseq #*000000000000000000000000000000000100000 3 34)
  ; 1234567890123456789012345678901
  #*0000000000000000000000000000001 )

(deftest cl-17-06-012 ()
    (subseq #*000000000000000000000000000000000100000 4 34)
  ; 123456789012345678901234567890
   #*000000000000000000000000000001 )

(deftest cl-17-06-013 ()
    (subseq #*000000000000000000000000000000000100000 3 35)
   #*00000000000000000000000000000010 )

(deftest cl-17-06-014 ()
    (subseq #*000000000000000000000000000000000100000 4 35)
 #*0000000000000000000000000000010 )


;;; 17.3.7 map
(deftest cl-17-07-001 ()
    (map 'string #'(lambda (x y)
                      (char "01234567890ABCDEF" (mod (+ x y) 16)))
           '(1 2 3 4)
           '(10 9 8 7))  "AAAA")

(deftest cl-17-07-002 ()
    (let ((seq '("lower" "UPPER" "" "123")))
      (values (map nil #'nstring-upcase seq) seq) )
  (values nil ("LOWER" "UPPER" "" "123")) )

(deftest cl-17-07-003 ()
    (map 'list #'- '(1 2 3 4)) (-1 -2 -3 -4) )

(deftest cl-17-07-004 ()
    (map 'string
          #'(lambda (x) (if (oddp x) #\1 #\0))
          '(1 2 3 4))
  "1010" )

(deftest cl-17-07-005 ()
    (map '(vector * 4) #'cons "abc" "de")
  :error error )


;;; 17.3.8 map-into
(deftest cl-17-08-001 ()
    (let ((a (list 1 2 3 4))
          (b (list 10 10 10 10)) )
     (values (map-into a #'+ a b) a b) )
  (values (11 12 13 14) (11 12 13 14) (10 10 10 10)) )

(deftest cl-17-08-002 ()
    (let ((a (list 11 12 13 14))
          (k '(one two three)) )
     (values (map-into a #'cons k a)  a k) )
  (values
    ((ONE . 11) (TWO . 12) (THREE . 13) 14)
    ((ONE . 11) (TWO . 12) (THREE . 13) 14)
    (one two three) ) )

(deftest cl-17-08-003 ()
    (let ((a (list 1 2 3 4)))
     (equal (map-into a #'gensym) a) )
  t )

;;; 17.3.9 reduce
(deftest cl-17-09-001 ()
    (reduce #'* '(1 2 3 4 5))  120)

(deftest cl-17-09-001 ()
    (reduce #'append '((1) (2)) :initial-value '(i n i t)) (I N I T 1 2) )

(deftest cl-17-09-002 ()
    (reduce #'append '((1) (2)) :from-end t
            :initial-value '(i n i t))
  (1 2 I N I T) )

;==(- (- (- 1 2) 3) 4)
(deftest cl-17-09-003 () (reduce #'- '(1 2 3 4)) -8)
;Alternating sum. ==(- 1 (- 2 (- 3 4)))
(deftest cl-17-09-004 () (reduce #'- '(1 2 3 4) :from-end t) -2)
(deftest cl-17-09-005 () (reduce #'+ '())  0)
(deftest cl-17-09-006 () (reduce #'+ '(3)) 3)
(deftest cl-17-09-007 () (reduce #'+ '(foo)) FOO)
(deftest cl-17-09-008 () (reduce #'list '(1 2 3 4))  (((1 2) 3) 4))

(deftest cl-17-09-009 ()
    (reduce #'list '(1 2 3 4) :from-end t)
  (1 (2 (3 4))) )

(deftest cl-17-09-010 ()
    (reduce #'list '(1 2 3 4) :initial-value 'foo)
  ((((foo 1) 2) 3) 4) )

(deftest cl-17-09-011 ()
    (reduce #'list '(1 2 3 4)
        :from-end t :initial-value 'foo)
  (1 (2 (3 (4 foo)))) )

(deftest cl-17-09-012 () (reduce #'* '#(1 2 3 4 5)) 120)

;==(- (- (- 1 2) 3) 4)
(deftest cl-17-09-013 () (reduce #'- '#(1 2 3 4))  -8)
;Alternating sum. ==(- 1 (- 2 (- 3 4)))
(deftest cl-17-09-014 () (reduce #'- '#(1 2 3 4) :from-end t) -2)
(deftest cl-17-09-015 () (reduce #'+ '#())  0)
(deftest cl-17-09-016 () (reduce #'+ '#(3)) 3)
(deftest cl-17-09-016 () (reduce #'+ '#(foo)) FOO)
(deftest cl-17-09-017 () (reduce #'list '#(1 2 3 4))  (((1 2) 3) 4))

(deftest cl-17-09-018 ()
    (reduce #'list '#(1 2 3 4) :from-end t)
  (1 (2 (3 4))) )

(deftest cl-17-09-019 ()
    (reduce #'list '#(1 2 3 4) :initial-value 'foo)
  ((((foo 1) 2) 3) 4) )

(deftest cl-17-09-020 ()
    (reduce #'list '#(1 2 3 4)
        :from-end t :initial-value 'foo)
  (1 (2 (3 (4 foo)))) )


;;; 17.3.10 count
;;; 17.3.10 count-if
;;; 17.3.10 count-if-not
;;;  list x vector
;;;  from-end
(deftest cl-17-10-001 ()
    (labels (
      (test (expect fn item seq &rest args)
        (append
          (test-aux expect fn item (coerce seq 'list) args)
          (test-aux expect fn item (coerce seq 'vector) args)
          (test-aux expect fn item (make-displaced seq) args) ) )

      (test-aux (expect fn item seq args)
        (loop
          for from-end in '(nil t)
          for result = (apply fn item seq :from-end from-end args)
          unless (eql result expect)
            collect `(,fn ,item ',seq :from-end ,from-end ,@args) ) )
        )
        (append
          (test 0 'count nil nil)
          (test 2 'count #\a "how many A's are there in here?")
          (test 2 'count 1 '(1 2 3 4 5 1 2 3))
          (test 1 'count 1 '(1 2 3 4 5 1 2 3) :start 3)
          (test 0 'count 1 '(1 2 3 4 5 1 2 3) :start 3 :end 5)
          (test 2 'count-if (complement #'oddp) '((1) (2) (3) (4)) :key #'car)
          (test 2 'count-if-not #'oddp '((1) (2) (3) (4)) :key #'car)
          (test 2 'count-if #'upper-case-p "The Crying of Lot 49" :start 4) ) )
 nil )


;;;; 17.3.11 length
(deftest cl-17-11-001 () (length nil) 0)
(deftest cl-17-11-002 () (length '(a)) 1)
(deftest cl-17-11-003 () (length '(a b)) 2)
(deftest cl-17-11-004 () (length '(a . b)) :error error)


(deftest cl-17-11-101 () (length #())  0)
(deftest cl-17-11-102 () (length #(a)) 1)
(deftest cl-17-11-102 () (length #(a b)) 2)


(deftest cl-17-11-201 () (length "")   0)
(deftest cl-17-11-202 () (length "a")  1)
(deftest cl-17-11-203 () (length "ab") 2)

(deftest cl-17-11-301 () (length #*)   0)
(deftest cl-17-11-302 () (length #*1)  1)
(deftest cl-17-11-303 () (length #*0)  1)
(deftest cl-17-11-304 () (length #*01) 2)


;;; 17.3.12 reverse
(deftest cl-17-12-001 ()
    (let ((str "abc")) (values (reverse str)  str))
  (values "cba" "abc") )

(deftest cl-17-12-002 ()
    (let ((str (copy-seq "abc"))) (values (nreverse str) str))
  (values "cba" "cba") )

(deftest cl-17-12-003 ()
    (let ((lst '(1 2 3))) (values (reverse lst) lst))
  (values (3 2 1) (1 2 3)) )

(deftest cl-17-12-004 ()
    (let ((lst `(1 2 3))) (values (nreverse lst) lst))
  (values (3 2 1) (1)) )

(deftest cl-17-12-005 () (reverse "foo") "oof")
(deftest cl-17-12-006 () (reverse '(a b (c d) e)) (e (c d) b a))


;;; 17.3.13 sort, stable-sort
(deftest cl-17-13-001 ()
    (let ((tester (copy-seq "lkjashd")))    ; "lkjashd"
      (sort tester #'char-lessp)  )
  "adhjkls")

(deftest cl-17-13-002 ()
    (let ((tester (list '(1 2 3) '(4 5 6) '(7 8 9))))
                ;  ((1 2 3) (4 5 6) (7 8 9))
      (sort tester #'> :key #'car) )
  ((7 8 9) (4 5 6) (1 2 3)) )

(deftest cl-17-13-003 ()
    (let ((tester (list 1 2 3 4 5 6 7 8 9 0))) ; (1 2 3 4 5 6 7 8 9 0)
      (stable-sort tester #'(lambda (x y) (and (oddp x) (evenp y)))) )
  (1 3 5 7 9 2 4 6 8 0) )


(deftest cl-17-13-004 ()
    (let ((committee-data
                 (vector (list (list "JonL" "White") "Iteration")
                         (list (list "Dick" "Waters") "Iteration")
                         (list (list "Dick" "Gabriel") "Objects")
                         (list (list "Kent" "Pitman") "Conditions")
                         (list (list "Gregor" "Kiczales") "Objects")
                         (list (list "David" "Moon") "Objects")
                         (list (list "Kathy" "Chapman") "Editorial")
                         (list (list "Larry" "Masinter") "Cleanup")
                         (list (list "Sandra" "Loosemore") "Compiler") ) ) )
        (coerce (sort committee-data #'string-lessp :key #'cadar) 'list) )
((("Kathy" "Chapman") "Editorial")
 (("Dick" "Gabriel") "Objects")
 (("Gregor" "Kiczales") "Objects")
 (("Sandra" "Loosemore") "Compiler")
 (("Larry" "Masinter") "Cleanup")
 (("David" "Moon") "Objects")
 (("Kent" "Pitman") "Conditions")
 (("Dick" "Waters") "Iteration")
 (("JonL" "White") "Iteration")) )


(deftest cl-17-13-005 ()
    (let ((committee-data
                 (vector (list (list "JonL" "White") "Iteration")
                         (list (list "Dick" "Waters") "Iteration")
                         (list (list "Dick" "Gabriel") "Objects")
                         (list (list "Kent" "Pitman") "Conditions")
                         (list (list "Gregor" "Kiczales") "Objects")
                         (list (list "David" "Moon") "Objects")
                         (list (list "Kathy" "Chapman") "Editorial")
                         (list (list "Larry" "Masinter") "Cleanup")
                         (list (list "Sandra" "Loosemore") "Compiler") ) ) )
        (sort committee-data #'string-lessp :key #'cadar)
        (coerce (stable-sort committee-data #'string-lessp :key #'cadr)
                'list) )
 ;; Note that individual alphabetical order within `committees'
 ;; is preserved.
 ((("Larry" "Masinter") "Cleanup")
     (("Sandra" "Loosemore") "Compiler")
     (("Kent" "Pitman") "Conditions")
     (("Kathy" "Chapman") "Editorial")
     (("Dick" "Waters") "Iteration")
     (("JonL" "White") "Iteration")
     (("Dick" "Gabriel") "Objects")
     (("Gregor" "Kiczales") "Objects")
     (("David" "Moon") "Objects")) )


;;; 17.3.14 find
(deftest cl-17-14-001 ()
    (find #\d "here are some letters that can be looked at" :test #'char>)
  #\Space )

(deftest cl-17-14-002 () (find 'a '(d c b a)) a)


;;; 17.3.14 find-if
(deftest cl-17-14-101 ()
    (find-if #'oddp '(1 2 3 4 5) :end 3 :from-end t)  3)

(deftest cl-17-14-102 ()
    (find-if #'oddp #(1 2 3 4 5) :end 3 :from-end t)  3)

(deftest cl-17-14-103 ()
    (find-if (complement #'complexp)
                 '(3.5 2 #C(1.0 0.0) #C(0.0 1.0))
                 :start 2)   nil)

(deftest cl-17-14-104 ()
    (find-if (complement #'complexp)
                 '#(3.5 2 #C(1.0 0.0) #C(0.0 1.0))
                 :start 2)   nil)


;;; 17.3.17 mismatch
(deftest cl-17-17-001 ()
    (mismatch "abcd" "ABCDE" :test #'char-equal)
  4)

(deftest cl-17-17-002 ()
    (mismatch '(3 2 1 1 2 3) '(1 2 3) :from-end t) 3)

(deftest cl-17-17-003 ()
    (mismatch '(3 2 1 1 2 3) #(1 2 3) :from-end t) 3)

(deftest cl-17-17-004 ()
    (mismatch #(3 2 1 1 2 3) '(1 2 3) :from-end t) 3)

(deftest cl-17-17-005 ()
    (mismatch #(3 2 1 1 2 3) #(1 2 3) :from-end t) 3)

(deftest cl-17-17-006 ()
    (mismatch '(1 2 3) '(2 3 4) :test (complement #'eq) :key #'oddp)  nil )

(deftest cl-17-17-007 ()
    (mismatch '(1 2 3) #(2 3 4) :test (complement #'eq) :key #'oddp)  nil )

(deftest cl-17-17-008 ()
    (mismatch #(1 2 3) '(2 3 4) :test (complement #'eq) :key #'oddp)  nil )

(mismatch #(1 2 3) #(2 3 4) :test (complement #'eq) :key #'oddp)  nil

(deftest cl-17-17-009 ()
    (mismatch '(1 2 3 4 5 6) '(3 4 5 6 7) :start1 2 :end2 4)  nil )

(deftest cl-17-17-010 ()
    (mismatch '(1 2 3 4 5 6) #(3 4 5 6 7) :start1 2 :end2 4)  nil )

(deftest cl-17-17-011 ()
    (mismatch #(1 2 3 4 5 6) '(3 4 5 6 7) :start1 2 :end2 4)  nil )

(deftest cl-17-17-012 ()
    (mismatch #(1 2 3 4 5 6) #(3 4 5 6 7) :start1 2 :end2 4)  nil )


;;; 17.3.18 replace
(deftest cl-17-18-001 ()
    (replace "abcdefghij" "0123456789" :start1 4 :end1 7 :start2 4)
  "abcd456hij" )

(deftest cl-17-18-002 ()
    (let ((lst (copy-seq "012345678")))
      (values (replace lst lst :start1 2 :start2 0) lst) )
  (values "010123456" "010123456") )

(deftest cl-17-18-003 ()
    (replace #(0 1 2 3 4 5 6 7 8) #(- a b c d e f g h) :start1 2 :end1 5
                                                       :start2 2 :end2 8 )
  #(0 1 b c d 5 6 7 8) )

(deftest cl-17-18-004 ()
    (replace (copy-seq #*00000000) #*01110)
  #*01110000 )

(deftest cl-17-18-006 ()
    (let ((bv (replace (copy-seq #40*0) #30*00001 :start1 5 :start2 5)))
      (values bv (count 1 bv)) )
  (values #40*0000011111111111111111111111110000000000 25) )


;; 3 dword to 2 dword
;; 0123456789
;; 0000000000  0
;; 1111111111  10
;; 1111111111  20
;; 1111111111  30
;; 1111111111  40
;; 1111111111  50
;; 0000000000  60
;; 0000000000  70
;; 0000000000  80
;; 0000000000  90
(deftest cl-17-18-007 ()
    (let ((bv (replace (copy-seq #100*0) #100*1
                            :start1 10 :end1 60
                            :start2 30 :end2 80 )) )
      (values (count 1 bv)
              (aref bv 9)
              (aref bv 10)
              (aref bv 11)
              (aref bv 59)
              (aref bv 60)
              (aref bv 61) ) )
  (values 50 0 1 1 1 0 0) )

;; backward copy
;;      0123456789
;;      1111100000
;;      ---dddd---
;;      -ssss-----
;;      1111111---
(deftest cl-17-18-008 ()
  (let ((b1 (make-array 100 :element-type 'bit :initial-element 0)))
    (fill b1 1 :end 50)
    (replace b1 b1 :start1 30 :end1 70 :start2 10 :end2 50)
    (count 1 b1) )
  70 )


;; forward copy
;;      0123456789
;;      1111100000
;;      -dddd-----
;;      ---ssss---
;;      1110000---
(deftest cl-17-18-009 ()
  (let ((b1 (make-array 100 :element-type 'bit :initial-element 0)))
    (fill b1 1 :end 50)
    (replace b1 b1 :start2 30 :end2 70 :start1 10 :end1 50)
    (count 1 b1) )
  30 )


;;; 17.3.16 search
(deftest cl-17-16-001 () (search "dog" "it's a dog's life")           7)
(deftest cl-17-16-002 () (search "dog" "dog dog dog cat" :from-end t) 8)

(deftest cl-17-16-003 () (search "foo" "foobar")  0)
(deftest cl-17-16-004 () (search "bar" "foobar")  3)
(deftest cl-17-16-005 () (search "oba" "foobar")  2)

(deftest cl-17-16-006 () (search '(0 1) '(2 4 6 1 3 5) :key #'oddp) 2)
(deftest cl-17-16-007 () (search '(0 1) #(2 4 6 1 3 5) :key #'oddp) 2)
(deftest cl-17-16-008 () (search #(0 1) '(2 4 6 1 3 5) :key #'oddp) 2)
(deftest cl-17-16-009 () (search #(0 1) #(2 4 6 1 3 5) :key #'oddp) 2)

(deftest cl-17-16-010 () (search '(0 1) '(- 0 1 - 0 1 -) :from-end t) 4)
(deftest cl-17-16-011 () (search '(0 1) #(- 0 1 - 0 1 -) :from-end t) 4)
(deftest cl-17-16-012 () (search #(0 1) '(- 0 1 - 0 1 -) :from-end t) 4)
(deftest cl-17-16-013 () (search #(0 1) #(- 0 1 - 0 1 -) :from-end t) 4)


;;; 17.3.19 substitute
(deftest cl-17-19-001 () (substitute #\. #\SPACE "0 2 4 6") "0.2.4.6")
(deftest cl-17-19-002 () (substitute 9 4 '(1 2 4 1 3 4 5)) (1 2 9 1 3 9 5))

(deftest cl-17-19-003 () (substitute 9 4 '(1 2 4 1 3 4 5) :count 1)
  (1 2 9 1 3 4 5) )

(deftest cl-17-19-004 ()
    (substitute 9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5) )

(deftest cl-17-19-005 ()
    (substitute 9 3 '(1 2 4 1 3 4 5) :test #'>)
  (9 9 4 9 3 4 5) )

(deftest cl-17-19-006 ()
    (substitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car)
  ((1) (2) (3) 0) )

(deftest cl-17-19-007 ()
    (substitute-if 9 #'oddp '(1 2 4 1 3 4 5))
  (9 2 4 9 9 4 9) )

(deftest cl-17-19-008 ()
    (substitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5) )


;;; 17.3.20 concatenate
(deftest cl-17-20-001 ()
    (concatenate 'string "all" " " "together" " " "now")
  "all together now" )

(deftest cl-17-20-002 ()
    (concatenate 'list "ABC" '(d e f) #(1 2 3) #*1011)
 (#\A #\B #\C D E F 1 2 3 1 0 1 1) )

(deftest cl-17-20-003 ()
    (concatenate 'list)
  NIL )

;  (concatenate '(vector * 2) "a" "bc") should signal an error

(deftest cl-17-20-004 ()
    (concatenate 'list   '(1 2) #(a 3))   (1 2 a 3) )

(deftest cl-17-20-005 ()
  (concatenate 'vector '(1 2) #(a 3))   #(1 2 a 3))


;;; 17.3.21 merge
(deftest cl-17-21-001 ()
    (merge 'list (list 1 3 4 6 7) (list 2 5 8) #'<)
  (1 2 3 4 5 6 7 8) )

(deftest cl-17-21-002 ()
    (merge 'string (copy-seq "BOY") (copy-seq "nosy") #'char-lessp)
  "BnOosYy")

(deftest cl-17-21-003 ()
    (merge 'vector (vector '(red . 1) '(blue . 4))
                   (vector '(yellow . 2) '(green . 7))
           #'< :key #'cdr )
 #((red . 1) (yellow . 2) (blue . 4) (green . 7)) )

; (merge '(vector * 4) '(1 5) '(2 4 6) #'<) should signal an error


;;; 17.3.22 delete
;;; 17.3.22 remove
(deftest cl-17-22-001 ()
    (labels (
      (test (expect item seq &rest args)
        (append
          (test-aux expect item (concatenate 'list   seq)   args)
          (test-aux expect item (concatenate 'vector seq) args) ) )

      (test-aux (expect item seq args)
        (loop
          for fn in '(remove delete)
          for result = (apply fn item seq args)
            unless (sequence-equal result (coerce expect (type-of seq)))
              collect `(,fn ,item ,seq ,@args) ) )
      )
      (let ((tester '(1 2 4 1 3 4 5)))
        (test nil            nil nil)
        (test "abc"          #\- "--abc--")
        (test "--abc"        #\- "--abc--" :start 2)
        (test "-abc-"        #\- "--abc--" :start 1 :end 6)
        (test '(1 2 1 3 5)   4 tester)
        (test '(1 2 1 3 4 5) 4 tester :count 1)
        (test '(1 2 4 1 3 5) 4 tester :from-end t)
        (test '(4 3 4 5)     3 tester :test #'>)

        (test '(1 3 5 9)        4 '(1 3 4 5 9))
        (test '(1 2 1 3 5)      4 '(1 2 4 1 3 4 5))
        (test '(1 2 1 3 4 5)    4 '(1 2 4 1 3 4 5) :count 1)
        (test '(1 2 4 1 3 5)    4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
        (test '(4 3 4 5)        3 '(1 2 4 1 3 4 5) :test #'>)
      ) ) ; labels
 nil )


;;; 17.3.22 delete-if
;;; 17.3.22 remove-if
(deftest cl-17-22-101 ()
    (labels (
      (test (expect predicate sequence &rest args)
        (append
          (test-aux expect '(remove-if delete-if) predicate sequence args)
          (test-aux expect
                    '(delete-if-not remove-if-not)
                    (complement predicate)
                    sequence args ) ) )
      (test-aux (expect fns predicate sequence args)
        (loop
          for fn in fns
          append
            (loop
              for type in '(list vector)
              for seq = (concatenate type sequence)
              for result = (apply fn predicate seq args)
              unless (sequence-equal result (coerce expect type))
                collect `(,fn ,predicate ,seq ,@args) )) )
      )
      (let ((tester '(1 2 4 1 3 4 5)))
        (test nil            #'oddp  nil)
        (test '(2 4 4)       #'oddp  tester)
        (test '(1 3 5)       #'evenp tester)
        (test '(1 2 4 1 3 5) #'evenp tester :count 1 :from-end t)
      ) ) ; labels
  nil )


;;; 17.3.23 delete-duplicates
(deftest cl-17-23-001 () (delete-duplicates (list))             nil)
(deftest cl-17-23-002 () (delete-duplicates (vector))           #())
(deftest cl-17-23-003 () (delete-duplicates (list 1))           (1))
(deftest cl-17-23-004 () (delete-duplicates (vector 1))         #(1))
(deftest cl-17-23-005 () (delete-duplicates (list 1 2))         (1 2))
(deftest cl-17-23-006 () (delete-duplicates (vector 1 2))       #(1 2))
(deftest cl-17-23-007 () (delete-duplicates (list 1 2 1))       (2 1))
(deftest cl-17-23-008 () (delete-duplicates (vector 1 2 1))     #(2 1))
(deftest cl-17-23-009 () (delete-duplicates (list 1 2 1 2))     (1 2))
(deftest cl-17-23-010 () (delete-duplicates (vector 1 2 1 2))   #(1 2))

(deftest cl-17-23-011 ()
    "Note: oddp returns generalized boolean. To get same result in ANSI spec., we need to wrap ODDP to return boolean."
    (delete-duplicates (list 0 1 2 3 4 5 6)
        :key (lambda (x) (not (null (oddp x)))) )
    (5 6) )

(deftest cl-17-23-012 ()
    "Note: oddp returns generalized boolean. To get same result in ANSI spec., we need to wrap ODDP to return boolean."
    (delete-duplicates (vector 0 1 2 3 4 5 6)
        :key (lambda (x) (not (null (oddp x)))) )
    #(5 6) )

(deftest cl-17-23-013 ()
    "Note: oddp returns generalized boolean. To get same result in ANSI spec., we need to wrap ODDP to return boolean."
    (delete-duplicates (list 0 1 2 3 4 5 6)
        :start 1 :end 6
        :key (lambda (x) (not (null (oddp x)))) )
    (0 4 5 6) )

(deftest cl-17-23-014 ()
    "Note: oddp returns generalized boolean. To get same result in ANSI spec., we need to wrap ODDP to return boolean."
    (delete-duplicates (vector 0 1 2 3 4 5 6)
        :start 1 :end 6
        :key (lambda (x) (not (null (oddp x)))) )
    #(0 4 5 6) )

(deftest cl-17-23-015 ()
    (delete-duplicates (concatenate 'list "abcbbdde") :from-end t)
    #.(concatenate 'list "abcde") )

(deftest cl-17-23-016 ()
    (delete-duplicates (concatenate 'vector "abcbbdde") :from-end t)
    #.(concatenate 'vector "abcde") )

(deftest cl-17-23-017 ()
    (delete-duplicates (concatenate 'list "abcbbdde")
        :start 2 :end 6 :from-end nil )
    #.(concatenate 'list "abcbdde") )

(deftest cl-17-23-018 ()
    (delete-duplicates (concatenate 'vector "abcbbdde")
        :start 2 :end 6 :from-end nil)
    #.(concatenate 'vector "abcbdde") )

(deftest cl-17-23-019 ()
    (delete-duplicates (concatenate 'list "abcbbdde")
        :start 2 :end 6 :from-end t )
    #.(concatenate 'list "abcbdde") )

(deftest cl-17-23-020 ()
    (delete-duplicates (concatenate 'vector "abcbbdde")
        :start 2 :end 6 :from-end t )
    #.(concatenate 'vector "abcbdde") )

(deftest cl-17-23-021 ()
    (delete-duplicates (concatenate 'list '((foo #\a) (bar #\%) (baz #\A)))
        :test #'char-equal :key #'cadr )
    ((bar #\%) (baz #\A)) )

(deftest cl-17-23-022 ()
    (delete-duplicates (concatenate 'vector '((foo #\a) (bar #\%) (baz #\A)))
        :test #'char-equal :key #'cadr )
    #((bar #\%) (baz #\A)) )

(deftest cl-17-23-023 ()
    (delete-duplicates (concatenate 'list '((foo #\a) (bar #\%) (baz #\A)))
        :test #'char-equal :key #'cadr :from-end nil )
    ((bar #\%) (baz #\A)) )

(deftest cl-17-23-024 ()
    (delete-duplicates (concatenate 'vector '((foo #\a) (bar #\%) (baz #\A)))
        :test #'char-equal :key #'cadr :from-end t )
    #((foo #\a) (bar #\%)) )


;;; 17.3.23 remove-duplicates
(deftest cl-17-23-101 () (remove-duplicates (list))             nil)
(deftest cl-17-23-102 () (remove-duplicates (vector))           #())
(deftest cl-17-23-103 () (remove-duplicates (list 1))           (1))
(deftest cl-17-23-104 () (remove-duplicates (vector 1))         #(1))
(deftest cl-17-23-105 () (remove-duplicates (list 1 2))         (1 2))
(deftest cl-17-23-106 () (remove-duplicates (vector 1 2))       #(1 2))
(deftest cl-17-23-107 () (remove-duplicates (list 1 2 1))       (2 1))
(deftest cl-17-23-108 () (remove-duplicates (vector 1 2 1))     #(2 1))
(deftest cl-17-23-109 () (remove-duplicates (list 1 2 1 2))     (1 2))
(deftest cl-17-23-110 () (remove-duplicates (vector 1 2 1 2))   #(1 2))

(deftest cl-17-23-111 ()
    (remove-duplicates (list 0 1 2 3 4 5 6)
        :key (lambda (x) (not (null (oddp x)))) )
    (5 6) )

(deftest cl-17-23-112 ()
    (remove-duplicates (vector 0 1 2 3 4 5 6)
        :key (lambda (x) (not (null (oddp x)))) )
    #(5 6) )

(deftest cl-17-23-113 ()
    "Note: oddp returns generalized boolean. To get same result in ANSI spec., we need to wrap ODDP to return boolean."
    (remove-duplicates (list 0 1 2 3 4 5 6)
        :start 1 :end 6
        :key (lambda (x) (not (null (oddp x)))) )
    (0 4 5 6) )

(deftest cl-17-23-114 ()
    "Note: oddp returns generalized boolean. To get same result in ANSI spec., we need to wrap ODDP to return boolean."
    (remove-duplicates (vector 0 1 2 3 4 5 6)
        :start 1 :end 6
        :key (lambda (x) (not (null (oddp x)))) )
    #(0 4 5 6) )

(deftest cl-17-23-115 ()
    (remove-duplicates (concatenate 'list "abcbbdde") :from-end t)
    #.(concatenate 'list "abcde") )

(deftest cl-17-23-116 ()
    (remove-duplicates (concatenate 'vector "abcbbdde") :from-end t)
    #.(concatenate 'vector "abcde") )

(deftest cl-17-23-117 ()
    (remove-duplicates (concatenate 'list "abcbbdde")
        :start 2 :end 6 :from-end nil )
    #.(concatenate 'list "abcbdde") )

(deftest cl-17-23-118 ()
    (remove-duplicates (concatenate 'vector "abcbbdde")
        :start 2 :end 6 :from-end nil)
    #.(concatenate 'vector "abcbdde") )

(deftest cl-17-23-119 ()
    (remove-duplicates (concatenate 'list "abcbbdde")
        :start 2 :end 6 :from-end t )
    #.(concatenate 'list "abcbdde") )

(deftest cl-17-23-120 ()
    (remove-duplicates (concatenate 'vector "abcbbdde")
        :start 2 :end 6 :from-end t )
    #.(concatenate 'vector "abcbdde") )

(deftest cl-17-23-121 ()
    (remove-duplicates (concatenate 'list '((foo #\a) (bar #\%) (baz #\A)))
        :test #'char-equal :key #'cadr )
    ((bar #\%) (baz #\A)) )

(deftest cl-17-23-122 ()
    (remove-duplicates (concatenate 'vector '((foo #\a) (bar #\%) (baz #\A)))
        :test #'char-equal :key #'cadr )
    #((bar #\%) (baz #\A)) )

(deftest cl-17-23-123 ()
    (remove-duplicates (concatenate 'list '((foo #\a) (bar #\%) (baz #\A)))
        :test #'char-equal :key #'cadr :from-end nil )
    ((bar #\%) (baz #\A)) )

(deftest cl-17-23-124 ()
    (remove-duplicates (concatenate 'vector '((foo #\a) (bar #\%) (baz #\A)))
        :test #'char-equal :key #'cadr :from-end t )
    #((foo #\a) (bar #\%)) )
