;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-06-cl-loop.lisp#1 $
(in-package #:tc-user)

(deftest cl-06-00-001 ()
    (loop for numlist in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
          for a of-type integer = (first numlist)
          and b of-type integer = (second numlist)
          and c of-type float = (third numlist)
          collect (list c b a) )
  ((4.0 2 1) (8.3 6 5) (10.4 9 8)) )


(deftest cl-06-00-002 ()
    (loop for (a b c) of-type (integer integer float) in
       '((1 2 4.0) (5 6 8.3) (8 9 10.4))
       collect (list c b a) )
  ((4.0 2 1) (8.3 6 5) (10.4 9 8)) )


(deftest cl-06-00-003 ()
    (loop for (a b c) of-type float in
           '((1.0 2.0 4.0) (5.0 6.0 8.3) (8.0 9.0 10.4))
           collect (list c b a))
 ((4.0 2.0 1.0) (8.3 6.0 5.0) (10.4 9.0 8.0)) )


(deftest cl-06-00-004 ()
    (loop for i to 5 collect i collect i)
  (0 0 1 1 2 2 3 3 4 4 5 5) )

(deftest cl-06-00-005 ()
    (loop for i to 5 collect i into x collect i into x finally (return x))
  (0 0 1 1 2 2 3 3 4 4 5 5) )

;; Initialize and declare variables in parallel by using the AND construct.
(deftest cl-06-00-006 ()
    (loop with (a b) of-type float = '(1.0 2.0)
          and (c d) of-type integer = '(3 4)
          and (e f)
          return (list a b c d e f))
 (1.0 2.0 3 4 NIL NIL) )

;; If nil is used in a destructuring list, no variable is provided for its
;; place. 
(deftest cl-06-00-007 ()
    (loop for (a nil b) = '(1 2 3)
          do (return (list a b)))
 (1 3) )


;; Note that dotted lists can specify destructuring. 
(deftest cl-06-00-008 ()
    (loop for (x . y) = '(1 . 2)
          do (return y))
 2 )

(deftest cl-06-00-009 ()
    (loop for ((a . b) (c . d)) of-type ((float . float) (integer . integer)) in
           '(((1.2 . 2.4) (3 . 4)) ((3.4 . 4.6) (5 . 6)))
           collect (list a b c d))
 ((1.2 2.4 3 4) (3.4 4.6 5 6)) )


;;; 6.1.2.1.1 The for-as-arithmetic subclause

(deftest cl-06-21-001 ()
    (let ((x 1)) (loop for i from x by (incf x) to 10 collect i))
 (1 3 5 7 9) )

(deftest cl-06-21-002 ()
    (let ((x 1)) (loop for i by (incf x) from x to 10 collect i))
 (2 4 6 8 10) )


;;; 6.1.2.1.1.1 Examples of for-as-arithmetic subclause

;; Print some numbers.
(deftest cl-06-21-101 ()
    (with-output-to-string (*standard-output*)
      (loop for i from 1 to 3
             do (print i)) )
"
1 
2 
3 " )


;; Print every third number.
(deftest cl-06-21-102 ()
    (with-output-to-string (*standard-output*)
     (loop for i from 10 downto 1 by 3
           do (print i)) )
"
10 
7 
4 
1 " )

;; Step incrementally from the default starting value.
(deftest cl-06-21-103 ()
    (with-output-to-string (*standard-output*)
     (loop for i below 3
           do (print i)) )
"
0 
1 
2 " )


;;; 6.1.2.1.2.1 Examples of for-as-in-list subclause
;; Print every item in a list.
(deftest cl-06-21-201 ()
    (with-output-to-string (*standard-output*)
      (loop for item in '(1 2 3) do (print item)) )
"
1 
2 
3 " )
 
;; Print every other item in a list.
(deftest cl-06-21-202 ()
    (with-output-to-string (*standard-output*)
      (loop for item in '(1 2 3 4 5) by #'cddr
           do (print item)) )
"
1 
3 
5 " )
 
;; Destructure a list, and sum the x values using fixnum arithmetic.
(deftest cl-06-21-203 ()
    (loop for (item . x) of-type (t . fixnum) in '((A . 1) (B . 2) (C . 3))
          unless (eq item 'B) sum x)
 4 )

;6.1.2.1.3.1 Examples of for-as-on-list subclause
;; Collect successive tails of a list.
(deftest cl-06-21-303 ()
    (loop for sublist on '(a b c d)
           collect sublist)
 ((A B C D) (B C D) (C D) (D)) )

;; Print a list by using destructuring with the loop keyword ON.
(deftest cl-06-21-302 ()
    (with-output-to-string (*standard-output*)
      (loop for (item) on '(1 2 3)
           do (print item)) )
"
1 
2 
3 " )

;;; 6.1.2.1.4.1 Examples of for-as-equals-then subclause
;; Collect some numbers.
(deftest cl-06-21-401 ()
    (loop for item = 1 then (+ item 10)
           for iteration from 1 to 5
           collect item )
 (1 11 21 31 41) )


;6.1.2.1.5.1 Examples of for-as-across subclause
(deftest cl-06-21-501 ()
    (with-output-to-string (stream)
       (loop for char across (the simple-string "FooBar")
             do (write-char char stream) ) )
  "FooBar" )

;;; 6.1.2.1.7.1 Examples of for-as-package subclause
;;;
;;; Note: present-symbol returns both of internal and external symbols.
;;; This behavior is different from CLtL2.
;;;
(deftest cl-06-21-701 ()
    (let ((*package* (or (find-package "LOOP-TEST-PACKAGE")
                         (make-package "LOOP-TEST-PACKAGE") )) )
      ;; For effect, intern some symbols
      (read-from-string "(THIS IS A TEST)")
      (export (intern "THIS"))
      (sort (loop for x being each present-symbol of *package*
                  collect (symbol-name x) )
            #'string< ) )
  ("A" "IS" "TEST" "THIS") )


;;; 6.1.2.2 Local Variable Initializations
(deftest cl-06-22-001 ()
    (loop with a = 1
           with b = (+ a 2)
           with c = (+ b 3)
           return (list a b c))
  (1 3 6) )

(deftest cl-06-22-002 ()
    (loop with a = 1 
           and b = 2 
           and c = 3
           return (list a b c))
  (1 2 3) )

;;; 6.1.2.2.1 Examples of WITH clause
;; These bindings occur in sequence.
(deftest cl-06-22-100 ()
    (loop with a = 1 
          with b = (+ a 2) 
          with c = (+ b 3)
          return (list a b c))
 (1 3 6) )

;; These bindings occur in parallel.
(deftest cl-06-22-101 ()
    (let ((a 5) (b 10))
       (loop with a = 1
             and b = (+ a 2)
             and c = (+ b 3)
             return (list a b c)) )
 (1 7 13) )
 
;; This example shows a shorthand way to declare local variables 
;; that are of different types.
(deftest cl-06-22-102 ()
    (loop with (a b c) of-type (float integer float)
          return (format nil "~A ~A ~A" a b c))
  "0.0 0 0.0" )
 
;; This example shows a shorthand way to declare local variables 
;; that are the same type.
(deftest cl-06-22-103 ()
    (loop with (a b c) of-type float 
           return (format nil "~A ~A ~A" a b c))
 "0.0 0.0 0.0" )

;;; 6.1.3 Value Accumulation Clauses
;; Collect every name and the kids in one list by using 
;; COLLECT and APPEND.
(deftest cl-06-30-001 ()
    (loop for name in '(fred sue alice joe june)
          for kids in '((bob ken) () () (kris sunshine) ())
          collect name
          append kids)
  (FRED BOB KEN SUE ALICE JOE KRIS SUNSHINE JUNE) )

;;; 6.1.3.1 Examples of COLLECT clause
;; Collect all the symbols in a list.
(deftest cl-06-31-001 ()
    (loop for i in '(bird 3 4 turtle (1 . 4) horse cat)
          when (symbolp i) collect i)
  (BIRD TURTLE HORSE CAT) )

;; Collect and return odd numbers.
(deftest cl-06-31-002 ()
    (loop for i from 1 to 10
           if (oddp i) collect i)
 (1 3 5 7 9) )

;; Collect items into local variable, but don't return them.
(deftest cl-06-031-003 ()
    (with-output-to-string (*standard-output*)
      (loop for i in '(a b c d) by #'cddr
            collect i into my-list
            finally (print my-list)) )
"
(A C) " )

;;; 6.1.3.2 Examples of APPEND and NCONC clauses
;; Use APPEND to concatenate some sublists.
(deftest cl-06-32-001 ()
    (loop for x in '((a) (b) ((c)))
          append x)
  (A B (C)) )

;; NCONC some sublists together.  Note that only lists made by the
;; call to LIST are modified.
(deftest cl-06-32-002 ()
    (loop for i upfrom 0 
          as x in '(a b (c))
          nconc (if (evenp i) (list x) nil))
  (A (C)) )

;;; 6.1.3.3 Examples of COUNT clause
(deftest cl-06-33-001 ()
    (loop for i in '(a b nil c nil d e)
          count i)
  5 )

;;; 6.1.3.4 Examples of MAXIMIZE and MINIMIZE clauses
(deftest cl-06-33-002 ()
    (loop for i in '(2 1 5 3 4) maximize i) 5 )

(deftest cl-06-33-003 ()
    (loop for i in '(2 1 5 3 4) minimize i) 1 )

(deftest cl-06-33-004 ()
 (loop for i to 10 when (oddp i) minimize i) 1 )

;; In this example, FIXNUM applies to the internal variable that holds
;; the maximum value.
(deftest cl-06-33-005 ()
    (let ((series '(1.2 4.3 5.7)))
       (loop for v in series 
             maximize (round v) of-type fixnum) )
  6 )
 
;; In this example, FIXNUM applies to the variable RESULT.
(deftest cl-06-33-006 ()
    (let ((series '(1.2 4.3 5.7)))
     (loop for v of-type float in series
           minimize (round v) into result of-type fixnum
           finally (return result)) )
 1 )

;; 6.1.3.5 Examples of SUM clause
(deftest cl-06-33-007 ()
    (loop for i of-type fixnum in '(1 2 3 4 5)
           sum i)
  15 )

(deftest cl-06-33-008 ()
    (let ((series '(1.2 4.3 5.7)))
     (loop for v in series 
           sum (* 2.0 v)) )
  22.4 )

;;;; 6.1.4 Termination Test Clauses

;;; 6.1.4.1 Examples of REPEAT clause
(deftest cl-06-41-001 ()
    (with-output-to-string (*standard-output*)
       (loop repeat 3
             do (format t "What I say three times is true.|")) )
  "What I say three times is true.|What I say three times is true.|What I say three times is true.|" )

(deftest cl-06-41-002 ()
    (with-output-to-string (*standard-output*)
      (loop repeat -15
            do (format t "What you see is what you expect~%")) )
  "" )


;;;; 6.1.4.2 Examples of ALWAYS, NEVER, and THEREIS clauses

;; Make sure I is always less than 11 (two ways).
;; The FOR construct terminates these loops.
(deftest cl-06-42-001 ()
    (loop for i from 0 to 10
          always (< i 11))
  :boolean t )

(deftest cl-06-42-002 ()
    (loop for i from 0 to 10
          never (> i 11))
  :boolean t )
 
;; If I exceeds 10 return I; otherwise, return NIL.
;; The THEREIS construct terminates this loop.
(deftest cl-06-42-003 ()
    (loop for i from 0
           thereis (when (> i 10) i) )
  11 )

;;; The FINALLY clause is not evaluated in these examples.
(deftest cl-06-42-003 ()
    (with-output-to-string (*standard-output*)
      (loop for i from 0 to 10
            always (< i 9)
            finally (print "you won't see this")) )
  "" )

(deftest cl-06-42-004 ()
    (with-output-to-string (*standard-output*)
     (loop never t
           finally (print "you won't see this")) )
  "" )

(deftest cl-06-42-005 ()
    (loop thereis "Here is my value"
          finally (print "you won't see this") )
  "Here is my value" )
 
;; The FOR construct terminates this loop, so the FINALLY clause 
;; is evaluated.
(deftest cl-06-42-006 ()
    (with-output-to-string (*standard-output*)
       (loop for i from 1 to 10
             thereis (> i 11)
             finally (prin1 'got-here)) )
  "GOT-HERE" )


;;; 6.1.4.3 Examples of WHILE and UNTIL clauses
;(loop while (hungry-p) do (eat))
 
;; UNTIL NOT is equivalent to WHILE.
;(loop until (not (hungry-p)) do (eat))
 
;; Collect the length and the items of STACK.
(deftest cl-06-43-001 ()
    (let ((stack '(a b c d e f)))
       (loop for item = (length stack) then (pop stack)
             collect item
             while stack) )
  (6 A B C D E F) )

;; Use WHILE to terminate a loop that otherwise wouldn't terminate.
;; Note that WHILE occurs after the WHEN.
(deftest cl-06-43-002 ()
    (loop for i fixnum from 3
           when (oddp i) collect i
           while (< i 5))
  (3 5) )

;;;; 6.1.5 Unconditional Execution Clauses

;;; 6.1.5.1 Examples of unconditional execution
;; Print numbers and their squares.
;; The DO construct applies to multiple forms.
(deftest cl-06-51-001 ()
    (with-output-to-string (*standard-output*)
       (loop for i from 1 to 3
           do (print i)
              (print (* i i))) )
"
1 
1 
2 
4 
3 
9 " )

;;;; 6.1.6 Conditional Execution Clauses

;;;6.1.6.1 Examples of WHEN clause
;; Signal an exceptional condition.
(deftest cl-06-61-001 ()
    (loop for item in '(1 2 3 a 4 5)
         when (not (numberp item))
          return (list 'cerror "enter new value" "non-numeric value: ~s" item))
  (cerror "enter new value" "non-numeric value: ~s" A) )

;; The previous example is equivalent to the following one.
(deftest cl-06-61-002 ()
    (loop for item in '(1 2 3 a 4 5)
           when (not (numberp item))
            do (return 
                (list 'cerror "Enter new value" "non-numeric value: ~s" item)))
  (cerror "Enter new value" "non-numeric value: ~s" A) )

;; This example parses a simple printed string representation from 
;; BUFFER (which is itself a string) and returns the index of the
;; closing double-quote character.
(deftest cl-06-61-003 ()
    (let ((buffer "\"a\" \"b\""))
       (loop initially (unless (char= (char buffer 0) #\")
                         (loop-finish))
             for i of-type fixnum from 1 below (length (the string buffer))
             when (char= (char buffer i) #\")
              return i ) )
 2 )

;; The collected value is returned.
(deftest cl-06-61-004 ()
    (loop for i from 1 to 10
           when (> i 5)
             collect i
           finally (prin1 'got-here))
 (6 7 8 9 10) )

;; Return both the count of collected numbers and the numbers.
(deftest cl-06-61-005 ()
    (loop for i from 1 to 10
           when (> i 5)
             collect i into number-list
             and count i into number-count
           finally (return (values number-count number-list)) )
 (values 5 (6 7 8 9 10)) )

;;;; 6.1.7 Miscellaneous Clauses

;;; 6.1.7.1 Control Transfer Clauses

;;;6.1.7.1.1 Examples of NAMED clause
;; Just name and return.
(deftest cl-06-71-001 ()
    (let ((string (make-array 10 :element-type 'character
                                 :initial-element #\_
                                 :fill-pointer 0
                                 :adjustable t )))
      (with-output-to-string (*standard-output* string)
        (values (loop named max
                  for i from 1 to 10
                  do (print i)
                  do (return-from max 'done) )
                string ) ) )
  (values done
"
1 " ) )

;;;; 6.1.8 Examples of Miscellaneous Loop Features
(deftest cl-06-80-001 ()
    (let ((i 0))                     ; no loop keywords are used
        (loop (incf i) (if (= i 3) (return i))))
 3 )

(deftest cl-06-80-002 ()
 (let ((i 0)(j 0))
    (tagbody
      (loop (incf j 3) (incf i) (if (= i 3) (go exit)))
      exit)
    j)
  9 )

;; In the following example, the variable x is stepped before y is stepped;
;; thus, the value of y reflects the updated value of x: 
(deftest cl-06-80-003 ()
    (loop for x from 1 to 10 
           for y = nil then x 
           collect (list x y))
 ((1 NIL) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (10 10)) )

;; In this example, x and y are stepped in parallel: 
(deftest cl-06-80-004 ()
    (loop for x from 1 to 10 
           and y = nil then x 
           collect (list x y))
  ((1 NIL) (2 1) (3 2) (4 3) (5 4) (6 5) (7 6) (8 7) (9 8) (10 9)) )

;;; 6.1.8.1 Examples of clause grouping
;; Group conditional clauses.
(deftest cl-06-81-001 ()
    (loop for i in '(1 324 2345 323 2 4 235 252)
           when (oddp i)
             do (print i)
             and collect i into odd-numbers
             and do (terpri)
           else                              ; I is even.
             collect i into even-numbers
           finally
             (return (values odd-numbers even-numbers)))
 (values (1 2345 323 235) (324 2 4 252)) )

;; Collect numbers larger than 3.
(deftest cl-06-81-002 ()
    (loop for i in '(1 2 3 4 5 6)
           when (and (> i 3) i)
           collect it)                      ; IT refers to (and (> i 3) i).
  (4 5 6) )

;; Find a number in a list.
(deftest cl-06-81-003 ()
    (loop for i in '(1 2 3 4 5 6)
           when (and (> i 3) i)
           return it)
  4 )

;; The above example is similar to the following one.
(deftest cl-06-81-004 ()
    (loop for i in '(1 2 3 4 5 6)
           thereis (and (> i 3) i))
  4 )


;; Nest conditional clauses.
(deftest cl-06-81-005 ()
    (let ((list '(0 3.0 apple 4 5 9.8 orange banana)))
       (loop for i in list
             when (numberp i)
               when (floatp i)
                 collect i into float-numbers
               else                                  ; Not (floatp i)
                 collect i into other-numbers
             else                                    ; Not (numberp i)
               when (symbolp i) 
                 collect i into symbol-list
               else                                  ; Not (symbolp i)
                 do (error "found a funny value in list ~S, value ~S~%" list i)
             finally
               (return (values float-numbers other-numbers symbol-list))))
  (values (3.0 9.8) (0 4 5) (APPLE ORANGE BANANA)) )


;; Without the END preposition, the last AND would apply to the
;; inner IF rather than the outer one.
(deftest cl-06-81-006 ()
    (with-output-to-string (*standard-output*)
      (loop for x from 0 to 3 
           do (print x)
           if (zerop (mod x 2))
             do (princ " a")
              and if (zerop (floor x 2))
                    do (princ " b")
                    end
              and do (princ " c")) )
"
0  a b c
1 
2  a c
3 " )


;;; hash-table
(deftest cl-06-81-007 ()
    (let ((htb (make-hash-table)))
      (dotimes (i 26) (setf (gethash i htb) (code-char (+ 65 i))))
      (loop for key being each hash-key of htb
        collect key ) )
  (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25) )

(deftest cl-06-81-008 ()
    (let ((htb (make-hash-table)))
      (dotimes (i 26) (setf (gethash i htb) (code-char (+ 65 i))))
      (coerce
        (loop for val being each hash-value of htb
          collect val )
        'string ) )
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" )

(deftest cl-06-81-009 ()
    (let ((htb (make-hash-table)))
      (dotimes (i 5) (setf (gethash i htb) (code-char (+ 65 i))))
      (loop for key being each hash-key of htb using (hash-value val)
        collect (cons key val) ) )
  ((0 . #\A) (1 . #\B) (2 . #\C) (3 . #\D) (4 . #\E)) )

(deftest cl-06-81-010 ()
    (let ((htb (make-hash-table)))
      (dotimes (i 5) (setf (gethash i htb) (code-char (+ 65 i))))
      (loop for val being each hash-value of htb using (hash-key key)
        collect (cons key val) ) )
  ((0 . #\A) (1 . #\B) (2 . #\C) (3 . #\D) (4 . #\E)) )

