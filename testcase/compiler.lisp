(setq c6::*settings* nil)
(load "../lisp/genesis/g00-loadup.lisp")

(load "/proj/evcl/lisp/macro/m03-eval.lisp")
(load "/proj/evcl/lisp/macro/m05-control.lisp")
(load "/proj/evcl/lisp/devel/d05-control.lisp")


(load "/proj/evcl/lisp/devel/d03-eval.lisp")

(load "/proj/evcl/lisp/macro/m06-loop.lisp")
(load "/proj/evcl/lisp/macro/m07-object.lisp")

(defun foo (funobj ofs)
    (declare (type (integer 0 #x10000) ofs))
  (let ((tagof-funobj 1))
    (without-garbage-collection
      (the native-code-function
        (.unbox-int (+ (.box-int funobj) (- ofs tagof-funobj) ofs)) ) ) ) )

(defun keywordp (x)
  (and (symbolp x) (eq (symbol-package x) #.(symbol-package :key))) )

; Function :TOPLEVEL

(setq c6::*settings* '((:time . 1)))
cf "/proj/evcl3/lisp/regex/regex-byte-exec"


(setq c6::*settings* '((* (:debug . 9))))

(setq c6::*settings* '((c6::x86-ra-ls (:debug . 9))))
(load "/proj/evcl3/compiler/test1.lisp")

(setq c6::*settings* '((c6::x86-ra-ls (:debug . 9))))
(load "/proj/evcl3/compiler/test2.lisp")

(load "/proj/evcl/lisp/clos/o11-method.lisp")


(setq c6::*settings* '((c6::x64-ra-ls (:debug . 9))))
(defun foo (n) (dotimes (i n) (print i)))
(defun foo (n) (declare (type fixnum n)) (dotimes (i n) (print i)))
(defun foo () (loop for i below 10 do (print i)))
(defun foo () (dotimes (i 10) (print i)))
(defun foo (n) (declare (type fixnum n)) (loop repeat n do (print 'bar)))
(defun foo (list) (dolist (x list) (print x)))

;; We should declare v. Since it is legal to pass empty sequence such as
;; nil, "", #*, and so on.
(defun foo (v x) (dotimes (i (length v) v) (setf (svref v i) x)))

(defun foo (v) (dotimes (i (length v) v) (incf (svref v i))))


(defun foo (v x)
  (block nil
    (let ((i 0)
          (n (length v)) )
      (tagbody
        (if (not (< i n)) (go end))
        (print 'foo)
       loop
        (setf (svref v i) x)
        (incf i)
        (if (< i n) (go loop))
       end
        (return v) ) )) )
  


(defun foo (n) (bar) n)

(setq c6::*settings* '((c6::x64-ra-ls (:debug . 9))))
(load "/proj/evcl3/compiler/test1.lisp")

m1 (loop for x to 10 for y = (sin x) do (print y))
m1 (do ((x init step)) (test result) body)

(load "/proj/evcl/lisp/macro/m06-loop.lisp")


(setq c6::*settings* '((c6::x64-ra-ls (:debug . 9))))
(load "/proj/evcl3/compiler/test2.lisp")



(setq c6::*settings* '((* (:debug . 9))))
(setq c6::*settings* '((c6::cl-parse (:debug . 9))))
(setq c6::*settings* nil)

(c::parse-macro-aux 'foo '(a b) '((list a b)))

(defun foo (x) (values x (nreverse x) (nreverse x)))
;; funcall
(funcall (macro-function 'defun) '(defun foo (x) 1) nil)


;; VALUES
(lambda (a b c d e) (list a b c d e))
(lambda (a b c d e f) (list a b c d e f))
(lambda (a b c d e f g h i j k l m n) (list a b c d e f g h i j k l m n))
(defun foo (a b c d e f g h i j k l m n) (list n m l k j i h g f e d c b a))
(defun foo (a b c d e f g h i j k l) (list l k j i h g f e d c b a))
(defun foo (a b c d e f g h i j k l) (list b a d c f e h g j i l k))



;; RA
#'(lambda (x) (foo) x)

;; RA
#'(lambda (x y) (foo) (list x y))

;; if
#'(lambda (x) (if x (foo x) (bar x)) x)

;; if
#'(lambda (x) (if x (foo)))

;; PHI
#'(lambda (x) (foo (if x x 1234)))

;; PHI
#'(lambda (x) (foo (if x x (bar))) x)

;; setq
#'(lambda (x y) (if (not x) (setq y 'foo)) y)

;; TLV
(defun foo () (setq *gensym-counter* (1+ *gensym-counter*)))

;; special variable
(defun foo () (setq *foo* (1+ *foo*)))

;; RA
#'(lambda () (foo (bar) (baz)))

;; the
#'(lambda () (the fixnum (foo)))
(defun foo (x y) (declare (type fixnum x y)) (the fixnum (+ x y)))

;; block
(defun block-01 () (block foo (bar)))
(defun block-02 () (block foo (bar)) 1)

;; return-from
#'(lambda (x) (block foo (if x (return-from foo 1)) (bar)))

;; tagbody
#'(lambda () (tagbody 1 (foo) 2 (bar) 3 (baz)))

;; go
#'(lambda (x) (tagbody (init) loop (foo) (if (bar) (go loop))))

;; let
#'(lambda () (let ((x (foo)) (y x)) (list x y)))

;; let*
#'(lambda () (let* ((x (foo)) (y x)) (list x y)))

;; ValuesInsn
#'(lambda (x y) (list y x))     ; swap
#'(lambda (x y z) (list y z x)) ; rotate
#'(lambda (a b c d e f g h) h)
#'(lambda (a b c d e f g h) (list h g f e d c b a))
#'(lambda (eax edx esp) (list esp eax))


;; flet
(flet ((foo () (if (bar) (foo)))) #'foo)

;; labels
(labels ((foo () (if (bar) (foo)))) #'foo)

(labels (((setf foo) (x y) (list x y))) #'(setf foo))



;; CLtL2 p.48
(defun illegal-example ()
  (let ((y (block here #'(lambda (z) (return-from here z)))))
    (if (numberp y) y (funcall y 5)) ) )

;; self-tail-call
(defun foo () (print 'foo) (foo))


;; (tak 18 12 6) => 7
(defun tak (x y z)
    (if (>= y x)
        z
        (tak (tak (1- x) y z)
             (tak (1- y) z x)
             (tak (1- z) x y) )) )

;; 0903 i1586.4.1/3.1GHz 218ms
;; 0904 i1586.4.1/3.1GHz 140ms (self tail call)

;; let special
#'(lambda () (let ((*print-length* 1)) (foo)))
#'(lambda () (let ((*print-length* 1) (*print-level* 2)) (foo)))
#'(lambda () (let ((*print-length* (foo)) (*print-level* (bar))) (baz)))
#'(lambda () (let ((*print-length* 1)) (foo)) (let ((*print-length* 1)) (bar)))
#'(lambda () (let ((*print-length* 1)) (foo) (let ((*print-length* 1)) (bar))))

#'(lambda (x) (let ((*print-length* 1)) (if x x (foo))))


;; let* special
#'(lambda () (let* ((*print-length* 1) (*print-level* 2)) (foo)))
#'(lambda () (let* ((*print-length* (foo)) (*print-level* (bar))) (baz)))
#'(lambda () (let* ((*print-length* (foo)) (*print-level* (bar))) (error "quux")))


;; lambda special
#'(lambda (*print-length*) (foo))

;; BUGBUG: We should merge tail of BB17 and BB18 to integrate two CLOSE
;; instructions.
(defun foo ()
  (block nil
    (let ((*print-length* 1))
      (when (foo) (return))
      (baz) )) )

;; declare special
(lambda (*foo*) (declare (special *foo*)) (foo))

;; flet - upvar
(defun flet-unused () (flet ((foo () (format t "foo~%"))) 1))


(defun upvar-fixed-1 (x)
  (flet ((foo () (format t "foo: x=~S~%" x)))
      (declare (notinline foo))
    (foo) ) )

(defun upvar-fixed-2 (x)
  (flet ((foo () (format t "foo: x=~S~%" x))
         (bar () (format t "bar: x=~S~%" x)) )
      (declare (notinline foo bar))
    (foo) (bar) ) )

(defun upvar-fixed-3 (x)
  (flet ((foo ()
    (flet ((bar () (format t "bar: x=~S~%" x)))
        (declare (notinline bar))
      (format t "foo: x=~S~%" x) (bar) ) ) )
    (declare (notinline foo))
    (foo) ) )

(defun upvar-fixed-4 (x)
  (labels ((foo () (format t "foo=~S~%" x) (bar))
           (bar () (format t "bar: ~S~%" x)) )
      (declare (notinline foo bar))
    (foo) ) )

(defun upvar-var-1 (x)
  (labels ((foo () (format t "foo=~S~%" x) (bar))
           (bar () (format t "bar: ~S~%" x)) )
      (declare (notinline foo bar))
    (foo) (bar) ) )

(defun upvar-var-2 (x)
  (labels ((foo () (bar))
           (bar () (setq x (1+ x))) )
      (declare (notinline foo bar))
    (foo) (bar) x ) )

;; for split closure - cvar-ro
(defun upvar-var-3 (x)
  (flet ((bar () (flet ((foo () x)) (declare (notinline foo)) (foo))))
    #'bar ) )

;; for split closure - cvar-rw
(defun upvar-var-4 (x)
  (flet ((bar () (flet ((foo () (setq x (1+ x))))
    (declare (notinline foo)) (foo) ) ))
    #'bar ) )

;; for split closure - cvar-rw refernced twice
(defun upvar-var-5 (x)
  (labels (
    (foo () (setq x (1+ x)))
    (bar () (format t "bar: x=~S~%" x) (foo)) )
    (declare (notinline foo))
    #'bar ) )

;; upvars in finally procedure
(defun upvar-var-6 (file)
  (lambda ()
    (let ((abort-p t))
      (unwind-protect
          (progn (process) (setq abort-p nil))
        (format t "abort-p=~S file=~S~%" abort-p file) ) ) ) )


;; closure
(defun closed-ro-1 () (let ((x 1)) #'(lambda () x)))    ; no closed variable
(defun closed-ro-2 (x) #'(lambda () x))
(defun closed-rw-1 (x) #'(lambda () (setq x (1+ x))))
(defun closed-rw-2 (x)
  (flet ((inc () (setq x (1+ x))) (dec () (setq x (1- x))))
    (values #'inc #'dec) ) )


(defun upvar-fixed-closed-ro-1 ()
  (let ((x 1))
    (flet ((foo () x))
      (values #'(lambda () (setq x (1+ x)))
              (foo) ) ) ) )


;; mixed path: check is called both non-closure and closure.
(defun upvar-mixed-1 (x)
  (labels ((check (y) (setq x y)))
    (check 'bar)
    #'(lambda () (check 'foo)) ) )

;; foo is called in multi path.
(defun upvar-mul-1 (x)
  (labels (
      (foo () (format t "foo: x=~S~%" x))
      (bar () (format t "bar: x=~S~%" x) (foo))
        ) (declare (notinline foo bar))
    (foo)
    (bar) ) )

;; locally
(lambda () (locally (declare (values fixnum)) (foo)))

;; ext:lambda-name
(lambda () (declare (ext:lambda-name (foo bar))) 1)
; (defun foo () (declare (ext:lambda-name (foo bar))) 1)  ; error


;; inner function name => (labels foo bar)
(defun fname-01 () (flet ((foo () (flet ((bar () 1)) #'bar))) (foo)))
(defun fname-02 () (return-from fname-02 1))
(defun fname-03 () (lambda () (flet ((foo () 1)) (foo))))
(defun fname-04 ()
  (lambda () (declare (ext:lambda-name bar)) (flet ((foo () 1)) (foo))) )
(defun fname-05 () (lambda () (lambda () (lambda () 1))))


;; macroexpand
(do ((x 0 (1+ x))) ((>= x 10) x) (format t "~D~%" x))
(dolist (x '(1 2 3)) (format t "~S~%" x))
(dotimes (x 10) (format t "~D~%" x))


;; from m02-backquote.lisp
;;  (cddr form) = (cdr (cdr form))
(defun quote-form-p (form keyword)
  (cond
    ((not (consp form)) nil)
    ((not (eq (car form) keyword)) nil)
    ((and (consp (cdr form)) (null (cddr form))) t)
    (t (error "Malformed ~S form: ~S" keyword form) )) )

;; &rest
(defun foo (&rest x) x)
(defun foo (a b c &rest x)  (list a b c x))
(defun foo (a b c d e f g h &rest x)  (list a b c d e f g h x))

(defun foo (&rest x)
  (dolist (a x)
    (format t "~S~%" a) ) )

(defun foo (&rest x) (declare (dynamic-extent x)) (format t "foo=~S~%" x))

(defun foo (x &rest y) (declare (dynamic-extent y))
    (format t "foo=~S ~S~%" x y)
    (apply #'bar x :key1 "BOO"  y) )


(defun bar (x &rest y)
    (declare (dynamic-extent y))
 (format t "bar ~S ~S~%" x y) )


;; for member
(defun rest-01 (a b &rest x) (format t "foo: ~S ~S ~S~%" a b x))

(defun rest-02 (a b &rest x) (declare (dynamic-extent x)) (format t "foo: ~S ~S ~S~%" a b x))



(defun foo (&rest x)
    (declare (dynamic-extent x))
  (format t "x=~S~%" x)
  (dolist (a x)
    (format t "~S~%" a) ) )

;; &aux
(defun foo (&aux x (y 1)) (list x y))
(defun foo (&aux x (y 1)) (declare (special y)) (list x y))

;; values
(defun foo () (values 1 2 3))
(defun foo () (values 1 2 3) 1) ; void
(defun foo () (values 1 (return-from foo 2) 3)) ; unreachable
(defun foo () (values))

;; for analyze-lambda-list
(defun foo () (values 'a 'b 'c 'd 'e 'f 'g))

;; multiple-value-bind
(defun foo () (multiple-value-bind (x y) (bar) (list x y)))
(defun foo () (multiple-value-bind (x y) (values 1 2) (list x y)))
(defun foo () (multiple-value-bind (x y) (values) (list x y)))
(defun foo () (multiple-value-bind (x y) (values 1 2 3) (list x y)))

(defun foo ()
  (labels ((bar () (values 1 2)))
    (multiple-value-bind (x y) (bar) (list x y)) ) )

;;; for analyze-lambda-list
(defun bar (x)
  (multiple-value-bind (extra
                        requireds optionals rests keywords
                        auxiliaries
                        var-env ) (c::analyze-lambda-list x t nil)
    (format t "; extra=~S~%" extra)
    (format t "; requireds=~S~%" requireds)
    (format t "; optionals=~S~%" optionals)
    (format t "; rests=~S~%" rests)
    (format t "; keywords=~S~%" keywords)
    (format t "; auxiliaries=~S~%" auxiliaries)
    (format t "; var-end=~S~%" var-env) ) )

;;; from parse-destructuring-bind
(defun bar (x)
  (multiple-value-bind (extra
                        requireds optionals rests keywords
                        auxiliaries
                        var-env ) (c::analyze-lambda-list x t nil)
       (when extra
          (macro-error "Can't use lambda-list-keyword ~S after ~S."
            (first extra)
            (ldiff pattern extra) ))
    (format t "; requireds=~S~%" requireds)
    (format t "; optionals=~S~%" optionals)
    (format t "; rests=~S~%" rests)
    (format t "; keywords=~S~%" keywords)
    (format t "; auxiliaries=~S~%" auxiliaries)
    (format t "; var-end=~S~%" var-env) ) )



;; multiple-value-call
(defun mvcall-01 () (multiple-value-call 'bar))
(defun mvcall-02 () (multiple-value-call #'bar))
(defun mvcall-03 () (multiple-value-call #'bar 1))
(defun mvcall-04 () (multiple-value-call #'bar 1 2))
(defun mvcall-05 () (multiple-value-call #'bar (baz)))
(defun mvcall-06 () (multiple-value-call #'bar (baz) (quux)))
(defun mvcall-07 (fn) (multiple-value-call fn (bar)))
(defun mvcall-08 () (multiple-value-call #'list (bar)))


;; &optional
(defun foo (a &optional b) (list a b))
(defun foo (a &optional (b)) (list a b))
(defun foo (a &optional (b 'default)) (list a b))
(defun foo (a &optional (b (1+ a))) (list a b))
(defun foo (a &optional (b (1+ a) b-p)) (list a b b-p))
(defun foo (&optional a b c) (list a b c))

(defun bar () (declare (special *foo*)) *foo*)
(defun foo (&optional (*foo* 10) (c (bar))) (declare (special *foo*)) c)

;; symbol-macrolet
(defun foo () (symbol-macrolet ((foo "This is foo.")) foo))
(defun foo (x) (symbol-macrolet ((foo (car x))) (setf (car x) 1)))

;; apply
(defun foo (fn x) (apply fn x))
(defun foo (fn x) (apply fn 1 2 3 x))

;; values-list
(values-list '(
    a b c d e f g h i j k l m n o p q r s t u v w x y z ; 26
    a b c d e f g h i j k l m n o p q r s t u v w x y z ; 52
    a b c d e f g h i j k l m n o p q r s t u v w x y z ; 78
    a b c d e f g h i j k l m n o p q r s t u v w x y z ; 104
    a b c d e f g h i j k l m n o p q r s t u v w x     ; 130 - 2
 ) )

;; split closure
(defun foo (x) (flet ((bar () (format t "bar=~S~%" x))) (bar) #'bar))

;; useless mutual call functions
(labels ((foo () (bar)) (bar () (foo))) 1)

;; unwind-protect
(defun foo () (unwind-protect (format t "protected~%")))
(defun foo () (unwind-protect (format t "protected~%") (bar)))

;; MVSAVE/MVRESTOR + strong save values
(defun foo ()
  (unwind-protect (let ((*print-length* nil)) (bar)) (format t "cleanup")) )

;; MVSAVE/MVRESTOR + strong save values
(defun foo ()
  (let ((*print-length* nil)) (unwind-protect (bar) (format t "cleanup"))) )

;; multiple MVSAVE/MVRESTORE
(defun foo (x *print-length*)
  (labels ((bar () (if y (return-from foo 1) (quux))))
      (declare (notinline bar))
    (when x (bar) (baz)) ) )


;; like with-open-file
(defun foo (stream)
  (labels ((my-close (a b c) (format t "my-close: ~S ~S ~S~%" a b c)))
    (let ((x :abort))
      (unwind-protect
          (progn (format t "do something~%") (setq x nil))
        (when stream (my-close stream :abort x)) ) ) ) )

;; multiple-value-prog1
(defun mvprog1-01 () (multiple-value-prog1 (bar)))
(defun mvprog1-02 () (multiple-value-prog1 (bar) (baz)))
(defun mvprog1-03 () (multiple-value-prog1 (values 1 2 3) (baz)))
(defun mvprog1-04 (x) (multiple-value-prog1 (values (car x) (cdr x)) (baz)))

;; nth-value
(defun nth-value-01 () (nth-value 0 (bar)))
(defun nth-value-02 (x) (nth-value x (bar)))
(defun nth-value-03 () (nth-value 0 (values 'a 'b 'c)))
(defun nth-value-04 () (nth-value 2 (values 'a 'b 'c)))
(defun nth-value-05 () (nth-value 4 (values 'a 'b 'c)))
(defun nth-value-06 () (nth-value 1 (bar)))
(defun bar () (values 'a 'b 'c 'd))

;; catch
(defun foo () (catch 'foo (bar) 'not-cached))

;; throw -- local catch
(defun foo (x) (catch 'foo (when x (throw 'foo 1)) 2))

;; throw -- non-literal tag
(defun foo (x) (catch 'foo (when x (throw x 1)) 2))

;; unwind-protect
(defun foo () (catch 'foo (unwind-protect (bar) (format t "cleanup~%"))))
(defun bar () (throw 'foo 'from-bar))


;; nonlocal return-from and x86-RESTOREVALUES and x86-SAVEVALUES
(defun ret-01 (x)
  (flet ((bar () (return-from ret-01 'bar)))
    (declare (notinline bar))
    (when x (bar)) 'foo ) )

;; nonlocal LiveIn
(defun ret-02 (x)
  (block bar
    (labels ((baz () (return-from bar)))
        (declare (notinline baz))
      (bar)
      (if x (baz) (setq x 'foo)) ) )
  x )

(defun ret-03 (x)
  (flet ((bar () (return-from ret-03 'bar)))
    (declare (notinline bar))
    (if x (bar) (error "foo")) ) )

(defun ret-04 (x)
  (flet ((bar () (return-from ret-04 'bar)))
    (declare (notinline bar))
    (if x (bar) (baz)) ) )


;; nonlocal go
(defun go-01 (x)
  (tagbody
    (format t "foo~%")
    (flet ((bar-1 () (go 1)) (bar-2 () (go 2)))
        (declare (notinline bar-1 bar-2))
      (if x (bar-1) (bar-2)) )
    1 (format t "one~%") (return-from go-01 1)
    2 (format t "two~%") (return-from go-01 2) ) )

;; &key
(defun key-01 (&key a b c) (list a b c))
(defun key-02 (&key (a 'init-a)) a)
(defun key-03 (&key (a (list "a")) (b (list "b"))) (values a b))

(defun key-04 (&key (a 'init-a a-p) (b 'init-b b-p))
    (values (cons a a-p) (cons b b-p)) )

(defun key-05 (&key (a 'init-a a-p) (b 'init-b b-p))
      (declare (special a))
    (values (cons a a-p) (cons b b-p)) )

(defun key-06 (&key ((:x a) 'init-a a-p) ((:y b) 'init-b b-p))
    (values (cons a a-p) (cons b b-p)) )

(defun key-07 (&key a b &allow-other-keys) (list a b))

;; for member
(defun key-08 (item list &key test key) (list item list test key))



;; optimize quality
(defun qua-01 () (declare (optimize (safety 0))) *foo*)
(defun qua-02 () (declare (optimize (safety 3))) *foo*)

;; ftype
(defun ftype-01 (x) (funcall #'(setf car) 'ftype-01 x))

;; declare values
#+error
(defun decl-01 (x) (declare (values list list)) (values x 1))

(defun decl-02 (x) (declare (values list list)) (values x nil))

(defun decl-03 (x) (declare (values list list)) x)

; runtime-cast
(defun decl-04 (x) (declare (values fixnum fixnum)) x)

(defmacro moo (a b) `'(moo ,a ,b))
(defun foo () (declare (values list (or string null))) (values (bar) (baz)))
(defun foo (x y) (declare (type fixnum x y) (values fixnum)) (+ x y))

;; typep + SELECT
(defun foo (x) (typep x 'cons))
(defun foo (x) (typep x 'package))      ; single
(defun foo (x) (typep x 'float))        ; or
(defun foo (x) (typep x 'array))        ; range
(defun foo (x) (typep x 'integer))      ; fixnum + bignum
(defun foo (x) (typep x 'rational))     ; fixnum + (or bignum ratio)
(defun foo (x) (typep x 'real))         ; fixnum + range

;; typep + BRANCH
(defun foo (x) (if (typep x 'package) (print 'true)))   ; single
(defun foo (x) (if (typep x 'float)   (print 'true)))   ; or
(defun foo (x) (if (typep x 'array)   (print 'true)))   ; range
(defun foo (x) (if (typep x 'integer) (print 'true)))   ; fixnum + bignum

;; SELECT+BRANCH
(defun foo (x) (if (not x) 'got-nil 'got-true))

;; inline
(defun foo () (labels ((exit () (error "exit"))) (exit)))

;; ref
(defun foo (x) (ref environment variables x))
(defun foo (x y) (funcall #'(setf %ref) y 'environment 'variables x))
(defun len (x) (ref simple-vector length x))


;; macrolet
(defun foo () (macrolet ((bar (x) `'(this is bar with ,x))) (bar 123)))

;; upvar + optional => We need to patch arity check instruction.
(defun foo (x)
  (labels (
    (bar (a &optional (b 'default)) (format t "bar ~S ~S x=~S~%" a b x))
    (baz () (bar 'baz))
    )
    (declare (notinline bar))
    (bar 'foo "this is optional param.")
    (baz) ) )

;; inline returns values and receives single
(defun foo ()
  (labels ((bar () (baz)))
    (let ((x (bar))) x) ) )

;; (/ 4500 501) when B=10
;         12345678123456781234567812345678
(setq u #x7FFFFFFF800000000000000000000000)
; BIGNUM: 170141183420855150474555134919112130560
;         123456781234567812345678
(setq v #x800000000000000000000001)
; BIGNUM: 39614081257132168796771975169

;; inline and block frame
(defun foo (x)
  (labels ((bar () (return-from foo 1)))
    (let ((*print-base* 16)) (if x (format t "~D" x) (bar))) ) )

;; inline and tagbody frame
(defun foo (x)
  (tagbody
    (labels ((bar () (go exit)))
      (let ((*print-base* 16)) (if x (format t "~D" x) (bar))) )
    exit ) )

;;; values declaration
(defun foo (ty)
   (declare (values t t))
    (cond
      ((null ty)    (values nil nil))
      ((symbolp ty) (parse-symbol ty))
      ((classp ty)  (values :class ty))
      ((consp ty)   (parse-cons ty))
      (t (error "Invalid type specifier: ~S" ty)) ) )

;; known values
(defun foo (a b) (multiple-value-bind (x y) (gethash a b) (list x y)))
(defun foo (a b) (unwind-protect (gethash a b) (clean)))
(defun foo (a b) (multiple-value-prog1 (gethash a b) (clean)))

;; x86:STORE+SLOT
(defun foo ()
  (let* ((htb (ref environment variables *environment*))
         (alist  (rest (gethash/eq 'cl:*macroexpand-hook* htb)))
         (tlvrec (cdr (assq 'tlv alist))) )
   (funcall #'(setf %ref) #'c::macroexpand-hook 'tlv-record 'value tlvrec) ) )

(defun foo () (do-external-symbols (x :cl) (format t "~S~%" x)))

(defun foo (x)
  (if (bar x)
    (tagbody top
      (if (baz x) (if (zerop x) (go exit)))
      (go top) exit) )
  2 )

;; from make-package
(defun foo (name)
    (loop
      (let ((present (find-package (setq name (string name)))))
        (unless present (return))
        (restart-case
            (error 'package-already-exists
                   :package present
                   :name    name )
          (store-value (another)
            :report "Use another name for new package."
            (setq name another) )) )) )


;;;; macro syntax error => should stop compilation
;;; from find-class
(defun foo (name &optional (error-p t) env)
  (let ((env (or env *environment*)))
    (loop
      (with-latch ((ref environment latch env) :shared)
        (let ((class (gethash/eq name (ref environment classes env))))
          (when class (return-from foo class))
        (setq env (ref environment outer))  ;;; ERROR!
        (when (null env) (return)) )) )
    (when error-p (error 'si::class-not-found :name name)) ) )

(defun foo (&key a b c d e f g h)
  (list a b c d e f g h) )

;;; intern-eql-specializer
;; opt-inline changes CALL list %vx to CALL list %rx for saving values.
(defun intern-eql-specializer (x)
  (labels (
    (intern-aux ()
      (values (foo x)) )
    )
    (let ((var *eql-specializers-latch*))
      (lock-latch var :exclusive)
      (unwind-protect (intern-aux)
        (unlock-latch var) ) ) ) )

;; Function type of (setf foo) should not be values.
(defun foo (x) (funcall #'(setf foo) x))

;;; Setf-function must take at least one parameter.
(defun (setf foo) () 1)

;;; Enforce function type of setf-function.
(defun (setf foo) (x) (declare (type list x)) 1)
(defun (setf foo) (x) (declare (type list x)) x)

;;; from parse-namestring
(defun foo (thing &optional host &key (start 0))
  (format t "; thing=~S host=~S start=~S~%" thing host start) )

;;; from load. exit bblock of finally has RET void
(defun ld (f) (with-open-file (s f) (read s)))

;;; unwind-protect
(defun foo ()
  (let ((abort t) (s (open-file)))
    (unwind-protect
        (progn (protected) (setq abort nil))
      (close/2 s abort) ) ) )

;; gcmap
(defun foo (x) (one) (let ((*print-length* x)) (two) (three) x))

;; gcmap
(defun foo (&optional
                (a (init1))
                (*print-length* (init2))
                (*print-level* (init3)) )
  (bar a) )

;;; (std-compute-effective-method make-emf-primary) in o04-emf.lisp
;;; all registers are blocked between values-list and apply.
(defun foo (fn)
  (lambda (&rest args) (let ((*next-methods* '())) (apply fn args))) )

;; elaborage_value_type
(defun foo (x)
  (labels ((my-error () (error "FOO")))
    (unless (car x) (my-error))
    (when   (cdr x) (my-error))
    x ) )


;; elaborage_value_type
(defun foo (x y)
    (declare (type fixnum x))
  (labels ((bar () (if y x 100)))
    (bar) ) )

;; keyboard-interrupt
(defun foo () (loop (bar)))
(defun bar () (baz))
(defun baz () (quux))
(defun quux () 1)

(defun foo () (loop for i from 0 do (format t "foo ~S~%" i)))

;; the and callee
(defun foo (x)
    (declare (values t t))
  (funcall (the (function () (values t t)) x)) )

;; the
(defun foo (x) (declare (values symbol)) x)

;; argument type error
(defun foo (x) (car (+ x 1)))

;; Make bar compatible with foo.
(defun foo () (declare (values nil)) (the nil (bar)))

;; Unrecognized keyword argument
(defun foo (x y) (member x y :foo 1))

;; Odd number of keyword arguments
(defun foo (x y) (member x y :test))


;; Keyword parameter type error
(defun foo (x y) (member x y :test 1))

;;; backtrace
(defun foo ()
  (unwind-protect
      (print (backtrace))
    (format t "cleanup~%") ) )

;; PHI %v
(defun foo (form)
  (let ((level *print-level*))
    (restart-case (eval form)
      (abort ()
        :report (format nil "Return to command-loop ~D." level)
        (format t "~&; Back to command-loop ~D.~%" level) )) ) )

(defun foo (form)
  (let ((level *print-level*))
    (restart-case (values (bar) (baz))
      (abort ()
        :report (format nil "Return to command-loop ~D." level)
        (format t "~&; Back to command-loop ~D.~%" level) )) ) )

;; PHI %v
(defun foo (form)
  (let ((*print-level* 10))
    (eval form) ) )

;; PHI %v: x64 requires two registers two restore value of *foo*.
(defun foo (form)
  (let ((*foo* 10)) (declare (special *foo*))
    (eval form) ) )

;; PHI %v
(defun foo (x)
  (block nil
    (let ((*print-level* nil))
      (if x (return (bar)) (return (baz))) )) )

;; frame and inline
(defun foo ()
  (labels ((bar () (unwind-protect (protected) (cleanup))))
    (let ((*print-level* nil)) (bar)) ) )

;; &optional
(defun dis (&optional (fspec (if (fspec-p *) (get-arg nil))))
  (when fspec (disassemble fspec)) )

;; closed-cell => closed-cell+stack-cell
(defun foo () (ignore-errors (bar)))

;; literal-cell => literal-cell+stack-cell
(defun foo (x)
  (tagbody
    (let ((*print-level* (lambda () (print x) (go exit))))
      (bar) )
    exit
      (print x) ) )

;; no values
(defun foo (x)
  (labels ((bar (x) (if x (values) 123))) (declare (notinline bar))
    (bar x) ) )

;; PHI+VALUES
(defun foo ()
  (multiple-value-bind (x y)
      (case (bar)
        ((1)'one)
        ((2) (values 'a 'b))
        (otherwise (baz)) )
    (if y 'ok x) ) )

(defun foo ()
  (multiple-value-bind (x y)
      (ecase (bar)
        ((1)'one)
        ((2) (values 'a 'b)) )
    (if y 'ok x) ) )

;; infinite loop
(defun foo () (let ((*print-length* nil)) (loop (bar))))
(defun foo () (let ((*print-length* nil)) (dotimes (i 10) (bar))))

(defun foo ()
  (let ((*print-length* nil)) (one))
  (let ((*print-length* t)) (tagbody loop (two) (go loop))) )

(defun foo () (let ((*print-length* nil)) (error "foo")))


(defun foo (x) (unless (bar)))
#|
BRANCH %b12 BB10 BB11
BB10: RET 'nil
BB11: RET 'nil
|#

(defun foo (x) (if (not (consp x)) 'not-cons 'cons))

;; ADD
(defun foo (a b) (+ a b))
(defun foo (a b) (the fixnum (+ a b)))
(defun foo (a b) (declare (type fixnum a b)) (the fixnum (+ a b)))

;; Type analysis
(defun foo (x)
  (labels ((even? (n) (if (= n 0) t   (odd? (1- n))))
           (odd?  (n) (if (= n 0) nil (even? (1- n)))) )
      (declare (notinline even? odd?))
    (odd? x) ) )

;; type back propagation
(defun middle (x) (nth (truncate (length x) 2) x))

;; style-warn: useless
(defun foo (y) (mapcar (lambda (x) `(foo ,x)) y))

;; void type
(defun foo () (labels ((bar () (print 'bar))) (bar) (bar) 3))

;; values type
(defun foo (x) (read-from-string x))
(defun foo (x) (declare (values t)) (read-from-string x))
(defun foo (x) (declare (values)) 1)        ; error
(defun foo (x) (declare (values nil)) 1)    ; error

;; function value is (values &optional (eql 1))
(defun foo (x) "conditional read" (if x 1 (values)))

;; function value is (values &optional null)
(defun foo (x) "conditional read" (if x nil (values)))




(defun foo (a)
  (multiple-value-bind (x y) (decode-universal-time a) (list x y)) )

(defun foo () (declare (values symbol symbol)) (values 'x 'y))
(defun foo () (declare (values array-rank)) 1)
(defun foo () (the fixnum 1))
(defun foo () (declare (values fixnum)) 1)
(defun foo () (labels ((bar (x) (declare (type fixnum x)) x)) (bar 1)))
(defun foo () (declare (values &rest t)) 1)

;; parameter check
(defun foo (&optional (x nil x-p))
    "x-p can't be nil or t."
    (declare (type fixnum x-p)) (list x x-p))

(defun foo (x) (declare (type fixnum x)) x)
(defun foo (&optional (x 0)) (declare (type fixnum x)) x)
(defun foo (&optional (x (init))) (declare (type fixnum x)) x)
(defun foo (x y z) (declare (type fixnum x y z)) (if (< x y) z))

(defun foo (x) (declare (type (or environment null) x)) x)
(defun foo (x) (declare (type (or environment null fixnum) x)) x)
(defun foo (x) (declare (type (or function symbol) x)) x)
(defun foo (x) (declare (type class x)) x)

;; (setf find-type)
(defun foo (expander name &optional errorp env)
    (declare (ignore errorp))
    (declare (type symbol name))
    (declare (type (or environment null) env))
  (list expander name env) )


(defun foo () (multiple-value-list (bar)))

;;; 12.2.16 max, min
(defun foo (x y z) (max x y z))

(defun foo () (declare (values fixnum))
  (multiple-value-call #'+/2 (the (values fixnum fixnum) (bar))) )

;;; 1.2.62 logand
(defun foo (x) (logand x 15))
(defun foo (x) (declare (type fixnum x)) (logand x 15))
(defun foo (x) (declare (type fixnum x)) (logior x 15))
(defun foo (x) (declare (type fixnum x)) (logxor x 15))
(defun foo (x y) (declare (type fixnum x y)) (logxor x y))

;;; argument type propagation
(defun foo ()
  (labels ((bar (x) (length x)))
    (list (bar "foo") (bar "baz")) ) )

;;; ANSI Common Lisp. Paul Graham. p. 249
(defun var? (x) (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))

;;; double-float
(defun foo (x) (declare (type double-float x)) (+ x 1d0))

(defun foo (x)
    (declare (type double-float x))
  (setq x (+ x 1d0))
  (format t "foo=~S~%" x)
  x )


(defun foo (x)
    (declare (type double-float x))
  (let ((y (+ x 1d0 x)))(format t "foo~%") y) )


(defun foo (n)
  (let ((x 0d0))
        (declare (type double-float x))
    (dotimes (i n x) (setq x (+ x 1d0))) ) )


;;; single-float
(defun foo (x) (declare (type single-float x)) (+ x 1f0))

;; This doesn't make spill. :-<
(defun foo (a b c d e f g h i)
    (declare (type double-float a b c d e f g h i))
  (+ a b c d e f g h i) )

;; opt-Box
(defun foo ()
  (let ((x 0f0)) (declare (type single-float x))
    (dotimes (i 10 x) (incf x 1f0)) ) )


(defun foo (x)
    (declare (type single-float x))
  (setq x (+ x 1f0))
  (format t "foo=~S~%" x)
  x )


;;; 12.2.24 *
(defun foo (x) (declare (type single-float x)) (* x 2f0))
(defun foo (x) (declare (type double-float x)) (* x 2d0))

(defun foo (x) (declare (type single-float x)) (> x 2f0))
(defun foo (x) (declare (type single-float x)) (>= x 2f0))

;;; bound
(defun foo (x) (loop for ch across x do (print x)))

;; <=
(defun foo (x) (<= 0 x 10))
(defun foo (x) (when (<= 0 x 10) (print 'ok)))
(defun foo (s e) (<= 0 s e 10))

;; svref
(defun foo (v i) (svref v i))
(defun foo (v i x) (setf (svref v i) x))

;; x64: 123 is imm32.
(defun foo (v i) (setf (svref v i) 123))

(defun foo (i) (svref #(zero one two) i))
(defun foo (i x) (setf (svref #(zero one two) i) x))

;; a=rax, b=rdx, c=rbx, d=rsi, e=rdi, f=r8, g=r9, h=r10, i=r11, j=r14 k=r15
(defun foo (a b c d e f g h i j k l)
    (values a b c d e f g (svref a g) h i j k l) )


;; cons type
(defun foo (x) (typep x '(cons fixnum null)))

;;;; 49 Internals

(defun foo (a i) (!elt 'sequence-index a i))
(defun foo (a i) (!elt 'sequence-index (the array a) i))
(defun foo (a i n) (setf (!elt 'sequence-index (the array a) i) n))

(defun foo (v i) (!elt 'float64 v i))
(defun foo (v i x) (setf (!elt 'float64 v i) x))

(defun foo (v i) (!elt 'float32 v i))
(defun foo (v i x) (setf (!elt 'float32 v i) x))

(defun foo (x y) (declare (double-float x y)) (+ x y))

(defun foo (v i)   (!elt 'int8 v i))
(defun foo (v i a) (setf (!elt 'int8 v i) a))
(defun foo (v i)   (setf (!elt 'int8 v i) 123))

(defun foo (v i)   (!elt 'int16 v i))
(defun foo (v i a) (setf (!elt 'int16 v i) a))
(defun foo (v i)   (setf (!elt 'int16 v i) 123))

(defun foo (v i)   (!elt 'int32 (the bignum v) i))
(defun foo (v i a) (setf (!elt 'int32 v i) a))
(defun foo (v i)   (setf (!elt 'int32 v i) 123))

(defun foo (v i)   (!elt 'uint32 (the bignum v) i))
(defun foo (v i a) (setf (!elt 'uint32 v i) a))
(defun foo (v i)   (setf (!elt 'uint32 v i) 123))


(defun foo (v i)   (!elt 'int (the bignum v) i))
(defun foo (v i a) (setf (!elt 'int v i) a))
(defun foo (v i)   (setf (!elt 'int v i) 123))

(defun foo (v i)   (!elt 'uint (the bignum v) i))
(defun foo (v i a) (setf (!elt 'uint v i) a))
(defun foo (v i)   (setf (!elt 'uint v i) 123))

(defun foo (x y) (declare (type double-float x y)) (+ (* x y) 2d0))

(defun foo (x) (declare (type double-float x)) (float x))
(defun foo (x) (declare (type single-float x)) (float x 0d0))

(defun foo (x) (declare (type double-float x)) (coerce x 'single-float))

;; From c4-parser.doc, we don't need to set nil to x. Since reference of
;; x is only when x-p is true.
;; 2007-02-05: IDEA: transform x-p test to $rn >= 1. Then merge comparsions
;; of initform of x and when.
(defun foo (&optional (x nil x-p)) (when x-p (print x)))

;; From c4-parser.doc, we should report function f1 and f2 aren't used.
(defun foo () (labels ((f1 () (f2)) (f2 () (f1))) 1))

;; function-annotation-ref
(defun foo (fn ofs)
    (declare (type native-code-function fn))
  (!value (!+ (!int fn) (!elt 'int32 fn ofs))) )

(defun foo (fn ofs)
    (declare (type native-code-function fn))
  (!value
    (locally (declare (optimize (safety 0)))
      (the (signed-byte 32) (+ (!int fn) (!elt 'int32 fn ofs))) ) ) )

(defun foo (fn)
  (ref fundesc code-size (the fundesc (.unbox-int fn))) )

(defun foo (fn ofs)
    (declare (type native-code-function fn))
  (setf (!elt 'uint32 fn ofs) 0) )


;;; cg-UpVar: Mixed Path
;;; See d24-fasl.lisp
(defun foo (a)
  (let ((labels '()))
    (labels (
      (fasl-op-setlabel (x)
        (mark-object x) )
      (mark-object (x)
        (push (cons a x) labels) )
      (main ()
        (mark-object nil) )
    )
    (values (main) #'fasl-op-setlabel) ) ) )

;;; FIXME: 2007-02-25: How do I type check foreign-object? We don't want to
;;; use safety=0 to do this. Alternative is !cast function.
(defun foo (desc cb)
    (declare (optimize (safety 0)))
  (setf (ref fundesc code-size desc) cb) )

(defun foo (desc idx list) (setf (!elt 'uint32 desc idx) (length list)))

(defun poke-i16 (fn idx i16) (declare (native-code-function fn)) (setf (!elt 'int16 fn idx) i16))

(defun poke-u16 (fn idx u16) (declare (native-code-function fn)) (setf (!elt 'uint16 fn idx) u16))


(defun poke-i32 (fn idx i32) (declare (native-code-function fn)) (setf (!elt 'int32 fn idx) i32))

(defun poke-u32 (fn idx u32) (declare (native-code-function fn)) (setf (!elt 'uint32 fn idx) u32))


(defun poke-i64 (fn idx i64) (declare (native-code-function fn)) (setf (!elt 'int64 fn idx) i64))

(defun poke-u64 (fn idx u64) (declare (native-code-function fn)) (setf (!elt 'uint64 fn idx) u64))


; Style-warn: ../../arch/generic/lisp/runtime/gen-r49-cell.lisp(53): Use extra value of function (SETF GETHASH/EQ).

(defun foo (x)
  (declare (ftype (function (t) t) not-exist)) (not-exist x))

(defun foo (c) (ref single-float-complex realpart c))
(defun (setf foo) (y c) (setf (ref single-float-complex realpart c) y))


;;; float contagion
(defun foo (x) (declare (type single-float x)) (+ x 10))
(defun foo (x) (declare (fixnum x)) (float x))

;;; negative case
(defun foo (x) (declare (type symbol x)) (svref x 0))
(defun foo (v x) (declare (type simple-vector v) (type symbol x)) (svref v x))

;;; positive case
(defun foo (x) (declare (type sequence-index x)) (declare (values (or sequence-index null))) x)

(defun foo (x) (declare (fixnum x)) (+ x 1d0))

(defun foo (x)
    (declare (type (integer #x-80000000 #x7fffffff) x))
  (logand #xFFFFFFFF (+ x 10)) )

(defun foo (x)
  (let ((ix (decode-float32 x)))
    (values (ldb (byte 23 0) ix)
            (ldb (byte 8 23) ix)
            (ldb (byte 1 31) ix) ) ) )

(defun foo (x)
  (let ((ix (decode-float32 x)))
    (values (logand (ash ix 0)   #x007FFFFF)
            (logand (ash ix -23) #xFF)
            (logand (ash ix 30)  #x1) ) ) )

;;; 32bit int
(defun foo () (loop repeat #.(ash 1 29) do (print 'foo)))

(defun foo (x y) (declare (type (signed-byte 29) x y)) (+ x y))
(defun foo (x y) (declare (type (signed-byte 30) x y)) (+ x y))
(defun foo (x y) (declare (type fixnum x y)) (+ x y))
(defun foo (x y) (declare (type (signed-byte 32) x y)) (+ x y))

(defun foo (x) (declare (type fixnum x)) (ash x -10))

;;; ENCODE instruction
(defun foo (x) (declare (single-float x)) (+ x 10))
(defun foo (x) (declare (double-float x)) (+ x 10))

;;; NEG instruction
(defun foo (x) (declare (single-float x)) (- x))
(defun foo (x) (declare (double-float x)) (- x))

;;; Type inference
(defun foo (x) (car (reverse x)))

;;; ref and type-error
(defun foo (x) (declare (type value-cell x)) (ref plist-cell plist x))
(defun foo (x) (setf (ref plist-cell plist (intern-value-cell x)) 1))

(defun foo (x)
  (let ((cell (intern-value-cell x)))
    (setf (ref plist-cell plist cell) 1) ) )

(defun (setf cl:symbol-plist) (plist symbol)
    (declare (type symbol symbol))
    (declare (values t))
  (let ((cell (list symbol) #+nil (intern-plist-cell symbol)))
    (setf (ref value-cell value cell) plist) ) )

;; STORE %r1, #'bar in CATCH frame ... #'bar MUST not closure.
(defun foo (x)
  (labels ((bar () (catch 'foo (incf x))))
    (declare (notinline bar))
    (bar) ) )

(defun foo (x) (let ((y (.unbox-int x))) (bar) (.box-int y)))


(defun foo () (with-compilation-unit () (format t "This is foo~%")))


(defun foo () (labels ((bar () 1)) #'bar))
(defun foo (x) (labels ((bar () x)) #'bar))
(defun foo (x) (labels ((bar () x) (baz () (bar))) #'baz))

(defun foo (x y)
  (labels (
    (bar (a)
      (format t "; bar: a=~S y=~S~%" a y)
      (typecase a
        (cons   (mapcar #'bar a))
        (string (bar (intern a)) a)
        (otherwise (member a y)) ) )
    )
    (mapcar #'bar x) ) )

(defun foo (x)
  (labels ((bar () (baz)) (baz () (format t "; baz ~S~%" x)))
    (declare (notinline bar baz))
    (bar) #'bar ) )

(defun foo () (labels ((bar () 1)) (declare (inline bar)) (bar)))

(defun foo (x)
  (labels (
    (kar (x) (car x))
    (kdr (x) (cdr x))
    )
    (declare (notinline kar kdr))
  (values (kar x) (kdr x)) ) )


(defun foo (x)
  (lambda (a b &rest c)
    (format t "a=~S b=~S c=~S x=~S~%" a b c x)
    (incf x) ) )

(defun foo (x)
  (lambda (a b &optional (c 'c) (d 'd))
    (format t "a=~S b=~S c=~S d=~S x=~S~%" a b c d x)
    (incf x) ) )

(defun foo (x)
  (flet (
    (bar (a b &optional (c 'c) (d 'd))
      (format t "a=~S b=~S c=~S d=~S x=~S~%" a b c d x)
      (incf x) )
    )
    (bar 123 456)
    #'bar ) )
