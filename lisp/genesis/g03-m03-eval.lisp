(in-package :si)


(macrolet (
  (define (name when ret final)
   `(defun ,name (predicate list)
      (dolist (elt list ,final)
        (let ((value (funcall predicate elt)))
          (,when value (return ,ret)) ) ) ) )
  )
  ;;
  (define cl:every    unless nil   t)
  (define cl:notany   when   nil   t)
  (define cl:notevery when   nil   t)
  (define cl:some     when   value nil) )


(setf (macro-function 'defmacro) #'(lambda (form env)
    (declare (ext:lambda-name (macro-function defmacro)))
    (declare (ignore env))
  (let ((name        (cadr  form))
        (lambda-list (caddr form))
        (body        (cdddr form)) )
  (multiple-value-bind (expander doc-string)
      (c::parse-macro-aux name lambda-list body)
   `(progn
      (eval-when (:compile-toplevel)
        (c::%defmacro ',name ',lambda-list ,expander) )
      (si::%defmacro ',name ',lambda-list ,expander ,doc-string) ) ) ) ))

(defun (setf documentation) (doc-string name &optional (kind 'function))
    (declare (ignore name kind))
  doc-string )


(defun si::%defmacro (name lambda-list expander doc-string)
    (declare (ignore lambda-list))
  (setf (macro-function name) expander)
  (setf (documentation name 'function) (or doc-string ""))
  name )



(defun cl:- (z &rest z*)
    (declare (dynamic-extent z*))
  (if (null z*)
      (-/1 z)
    (dolist (z2 z* z)
      (setq z (-/2 z z2)) ) ) )

(defun cl:/ (z &rest z*)
    (declare (dynamic-extent z*))
  (if (null z*)
      (//2 1 z)
    (dolist (z2 z* z)
      (setq z (//2 z z2)) ) ) )

(defun cl:+ (&rest z*)
    (declare (dynamic-extent z*))
  (let ((z 0))
    (dolist (z2 z* z)
      (setq z (+/2 z z2)) ) ) )

(defun cl:* (&rest z*)
    (declare (dynamic-extent z*))
  (let ((z 1))
    (dolist (z2 z* z)
      (setq z (*/2 z z2)) ) ) )

(defun cl:= (z1 &rest z*)
    (declare (dynamic-extent z*))
  (dolist (z2 z* t)
    (unless (=/2 z1 z2) (return nil)) ) )

(defun cl:/= (z1 &rest z*)
    (declare (dynamic-extent z*))
  (if (null z*)
      (progn (check-type z1 real) t)
    (loop
      (when (null z*) (return t))
      (dolist (z2 z*)
        (unless (/=/2 z1 z2) (return-from /= nil)) )
      (setq z1 (pop z*)) )) )

(macrolet (
  (define (name)
   `(defun ,name (real-1 &rest reals)
        (declare (dynamic-extent reals))
      (if (null reals)
          (progn (check-type real-1 real) t)
        (dolist (real-2 reals t)
          (unless (,(intern (format nil "~A/2" name)) real-1 real-2)
            (return nil) )
          (setq real-1 real-2) )) ) )
  )
  ;;
  (define cl:<)
  (define cl:<=)
  (define cl:>)
  (define cl:>=) )



(defun cl:length (x)
    (declare (values fixnum))
  (cond
    ((listp x)
      (let ((n 0)) (dolist (elt x n) (setq n (1+ n)))) )
    ((vectorp x)
      (ref data-vector length x) )
    (t
      (error 'type-error :datum x :expected-type 'cl:sequence) )) )


;; This is simple version of multiple-value-setq. When var is symbol-macro,
;; expansion doesn't confirm argument evaluation rule.
(defmacro cl:multiple-value-setq ((&rest var*) form)
  (let ((bind (list 0))
        (setq (list 'setq)) )
    (let ((bind-last bind)
          (setq-last setq) )
      (dolist (var var*)
        (let ((tmp (make-symbol (symbol-name var))))
          (setq bind-last (setf (cdr bind-last) (list tmp)))
          (setq setq-last (cdr (setf (cdr setq-last) (list var tmp)))) ) )
      (setq bind (cdr bind))
      `(multiple-value-bind ,bind ,form ,setq ,(first bind)) ) ) )


;; loading m05-control.lisp requires
;;  get-setf-expansion <= psetf <= psetq <= do

;; (psetq x 1 y 2) => (let ((#:x 1) (#:y 2)) (setq x #:x y #:y) nil)
(defmacro cl:psetq (&rest var-form*)
  (let* ((bind* '())
         (setq* (list 'setq))
         (setq*-last setq*) )
    (loop
      (when (null var-form*)
        (setq bind* (nreverse bind*))
        (return `(let ,bind* ,setq* nil)) )
      (let* ((var  (pop var-form*))
             (form (pop var-form*))
             (tmp (make-symbol (symbol-name var))) )
        (push `(,tmp ,form) bind*)
        (setq setq*-last
          (cdr (setf (cdr setq*-last) (list var tmp))) ) )) ) )

;;; assq
;;; for get-setf-expansion
(defun assq (key alist)
    (declare (type list alist))
    (declare (values list))
  (dolist (key.val alist nil)
    (when (eq (car key.val) key) (return key.val)) ) )


;;; fifth
; parse-destructuring-bind
(defun cl:fifth (x) (first (cddddr x)))

;;; function-name-p
(defun function-name-p (x)
  (or (symbolp x)
      (and (consp x) (eq (car x) 'setf)
           (consp (cdr x)) (symbolp (second x))
           (null (cddr x)) )) )

;;; function-information
(defun c::function-information (fname &optional env)
    (declare (type symbol name))
    (declare (values symbol symbol list))
  (let ((env  (or env *environment*))
        (name
          (cond
            ((symbolp fname) fname)
            ((function-name-p fname) (intern-setf-cell (second fname)))
            (t (error 'type-error :datum fname
                      :expected-type 'ext:function-name ) )) ))
    (loop
      (let ((info  (gethash/eq name (ref environment functions env)))
            (outer (ref environment outer env)) )
        (when info
          (return (values (car info)
                          (null (ref environment types env))
                          (cdr info) )))
        (setq env outer)
        (when (null env) (return (values nil nil nil))) )) ) )


;;; variable-information
(defun c::variable-information (name &optional env)
    (declare (type symbol name))
    (declare (values symbol symbol list))
  (let ((env (or env *environment*)))
    (loop
      (let ((info  (gethash/eq name (ref environment variables env)))
            (outer (ref environment outer env)) )
        (when info
          (return (values (car info)
                          (null (ref environment types env))
                          (cdr info) )))
        (setq env outer)
        (when (null env) (return (values nil nil nil))) )) ) )

;;;; macroexpand-hook
;
(defun c6::macroexpand-hook (expander form env)
    (funcall expander form env) )

(setq *macroexpand-hook* #'c6::macroexpand-hook)

(defmacro ext:deftlv (name &optional (initform nil initform-p) doc-string)
  `(progn
     (eval-when (:compile-toplevel)
       (c::%deftlv ',name ',initform) )
     ,(if (not initform-p)
          `(si::%deftlv ',name nil ,doc-string nil)
        `(if (boundp ',name)
           (si::%deftlv ',name nil ,doc-string nil)
         (si::%deftlv ',name ,initform ,doc-string t) ))) )


(defun mapcar/1 (fn list)
  (let* ((result (list 0))
         (last   result) )
    (dolist (x list (cdr result))
      (setq last (setf (cdr last) (list (funcall fn x)))) ) ) )


(defun mapcar/2 (fn list1 list2)
  (let* ((result (list 0))
         (last   result) )
    (loop
      (when (or (endp list1) (endp list2)) (return (cdr result)))
      (setq last (setf (cdr last)
        (list (funcall fn (pop list1) (pop list2))) ))) ) )

(defmacro with-collector ((name) &body decl*-form*)
 `(let* ((.anchor (list 0))
         (.last   .anchor) )
    (macrolet (
        (,name (x) `(setq .last (setf (cdr .last) (list ,x))))
        (collection () '(cdr .anchor)) )
      ,@decl*-form* (collection) ) ) )


(defun cl:mapcar (fn list1 &rest list*)
  (cond
    ((null list*) (mapcar/1 fn list1))
    ((null (rest list*)) (mapcar/2 fn list1 (first list*)))
    (t
      (let ((list+  (cons list1 list*)))
        (with-collector (collect)
          (block outer
            (loop
              (collect
                (apply fn
                  (with-collector (collect)
                    (let ((runner list+))
                      (loop
                        (when (endp runner) (return))
                        (let ((list (first runner)))
                          (when (null list) (return-from outer))
                          (collect (first list))
                          (setf (first runner) (rest list)) )
                        (pop runner) ) ) ))))) ) ) )) )


(defun (setf cl:compiler-macro-function) (expander fname &optional env)
  (let* ((name
             (cond
               ((symbolp fname) fname)
               ((function-name-p fname) (intern-setf-cell (second fname)))
               (t (error 'type-error fname
                     :expected-type 'ext:function-name ))) )
         (env (or env *environment*))
         (htb (ref environment functions env)) )
    (let ((frob (gethash/eq name htb)))
      (when (null frob)
        (setq frob (list nil))
        (setf (gethash/eq name htb) frob) )
      (let ((key.val (assq :compiler-macro (rest frob))))
        (if (null key.val)
              (setf (rest frob)
                (cons (cons :compiler-macro expander) (rest frob)) )
          (setf (cdr key.val) expander) ) ) )
    expander ) )


(defun (setf cl:documentation) (doc-string name &optional kind)
    (declare (ignore name kind))
  doc-string )


(defun %define-compiler-macro (name ll expander doc-string)
    (declare (ignore ll))
  (setf (compiler-macro-function name) expander)
  (setf (documentation name 'compiler-macro) (or doc-string ""))
  name )

