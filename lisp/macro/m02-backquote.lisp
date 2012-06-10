;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 2 Syntax - Backquote
;;; lisp/macro/m02-backquote.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/macro/m02-backquote.lisp#4 $
;;;
;
(in-package :si)

;;; Backquote parseing
;;;   (parse (<non-splicing>* <splicing>* <element>*))
;;;   =>
;;;     (list* <non-splicing>* (append <splicing*> (parse <element>*)))
;;
;;; Code:
;;;     (append)                    append form
;;;     (eval . <form>)             argument
;;;     (funcall . [<vector>])      start of function form
;;;     (cons)                      cons form
;;;     (list)                      list form
;;;     (list*)                     list* form
;;;     (list-to-vector)            list-to-vector form
;;;     (quote . <datum>)           literal argument
;;;
(defun process-backquote (x)
  (let ((codes '()))
  (labels (

    ;; Assemble lisp form from backquote code.
    (assemble ()
      (labels (
        (need-quote-p (x)
          (or (symbolp x) (consp x)) )
        )
      (let ((ostack '())    ;; operator stack
            (astack '())    ;; arguments stack
            (args '()) )
        (dolist (code codes (car args))
          (case (car code)
            ((eval)
              (push (cdr code) args) )
            ((funcall)
              (let ((arg (cons (pop ostack) args)))
                (setq args (pop astack))
                (push arg args) ) )
            ((quote)
              (let ((datum (cdr code)))
                (when (need-quote-p datum)
                  (setq datum (list 'quote datum)) )
                (push datum args) ) )
            (otherwise
              (push (car code) ostack)
              (push args astack)
              (setq args nil) )) ) ) ) )

    (emit (opcode &optional operand)
      (case opcode
        ((append nconc)   ; (append x) => x
          (let ((last-1 (car codes))
                (last-2 (cadr codes)) )
            (if (eq (car last-2) 'funcall)
                (progn
                  (pop codes)
                  (setf (car last-2) (car last-1))
                  (setf (cdr last-2) (cdr last-1)) )
              (emit-aux opcode operand)) ) )
        ((cons list list*)   ; (list 'a 'b 'c) => (quote (a b . c))
          (let ((runner codes)
                (elts nil)
                (first (not (eq opcode 'list))) )
            (loop
              (let ((code (car runner)))
                (when (eq (car code) 'funcall)
                  (setf (car code) 'quote)
                  (setf (cdr code) elts)
                  (setq codes runner)
                  (return) )
                (unless (eq (car code) 'quote)
                  (emit-aux opcode operand)
                  (return) )
                (if first
                    (setq elts (cdr code) first nil)
                  (push (cdr code) elts))
                (setq runner (cdr runner)) )) ) )
        ((list-to-vector)   ; (list-to-vector 'a 'b 'c) => #(a b c)
          (let ((last (car codes)))
            (if (eq (car last) 'quote)
                (progn
                  (pop codes)
                  (setf (car (car codes)) 'quote) )
              (emit-aux opcode operand) ) ) )
        (otherwise
          (emit-aux opcode operand) )) )

    (emit-aux (opcode operand)
      (let ((code (cons opcode operand)))
         (push code codes) ) )

    (parse (x level)
        (declare (type sequence-index level))
      (cond
        ((eql level 0)
          (emit 'eval x) )
        ((simple-vector-p x)
          (parse-vector x level) )
        ((atom x)
          (emit 'quote x) )
        ((quote-p x 'backquote)
          (emit 'funcall)
          (emit 'quote 'backquote)
          (parse (cadr x) (1+ level))
          (emit 'list) )
        ((quote-p x 'unquote)
          (if (eql level 1)
              (emit 'eval (cadr x))
            (progn
              (emit 'funcall)
              (emit 'quote 'unquote)
              (parse (cadr x) (1- level))
              (emit 'list) )) )
        ((splicing-p x)
          (if (eql level 1)
              (error ",~C~S after `" (unparse x) (cadr x))
            (progn
              (emit 'funcall)
              (emit 'quote (car x))
              (parse (cadr x) (1- level))
              (emit 'list) )) )
        (t
          (parse-list x level) )) )

    (parse-list (x level)
        (declare (type sequence-index level))
      (let ((nelts 0)
            (splicing nil)
            (ostack '()) )
        (loop
          (cond
            ((null x)
              (return) )
            ((or (atom x) (quote-p x 'unquote) (quote-p x 'backquote))
              (when (eq (car ostack) 'list)
                (setf (car ostack) (if (eql nelts 1) 'cons 'list*)) )
              (parse x level)
              (return) )
            ((splicing-p x)
              (error "Dotted ,~C~S" (unparse x) (cdr x)) )
            (t
              (let ((elt (pop x)))
                (cond
                  ((and (eql level 1) (splicing-p elt))
                    (unless (eql nelts 0)
                      (setf (car ostack) (if (eql nelts 1) 'cons 'list*))
                      (setq nelts 0) )

                    (unless (eq splicing (car elt))
                      (setq splicing (car elt))
                      (push (if (eq splicing 'unquote-splicing)
                                'append
                              'nconc )
                            ostack )
                      (emit 'funcall) )

                    (emit 'eval (cadr elt)) )
                 (t
                   (setq splicing nil)
                   (when (eql nelts 0)
                     (push 'list ostack)
                     (emit 'funcall) )
                   (parse elt level)
                   (incf nelts) )) ) )))
        (dolist (op ostack)
          (emit op) ) ) )

    (parse-vector (vector level)
        (declare (type simple-vector vector))
        (declare (type sequence-index level))
      (let* ((anchor (list 0))
             (tail anchor) )
        (dotimes (i (length vector))
          (setq tail (setf (cdr tail) (list (svref vector i)))) )
        (emit 'funcall vector)
        (parse-list (cdr anchor) level)
        (emit 'list-to-vector) ) )

    (quote-p (x op)
      (when (and (consp x) (eq (car x) op))
        (unless (and (consp (cdr x))  (null (cddr x)))
          (error "Malformed ~S" x) )
        x ) )

    (splicing-p (x)
      (or (quote-p x 'unquote-splicing) (quote-p x 'unquote-nsplicing)) )

    (unparse (x)
      (if (eq (car x) 'unquote-splicing) #\@ #\,) )
    )
    ;;
    (parse x 1)
    (assemble) ) ) )
#|
;;;;  BACKQUOTE macro.
;;;
;;;
;
(defmacro backquote (templ)
  (process-backquote (cadr templ)) )
|#

#||
(define t1 '`(list ,(+ 1 2) 4))
; => (list 3 4)

(progn
  (setq name 'a)
  (setq t2 '`(list ,name ',name)) )
; => `(list a (unquote a))

`(a ,(+ 1 2) ,@(mapcar #'abs '(4 -5 6)) b)
 => (a 3 4 5 6 b)

`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
; => ((foo 7) . cons)

`#(10 5 ,(sqrt 4) ,@(mapcar #'sqrt '(16 9)) 8))
; => #(10 5 2 4 3 8)

`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
; => (a `(b ,(+ 1 2) ,(foo 4 d) e) f)

(let ((name1 'x)
      (name2 'y) )
  `(a `(b ,,name1 ,',name2 d) e) )
; => (a `(b ,x ,'y d) e)
||#


#|
(setf (macro-function 'backquote)
  (lambda (form env)
      (declare (lambda-name (macro-function backquote)))
      (declare (ignore env))
    (bq-parse-all (second form)) ) )

;; from with-collect
(defun foo (name decl*-form*)
  `(let* ((.anchor (list 0))
           (.last   .anchor) )
      (macrolet (
          (,name (x) `(setq .last (setf (cdr .last) (list ,x))))
          (collection () '(cdr .anchor)) )
        ,@decl*-form* (collection) ) ) )

(foo 'name '((collect 1)))

;; defun
(defun foo (name lambda-list body)
  `(progn
     (eval-when (:compile-toplevel)
       (%defun ',name ',lambda-list) )
     (si::%defun ',name ',lambda-list
       (labels ((,name ,lambda-list ,@body)) #',name) )) )

;; setf
(defun foo (vars vals form stores writer-form)
 `(let (,.(mapcar #'list vars vals)
        (,(first stores) ,form) )
    ,writer-form ) )


`(let ,(mapcar #'list vars vals)
    (multiple-value-bind ,stores
        ,form
      ,writer-form ) )

;; define-setf-expander

     m1  `#'(lambda (,var-form ,(or var-env temp-env))
               (declare (ext:lambda-name (:setf-expander ,access-fn)))
           ,.(unless var-env `((declare (ignore ,temp-env))))
           ,.(when doc-string (list doc-string))
           (labels (
             (,fn-syntax-error (cur src pat)
               (syntax-error '(,access-fn ,lambda-list)
                             ,var-form
                             cur
                             src
                             pat ) )
             )
             (block ,access-fn ,program) ) ))

|#
