;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 14 Conses
;;; lisp/runtime/r14-cons.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r14-cons.lisp#6 $
;;;
;;; See Also: r14-set.lisp
;;;
;;; Description:
;;;  This fils contains following functions:
;;;     append/2          14.2.26 genesis
;;;     assq                14.2.36 genesis
;;;     assl                14.2.36
;;;     mapcar/1            14.2.34
;;;     mapcar/2            14.2.34
;;;     mapcan/2            14.2.34
;;;     meml                14.2.33
;;;     memq                14.2.33 genesis
;;;     nconc/2           14.2.25 genesis
;;;     plist-put           14.2.41 (setf getf)
;;;     plist-rem           14.2.42 remf
;;;     proper-list-p       internal
;;;     safe-list-length    internal
;;;
;;;     acons               14.2.35
;;;     append              14.2.26
;;;     assoc               14.2.36
;;;     assoc-if            14.2.36
;;;     assoc-if-not        14.2.36 deprecated
;;;     atom                14.2.7  genesis
;;;     butlast             14.2.28
;;;     car                 14.2.9  genesis
;;;     caar                14.2.9  genesis
;;;     caaar               14.2.9  genesis
;;;     caaaar              14.2.9  genesis
;;;     ...
;;;     cddddr              14.2.9  genesis
;;;     consp               14.2.6 boot
;;;     copy-alist          14.2.37
;;;     copy-list           14.2.14 genesis
;;;     copy-tree           14.2.10
;;;     eighth              14.2.21
;;;     endp                14.2.23 boot
;;;     fifth               14.2.21
;;;     firth               14.2.21 boot
;;;     fourth              14.2.21
;;;     get-properties      14.2.40
;;;     getf                14.2.41
;;;     last                14.2.29 genesis
;;;     ldiff               14.2.30
;;;     list                14.2.15 boot
;;;     list*               14.2.15 boot
;;;     list-length         14.2.16
;;;     listp               14.2.17 boot
;;;     make-list           14.2.18
;;;     mapc                14.2.34
;;;     mapcan              14.2.34
;;;     mapcar              14.2.34
;;;     mapcon              14.2.34
;;;     mapl                14.2.34
;;;     maplist             14.2.34
;;;     nbutlast            14.2.28
;;;     nconc               14.2.25 genesis
;;;     ninth               14.2.21
;;;     nreconc             14.2.27 (setf values)
;;;     nsublis             14.2.11
;;;     nsubst              14.2.12
;;;     nsubst-if           14.2.12
;;;     nth                 14.2.22
;;;     nthcdr              14.2.31
;;;     null                14.2.24 boot & compiler
;;;     pairlis             14.2.38
;;;     rassoc              14.2.39
;;;     rassoc-if           14.2.39
;;;     revappend           14.2.27
;;;     rplaca              14.2.8
;;;     rplacd              14.2.8
;;;     seventh             14.2.21
;;;     sixth               14.2.21
;;;     subst               14.2.12 (setf the)
;;;     subst-if            14.2.12
;;;     sublis              14.2.11
;;;     tailp               14.2.30
;;;     tenth               14.2.21
;;;     third               14.2.21
;;;     tree-equal          14.2.13
;
(in-package :si)

;(declaim (optimize (speed 3) (safety 1) (space 0) (debug 0)))

;;;; append/2
;
(defun append/2 (list-1 list-2)
    (declare (type list list-1))
    (declare (type t list-2))
    (declare (values t))
  (if (null list-1)
      list-2
    (let* ((head (list (first list-1)))
           (tail head) )
      (dolist (elt (rest list-1))
        (setq tail (setf (rest tail) (list elt))) )
      (setf (rest tail) list-2)
      head )) )


;;;; assl
;;;
;;; Description:
;;;  (assoc item alist :test #'eql)
;
(defun assl (item alist)
  (dolist (key.val alist)
    (when (eql (car key.val) item) (return key.val)) ) )


;;;; assq
;;;
;;; Description:
;;;  (assoc item alist :test #'eq)
;
(defun assq (item alist)
  (dolist (key.val alist)
    (when (eq (car key.val) item) (return key.val)) ) )


;;;; delq
;
(defun delq (item list &optional n)
    (declare (type list list))
    (declare (type (or fixnum null) n))
    (declare (values list))
  (let ((n (or n most-positive-fixnum)))
      (declare (type fixnum n))
    (loop
      (when (or (zerop n) (endp list)) (return-from delq list))
      (unless (eq item (car list)) (return))
      (setq list (cdr list))
      (decf n) )

    (let ((runner list)
          (prev list) )
      (loop
        (when (endp runner)
          (return-from delq list) )

        (let ((next (cdr runner)))
          (if (not (eq item (car runner)))
              (setq prev runner)
            (progn
              (setf (cdr prev) next)
              (decf n)
              (when (zerop n) (return-from delq list)) ))
          (setq runner next) )) ) ) )


;;;; ensure-test-function
;
(defun ensure-test-function (test test-not)
    (declare (values (or function symbol)))
  (cond
    ((and test test-not)
      (error "Can't specifiy both ~S and ~S." :test :test-not) )
    (test
      test )
    (test-not
      (complement test-not) )
    (t
      #'eql )) )


;;;; meml
;;;
;;; Description:
;;;  (member item alist :test #'eql)
;
(defun meml (item list)
    (declare (type list list))
    (declare (values list))
  (loop
    (when (endp list) (return list))
    (when (eql item (car list)) (return list))
    (setq list (cdr list)) ) )


;;;; memq
;;;
;;; Description:
;;;  (member item alist :test #'eq)
;
(defun memq (item list)
    (declare (type list list))
    (declare (values list))
  (loop
    (when (endp list) (return list))
    (when (eq item (car list)) (return list))
    (setq list (cdr list)) ) )


;;;; nconc/2
;;;
;;; Note: nconc/nconc/2 = 1.77
;
(defun nconc/2 (list-1 list-2)
  (if (endp list-1)
      list-2
    (let ((scan-1 list-1))
      (loop
        (let ((next-1 (rest scan-1)))
          (when (null next-1)
            (setf (rest scan-1) list-2)
            (return list-1) )
          (setq scan-1 next-1) )) )) )


;;;; plist-put
;;;
;;; Called by:
;;;     (setf getf)
;
(defun plist-put (plist indicator value)
  (do ((scan plist (cdr scan)))
      ((endp scan) (list* indicator value plist))
    (when (eq indicator (pop scan))
      (setf (car scan) value)
      (return plist) ) ) )


;;;; plist-rem
;;;
;;; Returns:
;;;     plist
;;;     removed-p
;;;
;;; Called by:
;;;     remf
;
(defun plist-rem (plist indicator)
    (declare (type list plist))
    (declare (values list t))
  (let ((scan plist)
        (prev nil) )
    (loop
      (when (endp scan) (return (values plist nil)))
      (let ((indicator-1 (pop scan))
            (next-1      (cdr scan)) )
        (when (eq indicator indicator-1)
          (if prev
              (setf (cdr prev) next-1)
            (setq plist next-1) )
          (return (values plist t)) )
        (setq prev scan)
        (setq scan next-1) )) ) )


;;;; proper-list-p
;;;
;;; Description:
;;;  Returns length of list when specified list is proper-list otherwise
;;;  returns false. Circulated list and dotted-list are considered as
;;;  non-proper-list.
;
(defun proper-list-p (list)
    (declare (values (or sequence-index null)))
  (cond
    ((null list) 0)
    ((not (consp list)) nil)
    (t
      (let ((length 0)
            (fast   list)
            (slow   list) )
         (declare (type sequence-index length))
       (loop
         (when (null fast) (return length))
         (unless (consp fast) (return nil))
         (incf length)
         (setq fast (cdr fast))

         (when (null fast) (return length))
         (unless (consp fast) (return nil))
         (incf length)
         (setq fast (cdr fast))

         (setq slow (cdr slow))
         (when (eq fast slow) (return nil)) ) ) )) )


;;;; Safe List length
;;;
;;; Returns:
;;;   length of list or (- n) when not propert-list.
;
(defun safe-list-length (list)
    (declare (values fixnum))
    (declare (optimize (speed 3) (safety 0)))
  (let ((n 0) (fast list) (slow list))
      (declare (type sequence-index n))
    (loop
      (unless (consp fast) (return))
      (setq fast (cdr fast))
      (setq n (1+ n))
      (unless (consp fast) (return))
      (setq fast (cdr fast))
      (setq n (1+ n))

      (setq slow (cdr slow))
      (when (eq fast slow) (return)) )

   (if (null fast) n (- -1 n)) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;

;;;; 14.2.35 acons
(defun cl:acons (key datum alist)
  (cons (cons key datum) alist) )


;;;; 14.2.36 assoc
(defun cl:assoc (item alist &key key test test-not)
  (setq test (ensure-test-function test test-not))
  (dolist (key.val alist)
    (when (and key.val (funcall test item (funcall/key key (car key.val))))
      (return key.val) ) ) )


;;;; 14.2.36 assoc-if
(defun cl:assoc-if (predicate alist &key key)
  (dolist (key.val alist)
    (when (and key.val (funcall predicate (funcall/key key (car key.val))))
      (return key.val) ) ) )


;;;; 14.2.26 append
(defun cl:append (&rest lists)
  (declare (dynamic-extent lists))
  (do* ((result (list 1))
        (last   result)
        (scan   lists (cdr scan)) )
       ((null (cdr scan))
        (setf (cdr last) (car scan))
        (cdr result) )
    (let ((list (car scan)))
      (declare (type list list))
      (dolist (elt list)
        (let ((cons (cons elt nil)))
          (setf (cdr last) cons) 
          (setq last cons) ) ) ) ) )


;;;; 14.2.7 atom
(defun cl:atom (object)
  (not (consp object)) )


;;;; 14.2.28 butlast
(defun cl:butlast (list &optional (n 1))
    (check-type list list)
    (check-type n (integer 0 *))
  (let ((result '()))
    (loop
      (when (atom list) (return))
      (push (pop list) result) )
    (loop repeat n do
      (pop result) )
    (nreverse result) ) )


;;;; 14.2.9 car, cdr, ..., cddddr
(macrolet (
  (define ()
    (labels (
      (make (n)
        (loop
          for k below (ash 1 n)
          for name = (make-name k n)
          for reader = (make-form k n)
            collect `(defun ,name (x) ,reader)
            collect `(defun (setf ,name) (y x) (setf ,reader y)) ) )

      (make-form (k n)
        (loop
          with form = 'x
          repeat n do
            (if (evenp k)
                (setq form `(car ,form))
            (setq form `(cdr ,form)) )
            (setq k (ash k -1))
        finally (return form) ) )

      ;; make-name
      (make-name (k n)
        (loop
          with name = (make-string (+ n 2))
          for index = n then (1- index)
          repeat n do
            (setf (schar name index) (if (evenp k) #\A #\D))
            (setq k (ash k -1))
          finally
            (setf (schar name 0) #\C)
            (setf (schar name (+ n 1)) #\R)
            (return (intern name)) ) )
    )
    `(progn
        ,.(make 2)
        ,.(make 3)
        ,.(make 4) ) ) )
    )
    (define) )


;;;; 14.2.21 first, second, third, fourth, fifth, sixth, seventh, eighth,
;;; ninth, tenth
;
(defun cl:second  (x) (cadr x))
(defun cl:third   (x) (caddr x))
(defun cl:fourth  (x) (cadddr x))
(defun cl:fifth   (x) (car    (cddddr x)))
(defun cl:sixth   (x) (cadr   (cddddr x)))
(defun cl:seventh (x) (caddr  (cddddr x)))
(defun cl:eighth  (x) (cadddr (cddddr x)))
(defun cl:ninth   (x) (car    (cddddr (cddddr x))))
(defun cl:tenth   (x) (cadr   (cddddr (cddddr x))))

(defun (setf cl:second)  (y x) (setf (cadr x) y))
(defun (setf cl:third)   (y x) (setf (caddr x) y))
(defun (setf cl:fourth)  (y x) (setf (cadddr x) y))
(defun (setf cl:fifth)   (y x) (setf (car    (cddddr x)) y))
(defun (setf cl:sixth)   (y x) (setf (cadr   (cddddr x)) y))
(defun (setf cl:seventh) (y x) (setf (caddr  (cddddr x)) y))
(defun (setf cl:eighth)  (y x) (setf (cadddr (cddddr x)) y))
(defun (setf cl:ninth)   (y x) (setf (car    (cddddr (cddddr x))) y))
(defun (setf cl:tenth)   (y x) (setf (cadr   (cddddr (cddddr x))) y))


;;;; 14.2.37 copy-alist
(defun cl:copy-alist (alist)
    (declare (values list))
  (let ((result '()))
    (dolist (key.datum alist)
      (push (if (consp key.datum)
                (cons (car key.datum) (cdr key.datum))
              key.datum )
            result ) )
    (nreverse result) ) )


;;;; 14.2.14 copy-list
(defun cl:copy-list (list)
    (declare (values list))
    (declare (type list list))
  (when list
    (let* ((head (list (first list)))
           (tail head)
           (runner list) )
      (loop
        (setq runner (rest runner))
        (unless (consp runner)
          (setf (rest tail) runner)
          (return head) )
        (setq tail (setf (rest tail) (list (first runner)))) ) )) )


;;;; 14.2.10 copy-tree
(defun cl:copy-tree (tree)
    (declare (values t))
  (if (not (consp tree))
    tree
    (let* ((head (list (copy-tree (first tree))))
           (tail head)
           (scan tree) )
      (loop
        (setq scan (rest scan))
        (unless (consp scan)
          (setf (rest tail) scan)
          (return head) )
        (setq tail (setf (rest tail) (list (copy-tree (first scan))))) ) )) )


;;;; 12.2.41 getf
;;;
;;; Note: (setf getf) is implemented as setf-expander.
;
(defun cl:getf (plist indicator &optional default)
  (loop
    (when (endp plist) (return default))
    (when (eq indicator (first plist)) (return (second plist)))
    (setq plist (cddr plist)) ) )


;;;; 12.2.40 get-properties
(defun cl:get-properties (plist indicator-list)
    (declare (type list plist))
    (declare (type list indicator-list))
    (declare (values t t list))
  (do ((scan plist (cddr scan)))
      ((endp scan) (values nil nil nil))
    (let ((indicator (car scan)))
      (when (member indicator indicator-list :test #'eq)
        (return (values indicator (cadr scan) scan)) ) ) ) )


;;;; 14.2.29 last
(defun cl:last (list &optional (n 1))
    (check-type list list)
    (check-type n (integer 0 *))
  (do ((runner list (cdr runner))
       (result list)
       (i 0 (1+ i)) )
      ((atom runner) result)
        (declare (type ext:sequence-index i))
    (when (>= i n) (pop result)) ) )


;;;; 14.2.30 ldiff
(defun ldiff (list object)
    (declare (values list))
    (check-type list list)
  (do ((list list (cdr list))
       (r '() (cons (car list) r)))
      ((atom list)
       (if (eql list object) (nreverse r) (nreconc r list)) )
    (when (eql object list)
      (return (nreverse r)) ) ) )


;;;; 14.2.16 list-length
(defun cl:list-length (list)
    (declare (type list list))
    (declare (values (or ext:sequence-index null)))
  (do ((len 0 (+ len 2))
       (fast list (cddr fast))
       (slow list (cdr slow)) )
      ((endp fast) len)
        (declare (type ext:sequence-index len))
    (when (endp (cdr fast))
      (return (1+ len)) )
    (when (and (eq fast slow) (> len 0))
      (return nil) ) ) )


;;;; 14.2.18 make-list
(defun cl:make-list (size &key initial-element)
    (declare (type ext:sequence-index size))
  (let ((result '()))
    (loop repeat size do
      (push initial-element result) )
    result ) )


;;;; 14.2.34 mapc, mapcar, mapcan, mapl, maplist, mapcon
;;; BUGBUG: REVIEW: Should we use sentinel and (setf last)?
(macrolet (
    (defmapper (fn-n fn-1 fn-2 bind*
                result-form
                call-1-form call-2-form call-n-form fn-car )
      (let ((block-name '#:|map-block|))
      `(progn
         ;; mapcar/1
         (defun ,fn-1 (function list-1)
           (let ,bind*
             (do ((scan-1 list-1 (cdr scan-1)))
                 ((endp scan-1) ,result-form)
               ,call-1-form ) ) )

         ;; mapcar/2
         (defun ,fn-2 (function list-1 list-2)
           (let ,bind*
             (do ((scan-1 list-1 (cdr scan-1))
                  (scan-2 list-2 (cdr scan-2)) )
                 ((or (endp scan-1) (endp scan-2)) ,result-form)
               ,call-2-form ) ) )

         ;; mapcar-n
         (defun ,fn-n (function list-1 &rest lists)
              (declare (dynamic-extent lists))
           (cond
             ((null lists)        (,fn-1 function list-1))
             ((null (cdr lists))  (,fn-2 function list-1 (car lists)))
             (t
               (push list-1 lists)
               (let ,bind*
                 (block ,block-name
                   (do ((scan list-1 (cdr scan)))
                       ((endp scan))
                     (let ((args '()))
                       (do ((lists lists (cdr lists)))
                           ((endp lists))
                         (when (endp (car lists))
                           (return-from ,block-name) )
                         (push (,fn-car lists) args)
                         (setf (car lists) (cdar lists)) )
                       (setq args (nreverse args))
                       ,call-n-form ) ) )
                 ,result-form ) )) ) ) ) )
    )
    ;;
    ;; mapc, mapcar, mapcan
    ;;
    (defmapper cl:mapc mapc/1 mapc/2
            ()
            list-1
            (funcall function (car scan-1))
            (funcall function (car scan-1) (car scan-2))
            (apply function args)
            caar )

    (defmapper cl:mapcar mapcar/1 mapcar/2
            ((result '()))
            (nreverse result)
            (push (funcall function (car scan-1)) result)
            (push (funcall function (car scan-1) (car scan-2)) result)
            (push (apply function args) result)
            caar )

    (defmapper cl:mapcan mapcan/1 mapcan/2
            ((result '()))
            (nreverse result)
            (setq result (nreconc (funcall function (car scan-1)) result))
            (setq result (nreconc (funcall function (car scan-1) (car scan-2))
                                  result ))
            (setq result (nreconc (apply function args) result))
            caar )
    ;;
    ;; mapl, maplist, mapcon
    ;;
    (defmapper cl:mapl mapl/1 mapl/2
            ()
            list-1
            (funcall function scan-1)
            (funcall function scan-1 scan-2)
            (apply function args)
            car )

    (defmapper cl:maplist maplist/1 maplist/2
            ((result '()))
            (nreverse result)
            (push (funcall function scan-1) result)
            (push (funcall function scan-1 scan-2) result)
            (push (apply function args) result)
            car )

    (defmapper cl:mapcon mapcon/1 mapcon/2
            ((result '()))
            (nreverse result)
            (setq result (nreconc (funcall function scan-1) result))
            (setq result (nreconc (funcall function scan-1 scan-2) result))
            (setq result (nreconc (apply function args) result))
            car ) )


;;;; 14.2.28 nbutlast
(defun cl:nbutlast (list &optional (n 1))
    (check-type list list)
    (check-type n (integer 0 *))
  (let* ((length
           (loop
             with runner = list
             for count = 0 then (1+ count)
             while (consp runner) do
               (setq runner (rest runner))
             finally
               (return count) ) )
         (count  (- length n)) )
      (declare (type fixnum count))
    (when (> count length)
      (setq count length) )
    (when (plusp count)
      ;; Set CDR of count-1'th cons to NIL.
      (let ((runner list))
        (loop
          (when (= count 1)
            (setf (cdr runner) nil)
            (return list) )
          (setq runner (cdr runner))
          (decf count) ) ) )) )


;;;; 14.2.25 nconc
;;; Example:
;;;     (nconc)               => nil
;;;     (nconc x)             => x
;;;     (nconc '(1 2) 3)      => (1 2 . 3)
;;;     (nconc '(1 2) '(3 4)) => (1 2 3 4)
;;;     (nconc nil)           => nil
;;;     (nconc nil 1)         => 1
;;;     (nconc nil '(1 2))    => (1 2)
;;;     (nconc '(1 2) nil)    => (1 2)
;;;
;;; Description:
;;;  Sets cdr of the last element of list to next list except for the
;;;  last argument:
;;;     (setf (cdr (last list[n])) list[n + 1])
;;;
;;; Note:
;;;   (nconc nil . lists) == (nconc . lists)
;
(defun cl:nconc (&rest lists)
    (declare (dynamic-extent lists))
  (cond
    ((null lists) '())
    ((null (cdr lists)) (car lists))
    (t
      (let ((list1 (car lists)))
        ;; Skip leading nil's
        (loop
          (setq lists (cdr lists))
          (unless (null list1) (return))
          (setq list1 (car lists))
          (when (null lists) (return)) )

        (unless (null lists)
          (let ((last (last list1)))
            (loop
              (let ((list (car lists)))
               (setf (cdr last) list)
               (setq lists (cdr lists))
               (when (null lists) (return))
               (when list (setq last (last list))) )) ))
         list1 ) )) )


;;;; 14.2.27 nreconc
;;; Note:
;;;     (nreconc list tail) == (nconc (nreverse list) tail)
(defun cl:nreconc (list tail)
    (declare (type list list))
  (do ((scan tail))
      ((endp list) scan)
    (rotatef (cdr list) scan list) ) )


;;;; 14.2.11 nsublis
(defun cl:nsublis (alist tree &key key test test-not)
  (setq test (ensure-test-function test test-not))
  (labels (
    (nsublis-aux (alist tree key test)
      (let ((key.datum (assoc (funcall/key key tree) alist :test test)))
        (cond
          (key.datum
            (cdr key.datum) )
          ((atom tree)
            tree )
          (t
            (setf (car tree) (nsublis-aux alist (car tree) key test))
            (setf (cdr tree) (nsublis-aux alist (cdr tree) key test))
           tree )) ) )
    )
    ;;
    ;; nsublis
    ;;
    (nsublis-aux alist tree key test) ) )


;;;; 14.2.12 nsubst
(defun cl:nsubst (new old tree &key key test test-not)
  (setq test (ensure-test-function test test-not))
  (labels (
    (nsubst-aux (new old tree key test)
      (cond
        ((funcall test old (funcall/key key tree)) new)
        ((atom tree) tree)
        (t
          (setf (car tree) (nsubst-aux new old (car tree) key test))
          (setf (cdr tree) (nsubst-aux new old (cdr tree) key test))
          tree )) )
    )
    ;;
    ;; nsubst
    ;;
    (nsubst-aux new old tree key test) ) )


;;;; 14.2.12 nsubst-if
(defun cl:nsubst-if (new predicate tree &key key)
  (labels (
    (nsubst-if-aux (new predicate tree key)
      (cond
        ((funcall predicate (funcall/key key tree)) new)
        ((atom tree) tree)
        (t
          (setf (car tree) (nsubst-if-aux new predicate (car tree) key))
          (setf (cdr tree) (nsubst-if-aux new predicate (cdr tree) key))
          tree )) )
    )
    ;;
    ;; nsubst-if
    ;;
    (nsubst-if-aux new predicate tree key) ) )


;;;; 14.2.22 nth
(defun cl:nth (n list)
    (declare (type ext:sequence-index n))
    (declare (type list  list))
  (car (nthcdr n list)) )


;;;; (setf nth)
(defun (setf cl:nth) (object n list)
    (declare (type ext:sequence-index n))
    (declare (type list  list))
  (setf (car (nthcdr n list)) object) )


;;;; 14.2.31 nthcdr
(defun cl:nthcdr (n list)
    (declare (type ext:sequence-index n))
    (declare (type list list))
  (let ((runner list))
    (loop
      (when (zerop n) (return runner))
      (when (endp runner) (return nil))
      (decf n)
      (setq runner (cdr runner)) ) ) )


;;;; 14.2.38 pairlis
(defun pairlis (keys data &optional alist)
  (loop
    (when (and (endp keys) (endp data))
      (return) )
    (push (cons (pop keys) (pop data)) alist) )
  alist )


;;;; 14.2.24 null
(defun cl:null (object)
  (not object) )


;;;; 14.2.39 rassoc
(defun cl:rassoc (item alist &key key test test-not)
  (setq test (ensure-test-function test test-not))
  (dolist (key.val alist)
    (when (and key.val (funcall test item (funcall/key key (cdr key.val))))
      (return key.val) ) ) )


;;;; 14.2.39 rassoc-if
(defun cl:rassoc-if (predicate alist &key key)
  (dolist (key.val alist)
    (when (and key.val (funcall predicate (funcall/key key (cdr key.val))))
      (return key.val) ) ) )


;;;; 14.2.27 revappend
;;;
;;; Note:
;;;     (nreconc list tail) == (nconc (nreverse list) tail)
;
(defun cl:revappend (list tail)
  (let ((result tail))
    (dolist (elt list)
      (push elt result) )
    result ) )


;;;; 14.2.8 rplaca
(defun cl:rplaca (cons object)
  (setf (car cons) object)
  cons )


;;;; 14.2.8 rplacd
(defun cl:rplacd (cons object)
  (setf (cdr cons) object)
  cons )


;;;; 14.2.11 sublis
(defun cl:sublis (alist tree &key key test test-not)
  (setq test (ensure-test-function test test-not))
  (labels (
    (sublis-aux (alist tree key test)
      (let ((key.datum (assoc (funcall/key key tree) alist :test test)))
        (cond
          (key.datum (cdr key.datum))
          ((atom tree) tree)
          (t (let ((a (sublis-aux alist (car tree) key test))
                   (d (sublis-aux alist (cdr tree) key test)) )
               (if (and (eql a (car tree)) (eql d (cdr tree)))
                   tree
               (cons a d) ) ) )) ) )
    )
    ;;
    ;; sublis
    ;;
    (sublis-aux alist tree key test) ) )


;;;; 14.2.12 subst
(defun cl:subst (new old tree &key key test test-not)
  (setq test (ensure-test-function test test-not))
  (labels (
    (subst-aux (new old tree key test)
      (cond
        ((funcall test old (funcall/key key tree))
          new )

        ((atom tree)
          tree )

        (t
          (let ((car (subst-aux new old (car tree) key test))
                (cdr (subst-aux new old (cdr tree) key test)) )
            (if (and (eql car (car tree)) (eql cdr (cdr tree)))
                tree
                (cons car cdr) ) )) ) )
    )
    ;;
    ;; subst
    ;;
    (subst-aux new old tree key test) ) )


;;;; 14.2.12 subst-if
(defun cl:subst-if (new predicate tree &key key)
  (labels (
    (subst-if-aux (new predicate tree key)
      (cond
        ((funcall predicate (funcall/key key tree))
          new )

        ((atom tree)
          tree )

        (t
          (let ((car (subst-if-aux new predicate (car tree) key))
                (cdr (subst-if-aux new predicate (cdr tree) key)) )
            (if (and (eql car (car tree)) (eql cdr (cdr tree)))
                tree
                (cons car cdr) ) )) ) )
    )
    ;;
    ;; subst-if
    ;;
    (subst-if-aux new predicate tree key) ) )


;;;; 14.2.30 tailp
(defun cl:tailp (object list)
    (declare (type list list))
  (do ((list list (cdr list)))
      ((atom list) (eql list object))
    (if (eql object list)
        (return t) ) ) )


;;;; 14.2.13 tree-equal
(defun cl:tree-equal (list1 list2 &key test test-not)
    (declare (type list list1 list2))
  (setq test (ensure-test-function test test-not))
  (labels (
    (tree-equal-aux (x y test)
      (if (not (and (consp x) (consp y)))
          (funcall test x y)
        (and (tree-equal-aux (car x) (car y) test)
             (tree-equal-aux (cdr x) (cdr y) test) )) )
    )
    ;;
    ;; tree-equal
    ;;
    (tree-equal-aux list1 list2 test) ) )
