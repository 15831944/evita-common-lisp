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
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r14-set.lisp#2 $
;;;
;;; Description:
;;;  This fils contains following functions:
;;;     adjoin              14.2.44
;;;     intersection        14.2.43
;;;     nintersection       14.2.43
;;;     nset-difference     14.2.46
;;;     nset-exclusive-or   14.2.47
;;;     member              14.2.33
;;;     member-if           14.2.33
;;;     nunion              14.2.49
;;;     set-difference      14.2.46
;;;     set-exclusive-or    14.2.47
;;;     subsetp             14.2.48
;;;     union               14.2.49
;;;
;;; Note:
;;; Since ANSI-CL requires the first argument of test is a result of key
;;; function with an element of list1 and second argument of test is a result
;;; of key function with an element of list2. To meet this requirement, we
;;; can't use remove-duplicates to implement union, such as:
;;;   (defun nunion (x y) (delete-duplicates (nconc x y))
;;;   (defun union (x y) (remove-duplicates (append x y))
;
(in-package :si)

;;;; 14.2.44 adjoin
;;;; 14.2.43 intersection
;;;; 14.2.43 nintersection
;;;; 14.2.46 nset-difference
;;;; 14.2.47 nset-exclusive-or
;;;; 14.2.49 nunion
;;;; 14.2.46 set-difference
;;;; 14.2.47 set-exclusive-or
;;;; 14.2.48 subsetp
;;;; 14.2.49 union
;
(macrolet (
  ;; member-1-2
  (member-1-2 (key1 list2)
    `(let ((.runner2 ,list2))
       (loop
         (when (endp .runner2) (return nil))
         (let* ((.elt2 (pop .runner2))
                (.key2 (funcall/key key .elt2)) )
           (when (funcall test ,key1 .key2) (return t)) ) ) ) )

  ;; member-2-1
  (member-2-1 (key2 list1)
    `(let ((.runner1 ,list1))
       (loop
         (when (endp .runner1) (return nil))
         (let* ((.elt1 (pop .runner1))
                (.key1 (funcall/key key .elt1)) )
           (when (funcall test .key1 ,key2) (return t)) ) ) ) )
  )
  ;;

  ;; adjoin
  (defun cl:adjoin (item1 list2 &key key test test-not)
      (declare (type proper-list list2))
      (declare (values proper-list))
    (setq test (ensure-test-function test test-not))

    (let ((key1 (funcall/key key item1)))
      (if (member-1-2 key1 list2)
          list2
        (cons item1 list2) ) ) )

  ;; intersection
  (defun cl:intersection (list1 list2 &key key test test-not)
      (declare (type proper-list list1 list2))
      (declare (values proper-list))
    (setq test (ensure-test-function test test-not))

    (let ((result '()))
      (dolist (elt1 list1 result)
        (let ((key1 (funcall/key key elt1)))
          (when (member-1-2 key1 list2)
            (push elt1 result) ) ) ) ) )

  ;; nintersection
  (defun cl:nintersection (list1 list2 &key key test test-not)
      (declare (type proper-list list1 list2))
      (declare (values proper-list))
    (setq test (ensure-test-function test test-not))

    (let ((runner1 list1)
          (prev1   nil) )
      (loop
        (when (endp runner1) (return list1))
        (let* ((elt1  (car runner1))
               (next1 (cdr runner1))
               (key1 (funcall/key key elt1)) )
          (cond
            ((member-1-2 key1 list2)
              (setq prev1 runner1) )
            (prev1
              (setf (cdr prev1) next1) )
            (t
              (setq list1 next1) ))
          (setq runner1 next1) )) ) )

  ;; nset-difference
  (defun cl:nset-difference (list1 list2 &key key test test-not)
      (declare (type proper-list list1 list2))
      (declare (values proper-list))
    (setq test (ensure-test-function test test-not))

    (let ((runner1 list1)
          (prev1   nil) )
      (loop
        (when (endp runner1) (return list1))
        (let* ((elt1  (car runner1))
               (next1 (cdr runner1))
               (key1 (funcall/key key elt1)) )
          (cond
            ((not (member-1-2 key1 list2)) (setq prev1 runner1))
            (prev1 (setf (cdr prev1) next1))
            (t (setq list1 next1)) )
          (setq runner1 next1) )) ) )

  ;; nset-exclusive-or
  (defun cl:nset-exclusive-or (list1 list2 &key key test test-not)
      (declare (type proper-list list1 list2))
      (declare (values proper-list))
    (setq test (ensure-test-function test test-not))

    (let ((curr1 list1)
          (prev1 nil) )
      (loop
        (when (endp curr1) (return))
        (let* ((elt1  (car curr1))
               (next1 (cdr curr1))
               (key1  (funcall/key key elt1)) )
          (let ((curr2 list2)
                (prev2 nil) )
            (loop
              (when (endp curr2)
                (setq prev1 curr1)
                (return) )
              (let* ((elt2  (car curr2))
                     (next2 (cdr curr2))
                     (key2  (funcall/key key elt2)) )
                (when (funcall test key1 key2)
                  (if prev2
                      (setf (cdr prev2) next2)
                    (setq list2 next2) )
                  (if prev1
                      (setf (cdr prev1) next1)
                    (setq list1 next1) )
                  (return) )
                (setq prev2 curr2)
                (setq curr2 next2) )) )
          (setq curr1 next1) ))
      (if prev1
          (setf (cdr prev1) list2)
        (setq list1 list2) ) )
      list1 )

  ;; nunion
  ;;  Connects CDR of each cons of list1 to list2 when CAR of that cons
  ;;   isn't appeared in list.
  (defun cl:nunion (list1 list2 &key key test test-not)
      (declare (type proper-list list1 list2))
      (declare (values proper-list))
    (setq test (ensure-test-function test test-not))

    (if (null list2)
        list1
      (let ((result list2)
            (runner list1) )
        (loop
          (when (endp runner) (return result))
          (let* ((curr runner)
                 (elt1 (pop runner))
                 (key1 (funcall/key key elt1)) )
            (unless (member-1-2 key1 list2)
              (setf (cdr curr) result)
              (setq result curr) ) )) )) )

  ;; set-difference
  ;;  Conses elements in list1 which aren't in list2.
  (defun cl:set-difference (list1 list2 &key key test test-not)
      (declare (type proper-list list1 list2))
      (declare (values proper-list))
    (setq test (ensure-test-function test test-not))

    (let ((result '()))
      (dolist (elt1 list1 result)
        (let ((key1 (funcall/key key elt1)))
          (unless (member-1-2 key1 list2)
            (push elt1 result) ) ) ) ) )

  ;; set-exclusive-or
  (defun cl:set-exclusive-or (list1 list2 &key key test test-not)
      (declare (type proper-list list1 list2))
      (declare (values proper-list))
    (setq test (ensure-test-function test test-not))

    (let ((result '()))
      (dolist (elt1 list1)
        (let ((key1 (funcall/key key elt1)))
          (unless (member-1-2 key1 list2) (push elt1 result)) ) )
      (dolist (elt2 list2 result)
        (let ((key2 (funcall/key key elt2)))
          (unless (member-2-1 key2 list1) (push elt2 result)) ) ) ) )

  ;; subsetp
  (defun cl:subsetp (list1 list2 &key key test test-not)
      (declare (type proper-list list1 list2))
      (declare (values t))
    (setq test (ensure-test-function test test-not))

    (dolist (elt1 list1 t)
      (let ((key1 (funcall/key key elt1)))
        (unless (member-1-2 key1 list2) (return nil)) ) ) )

  ;; union
  (defun cl:union (list1 list2 &key key test test-not)
      (declare (type proper-list list1 list2))
      (declare (values proper-list))
    (setq test (ensure-test-function test test-not))

    (let ((result (copy-list list2)))
      (dolist (elt1 list1 result)
        (let ((key1 (funcall/key key elt1)))
          (unless (member-1-2 key1 list2)
            (push elt1 result) ) ) ) ) )

 ) ; macrolet


;;;; 14.2.33 member
;;;     member item list &key key test => tail
;
(defun cl:member (item list &key key test test-not)
    (declare (type proper-list list))
    (declare (values proper-list))

  (setq test (ensure-test-function test test-not))

  (do ((runner list (cdr runner)))
      ((endp runner) nil)
    (when (funcall test item (funcall/key key (car runner)))
      (return runner) ) ) )


;;;; 14.2.33 member-if
;
(defun cl:member-if (predicate list &key key)
    (declare (type proper-list list))
    (declare (values proper-list))

  (do ((runner list (cdr runner)))
      ((endp runner) nil)
    (when (funcall predicate (funcall/key key (car runner)))
      (return runner) ) ) )
