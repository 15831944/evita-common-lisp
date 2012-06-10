(in-package :si)


(macrolet (
  (defmap (name)
   `(define-compiler-macro ,name (fn list1 &rest list*)
      (cond
        ((null list*)
          `(,(intern ,(format nil "~A/1" name) (symbol-package 'si))
                ,fn ,list1 ) )
        ((null (rest list*))
          `(,(intern ,(format nil "~A/2" name) (symbol-package 'si))
                ,fn ,list1 ,(first list*) ) )) ) )
  )
  ;;
  (defmap mapcar) )

;;; for loop

(defun cl:keywordp (x)
  (and (symbolp x) (eq (symbol-package x) #.(symbol-package :key))) )

(macrolet (
  (define (name nameq)
   `(define-compiler-macro ,name
            (item list &rest rest &key key test &allow-other-keys)
                (declare (ignore rest))
      (when (null test)
        (when (keywordp item) (setq test '(function eq))) )
      (cond
        (key nil)
        ((or (equal test '(function eq)) (equal test '(quote eq)))
          `(,',nameq ,item ,list) )) ) )
  )
  (define cl:assoc assq)
  (define cl:member memq) )


;;; for loop
(defun string= (string1 string2 &key
                    (start1 0) (end1 (length string1))
                    (start2 0) (end2 (length string2)) )
  (labels (
    (compare (v1 v2 s1 e1 s2 e2)
        (declare (values fixnum))
        (declare (type simple-string v1 v2))
        (declare (type fixnum s1 s2 e1 e2))
      (let ((delta (- (- e1 s1) (- e2 s2))))
        (if (not (zerop delta))
            delta
          (let ((i1 s1) (i2 s2))
              (declare (type fixnum i1 i2))
            (loop
              (when (eql i1 e1) (return 0))
              (let ((delta (- (char-int (schar v1 i1))
                              (char-int (schar v2 i2)) ) ))
                (unless (zerop delta) (return delta))
                (incf i1)
                (incf i2) ))) ) ) )
    )
    ;;
    (zerop (compare string1 string2 start1 end1 start2 end2)) ) )


;;; for loop
(defun cl:subtypep (ty1 ty2 &optional env)
    (declare (values t t))
    (declare (ignore env))
  (cond
    ((eq ty1 ty2)  (values t t))
    ((eq ty2 't)   (values t t))
    ((eq ty1 't)   (values nil t))
    ((eq ty1 'nil) (values t t))
    ((eq ty2 'nil) (values nil t))
    (t (values nil nil)) ) )

;;; for loop
(defun c::one-argument-form-p (form operator)
  (and (consp form)
       (eq operator (car form))
       (null (cddr form)) ) )


;;; for loop
(defun memq (key list)
    (declare (type list list))
    (declare (values list))
  (loop
    (when (endp list) (return nil))
    (when (eq (car list) key) (return list))
    (setq list (cdr list)) ) )

(defmacro cl:pushnew (item place &rest args)
  (let ((tmp '#:tmp))
    `(let ((.item ,item))
      (if (member .item ,place ,@args)
          (let ((,tmp ,place)) ,tmp)
        (push .item ,place) ) ) ) )

(defun cl:copy-list (list)
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

(defun cl:nreconc (list tail)
    (declare (values t))
    (declare (type list list))
  (let ((runner tail))
    (loop
      (when (endp list) (return))
      (let ((next (cdr list)))
        (setf (cdr list) runner)
        (setq runner list)
        (setq list next) ))
    runner ) )
