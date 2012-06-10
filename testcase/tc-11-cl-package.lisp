;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-11-cl-package.lisp#1 $
(in-package :tc-user)

;;; 11.2.2 export
(deftest cl-11-02-001 ()
    (let ((package (find-package "TEMP")) symbol-1 status-1 symbol-2 status-2)
      (when package (delete-package package))
      (setq package (make-package "TEMP" :use nil))
      (use-package 'temp)
      (intern "TEMP-SYM" 'temp)
      (multiple-value-setq (symbol-1 status-1) (find-symbol "TEMP-SYM"))
      (export (find-symbol "TEMP-SYM" 'temp) 'temp)
      (multiple-value-setq (symbol-2 status-2) (find-symbol "TEMP-SYM"))
      (unuse-package 'temp)
      (values symbol-1
              status-1
             (symbol-name symbol-2)
             (bool (eq package (symbol-package symbol-2)))
             status-2 ) )
  (values nil nil "TEMP-SYM" t :inherited) )


;;; 11.2.3 find-symbol
(deftest cl-11-03-001 () (find-symbol "NEVER-BEFORE-USED") (values NIL NIL))
(deftest cl-11-03-002 () (find-symbol "NEVER-BEFORE-USED") (values NIL NIL))
;(intern "NEVER-BEFORE-USED")   (NEVER-BEFORE-USED NIL)
;(intern "NEVER-BEFORE-USED")   (NEVER-BEFORE-USED :INTERNAL)
;(find-symbol "NEVER-BEFORE-USED")   NEVER-BEFORE-USED :INTERNAL
(deftest cl-11-03-003 () (find-symbol "never-before-used") (values NIL NIL))
(deftest cl-11-03-004 () (find-symbol "car")               (values NIL NIL))

(deftest cl-11-03-005 () (find-symbol "CAR" 'common-lisp-user)
    (values CAR :INHERITED) )

(deftest cl-11-03-006 () (find-symbol "CAR" 'common-lisp)
    (values CAR :EXTERNAL) )

(deftest cl-11-03-007 () (find-symbol "NIL" 'common-lisp-user)
    (values NIL :INHERITED) )

(deftest cl-11-03-008 () (find-symbol "NIL" 'common-lisp)
    (values NIL :EXTERNAL) )


;;; 11.2.4 find-package
(deftest cl-11-04-001 ()
    (find-package 'common-lisp)
  #.(symbol-package 'car) )

(deftest cl-11-04-002 ()
    (find-package 'common-lisp-user)
  #.(car (member "COMMON-LISP-USER" (list-all-packages)
                    :key 'package-name
                    :test 'string= )) )

(deftest cl-11-04-003 () (find-package 'not-there) nil)


;;; 11.2.5 find-all-symbols
(deftest cl-11-05-001 () (find-all-symbols 'car) (car))

(deftest cl-11-05-002 () 
    (let ((package (find-package 'temp)) symbol symbols)
      (when package (delete-package 'temp))
      (setq package (make-package 'temp :use nil))
      (setq symbol (intern "CAR" package))   ; TEMP::CAR, NIL
      (setq symbols (find-all-symbols 'car))
      (values (length symbols)
              (not (null (member symbol symbols :test #'eq)))
              (not (null (member 'car   symbols :test #'eq))) ) )
  (values 2 t t) )


;;; 11.2.6 import
(deftest cl-11-06-001 ()
    (let ((package (find-package "TEMP")))
      (when package (delete-package package))
      (setq package (make-package "TEMP" :use nil))

      (values
        (import 'cl:car package)
        (multiple-value-list (find-symbol "CAR" package))
        (multiple-value-list (find-symbol "CDR" package)) ) )
  (values t (car :internal) (nil nil)) )


;;; 11.2.8 rename-package
(deftest cl-11-08-001 ()
    (locally
      (declare (notinline find-package))
      (let ((package (or (find-package 'temporary)
                         (find-package "TEMP")
                         (find-package 'ephemeral) ) ) )
        (when package (delete-package package))
        (setq package (make-package 'temporary :nicknames '("TEMP")))
        (values
          (package-name (rename-package 'temp 'ephemeral))
          (package-nicknames (find-package 'ephemeral))
          (find-package 'temporary)
          (package-name
            (rename-package 'ephemeral 'temporary '(temp fleeting)) )
          (package-nicknames (find-package 'temp)) ) ) )
  (values "EPHEMERAL" () nil "TEMPORARY" ("TEMP" "FLEETING")) )


;;; 11.2.9 shadow
(deftest cl-11-09-001 ()
    (let ((package (find-package "TEMP")) temp-car status)
      (when package (delete-package package))
      (setq package (make-package "TEMP"))
      (values
        (multiple-value-list (find-symbol "CAR" 'temp))
        (shadow 'car 'temp)
        (progn
          (multiple-value-setq (temp-car status) (find-symbol "CAR" 'temp))
          (list (package-name (symbol-package temp-car)) status) )
        (eq 'car (first (package-shadowing-symbols 'temp))) ) )
  (values (car :inherited) t ("TEMP" :internal) nil) )

(deftest cl-11-09-002 ()
    (let ((package (find-package "TEST-1")) symbol)
      (when package (delete-package package))
      (setq package (make-package "TEST-1"))
      (setq symbol  (intern "TEST" package))
      (shadow symbol package)
      (shadow 'test  package)
      (not (null (member symbol (package-shadowing-symbols package)))) )
  t )

(deftest cl-11-09-003 ()
    (let ((package (find-package "TEST-2")) symbol)
      (when package (delete-package package))
      (setq package (make-package "TEST-2"))
      (setq symbol  (intern "TEST" package))
      (export symbol package)
      (multiple-value-prog1
          (use-package 'test-2 'test-1) ; should not error
        (unuse-package 'test-2 'test-1) ) )
  t )


;;; 11.2.13 with-package-iterator
(deftest cl-11-13-001 ()
  (with-package-iterator (next '(:tc-user) :internal)
    (multiple-value-bind (more sym acc pkg) (next)
      (values more
              (bool (symbolp sym))
              (bool (eq acc :internal))
              (bool (eq (find-package :tc-user) pkg)) ) ) )
  (values t t t t) )

(deftest cl-11-13-002 ()
  "Invalid symbol-type."
  (with-package-iterator (next '(:tc-user) :foo) (next))
  :error program-error )


;;; 11.2.21 intern
(deftest cl-11-21-001 ()
    (let ((package (find-package "TEMP"))
          symbol-1 status-1
          symbol-2 status-2
          symbol-3 status-3
          symbol-4 status-4 )
      (when package (delete-package package))
      (setq package (make-package "TEMP" :use nil))

      (let ((sym (find-symbol "NEVER-BEFORE" :keyword)))
        (when sym (unintern sym :keyword)) )

      (multiple-value-setq (symbol-1 status-1)
        (intern "Never-Before" package) )

      (multiple-value-setq (symbol-2 status-2)
        (intern "Never-Before" package) )

      (multiple-value-setq (symbol-3 status-3)
        (intern "NEVER-BEFORE" :keyword) )

      (multiple-value-setq (symbol-4 status-4)
        (intern "NEVER-BEFORE" :keyword) )

      (values
        (package-name (symbol-package symbol-1))
        (symbol-name symbol-1)
        status-1

        (package-name (symbol-package symbol-2))
        (symbol-name symbol-2)
        status-2

        (package-name (symbol-package symbol-3))
        (symbol-name symbol-3)
        status-3

        (package-name (symbol-package symbol-4))
        (symbol-name symbol-4)
        status-4 ) )
  (values
    "TEMP" "Never-Before" NIL
    "TEMP" "Never-Before" :internal
    "KEYWORD" "NEVER-BEFORE" NIL
    "KEYWORD" "NEVER-BEFORE" :external ) )


;;; 11.2.22 package-name
(deftest cl-11-22-001 ()
    (let ((*package* *package*))
      (in-package "COMMON-LISP-USER")
      (package-name *package*) )
  "COMMON-LISP-USER" )

(deftest cl-11-22-002 () (package-name :keyword) "KEYWORD")

(deftest cl-11-22-003 ()
    (package-name (find-package 'common-lisp))  "COMMON-LISP" )

(deftest cl-11-22-004 ()
    (let ((package (find-package 'foo)))
      (when package (delete-package 'foo))
      (setq package (make-package 'foo))
      (rename-package "FOO" "FOO0")
      (prog1
          (package-name package)
        (delete-package package) ) )
  "FOO0" )



;;; 11.2.23 package-nicknames
(deftest cl-11-23-001 ()
    (let ((package (or (find-package 'temporary)
                       (find-package "TEMP")
                       (find-package "temp") ) ) )
      (when package (delete-package package))
      (setq package (make-package 'temporary :nicknames '("TEMP" "temp")))
      (prog1
        (package-nicknames package)
        (delete-package package) ) )
 ("TEMP" "temp") )


;;; 11.2.24 package-shadowing-symbols
(deftest cl-11-24-001 ()
    (let ((package (or (find-package 'temp)))
          (results '())
          temp-cdr
          temp-pill )
      (when package (delete-package package))
      (setq package (make-package 'temp))
      (push (package-shadowing-symbols package) results)
      (push (shadow 'cdr 'temp) results)
      (setq temp-cdr (find-symbol "CDR" 'temp))
      (let ((symbols (package-shadowing-symbols package)))
        (push (list (bool (eq temp-cdr (first symbols)))
                    (null (rest symbols)) )
              results ) )
       (setq temp-pill (intern "PILL" 'temp))
       (push (shadowing-import temp-pill 'temp) results)
       (let ((symbols (package-shadowing-symbols package)))
         (push (list (bool (= 2 (length symbols)))
                     (not (null (member temp-cdr symbols :test #'eq)))
                     (not (null (member temp-pill symbols :test #'eq))) )
               results ) )
       (values-list (nreverse results)) )
  (values nil t (t t) t (t t t)) )


;;; 11.2.10 shadowing-import
(deftest cl-11-10-001 ()
    (let ((package-1 (find-package "TEMP-1"))
          (package-2 (find-package "TEMP-2"))
          symbol-1 symbol-2)

      (when package-1 (delete-package package-1))
      (setq package-1 (make-package "TEMP-1"))

      (when package-2 (delete-package package-2))
      (setq package-2 (make-package "TEMP-2"))

      (setq symbol-1 (intern "CONFLICT" package-1))
      (setq symbol-2 (intern "CONFLICT" package-2))

      (values
        (package-shadowing-symbols package-1)
        (list (package-name (symbol-package symbol-1)) (symbol-name symbol-1))
        (shadowing-import symbol-2 package-1)
        (let* ((symbols (package-shadowing-symbols package-1))
               (symbol  (first symbols)) )
          (list (length symbols)
                (package-name (symbol-package symbol))
                (symbol-name symbol) ) )
        (let ((symbol (find-symbol "CONFLICT" package-1)))
          (list (package-name (symbol-package symbol))
                (symbol-name symbol) ))) )
  (values nil ("TEMP-1" "CONFLICT")
          t (1 "TEMP-2" "CONFLICT")
          ("TEMP-2" "CONFLICT") ) )


;;; 11.2.13 with-package-iterator
(deftest cl-11-13-001 ()
    (let ((count 0) (the-package (find-package :cl)))
      (with-package-iterator (next-symbol (list the-package) :external)
        (loop
          (multiple-value-bind (more? symbol status package) (next-symbol)
            (unless more? (return))
            (when (and (eq :external status)
                       (symbolp symbol)
                       (eq the-package package) )
              (incf count) ) )) )
      (bool (eql count 978)) ) t )

#+evcl
(deftest cl-11-13-002 ()
    (let ((count 0) (the-package (find-package :system)))
      (with-package-iterator (next-symbol (list the-package) :internal)
        (loop
          (multiple-value-bind (more? symbol status package) (next-symbol)
            (unless more? (return))
            (when (and (symbolp symbol)
                       (eq :internal status)
                       (eq the-package package) )
              (incf count) ) )) )
      (bool (eql count
                 (svref (ext:ref package si::internal-table the-package)
                        0 ))) )
  :boolean t )

(deftest cl-11-13-003 ()
    (let ((count 0)
          (checked 0)
          (the-package (find-package :cl-user)))
      (with-package-iterator (next-symbol (list the-package) :inherited)
        (loop
          (multiple-value-bind (more? symbol status package) (next-symbol)
            (unless more? (return))
            (when (and (eq :inherited status)
                       (symbolp symbol)
                       (eq the-package package) )
              (incf count)
              (when (eq :inherited
                        (nth-value 1 (find-symbol (symbol-name symbol)
                                                  the-package )))
                (incf checked) )) )) )
      (eq count checked) )
  :boolean t )

(deftest cl-11-13-004 ()
    (with-package-iterator (next-symbol (list :cl) :external)
      (loop
        (multiple-value-bind (more? symbol) (next-symbol)
          (unless more? (return nil))
          (when (null symbol) (return t)) )) )
  :boolean t )


;;; 11.2.14 unexport
(deftest cl-11-14-001 ()
    (let ((package-1 (find-package "TEMP-1"))
          (package-2 (find-package "TEMP-2")) )
      (when package-1 (delete-package package-1))
      (setq package-1 (make-package "TEMP-1"))

      (when package-2 (delete-package package-2))
      (setq package-2 (make-package "TEMP-2"))

      (values
        (export (intern "CONTRABAND" package-1) package-1)
        (multiple-value-list (find-symbol "CONTRABAND" package-2))
        (use-package package-1 package-2)
        (multiple-value-bind (found status)
            (find-symbol "CONTRABAND" package-2)
          (list (package-name (symbol-package found))
                (symbol-name found)
                status ) )
        (unexport (find-symbol "CONTRABAND" package-2) package-1)
        (multiple-value-list (find-symbol "CONTRABAND" package-2))
        (unuse-package package-1 package-2) ) )
    (values t (nil nil) t ("TEMP-1" "CONTRABAND" :inherited) t (nil nil) t) )


;;; 11.2.15 unintern
(deftest cl-11-15-001 ()
    (let ((package (find-package "TEMP")) symbol)
      (when package (delete-package package))
      (setq package (make-package "TEMP"))
      (setq symbol (intern "UNPACK" 'temp))
      (values
        (unintern symbol 'temp)
        (multiple-value-list (find-symbol "UNPACK" 'temp))
        (symbol-package symbol)
        (symbol-name    symbol) ) )
  (values :internal (nil nil) nil "UNPACK") )


;;; 11.2.17 unuse-package
(deftest cl-11-17-001 ()
    (let ((*package* (find-package :cl-user))
          (package (find-package 'temp))
          unuse-package
          symbol-1 status-1
          symbol-2 status-2
          symbol-3 status-3 )
      (let ((x (find-symbol "SHOES" :cl-user)))
        (when x (unintern x :cl-user)) )
      (when package (delete-package package))
      (setq package (make-package "TEMP"))
      (export (intern "SHOES" 'temp) 'temp)
      (multiple-value-setq (symbol-1 status-1) (find-symbol "SHOES"))
      (use-package 'temp)
      (multiple-value-setq (symbol-2 status-2) (find-symbol "SHOES"))
      (setq unuse-package (unuse-package 'temp))
      (multiple-value-setq (symbol-3 status-3) (find-symbol "SHOES"))
      (values symbol-1 status-1
              (package-name (symbol-package symbol-2))
              (symbol-name symbol-2)
              status-2
              unuse-package
              symbol-3 status-3 ) )
  (values nil nil "TEMP" "SHOES" :inherited t nil nil) )


;;; 11.2.20 do-all-symbols
;;; 11.2.20 do-external-symbols
;;; 11.2.20 do-symbols
;;;
;;; Check symbol nil.
(deftest cl-11-20-001 ()
    (do-all-symbols (var (find-package :cl))
      (when (null var) (return t)) ) t )

(deftest cl-11-20-002 ()
    (do-external-symbols (var (find-package :cl))
      (when (null var) (return t)) ) t )

(deftest cl-11-20-003 ()
    (do-symbols (var (find-package :cl))
      (when (null var) (return t)) ) t )
