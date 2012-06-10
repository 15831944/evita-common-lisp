;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: TC-User; Base: 10 -*-
;;; @(#)$Id: //proj/evcl3/mainline/testcase/tc-19-cl-filename.lisp#1 $
(in-package :tc-user)

;;;; 19.4.3 pathname
(deftest cl-19-04-03-001 (:depend-to (cl-19-04-05-001))
  (pathnamep (pathname ""))
  :boolean t )

(deftest cl-19-04-03-002 (:depend-to (cl-19-04-05-001))
  (pathnamep (pathname "foo"))
  :boolean t )


;;;; 19.4.4 make-pathname
(deftest cl-19-04-04-001 (:depend-to (cl-19-04-05-001))
  (pathnamep (make-pathname))
  :boolean t )

(deftest cl-19-04-04-002 (:depend-to (cl-19-04-04-001))
  (pathname-type (make-pathname :type "foo"))
  "foo" )

(deftest cl-19-04-04-003 (:depend-to (cl-19-04-04-001))
  (pathname-name (make-pathname :name "foo"))
  "foo" )

(deftest cl-19-04-04-004 (:depend-to (cl-19-04-04-001))
  (pathname-host (make-pathname :host "SYS"))
  "SYS" )

;;;; 19.4.5 pathnamp
(deftest cl-19-04-05-001 (cl-19) (pathnamep (make-pathname)) :boolean t)
(deftest cl-19-04-05-002 () (pathnamep 0) nil)
(deftest cl-19-04-05-003 () (pathnamep 1) nil)
(deftest cl-19-04-05-004 () (pathnamep nil) nil)
(deftest cl-19-04-05-005 () (pathnamep '(1)) nil)
(deftest cl-19-04-05-006 () (pathnamep "foo") nil)
(deftest cl-19-04-05-007 () (pathnamep "") nil)
(deftest cl-19-04-05-008 () (pathnamep #()) nil)
(deftest cl-19-04-05-009 () (pathnamep #(1)) nil)
(deftest cl-19-04-05-010 () (pathnamep #*) nil)
(deftest cl-19-04-05-011 () (pathnamep #*11011) nil)

;;;; 19.4.6 pathname-host
(deftest cl-19-04-06-001 (:depend-to (cl-19-04-04-001))
  (pathname-host (make-pathname :host "SYS"))
  "SYS" )

;;;; 19.4.6 pathname-device
(deftest cl-19-04-06-101 (:depend-to (cl-19-04-04-001))
  (pathname-device (make-pathname))
  nil )

;;;; 19.4.6 pathname-directory
(deftest cl-19-04-06-201 (:depend-to (cl-19-04-04-001))
  (pathname-directory (make-pathname))
  nil )

;;;; 19.4.6 pathname-name
(deftest cl-19-04-06-301 (:depend-to (cl-19-04-04-001))
  (pathname-name (make-pathname))
  nil )

(deftest cl-19-04-06-302 (:depend-to (cl-19-04-04-001))
  (pathname-name (make-pathname :name "foo") :case :common)
  "FOO" )

;;;; 19.4.6 pathname-type
(deftest cl-19-04-06-401 (:depend-to (cl-19-04-04-001))
  (pathname-type (make-pathname))
  nil )

(deftest cl-19-04-06-402 (:depend-to (cl-19-04-04-001))
  (pathname-type (make-pathname :type "foo") :case :common)
  "FOO" )

;;;; 19.4.6 pathname-version
(deftest cl-19-04-06-501 (:depend-to (cl-19-04-04-001))
    "Platform dependent"
  (pathname-version (make-pathname))
  :unspecific )


;;;; 19.4.7  load-logical-pathname-translations
;;;; 19.4.8  logical-pathname-translations

;;;; 19.4.9  logical-pathname
(deftest cl-19-04-09-001 ()
  (pathnamep (logical-pathname "sys:"))
  :boolean t )

(deftest cl-19-04-09-002 ()
  (pathnamep (logical-pathname "sys:a"))
  :boolean t )

(deftest cl-19-04-09-100 ()
  (pathnamep (logical-pathname ""))
  :error t )

;;;; 19.4.10 *default-pathname-defaults*
(deftest cl-19-04-10-001 ()
  (pathnamep *default-pathname-defaults*)
  :boolean t )

;;;; 19.4.11 namestring
(deftest cl-19-04-11-001 ()
    "Depends on platform"
  (string-equal (namestring "/foo/bar.lisp") "/foo/bar.lisp")
  :boolean t )

;;;; 19.4.11 directory-namestring
;; Depend on platform
(deftest cl-10-04-11-101 () (directory-namestring "")         "")
(deftest cl-10-04-11-102 () (directory-namestring "/")        "/")
(deftest cl-10-04-11-103 () (directory-namestring "/a/b/c")   "/a/b/")
(deftest cl-10-04-11-104 () (directory-namestring "/a/b/c/")  "/a/b/c/")
(deftest cl-10-04-11-105 () (directory-namestring ".")        "")
(deftest cl-10-04-11-106 () (directory-namestring "./a/b/c")  "a/b/")
(deftest cl-10-04-11-107 () (directory-namestring "./a/b/c/") "a/b/c/")

;;;; 19.4.11 file-namestring
;; Depend on platform
(deftest cl-19-04-11-201 () (file-namestring "")                    "")
(deftest cl-19-04-11-202 () (file-namestring "/")                   "")
(deftest cl-19-04-11-203 () (file-namestring "/a/b/c")              "c")
(deftest cl-19-04-11-204 () (file-namestring "/a/b/c/")             "")
(deftest cl-19-04-11-205 () (file-namestring ".")                   "")
(deftest cl-19-04-11-206 () (file-namestring "./a/b/c")             "c")
(deftest cl-19-04-11-207 () (file-namestring "./a/b/c/")            "")
(deftest cl-19-04-11-208 () (file-namestring "/foo/bar/baz")        "baz")
(deftest cl-19-04-11-209 () (file-namestring "/foo/bar/baz.lisp")   "baz.lisp")


;;;; 19.4.11 host-namestring
;;;; 19.4.11 enough-namestring
;;;; 19.4.12 parse-namestring

;;;; 19.4.13 wild-pathname-p
(deftest cl-19-04-13-001 ()
  (wild-pathname-p (make-pathname :name :wild)) :boolean t )

(deftest cl-19-04-13-002 ()
  (wild-pathname-p (make-pathname :name :wild) :name) :boolean t)

(deftest cl-19-04-13-002 ()
  (wild-pathname-p (make-pathname :name :wild) :type) nil)

(deftest cl-19-04-13-101 ()
  "Depends on platform"
  (wild-pathname-p "foo") nil )

(deftest cl-19-04-13-102 ()
  "Depends on platform"
  (wild-pathname-p "*") :boolean t )

;;;; 19.4.14 pathname-match-p
(deftest cl-19-04-14-001 ()
  (pathname-match-p "foo" (make-pathname))
  :boolean t )

(deftest cl-19-04-14-002 ()
  (pathname-match-p "foo" (make-pathname :name "foo"))
  :boolean t )

(deftest cl-19-04-14-003 ()
  (pathname-match-p "foo" (make-pathname :name "bar"))
  nil )

(deftest cl-19-04-14-005 ()
  (pathname-match-p "foo" (make-pathname :name :wild))
  :boolean t )

;;;; 19.4.15 translate-logical-pathname
;;;; 19.4.16 translate-pathname

;;;; 19.4.17 merge-pathnames
(deftest cl-19-04-17-001 ()
  (let ((p (merge-pathnames (make-pathname) (make-pathname :name "foo"))))
    (pathname-name p :case :common) )
  "FOO" )

(deftest cl-19-04-17-002 ()
  (let ((p (merge-pathnames (make-pathname) (make-pathname :type "foo"))))
    (pathname-type p :case :common) )
  "FOO" )

(deftest cl-19-04-17-003 ()
  (let ((p (merge-pathnames
                (make-pathname)
                (make-pathname :directory '(:absolute "foo" "bar")) ) ))
    (pathname-directory p :case :common) )
  (:absolute "FOO" "BAR") )
