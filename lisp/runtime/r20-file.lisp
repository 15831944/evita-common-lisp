;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 20 Files
;;; lisp/runtime/r20-file.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r20-file.lisp#3 $
;;;
;;; Description:
;;;  This fils contains following functions:
;;;     ensure-physical-pathname    internal
;;;
;;;  Public Functions:
;;;     delete-file                 20.2.8
;;;     directory                   20.2.1
;;;     probe-file                  20.2.2  NYI
;;;     ensure-directories-exist    20.2.3
;;;     file-author                 20.2.5
;;;     file-error                  20.2.9  condition
;;;     file-error-pathname         20.2.10
;;;     file-write-date             20.2.6
;;;     rename-file                 20.2.7
;;;     truename                    20.2.4
;
(in-package :si)

;;;; ensure-physical-pathname
;;;
;;; Syntax:
;;;     ensure-physical-pathname operation filespec
;;;         => physical-pathname
;
(defun ensure-physical-pathname (filespec)
    (declare (type pathname-designator filespec))
    (declare (values pathname))
  (let ((pathname (pathname filespec)))
    (when (wild-pathname-p pathname)
      (error 'wildcard-not-allowed :pathname  pathname) )
    (translate-logical-pathname pathname) ) )

#+win32
(defconstant *platform-error-alist* '(
    (   2 . file-not-found)
    (   3 . path-not-found)
    (   5 . access-error)
    ;; 17 . not same device
    ;; 19 . write protect
    ;; 21 . not ready
    ;; 32 . sharing violation
    ;; 33 . lock violation
    ;; 34 . wrong disk
    ;; 36 . sharing buffer exceeded
    ;; 38 . handle eof
    ;; 39 . handle disk full
    (  80 . file-already-exists)    ; ERROR_FILE_EXISTS: The file exists.
    ;; 126 . MOD_NOT_FOUND
    ;; 127 . PROC_NOT_FOUND
    ( 183 . file-already-exists)    ; ERROR_ALREADY_EXISTS: Cannot create a file when that file already exists.
    ) )


;;;; platform-file-error
;
(defun platform-file-error (operation pathname code &optional pathname-2)
  (let ((class (cdr (assoc code *platform-error-alist*))))
    (when (null class)
      (error 'platform-error
             :code code
             :operation operation
             :operands  (if pathname-2
                            (list pathname pathname-2)
                          (list pathname) )) )

    (case class
      ((file-already-exists)
        (when (eq 'rename-file operation)
          (setq pathname pathname-2) ) ))

    (error class :pathname pathname) ) )


;;;; unix-time-to-universal-time
;
(defun unix-time-to-universal-time (unix-time)
    (declare (type (integer 0) unix-time))
  (+ unix-time #.(encode-universal-time 0 0 0 1 1 1970 0)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;

;;;; 20.2.1 directory
;
(defun cl:directory (pathspec &key)
  (let* ((pathname (truename pathspec))
         (defaults (make-pathname :name nil :type nil :defaults pathname)) )
    (multiple-value-bind (handle filename attr)
        (.find-first-file
            (namestring (make-pathname :name "*"
                                       :type "*"
                                       :defaults pathname )))
      (unwind-protect
          (loop
            with result = nil
            while filename do
             (unless (or (string= "." filename) (string= ".." filename))
               (setq result (merge-pathnames filename defaults))
               (cond
                  ((not (pathname-match-p result pathname))
                    (setq result nil) )

                  ((logbitp 4 attr)
                    (setq result
                      (make-pathname
                        :directory (append (pathname-directory defaults)
                                           (list filename) )
                        :defaults  defaults )) )))
             when result
                collect result
              do
                (multiple-value-setq (filename attr)
                  (.find-next-file handle) ))
        (.find-close handle) ) ) ) )


;;;; 20.2.8 delete-file
;
(defun cl:delete-file (filespec)
  (let* ((pathname (ensure-physical-pathname filespec))
         (error-p  (.delete-file (namestring pathname))) )
    (when error-p
      (platform-file-error 'delete-file pathname error-p) )
    t ) )


;;;; 20.2.3 ensure-directories-exist
;
(defun cl:ensure-directories-exist (pathspec &key verbose &aux herald-p)
  (labels (
    ;; create
    (create (pathname level)
      (let ((dirname (directory-namestring pathname)))
        (when (string= "" dirname)
          (return-from create nil) )
        (loop for code = (.create-directory dirname) do
          (case code
            ((nil)      ; created
              (notice dirname)
              (return t) )

            ((183)      ; already exists
              (return nil) )

            ((3)        ; path not exists
              (notice nil)
              (create (make-pathname
                         :directory (butlast (pathname-directory dirname)) )
                      (1+ level) ) )
          (otherwise
            (platform-file-error
              'ensure-directories-exist dirname code ) ))) ) )

    ;; notice
    (notice (dirname)
      (when verbose
        (unless herald-p
          (format t ";; ensure-directories-exist creating ~A~%" pathspec)
          (setq herald-p t) )
        (when dirname
          (format t ";; Directory ~A is created.~%" dirname) )) )
    )
    ;;
    ;; ensure-directories-exist
    ;;
    (values pathspec
            (create (ensure-physical-pathname pathspec) 0) ) ) )


;;;; 20.2.5 file-author
;
(defun cl:file-author (filespec)
  (let ((pathname (pathname filespec)))
      (declare (ignore pathname))
    nil ) )


;;;; 20.2.6 file-write-date
;
(defun cl:file-write-date (filespec)
  (let ((pathname (ensure-physical-pathname filespec)))
    (multiple-value-bind (type write access create size)
        (.file-attributes (namestring pathname))
        (declare (ignore access size))
      (unless type
        (platform-file-error 'file-write-date pathname create) )
      (unix-time-to-universal-time write) ) ) )


;;;; 20.2.2 probe-file
;
(defun cl:probe-file (pathspec)
  (let ((pathname (truename (ensure-physical-pathname pathspec))))
    (and (open pathname :direction :probe)
         pathname ) ) )


;;;; 20.2.7 rename-file
;
(defun cl:rename-file (filespec new-name)
  (let* ((old-pathname (pathname filespec))
         (old-truename (truename old-pathname))
         (new-pathname (merge-pathnames new-name old-pathname))
         (new-truename (truename new-pathname))
         (error-p (.rename-file (namestring old-truename)
                                (namestring new-truename) )) )
    (when error-p
      (platform-file-error 'rename-file old-pathname error-p new-pathname) )
    (values new-pathname new-truename) ) )


;;;; 20.2.4 truename
;
(defun cl:truename (filespec)
  (let ((pathname (translate-logical-pathname filespec)))
    (truename-using-host (ref pathname host pathname) pathname) ) )


;;;; rename-if-exists
(defun rename-if-exists (filespec)
    (declare (values (or pathname null)))
    (declare (type pathname-designator filespec))
  (loop
    with old-pathname = (truename filespec)
    with old-namestring = (namestring old-pathname)
    with name = (pathname-name old-pathname)
    for i from 0
    for new-pathname = (make-pathname
                            :name (format nil "~D.~A" i name)
                            :defaults old-pathname )
    for new-namestring = (namestring new-pathname)
    for errno = (.rename-file old-namestring new-namestring) do
 (format t "; rename-if-exists errno=~D ~S~%" errno new-pathname)
      (case errno
        ((nil) (return new-pathname))
        ((183)  #+nil ERROR_ALREADY_EXISTS)
        (otherwise (return nil)) )) )
