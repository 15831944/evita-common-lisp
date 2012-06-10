;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 19 Filenames
;;; lisp/runtime/r19-filename.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r19-filename.lisp#4 $
;;;
;;; Description:
;;;  Most of functions in this file are wrapper of corresponding generic-
;;;  function dicrimated by pathname-host or pathname. These GF names start
;;;  with "internal-".
;;;
;;;  This fils contains following functions:
;;;     host-convert-case                   internal
;;;     find-pathname-host                  internal
;;;     pathname-equal                      internal
;;;     pathname-not-matched                internal
;;;     pathname-sxhash                     internal
;;;
;;;     directory-namestring                19.4.11
;;;     enough-namestring                   19.4.11
;;;     file-namestring                     19.4.11
;;;     host-namestring                     19.4.11
;;;     make-pathname                       19.4.4
;;;     merge-pathnames                     19.4.17
;;;     namestring                          19.4.11
;;;     parse-namestring                    19.4.12
;;;     pathname                            19.4.3
;;;     pathname-device                     19.4.6
;;;     pathname-directory                  19.4.6
;;;     pathname-host                       19.4.6
;;;     pathname-match-p                    19.4.14
;;;     pathname-name                       19.4.6
;;;     pathname-type                       19.4.6
;;;     pathname-version                    19.4.6
;;;     pathnamep                           19.4.5
;;;     translate-pathname                  19.4.16
;;;     wild-pathname-p                     19.4.13
;;;
;
(in-package :si)

;;;; find-pathname-host
;
(defun find-pathname-host (hostspec &optional (error-p t))
    (declare (type pathname-host-designator hostspec))
    (declare (values (or basic-host null)))
  (labels (
    ;; find-host
    (find-host (name)
        (declare (values (or basic-host null)))
        (declare (type string name))
      (with-latch (*pathname-hosts-latch* :shared)
        (find name *pathname-hosts*
            :key  #'host-name
            :test #'string-equal )) )
    )
    ;;
    (etypecase hostspec
      (basic-host hostspec)
      (string
        (or (find-host hostspec)
            (when error-p (error 'unknown-host-name :name hostspec)) ) )) ) )


;;;; intern-pathname-host
(defun intern-pathname-host (hostspec make-fn)
    (declare (type pathname-host-designator hostspec))
    (declare (type (function (string) basic-host) make-fn))
    (declare (values basic-host))
  (labels (
    ;; intern-host
    (intern-host (name)
        (declare (values basic-host))
        (declare (type string name))
      (with-latch (*pathname-hosts-latch*)
        (or (find name *pathname-hosts*
                        :key  #'host-name
                        :test #'string-equal )
            (let ((host (funcall make-fn name)))
              (push host *pathname-hosts*)
              host )) ) )
    )
    ;;
    (etypecase hostspec
      (basic-host hostspec)
      (string (intern-host hostspec)) ) ) )


;;;; host-name
;
(defun host-name (host)
    (declare (values string))
    (declare (type basic-host host))
  (ref basic-host name host) )


;;;; host-convert-case
;;;
;;; Called by:
;;;     make-pathname
;;;     pathname-device
;;;     pathname-directory
;;;     pathname-name
;;;     pathname-host
;;;     pathname-type
;;;     pathname-version
;;;
;;; case of string
;;;   :upper  => host's customary case
;;;   :lower  => opposite of host's customary case
;;;   :mixed  => itself
;
(defun host-convert-case (host thing)
    (declare (type basic-host host))
  (labels (
    (string-case (string)
        (declare (values (member :upcase :downcase :mixed)))
      (multiple-value-bind (string start end)
          (string-data string 0 (length string))
          (declare (ignore start))
        (let ((nuppers 0)
              (nlowers 0)
              (nothers 0) )
            (declare (type sequence-index nchars nuppers nlowers nothers))
          (dotimes (i end)
            (let ((char (schar string i)))
             (cond
               ((not (both-case-p char))
                (incf nothers) )
               ((upper-case-p char)
                (unless (zerop nlowers) (return-from string-case :mixed))
                (incf nuppers) )
               (t
                (unless (zerop nuppers) (return-from string-case :mixed))
                (incf nlowers) )) ) )
          (cond
            ((zerop nlowers) :upcase)
            ((zerop nuppers) :downcase)
            (t               :mixed) ) ) ) )
    )
    ;;
  (typecase thing
    (cons
      (let ((results '()))
        (dolist (subthing thing (nreverse results))
          (push (host-convert-case host subthing) results) ) ) )
    (string
      (ecase (string-case thing)
        ((:mixed) thing)
        ((:downcase)
          (ecase (ref basic-host customary-case host)
            ((:downcase) (string-upcase   thing))
            ((:upcase)   (string-downcase thing)) ) )
        ((:upcase)
          (ecase (ref basic-host customary-case host)
            ((:downcase) (string-downcase thing))
            ((:upcase)   (string-upcase   thing)) ) )) )
    (otherwise
      thing )) ) )


;;;; pathname-equal
;;;
;;; Called by:
;;;     equal
;;;
;;; Description:
;;;  Equal Pathnames?
;;;
;;; BUGBUG: NYI: functinally equivalent
;
(defun pathname-equal (x y)
    (assert (pathnamep x))
    (assert (pathnamep y))
 (and (pathname-equal-using-host (ref pathname host x) x y) ) )


;;;; pathname-not-matched
;;;
;;; Called by:
;;;  translate-pathname-using-host
;;;
;;; Description:
;;;  Signales pathname-not-matched error.
;
(defun pathname-not-matched (pathname wildcard)
   (error 'pathname-not-matched
          :pathname pathname
          :wildcard wildcard ) )


;;;; pathname-sxhash
;;;
;;; Description:
;;;  Returns hash-code of pathname
;;;
;;; Called by:
;;;     sxhash/equal
;;;
(defun pathname-sxhash (pathname)
    (declare (type pathname pathname))
  (logxor (sxhash (pathname-host      pathname))
          (sxhash (pathname-device    pathname))
          (sxhash (pathname-directory pathname))
          (sxhash (pathname-name      pathname))
          (sxhash (pathname-type      pathname))
          (sxhash (pathname-version   pathname)) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;

;;;; 19.4.11 directory-namestring
;;;; 19.4.11 file-namestring
;;;; 19.4.11 namestring
;
(macrolet (
    (define (name)
       (let ((fn-name (intern (format nil "INTERNAL-~A" name))))
         `(defun ,name (pathname)
            (let ((pathname (pathname pathname)))
              (with-output-to-string (stream)
                (,fn-name pathname stream) ) ) ) ) )
    )
    ;;
    (define cl:directory-namestring)
    (define cl:file-namestring)
    (define cl:namestring) )


;;;; 19.4.11 enough-namestring
;
(defun cl:enough-namestring (pathname &optional
                                (defaults *default-pathname-defaults*) )
  (let ((pathname (pathname pathname))
        (defaults (pathname defaults)) )
    (with-output-to-string (stream)
      (internal-enough-namestring pathname defaults stream) ) ) )


;;;; 19.4.11 host-namestring
;
(defun cl:host-namestring (pathname)
  (let ((pathname (pathname pathname)))
    (host-name (ref pathname host pathname)) ) )


;;;; 19.4.4 make-pathname
;;;
;;; host        string, nil, pathname-host
;;; device      string, nil, :unspecific
;;; directory   string, list, :wild
;;; name        string, nil, :unspecific, :wild
;;; type        string, nil, :unspecific, :wild
;;; version     string, list of strings, :newest, :unspecific, :wild
;;;             (:oldest, :previous, :installed)
;
(defun cl:make-pathname (&key host
                              (device nil device-p)
                              (directory nil directory-p)
                              (name nil name-p)
                              (type nil type-p)
                              (version nil version-p)
                              (defaults  *default-pathname-defaults*)
                              (case :local) )
    (check-type case (member :local :common))
  (setq defaults (pathname defaults))

  ;; host
  ;;
  (if (not host)
      (setq host (ref pathname host defaults))
    (progn
      (setq host (find-pathname-host host))
      (unless (eq (ref pathname host defaults) host)
        (setq defaults nil) )))

  (when (eq :common case)
    (setq device    (host-convert-case host device))
    (setq directory (host-convert-case host directory))
    (setq name      (host-convert-case host name))
    (setq type      (host-convert-case host type))
    (setq version   (host-convert-case host version)) )


  ;; device
  (setq device (parse-device-component
                  host
                  (if device-p
                      device
                    (and defaults (ref pathname device defaults))) ))

  ;; directory
  (setq directory (parse-directory-component
                    host
                    (if directory-p
                        directory
                     (and defaults (ref pathname directory defaults)) )))

  ;; name
  (setq name (parse-name-component
                host
                (if name-p
                    name
                  (and defaults (ref pathname name defaults)) )))

  ;; type
  (setq type (parse-type-component
                host
                (if type-p
                    type
                  (and defaults (ref pathname type defaults)) )))

  ;; version
  (setq version (parse-version-component
                  host
                  (if version-p
                      version
                    (and defaults (ref pathname version defaults)) )))

  (make-pathname-using-host host device directory name type version) )


;;;; 19.4.17 merge-pathnames
;;;
;;  host    defaults
;;; device  defaults (host = defualts), host (host /= defaults)
;;; name    defaults
;;; type    defaults
;;; version default-version (name), defaults
;;;
;;; Note: host of pathname and defaults must be same type, otherwise
;;; this function signals error.
;;;
;
(defun cl:merge-pathnames (pathname
                           &optional (defaults *default-pathname-defaults*)
                                     (default-version :newest) )
  (setq defaults (pathname defaults))

  (let ((*default-pathname-defaults* defaults))
    (setq pathname (pathname pathname)) )

  (let ((host      (ref pathname host      pathname))
        (device    (ref pathname device    pathname))
        (directory (ref pathname directory pathname))
        (name      (ref pathname name      pathname))
        (type      (ref pathname type      pathname))
        (version   (ref pathname version   pathname)) )

    ;; host
    ;;
    (unless (eq (type-of host) (type-of (ref pathname host defaults)))
      (pathname-not-matched pathname defaults) )

    ;; device
    ;;
    (unless device
      (setq device (if (eq (ref pathname host defaults) host)
                       (ref pathname device defaults)
                     (ref basic-host default-device host) )))

    ;; direcotry
    ;;
    (let ((default-directory (ref pathname directory defaults)))
      (cond
        ((not directory)
          (setq directory default-directory) )

        ((and (consp directory)
              (eq :relative (first directory))
              (consp default-directory) )
          (let ((scan (append (rest default-directory) (rest directory)))
                (results '()) )
            (loop
              (when (null scan)
                (setq directory (cons (first default-directory)
                                      (nreverse results) ))
                (return) )
              (let ((name (pop scan)))
                (if (eq :back (first scan))
                    (setq scan (rest scan))
                  (push name results) ) )) ) )) )

    ;; name & version
    ;;
    (if name
        (unless version (setq version default-version))
      (progn
        (setq name (ref pathname name defaults))
        (unless version
          (setq version (ref pathname version defaults)) )))

    ;; type
    (unless type
      (setq type (ref pathname type defaults)) )

    (make-pathname-using-host host device directory name type version) ) )


;;;; 19.4.12 parse-namestring
;;;
;;; Syntax:
;;;     parse-namestring thing &optional host defaults
;;;                            &key start end junk-allowed
;;;         => pathname, position
;;;
;;; Note:
;;;  defaults is used only for default value of host.
;
(defun cl:parse-namestring (thing
                            &optional host
                                     (defaults *default-pathname-defaults*)
                            &key     (start 0)
                                     end
                                     junk-allowed )
  (check-type thing (or string pathname stream))

  (setq defaults (pathname defaults))

  (etypecase host
    (basic-host)
    (string (setq host (find-pathname-host host)))
    (null
      (let ((colon (and (stringp thing)
                        (position #\: thing :start start :end end) )))
        (when colon
          (setq host (find-pathname-host (subseq thing start colon) nil))
          (when host (setq start (1+ colon))) )

      (unless host
        (setq host (ref pathname host defaults)) ) ) ))

  (when (streamp thing)
    (setq thing (pathname thing)) )

  (when (pathnamep thing)
    (unless (eq (ref pathname host thing) host)
      (error "~S must be ~S." host (ref pathname host thing)) )
    (return-from parse-namestring (values thing start)) )

  (setq end (ensure-bounding-indexes thing start end))

  (multiple-value-bind (pathname index)
      (parse-namestring-using-host host thing start end)
    (unless junk-allowed
      (when (or (not pathname) (/= index end))
        (error 'pathname-parse-error
               :host     host
               :string   (subseq thing start end)
               :position index )))
    (values pathname index) ) )


;;;; 19.4.3 pathname
;
#+nil
(progn
  (defmethod cl:pathname ((pathname pathname))
      (declare (values pathname))
    pathname )

  (defmethod cl:pathname ((stream stream))
    (stream-pathname stream) )

  (defmethod cl:pathname ((pathname string))
      (declare (values pathname))
    (values (parse-namestring pathspec)) ) )

(defun cl:pathname (pathspec)
    (declare (values pathname))
  (etypecase pathspec
    (pathname   pathspec)
    (string     (values (parse-namestring pathspec)))
    (stream     (stream-pathname pathspec)) ) )


;;;; 19.4.6 pathname-device
;;;; 19.4.6 pathname-directory
;;;; 19.4.6 pathname-name
;;;; 19.4.6 pathname-type
;;;; 19.4.6 pathname-version
;
(macrolet (
    (define-reader (component)
      (let ((fn-name   (intern (format nil "PATHNAME-~A" component))))
        `(defun ,fn-name (pathname &key (case :local))
           (setq pathname (pathname pathname))
           (ecase case
             (:local  (ref pathname ,component pathname))
             (:common
                (host-convert-case (ref pathname host pathname)
                                   (ref pathname ,component pathname) ))) ) ) )
    )
    ;;
    (define-reader device)
    (define-reader directory)
    (define-reader name)
    (define-reader type)
    (define-reader version) )


;;;; 19.4.6 pathname-host
;;;
;
(defun cl:pathname-host (pathname &key (case :local))
  (setq pathname (pathname pathname))
  (let ((name (ref basic-host name (ref pathname host pathname))))
    (ecase case
      (:local  name)
      (:common (host-convert-case name)) ) ) )


;;;; 19.4.14 pathname-match-p
;;;
;;; Syntax:
;;;     pathname-match-p pathname wildcard => generalize-boolean
;
(defun cl:pathname-match-p (pathname wildcard)
    (setq pathname (pathname pathname))
    (setq wildcard (pathname wildcard))
  (values (pathname-match-p-using-host
             (ref pathname host pathname) pathname wildcard) ) )


;;;; 19.4.5 pathnamep
;
(defun cl:pathnamep (object)
  (si::subclassp (class-of object) (find-class 'pathname)) )


;;;; 19.4.16 translate-pathname
;;;
;;; Note: host of pathname and from-wildcard must be eq.
;;; Note: Type of host of to-wildcard may be different from pathname's host.
;
(defun cl:translate-pathname (pathname from-wildcard to-wildcard &key)
  (setq pathname      (pathname pathname))
  (setq from-wildcard (pathname from-wildcard))
  (setq to-wildcard   (pathname to-wildcard))

  (translate-pathname-using-host
    (ref pathname host pathname) pathname from-wildcard to-wildcard) )


;;;; 19.4.13 wild-pathname-p
;
(defun cl:wild-pathname-p (pathname &optional field-key)
      (setq pathname (pathname pathname))
      (check-type field-key (member :host :device :directory :name :type
                                    :version nil ))

  (let ((host (ref pathname host pathname)))
    (case field-key
      ((nil)
        (or (wild-pathname-p-using-host host pathname :device)
            (wild-pathname-p-using-host host pathname :directory)
            (wild-pathname-p-using-host host pathname :name)
            (wild-pathname-p-using-host host pathname :type)
            (wild-pathname-p-using-host host pathname :version) ) )

      ((:host) nil)
      (otherwise
        (wild-pathname-p-using-host host pathname field-key)) ) ) )
