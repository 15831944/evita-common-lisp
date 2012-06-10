;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 19 Filenames - Windows File System
;;; lisp/runtime/r19-windows.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r19-windows.lisp#5 $
;;;
;;; Description:
;;;  This fils contains pathname functions for Windows file system.
;;;     make-windows-host
;;;     windows-drive-name-p
;;;     windows-match-device-p
;;;     windows-match-directory-p
;;;     windows-match-component-p
;;;     windows-parse-component
;;;     windows-translate-component
;;;     windows-unparse-directory
;;;     windows-unparse-element
;;;
;;; Generic Functions:
;;;     make-pathname-using-host
;;;     internal-pathname-device-match-p
;;;     internal-pathname-directory-match-p
;;;     pathname-equal-using-host-p
;;;     internal-pathname-name-match-p
;;;     internal-pathname-type-match-p
;;;     internal-pathname-version-match-p
;;;     truename-using-host
;;;     wild-pathname-p-using-host
;
(in-package :si)

(declaim (ftype (function (string) windows-host)
  intern-windows-pathname-host ) )

(declaim (ftype (function (string) logical-host)
  make-windows-host ) )

(declaim (ftype (function (t) t)
  windows-drive-name-p ) )

(declaim (ftype (function (t t) (values t list))
  windows-match-component-p ) )

(declaim (ftype (function (t t) t)
  windows-match-device-p ) )

(declaim (ftype (function (t t) (values t list))
  windows-match-directory-p ) )

(declaim (ftype (function (windows-host t t) t)
  windows-parse-component ) )

(declaim (ftype (function (cons list) simple-string)
  windows-translate-component ) )

(declaim (ftype (function (t stream) unspecified)
  windows-unparse-directory ) )

(declaim (ftype (function (t stream) unspecified)
  windows-unparse-element ) )


;;;; intern-windows-pathname-host
(defun intern-windows-pathname-host (name)
    (declare (type string name))
    (declare (values windows-host))
  (let ((host (intern-pathname-host name #'make-windows-host)))
    (check-type host windows-host)
    host ) )


;;;; make-windows-host
;
(defun make-windows-host (name)
    (declare (type string name))
    (declare (values windows-host))
 (let ((host (make-instance 'windows-host)))
    (setf (ref basic-host name  host)          name)
    (setf (ref basic-host local-case host)     :preserved)
    (setf (ref basic-host customary-case host) :downcase)
    (setf (ref basic-host default-device host) nil)
    host ) )


;;;; windows-drive-name-p
;;;
;;; Called by:
;;;   internal-enough-namestring
;;;   internal-name-string
;;;   parse-device-component
;;;   truename-using-host
;;;
;;; Description:
;;;  Returns true when given name has Windows drive syntax, alphabet follwed
;;;  by colon(:), e.g. A:, C:, and so on.
;
(defun windows-drive-name-p (name)
  (and (stringp name)
       (= 2 (length name))
       (let ((drive (char name 0)))
         (or (char<= #\A drive #\Z) (char<= #\a drive #\z)) )
       (eql #\: (char name 1))) )


;;;; windows-match-component-p
;;;
;;; Syntax:
;;;   windows-match-component-p component pattern => boolean, matched-strings
;;;
;;; Arguments and Values:
;;;   component a string, list, :wild, nil, :unspecific
;;;   pattern   a string, list, :wild, nil, :unspecific
;;;
;;; Called by:
;;;   pathname-match-p-using-host
;
(defun windows-match-component-p (component pattern)
    (declare (values t list))
  (let ((mstrings '())
        (mstart   nil) )
    (labels (
      (end-wild-match (string mend)
        (when mstart
          (push (subseq string mstart mend) mstrings) )
        (setq mstart nil) )

      (match (string pattern index end)
        (loop
          (when (null pattern)
            (when (= index end)
              (end-wild-match string index)
              (return t) )
            (return nil) )

          (let ((pat (pop pattern)))
            (etypecase pat
              (string
                (end-wild-match string index)
                (let ((nindex (string-not-equal string pat :start1 index)))
                  (cond
                    ((not nindex)
                      (setq index end) )
                    ((/= nindex (+ index (length pat)))
                      (return-from match nil) )
                    (t
                      (setq index nindex) )) ) )

              ((eql :wild-1)
               (when (= index end)
                 (return nil) )

                (end-wild-match string index)
                (push (subseq string index (1+ index)) mstrings)
                (incf index) )

              ((eql :wild)
                (end-wild-match string index)
                (when (= index end)
                  (push "" mstrings)
                  (return t) )
                (let ((aindex         index)
                      (saved-mstrings mstrings) )
                  (setq index end)
                  (loop
                    (when (< index aindex)
                      (return-from match nil) )

                    (setq mstart   aindex)
                    (setq mstrings saved-mstrings)

                    (when (match string pattern index end)
                      (return-from match t) )

                    (decf index) ) ) )) )) )
    )
    ;;
    ;; windows-match-component-p
    ;;
    (etypecase pattern
      ((or null (eql :wild))
        (values t (list component)) )

      ((eql :unspecific)
        (values (eq component :unspecific) '()) )

      (string
        (values (and (stringp component)
                     (string-equal component pattern) )
                '() ) )

      (cons
        (if (and (stringp component)
                 (match component pattern 0 (length component)) )
            (values t (nreverse mstrings))
          (values nil '()) ) ))) ) )


;;;; windows-match-device-p
;;;
;;; Called by:
;;;  pathname-match-p-using-host
;;;  translate-pathname-using-host
;
(defun windows-match-device-p (device pattern)
  (if (not (stringp device))
      (not (stringp pattern))
    (if (stringp pattern)
        (string-equal device pattern)
      t )) )


;;;; windows-match-directory-p
;;;
;;; Syntax:
;;;   windows-match-directory-p component pattern => boolean, matched-strings
;;;
;;; Arguments and Values:
;;;   component a string, list, :wild, nil, :unspecific
;;;   pattern   a string, list, :wild, nil, :unspecific
;;;
;;; Called by:
;;;  pathname-match-p-using-host
;
(defun windows-match-directory-p (component pattern)
  (let ((mstrings '())
        (mstart   nil) )
    (labels (
      (end-wild-match (string mend)
        (when mstart
          (push (subseq string mstart mend) mstrings) )
        (setq mstart nil) )

      (match (string pattern index end)
        (loop
          (when (null pattern)
            (when (= index end)
              (end-wild-match string index)
              (return t) )
            (return nil) )

          (let ((pat (pop pattern)))
            (etypecase pat
              (string
                (when (= index end)
                  (return nil) )

                (end-wild-match string index)
                (unless (windows-match-component-p (nth index string) pat)
                  (return nil) )
                (incf index) )

              (cons
                (when (= index end)
                  (return nil) )

                (end-wild-match string index)
                (multiple-value-bind (match-p matched)
                    (windows-match-component-p (nth index string) pat)
                  (unless match-p
                    (return nil) )
                  (push matched mstrings)
                  (incf index) ) )

              ((eql :wild)
                (when (= index end)
                  (return nil) )

                (end-wild-match string index)
                (push (subseq string index (1+ index)) mstrings)
                (incf index) )

              ((eql :wild-inferiors)
                (end-wild-match string index)
                (when (= index end)
                  (push '() mstrings)
                  (return t) )
                (let ((aindex         index)
                      (saved-mstrings mstrings) )
                  (setq index end)
                  (loop
                    (when (< index aindex)
                      (return-from match nil) )

                    (setq mstart   aindex)
                    (setq mstrings saved-mstrings)

                    (when (match string pattern index end)
                      (return-from match t) )

                    (decf index) ) ) )) )) )
    )
    ;;
    ;; windows-match-directory-p
    ;;
    (if (not (consp component))
        (values (eq component pattern) '())
      (etypecase pattern
        ((or null (eql :wild) (eql :unspecific))
          (values t (list (rest component))) )

        (string
          (values (string-equal component pattern) '()) )

        (cons
          (cond
            ((not (eq (pop component) (pop pattern)))
              (values nil '()) )
            ((match component pattern 0 (length component))
              (values t (nreverse mstrings)) )
            (t
              (values nil '()) )) ))) ) ) )


;;;; windows-parse-component
;;;
;;; Called by:
;;;  parse-directory-component
;;;  parse-name-component
;;;  parse-type-component
;;;
;;; Description:
;;;  Parses pathanme component for Windows.
;
(defun windows-parse-component (host name component)
    (declare (type windows-host host))
  (cond
    ((eq nil         name) name)
    ((eq :wild       name) name)
    ((eq :unspecific name) name)
    ((not (stringp name))
      (error 'bad-pathname-component
             :host      host
             :component component
             :datum     name ) )
    ((string= "*" name) :wild)
    ((string= "?" name) :wild-1)
    (t
      (let ((elts   '())
            (last   0)
            (length (length name)) )
        (dotimes (index length)
          (let ((char (char name index)))
            (cond
              ((or (< (char-code char) #x20)
                   (position char +windows-reserved-chars+) )
                (error 'bad-pathname-character
                       :host      host
                       :component component
                       :datum     char ) )

              ((eql #\* char)
                (unless (= last index)
                  (push (subseq name last index) elts)
                (push ':wild-any elts)
                (setq last (1+ index)) ) )

              ((eql #\? char)
                (unless (= last index)
                  (push (subseq name last index) elts) )
                (push ':wild-1 elts)
                (setq last (1+ index)) ) ) ) )
        (if (null elts)
            name
          (progn
            (unless (= last length)
              (push (subseq name last length) elts) )
            (nreverse elts) )) ) )) )


;;;; windows-translate-component
;;;
;;; Syntax:
;;;   windows-translate-component host elts parts => component
;;;
;;; Arguments and Values:
;;;   host      a windows-host.
;;;   elts      a list of string, :wild-1 or :wild.
;;;   parts     a list of string.
;;;   component a string.
;;;
;;; Called by:
;;;  translate-component-using-host
;;;  translate-directory-using-host
;
(defun windows-translate-component (pats parts)
    (declare (type cons pats))
    (declare (type list parts))
  (let ((elts '()))
    (dolist (pat pats)
      (when (or (eq :wild pat) (eq :wild-1 pat))
        (setq pat (pop parts)) )
      (push pat elts) )
    (setq elts (nreverse elts))
    (with-output-to-string (stream)
      (dolist (pat elts)
        (when pat (write-string pat stream)) )) ) )


;;;; windows-unparse-directory
;;;
;
(defun windows-unparse-directory (directory stream)
  (when (consp directory)
    (ecase (pop directory)
      (:absolute (write-char #\/ stream))
      (:relative) )
    (dolist (elt directory)
      (case elt
        (:back
          (write-string ".." stream) )
       (:wild-inferiors
         (write-string "**" stream) )
       (otherwise
         (windows-unparse-element elt stream) ))
       (write-char #\/ stream) )) )


;;;; windows-unparse-element
;;;
;;; Called by:
;;;     internal-directory-namestring
;;;     internal-file-namestring
;
(defun windows-unparse-element (elt stream)
    (declare (type stream stream))
  (cond
   ((stringp elt)    (write-string elt stream))
   ((eq :wild elt)   (write-char #\* stream))
   ((eq :wild-1 elt) (write-char #\? stream))
   ((consp elt)
     (dolist (elt-2 elt)
       (cond
         ((stringp elt-2)    (write-string elt-2 stream))
         ((eq :wild   elt-2) (write-char #\* stream))
         ((eq :wild-1 elt-2) (write-char #\? stream)) ) ) )) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Methods
;;;;

;;;; internal-directory-namestring
;
(defmethod internal-directory-namestring ((pathname windows-pathname) stream)
    (declare (type windows-pathname pathname))
    (declare (type stream stream))
  (windows-unparse-directory (ref pathname directory pathname) stream) )


;;;; internal-enough-namestring
;;;
;;; Note: type name must be followed by name and dot(.).
;
(defmethod internal-enough-namestring ((pathname windows-pathname)
                                       defaults stream )
    (declare (type windows-pathname pathname))
    (declare (type pathname defaults))
    (declare (type stream stream))

  (setq defaults (translate-logical-pathname defaults))

  (unless (eq (ref pathname host pathname) (ref pathname host defaults))
    (return-from internal-enough-namestring (namestring pathname)) )

  ;; drive name or share name
  ;;
  (let ((device-1 (ref pathname device pathname)))
    (when (stringp device-1)
      (let ((device-2 (ref pathname device defaults)))
        (unless (and (stringp device-2) (string-equal device-1 device-2))
          (if (windows-drive-name-p device-1)
              (write-string device-1 stream)
            (progn
              (write-string "//" stream)
              (host-name (ref pathname host pathname))
              (write-char #\/ stream)
              (write-string device-1 stream) ))) )) )

  ;; directory
  ;;
  (let ((dir-1 (ref pathname directory pathname)))
    (when (consp dir-1)
      (let ((dir-2 (ref pathname directory defaults)))
        (if (not (consp dir-2))
            (windows-unparse-directory dir-1 stream)
          (let ((scan-1 dir-1)
                (scan-2 dir-2) )
            (loop
              (unless (equalp (car scan-1) (car scan-2))
                (cond
                  ((not (null scan-2))
                    ;; dir-2 is longer than dir-1.
                    (setq scan-1 dir-1) )
                  ((not (eq scan-1 dir-1))
                    (setq scan-1 (cons :relative scan-1)) ))
                (windows-unparse-directory scan-1 stream)
                (return) )

              ;; dir-1 and dir-2 are equal.
              (when (null scan-1)
                (return) )

              ;; go next element
              (setq scan-1 (cdr scan-1))
              (setq scan-2 (cdr scan-2)) ) )) )) )

  ;; name and type
  ;;
  (let* ((name-1      (ref pathname name pathname))
         (name-2      (ref pathname name defaults))
         (same-name-p (equalp name-1 name-2)) )
    (unless same-name-p
      (windows-unparse-element name-1 stream) )

    (let ((type-1 (ref pathname type pathname)))
      (unless (or (eq nil type-1) (eq :unspecific type-1))
        (let ((type-2 (ref pathname type defaults)))
          (unless (equalp type-1 type-2)
            (when same-name-p
              (windows-unparse-element name-1 stream) )
            (write-char #\. stream)
            (windows-unparse-element type-1 stream)) )) ) ) )


;;;; internal-file-namestring
;
(defmethod internal-file-namestring ((pathname windows-pathname) stream)
    (declare (type stream stream))
  (windows-unparse-element (ref pathname name pathname) stream)
  (let ((type (ref pathname type pathname)))
    (unless (or (eq nil type) (eq :unspecific type))
      (write-char #\. stream)
      (windows-unparse-element type stream) ) ) )


;;;; internal-namestring
;;;
;;; Called by:
;;;     print-pathname
;
(defmethod internal-namestring ((pathname windows-pathname) stream)
    (declare (type stream stream))
  (let ((device (ref pathname device pathname)))
    (when (stringp device)
      (unless (windows-drive-name-p device)
        (write-string "//" stream)
        (write-string (host-name (ref pathname host pathname)) stream)
        (write-char #\/ stream) )
      (write-string device stream) ) )
  (internal-directory-namestring pathname stream)
  (internal-file-namestring pathname stream) )


;;;; make-pathname-using-host
;
(defmethod make-pathname-using-host ((host windows-host)
                                   device directory name type version )
    (declare (ignore version))
  (let ((pathname (make-instance 'windows-pathname)))
    (setf (ref pathname host pathname)      host)
    (setf (ref pathname device pathname)    device)
    (setf (ref pathname directory pathname) directory)
    (setf (ref pathname name pathname)      name)
    (setf (ref pathname type pathname)      type)
    (setf (ref pathname version pathname)   :unspecific)
    pathname ) )


;;;; parse-device-component
;;;
;;; Called by:
;;;  make-pathname
;;;
;;; Description:
;;;  Parses wild name.
;
(defmethod parse-device-component ((host windows-host) device)
  (cond
    ((not device) device)
    ((eq :unspecific device) device)
    ((windows-drive-name-p device) device)
    ((stringp device)
      (dotimes (i (length device))
        (when (position (char device i) +windows-reserved-chars+)
          (error 'bad-pathname-character
                 :host      host
                 :component :device
                 :datum     (char device i) )) )
      device )
    (t
      (error 'bad-pathname-component
             :host      host
             :component :device
             :datum     device ) )) )


;;;; parse-directory-component
;;;
;;; Called by:
;;;  make-pathname
;;;
;;; Description:
;;;  Parses wild name.
;
(defmethod parse-directory-component ((host windows-host) directory)
  (cond
    ((not directory) directory)
    ((eq :unspecific directory) directory)
    ((eq :wild directory)
      (list :absolute :wild-inferiors) )
    ((stringp directory)
      (cond
        ((string= "."  directory) (list :relative))
        ((string= ".." directory) (list :relative :back))
        (t (list :relative directory) )) )
    ((not (consp directory))
      (error 'bad-pathname-component
             :host      host
             :component :directory
             :datum     directory ) )
    ((or (eq :absolute (first directory))
         (eq :relative (first directory)) )
      (let ((elts '()))
        (dolist (elt (rest directory))
          (typecase elt
            (string
              (push (windows-parse-component host elt :directory) elts) )

            ((eql :wild-inferiors)
              (unless elts
                (error 'bad-pathname-component
                       :host        host
                       :component   :directory
                       :datum       directory ))
              (push elt elts) )

            ((member :back :up :wild)
              (push elt elts) )

            (otherwise
              (error 'bad-pathname-component
                     :host      host
                     :component :directory
                     :datum     directory ) )) )
        (cons (first directory) (nreverse elts)) ) )
    (t
      (error 'bad-pathname-component
             :host      host
             :component :directory
             :datum     directory ) )) )


;;;; parse-name-component
;;;
;;; Called by:
;;;     make-pathname
;;;
;;; Description:
;;;  Parses wild name.
;
(defmethod parse-name-component ((host windows-host) name)
  (windows-parse-component host name :name) )


;;;; parse-namestring-using-host
;;;
;;; Syntax:
;;;     parse-namestring-using-host hsot string start end => pathname, index
;;;
;;; Called by:
;;;     parse-namestring
;;;
;;; BUGBUG: We should rewrite this method for simpler or straight.
;;;
;
(defmethod parse-namestring-using-host ((host windows-host) string start end)
    (declare (type string string))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
    (declare (values pathname sequence-index))
  (setq end (ensure-bounding-indexes string start end))
  (let ((device     nil)
        (index      start)
        char )
    (labels (
      ;; Get character from source string
      ;;
      (getch ()
        (if (>= index end)
            nil
          (let ((char (char string index)))
            (incf index)
            (cond
              ((char= #\/ char) char)
              ((char= #\\ char) char)
              ((char< char #\Space)
                (decf index)
                nil )
              ((position char +windows-reserved-chars+)
                (decf index)
                nil )
              (t char) ) )) )

      ;; Parse element string
      ;;
      (parse (string start end)
          (assert (<= start end))
        (let ((index  start)
              (nchars 0)
              (strings '()) )
          (loop
            (when (= index end) (return))
            (let ((char (char string index)))
              (case char
                ((#\*)
                  (when (plusp nchars)
                    (push (subseq string (- index nchars) index)
                          strings ) )
                  ;; "**" => error
                  (when (eq :wild (first strings))
                    (return-from parse-namestring-using-host
                                 (values nil (1+ index)) ) )
                  (push :wild strings)
                  (setq nchars 0) )

                ((#\?)
                  (when (plusp nchars)
                    (push (subseq string (- index nchars) index)
                          strings ) )
                  (push :wild-1 strings)
                  (setq nchars 0) )

                (otherwise
                  (incf nchars) ))
               (incf index) ))

         (assert (= index end))
         (cond
           ((zerop nchars)
             (if (rest strings)
                 (setq strings (nreverse strings))
               (setq strings (first strings)) ) )
           ((null strings)
             (setq strings (subseq string (- index nchars) index)) )
           (t
             (push (subseq string (- index nchars) index) strings)
             (setq strings (nreverse strings)) ))
         strings ) )

      ;; Back source pointer.
      (ungetch ()
        (assert (> index start))
        (decf index) )
      )
      ;;
      ;; parse-namestring-using-host
      ;;
      (setq char (getch))

      ;; Empty string
      ;;
      (when (null char)
        (return-from parse-namestring-using-host
          (values (make-pathname :host host) index) ))

      (cond
        ;; [A-Za-z] ':'
        ;;
        ((or (char<= #\A char #\Z) (char<= #\a char #\z))
          (let ((char-2 (if (>= index end)
                            nil
                          (prog1
                            (char string index)
                            (incf index) ))) )
            (cond
              ;; "a"
              ((not char-2)
                (return-from parse-namestring-using-host
                  (values (make-pathname :host host :name (string char))
                          index )) )

              ((char/= #\: char-2)
                (ungetch) )

              (t
                (setq device (subseq string (- index 2) index))
                (setq char (getch))
                ;; "a:"
                (unless char
                  (return-from parse-namestring-using-host
                    (values (make-pathname :host host :device device)
                            index ))) )) ) )

        ;; '//' host '/' share
        ;; '/' (elt '/')* name
        ((or (char= #\/ char) (char= #\\ char))
          (setq char (getch))

          (unless char
            (return-from parse-namestring-using-host
              (values (make-pathname :host host :directory '(:absolute))
                      index )))

          (if (not (or (char= #\/ char) (char= #\\ char)))
              ;; '/' (elt '/')* name
              (progn
                (ungetch)
                (setq char #\/) )

            ;; '//' host '/' share
            (progn
              (let ((start index))
                (loop
                  (setq char (getch))

                  (unless char
                    (return-from parse-namestring-using-host
                                 (values nil index) ) )

                  (when (or (char= #\/ char) (char= #\\ char))
                    (return) ))

                 (setq host (subseq string start (1- index)))
                 (setq host (make-windows-host host)) )

              (let ((start index))
                (loop
                  (setq char (getch))

                  (unless char
                    (setq device (subseq string start index))
                    (return-from parse-namestring-using-host
                      (values (make-pathname :host   host
                                             :device device )
                              index )))

                  (when (or  (char= #\/ char) (char= #\\ char))
                    (return) ))
                (setq device (subseq string start (1- index))) ))) ))

    ;; directory
    ;;
    (let ((nchars    0)
          (elts      '())
          (dot-pos   nil)
          (directory '())
          (dirhead   :relative)
          (name      nil)
          (type      nil) )
      (when (or (char= #\/ char) (char= #\\ char))
        (setq dirhead :absolute)
        (setq char (getch)) )

      (loop
        (unless char (return))

        (case char
          ((#\/ #\\)
            ;; Double slash?
            (when (zerop nchars)
              (return-from parse-namestring-using-host (values nil index)) ) 

            (push nchars elts)
            (setq nchars 0)
            (setq dot-pos nil) )

          (#\.
            (setq dot-pos (1- index))
            (incf nchars) )

          (otherwise
            (incf nchars) ))

        (setq char (getch)) )

      (setq end index)  ; remember position where parse stopped.

      ;; index points char after slash(/).

      ;; name '.' type
      ;;
      (cond
        ((zerop nchars))                ; namesting ends with slash(/)
        ((and (= 1 nchars) dot-pos))    ; the last elt is dor(.).
        (t
          (if dot-pos
              (setq type (parse string (1+ dot-pos) index))
            (setq dot-pos index) )
          (decf index nchars)
          (setq name (parse string index dot-pos)) ))

      ;; subdirs
      ;;
      (dolist (nchars elts)
        (decf index (1+ nchars))
        (cond
          ;; "." => ignore
          ((and (= 1 nchars) (char= #\. (char string index)))
            )

          ;; "*" => :wild
          ((and (= 1 nchars) (char= #\* (char string index)))
            (push :wild directory) )

          ;; ".." => :back
          ((and (= 2 nchars) (char= #\. (char string index))
                             (char= #\. (char string (1+ index))) )
            (push :back directory) )

          ;; "**" => :wild-inferiors
          ((and (= 2 nchars) (char= #\* (char string index))
                             (char= #\* (char string (1+ index))) )
            (push :wild-inferiors directory) )

          ;; name '/' '..' => ''
          ((eq :back (car directory))
            (pop directory) )

          ;; otherwise
          (t
           (push (parse string index (+ index nchars)) directory) )) )

      (when (or directory (eq :absolute dirhead))
        (setq directory (cons dirhead directory)) )

    ;; Return values
    (values (make-pathname-using-host host device directory name type nil)
            end ) ) ) ) )


;;;; parse-type-component
;;;
;;; Called by:
;;;  make-pathname
;;;
;;; Description:
;;;  Parses wild name.
;
(defmethod parse-type-component ((host windows-host) type)
  (windows-parse-component host type :type) )


;;;; parse-version-component
;;;
;;; Called by:
;;;  make-pathname
;;;
;;; Description:
;;;  Parses wild name.
;
(defmethod parse-version-component ((host windows-host) version)
  (if (member version '(:newest :unspecific nil) :test #'eq)
      version
    (error 'bad-pathname-component
           :host        host
           :component   :version
           :datum       version )) )


;;;; pathname-equal-using-host
;;;
;;; Called by:
;;;  pathname-equal
;
(defmethod pathname-equal-using-host ((host windows-host) x y)
  (and (eq    (ref pathname host     x) (ref pathname host     y))
       (equalp (pathname-device    x) (pathname-device    y))
       (equalp (pathname-directory x) (pathname-directory y))
       (equalp (pathname-name      x) (pathname-name      y))
       (equalp (pathname-type      x) (pathname-type      y)) ) )


;;;; pathname-match-p-using-host
;;;
;;; Called by:
;;;  pathname-match-p
;
(defmethod pathname-match-p-using-host ((host windows-host) pathname wildcard)
    (declare (type windows-pathname wildcard))
  (and
    (eq host (ref pathname host wildcard))
    (windows-match-device-p    (ref pathname device pathname)
                               (ref pathname device wildcard) )

    (windows-match-directory-p (ref pathname directory pathname)
                               (ref pathname directory wildcard) )

    (windows-match-component-p (ref pathname name pathname)
                               (ref pathname name wildcard) )

    (windows-match-component-p (ref pathname type pathname)
                               (ref pathname type wildcard) )) )


;;;; translate-component-using-host
;;;
;;; Syntax:
;;;   translate-component-using-host host elts parts => component
;;;
;;; Arguments and Values:
;;;   host      a windows-host.
;;;   elts      a list of string, :wild-1 or :wild.
;;;   parts     a list of string.
;;;   component a string.
;;;
;;; Called by:
;;;  translate-pathname-using-host
;
(defmethod translate-component-using-host ((host windows-host) elts parts)
  (windows-translate-component elts parts) )


;;;; translate-directory-using-host
;;;
;;; Called by:
;;;   translate-pathname-using-host
;;;
;;; Description:
;;;  Replaces wildcard elements with parts.
;;;
;;; Note: When replaces :wild with part from :wild-inferiors, the result
;;; takes only the first element only.
;
(defmethod translate-directory-using-host ((host windows-host) pat-dir parts)
  (let ((dir '()))
    (dolist (pat pat-dir)
      (cond
        ((consp pat)
          (push (windows-translate-component pat (pop parts)) dir) )

        ((eq :wild-inferiors pat)
          (setq dir (nreconc (pop parts) dir)) )

        ((eq :wild pat)
          (let ((part (pop parts)))
            (when (consp part)
              (setq part (first part)) )
            (push part dir) ) )

        ((eq :wild-1 pat)
          (let ((part (pop parts)))
            (when (consp part)
              (setq part (first part)) )
            (push (subseq part 0 1) dir) ) )

        (t
          (push pat dir) )) )
    (nreverse dir) ) )


;;;; translate-pathname-using-host
;;;
;;; Note: signal pathname-not-matched when pathname doesn't match to
;;; from-wildcard.
;
(defmethod translate-pathname-using-host
        ((host-1 windows-host) pathname from-wildcard to-wildcard)
    (declare (type pathname from-wildcard))
    (declare (type pathname to-wildcard))
  (let ((host-1    (ref pathname host      pathname))
        (device-1  (ref pathname device    pathname))
        (dir-1     (ref pathname directory pathname))
        (name-1    (ref pathname name      pathname))
        (type-1    (ref pathname type      pathname))
        (version-1 (ref pathname version pathname))
        dir-parts
        name-parts
        type-parts )

    ;; host
    ;;
    (unless (eq host-1 (ref pathname host from-wildcard))
      (pathname-not-matched pathname from-wildcard) )

    ;; device
    ;;
    (unless (windows-match-device-p device-1 (ref pathname device from-wildcard))
      (pathname-not-matched pathname from-wildcard) )

    ;; directory
    ;;
    (multiple-value-bind (match-p matched)
        (windows-match-directory-p dir-1 (ref pathname directory from-wildcard))
      (unless match-p
        (pathname-not-matched pathname from-wildcard) )

      (setq dir-parts matched) )

    ;; name
    ;;
    (multiple-value-bind (match-p matched)
        (windows-match-component-p name-1 (ref pathname name from-wildcard))
      (unless match-p
        (pathname-not-matched pathname from-wildcard) )

      (setq name-parts matched) )

    ;; type
    ;;
    (multiple-value-bind (match-p matched)
        (windows-match-component-p type-1 (ref pathname type from-wildcard))
      (unless match-p
        (pathname-not-matched pathname from-wildcard) )

      (setq type-parts matched) )

    ;; Translate
    ;;
    (let* ((host-2    (ref pathname host      to-wildcard))
           (device-2  (ref pathname device    to-wildcard))
           (dir-2     (ref pathname directory to-wildcard))
           (name-2    (ref pathname name      to-wildcard))
           (type-2    (ref pathname type      to-wildcard))
           (version-2 (ref pathname version   to-wildcard))
           (convert-p (not (eq (ref basic-host customary-case host-1)
                                (ref basic-host customary-case host-2) ))) )

      ;; device
      ;;
      (unless (or (stringp device-2) (eq :unspecific device-2))
        (when convert-p
          (setq device-1 (host-convert-case host-1 device-1)) )

        (setq device-2 device-1) )

      ;; directory
      ;;
      (cond
        ((or (eq :wild dir-2) (eq nil dir-2))
          (when convert-p
            (setq dir-1 (host-convert-case host-1 dir-1))
            (setq dir-1 (host-convert-case host-2 dir-1)) )

          (setq dir-2 dir-1) )

        ((consp dir-2)
          (when convert-p
            (setq dir-parts (host-convert-case host-1 dir-parts))
            (setq dir-parts (host-convert-case host-2 dir-parts)) )

          (setq dir-2
            (translate-directory-using-host host-2 dir-2 dir-parts) ) ))

      ;; name
      ;;
      (cond
        ((or (eq :wild name-2) (eq nil name-2))
          (when convert-p
            (setq name-1 (host-convert-case host-1 name-1))
            (setq name-1 (host-convert-case host-2 name-1)) )

          (setq name-2 name-1) )

        ((consp name-2)
          (when convert-p
            (setq name-parts (host-convert-case host-1 name-parts))
            (setq name-parts (host-convert-case host-2 name-parts)) )

          (setq name-2
            (translate-component-using-host host-2 name-2 name-parts) ) ))

      ;; type
      ;;
      (cond
        ((or (eq :wild type-2) (eq nil type-2))
          (when convert-p
            (setq type-1 (host-convert-case host-1 type-1))
            (setq type-1 (host-convert-case host-2 type-1)) )

          (setq type-2 type-1) )

        ((consp type-2)
          (when convert-p
            (setq type-parts (host-convert-case host-1 type-parts))
            (setq type-parts (host-convert-case host-2 type-parts)) )

          (setq type-2
            (translate-component-using-host host-2 type-2 type-parts) ) ))

      ;; version
      ;;
      (when (or (eq :wild version-2) (eq nil version-2))
        (setq version-2 version-1) )

      ;; Result
      ;;
      (make-pathname-using-host host-2
                                device-2
                                dir-2
                                name-2
                                type-2
                                version-2 ) ) ) )


;;;; truename-using-host
;;;
;;; Called by:
;;;  truename
;
(defmethod truename-using-host ((host windows-host) pathname)
  (let ((host      (ref pathname host      pathname))
        (device    (ref pathname device    pathname))
        (directory (ref pathname directory pathname)) )
     (cond
      ;; No device
      ((null device)
        (setq directory (parse-namestring (.current-directory) host))
        (setq pathname (merge-pathnames pathname directory)) )

      ;; Remote file
      ((not (windows-drive-name-p device))
       #+nil "Remove file is always absolute."
       )

      ;; Relative directory
      ((or (null directory)
           (and (consp directory) (eq :relative (first directory))) )
       (let ((drive (- (char-code (char-upcase (schar device 0))) 64)))
         (setq directory (parse-namestring (.current-directory drive) host)) )
       (setq pathname (merge-pathnames pathname directory)) )) )
  pathname )


;;;; wild-pathname-p-using-host
;;;
;;; Called by:
;;;  wild-pathname-p
;
(defmethod wild-pathname-p-using-host ((host windows-host) pathname field-key)
  (ecase field-key
    ((:type)
      (or (eq :wild   (ref pathname type pathname))
          (eq :wild-1 (ref pathname type pathname))
          (consp (ref pathname type pathname)) ) )

    ((:name)
      (or (eq :wild   (ref pathname name pathname))
          (eq :wild-1 (ref pathname type pathname))
          (consp (ref pathname name pathname)) ) )

    ((:device) nil)
    ((:version) nil)

    ((:directory)
       (let ((directory (ref pathname directory pathname)))
         (and (consp directory)
              (dolist (elt directory nil)
                (when (or (eq :wild elt)
                          (eq :wild-inferiors elt)
                          (consp elt) )
                  (return t) ) )) ) )) )
