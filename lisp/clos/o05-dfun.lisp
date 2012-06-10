;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - Discriminator
;;; lisp/clos/o05-dfun.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o05-dfun.lisp#5 $
;;;
;;; Description:
;;;  This file contains method dispatch related functions.
;;;
;;; Internal Funcitons:
;;;     classd-of
;;;     compute-discriminator
;;;     compute-discriminator-name
;;;     enlarge-method-cache
;;;     initial-discriminator
;;;     make-accessor-cache
;;;     make-discriminator/reader
;;;     make-discriminator/reader-1
;;;     make-discriminator/reader-n
;;;     make-discriminator/writer
;;;     make-discriminatorwriter-1
;;;     make-discriminatorwriter-n
;;;     power-2
;;;     set-generic-function-lambda-list
;;;     update-discriminator
;;;
;;; MOP Functions:
;;;     compute-discriminating-function         MOP
;
(in-package :si)

;;;; *discriminator-constructors*
;;;
;;; Used by:
;;;   make-discriminator/checking
;;;   make-discriminator/dispatch
;;;
;;; Description:
;;;  Discriminator constructor mapping table.
;;;  The key is
;;;     <type> <num-reqs> ['+' | '-'] <num-opts> <num-rest>
;;;
;;;     <type> ::= 'C' -- checking
;;;                'D' -- dispatching
;;;
;;;  The value is constructor funciton, which take two argument, generic-
;;;  function and vector, which elements are method funciton and specializers.
;;;
;;; BUGBUG: NYI: Must be MT-safe.
;
(defvar *discriminator-constructors* (make-hash-table :test #'equal))


;;;; check-arg/accessor
;;;
;;; Called By:
;;;  make-discriminaotr/reader-1
;;;  make-discriminaotr/writer-1
;;;
;;; Description:
;;;  Checks argument for accessor discriminator.
;;;
;;; Note:
;;;  This macro is used at both of compilation and runtime.
;
(defmacro check-arg/accessor (object class)
  `(let* ((classd    (classd-of ,object))
          (hash-code (ref classd hash-code classd)) )
     (cond
       ((zerop hash-code)
         (update-obsolete-instance object)
         0 )
       ((subclassp (ref classd class classd) ,class)
         hash-code )
       (t
         -1 )) ) )


;;;; check-arg/checking
;;;
;;; Used by:
;;;  make-dfun-ctor/checking
;;;
;;; Description:
;;;  Checks an argument in checking discriminator.
;;;
;;; Note: This macro is used during runtime discrimnator compilation.
;
(defmacro check-arg/checking (arg specl has-eql-p)
  `(progn
     ;; Is a argumet an obsolete instance?
     (let ((classd (classd-of ,arg)))
        (when (zerop (ref classd hash-code classd))
           (update-obsolete-instance ,arg)
           (go obsolete-instance) ) )

     ;; Does argument match specialzer?
     ,(if (not has-eql-p)
          `(unless (subclassp (class-of ,arg) ,specl)
            (go not-applicable) )
        `(etypecase ,specl
           (class
             (unless (subclassp (class-of ,arg) ,specl)
               (go not-applicable) ) )
           (eql-specializer
             (unless (eql ,arg (eql-specializer-object ,specl))
             (go not-applicable) ))))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Local Functions
;;;;

;;;; classd-of
;;;
;;; Called by:
;;;   make-dfun-ctor/checking
;;;   make-dfun-ctor/dispatch
;;;
;;; Description:
;;;  Returns class descriptor for specified object.
;;;
;;; Note: In EVM, this function is an intrinsic.
;
#+nil
(defun classd-of (object)
    (declare (type t object))
    (declare (values class-description))
  (typecase (class-of object)
   (standard-class
     (let ((self (ref instance self object)))
       (ref instance classd self) ) )
   (structure-class
     (ref structure-object classd object) )
   (funcallable-standard-class
     (ref funcallable-instance classd object) )
   (otherwise
     (ref built-in-class instance-description (class-of object)) )) )


;;;; compute-discriminator
;;;
;;; Called by:
;;;   initial-discriminator
;
(defun compute-discriminator (gf)
    (declare (values function))
    (declare (type generic-function gf))
  (labels (
    ;; default-method-p
    (default-method-p (method)
        (declare (type method method))
        (declare (values t))
      (and (null (slot-value method 'qualifiers))
           (every (lambda (specl) (eq (find-class 't) specl))
                  (slot-value method 'specializers) )) )

    ;; make-discriminator/no-applicable-method
    (make-discriminator/no-applicable-method (gf)
        (declare (type generic-function gf))
        (declare (values function))
      (let ((dfun
              (lambda (&rest args)
                    (declare (ext:lambda-name
                               (:discriminator no-applicable-method)))
                    (declare (dynamic-extent args))
                  (apply #'no-applicable-method gf args) ) ))
        dfun ) )

    ;; make-no-applicable-method-emf
    ;;
    (make-no-applicable-method-emf (gf)
        (declare (type generic-function gf))
        (declare (values function))
      (let ((emf
              (lambda (&rest args)
                    (declare (ext:lambda-name
                               (:wrapper no-applicable-method)))
                    (declare (dynamic-extent args))
                  (apply #'no-applicable-method gf args) ) ))
        emf ) )

    ;; reader-method-p
    ;;
    (reader-method-p (method)
        (declare (type method method))
        (declare (values boolean))
      (and (typep method 'standard-reader-method)
           (let ((specls (slot-value method 'specializers)))
             (and (null (rest specls))
                  (typep (first specls) 'class) ) )) )

    ;; writer-method-p
    ;;
    (writer-method-p (method)
        (declare (type method method))
        (declare (values boolean))
      (and (typep method 'standard-writer-method)
           (let ((specls (slot-value method 'specializers)))
             (and (eq (find-class 't) (first specls))
                  (typep (second specls) 'class) ) )) )
    )
    ;;
    ;; compute-discriminator
    ;;
    (let* ((param-info      (slot-value gf 'param-info))
           (methods         (slot-value gf 'methods))
           (nmethods        0)
           (nprimaries      0)
           (nauxiliaries    0)
           (primary-1        nil)
           (default-method  nil)
           (eql-methods     '()) )
        (declare (type param-info         param-info))
        (declare (type list               methods))
        (declare (type (unsigned-byte 24) nmethods))
        (declare (type (unsigned-byte 24) nprimaries))
        (declare (type (unsigned-byte 24) nauxiliaries))
        (declare (type (or method null)   primary-1))
        (declare (type (or method null)   default-method))
        (declare (type list               eql-methods))
      ;; Analyze methods
      ;;
      (loop
        for scan on methods
        for method = (first scan) do
          (incf nmethods)

          (when (and (null default-method) (default-method-p method))
            (setq default-method method) )

          (when (some (lambda (specializer)
                           (typep specializer 'eql-specializer) )
                      (slot-value method 'specializers) )
            (push method eql-methods) )

          (if (slot-value method 'qualifiers)
              (incf nauxiliaries)
            (progn
              (incf nprimaries)
              (unless (eq method default-method)
                (setq primary-1 method) ))))

      (cond
        ;; No applicable method
        ((zerop nprimaries)
         (make-discriminator/no-applicable-method gf) )

        ;; Only one applicable method
        ((= nmethods 1)
          (cond
            (default-method
              (slot-value default-method 'function) )

            ((reader-method-p primary-1)
              (make-discriminator/reader gf primary-1) )

            ((writer-method-p primary-1)
              (make-discriminator/writer gf primary-1) )

            (t
              (make-discriminator/checking
                gf
                param-info
                eql-methods
                primary-1
                (make-no-applicable-method-emf gf) ) )) )

        ;; one specialized method + default method
        ((and (= nmethods 2) (= nprimaries 2) default-method)
          (make-discriminator/checking
            gf
            param-info
            eql-methods
            primary-1
            (slot-value default-method 'function) ) )

        ;; Generic case.
        (t
          (make-discriminator/dispatch
            gf
            param-info
            eql-methods
            methods
            (if default-method
                (slot-value default-method 'function)
              (make-no-applicable-method-emf gf) )) )) ) ) )


;;;; compute-discriminator-name
;;;
;;; Called by:
;;;   make-discriminator/checking
;;;   make-discriminator/dispatch
;;;
;;; Description:
;;;  Returns name of discriminator constructor, which is also key of
;;;  discriminator consturctor table.
;;;
;;; BUGBUG: We'll use fixnum after debugging.
;
(defun compute-discriminator-name (type param-info
                                   has-eql-p needs-next-methods-p
                                   specls )
  (list type
        (ref param-info nreqs param-info)
        (ref param-info nopts param-info)
        (and (ref param-info keys param-info) '&rest)
        (mapcar (lambda (x) (if x 1 0)) specls)
        (and has-eql-p 'eql)
        (and needs-next-methods-p 'call-next-method) ) )


;;;; enlarge-method-cache
;;;
;;; Called by:
;;;  make-dfun-ctor/dispatch
;
(defun enlarge-method-cache (old-cache nspecls line-size)
    (declare (type simple-vector old-cache))
    (declare (type (unsigned-byte 16) nspecls))
    (declare (type (unsigned-byte 16) line-size))
    (declare (values simple-vector))
  (loop
    with new-cache  = (make-simple-vector (ash (length old-cache) 1) nil)
    with mask       = (- (length new-cache) line-size)
    with entry-size = (1+ nspecls)
    for old-index from 0 below (length old-cache) by line-size
    finally
      (return new-cache)
    do
      (when (svref old-cache old-index)
        (loop
          with hash-code =
            (loop
              for jndex from old-index below (+ nspecls old-index)
              for classd = (svref old-cache jndex)
              with hash-code = 0
              finally (return hash-code)
              do (incf hash-code (ref classd hash-code classd)) )
          with start = (logand hash-code mask)
          for  index = start then (logand (+ index line-size) mask)
          do
            (unless (svref new-cache index)
              (replace new-cache old-cache
                       :start1 index
                       :end1   (+ index entry-size)
                       :start2 old-index )
              (return) )))) )


;;;; initial-discriminator
;
(defun initial-discriminator (gf &rest args)
    (declare (type generic-function gf))
    (declare (dynamic-extent args))
  (set-funcallable-instance-function
    gf
    (compute-discriminator gf) )
  (apply gf args) )


;;;; make-accessor-cache
;;;
;;; Called by:
;;;  make-discriminator/reader-n
;;;  make-discriminator/writer-n
;
(defun make-accessor-cache (class slot-name)
    (declare (type class class))
    (declare (type symbol slot-name))
    (declare (values simple-vector))
  (labels (
    (collect-locations (class locations)
        (declare (type class class))
        (declare (type list locations))
        (declare (values list))
      (loop for subclass in (slot-value class 'direct-subclasses) do
        (setq locations (collect-locations subclass locations)) )

      (loop for slotd in (slot-value class 'slots) do
        (when (eq slot-name (slot-value slotd 'name))
          (push (cons class (slot-value slotd 'location)) locations)
          (return) ))
      locations )
    )
    ;;
    ;; make-accessor-cache
    ;;
    (loop
      with locations  = (collect-locations class '())
      with cache-size of-type ext:sequence-index =
        (power-2 (* 2 (length locations)))
      with mask of-type ext:sequence-index = (- cache-size 2)
      with cache = (make-simple-vector cache-size nil)
      for (class . location) of-type (class . ext:sequence-index) in locations
      finally (return cache)
      do
        (loop
          with classd    = (slot-value class 'instance-description)
          with hash-code = (ref classd hash-code classd)
          with start     = (logand hash-code mask)
          with index     = start
          do
            (when (null (svref cache index))
              (setf (svref cache index)      classd)
              (setf (svref cache (1+ index)) location)
              (return) )
            (setq index (logand (+ 2 index) mask))
            (assert (not (eq start index))) )) ) )


;;; See o05-cdfun.lisp for
;;;     make-discriminator/checking
;;;     make-discriminator/dispatch

;;;; make-discriminator/reader
;;;
;;; Arguments and Values:
;;;  method     - A standad-reader-method object which has only one class
;;                specializer.
;;;
;;; Called by:
;;;  compute-discriminator
;;;
;;; Description:
;;;  Returns reader discriminator.
;
(defun make-discriminator/reader (gf method)
    (declare (type generic-function gf))
    (declare (type method method))
    (declare (values function))
  (let* ((class     (first (slot-value method 'specializers)))
         (slotd     (slot-value method 'slot-definition))
         (slot-name (slot-value slotd 'name))
         (location  (same-slot-location-p class slot-name))
         (dfun
           (if location
               (make-discriminator/reader-1 gf class location)
             (make-discriminator/reader-n gf class slot-name) ) ))
    (setf (ext:function-name dfun)
          `(:discriminator :reader ,(slot-value gf 'name)) )
    dfun ) )


;;;; make-discriminaotr/reader-1
;;;
;;; Called by:
;;;   compute-discriminator/reader
;;;
;;; Description:
;;;  Returns closure for reading slot of object of a class. A class and its
;;;  all subclasses must have same slot location.
;
(defun make-discriminator/reader-1 (gf class location)
    (declare (type generic-function gf))
    (declare (type class            class))
    (declare (type (or fixnum cons) location))
    (declare (values function))
  (etypecase location
    (fixnum
      (etypecase class
        (standard-class
          (lambda (object)
                (declare (ext:lambda-name
                           (:discriminator :reader-1 :local std) ))
              (let ((index location)
                    (hash-code (check-arg/accessor object class)))
                  (cond
                    ((plusp hash-code)
                      (standard-instance-access object index) )
                    ((minusp hash-code)
                      (no-applicable-method gf object) )
                    (t
                      (initial-discriminator gf object) )) ) ) )

        (funcallable-standard-class
          (lambda (object)
                (declare (ext:lambda-name
                           (:discriminator :reader-1 :local fsc) ))
              (let ((index location)
                    (hash-code (check-arg/accessor object class)))
                (cond
                  ((plusp hash-code)
                    (funcallable-standard-instance-access object index) )
                  ((minusp hash-code)
                    (no-applicable-method gf object) )
                  (t
                    (initial-discriminator gf object) )) ) ) )) )
     (cons
       (lambda (object)
                (declare (ext:lambda-name
                           (:discriminator :reader :shared) ))
            (let ((name.value location)
                  (hash-code  (check-arg/accessor object class)) )
             (cond
               ((plusp hash-code)
                 (cdr name.value) )
               ((minusp hash-code)
                 (no-applicable-method gf object) )
               (t
                 (initial-discriminator gf object) )) ) ) )) )


;;;; make-discriminaotr/reader-n
;;;
;;; Called by:
;;;   compute-discriminator/reader
;;;
;;; Description:
;;;  Returns closure for reading slot of object of a class. A class and its
;;;  subclasses have different slot locations.
;;;
;;; BUGBUG: This discriminator doesn't work if subclasses don't have
;;; instances at creating this discriminator. See, cell-error-name and
;;; unbound-variable class.
;
(defun make-discriminator/reader-n (gf class slot-name)
    (declare (values function))
    (declare (type generic-function  gf))
    (declare (type class  class))
    (declare (type symbol symbol-name))
  (let ((gf    gf)
        (cache (make-accessor-cache class slot-name)) )
    (lambda (object)
          (declare (ext:lambda-name (:discriminator :reader)))
        (let* ((classd    (classd-of object))
               (hash-code (ref classd hash-code classd)) )
           (block reader
             (when (zerop hash-code)
               (update-obsolete-instance object)
               (return-from reader (initial-discriminator gf object)) )

              (loop
                 with mask  = (- (length cache) 2)
                 with start = (logand hash-code mask)
                 with index = start
                 do
                   (when (eq (svref cache index) classd)
                     (let ((location (svref cache (1+ index))))
                       (return-from reader
                         (etypecase location
                           (fixnum
                             (etypecase (ref classd class classd)
                               (standard-class
                                 (standard-instance-access object location) )
                               (funcallable-standard-class
                                 (funcallable-standard-instance-access
                                   object location ) )) )
                           (cons
                             (cdr location) ))) ))

                   (setq index (logand (+ 2 index) mask))

                   (when (eq start index)
                     (return-from reader
                       (no-applicable-method gf object) )) )) ) ) ) )


;;;; make-discriminator/writer
;;;
;;; Arguments and Values:
;;;  method     - A standad-writer-method object which has only one class
;;                specializer.
;;;
;;; Called by:
;;;  compute-discriminator
;;;
;;; Description:
;;;  Returns writer discriminator.
;
(defun make-discriminator/writer (gf method)
  (let* ((class     (second (slot-value method 'specializers)))
         (slotd     (slot-value method 'slot-definition))
         (slot-name (slot-value slotd 'name))
         (location  (same-slot-location-p class slot-name))
         (dfun
           (if location
               (make-discriminator/writer-1 gf class location)
             (make-discriminator/writer-n gf class slot-name) ) ))
    (setf (ext:function-name dfun)
          `(:discriminator :writer ,(slot-value gf 'name)) )
    dfun ) )


;;;; make-discriminaotr/writer-1
;;;
;;; Called by:
;;;   compute-discriminator/writer
;;;
;;; Description:
;;;  Returns closure for writting slot of object of a class. A class and its
;;;  all subclasses must have same slot location.
;
(defun make-discriminator/writer-1 (gf class location)
    (declare (type generic-function gf))
    (declare (type class            class))
    (declare (type (or fixnum cons) location))

  (etypecase location
    (fixnum
      (etypecase class
        (standard-class
          (let ((gf gf)
                (class class)
                (index location) )
            (lambda (value object)
                  (declare (ext:lambda-name
                             (:discriminator :writer :local) ))
                (let ((hash-code (check-arg/accessor object class)))
                  (cond
                    ((plusp hash-code)
                      (setf (standard-instance-access object index) value) )
                    ((minusp hash-code)
                      (no-applicable-method gf value object) )
                    (t
                      (initial-discriminator gf value object) )) ) ) ) )

        (funcallable-standard-class
          (let ((gf gf)
                (class class)
                (index location) )
            (lambda (value object)
                  (declare (ext:lambda-name
                             (:discriminator :writer :local) ))
                (let ((hash-code (check-arg/accessor object class)))
                  (cond
                    ((plusp hash-code)
                      (setf
                        (funcallable-standard-instance-access object index)
                        value ) )
                    ((minusp hash-code)
                      (no-applicable-method gf value object) )
                    (t
                      (initial-discriminator gf value object) )) ) ) )) ) )
     (cons
       (let ((gf gf)
             (class class)
             (name.value location) )
         (lambda (value object)
                  (declare (ext:lambda-name
                             (:discriminator :writer :shared) ))
              (let ((hash-code  (check-arg/accessor object class)))
               (cond
                 ((plusp hash-code)
                   (setf (cdr name.value) value) )
                 ((minusp hash-code)
                   (no-applicable-method gf value object) )
                 (t
                   (initial-discriminator gf value object) )) ) ) )) ) )


;;;; make-discriminaotr/writer-n
;;;
;;; Called by:
;;;   compute-discriminator/writer
;;;
;;; Description:
;;;  Returns closure for writting slot of object of a class. A class and its
;;;  subclasses have different slot locations.
;
(defun make-discriminator/writer-n (gf class slot-name)
    (declare (values function))
    (declare (type generic-function  gf))
    (declare (type class class))
    (declare (type symbol slot-name ))
  (let ((gf    gf)
        (cache (make-accessor-cache class slot-name)) )
    (lambda (value object)
          (declare (ext:lambda-name (:discriminator :writer)))
        (let* ((classd    (classd-of object))
               (hash-code (ref classd hash-code classd)) )
           (block writer
             (when (zerop hash-code)
               (update-obsolete-instance object)
               (return-from writer (initial-discriminator gf object)) )

              (loop
                 with mask  = (- (length cache) 2)
                 with start = (logand hash-code mask)
                 with index = start
                 do
                   (when (eq (svref cache index) classd)
                     (let ((location (svref cache (1+ index))))
                       (return-from writer
                         (etypecase location
                           (fixnum
                             (etypecase (ref classd class classd)
                               (standard-class
                                 (setf
                                   (standard-instance-access object location)
                                   value ) )
                               (funcallable-standard-class
                                 (setf
                                   (funcallable-standard-instance-access
                                     object location )
                                   value ) )) )
                           (cons
                             (setf (cdr location) value) ))) ))

                   (setq index (logand (+ 2 index) mask))

                   (when (eq start index)
                     (return-from writer
                       (no-applicable-method gf value object) )) )) ) ) ) )


;;;; power-2
;;;
;;; Description:
;;;  Returns least greater power of 2 of given number.
;
(defun power-2 (k)
    (declare (type (integer 0 #.call-arguments-limit) k))
  (loop for n = 2 then (ash n 1)
        until (>= n k)
        finally (return n) ) )


;;;; same-slot-location-p
;;;
;;; Called by:
;;;  make-discriminator/reader
;;;  make-discriminator/writer
;
(defun same-slot-location-p (class slot-name)
  (labels (
    (check (class slot-name location)
      (loop
        with slots    = (slot-value class 'slots)
        for slotd in slots
        do
          (when (eq (slot-value slotd 'name) slot-name)
            (let ((location-2 (slot-value slotd 'location)))
              (cond
                ((not location)
                  (setq location location-2) )
                ((not (eq location-2 location))
                  (return-from check '*) ))
              (return) )))

      (dolist (subclass (slot-value class 'direct-subclasses))
        (let ((location-2 (check subclass slot-name location)))
          (when (eq '* location-2)
            (return-from check '*) )
          (when location-2
            (setq location location-2) ) ))
      location )
    )
    ;;
    ;; same-lost-location-p
    ;;
    (let ((location (check class slot-name nil)))
      (when (eq '* location) (setq location nil))
      location ) ) )


;;;; set-generic-function-lambda-list
;;;
;;; Called by:
;;;   add-method
;;;   shared-initialize :after (generic-function)
;;;
;;; Description:
;;;  Update param-info of specified generic-function from lambda-list and
;;;  apo.
;
(defun set-generic-function-lambda-list (gf lambda-list apo)
    (declare (type generic-function gf))
    (declare (type list lambda-list))
    (declare (type list apo))
    (declare (values unspecified))
  (multiple-value-bind (scan reqs opts rests keys)
      (xc::analyze-lambda-list lambda-list)
      (declare (ignore scan))
    (setf (slot-value gf 'param-info)
          (make-param-info
            :nreqs          (length reqs)
            :nopts          (length (rest opts))
            :keys           (or keys (not (null rests)))
            :lambda-list    lambda-list
            :order
              (loop for name in apo
                    collect (position name reqs :test #'eq) ))) )
  (update-discriminator gf) )


;;;; summarize-specializers
;;;
;;; Syntax:
;;;     summarize-specializers nreqs methods => speclvec, nspecls
;;;
;;; Arguments and Values:
;;;   speclvec  - A simple vector which elements represent corresponding
;;;               specializer is class t (nil) or not (t).
;;;   nspecls   - A non-negative integer represents number of non-class t
;;;               specializers.
;;;
;;; Called by:
;;;   make-discriminator/dispatch
;;;
;;; Description:
;;;  Returns vector that element represents corresponding specializer is
;;;  class t or not and number of specializers.
;
(defun summarize-specializers (nreqs methods)
    (declare (type (unsigned-byte 16) nreqs))
    (declare (type list methods))
    (declare (values simple-vector (unsigned-byte 16)))
  (let ((nunspecls nreqs)
        (speclvec (make-simple-vector nreqs nil)) )
    (loop named methods
      for method in methods do
        (loop
          for specl in (slot-value method 'specializers)
          for index = 0 then (1+ index) do
            (unless (svref speclvec index)
              (unless (eq (find-class 't) specl)
                (setf (svref speclvec index) t)
                (decf nunspecls)
                (when (zerop nunspecls)
                  (return-from methods) )))))

    (values speclvec (- nreqs nunspecls)) ) )


;;;; update-discriminator
;;;
;;; Called by:
;;;   add-method
;;;   set-generic-function-lambda-list
;;;
;
(defun update-discriminator (gf)
  (set-funcallable-instance-function
    gf
    (lambda (&rest args)
        (declare (dynamic-extent args))
        (declare (ext:lambda-name (:discriminator)))
      (apply #'initial-discriminator gf args) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MOP Functions
;;;

;;;; MOP compute-discriminating-function
;;;
;;; Note: This function will be converted into generic-function.
;
(defun clos:compute-discriminating-function (gf)
  (compute-discriminator gf) )
