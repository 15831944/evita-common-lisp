;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 7 Objects - Effective Method
;;; lisp/clos/o04-efm.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o04-emf.lisp#3 $
;;;
;;; Description:
;;;  This file contains method dispatch related functions.
;;;
;;; Internal Funcitons:
;;;     %defgeneric
;;;     %defmethod
;;;     call-method-n
;;;     method-needs-next-methods-p
;;;     no-next-method-error
;;;
;;; MOP Functions:
;;;     compute-effective-method                MOP
;;;     make-method-lambda                      MOP
;;;
;;; Note: This file CAN NOT include defmethod form.
;
(in-package :si)

;;;; %defgeneric
;
(defun %defgeneric (fname lambda-list &rest initargs)
    (declare (dynamic-extent initargs))
  (let ((gf (apply #'ensure-generic-function
                   fname
                   :lambda-list lambda-list
                   initargs ) ))
    (dolist (method (generic-function-methods gf))
      (dolist (specializer (method-specializers method))
        (remove-direct-method specializer method) ) )
    (setf (slot-value gf 'methods) '())
    gf ) )


;;;; %defmethod
;;;
;;; Arguments and Values
;;;   name          - a name of method.
;;;   qualifiers    - a list of qualifiers.
;;;   specializers  - a list of names of specializers.
;;;   function      - a method funciton.
;;;   initargs      - an initargs for ...
;;;
;;; Called by:
;;;  defmehtod
;
(defun %defmethod (fname qualifiers lambda-list specializers function
                       &rest initargs
                       &key  lambda-name
                             needs-next-methods-p
                       &allow-other-keys )
    (declare (type t fname))
    (declare (type list qualifiers))
    (declare (type list lambda-list))
    (declare (type list specializers))
    (declare (type function function))
    (declare (dynamic-extent initargs))
    (declare (ignore lambda-name))

  (remf initargs :needs-next-methods-p)

  (let* ((gf (ensure-generic-function fname))
         (mt (apply #'make-instance
                    (slot-value gf 'method-class)
                    :function         function
                    :lambda-list      lambda-list
                    :specializers     specializers
                    :qualifiers       qualifiers
                    initargs ) ))

    ;; Note: We don't fill generic-function slot here. add-method will
    ;; set generic-function slot.

    ;; Embeds method object into method function.
    ;; As of build 2714, embedding method is taken place only if method-
    ;; function uses CALL-NEXT-METHOD. We need to pass method object to
    ;; function NO-NEXT-MEHOD-ERROR.
    (when (and needs-next-methods-p
               (subst-in-function mt needs-next-methods-p  function) )
      (setf (getf (slot-value mt 'plist) :needs-next-methods-p)
            needs-next-methods-p ))

    ;; Sets distinguish name to method function.
    #+nil
    (let ((name (ext:function-name function)))
      (when (null name)
        (setq name 
            `(method-function
              ,fname
              ,qualifiers
              ,(mapcar #'specializer-name specializers) ))
        (setf (ext:function-name function) name) ) )

    ;; Make this newly create method visible to others.
    (add-method gf mt)
    mt ) )


;;;; call-method-n
;;;
;;; Called by:
;;    compute-effective-method
;
(defun call-method-n (methods &rest args)
    (declare (type list methods))
    (declare (dynamic-extent args))
    (declare (optimize speed (safety 0)))
  (let ((method         (first methods))
        (*next-methods* (rest methods)) )
    (apply method args) ) )


;;;; method-needs-next-methods-p
;;;
;;; Called by:
;;;   make-discriminator/checking
;;;
;
(defun method-needs-next-methods-p (method)
    (declare (type method method))
    (declare (values t))
  (let ((plist (slot-value method 'plist)))
    (getf plist :needs-next-methods-p) ) )


;;;; no-next-method-error
;;;
;;; Called by:
;;;   call-next-method
;;;
;;; BUGBUG: Should be local function of compute-effective-method.
;
(defun no-next-method-error (method &rest args)
    (declare (type method method))
    (declare (dynamic-extent args))
  (apply #'no-next-method (method-generic-function method) method args) )


;;;; specializer-name
;;;
;;; Called by;
;;;  %defmethod
;;;  make-dfun-ctor/dispatch
;;;
;;; Description:
;;;  Returns name of specializer.
;
(defun specializer-name (specializer)
    (declare (type specializer))
    (declare (values t))
  (typecase specializer
    (class (class-name specializer))
    (eql-specializer
      `(eql ,(eql-specializer-object specializer)) )
    (otherwise specializer) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; MOP Functions
;;;;

;;;; MOP compute-effective-method
;;;
;;; Called by:
;;;  make-dfun-ctor/dispatch
;;;
;;; Description:
;;;  Retruns compiled effective-method for standard-method-combination.
;;; BUGBUG: NYI: method-combination
;
(defun std-compute-effective-method (gf mcomb methods)
    (declare (type generic-function gf))
    (declare (type method-combination mcomb))
    (declare (type list methods))
    (declare (values (or function null)))
    (declare (ignore gf mcomb))
  (labels (
    (make-emf-around (around emf)
        (declare (type list around))
        (declare (type function emf))
      (let ((around (nconc around (list (make-method emf)))))
        #'(lambda (&rest args)
              (declare (ext:lambda-name emf-around))
              (declare (dynamic-extent args))
            (apply #'call-method-n around args) ) ) )

    (make-emf-before-primary (before primary)
        (declare (type list before))
        (declare (type list primary))
      #'(lambda (&rest args)
            (declare (ext:lambda-name emf-before-primary))
          (dolist (method before)
            (apply #'call-method-n (list method) args) )
          (apply #'call-method-n primary args) ) )

    (make-emf-before-primary-after (before primary after)
        (declare (type list before))
        (declare (type list primary))
        (declare (type list after))
      #'(lambda (&rest args)
            (declare (ext:lambda-name emf-before-primary-after))
          (dolist (method before)
            (apply #'call-method-n (list method) args) )
          (multiple-value-prog1
              (apply #'call-method-n primary args)
            (dolist (method after)
              (apply #'call-method-n (list method) args) ) ) ) )

    (make-emf-primary (primary-fns primary-methods)
        (declare (type list primary-fns))       ; list of method-function
        (declare (type list primary-methods))   ; list of methods
      (let ((method (first primary-methods)))
        (cond
          ((not (method-needs-next-methods-p method))
            (first primary-fns) )

          ((null (rest primary-fns))
            (let ((fn (first primary-fns)))
              #'(lambda (&rest args)
                    (declare (ext:lambda-name emf-primary-1))
                    (declare (dynamic-extent args))
                  (let ((*next-methods* '()))
                     (apply fn args) ) ) ) )
          (t
            #'(lambda (&rest args)
                  (declare (ext:lambda-name emf-primary))
                  (declare (dynamic-extent args))
                (apply #'call-method-n primary-fns args) ) )) ) )

    (make-emf-primary-after (primary after)
        (declare (type list primary))
        (declare (type list after))
      #'(lambda (&rest args)
            (declare (ext:lambda-name emf-primary-after))
            (declare (dynamic-extent args))
          (multiple-value-prog1
              (apply #'call-method-n primary args)
            (dolist (method after)
              (apply #'call-method-n (list method) args) )) ) )

    (make-method (emf)
        (declare (type function emf))
      #'(lambda (&rest args)
            (declare (ext:lambda-name (make-method emf)))
            (declare (dynamic-extent args))
          (let ((*next-methods* '()))
            (apply emf args) ) ) )
    )
    ;;
    ;; std-compute-emf
    ;;
    (loop
      for method in methods
      for qualifier = (first (slot-value method 'qualifiers))
      with after = '()
      when (eq nil qualifier)
        collect (slot-value method 'function) into primary
        and collect method into primary-methods
      else when (eq :before qualifier)
        collect (slot-value method 'function)into before
      else when (eq :after qualifier)
        do (push (slot-value method 'function) after)
      else when (eq :around qualifier)
        collect (slot-value method 'function) into around
     finally
      (when (null primary) (return nil))
      (let ((emf
              (cond
                ((and (null after) (null before))
                  (make-emf-primary primary primary-methods) )

                ((and after (null before))
                  (make-emf-primary-after primary after) )

                ((and (null after) before)
                  (make-emf-before-primary before primary) )

                (t
                  (make-emf-before-primary-after before primary after) )) ))
        (when around
          (setq emf (make-emf-around around emf)) )
        (return emf) )) ) )


;;;; MOP make-method-lambda
;;;
;;; For: defmethod
;;;
;;; See Also: sys:source;genesis;g11-clos.lisp
;;;
;;; BUGBUG: REVIEW: Where do we put declarations in lambda-expr?
;;; For example, if it has "optimize" declaration, do we apply it
;;; call-next-method?
;;;
;;; BUGBUG: We should not use apply for optional parameters.
;;;
;;; Note: method object may be prototype.
;;;
;;; Note: sym-mt will be replaced to a method object in %defmethod.
;;;
;;; Note: This function will be converted into generic-function. See
;;  function convert-to-gf in "o00-loadup".
;;;
;;; From ANSI-CL:
;;;     When call-next-method is called with arguments, the next method is
;;;     called with those arguments.
;;;
;;;     If call-next-method is called with arguments but omits optional
;;;     arguments, the next method called defaults those arguments. 
;
(defun clos:make-method-lambda (gf method lambda-expr env)
    (declare (type standard-generic-function gf))
    (declare (type standard-method method))
    (declare (type list lambda-expr))
    (declare (type t env))
    (declare (ignore gf method env))
    (declare (values cons list))
  (multiple-value-bind (reqs opts rests keys)
        (c::parse-lambda-list (second lambda-expr))
    (let* ((cookie (gensym))
           (tmps
             (loop for name in reqs
                collect (gensym (symbol-name name)) ) )
           (rests (or rests (when keys `(&rest ,(gensym)))))
           (var-rest
             (if (and (null opts) (null rests) (null keys)) nil '#:args) ))
    (values
      ;; Note no-next-method-error will store var-rest into condition
      ;; object.
     `(lambda (,@tmps ,@(and var-rest `(&rest ,var-rest)))
        ,@(and var-rest `((declare (dynamic-extent ,var-rest))))
        (flet (
          ;; call-next-method
          (cl:call-next-method (&rest cnm-args)
               (declare (dynamic-extent cnm-args))
               (declare (optimize (speed 3) (safety 0)))
             (when (null *next-methods*)
               ,(if (null var-rest)
                    `(if cnm-args
                         (apply #'no-next-method-error ',cookie cnm-args)
                       (no-next-method-error ',cookie ,@tmps) )
                `(if cnm-args
                     (apply #'no-next-method-error ',cookie cnm-args)
                   (no-next-method-error
                        ',cookie ,@tmps (copy-list ,var-rest) ))))
             (let ((.next-method.  (first *next-methods*))
                   (*next-methods* (rest  *next-methods*)) )
                ,(if (null var-rest)
                     `(funcall .next-method. ,@tmps)
                   `(apply .next-method. ,@tmps ,var-rest) ) ) )

          ;; next-method-p
          (cl:next-method-p ()
                (declare (optimize (speed 3) (safety 0)))
            (not (null *next-methods*)) )
          )
          ;;
            (declare (ignorable #'cl:call-next-method #'next-method-p))
            (declare (inline cl:call-next-method cl:next-method-p))
         ,(cond
            ((eq tmps var-rest)
              `(locally ,@(cddr lambda-expr)) )
            ((null var-rest)
              `(let ,(mapcar #'list reqs tmps) ,@(cddr lambda-expr)) )
            (t
              `(apply ,lambda-expr ,@tmps ,var-rest) )) ) )
      `(:needs-next-methods-p ',cookie) ) ) ) )

