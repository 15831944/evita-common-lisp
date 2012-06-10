;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - runtime - 9 Conditions
;;; lisp/runtime/r09-condition.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r09-condition.lisp#3 $
;;;
;;; Description:
;;;  This file contains implementations of following functions:
;;;
;;;  Public classes:
;;;     restart                             9.2.31
;;;
;;; Internal Functions:
;;;     %assert                             9.2.10  internal
;;;     assert-prompt                       9.2.10  internal
;;;     %check-type                         9.2.13  internal
;;;     print-restart
;;;
;;; Public Functions:
;;;     abort                               9.2.46
;;;     break                               9.2.23  with-simple-restart
;;;     cerror                              9.2.12  with-simple-restart
;;;     compute-restarts                    9.2.32
;;;     continue                            9.2.46
;;;     find-restart                        9.2.33
;;;     invoke-debugger                     9.2.22  NYI genesis
;;;     invalid-method-error                9.2.15  stab
;;;     invoke-restart                      9.2.34
;;;     invoke-restart-interactively        9.2.35
;;;     make-condition                      9.2.30
;;;     method-combination-error            9.2.16  stab
;;;     muffle-warning                      9.2.46
;;;     restart-name                        9.2.38
;;;;    signal                              9.2.17
;;;     simple-condition-format-arguments   9.2.19  r09-defcond.lisp
;;;     simple-condition-format-control     9.2.19  r09-defcond.lisp
;;;     store-value                         9.2.46
;;;     use-value                           9.2.46
;;;     warn                                9.2.20  restart-case
;
(in-package :si)

;;;; %assert
;;;
;;; Called by:
;;;  assert
;;;;
;;; Description:
;;;  Report Assertion Failure
;;;
;;; BUGBUG: NYI: Check compilation declaration.
;
(defun %assert (test-form places datum args)
  (labels (
    ;; assert-report
    (assert-report (stream places)
      (if places
          (format stream "Retry assertion with new value~P for ~{~S~^, ~}."
              (length places)
              places )
        (format stream "Retry assertion") ) )
    )
    ;;
    (let ((cond (if datum
                    (coerce-to-condition 'error datum args 'simple-error)
                  (make-condition 'assertion-failure :form test-form) )))
      (restart-case (error cond)
        (continue ()
          :report (lambda (stream) (assert-report stream places)) ) )
      nil ) ) )

#+nil
(defun %assert (test-form places datum args)
    (declare (ignore places))
  (let ((cond (if datum
                 (coerce-to-condition 'error datum args 'simple-error)
               (make-condition 'assertion-failure :form test-form) )))

    (error cond)
    nil ) )


;;;; assert-prompt
;;;
;;; Description:
;;;  Reads value for place form specified to assert macro.
;
(defun assert-prompt (place-form)
  (format *error-output* "Value for ~S: " place-form)
  (force-output *error-output*)
  (read *query-io*) )


;;;; %check-type
;;;
;
(defun %check-type (value type &optional string)
  (when (null string)
    (setq string (format nil "Specifiy value of type ~S." type)) )
  (restart-case (error 'type-error :datum value :expected-type type)
    (store-value (x)
      :report (lambda (s) (write-string string s))
      x ) ) )


;;;; coerce-to-condition
;;;
;;; BUGBUG: Stab
;
(defun coerce-to-condition (caller datum args class)
    (declare (type symbol caller))
    (declare (type (or condition string function symbol class) datum))
    (declare (type list args))
    (declare (type type-specifier class))
    (declare (values condition))
  (etypecase datum
    (condition
      (when args
        ;; BUGBUG: LOC: coerce-to-condition
        (cerror "Ignore the extra arguments."
                'simple-type-error
                :datum args
                :expected-type 'null
                :format-control "You may not supply extra arguments when giving ~S to ~S."
                :format-arguments (list datum caller) ))
      datum )

    ((or string function)
      (make-condition class
                     :format-control datum
                     :format-arguments args ) )

    ;; BUGBUG: NYI: condition-class
    ;; (or symbol condition-class)
    ((or symbol class)
      (apply #'make-condition datum args) ) ) )


;;;; invoke-restart-internal
;;;
;;; Called by:
;;;  abort
;;;  continue
;;;  invoke-restart
;;;  muffle-warning
;;;  store-value
;;;  use-value
;;;
;;; Description:
;;;  Calls restart specified by name and condition. If there is no such
;;;  restart, this function returns nil or signals restart-not-found when
;;;  error-p isn't nil.
;
(defun invoke-restart-internal (name condition error-p &rest args)
    (declare (dynamic-extent args))
  (let ((restart (find-restart name condition)))
    (cond
      (restart
        (apply (ref restart function restart) args) )
      (error-p
        (error 'restart-not-found
               :name name
               :condition condition ) )
      (t nil) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public Functions
;;;;


;;;; 9.2.46 abort
;;;; 9.2.46 continue
;;;; 9.2.46 muffle-warning
;;;  9.2.46 store-value
;;;  9.2.46 use-value
;
(macrolet (
  (define-restart-function (name arg error-p)
    (when arg (setq arg (list arg)))
    `(defun ,name (,@arg &optional condition)
        (invoke-restart-internal ',name condition ,error-p ,@arg) ) )
  )
  ;;
  (define-restart-function cl:abort          nil   t)
  (define-restart-function cl:continue       nil   nil)
  (define-restart-function cl:muffle-warning nil   t)
  (define-restart-function cl:store-value    value nil)
  (define-restart-function cl:use-value      value nil)
 ) ; macrolet


;;;; 9.2.23 break
;
(defun cl:break (&optional (format-control "Break") &rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (let ((*debugger-hook* nil))
      (invoke-debugger
          (make-condition 'simple-condition
                          :format-control   format-control
                          :format-arguments format-arguments )) )) )


;;;; 9.2.12 cerror
;
#|
(defun cl:cerror (format-control datum &rest args)
    (declare (type format-control format-control))
    (declare (type condition-designator datum))
  (with-simple-restart
      (continue "~A" (apply #'format nil format-control args))
    (apply #'error datum args) )
  nil )
|#

(defun cl:cerror (format-control datum &rest args)
    (declare (type format-control format-control))
    (declare (type condition-designator datum))
  (let* ((cond
           (coerce-to-condition 'error datum args 'simple-error) )
         (restart
           (make-restart
             :name 'continue
             :function
               (lambda ()
                 (return-from cerror nil) )
             :report-function
               (lambda (s)
                 (apply #'format s format-control args) )) ))
    (with-condition-restarts cond (list restart) (error cond)) ) )


;;;; 9.2.32 compute-restarts
;;;
;;; Note: Retunred restarts list should be reverse cronical order. Debugger
;;; prints available restarts by enumerating from start to end.
;
(defun cl:compute-restarts (&optional condition)
    (declare (type (or condition null) condition))
    (declare (values list))
  (let ((restarts '()))
    (if condition
        ;; Collect restarts associated to specified condition.
        (dolist (cluster *restart-clusters*)
          (when (eq (first cluster) condition)
            (dolist (restart (rest cluster))
              (pushnew restart restarts :test 'eq) )) )

        ;; Collect all restarts
        (dolist (cluster *restart-clusters*)
          (dolist (restart (rest cluster))
            (pushnew restart restarts :test 'eq) ) ))
    (nreverse restarts) ) )


;;;; 9.2.5 error
;
#-genesis
(defun cl:error (datum &rest args)
    (declare (type condition-designator datum))
    (declare (values nil))
  (let ((condition (coerce-to-condition 'error datum args 'simple-error)))
    (signal condition)
    (invoke-debugger condition) ) )


;;;; 9.2.33 find-restart
;
(defun cl:find-restart (identifier &optional condition)
    (declare (type restart-designator identifier))
    (declare (type (or null condition) condition))
  (if condition
      ;; Search in cluster associated with specified condition.
      (dolist (restart (cdr (assoc condition *restart-clusters* :test #'eq)))
        (when (or (eq restart identifier)
                  (eq (restart-name restart) identifier) )
          (return-from find-restart restart) ) )

    ;; Search in all clusters.
    (dolist (cluster *restart-clusters*)
      (dolist (restart (cdr cluster))
        (when (or (eq restart identifier)
                  (eq (restart-name restart) identifier) )
          (return-from find-restart restart) ) ) )) )


;;;; 9.2.15 invalid-method-error
;;;
;;; BUGBUG: stab
;
(defun cl:invalid-method-error (method format-control &rest args)
    (declare (ignore method))
  (apply #'error format-control args) )


;;;; 9.2.22 invoke-debugger
;;;
;;; See: dev;d25-toplevel.lisp
;;;

;;;; 9.2.34 invoke-restart
;
(defun cl:invoke-restart (name &rest args)
    (declare (type restart-designator name))
    (declare (dynamic-extent args))
  (apply #'invoke-restart-internal name nil t args) )


;;;; 9.2.35 invoke-restart-interactively
;
(defun cl:invoke-restart-interactively (name &optional condition)
    (declare (type restart-designator name))
  (let ((restart (find-restart name condition)))
    (unless restart
      (error 'simple-control-error
             :format-control   "Restart ~S isn't available."
             :format-arguments (list name) ))
    (apply (ref restart function restart)
           (let ((fn (ref restart interactive-function restart)))
             (and fn (funcall fn)) )) ) )


;;;; 9.2.30 make-condition
;;;
;;; BUGBUG: When we use CLOS instance for condition instance, we must
;;; defines shared-initialize :after for condition class to handle
;;; initargs that may have slot keywords other than initarg.
;
(defun cl:make-condition (class &rest initargs)
    (declare (dynamic-extent initargs))
  (apply #'make-instance class initargs) )


;;;; 9.2.16 method-combination-error
;;;
;;; BUGBUG: stab
;
(defun cl:method-combination-error (format-control &rest args)
  (apply #'error format-control args) )


;;;; 9.2.17 signal
;;;
;;; Note:
;;; This function shrinks active handlers when calling handler. When
;;; same type of condition is signaled in the handler, outer handler is
;;; called instead of calling same handler.
;;;
;
(defun cl:signal (datum &rest args)
    (declare (type condition-designator datum))
  (let ((condition (coerce-to-condition 'signal datum args 'simple-condition))
        (*handler-clusters* *handler-clusters*) )
    (if (let ((type *break-on-signals*)
              (*break-on-signals* nil) )
          (typep condition type) )
        (break "~A~%Break entered because of ~S" condition '*break-on-signals*)
      (loop
        (when (endp *handler-clusters*) (return))
        (let ((clusters (pop *handler-clusters*)))
          (dolist (type.handler clusters)
            (when (typep condition (car type.handler))
              (funcall (cdr type.handler) condition) ) ) )))
    nil ) )


;;;; 9.2.20 warn
;#-genesis
(defun cl:warn (datum &rest args)
  (let ((condition (coerce-to-condition 'warn datum args 'simple-warning)))
    (check-type condition warning)
    (restart-case (signal condition)
      (muffle-warning () :report "Ignore warning."
        (return-from warn nil) ))
    (format *error-output* "~&; ~:[Warning~;Style-warning~]: ~A~%"
      (typep condition 'style-warning)
      condition )
    nil ) )
