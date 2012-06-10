(in-package :si)

;;; For defmethod
;;;     class-prototype
;;;     ensure-generic-function-using-class
;;;     make-instance
;;;     slot-value
;;;     (setf slot-value)

;;; slot-value
;;; (setf slot-value)
;;; make-instance
(labels (
  ;; get-location
  (get-location (class slot-name)
    (let* ((slots  (ref standard-class slots (ref instance storage class)))
           (eslotd (find slot-name slots
                      :key (lambda (eslotd)
                              (ref standard-effective-slot-definition name
                                    (ref instance storage eslotd) ))) ))
      (unless eslotd
        (error "~S doent' have slot ~S." class slot-name) )
      (ref standard-effective-slot-definition location
          (ref instance storage eslotd )) ) )

  ;; access
  (access (class x location)
    (etypecase class
      (standard-class
        (if (consp location)
            (car location)
          (standard-instance-access x location) ) )
      (funcallable-standard-class
        (funcallable-standard-instance-access x location) )
      (structure-class
        (structure-instance-access x location) )) )

  ;; (setf access)
  ((setf access) (y class x location)
    (etypecase class
      (standard-class
        (if (consp location)
            (setf (car location) y)
          (setf (standard-instance-access x location) y) ) )
      (funcallable-standard-class
        (setf (funcallable-standard-instance-access x location) y) )
      (structure-class
        (setf (structure-instance-access x location) y) )) )
  )

  ;; slot-value
  (defun cl:slot-value (x slot-name)
      (declare (type symbol slot-name))
    (let* ((class    (class-of x))
           (location (get-location class slot-name)) )
      (access class x location) ) )

  ;; (setf slot-value)
  (defun (setf cl:slot-value) (y x slot-name)
      (declare (type symbol slot-name))
    (let* ((class    (class-of x))
           (location (get-location class slot-name)) )
      (setf (access class x location) y) ) )

  ;; make-instance
  (set-funcallable-instance-function #'make-instance
    (lambda (klass &rest initargs)
        (declare (lambda-name (:stab make-instance)))
      (loop
        with unspecified = '(unspecified)
        with class = (if (symbolp klass) (find-class klass) klass)
        with instanced =
                (or (ref standard-class instance-description
                                (ref instance storage class) )
                     (error "~S isn't finalized.") )
        with instance =
               (etypecase class
                 (standard-class
                   (.allocate-instance instanced) )
                 (funcallable-standard-class
                   (.allocate-funcallable-instance instanced) ))

        for eslotd in (ref class-description slots instanced)
        for st = (ref instance storage eslotd)
        for initarg =
          (loop
            with keys = (ref standard-effective-slot-definition initargs st)
            for (key val) on initargs by #'cddr
             when (member key keys)
                return val
            finally (return unspecified) )
        for initform =
          (ref standard-effective-slot-definition initform st)
        for initfunction =
          (ref standard-effective-slot-definition initfunction st)
        for location =
          (ref standard-effective-slot-definition location st)
        finally
          (return instance)
        do
          (setf (access class instance location)
            (cond
              ((not (eq initarg unspecified))
                initarg )
              ((functionp initfunction)
                (funcall initfunction) )
              ((eq initfunction 'quote)
                initform )
              (t
              '#.(unbound-marker)) ))) ))
 ) ; labels


;;; add-method
;;; class-prototype
;;; ensure-generic-function
(labels (
  )
  ;;
  (set-funcallable-instance-function #'clos:class-prototype
    (lambda (class)
       (check-type class class)
      (or (ref standard-class prototype
                (ref instance storage class) )
          (error "No class-prototype for ~S." class) ) ) )

  (set-funcallable-instance-function
    #'clos:ensure-generic-function-using-class
    (lambda (fn fname &rest initargs)
        (declare (lambda-name (:stab ensure-generic-function-using-class)))
        (declare (values generic-function))
        (declare (type (or function null) fn))
        (declare (type function-name fname))
        (declare (ignore initargs))
      (etypecase fn
        (generic-function fn)
        (null (error "We forgot to define generic-function ~S." fname) )
        (function (error "~S must be a generic-function." fname)) ) ) )
 ) ; labels
