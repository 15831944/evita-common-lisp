(in-package :si)

;;;; foreign-pointer
(defclass foreign-pointer (structure-object)
  ((type    :type class)
   (value   :type integer)
   (heap    :type fixnum) )
  (:metaclass structure-class) )

;; FIXME 2007-07-07 yosi@msn.com We should implement type "pointer".
(deftype pointer (class) (declare (ignore class)) 'foreign-pointer)

;;;; +null-pointer+
(defvar +null-pointer+
  (let ((p (make-instance 'foreign-pointer)))
    (setf (ref foreign-pointer type  p) (find-class 'void))
    (setf (ref foreign-pointer value p) 0)
    (setf (ref foreign-pointer heap  p) 0)
    p ) )

(declaim
  (ftype
    (function (class &optional (fixnum)) foreign-pointer)
    allocate-foreign-object )
  (ftype
    (function (foreign-pointer) foreign-pointer)
    free-foreign-object )
  (ftype
    (function (foreign-pointer) t)
    null-pointer-p )
  (ftype
    (function (foreign-pointer foreign-pointer) t)
    pointer-eq )
  ) ; declaim
