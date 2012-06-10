(in-package :win32)

(defun make-blob (n)
  (make-array n :element-type '(unsigned-byte 8)) )

(defun make-rect (&key (bottom 0) (left 0) (right 0) (top 0))
  (make-array 4 :element-type '(signed-byte 32)
                :initial-contents (list left top right bottom) ) )

(defun make-handle (&optional (value 0))
  (let ((handle (si::.allocate-binobj '#.(si::class-description 'si::handle))))
    (setf (ref si::handle si::value handle) value)
    handle ) )

(defun rect-height (rc)
    (declare (type (signed-byte 32)))
    (declare (type rect rc))
  (- (elt rc 3) (elt rc 1)) )

(defun rect-width (rc)
    (declare (type (signed-byte 32)))
    (declare (type rect rc))
  (- (elt rc 2) (elt rc 0)) )
