(in-package :si)

;;;; foreign-allocate
(defun allocate-foreign-object (class
                                &optional (heap (win32::|GetProcessHeap|)) )
    (declare (values foreign-pointer))
    (declare (type class class))
  (let* ((size (si::sizeof class))
         (fptr (win32::|HeapAlloc| heap 0 size))
         (ptr (make-instance 'foreign-pointer)) )
    (setf (ref foreign-pointer type  ptr) class)
    (setf (ref foreign-pointer value ptr) fptr)
    (setf (ref foreign-pointer heap  ptr) heap)
    ptr ) )


;;;; free-foreign-object
(defun free-foreign-object (obj)
    (declare (values foreign-pointer))
    (declare (type foreign-pointer obj))
  (win32::|HeapFree| (ref foreign-pointer heap obj)
                     0
                     (ref foreign-pointer value obj) )
  obj )
