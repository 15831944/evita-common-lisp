(in-package :si)

;;;; allocate-object
(defun allocate-object (class &optional (heap (fli:call "GetProcessHeap")))
    (declare (values foreign-pointer))
    (declare (type class class))
  (let* ((size  (si::sizeof class))
         (void* (fli:call "HeapAlloc"  heap 0 size))
         (ptr   (make-instance 'foreign-pointer)) )
    (setf (ref foreign-pointer type  ptr) class)
    (setf (ref foreign-pointer value ptr) void*)
    (setf (ref foreign-pointer heap  ptr) heap)
    ptr ) )


;;;; free-object
(defun free-object (obj)
    (declare (values foreign-pointer))
    (declare (type foreign-pointer obj))
  (fli:call "HeapFree"
            (ref foreign-pointer heap obj)
            0
            (ref foreign-pointer value obj) )
  obj )
