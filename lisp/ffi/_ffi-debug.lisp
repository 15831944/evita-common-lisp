(defclass sxhash-slot (foreign-object)
  ((object :type (not fixnum))
   (hash-code :type fixnum) )
  (:metaclass foreign-class) )

(defun fetch (p)
  (ref sxhash-slot object (.unbox-int p)) )

(defun size (x)
  (if (not (consp x))
      4
    (loop
      with size = 0
      with runner = x
      while (consp runner) do
        (incf size 8)
        (setq runner (cdr runner))
      finally (return size) )) )

(defun dump (s e)
  (loop
    with size = 0
    with count = 0
    for p from s below e by 8
    for obj = (ref sxhash-slot object (.unbox-int p)) do
      (unless (typep obj 'fixnum)
        (format t "~X ~D ~S~%" p count obj)
        (incf count)
        (incf size (size obj)) )
    finally
      (format t "size=~D~%" size) ) )

;;; (dump #x20ca0030 #x20cb0000)
