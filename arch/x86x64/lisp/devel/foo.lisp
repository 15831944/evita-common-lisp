(defun fdump (fn)
    (declare (type native-code-function fn))
  (si::with-code-annotation-iterator (next fn)
    (loop
      (multiple-value-bind (typ ofs datum) (next)
        (unless typ (return))
        (format t "typ=~S ofs=#x~X datum=~S~%" typ ofs datum))) ) )

(defun foo (n) (declare (type fixnum n)) (dotimes (i n) (print i)))


