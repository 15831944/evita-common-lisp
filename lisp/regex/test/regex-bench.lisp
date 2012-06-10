;;; From Mastering Regular Expressions, Jeffry E. F. Friedl, O'Reily, 2002
;;; P.237


(defun regex-bench (&optional (times-to-do 1000))
  (labels (
    (time-it (pattern text)
      (format t "; Start ~S~%" pattern)
      (let ((start (get-internal-real-time))
            (match (ext:eval-regex pattern "")) )
        (loop repeat times-to-do do
          (first-match match text) )
        (let ((end (get-internal-real-time)))
          (format t ";  It takes ~,3F sec.~%"
            (/ (float (- end start)) internal-time-units-per-second) ) ) ) )
    )
    (let ((text
            (with-output-to-string (s)
              (loop repeat times-to-do do (write-string "abababdedfg" s)) ) ))
     (time-it "^[a-g]+$" text)
     (time-it "^(a|b|c|d|e|f|g)+$" text) ) ) )
