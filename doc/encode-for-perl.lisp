(in-package :cl-user)

(defun encode-for-perl (filename)
  (with-open-file (in filename :external-format :shift_jis)
    (with-open-file (out
                        (make-pathname
                            :name (format nil "perl-~A"
                                    (pathname-name filename) )
                            :defaults filename )
                        :direction :output
                        :if-exists :supersede )
      (loop
        for ch = (read-char in nil)
        while ch
          if (<= (char-code ch) #x7F)
            do (write-char ch out)
          else
            do (format out "\\x{~04X}" (char-code ch)) ))) )
