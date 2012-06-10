    (defun foo (form)
      (multiple-value-bind (fn errors warns cookies)
          (c::compile-form form)
 (format t "; compile-form: fn=~S e=~S w=~S~%" fn errors warns)
        (when warns  (incf total-warns))
        (when (or (null fn) errors) (incf total-errors))
 (format t "; compile-form: cookies=~S~%" cookies)
        (loop
          with objtab = (slot-value fasd 'devel::objtab)
          for (cookie . form) in cookies do
            (setf (gethash cookie objtab) `(load-time-value ,form nil)) )
        fn ) )

