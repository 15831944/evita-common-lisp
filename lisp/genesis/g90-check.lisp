#-debug-kernel
(load "../../testcase/tc-loadup" :print t)

#-debug-kernel
(when (tc:run-tests :verbose nil)
  (format t "Test failed.") (exit-process 1) )

(format t "~2&~70~~%")
(format t "Image validation is successfully completed.~%")
(format t "~%")

(multiple-value-bind (s n h d m y) (get-decoded-time)
  (format t "Test is completed at ~A ~D, ~D ~2,'0D:~2,'0D:~2,'0D.~%"
    (svref #("***"
             "Jan" "Feb" "Mar" "Apr" "May" "Jun"
             "Jul" "Aug" "Sep" "Oct" "Nov" "Dec" )
           m )
    d y h n s ) )
(format t "~2%")

(si::exit-process 0)
;; EOF
