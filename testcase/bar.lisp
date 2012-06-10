(defun compile-toplevel-form (stream form fasd)
    (declare (values fixnum fixnum))
    (declare (type stream stream))
    (declare (type devel::fasd fasd))
  (let ((total-warns 0) (total-errors 0))
  (labels (
    ;; cm-warn
    (cm-warn (format &rest args)
      (format *error-output* "~A(~D): ~?~%"
        (truename (stream-pathname stream))
        (1+ (stream-line-number stream))
        format args )
      (incf total-warns) )

    ;; compile-form
    (compile-form (form)
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

    ;; process
    (process (form)
      (cond
        ((not (consp form))
          (cm-warn "Ignore toplevel literal form: ~S" form) )
        ((not (proper-list-p form))
          (cm-warn "Ignore malformed cons form: ~S" form) )
        (t (process-cons form)) ) )

    ;; process-cons
    (process-cons (form)
      (case (first form)
        ((defun)      (process/defun form))
        #+nil ((eval-when)  (process/eval-when form))
        ((in-package) (process/in-package form))
        ((progn)      (process/progn form))
        (otherwise    (process/others form)) ) )

    ;; process/defun
    (process/defun (form)
      (destructuring-bind (defun name lambda-list &body body) form
          (declare (ignore defun))
        (let ((fn (compile-form
                `(labels ((,name ,lambda-list ,@body)) #',name) ) ))
          (when fn
            (let ((datum `(defun ,name ,lambda-list ,(funcall fn))))
              (devel::fasd-write datum fasd) )) ) ) )

    ;; process/eval-when
    #+nil
    (process/eval-when (form)
      (destructuring-bind (eval-when (&rest situation*) &rest form*) form
        (let ((ct nil) (ex nil) (lt nil))
          (dolist (situation situation*)
            (case situation
              ((:compile-toplevel cl:compile) (setq ct t))
              ((:execute cl:eval) (setq ex t))
              ((:load-toplevel cl:load) (setq lt t))
              (otherwise (cm-warn "Unknown situation ~S" situation)) ) )
          (let ((c::*processing-mode*
                    (if ex :compile-time-too c::*processing-mode*) ))
            (when ct (eval `(progn ,@form*)))
            (when lt (dolist (form1 form*) (process form1))) ) ) ) )

    ;; process/in-package
    (process/in-package (form)
      (if (/= (length form) 2)
          (cm-warn "Syntax error: ~S" form)
        (progn
          (%in-package (second form))
          (devel::fasd-write form fasd) )) )

    ;; process/others
    (process/others (form)
      (let ((fn (compile-form form)))
        (when fn (devel::fasd-write `(funcall ,fn) fasd)) ) )

    ;; process/progn
    (process/progn (form)
      (dolist (form1 (rest form))
        (process form1) ) )
    )
    ;;
    (process form)
    (values total-warns total-errors) ) ) )
