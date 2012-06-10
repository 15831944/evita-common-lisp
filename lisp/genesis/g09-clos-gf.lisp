(in-package :si)

;;; For discriminator

;;;; cl:compile
(defun cl:compile (name &optional definition)
  (unless (or (null name) (si::function-name-p name))
    (error "Bad function name: ~S" name) )
  (multiple-value-bind (function warnings-p failure-p)
      (cond
        ((functionp definition)
          (values definition nil nil) )

        ((and (consp definition)
              (eq 'lambda (first definition)) )
          (multiple-value-bind (fn warns fails)
              (let ((c::*situation* 'compile)
                    (c::*target*    c::+eval-target+) )
                (c::compile-form definition) )
            ;; BUGBUG: NYI: load-time-value-form-function
            (unless (functionp fn)
              (error "Compilation failed.") )
            (values (funcall fn) warns fails) ) )

        ((and name (null definition))
          (unless (fboundp name)
            (error 'undefined-function :name name) )
          (values nil nil nil) )

        (t
          (error "Expect lambda expression or function: ~S" definition ) ))
    (when (and name definition function)
      (if (and (fboundp name) (macro-function name))
          (setf (macro-function name) function)
        (setf (fdefinition name) function) ))
    (values (or name function) warnings-p failure-p) ) )

;;;; make-param-info
(defun make-param-info (&key nreqs nopts keys lambda-list order)
  (let ((x (.allocate-record #.(class-description 'param-info))))
    (setf (ref param-info nreqs x) nreqs)
    (setf (ref param-info nopts x) nopts)
    (setf (ref param-info keys  x) keys)
    (setf (ref param-info lambda-list x) lambda-list)
    (setf (ref param-info order       x) order)
    x ) )
