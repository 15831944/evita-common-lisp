(in-package :si)

(define-format #\< ((mincol 0)
                    (colinc 1)
                    (minpad 0)
                    (padchar #\Space) )
  (let ((clause1-modifier 0)
        (clauses          '())
        (close-modifier   0) )
      (declare (type (unsigned-byte 2) clause1-modifier))
      (declare (type list clauses))

    ;; Looking for Right angle bracket
    ;;
    (let* ((clause1-start (ref format-context control-index context))
           (clause-start  clause1-start) )
      (loop
        (multiple-value-bind (tilde stop-char modifier)
            (format-skip-clause context #\> clause1-start)

          (push (cons clause-start tilde) clauses)

          (when (char= #\> stop-char)
            (setq close-modifier modifier)
            (return) )

          (when (null (rest clauses))
            (setq clause1-modifier modifier) )

          (setq clause-start (ref format-context control-index context)) ))
      (setq clauses (nreverse clauses)) )

    ;; Process segments
    ;;
    (cond
      ((logbitp 0 close-modifier)
        ;; ~< prefix ~; body ~; suffix ~:>
        ;;
        (when (params)
          (format-error-too-many-params context) )

        (unless (<= 1 (length clauses) 3)
          (format-error-pp-malformed context) )

        ;; Check modifier of the first ~;.
        (when (logbitp 0 clause1-modifier)
          (setf (ref format-context control-index context)
                (car (second clauses)) )
          (format-error-invalid-modifier context) )

        (let ((arg (if (not (logbitp 1 (modifier)))
                       (next-arg)
                     (prog1
                       (rest-args)
                       (setf (ref format-context arg-scan context) nil) )))
              (suffix (cond
                        ((= 3 (length clauses))
                          (subseq (ref format-context control-string context)
                                  (car (third clauses))
                                  (cdr (third clauses)) ) )

                        ((logbitp 0 (modifier))
                          ")" )
                        (t
                          "" )) )
              (prefix (cond
                        ((>= (length clauses) 2)
                          (let ((clause-1 (pop clauses)))
                            (subseq (ref format-context control-string context)
                                    (car clause-1)
                                    (cdr clause-1) ) ) )

                        ((logbitp 0 (modifier))
                          "(" )
                        (t
                          "" )) )
              (ctrl-string (ref format-context control-string context))
              (ctrl-start  (car (first clauses)))
              (ctrl-end    (cdr (first clauses))) )

          ;; ~< ... ~:@>
          ;;
          (when (logbitp 1 close-modifier)
            (setq ctrl-string (format-insert-fill ctrl-string
                                                  ctrl-start ctrl-end ))
            (setq ctrl-start 0)
            (setq ctrl-end   (length ctrl-string)) )

          ;; Call pretty-printer
          ;;
          (if (not (listp arg))
              ;; Simple optimization
              (write-object arg stream)
            (xc::pprint-logical-block-aux (stream stream
                                       arg
                                       prefix (logbitp 1 clause1-modifier)
                                       suffix )
              (catch 'up-and-out
                (format-interpreter-4
                  stream context
                  ctrl-string ctrl-start ctrl-end
                  arg
                  #'(lambda () (pprint-pop)) )) )) ) )

      ((logbitp 0 clause1-modifier)
        ;; ~< overflow ~:; left ~; text ... ~; right ~>
        ;;
        (when (logbitp 1 clause1-modifier)
          (format-error-invalid-modifier context) )

        (let ((segments '())
              (spare    0)
              (width    72) )

          (catch 'up-and-out
            (loop
              for scan on clauses
              for (ctrl-start . ctrl-end) = (first scan)
              do
                (push (format-to-string stream context ctrl-start ctrl-end)
                       segments )

                 ;; Parse the first "~;"
                 ;;
                 (when (eq clauses scan)
                   (setq ctrl-start (1+ ctrl-end))
                   (setq ctrl-end   (car (second clauses)))
                   (let ((nparams 0))
                     (loop
                       (multiple-value-bind (stop-char param ctrl-index)
                           (format-parse-param context ctrl-start ctrl-end)
                         (unless (= (1+ ctrl-start) ctrl-index)
                           (incf nparams)
                           (case nparams
                             (1 (setq spare param))
                             (2 (setq width param))
                             (otherwise
                               (setf (ref format-context control-index context)
                                     ctrl-index )
                               (format-error-too-many-params context) )))
                         (unless (char= #\, stop-char) (return))
                         (setq ctrl-start ctrl-index) )) ))))

          (setq segments (nreverse segments))

          (format-justify stream (rest segments)
                          (first segments) spare width
                          (modifier) mincol colinc minpad padchar ) ) )

      (t
        ;; ~< left ~; text ... ~; right ~>
        ;;
        (when (logbitp 1 clause1-modifier)
          (format-error-invalid-modifier context) )

        (let ((segments '()))
          (catch 'up-and-out
            (loop
              for (ctrl-start . ctrl-end) in clauses do
                (push (format-to-string stream context ctrl-start ctrl-end)
                       segments )))
          (setq segments (nreverse segments))

          (format-justify stream segments
                          nil 0 0
                          (modifier) mincol colinc minpad padchar ) ) )) ) )
