(in-package :tc)

;;;; %deftest
(defun %deftest (name &key plist documentation test-form expected)
  (labels (
    ;; intern-tc
    (intern-tc (name)
      (let ((tc (gethash name *test-cases*)))
        (if (null tc)
            (setf (gethash name *test-cases*) (make))
          (progn
            (setf (test-case-form           tc) test-form)
            (setf (test-case-expected       tc) expected)
            (setf (test-case-documentation  tc) documentation)
            (setf (test-case-plist tc) plist)
            tc )) ) )

    ;; make
    (make ()
      (let* ((tail (test-case-prev *test-case-anchor*))
             (tc  (make-test-case
                    :name           name
                    :form           test-form
                    :expected       expected
                    :documentation  documentation
                    :plist          plist
                    :prev           tail
                    :next           *test-case-anchor* ) ))
        (setf (test-case-prev *test-case-anchor*) tc)
        (setf (test-case-next tail) tc) ) )
    )
    ;;
    (let ((dependee-list
            (prog1
                (mapcar #'intern-tc (getf plist :depend-to))
              (remf plist :depend-to) ) )
          (tc (intern-tc name)) )

      (dolist (dependee dependee-list)
        (setf (test-case-depender-list dependee)
          (delete tc (test-case-depender-list dependee)) ) )

      (dolist (dependee dependee-list)
        (push tc (test-case-depender-list dependee)) )

      (setf (test-case-dependee-list tc) dependee-list)

      name ) ) )


;;;; find-test
(defun tc:find-test (name &optional (error-p t))
  (or (gethash name *test-cases*)
      (when error-p (error "No such test case ~S" name)) ) )


;;;; run-test
(defun tc:run-test (name &key (verbose t) (safe t))
    (declare (type (or symbol test-case) name))
    (declare (values boolean))
  (labels (
    ;; check-result
    (check-result (tc kind value)
        (let ((expected (test-case-expected tc)))
        (ecase kind
          ((:error)
            (and (eq (first expected) :error)
                 (typep value (second expected)) ) )
          ((:normal)
            (ecase (first expected)
              ((:boolean) (eq (not (null (first value))) (second expected)))
              ((:error)   nil)
              ((:primary) (sequence-equal (first value) (second expected)))
              ((:values)  (sequence-equal value (second expected))) ) )) ) )

    ;; run
    (run (tc)
      (if safe
          (run-safe tc)
        (eval (test-case-form tc)) ) )

    ;; run-safe
    (run-safe (tc)
      (multiple-value-bind (kind value)
          (handler-case
              (let ((form (test-case-form tc)))
                (values :normal (multiple-value-list (eval form))) )
            (error (c) (values :error c)) )
        (cond
          ((check-result tc kind value))
          (verbose
            (format t "; form:~&~:W~%" (test-case-form tc))
            (format t "; expect: ~S~%" (test-case-expected tc))
            (if (eq kind :error)
                (format t "; error: ~A~%" value)
              (format t "; result: ~S~%" value) ) )) ) )
    )
    ;;
    (let ((tc (if (typep name 'test-case) name (find-test name))))
      (let ((*package* (find-package :tc-user)))
        (not (null (run tc))) ) ) ) )


;;;; run-tests
(defun tc:run-tests (&key (verbose t) (priority 9))
  (let* ((head (list 0))
         (tail head) )
  (labels (
    ;; filter
    (filter (tc pri)
      (and (not (getf (test-case-plist tc) 'ignore))
           (eql (getf (test-case-plist tc) :priority 9) pri)
           (not (eq (gethash (test-case-name tc) *test-results*)
                    :succeeded ))) )

    ;; mark-failed
    (mark-failed (tc)
      (let ((name (test-case-name tc)))
        (format t "; FAILED: ~S~%" name)
        (setq tail (setf (cdr tail) (list tc)))
        (setf (gethash name *test-results*)
            (ecase (gethash name *test-results*)
              ((nil) :failed)
              ((:failed) :failed)
              ((:succeeded) :regression) )) ) )

    ;; mark-succeeded
    (mark-succeeded (tc)
      (setf (gethash (test-case-name tc) *test-results*) :succeeded) )

    ;; report-summary
    (report-summary ()
      (let* ((fails (cdr head))
             (ncases (hash-table-count *test-cases*))
             (nsucceedes (- ncases (length fails))) )
        (when verbose
          (format t "; ~D/~D ~S%~%"
            nsucceedes ncases (/ (float nsucceedes) ncases) ))
        (mapcar #'test-case-name fails) ) )

    ;; run-test-1
    (run-test-1 (tc)
      (when (and (not (succeeded-p tc))
                 (every #'succeeded-p (test-case-dependee-list tc)) )
        (when verbose (format t "~S~%" (test-case-name tc)))
        (if (not (run-test tc))
            (mark-failed tc)
          (let ((succeeded t))
            (mark-succeeded tc)
            (dolist (depender (test-case-depender-list tc) succeeded)
              (unless (run-test-1 depender)
                  (setq succeeded nil) ) ) )) ) )

    ;; succeeded-p
    (succeeded-p (tc)
      (eq (gethash (test-case-name tc) *test-results*) :succeeded) )
    )
    ;;
    (dotimes (pri (1+ priority) (report-summary))
      (loop
        for runner = (test-case-next *test-case-anchor*)
                then (test-case-next runner)
        until (eq runner *test-case-anchor*)
          when (filter runner pri)
            do (run-test-1 runner) ) ) ) ) )


;; sequence-equal = equal + vector-equal
(defun sequence-equal (x y)
  (labels (
    (vector-equal (x y)
      (when (and (vectorp x) (vectorp y) (eql (length x) (length y)))
        (dotimes (i (length x) t)
          (unless (sequence-equal (elt x i) (elt y i)) (return nil)) )) )
    )
    ;;
    (or (equal x y) (vector-equal x y)) ) )
