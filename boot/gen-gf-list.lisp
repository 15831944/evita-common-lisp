(in-package :cl-user)

(defmethod print-object ((x si::param-info) s)
  (format s "#<Arg-Info ~D ~D ~S ~S ~S>"
    (si::param-info-nreqs x)
    (si::param-info-nopts x)
    (si::param-info-keys x)
    (si::param-info-order x)
    (si::param-info-lambda-list x) ) )

(defun generate (f)
  (with-open-file (s f :direction :output :if-exists :supersede)
  (labels (
    ;; gen
    (gen (package)
      (do-external-symbols (x package) (try x) (try `(setf ,x))) )

    ;; try
    (try (x)
      (let ((fn (and (fboundp x) (fdefinition x))))
         (when (typep fn 'generic-function)
           (emit fn) ) ) )

    ;; emit
    (emit (gf)
      (let ((ai (slot-value gf 'si::param-info)))
        (format s "~%~4T// ~(~S ~A~)~%"
            (slot-value gf 'si::name) (si::param-info-lambda-list ai) )
        (format s "~4Tgf = install_gf(~A, ~D, ~D,~%~24T~A, ~A );~%"
          (encode (slot-value gf 'si::name))
          (si::param-info-nreqs ai)
          (si::param-info-nopts ai)
          (encode (si::param-info-keys ai))
          (encode (si::param-info-lambda-list ai)) )
        #+nil
        (dolist (mt (slot-value gf 'si::methods))
          (when (good-method-p mt)
            (format s "~8Tadd_method(gf, ~A, ~A);~%"
              (encode (method-qualifiers mt))
              (encode (mapcar #'class-name
                             (clos:method-specializers mt) )))) ) ) )

    ;; good-method-p
    #+nil
    (good-method-p (mt)
      (let ((gf (clos:method-generic-function mt)))
        (eq #'make-instance gf) ) )

    ;; good-method-p
    #+nil
    (good-method-p (mt)
      (dolist (s (clos:method-specializers mt) t)
        (unless (typep s 'class) (return nil))
        (let* ((name (class-name s))
               (p (symbol-package name)) )
          (unless (or (eq (find-package :cl) p)
                      (eq (find-package :clos) p) )
            (return nil) ) ) ) )

    ;; encode
    (encode (x)
      (etypecase x
        (null "nil")
        ((eql t) "t")
        (integer
          (format nil "Fixnum::Encode(~D)" x) )
        (cons
          (case (list-length x)
            ((1) (format nil "list(~A)" (encode (car x))))
            ((2) (format nil "list(~A, ~A)"
                    (encode (first x)) (encode (second x)) ) )
            ((3) (format nil "list(~A, ~A, ~A)"
                    (encode (first x)) (encode (second x))
                    (encode (third x)) ) )
            (otherwise
              (format nil "cons(~A, ~A)"
                (encode (car x))
                (encode (cdr x)) )) ) )
        (symbol
          (let ((p (symbol-package x)))
            (cond
              ((eq (find-package :keyword) p)
                (format nil "Q(\":~A\")" (symbol-name x)) )
              ((eq (find-package :cl) p)
                (if (find-class x nil)
                    (format nil "Q~(~A~)"
                        (substitute #\_  #\- (symbol-name x)) )
                  (format nil "Q(\"~A\")" (symbol-name x)) ) )
              ((eq (find-package :clos) p)
                (format nil "Q(\"CLOS:~A\")" (symbol-name x)) )
              (t
                (format nil "Q(\"~A\")" (symbol-name x)) )) ) )) )
    )
    ;;
    (gen :cl)
    (gen :clos)

    (try 'si::default-initargs)
    (try 'ext:function-name)
    (try '(setf ext:function-name))

    ;; 19 Filenames
    (try 'si::internal-directory-namestring)
    (try 'si::internal-enough-namestring)
    (try 'si::internal-file-namestring)
    (try 'si::internal-namestring)
    (try 'si::make-pathname-using-host)
    (try 'si::parse-device-component)
    (try 'si::parse-directory-component)
    (try 'si::parse-name-component)
    (try 'si::parse-namestring-using-host)
    (try 'si::parse-type-component)
    (try 'si::parse-version-component)
    (try 'si::pathname-equal-using-host)
    (try 'si::pathname-match-p-using-host)
    (try 'si::translate-component-using-host)
    (try 'si::translate-directory-using-host)
    (try 'si::translate-pathname-using-host)
    (try 'si::truename-using-host)
    (try 'si::wild-pathname-p-using-host)

    ;; 21 Streams
    (try 'si::stream-flags)

    (try 'ext:realize-instance)

    (try 'ext:stream-advance-to-column)
    (try 'ext:stream-clear-input)
    (try 'ext:stream-clear-output)
    (try 'ext:stream-finish-output)
    (try 'ext:stream-force-output)
    (try 'ext:stream-fresh-line)
    (try 'ext:stream-line-column)
    (try 'ext:stream-line-number)
    (try 'ext:stream-listen)
    (try 'ext:stream-output-width)
    (try 'ext:stream-pathname)
    (try 'ext:stream-peek-char)
    (try 'ext:stream-read-byte)
    (try 'ext:stream-read-bytes)
    (try 'ext:stream-read-char)
    (try 'ext:stream-read-char-no-hang)
    (try 'ext:stream-read-line)
    (try 'ext:stream-read-sequence)
    (try 'ext:stream-start-line-p)
    (try 'ext:stream-terpri)
    (try 'ext:stream-truename)
    (try 'ext:stream-unread-char)
    (try 'ext:stream-write-byte)
    (try 'ext:stream-write-bytes)
    (try 'ext:stream-write-char)
    (try 'ext:stream-write-sequence)
    (try 'ext:stream-write-string)

    ) ) )

(format t "~2%Type ~(~S~)~2%" '(generate "/proj/evcl3/boot/_bt_gf.inc"))

