;;;; parse-infix-stream
;;; Description:
;;;  Returns list form infix expression in stream.
(defun parse-infix-stream (stream)
    (declare (values t))
    (declare (type stream stream))
  (let ((unget nil))
  (labels (
    ;; parse-expr
    (parse-expr ()
      (parse-add-sub) )

    ;; parse-add-aub
    (parse-add-sub ()
      (loop
        with expr = (parse-mul-div)
        for token = (get-token)
        while (or (eq token '+) (eq token '-)) do
          (setq expr `(,token ,expr ,(parse-mul-div)))
        finally
          (unget-token token)
          (return expr) ) )

    ;; parse-mul-div
    (parse-mul-div ()
      (loop
        with expr = (parse-term)
        for token = (get-token)
        while (or (eq token '*) (eq token '/)) do
          (setq expr `(,token ,expr ,(parse-term)))
        finally
          (unget-token token)
          (return expr) ) )

    ;; parse-term
    (parse-term ()
      (let ((token (get-token)))
        (cond
          ((realp token) token)
          ((eq token '-)
            (let ((expr (parse-term)))
              (if (realp expr) (- expr) `(- ,expr)) ) )
          ((eq token '+) (get-token))
          ((eq token :open)
            (let ((expr (parse-expr)))
              (unless (eq (get-token) :close)
                (err "Unmatched open parenthesis.") )
              expr ) )
          ((eq token :eof) token)
          ((symbolp token) token)
          ((consp token) token)
          (t (err "Invalid token: ~S" token)) ) ) )


    ;; parse-aref (name)
    (parse-aref (name)
      (let ((expr `(aref ,name ,(parse-expr))))
        (unless (eq (get-token) :aref)
          (err "Expect close braket.") )
        expr ) )

    ;; parse-funcall
    (parse-funcall (name)
      (let ((expr (list name))
            (token (get-token)) )
        (if (eq token :close)
            expr
          (let ((last expr))
            (unget-token token)
            (loop
              (setq last (setf (cdr last) (list (parse-expr))))
              (let ((token (get-token)))
                (case token
                  ((:close) (return expr))
                  ((:comma))
                  (otherwise (err "Invalid funcall")) ) )))) ) )

    ;; get-token
    (get-token ()
      (let ((token (get-token-aux)))
        #+nil (format t "; get-token ~S~%" token)
        token ) )

    ;; get-token-aux
    (get-token-aux ()
      (if unget
          (shiftf unget nil)
        (let ((ch
                (loop
                  for ch = (read-char stream nil)
                  while (member ch '(#\Space #\Tab #\Newline #\Page))
                  finally (return ch) ) ))
          (cond
            ((null ch) :eof)
            ((digit-char-p ch)
              (get-token-real ch) )
            ((alpha-char-p ch)
              (get-token-var ch) )
            ((eql ch #\-) '-)
            ((eql ch #\+) '+)
            ((eql ch #\*) '*)
            ((eql ch #\-) '-)
            ((eql ch #\/) '/)
            ((eql ch #\,) :comma)
            ((eql ch #.(code-char #x28)) :open)
            ((eql ch #.(code-char #x29)) :close)
            ((eql ch #.(code-char #x5D)) :aref)
            ((eql ch #\;) :eof)
            (t (err "Invalid character: ~S" ch)) ) )) )

    ;; get-token-real
    ;;  digit* [.] digit+ [ e|E [+|-] digit+]
    (get-token-real (ch)
      (labels (
        (collect (ch)
          (loop
            with n = (or (digit-char-p ch) (err "Expect digit: ~S" ch))
            for k = 1 then (1+ k)
            for ch = (read-char stream nil)
            for digit = (and ch (digit-char-p ch))
            while digit do
              (setq n (+ (* n 10) digit))
            finally
              (when ch (unread-char ch stream))
              (return (values n k)) ) )

        ;; parse-e
        (parse-e ()
          (let ((ch (read-char stream)))
            (cond
              ((eql ch #\+) (collect (read-char stream)))
              ((eql ch #\-) (- (collect (read-char stream))))
              (t (collect ch)) ) ) )
        )
        (let ((n (collect ch))
              (ch (read-char stream nil)) )
          (when (eql ch #\.)
            (multiple-value-bind (f k) (collect (read-char stream))
              (setq n (float (+ n (/ f (expt 10 k))) 0d0)) )
            (setq ch (read-char stream nil)) )
          (case ch
            ((nil) n)
            ((#\e #\E)
              (let ((e (parse-e)))
                (if (minusp e)
                    (/ (float n 0d0) (expt 10 (- e)))
                  (float (* n (expt 10 e)) 0d0) ) ) )
            (otherwise
              (unread-char ch stream)
              n )) ) ) )

    ;; get-token-var
    (get-token-var (ch)
      (labels (
        (collect (ch)
          (with-output-to-string (s)
            (write-char (char-upcase ch) s)
            (loop for ch = (read-char stream nil)
              while (and ch (or (alphanumericp ch) (eql ch #\_))) do
                (write-char (char-upcase ch) s)
              finally (when ch (unread-char ch stream)) ) ) )
        )
        (let ((name (intern (collect ch)))
              (ch (read-char stream nil)) )
          (case ch
            ((nil) name)
            ((#.(code-char #x28))
              (parse-funcall name) )
            ((#.(code-char #x5B))
              (parse-aref name) )
            (otherwise
              (unread-char ch stream)
              name )) ) ) )

    ;; unget-token
    (unget-token (token)
      (when unget (err "Unget buffer is full (~S)." unget))
      (setq unget token) )

    ;; err
    (err (control &rest args)
      ;; FIXME 2007-03-12 string-input-column doesn't have stream-line-column
      ;; method. We should implement it.
      ;;(format t "Syntax error at ~D." (stream-line-column stream))
      (apply 'error control args) )
    )
    ;;
    (prog1
        (parse-expr)
      (let ((token (get-token)))
         (unless (eq token :eof)
           (format t "Extra token ~S" token) ) )) ) ) )

;;;; parse-infix
;;; Description:
;;;  Returns list form infix expression in string.
(defun parse-infix (string)
    (declare (values t))
    (declare (type string string))
  (with-input-from-string (s string)
    (parse-infix-stream s) ) )
