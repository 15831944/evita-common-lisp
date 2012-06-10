(in-package :si)

(defun xref-html (filename names)
  (with-open-file (out filename :direction :output :if-exists :supersede)
  (labels (
    ;; output-footer
    (output-footer ()
      (format out "</body></html>~%") )

    ;; output-header
    (output-header ()
      (format out "<html>~%")
      (format out "<body>~%")
      (format out "<h1>Cross Reference</h1>~%")
      (multiple-value-bind (s n h d m y) (get-decoded-time)
        (format out "<p>Generated at ~D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D.</p>~%"
            y m d h n s ) ) )

    ;; output-xref
    (output-xref (name)
      (let ((vec (cdr (gethash name *caller-table*)))
            (callees nil) )
        (dotimes (i (length vec))
          (let ((caller (svref vec i)))
            (when (functionp caller)
              (unless callees
                (format out "<h2>~S</h2><ol>~%" name)
                (setq callees t) )
              (format out "<li>~S</li>~%" (function-name caller)) ) ) )
        (when callees (format out "</ol>~%")) ) )
    )
    ;;
   (output-header)
   (dolist (name names) (output-xref name))
   (output-footer) ) ) )

(defun xref-pred ()
  (xref-html "/proj/evcl3/doc/xref.html" '(
        symbol
        packagep
        complexp floatp integerp numberp random-state-p rationalp realp
        character
        atom consp listp
        arrayp bit-vector-p simple-bit-vector-p simple-vector-p vectorp
        simple-string-p stringp
        hash-table-p
        pathnamep
        streamp
        readtablep
        ) ) )
