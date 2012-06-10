(in-package :tc-user)

(defun bool (x) (not (null x)))

(defun ignore-arguments (&rest x) (declare (ignore x)))

(defun make-displaced (seq)
 (let ((vec (make-array (+ (length seq) 6)
                        :initial-contents
                            (append (coerce "---" 'list)
                                    (coerce seq 'list)
                                     (coerce "---" 'list) )) ))
    (make-array (length seq) :displaced-to vec :displaced-index-offset 3) ) )

(defun single-value (x) x)

(defun multiple-values (&rest x) (values-list x))

;;;; bit-to-int
;;;   Converts bit-vector to unsinged integer.
;;;   This function is used for testing bit-vector operations.
(defun bit-to-int (v)
  (loop
    with acc = 0
    for k = 1 then (ash k 1)
    for bit across v
      when (eql bit 1) do (setq acc (logior acc k))
    finally (return acc) ) )
