;;; From http://www.hackersdelight.org/HDcode/magic.c
(defun idiv-magic (d)
    (declare (values (signed-byte 32) (integer 0 31)))
    (declare (type (signed-byte 32)))
  (when (<= -1 d 1) (error "Can't compute [-1, 1]: ~D" d))
  (labels ((modn (i) (logand (unsigned i) #.(1- (ash 1 32))))
           (unsigned (i) (if (minusp i) (+ #.(ash 1 32) i) i)) )
  (let* ((two31 (ash 1 31))
         (ad (abs d))
         (tt (if (minusp d) (1+ two31) two31))
         (anc (- tt 1 (rem tt ad)))
         (p 31)
         (q1 (truncate two31 anc))
         (r1 (modn (- two31 (* q1 anc))))
         (q2 (truncate two31 ad))
         (r2 (modn (- two31 (* q2 ad)))) )
      (declare (type (unsigned-byte 32) q1 r1 q2 r2))
    (loop
      (setq p (+ p 1))
      (setq q1 (modn (* q1 2)))
      (setq r1 (modn (* r1 2)))

      (when (>= r1 anc)
        (setq q1 (+ q1 1))
        (setq r1 (modn (- r1 anc))) )

      (setq q2 (modn (* q2 2)))
      (setq r2 (modn (* r2 2)))

      (when (>= r2 ad)
         (setq q2 (+ q2 1))
         (setq r2 (modn (- r2 ad))) )

     (let ((delta (modn (- ad r2))))
       (unless (or (< q1 delta)
                   (and (eql q1 delta) (eql r1 0)) )
         (let ((m (+ q2 1))
               (s (- p 32)) )
           (return (values (if (minusp d) (- m) m) s)) )) )) ) ) )

(eval-when (:load-toplevel :execute)
  (unless (find-class 'si::assertion-failure nil)
    (defclass si::assertion-failure (error) ((si::form :initarg :form))) ) )

(defun test-it (d expect)
  (let ((result (multiple-value-list (idiv-magic d))))
    (unless (equal result expect)
      (format t "(idiv-magic ~D) expect ~S but ~S."
        d expect result ) ) ) )

;(test-it 3 '(#x55555556 0))
;(test-it 5 '(#x33333334 0))
;(test-it 7 '(#x92492493 2))

(defun idiv (n d)
    (declare (values (signed-byte 32)))
    (declare (type (signed-byte 32) n d))
  (cond
    ((eql d 0)
      (error 'division-by-zero :operator '/ :operands (list n d)) )
    ((eql d 1)
      n )
    ((eql d -1)
      ;; (- n) can be overflow if n=#x-80000000.
      (logand (- n) #.(ash 1 32)) )
    (t
      (multiple-value-bind (m s) (idiv-magic d)
        (ash (* n m) (- -32 s)) ) )) )


