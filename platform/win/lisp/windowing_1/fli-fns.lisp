(in-package :si)

;;;; copy-byte-16
(defun copy-byte-16 (dst src len)
    (declare (values null))
    (declare (type integer dst src))
    (declare (type sequence-index len))
  (loop
    for n from len downto 0
    for ch = (ref uint16* value (.unbox-int src)) do
      (setf (ref uint16* value (.unbox-int dst)) ch)
      (incf dst 2)
      (incf src 2) ) )


;;;; strlen
(defun strlen (s)
    (declare (values sequence-index))
    (declare (type integer s))
  (loop
    for n of-type sequence-index = 0 then (1+ n)
    for p = s then (+ p 2)
    for ch = (ref uint16* value (.unbox-int p))
    until (eql ch 0)
    finally (return n) ) )


;;;; cstring
(defun cstring (s)
    (declare (values simple-string))
    (declare (type integer s))
  (let* ((len (strlen s))
         (str (make-string len)) )
    (copy-byte-16 (+ (.box-int str)
                     (- (sizeof 'simple-string) (tagof 'simple-string)) )
                  s len)
    str ) )


;;;; (setf cstring)
(defun (setf cstring) (str s &optional (start 0) end)
    (declare (values null))
    (declare (type string str))
    (declare (type integer s))
  (multiple-value-bind (string start end) (string-data str start end)
    (loop
      with p = s
      for idx from start below end
      for ch = (char-code (schar string idx)) do
        (setf (ref uint16* value (.unbox-int p)) ch)
        (incf p 2)
      finally
        (setf (ref uint16* value (.unbox-int p)) 0) ) ) )


;;;; null-pointer-p
(defun null-pointer-p (p)
    (declare (values t))
    (declare (type foreign-pointer p))
  (zerop (ref foreign-pointer value p)) )


;;;; cl:print-object
(defmethod cl:print-object ((o foreign-pointer) s)
  (print-unreadable-object (o s)
    (format s "Foreign-Pointer ~S #x~X"
        (class-name (slot-value o 'type))
        (slot-value o 'value) ))
  o )


;;;; pointer-eq
(defun pointer-eq (p q)
    (declare (values t))
    (declare (type foreign-pointer p q))
  (eql (ref foreign-pointer value p) (ref foreign-pointer value q)) )




(defun dump (fp)
  (loop
    for p = (slot-value fp 'value) then (1+ p)
    for i below 256 do
      (when (zerop (mod i 16)) (format t "~%~8,'0X -" p))
      (format t " ~2,'0X" (ref si::uint8* value (si::.unbox-int p))) ) )


(defun des (fp)
  (labels (
    ;; slot-access
    (slot-access (fp slotd)
      (let ((p (.unbox-int (+ (slot-value fp 'value)
                              (clos:slot-definition-location slotd) )) ))
        (case (clos:slot-definition-type slotd)
          ((int16)
            (ref int16* value p) )
          ((int32)
            (ref int32* value p) )
          ((int8)
            (ref int8* value p) )
          ((uint16)
            (ref uint16* value p) )
          ((uint32)
            (ref uint32* value p) )
          ((uint8)
            (ref uint8* value p) )
          (otherwise
            (ref uint32* value p) )) ) )
    )
    ;;
    (loop
      for slotd in (clos:class-slots (slot-value fp 'type)) do
        (format t "~S ~D~%"
                  (clos:slot-definition-name slotd)
                  (slot-access fp slotd) )) ) )
