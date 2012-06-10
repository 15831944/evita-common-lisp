(in-package :win32)

(deftype blob (&optional n) `(simple-array (unsigned-byte 8) (,n)))
(deftype rect ()  '(simple-array (signed-byte 32) (4)))

(macrolet (
  (define (name nth)
    (let ((fn (intern (format nil "RECT-~A" name))))
     `(progn
        (defun ,fn (rc)
            (declare (type (signed-byte 32)))
            (declare (type rect rc))
          (elt rc ,nth) )

        (defun (setf ,fn) (i rc)
            (declare (type (signed-byte 32)))
            (declare (type rect rc))
          (setf (elt rc ,nth) i) )

        (declaim
          (ftype (function (rect) (signed-byte 32))
            ,fn )

          (ftype (function ((signed-byte 32) rect) (signed-byte 32))
            (setf ,fn) ) )) ) )
    )
    ;;
    (define bottom 3)
    (define left   0)
    (define right  2)
    (define top    1)
 ) ; macrolet

(declaim
  (ftype (function (ext:sequence-index) blob)
    make-blob )

  (ftype (function (&optional integer) si::handle)
    make-handle )

  (ftype (function (&key (:bottom fixnum)
                         (:left   fixnum)
                         (:right  fixnum)
                         (:top    fixnum) )
                   rect )
    make-rect )

  (ftype (function (rect) (signed-byte 32))
    rect-height
    rect-width )
 ) ; declaim
