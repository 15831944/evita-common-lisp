;;;; logical-host
;
(defclass logical-host (basic-host)
  ((local-case      :initform   :preserved)
   (customary-case  :initform   :upcase)
   (translations    :initform   '()) )
  (:metaclass structure-class) )


;;;; logical-pathname
;
(defclass logical-pathname (pathname)
  ()
  (:metaclass structure-class) )


#|
;;;; *logical-hosts*
;
(defvar *logical-hosts* '())
(defvar *logical-hosts-latch* (make-latch '*logical-hosts*))
|#


;;;; +logical-reserved-chars+
;;;
;;; Description:
;;;  String which contains reserved characters in logical filename.
;
(defconstant +logical-reserved-chars+ ";")
