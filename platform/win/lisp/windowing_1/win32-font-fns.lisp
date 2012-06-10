(in-package :win32)

;;;; make-font
(defun make-font (&key (charset :default)
                       (family (required))
                       (height 0)
                       italic
                       strike
                       underline
                       (weight :normal)
                       (width 0) )
    (declare (values font))
    (declare (type string  family))
    (declare (type integer height))
  (macrolet (
    (enum-value (value &rest mappings)
     `(or (gethash ,value
                   (load-time-value
                     (let ((htb (make-hash-table :test 'eq)))
                       ,@(mapcar
                            (lambda (x)
                             `(setf (gethash ',(first x) htb) ',(second x)) )
                            mappings )
                       htb )))
          (error ,(format nil "Bad ~A value ~~S." value) ,value) ) )
    )
  (labels (
    (make-logfont ()
      (let* ((logfont (allocate-foreign-object (find-class 'logfont)))
             (p (si::.unbox-int (ref foreign-pointer value logfont))) )
        (setf (ref LOGFONT lfHeight      p) height)
        (setf (ref LOGFONT lfWidth       p) width)
        (setf (ref LOGFONT lfEscapement  p) 0)
        (setf (ref LOGFONT lfOrientation p) 0)

        (setf (ref LOGFONT lfWeight p)
          (enum-value weight
                    (:dont-care 0)
                    (:thin 100)
                    (:extra-light 200)
                    (:ultra-light 200)
                    (:light 300)
                    (:normal 400)
                    (:regular 400)
                    (:medium 500)
                    (:semi-bold 600)
                    (:demi-bold 600)
                    (:bold 700)
                    (:extra-bold 800)
                    (:ultra-bold 800)
                    (:heavy 900)
                    (:black 900 )))

        (setf (ref LOGFONT lfItalic    p) (if italic    1 0))
        (setf (ref LOGFONT lfUnderline p) (if underline 1 0))
        (setf (ref LOGFONT lfStrikeOut p) (if strike    1 0))

        (setf (ref LOGFONT lfCharset p)
          (enum-value charset
                      (:ansi        0)
                      (:default     1)
                      (:symbol      2)
                      (:shift_jis  128)
                      (:hangul  129)
                      (:gb2312  134)
                      (:big5  136)
                      (:oem  255)
                      (:johab  130)
                      (:hebrew  177)
                      (:arabic  178)
                      (:greek  161)
                      (:turkish  162)
                      (:vientamese  163)
                      (:thai  222)
                      (:easeurope  238)
                      (:russian  204)
                      (:mac  77)
                      (:baltic  186 )))

        (setf (ref LOGFONT lfOutPrecision p) 0)
        (setf (ref LOGFONT lfClipPrecision p) 0)
        (setf (ref LOGFONT lfQuality p) CLEARTYPE_NATURAL_QUALITY)
        (setf (ref LOGFONT lfPitchAndFamily p) 0)

        (setf (si::cstring (+ (ref foreign-pointer value logfont)
                              (offsetof 'LOGFONT 'lfFaceName) ))
              family )

        logfont ) )
    )
    ;;
    (let ((font (make-instance 'font)))
      (setf (ref font logfont font) (make-logfont))
      font ) ) ) )


;;;; cl:print-object font
(defmethod cl:print-object ((o font) s)
  (when (slot-boundp o 'logfont)
    (print-unreadable-object (o s :type t :identity t)
      (prin1
        (si::cstring (+ (ref foreign-pointer value (slot-value o 'logfont))
                        (offsetof 'LOGFONT 'lfFaceName) ))
        s )
      (when (slot-value o 'handle)
        (format s " ~X" (ref si::handle value (slot-value o 'handle))) )))
  o )


;;;; realize-font
(defun realize-font (font)
    (declare (values font))
    (declare (type font font))
  (setf (slot-value font 'handle)
    (|CreateFontIndirect| (slot-value font 'logfont)) )
  (setf (slot-value font 'text-metric)
    (allocate-foreign-object (find-class 'TEXTMETRIC)) )
  font )
