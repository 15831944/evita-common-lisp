(in-package :win32)

(declaim
  (ftype (function (font) fixnum)
         font-ascent
         font-descent
         font-height
         font-width )
  (ftype (function (drawable x-point y-point fixnum rect
                    string sequence-index sequence-end )
                   t )
         !draw-text )
  (ftype (function (drawable string sequence-index sequence-end)
                   (values fixnum fixnum) )
         !get-text-extent )
  (ftype (function (&key (:charset   font-charset)
                         (:family    string)
                         (:height    fixnum)
                         (:italic    t)
                         (:strike    t)
                         (:underline t)
                         (:weight    font-weight)
                         (:width     0) )
                   font )
         make-font )
  (ftype (function (font) font)
         realize-font )
  ) ; declaim
