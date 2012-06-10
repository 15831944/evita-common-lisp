(defun cl:array-dimension (x axis)
  (etypecase x
    (vector
      (if (eql axis 0)
          (array-total-size x)
        (error 'type-error :datum axis :expected-type '(integer 0 0)) ) )
    (array
      (if (<= 0 axis (1- (array-rank x)))
          #+explicit-pointer
          (let ((p (make-pointer x)))
            (pref sequence-index (pointer+ p (!offset-of array dimensions)))
          #+address-of
          (let ((p (!address-of (ref array dimensions x))))
            (!elt 'sequence-index p axis) )
          )
        (error 'type-error
            :datum axis
            :expected-type '(integer 0 ,(array-rank x)) )) )) )

(defun cl:array-dimensions (x)
  (etypecase x
    (vector
      (list (array-total-size x)) )
    (array
      (let* ((dims (list 0)) (tail dims))
        #+explicit-pointer
        (let ((p (pointer+ (make-pointer x) (!offset-of array dimensions))))
          (dotimes (axis (array-rank x) (cdr dims))
            (setq tail (setf (cdr tail) (list (pref sequence-index p))))
            (pointer+ p (!size-of sequnce-index)) ) )
        #+address-of
        (let ((p (!address-of (ref array dimensions x))))
          (dotimes (axis (array-rank x) (cdr dims))
            (setq tail (setf (cdr tail)
              (list (!elt 'sequence-index p axis)) )) ) )

        #+low-level-operator
        (let ((index (!offset-of array dimensions)))
          (dotimes (axis (array-rank x) (cdr dims))
            (setq tail (setf (cdr tail)
              (list (!elt 'sequence-index x index)) )) ) )
        ) )) )

:allocation :flexible ; ; from C99 flexible array
:allocation :vector     ; allow !elt

(defclass record (t)
  ((classd :type class-description)
   (fields :type t :allocation :vector) ) )

(defclass native-code-function (function)
  ((classd  :type class-description)
   (length  :type sequence-index)
   (name    :type (or cons symbol))
   (cookie  :type fixnum)
   (codevec :type (unsigned-byte 8) :allocation :vector) ) )

(defclass simple-vector (data-vector)
  ((elements :type t :allocation :vector)) )

(defclass simple-string (data-vector)
  ((elements :type character :allocation :vector)) )

(defclass u8-vector (data-vector)
  ((elements :type (unsigned-byte 8) :allocation :vector)) )

(defclass f32-vector (data-vector)
  ((elements :type single-float :allocation :vector)) )

;; If allocation is vector and type is compatible with native-type,
;; we use native-type.

;; For 32bit architecture,
;;  (unsigned-byte [1...29])     => fixnum
;;  (unsigned-byte {30, 31, 32}) => native


;; implicit pointer object
BAD !address-of form => pointer [Macro]
    form ... (ref class-name slot-name x) or object

REVIEW !offset-of class-name slot-name
    => Use :vector allocation slot.

REVIEW !length-of object => sequence-index
    for inspector?

BAD !tag class-name
  We introduce tag at lowering of ELT instruction ADD instruction.
  ELT (ptr <type>) %rd <- vector index
  ==>
  ADD int %r1 <- index (tag + offsetof vector)
  ADD (ptr <type>) %rd <- vector %r1

!box type native-object

BAD !scale native-type fixnum => native-integer

!elt 'lisp-type-or-native-type object index => object-or-native-value
    for accessing vector elements.
    index = scaled index

!elt* 'lisp-tyoe-or-native-type object byte-index => object-or-native-value 

!- form...
    for delta2 annotation

(defun cl:schar (string index)
  (check-type string simple-string)
  (if (<= 0 index (1- (length string)))
      (!box character
        (!elt 'uint16 (!address-of string elements) (!scale uint16 index)) )
    (error 'sequence-index-error :sequence string :index index) ) )

(defun elt/vector (vector index)
  (multiple-value-bind (index data) (vector-data vector index)
    (etypecase data
      (simple-vector     (svref data index))
      (simple-string     (schar data index))
      (simple-bit-vector (sbit  data index))

      ;; UInt
      ((simple-array (unsigned-byte 8) (*))
        (let ((ofs (+ (!offset-of (simple-array (unsigned-byte 8) (*))
                                  elements )
                      (!scale uint8 index) )) )
          (!box fixnum (!elt 'uint8 data offset)) ) )
      ((simple-array (unsigned-byte 16) (*))
        (!box fixnum (!elt 'uint16 data (!scale uint16 index))) )
      ((simple-array (unsigned-byte 32) (*))
        (!box integer (!elt 'uint32 data (!scale uint32 index))) )
      ((simple-array (unsigned-byte 64) (*))
        (!box integer (!elt 'uint64 data (!scale uint64 index))) )

      ;; Float
      ((simple-array single-float (*))
        (!box single-float (!elt 'float32 data (!scale float32 dindex))) )
      ((simple-array double-float (*))
        (!box double-float (!elt 'float64 data (!scale float64 index))) )

      ;; Complex
      ((simple-array (complex single-float) (*))
        (complex
          (!box single-float
            (!elt 'float32 (!scale float32 (+ (* index 2) 0))) )
          (!box single-float
            (!elt 'float32 (!scale float32 (+ (* index 2) 1))) )) )

      ((simple-array (complex double-float) (*))
        (complex
          (!box double-float
            (!elt 'float64 (!scale float64 (+ (* index 2) 0))) )
          (!box double-float
            (!elt 'float64 (!scale float64 (+ (* index 2) 1))) )) )
    ) ) )

;; Compiler inserts SCALE instruction from type of !elt.
(defun elt/vector (vector index)
  (multiple-value-bind (index data) (vector-data vector index)
    (etypecase data
      (simple-vector     (svref data index))
      (simple-string     (schar data index))
      (simple-bit-vector (sbit  data index))

      ;; UInt
      ((simple-array (unsigned-byte 8) (*))
        (!box fixnum (!elt 'uint8 data index)) )
      ((simple-array (unsigned-byte 16) (*))
        (!box fixnum (!elt 'uint16 data index)) )
      ((simple-array (unsigned-byte 32) (*))
        (!box integer (!elt 'uint32 data index)) )
      ((simple-array (unsigned-byte 64) (*))
        (!box integer (!elt 'uint64 data index)) )

      ;; Float
      ((simple-array single-float (*))
        (!box single-float (!elt 'float32 data index)) )
      ((simple-array double-float (*))
        (!box double-float (!elt 'float64 data index)) )

      ;; Complex
      ((simple-array (complex single-float) (*))
        (complex
          (!box single-float
            (!elt 'float32 (+ (* index 2) 0)) )
          (!box single-float
            (!elt 'float32 (+ (* index 2) 1)) )) )

      ((simple-array (complex double-float) (*))
        (complex
          (!box double-float
            (!elt 'float64 (+ (* index 2) 0)) )
          (!box double-float
            (!elt 'float64 (+ (* index 2) 1)) )) )
    ) ) )

(!defstruct FunObj.Desc
  (uint32   m_ofsGcMap)
  (uint32   m_ofsAnnon)
  (uint32   m_cbCodeVec)
  (uint32   m_nFrame) )

(defun function-annotations (fn)
    (declare (type native-code-function fn))
    (declare (values list))
  (labels (
    ;; decode
    (decode (runner delta1)
      (multiple-value-bind (ofs typ)
          (let ((frob (!elt* 'uint32 fn runner)))
             (values (ldb (byte 28 4) anon) (ldb (byte 4 0) anon)) )
        (multiple-value-bind (kind datum) (decode-aux ofs typ delta1)
          (values kind ofs datum) ) ) )

    ;; decode-aux
    (decode-aux (ofs typ delta1)
      (multiple-value-bind (kind datum)
        (ecase (ldb (byte 4 0) anon
          ((0)    ; lispval
            (values :lispval (!elt* 't fn ofs)) )
          ((1)    ; named-callee
            (values :named-callee
              (function-name
                (!box function
                  (- (!elt* 'uint ofs)
                     (!offset-of native-code-function code-vector) ))) ) )
          ((2)    ; local-callee
            (values :local-callee
              (!box function
                  (- (!elt* 'uint ofs)
                     (!offset-of native-code-function code-vector) ))) )
          ((4)    ; symbol-function
            (values :symbol-function
               (!box symbol
                   (- (!elt* 'uint ofs)
                      (!offset-of symbol function) )) ) )
           ((5)    ; symbol-value
            (values :symbol-value
              (ref value-cell name
                  (!box value-cell
                      (- (!elt* 'uint ofs)
                         (!offset-of value-cell value) )) )) )
           ((6)    ; symbol-set-function
            (values :setf
              (ref setf-cell name
                  (!box setf-cell
                      (- (!elt* 'uint ofs)
                         (!offset-of setf-cell function) )) ) ) )
           ((7)    ; delta2
              (values :delta2 (!- delta1 (!elt* 'uint ofs))) )
           ((8)    ; tlv-offset
              (values :tlv-offset
                  (- (!box fixnum (!elt* 'uint32 ofs))
                     (!offset-of thread tlv) )) )
           ((9)
              (values ...) )

    )
    ;;
    (let* ((desc      (- (ref native-code-function size fn) 16))
           (anon.list (!elt* 'uint32 fn (+ desc 1)))
           (anon.size (!elt* 'uint32 fn anon)) )
      (with-collector (collect)
        (loop
          for runner = (+ annon.list 4) then (+ runner 4)
          with delta1 = nil
          while (> anon.size 4) do
            (multiple-value-bind (kind ofs datum) (decode runner delta1)
              (collect (list kind ofs datum)) )) ) ) ) )


;; For pointer object, we should have Scalar Replacment of Aggregates (SRA)
make-pointer object => pointer
pointer+ pointer fixnum => pointer ... destruvtive update
pointer= pointer1 pointer2 => generaized-boolean
pointer- pointer fixnum => pointer (?)
pref type pointer [Macro]
!pref type pointer [Function]
!offset-of class-name slot-name [Macro]
!size-of type [Macro]
    type can be
        int, int{8,16,32,64}
        uint, uint{8,16,32,64}
        float{32,64}
