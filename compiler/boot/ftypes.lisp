
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 03 Evaluation and Compilation
;;
(c::function-information
    (ext:function-name &optional (or environment null))
    (values (or (member :function :macro :special-operator nil))
            t
            list ) )

(c::variable-information
    (symbol &optional (or null si::environment))
    (values (or (member :lexical :special :constant :symbol-macro nil))
            t
            list ) )

(dotted-rest-argument (t) nil)
(odd-number-of-keyword-arguments (t) nil)

(runtime-environment-p  (environment) t)
(toplevel-environment-p (environment) t)
(toplevel-environment ((or environment null)) environment)

(fun-info (function-name symbol &optional (or environment null))
    (values t t) )

(var-info (symbol symbol &optional (or environment null))
    (values t t) )

((setf fun-info) (t function-name symbol &optional (or environment null)) t)
((setf var-info) (t symbol symbol &optional (or environment null)) t)

;;; m00-fns.lisp
(c::analyze-body
    (list &optional t)
    (values list list (or string null)) )

(c::analyze-lambda-list
    (list &optional t t)
    (values list list list list list list list) )

(c::constant-value-p (t (or environment null)) (values t t))

(c::macro-error (format-control &rest t) nil)

(c::make-keyword-arguments-parser (symbol list t) (values list list))

(c::parse-destructuring-bind
    (list symbol t list t &optional symbol t)
    (values form symbol) )

(c::parse-lambda-list (list) (values list list list list list))
(c::parse-macro-aux (symbol list list) (values list (or string null)))

(c::syntax-error (form t &optional t t t) nil)
(c::style-warn (format-control &rest t) null)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 04 Types and Classes
;;

(find-type (symbol &optional t (or environment null))
  (or function symbol cons) )

((setf find-type)
  ((or function symbol cons) symbol &optional t (or environment null))
  (or function symbol cons) )

(parse-type (type-specifier (or environment null)) (values t t))

(parse-sequence-type (type-specifier)
    (values (or list vector) type-specifier ext:sequence-index) )

(vector-class (type-specifier &optional (or environment null)) class)

(c::parse-type-macro (symbol list list) (values list (or string null)))

(c::safe-subtypep
    (type-specifier type-specifier &optional (or environment null))
    (values t t) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 07 Objects
;;

(si::intern-class (symbol &optional (or environment null)) class)

;; [A]
(clos:accessor-method-slot-definition (accessor-method) slot-definition)
(clos:add-dependent (clos:metaobject t) ext:unspecified)
(clos:add-direct-method (clos:specializer method) ext:unspecified)
(clos:add-direct-subclass (class class) ext:unspecified)

;; [C]
(clos:compute-applicable-methods-using-classes
    (generic-function list) list )

(clos:compute-class-precedence-list (class) list)
(clos:compute-default-initargs (class) list)
(clos:compute-discriminating-function  (generic-function) function)

(clos:compute-effective-method
    (generic-function t t) (values method list) )

(clos:compute-effective-slot-definition
    (class symbol list) effective-slot-definition )

(clos:compute-slots (class) list)

;; [D]
(clos:direct-slot-definition-class (class &rest t) class)

;; [E]
(clos:effective-slot-definition-class (class &rest t) class)

(clos:ensure-class (symbol &key &allow-other-keys) class)

(clos:ensure-class-using-class
    ((or class null) symbol &key
        (:direct-default-initargs list)
        (:direct-slots list)
        (:direct-superclasses list)
        (:name symbol)
        (:metaclass (or class symbol))
        &allow-other-keys )
    class )

(clos:ensure-generic-function-using-class
    ((or generic-function null)
     ext:function-name
     &key
        (:argument-precedence-order list)
        (:declarations list)
        (:documentation (or string null))
        (:generic-function-class (or class symbol))
        (:lambda-list list)
        (:method-class (or class symbol))
        (:method-combination method-combination)
        (:name ext:function-name)
        &allow-other-keys )
    generic-function )

(clos:eql-specializer-object (eql-specializer) t)
(clos:extract-lambda-list (list) list)
(clos:extract-specializer-names (list) list)

(ensure-class-initargs (class list) (values class list))

;; [F]
(clos:finalize-inheritance (class) ext:unspecified)

(clos:find-method-combination
    (generic-function symbol list)
    method-combination )

(clos:funcallable-standard-instance-access
    (clos:funcallable-standard-object ext:sequence-index) t )

((setf clos:funcallable-standard-instance-access)
    (t clos:funcallable-standard-object ext:sequence-index) t )

;; [G]
(clos:generic-function-argument-precedence-order
    (generic-function) list )

(clos:generic-function-declarations
    (generic-function) list )

(clos:generic-function-lambda-list
    (generic-function) list )

(clos:generic-function-method-class
    (generic-function) class )

(clos:generic-function-method-combination
    (generic-function) method-combination )

(clos:generic-function-methods
    (generic-function) list )

(clos:generic-function-name
    (generic-function) ext:function-name )

;; [I]
(clos:intern-eql-specializer (t) clos:eql-specializer)

;; [M]
(clos:make-method-lambda
    (generic-function method list (or environment null))
    (values cons list) )

(clos:map-dependents (clos:metaobject function) ext:unspecified)

(clos:method-function (method) function)
(clos:method-generic-function (method) generic-function)
(clos:method-lambda-list (method) list)
(clos:method-specializers (method) list)

;; [R]
(clos:reader-method-class
    (class clos:direct-slot-definition &rest t)
    class )

(clos:remove-dependent (clos:metaobject t) ext:unspecified)
(clos:remove-direct-method (clos:specializer method) ext:unspecified)

(clos:remove-direct-subclass (class class) ext:unspecified)

;; [S]
(clos:set-funcallable-instance-function
    (funcallable-instance function) function )

(clos:slot-definition-allocation (clos:slot-definition) t)
(clos:slot-definition-initargs   (clos:slot-definition) list)
(clos:slot-definition-initform   (clos:slot-definition) t)

(clos:slot-definition-initfunction (clos:slot-definition)
    (or function null) )

(clos:slot-definition-name (clos:slot-definition) symbol)
(clos:slot-definition-type (clos:slot-definition) ext:type-specifier)

(clos:slot-boundp-using-class
    (class t effective-slot-definition) t )

(clos:slot-makunbound-using-class
    (class t effective-slot-definition) t )

(clos:slot-value-using-class
    (class t effective-slot-definition) t )

((setf clos:slot-value-using-class)
    (t class t effective-slot-definition) t )

(clos:specializer-direct-generic-functions
    (clos:specializer) list )

(clos:specializer-direct-methods
    (clos:specializer) list )

(clos:standard-instance-access
    (standard-object ext:sequence-index) t )

((setf clos:standard-instance-access)
    (t standard-object ext:sequence-index) t )

;; [U]
(clos:update-dependent (clos:metaobject t &rest t) ext:unspecified)

;; [V]
(clos:validate-superclass (class class) t)

;; [W]
(clos:writer-method-class
    (class direct-slot-definition &rest t) class )

;; clos/o05-dfun.lisp

(make-discriminator/reader (generic-function method) function)

(make-discriminator/reader-1
    (generic-function class (or fixnum cons)) function )

(make-discriminator/reader-n
    (generic-function class symbol) function )

(make-discriminator/writer (generic-function method) function)

(make-discriminator/writer-1
    (generic-function class (or fixnum cons)) function )

(make-discriminator/writer-n
    (generic-function class symbol) function )

(make-param-info
    (&key (:nreqs fixnum) (:nopts fixnum) (:keys (or list (eql t)))
          (:lambda-list list) (:order list) )
    param-info )

(power-2 (fixnum) fixnum)
(same-slot-location-p (class symbol) t)

(update-discriminator (generic-function) t)
(update-obsolete-instance (t) t)

;; clos/o06-cdfun.lisp
(make-discriminator/checking
    (generic-function param-info t method function)
    function )

(make-discriminator/dispatch
    (generic-function param-info list list function)
    function )

(make-temporary-lambda-list (param-info) (values list list symbol list list))

;; clos/o11-method.lisp
(std-compute-applicable-methods (generic-function list) list)

;;; gen-r07-clos
(compute-added-slots (t t) list)
(obsolete-instance-p (t) t)
(swap-instance-layout (standard-object standard-object) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 08 Structures
;;
(allocate-structure
    (structure-class &rest t) structure-object )

(ensure-structure-using-class
    ((or class null) symbol &key
        (:direct-default-initargs list)
        (:direct-slots list)
        (:direct-superclasses list)
        (:name symbol)
        (:metaclass (or class symbol))
        &allow-other-keys )
    class )

(find-structure (symbol &optional t (or environment null)) (or class null))

((setf find-structure)
    ((or class null) symbol &optional t (or environment null))
    (or class null) )

(structure-instance-access
    (structure-object ext:sequence-index) t )

((setf structure-instance-access)
    (t structure-object ext:sequence-index) t )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 09 Conditions
;;
(coerce-to-condition
    (symbol (or condition string function symbol class) list
        ext:type-specifier )
    condition )

(%assert (form list t list) null)
(%check-type (t type-specifier &optional string) t)

(runtime-type-error (type-specifier t condition-designator &rest t) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 11 Pacakges
;;
(ensure-package (package-designator) package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 12 Numbers
;;

;; BUGBUG: We should have fixnum, integer, float32 and float64 version
;; instead of real version.
;;

(truncate/2 (integer integer) (values integer integer))

(+/2 (number number) number)
(-/2 (number number) number)
(*/2 (number number) number)
(//2 (number number) number)
(-/1 (number) number)

(</2  (real real) t)
(<=/2 (real real) t)
(>/2  (real real) t)
(>=/2 (real real) t)
(=/2  (real real) t)
(/=/2 (real real) t)

(gcd/2 (integer integer) integer)

(logand/2 (integer integer) integer)
(logeqv/2 (integer integer) integer)
(logior/2 (integer integer) integer)
(logxor/2 (integer integer) integer)


;;;
;;; Functions for manupulating IEEE754 floating-point numbers.
;;;
(decode-float32 (single-float) (signed-byte 32))
(decode-float64 (double-float) (values (signed-byte 32) (signed-byte 32)))

(encode-float32 ((signed-byte 32)) single-float)
(encode-float64 ((signed-byte 32) (signed-byte 32)) double-float)

(decode-float32 (single-float) (signed-byte 32))
(encode-float32 (integer) single-float)

(decode-float64 (double-float) (values (signed-byte 32) (signed-byte 32)))
(encode-float64 (integer integer) double-float)

(integer-decode-float32 (single-float)
    (values (unsigned-byte 24)
            (integer -149 255)
            (member -1 1) ) )

(integer-decode-float64 (double-float)
    (values (unsigned-byte 53)
            (integer -1074 2047)
            (member -1 1) ) )

(scale-float32 (single-float fixnum) single-float)
(scale-float64 (double-float fixnum) double-float)

;;; float32
(float32-precision      (single-float) (integer 0 24))
(float32-rational       (single-float) rational)
(float32-rationalize    (single-float) rational)
(float32-sign           (single-float single-float) single-float)
(float32-truncate       (single-float) integer)

(float32-fceiling   (single-float) (single-float))
(float32-ffloor     (single-float) (single-float))
(float32-fround     (single-float) (single-float))
(float32-ftruncate  (single-float) (single-float))

(float32-mod (single-float single-float) (single-float))
(float32-rem (single-float single-float) (single-float))

;;; floa64
(float64-precision      (double-float) (integer 0 53))
(float64-rational       (double-float) rational)
(float64-rationalize    (double-float) rational)
(float64-sign           (double-float double-float) double-float)
(float64-truncate       (double-float) integer)

(float64-fceiling   (double-float) (double-float))
(float64-ffloor     (double-float) (double-float))
(float64-fround     (double-float) (double-float))
(float64-ftruncate  (double-float) (double-float))

(float64-mod (double-float double-float) (double-float))
(float64-rem (double-float double-float) (double-float))

;;; Mathematic Functions
;;;
;;; {,a}{sin,cos,tan}{,h} ... 2 x 3 x 2 = 12
;;;

(float32-rem-pio2 (single-float)
    (values fixnum single-float single-float single-float) )

(float64-rem-pio2 (double-float)
    (values fixnum double-float double-float double-float) )

(float32-kernel-cos (single-float single-float) single-float)
(float32-kernel-sin (single-float single-float fixnum) single-float)
(float32-kernel-tan (single-float single-float fixnum) single-float)

(float64-kernel-cos (double-float double-float) double-float)
(float64-kernel-sin (double-float double-float fixnum) double-float)
(float64-kernel-tan (double-float double-float fixnum) double-float)


(float32-abs    (single-float) single-float)
(float32-acos   (single-float) single-float)     ; 1
(float32-acosh  (single-float) single-float)     ; 2
(float32-asin   (single-float) single-float)     ; 3
(float32-asinh  (single-float) single-float)     ; 4
(float32-atan/1 (single-float) single-float)     ; 5
(float32-atan/2 (single-float single-float) single-float)
(float32-atanh  (single-float) single-float)     ; 6
(float32-cos    (single-float) single-float)     ; 7
(float32-cosh   (single-float) single-float)     ; 8
(float32-exp    (single-float) single-float)
(float32-expm1  (single-float) single-float)
(float32-expt   (single-float single-float) single-float)
(float32-hypot  (single-float single-float) single-float)
(float32-log    (single-float) single-float)
(float32-log1p  (single-float) single-float)
(float32-sin    (single-float) single-float)     ; 9
(float32-sinh   (single-float) single-float)     ; 10
(float32-sqrt   (single-float) single-float)
(float32-tan    (single-float) single-float)     ; 11
(float32-tanh   (single-float) single-float)     ; 12

(float64-abs    (double-float) double-float)
(float64-acos   (double-float) double-float)     ; 1
(float64-acosh  (double-float) double-float)     ; 2
(float64-asin   (double-float) double-float)     ; 3
(float64-asinh  (double-float) double-float)     ; 4
(float64-atan/1 (double-float) double-float)     ; 5
(float64-atan/2 (double-float double-float) double-float)
(float64-atanh  (double-float) double-float)     ; 6
(float64-cos    (double-float) double-float)     ; 7
(float64-cosh   (double-float) double-float)     ; 8
(float64-exp    (double-float) double-float)
(float64-expm1  (double-float) double-float)
(float64-expt   (double-float double-float) double-float)
(float64-hypot  (double-float double-float) double-float)
(float64-log    (double-float) double-float)
(float64-log1p  (double-float) double-float)
(float64-sin    (double-float) double-float)     ; 9
(float64-sinh   (double-float) double-float)     ; 10
(float64-sqrt   (double-float) double-float)
(float64-tan    (double-float) double-float)     ; 11
(float64-tanh   (double-float) double-float)     ; 12

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 13 Characters
;;
(char=/2             (character character) t)
(char/=/2            (character character) t)
(char</2             (character character) t)
(char<=/2            (character character) t)
(char>/2             (character character) t)
(char>=/2            (character character) t)

(char-equal/2           (character character) t)
(char-not-equal/2       (character character) t)
(char-lessp/2           (character character) t)
(char-not-greaterp/2    (character character) t)
(char-greaterp/2        (character character) t)
(char-not-lessp/2       (character character) t)

(character-category (character) unicode:category)
(whitespace-char-p (character) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 14 Conses
;;
(assq (t list) list)
(delq (t list) list)
(memq (t list) list)

(mapc/1    (function-designator list) list)
(mapcar/1  (function-designator list) list)
(mapcan/1  (function-designator list) list)
(mapl/1    (function-designator list) list)
(maplist/1 (function-designator list) list)
(mapcon/1  (function-designator list) list)

(mapc/2    (function-designator list list) list)
(mapcar/2  (function-designator list list) list)
(mapcan/2  (function-designator list list) list)
(mapl/2    (function-designator list list) list)
(maplist/2 (function-designator list list) list)
(mapcon/2  (function-designator list list) list)


(plist-put (list t t) list)
(plist-rem (list t) (values list t))

(proper-list-p (t) (values (or sequence-index null)))
(safe-list-length (t) fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 15 Arrays
;;
(array-data
    (array)
    (values data-vector ext:sequence-index ext:sequence-index) )

(array-index-error (array ext:sequence-index) nil)

(bit-not/2
    (simple-bit-vector simple-bit-vector
     sequence-index sequence-index
     sequence-index )
    simple-bit-vector )

(bit-and/2
    (simple-bit-vector simple-bit-vector simple-bit-vector
     sequence-index sequence-index sequence-index
     sequence-index )
    simple-bit-vector )

(bit-andc1/2
    (simple-bit-vector simple-bit-vector simple-bit-vector
     sequence-index sequence-index sequence-index
     sequence-index )
    simple-bit-vector )

(bit-andc2/2
    (simple-bit-vector simple-bit-vector simple-bit-vector
     sequence-index sequence-index sequence-index
     sequence-index )
    simple-bit-vector )

(bit-nand/2
    (simple-bit-vector simple-bit-vector simple-bit-vector
     sequence-index sequence-index sequence-index
     sequence-index )
    simple-bit-vector )

(bit-ior/2
    (simple-bit-vector simple-bit-vector simple-bit-vector
     sequence-index sequence-index sequence-index
     sequence-index )
    simple-bit-vector )

(bit-orc1/2
    (simple-bit-vector simple-bit-vector simple-bit-vector
     sequence-index sequence-index sequence-index
     sequence-index )
    simple-bit-vector )

(bit-orc2/2
    (simple-bit-vector simple-bit-vector simple-bit-vector
     sequence-index sequence-index sequence-index
     sequence-index )
    simple-bit-vector )

(bit-nor/2
    (simple-bit-vector simple-bit-vector simple-bit-vector
     sequence-index sequence-index sequence-index
     sequence-index )
    simple-bit-vector )

(bit-eqv/2
    (simple-bit-vector simple-bit-vector simple-bit-vector
     sequence-index sequence-index sequence-index
     sequence-index )
    simple-bit-vector )

(bit-xor/2
    (simple-bit-vector simple-bit-vector simple-bit-vector
     sequence-index sequence-index sequence-index
     sequence-index )
    simple-bit-vector )

(bounding-index-error
    (sequence ext:sequence-index ext:sequence-end) nil )

(internal-adjust-array
  (array list t array ext:sequence-index)
  array )

(internal-adjust-vector
  (vector ext:sequence-index t array ext:sequence-index t ext:sequence-index)
  vector )

(internal-make-array (list t array ext:sequence-index t) array)

(make-simple-vector (ext:sequence-index &optional t) simple-vector)

(vector-data
    (vector &optional ext:sequence-index ext:sequence-end)
    (values data-vector ext:sequence-index ext:sequence-index) )

(sbit/2 (simple-bit-vector ext:sequence-index) bit)
((setf sbit/2) (bit simple-bit-vector ext:sequence-index) bit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 16 Strings
;;
(.allocate-string  (ext:sequence-index) simple-string)

(string-ci-compare
    (string string
        ext:sequence-index ext:sequence-index
        ext:sequence-index ext:sequence-index )
    (values fixnum ext:sequence-index) )

(string-cs-compare
    (string string
        ext:sequence-index ext:sequence-index
        ext:sequence-index ext:sequence-index )
    (values fixnum ext:sequence-index) )

(string-data
    (string &optional ext:sequence-index ext:sequence-end)
    (values simple-string ext:sequence-index ext:sequence-index) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 17 Seqeunces
;;

(coerce-sublist-to-vector
    (list sequence-index sequence-end)
    (values simple-vector sequence-index) )

(ensure-sequence-index
    (sequence ext:sequence-index)
    ext:sequence-index )

(ensure-bounding-indexes
    (sequence ext:sequence-index ext:sequence-end)
    ext:sequence-index )

(length/list   (list)   ext:sequence-index)
(length/vector (vector) ext:sequence-index)

(mismatch/list/list/backward
    (list list
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator
        function-designator )
    (or ext:sequence-index null) )

(mismatch/list/list/forward
    (list list
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator
        function-designator )
    (or ext:sequence-index null) )

(mismatch/list/vector/backward
    (list vector
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator
        function-designator )
    (or ext:sequence-index null) )

(mismatch/list/vector/forward
    (list vector
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator
        function-designator )
    (or ext:sequence-index null) )

(mismatch/vector/list/backward
    (vector list
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator
        function-designator )
    (or ext:sequence-index null) )

(mismatch/vector/list/forward
    (vector list
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator
        function-designator )
    (or ext:sequence-index null) )

(mismatch/vector/vector/backward
    (vector vector
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator
        function-designator )
    (or ext:sequence-index null) )

(mismatch/vector/vector/forward
    (vector vector
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator
        function-designator )
    (or ext:sequence-index null) )

(remove/data-vector/backward
    (t data-vector ext:sequence-index ext:sequence-end
        function-designator function-designator fixnum )
    data-vector )

(remove-duplicates/data-vector/backward
    (data-vector ext:sequence-index ext:sequence-end
        function-designator function-designator )
    data-vector )

(remove-if/data-vector/backward
    (function-designator data-vector
        ext:sequence-index ext:sequence-end
        function-designator fixnum )
    data-vector )

(remove-if-not/data-vector/backward
    (function-designator data-vector
        ext:sequence-index ext:sequence-end
        function-designator fixnum )
    data-vector )

(remove/data-vector/forward
    (t data-vector ext:sequence-index ext:sequence-end
        function-designator function-designator fixnum )
    data-vector )

(remove-duplicates/data-vector/forward
    (data-vector ext:sequence-index ext:sequence-end
        function-designator function-designator )
    data-vector )

(remove-if/data-vector/forward
    (function-designator data-vector
        ext:sequence-index ext:sequence-end
        function-designator fixnum )
    data-vector )

(remove-if-not/data-vector/forward
    (function-designator data-vector
        ext:sequence-index ext:sequence-end
        function-designator fixnum )
    data-vector )

(replace/list/list
    (list list 
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end )
    list )

(replace/list/vector
    (list vector
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end )
    list )

(replace/vector/list
    (vector list 
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end )
    vector )

(replace/vector/vector
    (vector vector 
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end )
    vector )

(sequence-index-error   (sequence integer) nil)
(sequence-index-errorex (sequence integer integer) nil)

(search/list/list/backward
    (list list
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator function-designator )
    (or ext:sequence-index null) )

(search/list/list/forward
    (list list
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator function-designator )
    (or ext:sequence-index null) )

(search/list/vector/backward
    (list vector
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator function-designator )
    (or ext:sequence-index null) )

(search/list/vector/forward
    (list vector
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator function-designator )
    (or ext:sequence-index null) )

(search/vector/list/backward
    (vector list
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator function-designator )
    (or ext:sequence-index null) )

(search/vector/list/forward
    (vector list
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator function-designator )
    (or ext:sequence-index null) )

(search/vector/vector/backward
    (vector vector
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator function-designator )
    (or ext:sequence-index null) )

(search/vector/vector/forward
    (vector vector
        ext:sequence-index ext:sequence-end
        ext:sequence-index ext:sequence-end
        function-designator function-designator )
    (or ext:sequence-index null) )

(stable-sort/list (list function-designator function-designator) list)

(subseq/list   (list   ext:sequence-index ext:sequence-end) list)
(subseq/vector (vector ext:sequence-index ext:sequence-end) vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 18 Hash Tables
;;
(gethash/eq          (t hash-table &optional t) (values t t))
((setf gethash/eq)   (t t hash-table &optional t) t)

(sxhash/eql    (t) positive-fixnum)
(sxhash/eql    (t) positive-fixnum)
(sxhash/equal  (t) positive-fixnum)
(sxhash/equalp (t) positive-fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 19 Filenames
;;
(host-name (basic-host) string)

(make-logical-host (string) logical-host)
(logical-unparse-element (t stream) t)

#+win32 (make-windows-host (string) windows-host)
#+win32 (windows-unparse-element (t stream) t)

(parse-namestring-using-host
    (basic-host string ext:sequence-index ext:sequence-end)
    (values pathname ext:sequence-index) )

(pathname-equal (pathname pathname) t)
(pathname-sxhash (pathname) positive-fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 20 Files
;;
(.current-directory (&optional (or fixnum null)) simple-string)

((setf current-directory)
  (simple-string &optional (or fixnum null))
  simple-string )

(ensure-physical-pathname (ext:pathname-designator) pathname)


(.create-directory (t) t)         ; 20.2.3
(.delete-file (t) t)              ; 20.2.8
(.file-attributes (t)             ; 20.2.6
  (values fixnum            ; attribute
          unsigned-byte     ; mtime
          unsigned-byte     ; atime
          unsigned-byte     ; ctime
          unsigned-byte ) ) ; size

(.find-close (handle) null)       ; 20.2.1

(.find-first-file (string)        ; 20.2.1
  (values handle (or null string) fixnum ) )

(.find-next-file (handle)          ; 20.2.1
  (values (or null string) fixnum) )

(.remove-directory (t) t)         ; 20.2.8
(.rename-file (string string) t)  ; 20.2.7

(rename-if-exists (pathname-designator) (or pathname null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 21 Streams
;;
(close/2 ((or stream null) t) t)
(ext:stream-pathname (stream) pathname)
(make-pooled-string-output-stream () string-output-stream)
(free-pooled-string-output-stream (string-output-stream) null)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 22 Printer
;;
(pprint-logical-block-function
    (stream t string (or string null) string function) unspecified )

(pprint-object (t t) t)
(print-unreadable-object-function (t stream t t (or function null)) null)
(write-object (t stream) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 23 Reader
;;
(discard-token ((or character null) stream)
    (values (member :delimiter :eof :macro nil)
            (or character null) ) )

;; for compiler-macro
(fast-read-char (stream t) (or character null))

(read-char-and-attr (stream readtable) (values (or character null) fixnum))

(read-token ((or character null) stream)
    (values (member :delimiter :eof :macro :token)
            (or character null reader-token) ) )

(simple-reader-error (stream format-control &rest t) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 49 Internals
;;
(address-of (t) fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 49 Internals - Cell
;;
(find-plist-cell (symbol) (or plist-cell null))
(intern-plist-cell (symbol) plist-cell)

(find-setf-cell (symbol) (or setf-cell null))
(intern-setf-cell (symbol) setf-cell)

(find-value-cell (symbol) (or value-cell tlv-record null))
(intern-value-cell (symbol) (or value-cell tlv-record))


;; 49 Interanls - compiler support
(ext::required () t)
(ext::unspecified () null)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 49 Internals - Function Object
;;
(function-name (function) (or symbol list))
((setf function-name) ((or symbol list) function) (or symbol list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 49 Internals - TLV
;;
(%ref                (symbol symbol t) t)
((setf %ref)         (t symbol symbol t) t)

(tlv (fixnum) t)
((setf tlv) (t fixnum) t)

;; 49 Internal - Class Descriptions
(make-class-description (class list) class-description)

(.allocate-instance (class-description) standard-object)

(.allocate-funcallable-instance (class-description)
    clos:funcallable-standard-object)

(.allocate-structure (class-description) structure-object)

(.allocate-record (class-description) t)


(!elt (type-specifier t sequence-index) t)
((setf !elt) (t type-specifier t sequence-index) t)

(!elt* (type-specifier t sequence-index) t)
((setf !elt*) (t type-specifier t sequence-index) t)


(.box-int   (t) (signed-byte #+32bit 32 #+64bit 64))
(.box-uint  (t) (unsigned-byte #+32bit 32 #+64bit 64))
(.unbox-int ((signed-byte #+32bit 32 #+64bit 64)) int)



;; 50 Extensions - Lock
(lock-latch (latch (member :exclusive :shared)) latch)
(lock-mutex (latch) latch)
(make-latch (symbol) latch)
(make-mutex (symbol) mutex)
(unlock-latch (latch) latch)
(unlock-mutex (mutext) mutex)
