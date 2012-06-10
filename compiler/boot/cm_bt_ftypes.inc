// This is a program generated file. DO NOT EDIT!
// @(#)$Id: //proj/evcl3/mainline/compiler/boot/cm_bt_ftypes.inc#40 $

  // c6::function-information (function (function-name &optional (or environment null)) (values (or (member :function :macro :special-operator nil)) t list)))
  add_ftype(Q("C6::FUNCTION-INFORMATION"),
    list(Q("FUNCTION-NAME"), Q("&OPTIONAL"), list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    list(Q("VALUES"), list(Q("OR"), list(Q("MEMBER"), Q(":FUNCTION"), Q(":MACRO"), Q(":SPECIAL-OPERATOR"), nil)), t, Q("LIST")) );

  // c6::variable-information (function (symbol &optional (or null environment)) (values (or (member :lexical :special :constant :symbol-macro nil)) t list)))
  add_ftype(Q("C6::VARIABLE-INFORMATION"),
    list(Q("SYMBOL"), Q("&OPTIONAL"), list(Q("OR"), Q("NULL"), Q("ENVIRONMENT"))),
    list(Q("VALUES"), list(Q("OR"), list(Q("MEMBER"), Q(":LEXICAL"), Q(":SPECIAL"), Q(":CONSTANT"), Q(":SYMBOL-MACRO"), nil)), t, Q("LIST")) );

  // dotted-rest-argument (function (t) nil))
  add_ftype(Q("DOTTED-REST-ARGUMENT"),
    list(t),
    nil );

  // odd-number-of-keyword-arguments (function (t) nil))
  add_ftype(Q("ODD-NUMBER-OF-KEYWORD-ARGUMENTS"),
    list(t),
    nil );

  // runtime-environment-p (function (environment) t))
  add_ftype(Q("RUNTIME-ENVIRONMENT-P"),
    list(Q("ENVIRONMENT")),
    t );

  // toplevel-environment-p (function (environment) t))
  add_ftype(Q("TOPLEVEL-ENVIRONMENT-P"),
    list(Q("ENVIRONMENT")),
    t );

  // toplevel-environment (function ((or environment null)) environment))
  add_ftype(Q("TOPLEVEL-ENVIRONMENT"),
    list(list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    Q("ENVIRONMENT") );

  // fun-info (function (function-name symbol &optional (or environment null)) (values t t)))
  add_ftype(Q("FUN-INFO"),
    list(Q("FUNCTION-NAME"), Q("SYMBOL"), Q("&OPTIONAL"), list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    list(Q("VALUES"), t, t) );

  // var-info (function (symbol symbol &optional (or environment null)) (values t t)))
  add_ftype(Q("VAR-INFO"),
    list(Q("SYMBOL"), Q("SYMBOL"), Q("&OPTIONAL"), list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    list(Q("VALUES"), t, t) );

  // (setf fun-info) (function (t function-name symbol &optional (or environment null)) t))
  add_ftype(intern_setf_cell(Q("FUN-INFO")),
    list(t, Q("FUNCTION-NAME"), Q("SYMBOL"), Q("&OPTIONAL"), list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    t );

  // (setf var-info) (function (t symbol symbol &optional (or environment null)) t))
  add_ftype(intern_setf_cell(Q("VAR-INFO")),
    list(t, Q("SYMBOL"), Q("SYMBOL"), Q("&OPTIONAL"), list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    t );

  // c6::analyze-body (function (list &optional t) (values list list (or string null))))
  add_ftype(Q("C6::ANALYZE-BODY"),
    list(Q("LIST"), Q("&OPTIONAL"), t),
    list(Q("VALUES"), Q("LIST"), Q("LIST"), list(Q("OR"), Q("STRING"), Q("NULL"))) );

  // c6::analyze-lambda-list (function (list &optional t t) (values list list list list list list list)))
  add_ftype(Q("C6::ANALYZE-LAMBDA-LIST"),
    list(Q("LIST"), Q("&OPTIONAL"), t, t),
    list(Q("VALUES"), Q("LIST"), Q("LIST"), Q("LIST"), Q("LIST"), Q("LIST"), Q("LIST"), Q("LIST")) );

  // c6::constant-value-p (function (t (or environment null)) (values t t)))
  add_ftype(Q("C6::CONSTANT-VALUE-P"),
    list(t, list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    list(Q("VALUES"), t, t) );

  // c6::macro-error (function (format-control &rest t) nil))
  add_ftype(Q("C6::MACRO-ERROR"),
    list(Q("FORMAT-CONTROL"), Q("&REST"), t),
    nil );

  // c6::make-keyword-arguments-parser (function (symbol list t) (values list list)))
  add_ftype(Q("C6::MAKE-KEYWORD-ARGUMENTS-PARSER"),
    list(Q("SYMBOL"), Q("LIST"), t),
    list(Q("VALUES"), Q("LIST"), Q("LIST")) );

  // c6::parse-destructuring-bind (function (list symbol t list t &optional symbol t) (values form symbol)))
  add_ftype(Q("C6::PARSE-DESTRUCTURING-BIND"),
    list(Q("LIST"), Q("SYMBOL"), t, Q("LIST"), t, Q("&OPTIONAL"), Q("SYMBOL"), t),
    list(Q("VALUES"), Q("FORM"), Q("SYMBOL")) );

  // c6::parse-lambda-list (function (list) (values list list list list list)))
  add_ftype(Q("C6::PARSE-LAMBDA-LIST"),
    list(Q("LIST")),
    list(Q("VALUES"), Q("LIST"), Q("LIST"), Q("LIST"), Q("LIST"), Q("LIST")) );

  // c6::parse-macro-aux (function (symbol list list) (values list (or string null))))
  add_ftype(Q("C6::PARSE-MACRO-AUX"),
    list(Q("SYMBOL"), Q("LIST"), Q("LIST")),
    list(Q("VALUES"), Q("LIST"), list(Q("OR"), Q("STRING"), Q("NULL"))) );

  // c6::syntax-error (function (form t &optional t t t) nil))
  add_ftype(Q("C6::SYNTAX-ERROR"),
    list(Q("FORM"), t, Q("&OPTIONAL"), t, t, t),
    nil );

  // c6::style-warn (function (format-control &rest t) null))
  add_ftype(Q("C6::STYLE-WARN"),
    list(Q("FORMAT-CONTROL"), Q("&REST"), t),
    Q("NULL") );

  // find-type (function (symbol &optional t (or environment null)) (or function symbol cons)))
  add_ftype(Q("FIND-TYPE"),
    list(Q("SYMBOL"), Q("&OPTIONAL"), t, list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    list(Q("OR"), Q("FUNCTION"), Q("SYMBOL"), Q("CONS")) );

  // (setf find-type) (function ((or function symbol cons) symbol &optional t (or environment null)) (or function symbol cons)))
  add_ftype(intern_setf_cell(Q("FIND-TYPE")),
    list(list(Q("OR"), Q("FUNCTION"), Q("SYMBOL"), Q("CONS")), Q("SYMBOL"), Q("&OPTIONAL"), t, list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    list(Q("OR"), Q("FUNCTION"), Q("SYMBOL"), Q("CONS")) );

  // parse-type (function (type-specifier (or environment null)) (values t t)))
  add_ftype(Q("PARSE-TYPE"),
    list(Q("TYPE-SPECIFIER"), list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    list(Q("VALUES"), t, t) );

  // parse-sequence-type (function (type-specifier) (values (or list vector) type-specifier sequence-index)))
  add_ftype(Q("PARSE-SEQUENCE-TYPE"),
    list(Q("TYPE-SPECIFIER")),
    list(Q("VALUES"), list(Q("OR"), Q("LIST"), Q("VECTOR")), Q("TYPE-SPECIFIER"), Q("SEQUENCE-INDEX")) );

  // vector-class (function (type-specifier &optional (or environment null)) class))
  add_ftype(Q("VECTOR-CLASS"),
    list(Q("TYPE-SPECIFIER"), Q("&OPTIONAL"), list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    Q("CLASS") );

  // c6::parse-type-macro (function (symbol list list) (values list (or string null))))
  add_ftype(Q("C6::PARSE-TYPE-MACRO"),
    list(Q("SYMBOL"), Q("LIST"), Q("LIST")),
    list(Q("VALUES"), Q("LIST"), list(Q("OR"), Q("STRING"), Q("NULL"))) );

  // c6::safe-subtypep (function (type-specifier type-specifier &optional (or environment null)) (values t t)))
  add_ftype(Q("C6::SAFE-SUBTYPEP"),
    list(Q("TYPE-SPECIFIER"), Q("TYPE-SPECIFIER"), Q("&OPTIONAL"), list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    list(Q("VALUES"), t, t) );

  // intern-class (function (symbol &optional (or environment null)) class))
  add_ftype(Q("INTERN-CLASS"),
    list(Q("SYMBOL"), Q("&OPTIONAL"), list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    Q("CLASS") );

  // accessor-method-slot-definition (function (accessor-method) slot-definition))
  add_ftype(Q("ACCESSOR-METHOD-SLOT-DEFINITION"),
    list(Q("ACCESSOR-METHOD")),
    Q("SLOT-DEFINITION") );

  // add-dependent (function (metaobject t) unspecified))
  add_ftype(Q("ADD-DEPENDENT"),
    list(Q("METAOBJECT"), t),
    Q("UNSPECIFIED") );

  // add-direct-method (function (specializer method) unspecified))
  add_ftype(Q("ADD-DIRECT-METHOD"),
    list(Q("SPECIALIZER"), Q("METHOD")),
    Q("UNSPECIFIED") );

  // add-direct-subclass (function (class class) unspecified))
  add_ftype(Q("ADD-DIRECT-SUBCLASS"),
    list(Q("CLASS"), Q("CLASS")),
    Q("UNSPECIFIED") );

  // compute-applicable-methods-using-classes (function (generic-function list) list))
  add_ftype(Q("COMPUTE-APPLICABLE-METHODS-USING-CLASSES"),
    list(Q("GENERIC-FUNCTION"), Q("LIST")),
    Q("LIST") );

  // compute-class-precedence-list (function (class) list))
  add_ftype(Q("COMPUTE-CLASS-PRECEDENCE-LIST"),
    list(Q("CLASS")),
    Q("LIST") );

  // compute-default-initargs (function (class) list))
  add_ftype(Q("COMPUTE-DEFAULT-INITARGS"),
    list(Q("CLASS")),
    Q("LIST") );

  // compute-discriminating-function (function (generic-function) function))
  add_ftype(Q("COMPUTE-DISCRIMINATING-FUNCTION"),
    list(Q("GENERIC-FUNCTION")),
    Q("FUNCTION") );

  // compute-effective-method (function (generic-function t t) (values method list)))
  add_ftype(Q("COMPUTE-EFFECTIVE-METHOD"),
    list(Q("GENERIC-FUNCTION"), t, t),
    list(Q("VALUES"), Q("METHOD"), Q("LIST")) );

  // compute-effective-slot-definition (function (class symbol list) effective-slot-definition))
  add_ftype(Q("COMPUTE-EFFECTIVE-SLOT-DEFINITION"),
    list(Q("CLASS"), Q("SYMBOL"), Q("LIST")),
    Q("EFFECTIVE-SLOT-DEFINITION") );

  // compute-slots (function (class) list))
  add_ftype(Q("COMPUTE-SLOTS"),
    list(Q("CLASS")),
    Q("LIST") );

  // direct-slot-definition-class (function (class &rest t) class))
  add_ftype(Q("DIRECT-SLOT-DEFINITION-CLASS"),
    list(Q("CLASS"), Q("&REST"), t),
    Q("CLASS") );

  // effective-slot-definition-class (function (class &rest t) class))
  add_ftype(Q("EFFECTIVE-SLOT-DEFINITION-CLASS"),
    list(Q("CLASS"), Q("&REST"), t),
    Q("CLASS") );

  // ensure-class (function (symbol &key &allow-other-keys) class))
  add_ftype(Q("ENSURE-CLASS"),
    list(Q("SYMBOL"), Q("&KEY"), Q("&ALLOW-OTHER-KEYS")),
    Q("CLASS") );

  // ensure-class-using-class (function ((or class null) symbol &key (:direct-default-initargs list) (:direct-slots list) (:direct-superclasses list) (:name symbol) (:metaclass (or class symbol)) &allow-other-keys) class))
  add_ftype(Q("ENSURE-CLASS-USING-CLASS"),
    list(list(Q("OR"), Q("CLASS"), Q("NULL")), Q("SYMBOL"), Q("&KEY"), list(Q(":DIRECT-DEFAULT-INITARGS"), Q("LIST")), list(Q(":DIRECT-SLOTS"), Q("LIST")), list(Q(":DIRECT-SUPERCLASSES"), Q("LIST")), list(Q(":NAME"), Q("SYMBOL")), list(Q(":METACLASS"), list(Q("OR"), Q("CLASS"), Q("SYMBOL"))), Q("&ALLOW-OTHER-KEYS")),
    Q("CLASS") );

  // ensure-generic-function-using-class (function ((or generic-function null) function-name &key (:argument-precedence-order list) (:declarations list) (:documentation (or string null)) (:generic-function-class (or class symbol)) (:lambda-list list) (:method-class (or class symbol)) (:method-combination method-combination) (:name function-name) &allow-other-keys) generic-function))
  add_ftype(Q("ENSURE-GENERIC-FUNCTION-USING-CLASS"),
    list(list(Q("OR"), Q("GENERIC-FUNCTION"), Q("NULL")), Q("FUNCTION-NAME"), Q("&KEY"), list(Q(":ARGUMENT-PRECEDENCE-ORDER"), Q("LIST")), list(Q(":DECLARATIONS"), Q("LIST")), list(Q(":DOCUMENTATION"), list(Q("OR"), Q("STRING"), Q("NULL"))), list(Q(":GENERIC-FUNCTION-CLASS"), list(Q("OR"), Q("CLASS"), Q("SYMBOL"))), list(Q(":LAMBDA-LIST"), Q("LIST")), list(Q(":METHOD-CLASS"), list(Q("OR"), Q("CLASS"), Q("SYMBOL"))), list(Q(":METHOD-COMBINATION"), Q("METHOD-COMBINATION")), list(Q(":NAME"), Q("FUNCTION-NAME")), Q("&ALLOW-OTHER-KEYS")),
    Q("GENERIC-FUNCTION") );

  // eql-specializer-object (function (eql-specializer) t))
  add_ftype(Q("EQL-SPECIALIZER-OBJECT"),
    list(Q("EQL-SPECIALIZER")),
    t );

  // extract-lambda-list (function (list) list))
  add_ftype(Q("EXTRACT-LAMBDA-LIST"),
    list(Q("LIST")),
    Q("LIST") );

  // extract-specializer-names (function (list) list))
  add_ftype(Q("EXTRACT-SPECIALIZER-NAMES"),
    list(Q("LIST")),
    Q("LIST") );

  // ensure-class-initargs (function (class list) (values class list)))
  add_ftype(Q("ENSURE-CLASS-INITARGS"),
    list(Q("CLASS"), Q("LIST")),
    list(Q("VALUES"), Q("CLASS"), Q("LIST")) );

  // finalize-inheritance (function (class) unspecified))
  add_ftype(Q("FINALIZE-INHERITANCE"),
    list(Q("CLASS")),
    Q("UNSPECIFIED") );

  // find-method-combination (function (generic-function symbol list) method-combination))
  add_ftype(Q("FIND-METHOD-COMBINATION"),
    list(Q("GENERIC-FUNCTION"), Q("SYMBOL"), Q("LIST")),
    Q("METHOD-COMBINATION") );

  // funcallable-standard-instance-access (function (funcallable-standard-object sequence-index) t))
  add_ftype(Q("FUNCALLABLE-STANDARD-INSTANCE-ACCESS"),
    list(Q("FUNCALLABLE-STANDARD-OBJECT"), Q("SEQUENCE-INDEX")),
    t );

  // (setf funcallable-standard-instance-access) (function (t funcallable-standard-object sequence-index) t))
  add_ftype(intern_setf_cell(Q("FUNCALLABLE-STANDARD-INSTANCE-ACCESS")),
    list(t, Q("FUNCALLABLE-STANDARD-OBJECT"), Q("SEQUENCE-INDEX")),
    t );

  // generic-function-argument-precedence-order (function (generic-function) list))
  add_ftype(Q("GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER"),
    list(Q("GENERIC-FUNCTION")),
    Q("LIST") );

  // generic-function-declarations (function (generic-function) list))
  add_ftype(Q("GENERIC-FUNCTION-DECLARATIONS"),
    list(Q("GENERIC-FUNCTION")),
    Q("LIST") );

  // generic-function-lambda-list (function (generic-function) list))
  add_ftype(Q("GENERIC-FUNCTION-LAMBDA-LIST"),
    list(Q("GENERIC-FUNCTION")),
    Q("LIST") );

  // generic-function-method-class (function (generic-function) class))
  add_ftype(Q("GENERIC-FUNCTION-METHOD-CLASS"),
    list(Q("GENERIC-FUNCTION")),
    Q("CLASS") );

  // generic-function-method-combination (function (generic-function) method-combination))
  add_ftype(Q("GENERIC-FUNCTION-METHOD-COMBINATION"),
    list(Q("GENERIC-FUNCTION")),
    Q("METHOD-COMBINATION") );

  // generic-function-methods (function (generic-function) list))
  add_ftype(Q("GENERIC-FUNCTION-METHODS"),
    list(Q("GENERIC-FUNCTION")),
    Q("LIST") );

  // generic-function-name (function (generic-function) function-name))
  add_ftype(Q("GENERIC-FUNCTION-NAME"),
    list(Q("GENERIC-FUNCTION")),
    Q("FUNCTION-NAME") );

  // intern-eql-specializer (function (t) eql-specializer))
  add_ftype(Q("INTERN-EQL-SPECIALIZER"),
    list(t),
    Q("EQL-SPECIALIZER") );

  // make-method-lambda (function (generic-function method list (or environment null)) (values cons list)))
  add_ftype(Q("MAKE-METHOD-LAMBDA"),
    list(Q("GENERIC-FUNCTION"), Q("METHOD"), Q("LIST"), list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    list(Q("VALUES"), Q("CONS"), Q("LIST")) );

  // map-dependents (function (metaobject function) unspecified))
  add_ftype(Q("MAP-DEPENDENTS"),
    list(Q("METAOBJECT"), Q("FUNCTION")),
    Q("UNSPECIFIED") );

  // method-function (function (method) function))
  add_ftype(Q("METHOD-FUNCTION"),
    list(Q("METHOD")),
    Q("FUNCTION") );

  // method-generic-function (function (method) generic-function))
  add_ftype(Q("METHOD-GENERIC-FUNCTION"),
    list(Q("METHOD")),
    Q("GENERIC-FUNCTION") );

  // method-lambda-list (function (method) list))
  add_ftype(Q("METHOD-LAMBDA-LIST"),
    list(Q("METHOD")),
    Q("LIST") );

  // method-specializers (function (method) list))
  add_ftype(Q("METHOD-SPECIALIZERS"),
    list(Q("METHOD")),
    Q("LIST") );

  // reader-method-class (function (class direct-slot-definition &rest t) class))
  add_ftype(Q("READER-METHOD-CLASS"),
    list(Q("CLASS"), Q("DIRECT-SLOT-DEFINITION"), Q("&REST"), t),
    Q("CLASS") );

  // remove-dependent (function (metaobject t) unspecified))
  add_ftype(Q("REMOVE-DEPENDENT"),
    list(Q("METAOBJECT"), t),
    Q("UNSPECIFIED") );

  // remove-direct-method (function (specializer method) unspecified))
  add_ftype(Q("REMOVE-DIRECT-METHOD"),
    list(Q("SPECIALIZER"), Q("METHOD")),
    Q("UNSPECIFIED") );

  // remove-direct-subclass (function (class class) unspecified))
  add_ftype(Q("REMOVE-DIRECT-SUBCLASS"),
    list(Q("CLASS"), Q("CLASS")),
    Q("UNSPECIFIED") );

  // set-funcallable-instance-function (function (funcallable-instance function) function))
  add_ftype(Q("SET-FUNCALLABLE-INSTANCE-FUNCTION"),
    list(Q("FUNCALLABLE-INSTANCE"), Q("FUNCTION")),
    Q("FUNCTION") );

  // slot-definition-allocation (function (slot-definition) t))
  add_ftype(Q("SLOT-DEFINITION-ALLOCATION"),
    list(Q("SLOT-DEFINITION")),
    t );

  // slot-definition-initargs (function (slot-definition) list))
  add_ftype(Q("SLOT-DEFINITION-INITARGS"),
    list(Q("SLOT-DEFINITION")),
    Q("LIST") );

  // slot-definition-initform (function (slot-definition) t))
  add_ftype(Q("SLOT-DEFINITION-INITFORM"),
    list(Q("SLOT-DEFINITION")),
    t );

  // slot-definition-initfunction (function (slot-definition) (or function null)))
  add_ftype(Q("SLOT-DEFINITION-INITFUNCTION"),
    list(Q("SLOT-DEFINITION")),
    list(Q("OR"), Q("FUNCTION"), Q("NULL")) );

  // slot-definition-name (function (slot-definition) symbol))
  add_ftype(Q("SLOT-DEFINITION-NAME"),
    list(Q("SLOT-DEFINITION")),
    Q("SYMBOL") );

  // slot-definition-type (function (slot-definition) type-specifier))
  add_ftype(Q("SLOT-DEFINITION-TYPE"),
    list(Q("SLOT-DEFINITION")),
    Q("TYPE-SPECIFIER") );

  // slot-boundp-using-class (function (class t effective-slot-definition) t))
  add_ftype(Q("SLOT-BOUNDP-USING-CLASS"),
    list(Q("CLASS"), t, Q("EFFECTIVE-SLOT-DEFINITION")),
    t );

  // slot-makunbound-using-class (function (class t effective-slot-definition) t))
  add_ftype(Q("SLOT-MAKUNBOUND-USING-CLASS"),
    list(Q("CLASS"), t, Q("EFFECTIVE-SLOT-DEFINITION")),
    t );

  // slot-value-using-class (function (class t effective-slot-definition) t))
  add_ftype(Q("SLOT-VALUE-USING-CLASS"),
    list(Q("CLASS"), t, Q("EFFECTIVE-SLOT-DEFINITION")),
    t );

  // (setf slot-value-using-class) (function (t class t effective-slot-definition) t))
  add_ftype(intern_setf_cell(Q("SLOT-VALUE-USING-CLASS")),
    list(t, Q("CLASS"), t, Q("EFFECTIVE-SLOT-DEFINITION")),
    t );

  // specializer-direct-generic-functions (function (specializer) list))
  add_ftype(Q("SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"),
    list(Q("SPECIALIZER")),
    Q("LIST") );

  // specializer-direct-methods (function (specializer) list))
  add_ftype(Q("SPECIALIZER-DIRECT-METHODS"),
    list(Q("SPECIALIZER")),
    Q("LIST") );

  // standard-instance-access (function (standard-object sequence-index) t))
  add_ftype(Q("STANDARD-INSTANCE-ACCESS"),
    list(Q("STANDARD-OBJECT"), Q("SEQUENCE-INDEX")),
    t );

  // (setf standard-instance-access) (function (t standard-object sequence-index) t))
  add_ftype(intern_setf_cell(Q("STANDARD-INSTANCE-ACCESS")),
    list(t, Q("STANDARD-OBJECT"), Q("SEQUENCE-INDEX")),
    t );

  // update-dependent (function (metaobject t &rest t) unspecified))
  add_ftype(Q("UPDATE-DEPENDENT"),
    list(Q("METAOBJECT"), t, Q("&REST"), t),
    Q("UNSPECIFIED") );

  // validate-superclass (function (class class) t))
  add_ftype(Q("VALIDATE-SUPERCLASS"),
    list(Q("CLASS"), Q("CLASS")),
    t );

  // writer-method-class (function (class direct-slot-definition &rest t) class))
  add_ftype(Q("WRITER-METHOD-CLASS"),
    list(Q("CLASS"), Q("DIRECT-SLOT-DEFINITION"), Q("&REST"), t),
    Q("CLASS") );

  // make-discriminator/reader (function (generic-function method) function))
  add_ftype(Q("MAKE-DISCRIMINATOR/READER"),
    list(Q("GENERIC-FUNCTION"), Q("METHOD")),
    Q("FUNCTION") );

  // make-discriminator/reader-1 (function (generic-function class (or fixnum cons)) function))
  add_ftype(Q("MAKE-DISCRIMINATOR/READER-1"),
    list(Q("GENERIC-FUNCTION"), Q("CLASS"), list(Q("OR"), Q("FIXNUM"), Q("CONS"))),
    Q("FUNCTION") );

  // make-discriminator/reader-n (function (generic-function class symbol) function))
  add_ftype(Q("MAKE-DISCRIMINATOR/READER-N"),
    list(Q("GENERIC-FUNCTION"), Q("CLASS"), Q("SYMBOL")),
    Q("FUNCTION") );

  // make-discriminator/writer (function (generic-function method) function))
  add_ftype(Q("MAKE-DISCRIMINATOR/WRITER"),
    list(Q("GENERIC-FUNCTION"), Q("METHOD")),
    Q("FUNCTION") );

  // make-discriminator/writer-1 (function (generic-function class (or fixnum cons)) function))
  add_ftype(Q("MAKE-DISCRIMINATOR/WRITER-1"),
    list(Q("GENERIC-FUNCTION"), Q("CLASS"), list(Q("OR"), Q("FIXNUM"), Q("CONS"))),
    Q("FUNCTION") );

  // make-discriminator/writer-n (function (generic-function class symbol) function))
  add_ftype(Q("MAKE-DISCRIMINATOR/WRITER-N"),
    list(Q("GENERIC-FUNCTION"), Q("CLASS"), Q("SYMBOL")),
    Q("FUNCTION") );

  // make-param-info (function (&key (:nreqs fixnum) (:nopts fixnum) (:keys (or list (eql t))) (:lambda-list list) (:order list)) param-info))
  add_ftype(Q("MAKE-PARAM-INFO"),
    list(Q("&KEY"), list(Q(":NREQS"), Q("FIXNUM")), list(Q(":NOPTS"), Q("FIXNUM")), list(Q(":KEYS"), list(Q("OR"), Q("LIST"), list(Q("EQL"), t))), list(Q(":LAMBDA-LIST"), Q("LIST")), list(Q(":ORDER"), Q("LIST"))),
    Q("PARAM-INFO") );

  // power-2 (function (fixnum) fixnum))
  add_ftype(Q("POWER-2"),
    list(Q("FIXNUM")),
    Q("FIXNUM") );

  // same-slot-location-p (function (class symbol) t))
  add_ftype(Q("SAME-SLOT-LOCATION-P"),
    list(Q("CLASS"), Q("SYMBOL")),
    t );

  // update-discriminator (function (generic-function) t))
  add_ftype(Q("UPDATE-DISCRIMINATOR"),
    list(Q("GENERIC-FUNCTION")),
    t );

  // update-obsolete-instance (function (t) t))
  add_ftype(Q("UPDATE-OBSOLETE-INSTANCE"),
    list(t),
    t );

  // make-discriminator/checking (function (generic-function param-info t method function) function))
  add_ftype(Q("MAKE-DISCRIMINATOR/CHECKING"),
    list(Q("GENERIC-FUNCTION"), Q("PARAM-INFO"), t, Q("METHOD"), Q("FUNCTION")),
    Q("FUNCTION") );

  // make-discriminator/dispatch (function (generic-function param-info list list function) function))
  add_ftype(Q("MAKE-DISCRIMINATOR/DISPATCH"),
    list(Q("GENERIC-FUNCTION"), Q("PARAM-INFO"), Q("LIST"), Q("LIST"), Q("FUNCTION")),
    Q("FUNCTION") );

  // make-temporary-lambda-list (function (param-info) (values list list symbol list list)))
  add_ftype(Q("MAKE-TEMPORARY-LAMBDA-LIST"),
    list(Q("PARAM-INFO")),
    list(Q("VALUES"), Q("LIST"), Q("LIST"), Q("SYMBOL"), Q("LIST"), Q("LIST")) );

  // std-compute-applicable-methods (function (generic-function list) list))
  add_ftype(Q("STD-COMPUTE-APPLICABLE-METHODS"),
    list(Q("GENERIC-FUNCTION"), Q("LIST")),
    Q("LIST") );

  // compute-added-slots (function (t t) list))
  add_ftype(Q("COMPUTE-ADDED-SLOTS"),
    list(t, t),
    Q("LIST") );

  // obsolete-instance-p (function (t) t))
  add_ftype(Q("OBSOLETE-INSTANCE-P"),
    list(t),
    t );

  // swap-instance-layout (function (standard-object standard-object) t))
  add_ftype(Q("SWAP-INSTANCE-LAYOUT"),
    list(Q("STANDARD-OBJECT"), Q("STANDARD-OBJECT")),
    t );

  // allocate-structure (function (structure-class &rest t) structure-object))
  add_ftype(Q("ALLOCATE-STRUCTURE"),
    list(Q("STRUCTURE-CLASS"), Q("&REST"), t),
    Q("STRUCTURE-OBJECT") );

  // ensure-structure-using-class (function ((or class null) symbol &key (:direct-default-initargs list) (:direct-slots list) (:direct-superclasses list) (:name symbol) (:metaclass (or class symbol)) &allow-other-keys) class))
  add_ftype(Q("ENSURE-STRUCTURE-USING-CLASS"),
    list(list(Q("OR"), Q("CLASS"), Q("NULL")), Q("SYMBOL"), Q("&KEY"), list(Q(":DIRECT-DEFAULT-INITARGS"), Q("LIST")), list(Q(":DIRECT-SLOTS"), Q("LIST")), list(Q(":DIRECT-SUPERCLASSES"), Q("LIST")), list(Q(":NAME"), Q("SYMBOL")), list(Q(":METACLASS"), list(Q("OR"), Q("CLASS"), Q("SYMBOL"))), Q("&ALLOW-OTHER-KEYS")),
    Q("CLASS") );

  // find-structure (function (symbol &optional t (or environment null)) (or class null)))
  add_ftype(Q("FIND-STRUCTURE"),
    list(Q("SYMBOL"), Q("&OPTIONAL"), t, list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    list(Q("OR"), Q("CLASS"), Q("NULL")) );

  // (setf find-structure) (function ((or class null) symbol &optional t (or environment null)) (or class null)))
  add_ftype(intern_setf_cell(Q("FIND-STRUCTURE")),
    list(list(Q("OR"), Q("CLASS"), Q("NULL")), Q("SYMBOL"), Q("&OPTIONAL"), t, list(Q("OR"), Q("ENVIRONMENT"), Q("NULL"))),
    list(Q("OR"), Q("CLASS"), Q("NULL")) );

  // structure-instance-access (function (structure-object sequence-index) t))
  add_ftype(Q("STRUCTURE-INSTANCE-ACCESS"),
    list(Q("STRUCTURE-OBJECT"), Q("SEQUENCE-INDEX")),
    t );

  // (setf structure-instance-access) (function (t structure-object sequence-index) t))
  add_ftype(intern_setf_cell(Q("STRUCTURE-INSTANCE-ACCESS")),
    list(t, Q("STRUCTURE-OBJECT"), Q("SEQUENCE-INDEX")),
    t );

  // coerce-to-condition (function (symbol (or condition string function symbol class) list type-specifier) condition))
  add_ftype(Q("COERCE-TO-CONDITION"),
    list(Q("SYMBOL"), list(Q("OR"), Q("CONDITION"), Q("STRING"), Q("FUNCTION"), Q("SYMBOL"), Q("CLASS")), Q("LIST"), Q("TYPE-SPECIFIER")),
    Q("CONDITION") );

  // %assert (function (form list t list) null))
  add_ftype(Q("%ASSERT"),
    list(Q("FORM"), Q("LIST"), t, Q("LIST")),
    Q("NULL") );

  // %check-type (function (t type-specifier &optional string) t))
  add_ftype(Q("%CHECK-TYPE"),
    list(t, Q("TYPE-SPECIFIER"), Q("&OPTIONAL"), Q("STRING")),
    t );

  // runtime-type-error (function (type-specifier t condition-designator &rest t) nil))
  add_ftype(Q("RUNTIME-TYPE-ERROR"),
    list(Q("TYPE-SPECIFIER"), t, Q("CONDITION-DESIGNATOR"), Q("&REST"), t),
    nil );

  // ensure-package (function (package-designator) package))
  add_ftype(Q("ENSURE-PACKAGE"),
    list(Q("PACKAGE-DESIGNATOR")),
    Q("PACKAGE") );

  // truncate/2 (function (integer integer) (values integer integer)))
  add_ftype(Q("TRUNCATE/2"),
    list(Q("INTEGER"), Q("INTEGER")),
    list(Q("VALUES"), Q("INTEGER"), Q("INTEGER")) );

  // |+/2| (function (number number) number))
  add_ftype(Q("+/2"),
    list(Q("NUMBER"), Q("NUMBER")),
    Q("NUMBER") );

  // -/2 (function (number number) number))
  add_ftype(Q("-/2"),
    list(Q("NUMBER"), Q("NUMBER")),
    Q("NUMBER") );

  // */2 (function (number number) number))
  add_ftype(Q("*/2"),
    list(Q("NUMBER"), Q("NUMBER")),
    Q("NUMBER") );

  // //2 (function (number number) number))
  add_ftype(Q("//2"),
    list(Q("NUMBER"), Q("NUMBER")),
    Q("NUMBER") );

  // -/1 (function (number) number))
  add_ftype(Q("-/1"),
    list(Q("NUMBER")),
    Q("NUMBER") );

  // </2 (function (real real) t))
  add_ftype(Q("</2"),
    list(Q("REAL"), Q("REAL")),
    t );

  // <=/2 (function (real real) t))
  add_ftype(Q("<=/2"),
    list(Q("REAL"), Q("REAL")),
    t );

  // >/2 (function (real real) t))
  add_ftype(Q(">/2"),
    list(Q("REAL"), Q("REAL")),
    t );

  // >=/2 (function (real real) t))
  add_ftype(Q(">=/2"),
    list(Q("REAL"), Q("REAL")),
    t );

  // =/2 (function (real real) t))
  add_ftype(Q("=/2"),
    list(Q("REAL"), Q("REAL")),
    t );

  // /=/2 (function (real real) t))
  add_ftype(Q("/=/2"),
    list(Q("REAL"), Q("REAL")),
    t );

  // gcd/2 (function (integer integer) integer))
  add_ftype(Q("GCD/2"),
    list(Q("INTEGER"), Q("INTEGER")),
    Q("INTEGER") );

  // logand/2 (function (integer integer) integer))
  add_ftype(Q("LOGAND/2"),
    list(Q("INTEGER"), Q("INTEGER")),
    Q("INTEGER") );

  // logeqv/2 (function (integer integer) integer))
  add_ftype(Q("LOGEQV/2"),
    list(Q("INTEGER"), Q("INTEGER")),
    Q("INTEGER") );

  // logior/2 (function (integer integer) integer))
  add_ftype(Q("LOGIOR/2"),
    list(Q("INTEGER"), Q("INTEGER")),
    Q("INTEGER") );

  // logxor/2 (function (integer integer) integer))
  add_ftype(Q("LOGXOR/2"),
    list(Q("INTEGER"), Q("INTEGER")),
    Q("INTEGER") );

  // decode-float32 (function (single-float) (signed-byte 32)))
  add_ftype(Q("DECODE-FLOAT32"),
    list(Q("SINGLE-FLOAT")),
    list(Q("SIGNED-BYTE"), Fixnum::Encode(32)) );

  // decode-float64 (function (double-float) (values (signed-byte 32) (signed-byte 32))))
  add_ftype(Q("DECODE-FLOAT64"),
    list(Q("DOUBLE-FLOAT")),
    list(Q("VALUES"), list(Q("SIGNED-BYTE"), Fixnum::Encode(32)), list(Q("SIGNED-BYTE"), Fixnum::Encode(32))) );

  // encode-float32 (function ((signed-byte 32)) single-float))
  add_ftype(Q("ENCODE-FLOAT32"),
    list(list(Q("SIGNED-BYTE"), Fixnum::Encode(32))),
    Q("SINGLE-FLOAT") );

  // encode-float64 (function ((signed-byte 32) (signed-byte 32)) double-float))
  add_ftype(Q("ENCODE-FLOAT64"),
    list(list(Q("SIGNED-BYTE"), Fixnum::Encode(32)), list(Q("SIGNED-BYTE"), Fixnum::Encode(32))),
    Q("DOUBLE-FLOAT") );

  // decode-float32 (function (single-float) (signed-byte 32)))
  add_ftype(Q("DECODE-FLOAT32"),
    list(Q("SINGLE-FLOAT")),
    list(Q("SIGNED-BYTE"), Fixnum::Encode(32)) );

  // encode-float32 (function (integer) single-float))
  add_ftype(Q("ENCODE-FLOAT32"),
    list(Q("INTEGER")),
    Q("SINGLE-FLOAT") );

  // decode-float64 (function (double-float) (values (signed-byte 32) (signed-byte 32))))
  add_ftype(Q("DECODE-FLOAT64"),
    list(Q("DOUBLE-FLOAT")),
    list(Q("VALUES"), list(Q("SIGNED-BYTE"), Fixnum::Encode(32)), list(Q("SIGNED-BYTE"), Fixnum::Encode(32))) );

  // encode-float64 (function (integer integer) double-float))
  add_ftype(Q("ENCODE-FLOAT64"),
    list(Q("INTEGER"), Q("INTEGER")),
    Q("DOUBLE-FLOAT") );

  // integer-decode-float32 (function (single-float) (values (unsigned-byte 24) (integer -149 255) (member -1 1))))
  add_ftype(Q("INTEGER-DECODE-FLOAT32"),
    list(Q("SINGLE-FLOAT")),
    list(Q("VALUES"), list(Q("UNSIGNED-BYTE"), Fixnum::Encode(24)), list(Q("INTEGER"), Fixnum::Encode(-149), Fixnum::Encode(255)), list(Q("MEMBER"), Fixnum::Encode(-1), Fixnum::Encode(1))) );

  // integer-decode-float64 (function (double-float) (values (unsigned-byte 53) (integer -1074 2047) (member -1 1))))
  add_ftype(Q("INTEGER-DECODE-FLOAT64"),
    list(Q("DOUBLE-FLOAT")),
    list(Q("VALUES"), list(Q("UNSIGNED-BYTE"), Fixnum::Encode(53)), list(Q("INTEGER"), Fixnum::Encode(-1074), Fixnum::Encode(2047)), list(Q("MEMBER"), Fixnum::Encode(-1), Fixnum::Encode(1))) );

  // scale-float32 (function (single-float fixnum) single-float))
  add_ftype(Q("SCALE-FLOAT32"),
    list(Q("SINGLE-FLOAT"), Q("FIXNUM")),
    Q("SINGLE-FLOAT") );

  // scale-float64 (function (double-float fixnum) double-float))
  add_ftype(Q("SCALE-FLOAT64"),
    list(Q("DOUBLE-FLOAT"), Q("FIXNUM")),
    Q("DOUBLE-FLOAT") );

  // float32-precision (function (single-float) (integer 0 24)))
  add_ftype(Q("FLOAT32-PRECISION"),
    list(Q("SINGLE-FLOAT")),
    list(Q("INTEGER"), Fixnum::Encode(0), Fixnum::Encode(24)) );

  // float32-rational (function (single-float) rational))
  add_ftype(Q("FLOAT32-RATIONAL"),
    list(Q("SINGLE-FLOAT")),
    Q("RATIONAL") );

  // float32-rationalize (function (single-float) rational))
  add_ftype(Q("FLOAT32-RATIONALIZE"),
    list(Q("SINGLE-FLOAT")),
    Q("RATIONAL") );

  // float32-sign (function (single-float single-float) single-float))
  add_ftype(Q("FLOAT32-SIGN"),
    list(Q("SINGLE-FLOAT"), Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-truncate (function (single-float) integer))
  add_ftype(Q("FLOAT32-TRUNCATE"),
    list(Q("SINGLE-FLOAT")),
    Q("INTEGER") );

  // float32-fceiling (function (single-float) (single-float)))
  add_ftype(Q("FLOAT32-FCEILING"),
    list(Q("SINGLE-FLOAT")),
    list(Q("SINGLE-FLOAT")) );

  // float32-ffloor (function (single-float) (single-float)))
  add_ftype(Q("FLOAT32-FFLOOR"),
    list(Q("SINGLE-FLOAT")),
    list(Q("SINGLE-FLOAT")) );

  // float32-fround (function (single-float) (single-float)))
  add_ftype(Q("FLOAT32-FROUND"),
    list(Q("SINGLE-FLOAT")),
    list(Q("SINGLE-FLOAT")) );

  // float32-ftruncate (function (single-float) (single-float)))
  add_ftype(Q("FLOAT32-FTRUNCATE"),
    list(Q("SINGLE-FLOAT")),
    list(Q("SINGLE-FLOAT")) );

  // float32-mod (function (single-float single-float) (single-float)))
  add_ftype(Q("FLOAT32-MOD"),
    list(Q("SINGLE-FLOAT"), Q("SINGLE-FLOAT")),
    list(Q("SINGLE-FLOAT")) );

  // float32-rem (function (single-float single-float) (single-float)))
  add_ftype(Q("FLOAT32-REM"),
    list(Q("SINGLE-FLOAT"), Q("SINGLE-FLOAT")),
    list(Q("SINGLE-FLOAT")) );

  // float64-precision (function (double-float) (integer 0 53)))
  add_ftype(Q("FLOAT64-PRECISION"),
    list(Q("DOUBLE-FLOAT")),
    list(Q("INTEGER"), Fixnum::Encode(0), Fixnum::Encode(53)) );

  // float64-rational (function (double-float) rational))
  add_ftype(Q("FLOAT64-RATIONAL"),
    list(Q("DOUBLE-FLOAT")),
    Q("RATIONAL") );

  // float64-rationalize (function (double-float) rational))
  add_ftype(Q("FLOAT64-RATIONALIZE"),
    list(Q("DOUBLE-FLOAT")),
    Q("RATIONAL") );

  // float64-sign (function (double-float double-float) double-float))
  add_ftype(Q("FLOAT64-SIGN"),
    list(Q("DOUBLE-FLOAT"), Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-truncate (function (double-float) integer))
  add_ftype(Q("FLOAT64-TRUNCATE"),
    list(Q("DOUBLE-FLOAT")),
    Q("INTEGER") );

  // float64-fceiling (function (double-float) (double-float)))
  add_ftype(Q("FLOAT64-FCEILING"),
    list(Q("DOUBLE-FLOAT")),
    list(Q("DOUBLE-FLOAT")) );

  // float64-ffloor (function (double-float) (double-float)))
  add_ftype(Q("FLOAT64-FFLOOR"),
    list(Q("DOUBLE-FLOAT")),
    list(Q("DOUBLE-FLOAT")) );

  // float64-fround (function (double-float) (double-float)))
  add_ftype(Q("FLOAT64-FROUND"),
    list(Q("DOUBLE-FLOAT")),
    list(Q("DOUBLE-FLOAT")) );

  // float64-ftruncate (function (double-float) (double-float)))
  add_ftype(Q("FLOAT64-FTRUNCATE"),
    list(Q("DOUBLE-FLOAT")),
    list(Q("DOUBLE-FLOAT")) );

  // float64-mod (function (double-float double-float) (double-float)))
  add_ftype(Q("FLOAT64-MOD"),
    list(Q("DOUBLE-FLOAT"), Q("DOUBLE-FLOAT")),
    list(Q("DOUBLE-FLOAT")) );

  // float64-rem (function (double-float double-float) (double-float)))
  add_ftype(Q("FLOAT64-REM"),
    list(Q("DOUBLE-FLOAT"), Q("DOUBLE-FLOAT")),
    list(Q("DOUBLE-FLOAT")) );

  // float32-rem-pio2 (function (single-float) (values fixnum single-float single-float single-float)))
  add_ftype(Q("FLOAT32-REM-PIO2"),
    list(Q("SINGLE-FLOAT")),
    list(Q("VALUES"), Q("FIXNUM"), Q("SINGLE-FLOAT"), Q("SINGLE-FLOAT"), Q("SINGLE-FLOAT")) );

  // float64-rem-pio2 (function (double-float) (values fixnum double-float double-float double-float)))
  add_ftype(Q("FLOAT64-REM-PIO2"),
    list(Q("DOUBLE-FLOAT")),
    list(Q("VALUES"), Q("FIXNUM"), Q("DOUBLE-FLOAT"), Q("DOUBLE-FLOAT"), Q("DOUBLE-FLOAT")) );

  // float32-kernel-cos (function (single-float single-float) single-float))
  add_ftype(Q("FLOAT32-KERNEL-COS"),
    list(Q("SINGLE-FLOAT"), Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-kernel-sin (function (single-float single-float fixnum) single-float))
  add_ftype(Q("FLOAT32-KERNEL-SIN"),
    list(Q("SINGLE-FLOAT"), Q("SINGLE-FLOAT"), Q("FIXNUM")),
    Q("SINGLE-FLOAT") );

  // float32-kernel-tan (function (single-float single-float fixnum) single-float))
  add_ftype(Q("FLOAT32-KERNEL-TAN"),
    list(Q("SINGLE-FLOAT"), Q("SINGLE-FLOAT"), Q("FIXNUM")),
    Q("SINGLE-FLOAT") );

  // float64-kernel-cos (function (double-float double-float) double-float))
  add_ftype(Q("FLOAT64-KERNEL-COS"),
    list(Q("DOUBLE-FLOAT"), Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-kernel-sin (function (double-float double-float fixnum) double-float))
  add_ftype(Q("FLOAT64-KERNEL-SIN"),
    list(Q("DOUBLE-FLOAT"), Q("DOUBLE-FLOAT"), Q("FIXNUM")),
    Q("DOUBLE-FLOAT") );

  // float64-kernel-tan (function (double-float double-float fixnum) double-float))
  add_ftype(Q("FLOAT64-KERNEL-TAN"),
    list(Q("DOUBLE-FLOAT"), Q("DOUBLE-FLOAT"), Q("FIXNUM")),
    Q("DOUBLE-FLOAT") );

  // float32-abs (function (single-float) single-float))
  add_ftype(Q("FLOAT32-ABS"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-acos (function (single-float) single-float))
  add_ftype(Q("FLOAT32-ACOS"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-acosh (function (single-float) single-float))
  add_ftype(Q("FLOAT32-ACOSH"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-asin (function (single-float) single-float))
  add_ftype(Q("FLOAT32-ASIN"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-asinh (function (single-float) single-float))
  add_ftype(Q("FLOAT32-ASINH"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-atan/1 (function (single-float) single-float))
  add_ftype(Q("FLOAT32-ATAN/1"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-atan/2 (function (single-float single-float) single-float))
  add_ftype(Q("FLOAT32-ATAN/2"),
    list(Q("SINGLE-FLOAT"), Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-atanh (function (single-float) single-float))
  add_ftype(Q("FLOAT32-ATANH"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-cos (function (single-float) single-float))
  add_ftype(Q("FLOAT32-COS"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-cosh (function (single-float) single-float))
  add_ftype(Q("FLOAT32-COSH"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-exp (function (single-float) single-float))
  add_ftype(Q("FLOAT32-EXP"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-expm1 (function (single-float) single-float))
  add_ftype(Q("FLOAT32-EXPM1"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-expt (function (single-float single-float) single-float))
  add_ftype(Q("FLOAT32-EXPT"),
    list(Q("SINGLE-FLOAT"), Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-hypot (function (single-float single-float) single-float))
  add_ftype(Q("FLOAT32-HYPOT"),
    list(Q("SINGLE-FLOAT"), Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-log (function (single-float) single-float))
  add_ftype(Q("FLOAT32-LOG"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-log1p (function (single-float) single-float))
  add_ftype(Q("FLOAT32-LOG1P"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-sin (function (single-float) single-float))
  add_ftype(Q("FLOAT32-SIN"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-sinh (function (single-float) single-float))
  add_ftype(Q("FLOAT32-SINH"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-sqrt (function (single-float) single-float))
  add_ftype(Q("FLOAT32-SQRT"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-tan (function (single-float) single-float))
  add_ftype(Q("FLOAT32-TAN"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float32-tanh (function (single-float) single-float))
  add_ftype(Q("FLOAT32-TANH"),
    list(Q("SINGLE-FLOAT")),
    Q("SINGLE-FLOAT") );

  // float64-abs (function (double-float) double-float))
  add_ftype(Q("FLOAT64-ABS"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-acos (function (double-float) double-float))
  add_ftype(Q("FLOAT64-ACOS"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-acosh (function (double-float) double-float))
  add_ftype(Q("FLOAT64-ACOSH"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-asin (function (double-float) double-float))
  add_ftype(Q("FLOAT64-ASIN"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-asinh (function (double-float) double-float))
  add_ftype(Q("FLOAT64-ASINH"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-atan/1 (function (double-float) double-float))
  add_ftype(Q("FLOAT64-ATAN/1"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-atan/2 (function (double-float double-float) double-float))
  add_ftype(Q("FLOAT64-ATAN/2"),
    list(Q("DOUBLE-FLOAT"), Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-atanh (function (double-float) double-float))
  add_ftype(Q("FLOAT64-ATANH"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-cos (function (double-float) double-float))
  add_ftype(Q("FLOAT64-COS"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-cosh (function (double-float) double-float))
  add_ftype(Q("FLOAT64-COSH"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-exp (function (double-float) double-float))
  add_ftype(Q("FLOAT64-EXP"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-expm1 (function (double-float) double-float))
  add_ftype(Q("FLOAT64-EXPM1"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-expt (function (double-float double-float) double-float))
  add_ftype(Q("FLOAT64-EXPT"),
    list(Q("DOUBLE-FLOAT"), Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-hypot (function (double-float double-float) double-float))
  add_ftype(Q("FLOAT64-HYPOT"),
    list(Q("DOUBLE-FLOAT"), Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-log (function (double-float) double-float))
  add_ftype(Q("FLOAT64-LOG"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-log1p (function (double-float) double-float))
  add_ftype(Q("FLOAT64-LOG1P"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-sin (function (double-float) double-float))
  add_ftype(Q("FLOAT64-SIN"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-sinh (function (double-float) double-float))
  add_ftype(Q("FLOAT64-SINH"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-sqrt (function (double-float) double-float))
  add_ftype(Q("FLOAT64-SQRT"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-tan (function (double-float) double-float))
  add_ftype(Q("FLOAT64-TAN"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // float64-tanh (function (double-float) double-float))
  add_ftype(Q("FLOAT64-TANH"),
    list(Q("DOUBLE-FLOAT")),
    Q("DOUBLE-FLOAT") );

  // char=/2 (function (character character) t))
  add_ftype(Q("CHAR=/2"),
    list(Q("CHARACTER"), Q("CHARACTER")),
    t );

  // char/=/2 (function (character character) t))
  add_ftype(Q("CHAR/=/2"),
    list(Q("CHARACTER"), Q("CHARACTER")),
    t );

  // char</2 (function (character character) t))
  add_ftype(Q("CHAR</2"),
    list(Q("CHARACTER"), Q("CHARACTER")),
    t );

  // char<=/2 (function (character character) t))
  add_ftype(Q("CHAR<=/2"),
    list(Q("CHARACTER"), Q("CHARACTER")),
    t );

  // char>/2 (function (character character) t))
  add_ftype(Q("CHAR>/2"),
    list(Q("CHARACTER"), Q("CHARACTER")),
    t );

  // char>=/2 (function (character character) t))
  add_ftype(Q("CHAR>=/2"),
    list(Q("CHARACTER"), Q("CHARACTER")),
    t );

  // char-equal/2 (function (character character) t))
  add_ftype(Q("CHAR-EQUAL/2"),
    list(Q("CHARACTER"), Q("CHARACTER")),
    t );

  // char-not-equal/2 (function (character character) t))
  add_ftype(Q("CHAR-NOT-EQUAL/2"),
    list(Q("CHARACTER"), Q("CHARACTER")),
    t );

  // char-lessp/2 (function (character character) t))
  add_ftype(Q("CHAR-LESSP/2"),
    list(Q("CHARACTER"), Q("CHARACTER")),
    t );

  // char-not-greaterp/2 (function (character character) t))
  add_ftype(Q("CHAR-NOT-GREATERP/2"),
    list(Q("CHARACTER"), Q("CHARACTER")),
    t );

  // char-greaterp/2 (function (character character) t))
  add_ftype(Q("CHAR-GREATERP/2"),
    list(Q("CHARACTER"), Q("CHARACTER")),
    t );

  // char-not-lessp/2 (function (character character) t))
  add_ftype(Q("CHAR-NOT-LESSP/2"),
    list(Q("CHARACTER"), Q("CHARACTER")),
    t );

  // character-category (function (character) unicode:category))
  add_ftype(Q("CHARACTER-CATEGORY"),
    list(Q("CHARACTER")),
    Q("UNICODE:CATEGORY") );

  // whitespace-char-p (function (character) t))
  add_ftype(Q("WHITESPACE-CHAR-P"),
    list(Q("CHARACTER")),
    t );

  // assq (function (t list) list))
  add_ftype(Q("ASSQ"),
    list(t, Q("LIST")),
    Q("LIST") );

  // delq (function (t list) list))
  add_ftype(Q("DELQ"),
    list(t, Q("LIST")),
    Q("LIST") );

  // memq (function (t list) list))
  add_ftype(Q("MEMQ"),
    list(t, Q("LIST")),
    Q("LIST") );

  // mapc/1 (function (function-designator list) list))
  add_ftype(Q("MAPC/1"),
    list(Q("FUNCTION-DESIGNATOR"), Q("LIST")),
    Q("LIST") );

  // mapcar/1 (function (function-designator list) list))
  add_ftype(Q("MAPCAR/1"),
    list(Q("FUNCTION-DESIGNATOR"), Q("LIST")),
    Q("LIST") );

  // mapcan/1 (function (function-designator list) list))
  add_ftype(Q("MAPCAN/1"),
    list(Q("FUNCTION-DESIGNATOR"), Q("LIST")),
    Q("LIST") );

  // mapl/1 (function (function-designator list) list))
  add_ftype(Q("MAPL/1"),
    list(Q("FUNCTION-DESIGNATOR"), Q("LIST")),
    Q("LIST") );

  // maplist/1 (function (function-designator list) list))
  add_ftype(Q("MAPLIST/1"),
    list(Q("FUNCTION-DESIGNATOR"), Q("LIST")),
    Q("LIST") );

  // mapcon/1 (function (function-designator list) list))
  add_ftype(Q("MAPCON/1"),
    list(Q("FUNCTION-DESIGNATOR"), Q("LIST")),
    Q("LIST") );

  // mapc/2 (function (function-designator list list) list))
  add_ftype(Q("MAPC/2"),
    list(Q("FUNCTION-DESIGNATOR"), Q("LIST"), Q("LIST")),
    Q("LIST") );

  // mapcar/2 (function (function-designator list list) list))
  add_ftype(Q("MAPCAR/2"),
    list(Q("FUNCTION-DESIGNATOR"), Q("LIST"), Q("LIST")),
    Q("LIST") );

  // mapcan/2 (function (function-designator list list) list))
  add_ftype(Q("MAPCAN/2"),
    list(Q("FUNCTION-DESIGNATOR"), Q("LIST"), Q("LIST")),
    Q("LIST") );

  // mapl/2 (function (function-designator list list) list))
  add_ftype(Q("MAPL/2"),
    list(Q("FUNCTION-DESIGNATOR"), Q("LIST"), Q("LIST")),
    Q("LIST") );

  // maplist/2 (function (function-designator list list) list))
  add_ftype(Q("MAPLIST/2"),
    list(Q("FUNCTION-DESIGNATOR"), Q("LIST"), Q("LIST")),
    Q("LIST") );

  // mapcon/2 (function (function-designator list list) list))
  add_ftype(Q("MAPCON/2"),
    list(Q("FUNCTION-DESIGNATOR"), Q("LIST"), Q("LIST")),
    Q("LIST") );

  // plist-put (function (list t t) list))
  add_ftype(Q("PLIST-PUT"),
    list(Q("LIST"), t, t),
    Q("LIST") );

  // plist-rem (function (list t) (values list t)))
  add_ftype(Q("PLIST-REM"),
    list(Q("LIST"), t),
    list(Q("VALUES"), Q("LIST"), t) );

  // proper-list-p (function (t) (values (or sequence-index null))))
  add_ftype(Q("PROPER-LIST-P"),
    list(t),
    list(Q("VALUES"), list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL"))) );

  // safe-list-length (function (t) fixnum))
  add_ftype(Q("SAFE-LIST-LENGTH"),
    list(t),
    Q("FIXNUM") );

  // array-data (function (array) (values data-vector sequence-index sequence-index)))
  add_ftype(Q("ARRAY-DATA"),
    list(Q("ARRAY")),
    list(Q("VALUES"), Q("DATA-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")) );

  // array-index-error (function (array sequence-index) nil))
  add_ftype(Q("ARRAY-INDEX-ERROR"),
    list(Q("ARRAY"), Q("SEQUENCE-INDEX")),
    nil );

  // bit-not/2 (function (simple-bit-vector simple-bit-vector sequence-index sequence-index sequence-index) simple-bit-vector))
  add_ftype(Q("BIT-NOT/2"),
    list(Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    Q("SIMPLE-BIT-VECTOR") );

  // bit-and/2 (function (simple-bit-vector simple-bit-vector simple-bit-vector sequence-index sequence-index sequence-index sequence-index) simple-bit-vector))
  add_ftype(Q("BIT-AND/2"),
    list(Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    Q("SIMPLE-BIT-VECTOR") );

  // bit-andc1/2 (function (simple-bit-vector simple-bit-vector simple-bit-vector sequence-index sequence-index sequence-index sequence-index) simple-bit-vector))
  add_ftype(Q("BIT-ANDC1/2"),
    list(Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    Q("SIMPLE-BIT-VECTOR") );

  // bit-andc2/2 (function (simple-bit-vector simple-bit-vector simple-bit-vector sequence-index sequence-index sequence-index sequence-index) simple-bit-vector))
  add_ftype(Q("BIT-ANDC2/2"),
    list(Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    Q("SIMPLE-BIT-VECTOR") );

  // bit-nand/2 (function (simple-bit-vector simple-bit-vector simple-bit-vector sequence-index sequence-index sequence-index sequence-index) simple-bit-vector))
  add_ftype(Q("BIT-NAND/2"),
    list(Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    Q("SIMPLE-BIT-VECTOR") );

  // bit-ior/2 (function (simple-bit-vector simple-bit-vector simple-bit-vector sequence-index sequence-index sequence-index sequence-index) simple-bit-vector))
  add_ftype(Q("BIT-IOR/2"),
    list(Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    Q("SIMPLE-BIT-VECTOR") );

  // bit-orc1/2 (function (simple-bit-vector simple-bit-vector simple-bit-vector sequence-index sequence-index sequence-index sequence-index) simple-bit-vector))
  add_ftype(Q("BIT-ORC1/2"),
    list(Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    Q("SIMPLE-BIT-VECTOR") );

  // bit-orc2/2 (function (simple-bit-vector simple-bit-vector simple-bit-vector sequence-index sequence-index sequence-index sequence-index) simple-bit-vector))
  add_ftype(Q("BIT-ORC2/2"),
    list(Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    Q("SIMPLE-BIT-VECTOR") );

  // bit-nor/2 (function (simple-bit-vector simple-bit-vector simple-bit-vector sequence-index sequence-index sequence-index sequence-index) simple-bit-vector))
  add_ftype(Q("BIT-NOR/2"),
    list(Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    Q("SIMPLE-BIT-VECTOR") );

  // bit-eqv/2 (function (simple-bit-vector simple-bit-vector simple-bit-vector sequence-index sequence-index sequence-index sequence-index) simple-bit-vector))
  add_ftype(Q("BIT-EQV/2"),
    list(Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    Q("SIMPLE-BIT-VECTOR") );

  // bit-xor/2 (function (simple-bit-vector simple-bit-vector simple-bit-vector sequence-index sequence-index sequence-index sequence-index) simple-bit-vector))
  add_ftype(Q("BIT-XOR/2"),
    list(Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    Q("SIMPLE-BIT-VECTOR") );

  // bounding-index-error (function (sequence sequence-index sequence-end) nil))
  add_ftype(Q("BOUNDING-INDEX-ERROR"),
    list(Q("SEQUENCE"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END")),
    nil );

  // internal-adjust-array (function (array list t array sequence-index) array))
  add_ftype(Q("INTERNAL-ADJUST-ARRAY"),
    list(Q("ARRAY"), Q("LIST"), t, Q("ARRAY"), Q("SEQUENCE-INDEX")),
    Q("ARRAY") );

  // internal-adjust-vector (function (vector sequence-index t array sequence-index t sequence-index) vector))
  add_ftype(Q("INTERNAL-ADJUST-VECTOR"),
    list(Q("VECTOR"), Q("SEQUENCE-INDEX"), t, Q("ARRAY"), Q("SEQUENCE-INDEX"), t, Q("SEQUENCE-INDEX")),
    Q("VECTOR") );

  // internal-make-array (function (list t array sequence-index t) array))
  add_ftype(Q("INTERNAL-MAKE-ARRAY"),
    list(Q("LIST"), t, Q("ARRAY"), Q("SEQUENCE-INDEX"), t),
    Q("ARRAY") );

  // make-simple-vector (function (sequence-index &optional t) simple-vector))
  add_ftype(Q("MAKE-SIMPLE-VECTOR"),
    list(Q("SEQUENCE-INDEX"), Q("&OPTIONAL"), t),
    Q("SIMPLE-VECTOR") );

  // vector-data (function (vector &optional sequence-index sequence-end) (values data-vector sequence-index sequence-index)))
  add_ftype(Q("VECTOR-DATA"),
    list(Q("VECTOR"), Q("&OPTIONAL"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END")),
    list(Q("VALUES"), Q("DATA-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")) );

  // sbit/2 (function (simple-bit-vector sequence-index) bit))
  add_ftype(Q("SBIT/2"),
    list(Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX")),
    Q("BIT") );

  // (setf sbit/2) (function (bit simple-bit-vector sequence-index) bit))
  add_ftype(intern_setf_cell(Q("SBIT/2")),
    list(Q("BIT"), Q("SIMPLE-BIT-VECTOR"), Q("SEQUENCE-INDEX")),
    Q("BIT") );

  // .allocate-string (function (sequence-index) simple-string))
  add_ftype(Q(".ALLOCATE-STRING"),
    list(Q("SEQUENCE-INDEX")),
    Q("SIMPLE-STRING") );

  // string-ci-compare (function (string string sequence-index sequence-index sequence-index sequence-index) (values fixnum sequence-index)))
  add_ftype(Q("STRING-CI-COMPARE"),
    list(Q("STRING"), Q("STRING"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    list(Q("VALUES"), Q("FIXNUM"), Q("SEQUENCE-INDEX")) );

  // string-cs-compare (function (string string sequence-index sequence-index sequence-index sequence-index) (values fixnum sequence-index)))
  add_ftype(Q("STRING-CS-COMPARE"),
    list(Q("STRING"), Q("STRING"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")),
    list(Q("VALUES"), Q("FIXNUM"), Q("SEQUENCE-INDEX")) );

  // string-data (function (string &optional sequence-index sequence-end) (values simple-string sequence-index sequence-index)))
  add_ftype(Q("STRING-DATA"),
    list(Q("STRING"), Q("&OPTIONAL"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END")),
    list(Q("VALUES"), Q("SIMPLE-STRING"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-INDEX")) );

  // coerce-sublist-to-vector (function (list sequence-index sequence-end) (values simple-vector sequence-index)))
  add_ftype(Q("COERCE-SUBLIST-TO-VECTOR"),
    list(Q("LIST"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END")),
    list(Q("VALUES"), Q("SIMPLE-VECTOR"), Q("SEQUENCE-INDEX")) );

  // ensure-sequence-index (function (sequence sequence-index) sequence-index))
  add_ftype(Q("ENSURE-SEQUENCE-INDEX"),
    list(Q("SEQUENCE"), Q("SEQUENCE-INDEX")),
    Q("SEQUENCE-INDEX") );

  // ensure-bounding-indexes (function (sequence sequence-index sequence-end) sequence-index))
  add_ftype(Q("ENSURE-BOUNDING-INDEXES"),
    list(Q("SEQUENCE"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END")),
    Q("SEQUENCE-INDEX") );

  // length/list (function (list) sequence-index))
  add_ftype(Q("LENGTH/LIST"),
    list(Q("LIST")),
    Q("SEQUENCE-INDEX") );

  // length/vector (function (vector) sequence-index))
  add_ftype(Q("LENGTH/VECTOR"),
    list(Q("VECTOR")),
    Q("SEQUENCE-INDEX") );

  // mismatch/list/list/backward (function (list list sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("MISMATCH/LIST/LIST/BACKWARD"),
    list(Q("LIST"), Q("LIST"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // mismatch/list/list/forward (function (list list sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("MISMATCH/LIST/LIST/FORWARD"),
    list(Q("LIST"), Q("LIST"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // mismatch/list/vector/backward (function (list vector sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("MISMATCH/LIST/VECTOR/BACKWARD"),
    list(Q("LIST"), Q("VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // mismatch/list/vector/forward (function (list vector sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("MISMATCH/LIST/VECTOR/FORWARD"),
    list(Q("LIST"), Q("VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // mismatch/vector/list/backward (function (vector list sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("MISMATCH/VECTOR/LIST/BACKWARD"),
    list(Q("VECTOR"), Q("LIST"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // mismatch/vector/list/forward (function (vector list sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("MISMATCH/VECTOR/LIST/FORWARD"),
    list(Q("VECTOR"), Q("LIST"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // mismatch/vector/vector/backward (function (vector vector sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("MISMATCH/VECTOR/VECTOR/BACKWARD"),
    list(Q("VECTOR"), Q("VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // mismatch/vector/vector/forward (function (vector vector sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("MISMATCH/VECTOR/VECTOR/FORWARD"),
    list(Q("VECTOR"), Q("VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // remove/data-vector/backward (function (t data-vector sequence-index sequence-end function-designator function-designator fixnum) data-vector))
  add_ftype(Q("REMOVE/DATA-VECTOR/BACKWARD"),
    list(t, Q("DATA-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR"), Q("FIXNUM")),
    Q("DATA-VECTOR") );

  // remove-duplicates/data-vector/backward (function (data-vector sequence-index sequence-end function-designator function-designator) data-vector))
  add_ftype(Q("REMOVE-DUPLICATES/DATA-VECTOR/BACKWARD"),
    list(Q("DATA-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    Q("DATA-VECTOR") );

  // remove-if/data-vector/backward (function (function-designator data-vector sequence-index sequence-end function-designator fixnum) data-vector))
  add_ftype(Q("REMOVE-IF/DATA-VECTOR/BACKWARD"),
    list(Q("FUNCTION-DESIGNATOR"), Q("DATA-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FIXNUM")),
    Q("DATA-VECTOR") );

  // remove-if-not/data-vector/backward (function (function-designator data-vector sequence-index sequence-end function-designator fixnum) data-vector))
  add_ftype(Q("REMOVE-IF-NOT/DATA-VECTOR/BACKWARD"),
    list(Q("FUNCTION-DESIGNATOR"), Q("DATA-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FIXNUM")),
    Q("DATA-VECTOR") );

  // remove/data-vector/forward (function (t data-vector sequence-index sequence-end function-designator function-designator fixnum) data-vector))
  add_ftype(Q("REMOVE/DATA-VECTOR/FORWARD"),
    list(t, Q("DATA-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR"), Q("FIXNUM")),
    Q("DATA-VECTOR") );

  // remove-duplicates/data-vector/forward (function (data-vector sequence-index sequence-end function-designator function-designator) data-vector))
  add_ftype(Q("REMOVE-DUPLICATES/DATA-VECTOR/FORWARD"),
    list(Q("DATA-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    Q("DATA-VECTOR") );

  // remove-if/data-vector/forward (function (function-designator data-vector sequence-index sequence-end function-designator fixnum) data-vector))
  add_ftype(Q("REMOVE-IF/DATA-VECTOR/FORWARD"),
    list(Q("FUNCTION-DESIGNATOR"), Q("DATA-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FIXNUM")),
    Q("DATA-VECTOR") );

  // remove-if-not/data-vector/forward (function (function-designator data-vector sequence-index sequence-end function-designator fixnum) data-vector))
  add_ftype(Q("REMOVE-IF-NOT/DATA-VECTOR/FORWARD"),
    list(Q("FUNCTION-DESIGNATOR"), Q("DATA-VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FIXNUM")),
    Q("DATA-VECTOR") );

  // replace/list/list (function (list list sequence-index sequence-end sequence-index sequence-end) list))
  add_ftype(Q("REPLACE/LIST/LIST"),
    list(Q("LIST"), Q("LIST"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END")),
    Q("LIST") );

  // replace/list/vector (function (list vector sequence-index sequence-end sequence-index sequence-end) list))
  add_ftype(Q("REPLACE/LIST/VECTOR"),
    list(Q("LIST"), Q("VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END")),
    Q("LIST") );

  // replace/vector/list (function (vector list sequence-index sequence-end sequence-index sequence-end) vector))
  add_ftype(Q("REPLACE/VECTOR/LIST"),
    list(Q("VECTOR"), Q("LIST"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END")),
    Q("VECTOR") );

  // replace/vector/vector (function (vector vector sequence-index sequence-end sequence-index sequence-end) vector))
  add_ftype(Q("REPLACE/VECTOR/VECTOR"),
    list(Q("VECTOR"), Q("VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END")),
    Q("VECTOR") );

  // sequence-index-error (function (sequence integer) nil))
  add_ftype(Q("SEQUENCE-INDEX-ERROR"),
    list(Q("SEQUENCE"), Q("INTEGER")),
    nil );

  // sequence-index-errorex (function (sequence integer integer) nil))
  add_ftype(Q("SEQUENCE-INDEX-ERROREX"),
    list(Q("SEQUENCE"), Q("INTEGER"), Q("INTEGER")),
    nil );

  // search/list/list/backward (function (list list sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("SEARCH/LIST/LIST/BACKWARD"),
    list(Q("LIST"), Q("LIST"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // search/list/list/forward (function (list list sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("SEARCH/LIST/LIST/FORWARD"),
    list(Q("LIST"), Q("LIST"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // search/list/vector/backward (function (list vector sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("SEARCH/LIST/VECTOR/BACKWARD"),
    list(Q("LIST"), Q("VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // search/list/vector/forward (function (list vector sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("SEARCH/LIST/VECTOR/FORWARD"),
    list(Q("LIST"), Q("VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // search/vector/list/backward (function (vector list sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("SEARCH/VECTOR/LIST/BACKWARD"),
    list(Q("VECTOR"), Q("LIST"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // search/vector/list/forward (function (vector list sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("SEARCH/VECTOR/LIST/FORWARD"),
    list(Q("VECTOR"), Q("LIST"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // search/vector/vector/backward (function (vector vector sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("SEARCH/VECTOR/VECTOR/BACKWARD"),
    list(Q("VECTOR"), Q("VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // search/vector/vector/forward (function (vector vector sequence-index sequence-end sequence-index sequence-end function-designator function-designator) (or sequence-index null)))
  add_ftype(Q("SEARCH/VECTOR/VECTOR/FORWARD"),
    list(Q("VECTOR"), Q("VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    list(Q("OR"), Q("SEQUENCE-INDEX"), Q("NULL")) );

  // stable-sort/list (function (list function-designator function-designator) list))
  add_ftype(Q("STABLE-SORT/LIST"),
    list(Q("LIST"), Q("FUNCTION-DESIGNATOR"), Q("FUNCTION-DESIGNATOR")),
    Q("LIST") );

  // subseq/list (function (list sequence-index sequence-end) list))
  add_ftype(Q("SUBSEQ/LIST"),
    list(Q("LIST"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END")),
    Q("LIST") );

  // subseq/vector (function (vector sequence-index sequence-end) vector))
  add_ftype(Q("SUBSEQ/VECTOR"),
    list(Q("VECTOR"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END")),
    Q("VECTOR") );

  // gethash/eq (function (t hash-table &optional t) (values t t)))
  add_ftype(Q("GETHASH/EQ"),
    list(t, Q("HASH-TABLE"), Q("&OPTIONAL"), t),
    list(Q("VALUES"), t, t) );

  // (setf gethash/eq) (function (t t hash-table &optional t) t))
  add_ftype(intern_setf_cell(Q("GETHASH/EQ")),
    list(t, t, Q("HASH-TABLE"), Q("&OPTIONAL"), t),
    t );

  // sxhash/eql (function (t) positive-fixnum))
  add_ftype(Q("SXHASH/EQL"),
    list(t),
    Q("POSITIVE-FIXNUM") );

  // sxhash/eql (function (t) positive-fixnum))
  add_ftype(Q("SXHASH/EQL"),
    list(t),
    Q("POSITIVE-FIXNUM") );

  // sxhash/equal (function (t) positive-fixnum))
  add_ftype(Q("SXHASH/EQUAL"),
    list(t),
    Q("POSITIVE-FIXNUM") );

  // sxhash/equalp (function (t) positive-fixnum))
  add_ftype(Q("SXHASH/EQUALP"),
    list(t),
    Q("POSITIVE-FIXNUM") );

  // host-name (function (basic-host) string))
  add_ftype(Q("HOST-NAME"),
    list(Q("BASIC-HOST")),
    Q("STRING") );

  // make-logical-host (function (string) logical-host))
  add_ftype(Q("MAKE-LOGICAL-HOST"),
    list(Q("STRING")),
    Q("LOGICAL-HOST") );

  // logical-unparse-element (function (t stream) t))
  add_ftype(Q("LOGICAL-UNPARSE-ELEMENT"),
    list(t, Q("STREAM")),
    t );

  // make-windows-host (function (string) windows-host))
  add_ftype(Q("MAKE-WINDOWS-HOST"),
    list(Q("STRING")),
    Q("WINDOWS-HOST") );

  // windows-unparse-element (function (t stream) t))
  add_ftype(Q("WINDOWS-UNPARSE-ELEMENT"),
    list(t, Q("STREAM")),
    t );

  // parse-namestring-using-host (function (basic-host string sequence-index sequence-end) (values pathname sequence-index)))
  add_ftype(Q("PARSE-NAMESTRING-USING-HOST"),
    list(Q("BASIC-HOST"), Q("STRING"), Q("SEQUENCE-INDEX"), Q("SEQUENCE-END")),
    list(Q("VALUES"), Q("PATHNAME"), Q("SEQUENCE-INDEX")) );

  // pathname-equal (function (pathname pathname) t))
  add_ftype(Q("PATHNAME-EQUAL"),
    list(Q("PATHNAME"), Q("PATHNAME")),
    t );

  // pathname-sxhash (function (pathname) positive-fixnum))
  add_ftype(Q("PATHNAME-SXHASH"),
    list(Q("PATHNAME")),
    Q("POSITIVE-FIXNUM") );

  // .current-directory (function (&optional (or fixnum null)) simple-string))
  add_ftype(Q(".CURRENT-DIRECTORY"),
    list(Q("&OPTIONAL"), list(Q("OR"), Q("FIXNUM"), Q("NULL"))),
    Q("SIMPLE-STRING") );

  // (setf current-directory) (function (simple-string &optional (or fixnum null)) simple-string))
  add_ftype(intern_setf_cell(Q("CURRENT-DIRECTORY")),
    list(Q("SIMPLE-STRING"), Q("&OPTIONAL"), list(Q("OR"), Q("FIXNUM"), Q("NULL"))),
    Q("SIMPLE-STRING") );

  // ensure-physical-pathname (function (pathname-designator) pathname))
  add_ftype(Q("ENSURE-PHYSICAL-PATHNAME"),
    list(Q("PATHNAME-DESIGNATOR")),
    Q("PATHNAME") );

  // .create-directory (function (t) t))
  add_ftype(Q(".CREATE-DIRECTORY"),
    list(t),
    t );

  // .delete-file (function (t) t))
  add_ftype(Q(".DELETE-FILE"),
    list(t),
    t );

  // .file-attributes (function (t) (values fixnum unsigned-byte unsigned-byte unsigned-byte unsigned-byte)))
  add_ftype(Q(".FILE-ATTRIBUTES"),
    list(t),
    list(Q("VALUES"), Q("FIXNUM"), Q("UNSIGNED-BYTE"), Q("UNSIGNED-BYTE"), Q("UNSIGNED-BYTE"), Q("UNSIGNED-BYTE")) );

  // .find-close (function (handle) null))
  add_ftype(Q(".FIND-CLOSE"),
    list(Q("HANDLE")),
    Q("NULL") );

  // .find-first-file (function (string) (values handle (or null string) fixnum)))
  add_ftype(Q(".FIND-FIRST-FILE"),
    list(Q("STRING")),
    list(Q("VALUES"), Q("HANDLE"), list(Q("OR"), Q("NULL"), Q("STRING")), Q("FIXNUM")) );

  // .find-next-file (function (handle) (values (or null string) fixnum)))
  add_ftype(Q(".FIND-NEXT-FILE"),
    list(Q("HANDLE")),
    list(Q("VALUES"), list(Q("OR"), Q("NULL"), Q("STRING")), Q("FIXNUM")) );

  // .remove-directory (function (t) t))
  add_ftype(Q(".REMOVE-DIRECTORY"),
    list(t),
    t );

  // .rename-file (function (string string) t))
  add_ftype(Q(".RENAME-FILE"),
    list(Q("STRING"), Q("STRING")),
    t );

  // rename-if-exists (function (pathname-designator) (or pathname null)))
  add_ftype(Q("RENAME-IF-EXISTS"),
    list(Q("PATHNAME-DESIGNATOR")),
    list(Q("OR"), Q("PATHNAME"), Q("NULL")) );

  // close/2 (function ((or stream null) t) t))
  add_ftype(Q("CLOSE/2"),
    list(list(Q("OR"), Q("STREAM"), Q("NULL")), t),
    t );

  // stream-pathname (function (stream) pathname))
  add_ftype(Q("STREAM-PATHNAME"),
    list(Q("STREAM")),
    Q("PATHNAME") );

  // make-pooled-string-output-stream (function nil string-output-stream))
  add_ftype(Q("MAKE-POOLED-STRING-OUTPUT-STREAM"),
    nil,
    Q("STRING-OUTPUT-STREAM") );

  // free-pooled-string-output-stream (function (string-output-stream) null))
  add_ftype(Q("FREE-POOLED-STRING-OUTPUT-STREAM"),
    list(Q("STRING-OUTPUT-STREAM")),
    Q("NULL") );

  // pprint-logical-block-function (function (stream t string (or string null) string function) unspecified))
  add_ftype(Q("PPRINT-LOGICAL-BLOCK-FUNCTION"),
    list(Q("STREAM"), t, Q("STRING"), list(Q("OR"), Q("STRING"), Q("NULL")), Q("STRING"), Q("FUNCTION")),
    Q("UNSPECIFIED") );

  // pprint-object (function (t t) t))
  add_ftype(Q("PPRINT-OBJECT"),
    list(t, t),
    t );

  // print-unreadable-object-function (function (t stream t t (or function null)) null))
  add_ftype(Q("PRINT-UNREADABLE-OBJECT-FUNCTION"),
    list(t, Q("STREAM"), t, t, list(Q("OR"), Q("FUNCTION"), Q("NULL"))),
    Q("NULL") );

  // write-object (function (t stream) t))
  add_ftype(Q("WRITE-OBJECT"),
    list(t, Q("STREAM")),
    t );

  // discard-token (function ((or character null) stream) (values (member :delimiter :eof :macro nil) (or character null))))
  add_ftype(Q("DISCARD-TOKEN"),
    list(list(Q("OR"), Q("CHARACTER"), Q("NULL")), Q("STREAM")),
    list(Q("VALUES"), list(Q("MEMBER"), Q(":DELIMITER"), Q(":EOF"), Q(":MACRO"), nil), list(Q("OR"), Q("CHARACTER"), Q("NULL"))) );

  // fast-read-char (function (stream t) (or character null)))
  add_ftype(Q("FAST-READ-CHAR"),
    list(Q("STREAM"), t),
    list(Q("OR"), Q("CHARACTER"), Q("NULL")) );

  // read-char-and-attr (function (stream readtable) (values (or character null) fixnum)))
  add_ftype(Q("READ-CHAR-AND-ATTR"),
    list(Q("STREAM"), Q("READTABLE")),
    list(Q("VALUES"), list(Q("OR"), Q("CHARACTER"), Q("NULL")), Q("FIXNUM")) );

  // read-token (function ((or character null) stream) (values (member :delimiter :eof :macro :token) (or character null reader-token))))
  add_ftype(Q("READ-TOKEN"),
    list(list(Q("OR"), Q("CHARACTER"), Q("NULL")), Q("STREAM")),
    list(Q("VALUES"), list(Q("MEMBER"), Q(":DELIMITER"), Q(":EOF"), Q(":MACRO"), Q(":TOKEN")), list(Q("OR"), Q("CHARACTER"), Q("NULL"), Q("READER-TOKEN"))) );

  // simple-reader-error (function (stream format-control &rest t) nil))
  add_ftype(Q("SIMPLE-READER-ERROR"),
    list(Q("STREAM"), Q("FORMAT-CONTROL"), Q("&REST"), t),
    nil );

  // address-of (function (t) fixnum))
  add_ftype(Q("ADDRESS-OF"),
    list(t),
    Q("FIXNUM") );

  // find-plist-cell (function (symbol) (or plist-cell null)))
  add_ftype(Q("FIND-PLIST-CELL"),
    list(Q("SYMBOL")),
    list(Q("OR"), Q("PLIST-CELL"), Q("NULL")) );

  // intern-plist-cell (function (symbol) plist-cell))
  add_ftype(Q("INTERN-PLIST-CELL"),
    list(Q("SYMBOL")),
    Q("PLIST-CELL") );

  // find-setf-cell (function (symbol) (or setf-cell null)))
  add_ftype(Q("FIND-SETF-CELL"),
    list(Q("SYMBOL")),
    list(Q("OR"), Q("SETF-CELL"), Q("NULL")) );

  // intern-setf-cell (function (symbol) setf-cell))
  add_ftype(Q("INTERN-SETF-CELL"),
    list(Q("SYMBOL")),
    Q("SETF-CELL") );

  // find-value-cell (function (symbol) (or value-cell tlv-record null)))
  add_ftype(Q("FIND-VALUE-CELL"),
    list(Q("SYMBOL")),
    list(Q("OR"), Q("VALUE-CELL"), Q("TLV-RECORD"), Q("NULL")) );

  // intern-value-cell (function (symbol) (or value-cell tlv-record)))
  add_ftype(Q("INTERN-VALUE-CELL"),
    list(Q("SYMBOL")),
    list(Q("OR"), Q("VALUE-CELL"), Q("TLV-RECORD")) );

  // required (function nil t))
  add_ftype(Q("REQUIRED"),
    nil,
    t );

  // unspecified (function nil null))
  add_ftype(Q("UNSPECIFIED"),
    nil,
    Q("NULL") );

  // function-name (function (function) (or symbol list)))
  add_ftype(Q("FUNCTION-NAME"),
    list(Q("FUNCTION")),
    list(Q("OR"), Q("SYMBOL"), Q("LIST")) );

  // (setf function-name) (function ((or symbol list) function) (or symbol list)))
  add_ftype(intern_setf_cell(Q("FUNCTION-NAME")),
    list(list(Q("OR"), Q("SYMBOL"), Q("LIST")), Q("FUNCTION")),
    list(Q("OR"), Q("SYMBOL"), Q("LIST")) );

  // %ref (function (symbol symbol t) t))
  add_ftype(Q("%REF"),
    list(Q("SYMBOL"), Q("SYMBOL"), t),
    t );

  // (setf %ref) (function (t symbol symbol t) t))
  add_ftype(intern_setf_cell(Q("%REF")),
    list(t, Q("SYMBOL"), Q("SYMBOL"), t),
    t );

  // tlv (function (fixnum) t))
  add_ftype(Q("TLV"),
    list(Q("FIXNUM")),
    t );

  // (setf tlv) (function (t fixnum) t))
  add_ftype(intern_setf_cell(Q("TLV")),
    list(t, Q("FIXNUM")),
    t );

  // make-class-description (function (class list) class-description))
  add_ftype(Q("MAKE-CLASS-DESCRIPTION"),
    list(Q("CLASS"), Q("LIST")),
    Q("CLASS-DESCRIPTION") );

  // .allocate-instance (function (class-description) standard-object))
  add_ftype(Q(".ALLOCATE-INSTANCE"),
    list(Q("CLASS-DESCRIPTION")),
    Q("STANDARD-OBJECT") );

  // .allocate-funcallable-instance (function (class-description) funcallable-standard-object))
  add_ftype(Q(".ALLOCATE-FUNCALLABLE-INSTANCE"),
    list(Q("CLASS-DESCRIPTION")),
    Q("FUNCALLABLE-STANDARD-OBJECT") );

  // .allocate-structure (function (class-description) structure-object))
  add_ftype(Q(".ALLOCATE-STRUCTURE"),
    list(Q("CLASS-DESCRIPTION")),
    Q("STRUCTURE-OBJECT") );

  // .allocate-record (function (class-description) t))
  add_ftype(Q(".ALLOCATE-RECORD"),
    list(Q("CLASS-DESCRIPTION")),
    t );

  // !elt (function (type-specifier t sequence-index) t))
  add_ftype(Q("!ELT"),
    list(Q("TYPE-SPECIFIER"), t, Q("SEQUENCE-INDEX")),
    t );

  // (setf !elt) (function (t type-specifier t sequence-index) t))
  add_ftype(intern_setf_cell(Q("!ELT")),
    list(t, Q("TYPE-SPECIFIER"), t, Q("SEQUENCE-INDEX")),
    t );

  // !elt* (function (type-specifier t sequence-index) t))
  add_ftype(Q("!ELT*"),
    list(Q("TYPE-SPECIFIER"), t, Q("SEQUENCE-INDEX")),
    t );

  // (setf !elt*) (function (t type-specifier t sequence-index) t))
  add_ftype(intern_setf_cell(Q("!ELT*")),
    list(t, Q("TYPE-SPECIFIER"), t, Q("SEQUENCE-INDEX")),
    t );

  // .box-int (function (t) (signed-byte 32)))
  add_ftype(Q(".BOX-INT"),
    list(t),
    list(Q("SIGNED-BYTE"), Fixnum::Encode(32)) );

  // .box-uint (function (t) (unsigned-byte 32)))
  add_ftype(Q(".BOX-UINT"),
    list(t),
    list(Q("UNSIGNED-BYTE"), Fixnum::Encode(32)) );

  // .unbox-int (function ((signed-byte 32)) int))
  add_ftype(Q(".UNBOX-INT"),
    list(list(Q("SIGNED-BYTE"), Fixnum::Encode(32))),
    Q("INT") );

  // lock-latch (function (latch (member :exclusive :shared)) latch))
  add_ftype(Q("LOCK-LATCH"),
    list(Q("LATCH"), list(Q("MEMBER"), Q(":EXCLUSIVE"), Q(":SHARED"))),
    Q("LATCH") );

  // lock-mutex (function (latch) latch))
  add_ftype(Q("LOCK-MUTEX"),
    list(Q("LATCH")),
    Q("LATCH") );

  // make-latch (function (symbol) latch))
  add_ftype(Q("MAKE-LATCH"),
    list(Q("SYMBOL")),
    Q("LATCH") );

  // make-mutex (function (symbol) mutex))
  add_ftype(Q("MAKE-MUTEX"),
    list(Q("SYMBOL")),
    Q("MUTEX") );

  // unlock-latch (function (latch) latch))
  add_ftype(Q("UNLOCK-LATCH"),
    list(Q("LATCH")),
    Q("LATCH") );

  // unlock-mutex (function (mutext) mutex))
  add_ftype(Q("UNLOCK-MUTEX"),
    list(Q("MUTEXT")),
    Q("MUTEX") );


// EOF
