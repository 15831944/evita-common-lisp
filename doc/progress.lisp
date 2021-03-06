;;; How do we check setf macro?
;;;  5  apply, the, values
;;; 12  ldb, maek-field
;;; 14  getf
;;; 17  subseq

(defun show-progress ()
  (let ((fns 0)
        (macros 0)
        (setfs 0)
        (vars 0)
        (noftypes    '())
        (cl-fns      636)
        (cl-macros    94)
        (cl-setfs     65)
        (cl-vars     116) )
  (do-external-symbols (x :cl)
    (cond
      ((special-operator-p x))
      ((macro-function x) (incf macros))
      ((fboundp x)
        (unless (fun-info x 'ftype) (push x noftypes))
        (incf fns)) )
     (when (boundp x) (incf vars))
     (let ((fname `(setf ,x)))
       (when (fboundp fname)
         (unless (fun-info fname 'ftype) (push fname noftypes))
         (incf setfs) ) ) )
  (loop
    for (kind progress expect) in
            `(("Function" ,fns ,cl-fns)
              ("Macro"    ,macros ,cl-macros)
              ("Setf"     ,setfs  ,cl-setfs)
              ("Variable" ,vars   ,cl-vars) ) do
      (format t "~A ~D/~D ~S%~%" kind progress expect
        (/ (float progress) expect) ))
  noftypes ) )

(defun list-cl ()
  (let ((fs '())
        (ms '())
        (ss '())
        (vs '()) )
    (do-external-symbols (x :cl)
      (cond
        ((special-operator-p x))
        ((macro-function x) (push x ms))
        ((fboundp x) (push x fs)) )
      (when (boundp x) (push x vs))
      (when (fboundp `(setf ,x)) (push x ss)) )
    (values (sort vs #'string<)
            (sort fs #'string<)
            (sort ss #'string<)
            (sort ms #'string<) ) ) )


;;;; count-cl => 116, 636, 65, 94
(defun count-cl ()
  (multiple-value-bind (vs fs ss ms) (list-cl)
    (values (length vs) (length fs) (length ss) (length ms)) ) )

(defvar *cl-vars* '(
    *
    **
    ***
    *BREAK-ON-SIGNALS*
    *COMPILE-FILE-PATHNAME*
    *COMPILE-FILE-TRUENAME*
    *COMPILE-PRINT*
    *COMPILE-VERBOSE*
    *DEBUG-IO*
    *DEBUGGER-HOOK*
    *DEFAULT-PATHNAME-DEFAULTS*
    *ERROR-OUTPUT*
    *FEATURES*
    *GENSYM-COUNTER*
    *LOAD-PATHNAME*
    *LOAD-PRINT*
    *LOAD-TRUENAME*
    *LOAD-VERBOSE*
    *MACROEXPAND-HOOK*
    *MODULES*
    *PACKAGE*
    *PRINT-ARRAY*
    *PRINT-BASE*
    *PRINT-CASE*
    *PRINT-CIRCLE*
    *PRINT-ESCAPE*
    *PRINT-GENSYM*
    *PRINT-LENGTH*
    *PRINT-LEVEL*
    *PRINT-LINES*
    *PRINT-MISER-WIDTH*
    *PRINT-PPRINT-DISPATCH*
    *PRINT-PRETTY*
    *PRINT-RADIX*
    *PRINT-READABLY*
    *PRINT-RIGHT-MARGIN*
    *QUERY-IO*
    *RANDOM-STATE*
    *READ-BASE*
    *READ-DEFAULT-FLOAT-FORMAT*
    *READ-EVAL*
    *READ-SUPPRESS*
    *READTABLE*
    *STANDARD-INPUT*
    *STANDARD-OUTPUT*
    *TERMINAL-IO*
    *TRACE-OUTPUT*
    +
    ++
    +++
    -
    /
    //
    ///
    ARRAY-DIMENSION-LIMIT
    ARRAY-RANK-LIMIT
    ARRAY-TOTAL-SIZE-LIMIT
    BOOLE-1
    BOOLE-2
    BOOLE-AND
    BOOLE-ANDC1
    BOOLE-ANDC2
    BOOLE-C1
    BOOLE-C2
    BOOLE-CLR
    BOOLE-EQV
    BOOLE-IOR
    BOOLE-NAND
    BOOLE-NOR
    BOOLE-ORC1
    BOOLE-ORC2
    BOOLE-SET
    BOOLE-XOR
    CALL-ARGUMENTS-LIMIT
    CHAR-CODE-LIMIT
    DOUBLE-FLOAT-EPSILON
    DOUBLE-FLOAT-NEGATIVE-EPSILON
    INTERNAL-TIME-UNITS-PER-SECOND
    LAMBDA-LIST-KEYWORDS
    LAMBDA-PARAMETERS-LIMIT
    LEAST-NEGATIVE-DOUBLE-FLOAT
    LEAST-NEGATIVE-LONG-FLOAT
    LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT
    LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
    LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT
    LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT
    LEAST-NEGATIVE-SHORT-FLOAT
    LEAST-NEGATIVE-SINGLE-FLOAT
    LEAST-POSITIVE-DOUBLE-FLOAT
    LEAST-POSITIVE-LONG-FLOAT
    LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT
    LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
    LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT
    LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT
    LEAST-POSITIVE-SHORT-FLOAT
    LEAST-POSITIVE-SINGLE-FLOAT
    LONG-FLOAT-EPSILON
    LONG-FLOAT-NEGATIVE-EPSILON
    MOST-NEGATIVE-DOUBLE-FLOAT
    MOST-NEGATIVE-FIXNUM
    MOST-NEGATIVE-LONG-FLOAT
    MOST-NEGATIVE-SHORT-FLOAT
    MOST-NEGATIVE-SINGLE-FLOAT
    MOST-POSITIVE-DOUBLE-FLOAT
    MOST-POSITIVE-FIXNUM
    MOST-POSITIVE-LONG-FLOAT
    MOST-POSITIVE-SHORT-FLOAT
    MOST-POSITIVE-SINGLE-FLOAT
    MULTIPLE-VALUES-LIMIT
    NIL
    PI
    SHORT-FLOAT-EPSILON
    SHORT-FLOAT-NEGATIVE-EPSILON
    SINGLE-FLOAT-EPSILON
    SINGLE-FLOAT-NEGATIVE-EPSILON
    T ) )

(defvar *cl-fns* '(
    *
    +
    -
    /
    /=
    1+
    1-
    <
    <=
    =
    >
    >=
    ABORT
    ABS
    ACONS
    ACOS
    ACOSH
    ADD-METHOD
    ADJOIN
    ADJUST-ARRAY
    ADJUSTABLE-ARRAY-P
    ALLOCATE-INSTANCE
    ALPHA-CHAR-P
    ALPHANUMERICP
    APPEND
    APPLY
    APROPOS
    APROPOS-LIST
    AREF
    ARITHMETIC-ERROR-OPERANDS
    ARITHMETIC-ERROR-OPERATION
    ARRAY-DIMENSION
    ARRAY-DIMENSIONS
    ARRAY-DISPLACEMENT
    ARRAY-ELEMENT-TYPE
    ARRAY-HAS-FILL-POINTER-P
    ARRAY-IN-BOUNDS-P
    ARRAY-RANK
    ARRAY-ROW-MAJOR-INDEX
    ARRAY-TOTAL-SIZE
    ARRAYP
    ASH
    ASIN
    ASINH
    ASSOC
    ASSOC-IF
    ASSOC-IF-NOT
    ATAN
    ATANH
    ATOM
    BIT
    BIT-AND
    BIT-ANDC1
    BIT-ANDC2
    BIT-EQV
    BIT-IOR
    BIT-NAND
    BIT-NOR
    BIT-NOT
    BIT-ORC1
    BIT-ORC2
    BIT-VECTOR-P
    BIT-XOR
    BOOLE
    BOTH-CASE-P
    BOUNDP
    BREAK
    BROADCAST-STREAM-STREAMS
    BUTLAST
    BYTE
    BYTE-POSITION
    BYTE-SIZE
    CAAAAR
    CAAADR
    CAAAR
    CAADAR
    CAADDR
    CAADR
    CAAR
    CADAAR
    CADADR
    CADAR
    CADDAR
    CADDDR
    CADDR
    CADR
    CAR
    CDAAAR
    CDAADR
    CDAAR
    CDADAR
    CDADDR
    CDADR
    CDAR
    CDDAAR
    CDDADR
    CDDAR
    CDDDAR
    CDDDDR
    CDDDR
    CDDR
    CDR
    CEILING
    CELL-ERROR-NAME
    CERROR
    CHANGE-CLASS
    CHAR
    CHAR-CODE
    CHAR-DOWNCASE
    CHAR-EQUAL
    CHAR-GREATERP
    CHAR-INT
    CHAR-LESSP
    CHAR-NAME
    CHAR-NOT-EQUAL
    CHAR-NOT-GREATERP
    CHAR-NOT-LESSP
    CHAR-UPCASE
    CHAR/=
    CHAR<
    CHAR<=
    CHAR=
    CHAR>
    CHAR>=
    CHARACTER
    CHARACTERP
    CIS
    CLASS-NAME
    CLASS-OF
    CLEAR-INPUT
    CLEAR-OUTPUT
    CLOSE
    CLRHASH
    CODE-CHAR
    COERCE
    COMPILE
    COMPILE-FILE
    COMPILE-FILE-PATHNAME
    COMPILED-FUNCTION-P
    COMPILER-MACRO-FUNCTION
    COMPLEMENT
    COMPLEX
    COMPLEXP
    COMPUTE-APPLICABLE-METHODS
    COMPUTE-RESTARTS
    CONCATENATE
    CONCATENATED-STREAM-STREAMS
    CONJUGATE
    CONS
    CONSP
    CONSTANTLY
    CONSTANTP
    CONTINUE
    COPY-ALIST
    COPY-LIST
    COPY-PPRINT-DISPATCH
    COPY-READTABLE
    COPY-SEQ
    COPY-STRUCTURE
    COPY-SYMBOL
    COPY-TREE
    COS
    COSH
    COUNT
    COUNT-IF
    COUNT-IF-NOT
    DECODE-FLOAT
    DECODE-UNIVERSAL-TIME
    DELETE
    DELETE-DUPLICATES
    DELETE-FILE
    DELETE-IF
    DELETE-IF-NOT
    DELETE-PACKAGE
    DENOMINATOR
    DEPOSIT-FIELD
    DESCRIBE
    DESCRIBE-OBJECT
    DIGIT-CHAR
    DIGIT-CHAR-P
    DIRECTORY
    DIRECTORY-NAMESTRING
    DISASSEMBLE
    DOCUMENTATION
    DPB
    DRIBBLE
    ECHO-STREAM-INPUT-STREAM
    ECHO-STREAM-OUTPUT-STREAM
    ED
    EIGHTH
    ELT
    ENCODE-UNIVERSAL-TIME
    ENDP
    ENOUGH-NAMESTRING
    ENSURE-DIRECTORIES-EXIST
    ENSURE-GENERIC-FUNCTION
    EQ
    EQL
    EQUAL
    EQUALP
    ERROR
    EVAL
    EVENP
    EVERY
    EXP
    EXPORT
    EXPT
    FBOUNDP
    FCEILING
    FDEFINITION
    FFLOOR
    FIFTH
    FILE-AUTHOR
    FILE-ERROR-PATHNAME
    FILE-LENGTH
    FILE-NAMESTRING
    FILE-POSITION
    FILE-STRING-LENGTH
    FILE-WRITE-DATE
    FILL
    FILL-POINTER
    FIND
    FIND-ALL-SYMBOLS
    FIND-CLASS
    FIND-IF
    FIND-IF-NOT
    FIND-METHOD
    FIND-PACKAGE
    FIND-RESTART
    FIND-SYMBOL
    FINISH-OUTPUT
    FIRST
    FLOAT
    FLOAT-DIGITS
    FLOAT-PRECISION
    FLOAT-RADIX
    FLOAT-SIGN
    FLOATP
    FLOOR
    FMAKUNBOUND
    FORCE-OUTPUT
    FORMAT
    FOURTH
    FRESH-LINE
    FROUND
    FTRUNCATE
    FUNCALL
    FUNCTION-KEYWORDS
    FUNCTION-LAMBDA-EXPRESSION
    FUNCTIONP
    GCD
    GENSYM
    GENTEMP
    GET
    GET-DECODED-TIME
    GET-DISPATCH-MACRO-CHARACTER
    GET-INTERNAL-REAL-TIME
    GET-INTERNAL-RUN-TIME
    GET-MACRO-CHARACTER
    GET-OUTPUT-STREAM-STRING
    GET-PROPERTIES
    GET-SETF-EXPANSION
    GET-UNIVERSAL-TIME
    GETF
    GETHASH
    GRAPHIC-CHAR-P
    HASH-TABLE-COUNT
    HASH-TABLE-P
    HASH-TABLE-REHASH-SIZE
    HASH-TABLE-REHASH-THRESHOLD
    HASH-TABLE-SIZE
    HASH-TABLE-TEST
    HOST-NAMESTRING
    IDENTITY
    IMAGPART
    IMPORT
    INITIALIZE-INSTANCE
    INPUT-STREAM-P
    INSPECT
    INTEGER-DECODE-FLOAT
    INTEGER-LENGTH
    INTEGERP
    INTERACTIVE-STREAM-P
    INTERN
    INTERSECTION
    INVALID-METHOD-ERROR
    INVOKE-DEBUGGER
    INVOKE-RESTART
    INVOKE-RESTART-INTERACTIVELY
    ISQRT
    KEYWORDP
    LAST
    LCM
    LDB
    LDB-TEST
    LDIFF
    LENGTH
    LISP-IMPLEMENTATION-TYPE
    LISP-IMPLEMENTATION-VERSION
    LIST
    LIST*
    LIST-ALL-PACKAGES
    LIST-LENGTH
    LISTEN
    LISTP
    LOAD
    LOAD-LOGICAL-PATHNAME-TRANSLATIONS
    LOG
    LOGAND
    LOGANDC1
    LOGANDC2
    LOGBITP
    LOGCOUNT
    LOGEQV
    LOGICAL-PATHNAME
    LOGICAL-PATHNAME-TRANSLATIONS
    LOGIOR
    LOGNAND
    LOGNOR
    LOGNOT
    LOGORC1
    LOGORC2
    LOGTEST
    LOGXOR
    LONG-SITE-NAME
    LOWER-CASE-P
    MACHINE-INSTANCE
    MACHINE-TYPE
    MACHINE-VERSION
    MACRO-FUNCTION
    MACROEXPAND
    MACROEXPAND-1
    MAKE-ARRAY
    MAKE-BROADCAST-STREAM
    MAKE-CONCATENATED-STREAM
    MAKE-CONDITION
    MAKE-DISPATCH-MACRO-CHARACTER
    MAKE-ECHO-STREAM
    MAKE-HASH-TABLE
    MAKE-INSTANCE
    MAKE-INSTANCES-OBSOLETE
    MAKE-LIST
    MAKE-LOAD-FORM
    MAKE-LOAD-FORM-SAVING-SLOTS
    MAKE-PACKAGE
    MAKE-PATHNAME
    MAKE-RANDOM-STATE
    MAKE-SEQUENCE
    MAKE-STRING
    MAKE-STRING-INPUT-STREAM
    MAKE-STRING-OUTPUT-STREAM
    MAKE-SYMBOL
    MAKE-SYNONYM-STREAM
    MAKE-TWO-WAY-STREAM
    MAKUNBOUND
    MAP
    MAP-INTO
    MAPC
    MAPCAN
    MAPCAR
    MAPCON
    MAPHASH
    MAPL
    MAPLIST
    MASK-FIELD
    MAX
    MEMBER
    MEMBER-IF
    MEMBER-IF-NOT
    MERGE
    MERGE-PATHNAMES
    METHOD-COMBINATION-ERROR
    METHOD-QUALIFIERS
    MIN
    MINUSP
    MISMATCH
    MOD
    MUFFLE-WARNING
    NAME-CHAR
    NAMESTRING
    NBUTLAST
    NCONC
    NINTERSECTION
    NINTH
    NO-APPLICABLE-METHOD
    NO-NEXT-METHOD
    NOT
    NOTANY
    NOTEVERY
    NRECONC
    NREVERSE
    NSET-DIFFERENCE
    NSET-EXCLUSIVE-OR
    NSTRING-CAPITALIZE
    NSTRING-DOWNCASE
    NSTRING-UPCASE
    NSUBLIS
    NSUBST
    NSUBST-IF
    NSUBST-IF-NOT
    NSUBSTITUTE
    NSUBSTITUTE-IF
    NSUBSTITUTE-IF-NOT
    NTH
    NTHCDR
    NULL
    NUMBERP
    NUMERATOR
    NUNION
    ODDP
    OPEN
    OPEN-STREAM-P
    OUTPUT-STREAM-P
    PACKAGE-ERROR-PACKAGE
    PACKAGE-NAME
    PACKAGE-NICKNAMES
    PACKAGE-SHADOWING-SYMBOLS
    PACKAGE-USE-LIST
    PACKAGE-USED-BY-LIST
    PACKAGEP
    PAIRLIS
    PARSE-INTEGER
    PARSE-NAMESTRING
    PATHNAME
    PATHNAME-DEVICE
    PATHNAME-DIRECTORY
    PATHNAME-HOST
    PATHNAME-MATCH-P
    PATHNAME-NAME
    PATHNAME-TYPE
    PATHNAME-VERSION
    PATHNAMEP
    PEEK-CHAR
    PHASE
    PLUSP
    POSITION
    POSITION-IF
    POSITION-IF-NOT
    PPRINT
    PPRINT-DISPATCH
    PPRINT-FILL
    PPRINT-INDENT
    PPRINT-LINEAR
    PPRINT-NEWLINE
    PPRINT-TAB
    PPRINT-TABULAR
    PRIN1
    PRIN1-TO-STRING
    PRINC
    PRINC-TO-STRING
    PRINT
    PRINT-NOT-READABLE-OBJECT
    PRINT-OBJECT
    PROBE-FILE
    PROCLAIM
    PROVIDE
    RANDOM
    RANDOM-STATE-P
    RASSOC
    RASSOC-IF
    RASSOC-IF-NOT
    RATIONAL
    RATIONALIZE
    RATIONALP
    READ
    READ-BYTE
    READ-CHAR
    READ-CHAR-NO-HANG
    READ-DELIMITED-LIST
    READ-FROM-STRING
    READ-LINE
    READ-PRESERVING-WHITESPACE
    READ-SEQUENCE
    READTABLE-CASE
    READTABLEP
    REALP
    REALPART
    REDUCE
    REINITIALIZE-INSTANCE
    REM
    REMHASH
    REMOVE
    REMOVE-DUPLICATES
    REMOVE-IF
    REMOVE-IF-NOT
    REMOVE-METHOD
    REMPROP
    RENAME-FILE
    RENAME-PACKAGE
    REPLACE
    REQUIRE
    REST
    RESTART-NAME
    REVAPPEND
    REVERSE
    ROOM
    ROUND
    ROW-MAJOR-AREF
    RPLACA
    RPLACD
    SBIT
    SCALE-FLOAT
    SCHAR
    SEARCH
    SECOND
    SET
    SET-DIFFERENCE
    SET-DISPATCH-MACRO-CHARACTER
    SET-EXCLUSIVE-OR
    SET-MACRO-CHARACTER
    SET-PPRINT-DISPATCH
    SET-SYNTAX-FROM-CHAR
    SEVENTH
    SHADOW
    SHADOWING-IMPORT
    SHARED-INITIALIZE
    SHORT-SITE-NAME
    SIGNAL
    SIGNUM
    SIMPLE-BIT-VECTOR-P
    SIMPLE-CONDITION-FORMAT-ARGUMENTS
    SIMPLE-CONDITION-FORMAT-CONTROL
    SIMPLE-STRING-P
    SIMPLE-VECTOR-P
    SIN
    SINH
    SIXTH
    SLEEP
    SLOT-BOUNDP
    SLOT-EXISTS-P
    SLOT-MAKUNBOUND
    SLOT-MISSING
    SLOT-UNBOUND
    SLOT-VALUE
    SOFTWARE-TYPE
    SOFTWARE-VERSION
    SOME
    SORT
    SPECIAL-OPERATOR-P
    SQRT
    STABLE-SORT
    STANDARD-CHAR-P
    STORE-VALUE
    STREAM-ELEMENT-TYPE
    STREAM-ERROR-STREAM
    STREAM-EXTERNAL-FORMAT
    STREAMP
    STRING
    STRING-CAPITALIZE
    STRING-DOWNCASE
    STRING-EQUAL
    STRING-GREATERP
    STRING-LEFT-TRIM
    STRING-LESSP
    STRING-NOT-EQUAL
    STRING-NOT-GREATERP
    STRING-NOT-LESSP
    STRING-RIGHT-TRIM
    STRING-TRIM
    STRING-UPCASE
    STRING/=
    STRING<
    STRING<=
    STRING=
    STRING>
    STRING>=
    STRINGP
    SUBLIS
    SUBSEQ
    SUBSETP
    SUBST
    SUBST-IF
    SUBST-IF-NOT
    SUBSTITUTE
    SUBSTITUTE-IF
    SUBSTITUTE-IF-NOT
    SUBTYPEP
    SVREF
    SXHASH
    SYMBOL-FUNCTION
    SYMBOL-NAME
    SYMBOL-PACKAGE
    SYMBOL-PLIST
    SYMBOL-VALUE
    SYMBOLP
    SYNONYM-STREAM-SYMBOL
    TAILP
    TAN
    TANH
    TENTH
    TERPRI
    THIRD
    TRANSLATE-LOGICAL-PATHNAME
    TRANSLATE-PATHNAME
    TREE-EQUAL
    TRUENAME
    TRUNCATE
    TWO-WAY-STREAM-INPUT-STREAM
    TWO-WAY-STREAM-OUTPUT-STREAM
    TYPE-ERROR-DATUM
    TYPE-ERROR-EXPECTED-TYPE
    TYPE-OF
    TYPEP
    UNBOUND-SLOT-INSTANCE
    UNEXPORT
    UNINTERN
    UNION
    UNREAD-CHAR
    UNUSE-PACKAGE
    UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
    UPDATE-INSTANCE-FOR-REDEFINED-CLASS
    UPGRADED-ARRAY-ELEMENT-TYPE
    UPGRADED-COMPLEX-PART-TYPE
    UPPER-CASE-P
    USE-PACKAGE
    USE-VALUE
    USER-HOMEDIR-PATHNAME
    VALUES
    VALUES-LIST
    VECTOR
    VECTOR-POP
    VECTOR-PUSH
    VECTOR-PUSH-EXTEND
    VECTORP
    WARN
    WILD-PATHNAME-P
    WRITE
    WRITE-BYTE
    WRITE-CHAR
    WRITE-LINE
    WRITE-SEQUENCE
    WRITE-STRING
    WRITE-TO-STRING
    Y-OR-N-P
    YES-OR-NO-P
    ZEROP ) )

(defvar *cl-setfs* '(
       AREF
       BIT
       CAAAAR
       CAAADR
       CAAAR
       CAADAR
       CAADDR
       CAADR
       CAAR
       CADAAR
       CADADR
       CADAR
       CADDAR
       CADDDR
       CADDR
       CADR
       CAR
       CDAAAR
       CDAADR
       CDAAR
       CDADAR
       CDADDR
       CDADR
       CDAR
       CDDAAR
       CDDADR
       CDDAR
       CDDDAR
       CDDDDR
       CDDDR
       CDDR
       CDR
       CHAR
       CLASS-NAME
       COMPILER-MACRO-FUNCTION
       DOCUMENTATION
       EIGHTH
       ELT
       FDEFINITION
       FIFTH
       FILL-POINTER
       FIND-CLASS
       FIRST
       FOURTH
       GET
       GETHASH
       LOGICAL-PATHNAME-TRANSLATIONS
       MACRO-FUNCTION
       NINTH
       NTH
       READTABLE-CASE
       REST
       ROW-MAJOR-AREF
       SBIT
       SCHAR
       SECOND
       SEVENTH
       SIXTH
       SLOT-VALUE
       SVREF
       SYMBOL-FUNCTION
       SYMBOL-PLIST
       SYMBOL-VALUE
       TENTH
       THIRD ) )

(defvar *cl-macros* '(
      AND
      ASSERT
      CALL-METHOD
      CALL-NEXT-METHOD
      CASE
      CCASE
      CHECK-TYPE
      COND
      CTYPECASE
      DECF
      DECLAIM
      DEFCLASS
      DEFCONSTANT
      DEFGENERIC
      DEFINE-COMPILER-MACRO
      DEFINE-CONDITION
      DEFINE-METHOD-COMBINATION
      DEFINE-MODIFY-MACRO
      DEFINE-SETF-EXPANDER
      DEFINE-SYMBOL-MACRO
      DEFMACRO
      DEFMETHOD
      DEFPACKAGE
      DEFPARAMETER
      DEFSETF
      DEFSTRUCT
      DEFTYPE
      DEFUN
      DEFVAR
      DESTRUCTURING-BIND
      DO
      DO*
      DO-ALL-SYMBOLS
      DO-EXTERNAL-SYMBOLS
      DO-SYMBOLS
      DOLIST
      DOTIMES
      ECASE
      ETYPECASE
      FORMATTER
      HANDLER-BIND
      HANDLER-CASE
      IGNORE-ERRORS
      IN-PACKAGE
      INCF
      LAMBDA
      LOOP
      LOOP-FINISH
      MAKE-METHOD
      MULTIPLE-VALUE-BIND
      MULTIPLE-VALUE-LIST
      MULTIPLE-VALUE-SETQ
      NEXT-METHOD-P
      NTH-VALUE
      OR
      POP
      PPRINT-EXIT-IF-LIST-EXHAUSTED
      PPRINT-LOGICAL-BLOCK
      PPRINT-POP
      PRINT-UNREADABLE-OBJECT
      PROG
      PROG*
      PROG1
      PROG2
      PSETF
      PSETQ
      PUSH
      PUSHNEW
      REMF
      RESTART-BIND
      RESTART-CASE
      RETURN
      ROTATEF
      SETF
      SHIFTF
      STEP
      TIME
      TRACE
      TYPECASE
      UNLESS
      UNTRACE
      WHEN
      WITH-ACCESSORS
      WITH-COMPILATION-UNIT
      WITH-CONDITION-RESTARTS
      WITH-HASH-TABLE-ITERATOR
      WITH-INPUT-FROM-STRING
      WITH-OPEN-FILE
      WITH-OPEN-STREAM
      WITH-OUTPUT-TO-STRING
      WITH-PACKAGE-ITERATOR
      WITH-SIMPLE-RESTART
      WITH-SLOTS
      WITH-STANDARD-IO-SYNTAX ) )

(defun show-remains ()
  (multiple-value-bind (vs fs ss ms) (list-cl)
    (values
        (set-exclusive-or fs *cl-fns*)
        (set-exclusive-or vs *cl-vars*)
        (set-exclusive-or ss *cl-setfs*)
        (set-exclusive-or ms *cl-macros*) ) ) )




