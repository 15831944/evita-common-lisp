
    // add-method (gf method)
    gf = install_gf(Q("ADD-METHOD"), 2, 0,
                        nil, list(Q("GF"), Qmethod) );

    // type-error-datum (object)
    gf = install_gf(Q("TYPE-ERROR-DATUM"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // method-qualifiers (object)
    gf = install_gf(Q("METHOD-QUALIFIERS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // echo-stream-input-stream (object)
    gf = install_gf(Q("ECHO-STREAM-INPUT-STREAM"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // shared-initialize (instance slot-names &rest initargs)
    gf = install_gf(Q("SHARED-INITIALIZE"), 2, 0,
                        t, cons(Q("INSTANCE"), list(Q("SLOT-NAMES"), Q("&REST"), Q("INITARGS"))) );

    // allocate-instance (class &rest initargs)
    gf = install_gf(Q("ALLOCATE-INSTANCE"), 1, 0,
                        t, list(Qclass, Q("&REST"), Q("INITARGS")) );

    // cell-error-name (object)
    gf = install_gf(Q("CELL-ERROR-NAME"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // update-instance-for-different-class (previous current &rest initargs)
    gf = install_gf(Q("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS"), 2, 0,
                        t, cons(Q("PREVIOUS"), list(Q("CURRENT"), Q("&REST"), Q("INITARGS"))) );

    // stream-error-stream (object)
    gf = install_gf(Q("STREAM-ERROR-STREAM"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // slot-unbound (class object slot-name)
    gf = install_gf(Q("SLOT-UNBOUND"), 3, 0,
                        nil, list(Qclass, Q("OBJECT"), Q("SLOT-NAME")) );

    // compute-applicable-methods (gf args)
    gf = install_gf(Q("COMPUTE-APPLICABLE-METHODS"), 2, 0,
                        nil, list(Q("GF"), Q("ARGS")) );

    // stream-element-type (stream)
    gf = install_gf(Q("STREAM-ELEMENT-TYPE"), 1, 0,
                        nil, list(Qstream) );

    // make-load-form (object &optional (env))
    gf = install_gf(Q("MAKE-LOAD-FORM"), 1, 1,
                        nil, list(Q("OBJECT"), Q("&OPTIONAL"), list(Q("ENV"))) );

    // documentation (x doc-type)
    gf = install_gf(Q("DOCUMENTATION"), 2, 0,
                        nil, list(Q("X"), Q("DOC-TYPE")) );

    // (setf documentation) (new-value x doc-type)
    gf = install_gf(list(Q("SETF"), Q("DOCUMENTATION")), 3, 0,
                        nil, list(Q("NEW-VALUE"), Q("X"), Q("DOC-TYPE")) );

    // interactive-stream-p (stream)
    gf = install_gf(Q("INTERACTIVE-STREAM-P"), 1, 0,
                        nil, list(Qstream) );

    // remove-method (gf mt)
    gf = install_gf(Q("REMOVE-METHOD"), 2, 0,
                        nil, list(Q("GF"), Q("MT")) );

    // make-instances-obsolete (class)
    gf = install_gf(Q("MAKE-INSTANCES-OBSOLETE"), 1, 0,
                        nil, list(Qclass) );

    // two-way-stream-input-stream (object)
    gf = install_gf(Q("TWO-WAY-STREAM-INPUT-STREAM"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // arithmetic-error-operands (object)
    gf = install_gf(Q("ARITHMETIC-ERROR-OPERANDS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // function-keywords (method)
    gf = install_gf(Q("FUNCTION-KEYWORDS"), 1, 0,
                        nil, list(Qmethod) );

    // update-instance-for-redefined-class (instance added-slots discarded-slots plist &rest initargs)
    gf = install_gf(Q("UPDATE-INSTANCE-FOR-REDEFINED-CLASS"), 4, 0,
                        t, cons(Q("INSTANCE"), cons(Q("ADDED-SLOTS"), cons(Q("DISCARDED-SLOTS"), list(Q("PLIST"), Q("&REST"), Q("INITARGS"))))) );

    // no-next-method (gf method &rest args)
    gf = install_gf(Q("NO-NEXT-METHOD"), 2, 0,
                        t, cons(Q("GF"), list(Qmethod, Q("&REST"), Q("ARGS"))) );

    // type-error-expected-type (object)
    gf = install_gf(Q("TYPE-ERROR-EXPECTED-TYPE"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // find-method (gf qualifiers specializers &optional (error-p t))
    gf = install_gf(Q("FIND-METHOD"), 3, 1,
                        nil, cons(Q("GF"), cons(Q("QUALIFIERS"), list(Q("SPECIALIZERS"), Q("&OPTIONAL"), list(Q("ERROR-P"), t)))) );

    // package-error-package (object)
    gf = install_gf(Q("PACKAGE-ERROR-PACKAGE"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // reinitialize-instance (instance &rest initargs)
    gf = install_gf(Q("REINITIALIZE-INSTANCE"), 1, 0,
                        t, list(Q("INSTANCE"), Q("&REST"), Q("INITARGS")) );

    // simple-condition-format-control (object)
    gf = install_gf(Q("SIMPLE-CONDITION-FORMAT-CONTROL"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // file-error-pathname (object)
    gf = install_gf(Q("FILE-ERROR-PATHNAME"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // simple-condition-format-arguments (object)
    gf = install_gf(Q("SIMPLE-CONDITION-FORMAT-ARGUMENTS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // synonym-stream-symbol (object)
    gf = install_gf(Q("SYNONYM-STREAM-SYMBOL"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // close (stream &rest args)
    gf = install_gf(Q("CLOSE"), 1, 0,
                        t, list(Qstream, Q("&REST"), Q("ARGS")) );

    // concatenated-stream-streams (object)
    gf = install_gf(Q("CONCATENATED-STREAM-STREAMS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // no-applicable-method (gf &rest args)
    gf = install_gf(Q("NO-APPLICABLE-METHOD"), 1, 0,
                        t, list(Q("GF"), Q("&REST"), Q("ARGS")) );

    // initialize-instance (instance &rest initargs)
    gf = install_gf(Q("INITIALIZE-INSTANCE"), 1, 0,
                        t, list(Q("INSTANCE"), Q("&REST"), Q("INITARGS")) );

    // print-object (c s)
    gf = install_gf(Q("PRINT-OBJECT"), 2, 0,
                        nil, list(Q("C"), Q("S")) );

    // slot-missing (class object slot-name operation &optional (new-value))
    gf = install_gf(Q("SLOT-MISSING"), 4, 1,
                        nil, cons(Qclass, cons(Q("OBJECT"), cons(Q("SLOT-NAME"), list(Q("OPERATION"), Q("&OPTIONAL"), list(Q("NEW-VALUE")))))) );

    // make-instance (class &rest initargs)
    gf = install_gf(Q("MAKE-INSTANCE"), 1, 0,
                        t, list(Qclass, Q("&REST"), Q("INITARGS")) );

    // describe-object (object stream)
    gf = install_gf(Q("DESCRIBE-OBJECT"), 2, 0,
                        nil, list(Q("OBJECT"), Qstream) );

    // broadcast-stream-streams (object)
    gf = install_gf(Q("BROADCAST-STREAM-STREAMS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // open-stream-p (stream)
    gf = install_gf(Q("OPEN-STREAM-P"), 1, 0,
                        nil, list(Qstream) );

    // change-class (instance new-class)
    gf = install_gf(Q("CHANGE-CLASS"), 2, 0,
                        nil, list(Q("INSTANCE"), Q("NEW-CLASS")) );

    // stream-external-format (stream)
    gf = install_gf(Q("STREAM-EXTERNAL-FORMAT"), 1, 0,
                        nil, list(Qstream) );

    // class-name (object)
    gf = install_gf(Q("CLASS-NAME"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // (setf class-name) (value object)
    gf = install_gf(list(Q("SETF"), Q("CLASS-NAME")), 2, 0,
                        nil, list(Q("VALUE"), Q("OBJECT")) );

    // input-stream-p (stream)
    gf = install_gf(Q("INPUT-STREAM-P"), 1, 0,
                        nil, list(Qstream) );

    // echo-stream-output-stream (object)
    gf = install_gf(Q("ECHO-STREAM-OUTPUT-STREAM"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // two-way-stream-output-stream (object)
    gf = install_gf(Q("TWO-WAY-STREAM-OUTPUT-STREAM"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // output-stream-p (stream)
    gf = install_gf(Q("OUTPUT-STREAM-P"), 1, 0,
                        nil, list(Qstream) );

    // unbound-slot-instance (object)
    gf = install_gf(Q("UNBOUND-SLOT-INSTANCE"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // arithmetic-error-operation (object)
    gf = install_gf(Q("ARITHMETIC-ERROR-OPERATION"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:map-dependents (dependee function)
    gf = install_gf(Q("CLOS:MAP-DEPENDENTS"), 2, 0,
                        nil, list(Q("DEPENDEE"), Qfunction) );

    // clos:accessor-method-slot-definition (object)
    gf = install_gf(Q("CLOS:ACCESSOR-METHOD-SLOT-DEFINITION"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:find-method-combination (gf type options)
    gf = install_gf(Q("CLOS:FIND-METHOD-COMBINATION"), 3, 0,
                        nil, list(Q("GF"), Q("TYPE"), Q("OPTIONS")) );

    // clos:generic-function-lambda-list (gf)
    gf = install_gf(Q("CLOS:GENERIC-FUNCTION-LAMBDA-LIST"), 1, 0,
                        nil, list(Q("GF")) );

    // clos:class-finalized-p (class)
    gf = install_gf(Q("CLOS:CLASS-FINALIZED-P"), 1, 0,
                        nil, list(Qclass) );

    // clos:eql-specializer-object (object)
    gf = install_gf(Q("CLOS:EQL-SPECIALIZER-OBJECT"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:slot-definition-location (object)
    gf = install_gf(Q("CLOS:SLOT-DEFINITION-LOCATION"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:slot-boundp-using-class (class object slotd)
    gf = install_gf(Q("CLOS:SLOT-BOUNDP-USING-CLASS"), 3, 0,
                        nil, list(Qclass, Q("OBJECT"), Q("SLOTD")) );

    // clos:compute-discriminating-function (gf)
    gf = install_gf(Q("CLOS:COMPUTE-DISCRIMINATING-FUNCTION"), 1, 0,
                        nil, list(Q("GF")) );

    // clos:ensure-class-using-class (class name &rest args)
    gf = install_gf(Q("CLOS:ENSURE-CLASS-USING-CLASS"), 2, 0,
                        t, cons(Qclass, list(Q("NAME"), Q("&REST"), Q("ARGS"))) );

    // clos:method-lambda-list (object)
    gf = install_gf(Q("CLOS:METHOD-LAMBDA-LIST"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:class-default-initargs (class)
    gf = install_gf(Q("CLOS:CLASS-DEFAULT-INITARGS"), 1, 0,
                        nil, list(Qclass) );

    // clos:generic-function-method-class (object)
    gf = install_gf(Q("CLOS:GENERIC-FUNCTION-METHOD-CLASS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:compute-slots (class)
    gf = install_gf(Q("CLOS:COMPUTE-SLOTS"), 1, 0,
                        nil, list(Qclass) );

    // clos:class-slots (class)
    gf = install_gf(Q("CLOS:CLASS-SLOTS"), 1, 0,
                        nil, list(Qclass) );

    // clos:add-dependent (dependee dependent)
    gf = install_gf(Q("CLOS:ADD-DEPENDENT"), 2, 0,
                        nil, list(Q("DEPENDEE"), Q("DEPENDENT")) );

    // clos:class-precedence-list (class)
    gf = install_gf(Q("CLOS:CLASS-PRECEDENCE-LIST"), 1, 0,
                        nil, list(Qclass) );

    // clos:slot-definition-initform (object)
    gf = install_gf(Q("CLOS:SLOT-DEFINITION-INITFORM"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:slot-definition-initargs (object)
    gf = install_gf(Q("CLOS:SLOT-DEFINITION-INITARGS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:generic-function-methods (object)
    gf = install_gf(Q("CLOS:GENERIC-FUNCTION-METHODS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:slot-definition-type (object)
    gf = install_gf(Q("CLOS:SLOT-DEFINITION-TYPE"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:slot-value-using-class (class object slotd)
    gf = install_gf(Q("CLOS:SLOT-VALUE-USING-CLASS"), 3, 0,
                        nil, list(Qclass, Q("OBJECT"), Q("SLOTD")) );

    // (setf clos:slot-value-using-class) (value class object slotd)
    gf = install_gf(list(Q("SETF"), Q("CLOS:SLOT-VALUE-USING-CLASS")), 4, 0,
                        nil, cons(Q("VALUE"), list(Qclass, Q("OBJECT"), Q("SLOTD"))) );

    // clos:add-direct-method (class method)
    gf = install_gf(Q("CLOS:ADD-DIRECT-METHOD"), 2, 0,
                        nil, list(Qclass, Qmethod) );

    // clos:slot-exists-p-using-class (class object slot-name)
    gf = install_gf(Q("CLOS:SLOT-EXISTS-P-USING-CLASS"), 3, 0,
                        nil, list(Qclass, Q("OBJECT"), Q("SLOT-NAME")) );

    // clos:slot-makunbound-using-class (class object slotd)
    gf = install_gf(Q("CLOS:SLOT-MAKUNBOUND-USING-CLASS"), 3, 0,
                        nil, list(Qclass, Q("OBJECT"), Q("SLOTD")) );

    // clos:method-generic-function (object)
    gf = install_gf(Q("CLOS:METHOD-GENERIC-FUNCTION"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:update-dependent (dependent &rest initargs)
    gf = install_gf(Q("CLOS:UPDATE-DEPENDENT"), 1, 0,
                        t, list(Q("DEPENDENT"), Q("&REST"), Q("INITARGS")) );

    // clos:compute-effective-method (gf mcomb methods)
    gf = install_gf(Q("CLOS:COMPUTE-EFFECTIVE-METHOD"), 3, 0,
                        nil, list(Q("GF"), Q("MCOMB"), Q("METHODS")) );

    // clos:reader-method-class (class slotd &rest initargs)
    gf = install_gf(Q("CLOS:READER-METHOD-CLASS"), 2, 0,
                        t, cons(Qclass, list(Q("SLOTD"), Q("&REST"), Q("INITARGS"))) );

    // clos:effective-slot-definition-class (class &rest initargs)
    gf = install_gf(Q("CLOS:EFFECTIVE-SLOT-DEFINITION-CLASS"), 1, 0,
                        t, list(Qclass, Q("&REST"), Q("INITARGS")) );

    // clos:specializer-direct-generic-functions (specializer)
    gf = install_gf(Q("CLOS:SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"), 1, 0,
                        nil, list(Q("CLOS:SPECIALIZER")) );

    // clos:ensure-generic-function-using-class (existing fname &rest initargs &key ((generic-function-class gf-class) nil gf-class-p) ((method-class mt-class) nil mt-class-p) ((method-combination mt-comb) nil mt-comb-p))
    gf = install_gf(Q("CLOS:ENSURE-GENERIC-FUNCTION-USING-CLASS"), 2, 0,
                        cons(Q("&KEY"), list(cons(Q(":GENERIC-FUNCTION-CLASS"), cons(Q("GF-CLASS"), list(Q("GF-CLASS-P"), nil, t))), cons(Q(":METHOD-CLASS"), cons(Q("MT-CLASS"), list(Q("MT-CLASS-P"), nil, t))), cons(Q(":METHOD-COMBINATION"), cons(Q("MT-COMB"), list(Q("MT-COMB-P"), nil, t))))), cons(Q("EXISTING"), cons(Q("FNAME"), cons(Q("&REST"), cons(Q("INITARGS"), cons(Q("&KEY"), list(list(list(Q(":GENERIC-FUNCTION-CLASS"), Q("GF-CLASS")), nil, Q("GF-CLASS-P")), list(list(Q(":METHOD-CLASS"), Q("MT-CLASS")), nil, Q("MT-CLASS-P")), list(list(Q(":METHOD-COMBINATION"), Q("MT-COMB")), nil, Q("MT-COMB-P")))))))) );

    // clos:class-direct-superclasses (object)
    gf = install_gf(Q("CLOS:CLASS-DIRECT-SUPERCLASSES"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:make-method-lambda (gf mt form-lambda env)
    gf = install_gf(Q("CLOS:MAKE-METHOD-LAMBDA"), 4, 0,
                        nil, cons(Q("GF"), list(Q("MT"), Q("FORM-LAMBDA"), Q("ENV"))) );

    // clos:slot-definition-name (object)
    gf = install_gf(Q("CLOS:SLOT-DEFINITION-NAME"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:class-prototype (class)
    gf = install_gf(Q("CLOS:CLASS-PROTOTYPE"), 1, 0,
                        nil, list(Qclass) );

    // clos:generic-function-argument-precedence-order (gf)
    gf = install_gf(Q("CLOS:GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER"), 1, 0,
                        nil, list(Q("GF")) );

    // clos:validate-superclass (class new-super)
    gf = install_gf(Q("CLOS:VALIDATE-SUPERCLASS"), 2, 0,
                        nil, list(Qclass, Q("NEW-SUPER")) );

    // clos:method-function (object)
    gf = install_gf(Q("CLOS:METHOD-FUNCTION"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:generic-function-method-combination (object)
    gf = install_gf(Q("CLOS:GENERIC-FUNCTION-METHOD-COMBINATION"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:finalize-inheritance (class)
    gf = install_gf(Q("CLOS:FINALIZE-INHERITANCE"), 1, 0,
                        nil, list(Qclass) );

    // clos:compute-effective-slot-definition (class name dslotds)
    gf = install_gf(Q("CLOS:COMPUTE-EFFECTIVE-SLOT-DEFINITION"), 3, 0,
                        nil, list(Qclass, Q("NAME"), Q("DSLOTDS")) );

    // clos:generic-function-declarations (gf)
    gf = install_gf(Q("CLOS:GENERIC-FUNCTION-DECLARATIONS"), 1, 0,
                        nil, list(Q("GF")) );

    // clos:compute-class-precedence-list (class)
    gf = install_gf(Q("CLOS:COMPUTE-CLASS-PRECEDENCE-LIST"), 1, 0,
                        nil, list(Qclass) );

    // clos:slot-definition-allocation (slotd)
    gf = install_gf(Q("CLOS:SLOT-DEFINITION-ALLOCATION"), 1, 0,
                        nil, list(Q("SLOTD")) );

    // clos:generic-function-name (object)
    gf = install_gf(Q("CLOS:GENERIC-FUNCTION-NAME"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // (setf clos:generic-function-name) (value object)
    gf = install_gf(list(Q("SETF"), Q("CLOS:GENERIC-FUNCTION-NAME")), 2, 0,
                        nil, list(Q("VALUE"), Q("OBJECT")) );

    // clos:class-direct-subclasses (object)
    gf = install_gf(Q("CLOS:CLASS-DIRECT-SUBCLASSES"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:class-direct-slots (object)
    gf = install_gf(Q("CLOS:CLASS-DIRECT-SLOTS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:slot-definition-writers (object)
    gf = install_gf(Q("CLOS:SLOT-DEFINITION-WRITERS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:direct-slot-definition-class (class &rest initargs)
    gf = install_gf(Q("CLOS:DIRECT-SLOT-DEFINITION-CLASS"), 1, 0,
                        t, list(Qclass, Q("&REST"), Q("INITARGS")) );

    // clos:remove-direct-method (class method)
    gf = install_gf(Q("CLOS:REMOVE-DIRECT-METHOD"), 2, 0,
                        nil, list(Qclass, Qmethod) );

    // clos:add-direct-subclass (superclass subclass)
    gf = install_gf(Q("CLOS:ADD-DIRECT-SUBCLASS"), 2, 0,
                        nil, list(Q("SUPERCLASS"), Q("SUBCLASS")) );

    // clos:slot-definition-readers (object)
    gf = install_gf(Q("CLOS:SLOT-DEFINITION-READERS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:remove-direct-subclass (superclass subclass)
    gf = install_gf(Q("CLOS:REMOVE-DIRECT-SUBCLASS"), 2, 0,
                        nil, list(Q("SUPERCLASS"), Q("SUBCLASS")) );

    // clos:compute-default-initargs (class)
    gf = install_gf(Q("CLOS:COMPUTE-DEFAULT-INITARGS"), 1, 0,
                        nil, list(Qclass) );

    // clos:slot-definition-initfunction (object)
    gf = install_gf(Q("CLOS:SLOT-DEFINITION-INITFUNCTION"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:method-specializers (object)
    gf = install_gf(Q("CLOS:METHOD-SPECIALIZERS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:class-direct-default-initargs (class)
    gf = install_gf(Q("CLOS:CLASS-DIRECT-DEFAULT-INITARGS"), 1, 0,
                        nil, list(Qclass) );

    // clos:specializer-direct-methods (object)
    gf = install_gf(Q("CLOS:SPECIALIZER-DIRECT-METHODS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // clos:writer-method-class (class slotd &rest initargs)
    gf = install_gf(Q("CLOS:WRITER-METHOD-CLASS"), 2, 0,
                        t, cons(Qclass, list(Q("SLOTD"), Q("&REST"), Q("INITARGS"))) );

    // clos:remove-dependent (dependee dependent)
    gf = install_gf(Q("CLOS:REMOVE-DEPENDENT"), 2, 0,
                        nil, list(Q("DEPENDEE"), Q("DEPENDENT")) );

    // clos:compute-applicable-methods-using-classes (gf classes)
    gf = install_gf(Q("CLOS:COMPUTE-APPLICABLE-METHODS-USING-CLASSES"), 2, 0,
                        nil, list(Q("GF"), Q("CLASSES")) );

    // si::default-initargs (class initargs class-default-initargs)
    gf = install_gf(Q("DEFAULT-INITARGS"), 3, 0,
                        nil, list(Qclass, Q("INITARGS"), Q("CLOS:CLASS-DEFAULT-INITARGS")) );

    // ext:function-name (fn)
    gf = install_gf(Q("FUNCTION-NAME"), 1, 0,
                        nil, list(Q("FN")) );

    gf = install_gf(list(Q("SETF"), Q("FUNCTION-NAME")), 2, 0,
                        nil, list(Q("NAME"), Q("FN")) );

    // si::internal-directory-namestring (pathname stream)
    gf = install_gf(Q("INTERNAL-DIRECTORY-NAMESTRING"), 2, 0,
                        nil, list(Qpathname, Qstream) );

    // si::internal-enough-namestring (pathname defaults stream)
    gf = install_gf(Q("INTERNAL-ENOUGH-NAMESTRING"), 3, 0,
                        nil, list(Qpathname, Q("DEFAULTS"), Qstream) );

    // si::internal-file-namestring (pathname stream)
    gf = install_gf(Q("INTERNAL-FILE-NAMESTRING"), 2, 0,
                        nil, list(Qpathname, Qstream) );

    // si::internal-namestring (pathname stream)
    gf = install_gf(Q("INTERNAL-NAMESTRING"), 2, 0,
                        nil, list(Qpathname, Qstream) );

    // si::make-pathname-using-host (host device directory name type version)
    gf = install_gf(Q("MAKE-PATHNAME-USING-HOST"), 6, 0,
                        nil, cons(Q("HOST"), cons(Q("DEVICE"), cons(Q("DIRECTORY"), list(Q("NAME"), Q("TYPE"), Q("VERSION"))))) );

    // si::parse-device-component (host device)
    gf = install_gf(Q("PARSE-DEVICE-COMPONENT"), 2, 0,
                        nil, list(Q("HOST"), Q("DEVICE")) );

    // si::parse-directory-component (host directory)
    gf = install_gf(Q("PARSE-DIRECTORY-COMPONENT"), 2, 0,
                        nil, list(Q("HOST"), Q("DIRECTORY")) );

    // si::parse-name-component (host name)
    gf = install_gf(Q("PARSE-NAME-COMPONENT"), 2, 0,
                        nil, list(Q("HOST"), Q("NAME")) );

    // si::parse-namestring-using-host (host string start end)
    gf = install_gf(Q("PARSE-NAMESTRING-USING-HOST"), 4, 0,
                        nil, cons(Q("HOST"), list(Qstring, Q("START"), Q("END"))) );

    // si::parse-type-component (host type)
    gf = install_gf(Q("PARSE-TYPE-COMPONENT"), 2, 0,
                        nil, list(Q("HOST"), Q("TYPE")) );

    // si::parse-version-component (host version)
    gf = install_gf(Q("PARSE-VERSION-COMPONENT"), 2, 0,
                        nil, list(Q("HOST"), Q("VERSION")) );

    // si::pathname-equal-using-host (host x y)
    gf = install_gf(Q("PATHNAME-EQUAL-USING-HOST"), 3, 0,
                        nil, list(Q("HOST"), Q("X"), Q("Y")) );

    // si::pathname-match-p-using-host (host pathname wildcard)
    gf = install_gf(Q("PATHNAME-MATCH-P-USING-HOST"), 3, 0,
                        nil, list(Q("HOST"), Qpathname, Q("WILDCARD")) );

    // si::translate-component-using-host (host elts parts)
    gf = install_gf(Q("TRANSLATE-COMPONENT-USING-HOST"), 3, 0,
                        nil, list(Q("HOST"), Q("ELTS"), Q("PARTS")) );

    // si::translate-directory-using-host (host pat-dir parts)
    gf = install_gf(Q("TRANSLATE-DIRECTORY-USING-HOST"), 3, 0,
                        nil, list(Q("HOST"), Q("PAT-DIR"), Q("PARTS")) );

    // si::translate-pathname-using-host (host-1 pathname from-wildcard to-wildcard)
    gf = install_gf(Q("TRANSLATE-PATHNAME-USING-HOST"), 4, 0,
                        nil, cons(Q("HOST-1"), list(Qpathname, Q("FROM-WILDCARD"), Q("TO-WILDCARD"))) );

    // si::truename-using-host (host pathname)
    gf = install_gf(Q("TRUENAME-USING-HOST"), 2, 0,
                        nil, list(Q("HOST"), Qpathname) );

    // si::wild-pathname-p-using-host (host pathname field-key)
    gf = install_gf(Q("WILD-PATHNAME-P-USING-HOST"), 3, 0,
                        nil, list(Q("HOST"), Qpathname, Q("FIELD-KEY")) );

    // si::stream-flags (object)
    gf = install_gf(Q("STREAM-FLAGS"), 1, 0,
                        nil, list(Q("OBJECT")) );

    // ext:realize-instance (object &rest initargs)
    gf = install_gf(Q("REALIZE-INSTANCE"), 1, 0,
                        t, list(Q("OBJECT"), Q("&REST"), Q("INITARGS")) );

    // ext:stream-advance-to-column (stream column)
    gf = install_gf(Q("STREAM-ADVANCE-TO-COLUMN"), 2, 0,
                        nil, list(Qstream, Q("COLUMN")) );

    // ext:stream-clear-input (stream)
    gf = install_gf(Q("STREAM-CLEAR-INPUT"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-clear-output (stream)
    gf = install_gf(Q("STREAM-CLEAR-OUTPUT"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-finish-output (stream)
    gf = install_gf(Q("STREAM-FINISH-OUTPUT"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-force-output (stream)
    gf = install_gf(Q("STREAM-FORCE-OUTPUT"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-fresh-line (stream)
    gf = install_gf(Q("STREAM-FRESH-LINE"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-line-column (stream)
    gf = install_gf(Q("STREAM-LINE-COLUMN"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-line-number (stream)
    gf = install_gf(Q("STREAM-LINE-NUMBER"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-listen (stream)
    gf = install_gf(Q("STREAM-LISTEN"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-output-width (stream)
    gf = install_gf(Q("STREAM-OUTPUT-WIDTH"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-pathname (stream)
    gf = install_gf(Q("STREAM-PATHNAME"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-peek-char (stream)
    gf = install_gf(Q("STREAM-PEEK-CHAR"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-read-byte (stream)
    gf = install_gf(Q("STREAM-READ-BYTE"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-read-bytes (stream vector &optional start end)
    gf = install_gf(Q("STREAM-READ-BYTES"), 2, 2,
                        nil, cons(Qstream, cons(Qvector, list(Q("&OPTIONAL"), Q("START"), Q("END")))) );

    // ext:stream-read-char (stream)
    gf = install_gf(Q("STREAM-READ-CHAR"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-read-char-no-hang (stream)
    gf = install_gf(Q("STREAM-READ-CHAR-NO-HANG"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-read-line (stream)
    gf = install_gf(Q("STREAM-READ-LINE"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-read-sequence (stream sequence &optional (start 0) (end))
    gf = install_gf(Q("STREAM-READ-SEQUENCE"), 2, 2,
                        nil, cons(Qstream, cons(Qsequence, list(Q("&OPTIONAL"), list(Q("START"), Fixnum::Encode(0)), list(Q("END"))))) );

    // ext:stream-start-line-p (stream)
    gf = install_gf(Q("STREAM-START-LINE-P"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-terpri (stream)
    gf = install_gf(Q("STREAM-TERPRI"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-truename (stream)
    gf = install_gf(Q("STREAM-TRUENAME"), 1, 0,
                        nil, list(Qstream) );

    // ext:stream-unread-char (stream char)
    gf = install_gf(Q("STREAM-UNREAD-CHAR"), 2, 0,
                        nil, list(Qstream, Q("CHAR")) );

    // ext:stream-write-byte (stream byte)
    gf = install_gf(Q("STREAM-WRITE-BYTE"), 2, 0,
                        nil, list(Qstream, Q("BYTE")) );

    // ext:stream-write-bytes (stream vector &optional start end)
    gf = install_gf(Q("STREAM-WRITE-BYTES"), 2, 2,
                        nil, cons(Qstream, cons(Qvector, list(Q("&OPTIONAL"), Q("START"), Q("END")))) );

    // ext:stream-write-char (stream char)
    gf = install_gf(Q("STREAM-WRITE-CHAR"), 2, 0,
                        nil, list(Qstream, Q("CHAR")) );

    // ext:stream-write-sequence (stream sequence &optional (start 0) (end))
    gf = install_gf(Q("STREAM-WRITE-SEQUENCE"), 2, 2,
                        nil, cons(Qstream, cons(Qsequence, list(Q("&OPTIONAL"), list(Q("START"), Fixnum::Encode(0)), list(Q("END"))))) );

    // ext:stream-write-string (stream string &optional (start 0) (end))
    gf = install_gf(Q("STREAM-WRITE-STRING"), 2, 2,
                        nil, cons(Qstream, cons(Qstring, list(Q("&OPTIONAL"), list(Q("START"), Fixnum::Encode(0)), list(Q("END"))))) );
