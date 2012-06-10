//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - Boot Objects
// boot/bt_objet.inc
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/boot/bt_object.inc#17 $
//

#if ! defined(DEFKEY)
    #define DEFKEY(mp_cname, mp_name)
#endif // ! defined(DEFKEY)

#if ! defined(DEFOBJECT)
    #define DEFOBJECT(mp_cname, mp_expr)
#endif // ! defined(DEFOBJECT)

#if ! defined(DEFPACKAGE)
    #define DEFPACKAGE(mp_cname, mp_name, mp_alias, mp_ext, mp_int, mp_uses)
#endif // ! defined(DEFPACKAGE)

#if ! defined(DEFSYM)
    #define DEFSYM(mp_cname, mp_package, mp_name)
#endif // ! defined(DEFSYM)

#if ! defined(DEFTLV)
    #define DEFTLV(mp_cname, mp_name, mp_init)
#endif // ! defined(DEFTLV)

#if ! defined(DEFVAR)
    #define DEFVAR(mp_cname, mp_name)
#endif // ! defined(DEFVAR)

//////////////////////////////////////////////////////////////////////
//
// Packages
//
//  DEFPACKAGE must be placed before DEFKEY.
//
DEFPACKAGE(cl,      L"COMMON-LISP",         L"CL",      1700, 0,    L"")
DEFPACKAGE(cl_user, L"COMMON-LISP-USER",    L"CL-USER",    0, 2003, L"CL")
DEFPACKAGE(ext,     L"EXTENSION",           L"EXT",     2003, 0, L"CL")
DEFPACKAGE(si,      L"SYSTEM",              L"SI",      2003, 3003, L"CL EXT CLOS")
DEFPACKAGE(keyword, L"KEYWORD",             L"",        2003, 0,    L"")
DEFPACKAGE(clos,    L"CLOS",                L"",        150, 0,     L"CL EXT")
DEFPACKAGE(unicode, L"UNICODE",             L"",        150, 0,     L"")


//////////////////////////////////////////////////////////////////////
//
// Keywords
//
// For package
DEFKEY(arguments,           L"ARGUMENTS")       // too-many-arguments
DEFKEY(both,                L"BOTH")            // weak-vector
DEFKEY(constant,            L"CONSTANT")        // defconstant
DEFKEY(code,                L"CODE")            // DllResolve
DEFKEY(cr,                  L"CR")              // external-format
DEFKEY(crlf,                L"CRLF")            // external-format
DEFKEY(datum,               L"DATUM")           // type-error
DEFKEY(default,             L"DEFAULT")         // external-format
DEFKEY(downcase,            L"DOWNCASE")
DEFKEY(either,              L"EITHER")          // weak-vector
DEFKEY(eof,                 L"EOF")             // stream-read-char
DEFKEY(expected_type,       L"EXPECTED-TYPE")   // type-error
DEFKEY(external,            L"EXTERNAL")        // find-symbol
DEFKEY(exclusive,           L"EXCLUSIVE")       // waitable
DEFKEY(filename,            L"FILENAME")        // DllResolve
DEFKEY(function,            L"FUNCTION")        // too-many-arguments
DEFKEY(inherited,           L"INHERITED")       // infd-symbol
DEFKEY(input,               L"INPUT")
DEFKEY(internal,            L"INTERNAL")
DEFKEY(lf,                  L"LF")              // external-format
DEFKEY(locked,              L"LOCKED")
DEFKEY(key,                 L"KEY")             // weak-vector
DEFKEY(macro,               L"MACRO")
DEFKEY(name,                L"NAME")
DEFKEY(operation,           L"OPERATION")       // stream
DEFKEY(output,              L"OUTPUT")          // stream
DEFKEY(preserve,            L"PRESERVE")
DEFKEY(shared,              L"SHARED")          // waitable
DEFKEY(stream,              L"STREAM")          // stream
DEFKEY(special,             L"SPECIAL")         // defvar
DEFKEY(special_operator,    L"SPECIAL-OPERATOR")
DEFKEY(upcase,              L"UPCASE")
DEFKEY(value,               L"VALUE")           // weak-vector
DEFKEY(variable,            L"VARIABLE")        // defvar
DEFKEY(vector,              L"VECTOR")          // vector-index-error

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
DEFSYM(cl, Aallow_other_keys,   L"&ALLOW-OTHER-KEYS")   // type
DEFSYM(cl, Aoptional,           L"&OPTIONAL")       // type
DEFSYM(cl, Akey,                L"&KEY")            // type
DEFSYM(cl, Arest,               L"&REST")           // type
DEFSYM(cl, and,                 L"AND")             // type
DEFSYM(cl, bit,                 L"BIT")
DEFSYM(cl, clear_input,         L"CLEAR-INPUT")     // stream
DEFSYM(cl, clear_output,        L"CLEAR-OUTPUT")    // stream
DEFSYM(cl, close,               L"CLOSE")           // file-stream
DEFSYM(cl, defconstant,         L"DEFCONSTANT")
DEFSYM(cl, defvar,              L"DEFVAR")
DEFSYM(cl, delete_file,         L"DELETE-FILE")     // close
DEFSYM(cl, eq,                  L"EQ")
DEFSYM(cl, eql,                 L"EQL")             // hash-table
DEFSYM(cl, equal,               L"EQUAL")           // hash-table
DEFSYM(cl, equalp,              L"EQUALP")          // hash-table
DEFSYM(cl, finish_output,       L"FINISH-OUTPUT")   // stream
DEFSYM(cl, force_output,        L"FORCE-OUTPUT")    // stream
DEFSYM(cl, ftype,               L"FTYPE")           // augment_function
DEFSYM(cl, funcall,             L"FUNCALL")         // weak-vector
DEFSYM(cl, hash_table_test,     L"HASH-TABLE-TEST") // hash-table
DEFSYM(cl, listen,              L"LISTEN")          // stream
DEFSYM(cl, member,              L"MEMBER")
DEFSYM(cl, nil,                 L"NIL")
DEFSYM(cl, not,                 L"NOT");            // type
DEFSYM(cl, or,                  L"OR");             // error
DEFSYM(cl, quote,               L"QUOTE")
DEFSYM(cl, read_byte,           L"READ-BYTE")       // stream
DEFSYM(cl, read_char,           L"READ-CHAR")       // stream
DEFSYM(cl, setf,                L"SETF")
DEFSYM(cl, signed_byte,         L"SIGNED-BYTE")
DEFSYM(cl, storage_condition,   L"STORAGE-CONDITION")
DEFSYM(cl, symbol_plist,        L"SYMBOL-PLIST")
DEFSYM(cl, unread_char,         L"UNREAD-CHAR")     // stream
DEFSYM(cl, unsigned_byte,       L"UNSIGNED-BYTE")
DEFSYM(cl, write_byte,          L"WRITE-BYTE")      // stream
DEFSYM(cl, write_sequence,      L"WRITE-SEQUENCE")  // stream
DEFSYM(cl, write_string,        L"WRITE-STRING")    // stream

DEFSYM(ext, function_name,      L"FUNCTION-NAME")       // error
DEFSYM(ext, stream_line_column, L"STREAM-LINE-COLUMN")  // stream
DEFSYM(ext, stream_line_number, L"STREAM-LINE-NUMBER") // stream

DEFSYM(si, backquote,          L"BACKQUOTE")           // backquote
DEFSYM(si, tlv,                L"TLV");                // environment
DEFSYM(si, too_many_values,    L"TOO-MANY-VALUES")     // .values*
DEFSYM(si, unquote,            L"UNQUOTE")             // backquote
DEFSYM(si, unquote_splicing,   L"UNQUOTE-SPLICING")    // backquote
DEFSYM(si, unquote_nsplicing,  L"UNQUOTE-NSPLICING")   // backquote
DEFSYM(si, unsupported_stream_operation, L"UNSUPPORTED-STREAM-OPERATION")
DEFSYM(si, without_garbage_collection,   L"WITHOUT-GARBAGE-COLLECTION")

//////////////////////////////////////////////////////////////////////
//
// Thread Local Variables
//
#define DEFTLV_(x, y) DEFTLV(x, y, nil)

DEFTLV_(A,                           L"*")
DEFTLV_(AA,                          L"**")
DEFTLV_(AAA,                         L"***")
DEFTLV_(Abreak_on_signalsA,          L"*BREAK-ON-SIGNALS*")
DEFTLV_(Acompile_file_pathnameA,     L"*COMPILE-FILE-PATHNAME*")
DEFTLV_(Acompile_file_truenameA,     L"*COMPILE-FILE-TRUENAME*")
DEFTLV_(Acompile_printA,             L"*COMPILE-PRINT*")
DEFTLV(Acompile_verboseA,           L"*COMPILE-VERBOSE*", t)
DEFTLV_(Adebugger_hookA,             L"*DEBUGGER-HOOK*")
DEFTLV_(Adebug_ioA,                  L"*DEBUG-IO*")
DEFTLV_(Adefault_pathname_defaultsA, L"*DEFAULT-PATHNAME-DEFAULTS*")
DEFTLV_(Aerror_outputA,              L"*ERROR-OUTPUT*")
DEFTLV_(AfeaturesA,                  L"*FEATURES*")
DEFTLV(Agensym_counterA,            L"*GENSYM-COUNTER*", Fixnum::Encode(1))
DEFTLV_(Aload_pathnameA,             L"*LOAD-PATHNAME*")
DEFTLV_(Aload_printA,                L"*LOAD-PRINT*")
DEFTLV_(Aload_truenameA,             L"*LOAD-TRUENAME*")
DEFTLV(Aload_verboseA,              L"*LOAD-VERBOSE*", t)
DEFTLV_(Amacroexpand_hookA,          L"*MACROEXPAND-HOOK*")
DEFTLV_(AmodulesA,                   L"*MODULES*")
DEFTLV(ApackageA,                    L"*PACKAGE*", PACKAGE_cl_user)
DEFTLV(Aprint_arrayA,               L"*PRINT-ARRAY*", t)
DEFTLV(Aprint_baseA,                L"*PRINT-BASE*", Fixnum::Encode(10))
DEFTLV(Aprint_caseA,                L"*PRINT-CASE*", Kupcase)
DEFTLV_(Aprint_circleA,              L"*PRINT-CIRCLE*")
DEFTLV(Aprint_escapeA,              L"*PRINT-ESCAPE*", t)
DEFTLV(Aprint_gensymA,              L"*PRINT-GENSYM*", t)
DEFTLV_(Aprint_lengthA,              L"*PRINT-LENGTH*")
DEFTLV_(Aprint_levelA,               L"*PRINT-LEVEL*")
DEFTLV_(Aprint_linesA,               L"*PRINT-LINES*")
DEFTLV_(Aprint_miser_widthA,         L"*PRINT-MISER-WIDTH*")
DEFTLV_(Aprint_pprint_dispatchA,     L"*PRINT-PPRINT-DISPATCH*")
DEFTLV_(Aprint_prettyA,              L"*PRINT-PRETTY*")
DEFTLV_(Aprint_radixA,               L"*PRINT-RADIX*")
DEFTLV_(Aprint_readablyA,            L"*PRINT-READABLY*")
DEFTLV_(Aprint_right_marginA,        L"*PRINT-RIGHT-MARGIN*")
DEFTLV_(Aquery_ioA,                  L"*QUERY-IO*")
DEFTLV_(Arandom_stateA,              L"*RANDOM-STATE*")
DEFTLV(Aread_baseA,                 L"*READ-BASE*", Fixnum::Encode(10))
DEFTLV(Aread_default_float_formatA, L"*READ-DEFAULT-FLOAT-FORMAT*", Q("CL:SINGLE-FLOAT"))
DEFTLV(Aread_evalA,                 L"*READ-EVAL*", t)
DEFTLV_(Aread_suppressA,             L"*READ-SUPPRESS*")
DEFTLV_(AreadtableA,                 L"*READTABLE*")
DEFTLV_(Astandard_inputA,            L"*STANDARD-INPUT*")
DEFTLV_(Astandard_outputA,           L"*STANDARD-OUTPUT*")
DEFTLV_(Aterminal_ioA,               L"*TERMINAL-IO*")
DEFTLV_(Atrace_outputA,              L"*TRACE-OUTPUT*")
DEFTLV_(S,                           L"/")
DEFTLV_(SS,                          L"//")
DEFTLV_(SSS,                         L"///")
DEFTLV_(P,                           L"+")
DEFTLV_(PP,                          L"++")
DEFTLV_(PPP,                         L"+++")
DEFTLV_(_,                           L"-")

// CLOS
DEFTLV_(Anext_methodsA, L"*NEXT-METHODS*")

// Compiler
DEFTLV_(AenvironmentA,           L"*ENVIRONMENT*")

// Condition
DEFTLV_(Ahandler_clustersA, L"*HANDLER-CLUSTERS*")
DEFTLV_(Arestart_clustersA, L"*RESTART-CLUSTERS*")

// Stream
DEFTLV(Adefault_charsetA, L"*DEFAULT-CHARSET*",
    Fixnum::Encode(0) )

// Reader
DEFTLV_(Aread_backquoteA,               L"*READ-BACKQUOTE*")
DEFTLV_(Aread_labelsA,                  L"*READ-LABELS*")
DEFTLV_(Aread_line_number_tableA,       L"*READ-LINE-NUMBER-TABLE*")
DEFTLV_(Aread_start_line_numberA,       L"*READ-START-LINE-NUMBER*")
DEFTLV_(Aread_preserving_whitespaceA,   L"*READ-PRESERVING-WHITESPACE*")
DEFTLV_(Aread_pooled_tokenA,            L"*READ-POOLED-TOKEN*")

// 21 Streams

// Debug output for system
DEFTLV_(Adebug_outputA, L"*DEBUG-OUTPUT*")

DEFTLV_(Astack_overflow_handlerA, L"*STACK-OVERFLOW-HANDLER*");

//////////////////////////////////////////////////////////////////////
//
// Variables
//

// 03 Evaluation and Compilation - Direct Call
DEFVAR(Acaller_tableA,          L"*CALLER-TABLE*")
DEFVAR(Acaller_table_latchA,    L"*CALLER-TABLE-LATCH*")

DEFVAR(Aall_packagesA,          L"*ALL-PACKAGES*")
DEFVAR(Aall_packages_latchA,    L"*ALL-PACKAGES-LATCH*")

// 23 Reader
DEFVAR(Adefault_readtableA,     L"*DEFAULT-READTABLE*")
DEFVAR(Astandard_readtableA,    L"*STANDARD-READTABLE*")


DEFVAR(Adll_file_tableA,        L"*DLL-FILE-TABLE*")
DEFVAR(Adll_link_latchA,        L"*DLL-LINK-LATCH*")
DEFVAR(Adll_link_areaA,         L"*DLL-LINK-AREA*")

DEFVAR(Aruntime_environmentA,   L"*RUNTIME-ENVIRONMENT*")

DEFVAR(Asetf_tableA,            L"*SETF-TABLE*")
DEFVAR(Asetf_table_latchA,      L"*SETF-TABLE-LATCH*")

DEFVAR(Atlv_vectorA,            L"*TLV-VECTOR*")
DEFVAR(Atlv_vector_latchA,      L"*TLV-VECTOR-LATCH*")

DEFVAR(Avalue_tableA,           L"*VALUE-TABLE*")
DEFVAR(Avalue_table_latchA,     L"*VALUE-TABLE-LATCH*")

DEFVAR(Aweak_areaA,             L"*WEAK-AREA*");
DEFVAR(Aweak_area_latchA,       L"*WEAK-AREA-LATCH*")


// 07 Objects
DEFVAR(Astandard_method_combinationA, L"*STANDARD-METHOD-COMBINATION*")

// 13 Characters
DEFVAR(Achar_name_tableA, L"*CHAR-NAME-TABLE*");
DEFVAR(Aname_char_tableA, L"*NAME-CHAR-TABLE*");

// 21 Streams
DEFVAR(
    Adefault_charset_defaultA,
    L"*DEFAULT-CHARSET-DEFAULT*" );

DEFVAR(Acharset_tableA, L"*CHARSET-TABLE*");

// 49 Internals
// For image save. *image-save-time* contains universal time of image
// save time.
DEFVAR(Aimage_save_timeA, L"*IMAGE-SAVE-TIME*")

DEFOBJECT(ty_eql_t, list(Qeql, Q("T")))

DEFOBJECT(ty_signed_byte_8 , list(Qsigned_byte, Fixnum::Encode(8)))
DEFOBJECT(ty_signed_byte_16, list(Qsigned_byte, Fixnum::Encode(16)))
DEFOBJECT(ty_signed_byte_32, list(Qsigned_byte, Fixnum::Encode(32)))
DEFOBJECT(ty_signed_byte_64, list(Qsigned_byte, Fixnum::Encode(64)))

DEFOBJECT(ty_unsigned_byte_8 , list(Qunsigned_byte, Fixnum::Encode(8)))
DEFOBJECT(ty_unsigned_byte_16, list(Qunsigned_byte, Fixnum::Encode(16)))
DEFOBJECT(ty_unsigned_byte_32, list(Qunsigned_byte, Fixnum::Encode(32)))
DEFOBJECT(ty_unsigned_byte_64, list(Qunsigned_byte, Fixnum::Encode(64)))

// [F]
DEFOBJECT(ty_float32,   Q("FLOAT32"))
DEFOBJECT(ty_float64,   Q("FLOAT64"))
// [I]
DEFOBJECT(ty_int,       Q("INT"))
DEFOBJECT(ty_int16,     Q("INT16"))
DEFOBJECT(ty_int32,     Q("INT32"))
DEFOBJECT(ty_int64,     Q("INT64"))
DEFOBJECT(ty_int8,      Q("INT8"))
// [U]
DEFOBJECT(ty_uint,      Q("UINT"))
DEFOBJECT(ty_uint16,    Q("UINT16"))
DEFOBJECT(ty_uint64,    Q("UINT64"))
DEFOBJECT(ty_uint32,    Q("UINT32"))
DEFOBJECT(ty_uint8,     Q("UINT8"))


#define DEFOBJECT_1(mp_name) DEFOBJECT(mp_name, mp_name)

// [F]
    DEFOBJECT_1(QQforward_cell_marker)
    DEFOBJECT_1(QQfree_slot_marker)
// [N]
    DEFOBJECT_1(QQnull_string)
    DEFOBJECT_1(QQnull_vector)
// [R]
    DEFOBJECT_1(QQremoved_slot_marker)
// [U]
    DEFOBJECT_1(QQunbound_marker)


#undef DEFCLASS
#undef DEFKEY
#undef DEFOBJECT
#undef DEFPACKAGE
#undef DEFSYM
#undef DEFTLV
#undef DEFVAR
