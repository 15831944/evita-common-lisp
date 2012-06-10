#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - compiler builder
// compiler/boot/cm_bt_builder.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/boot/cm_bt_builder.cpp#31 $
//
// Description:
//  This file contains builder for compiler specific objects.
//
#include "../../boot/bt_defs.h"
#include "../../mini/mini_lisp.h"

//using namespace Kernel;
//using namespace MiniLisp;
//using namespace CommonLisp;

namespace Boot
{

extern Val ty_sequence_index;

extern Val defconstant(const char16*, Val);
extern Val deftype(Val, Val);

static Val PACKAGE_c6;
static Val Qtype_predicate;

//////////////////////////////////////////////////////////////////////
//
// augment function information
//
static void set_fun_info(Val name, Val key, Val datum)
{
    ASSERT(NULL != VAR_Aruntime_environmentA);

    Environment* pEnv = VAR(Aruntime_environmentA)->Decode<Environment>();
    Val htb = pEnv->m_functions;

    Val frob = gethash(name, htb);
    if (nil == frob)
    {
        frob = list(Kfunction);
        setf_gethash(frob, name, htb);
    }

    Val alist = cdr(frob);
    Val pair = assq(key, alist);
    if (nil != pair)
    {
        setf_cdr(datum, pair);
    }
    else
    {
        pair = cons(key, datum);
        setf_cdr(cons(pair, alist), frob);
    }
} // set_fun_info


// build_tys
static void build_tys()
{
    #define defty(mp_ty) defty2(mp_ty, Q##mp_ty)

    #define defty2(mp_ty, mp_val) \
        defobject(ty_##mp_ty, mp_val)

    #define define_ptr_ty(mp_ty) \
        define_ptr_ty2(mp_ty, Q##mp_ty)

    #define define_ptr_ty2(mp_ty, mp_val) \
        defobject(ty_ptr_##mp_ty, list(Qptr, mp_val));


    Val Qptr = Q("EXT:PTR");
    Val Qvalues = Q("VALUES");
    Val QArest  = Q("&REST");

    defty2(bool,   Q("C6::BOOL"));
    defty2(boolean, Q("BOOLEAN"));

    Val ty_values_rest_t = list(Qvalues, QArest, t);
    defty2(values_rest_t,     ty_values_rest_t);
    defty2(void,              Q("EXT:VOID"));
    defty2(unspecified,       Q("EXT:UNSPECIFIED"));

    defty2(setf_function,
        list(Qfunction, list(t, QArest, t), ty_values_rest_t) );

    defobject(ty_nil, nil);
    defobject(ty_ptr_t, list(Qptr, t));

    define_ptr_ty2(int, ty_int);

    // 3 Evaluation and Compilation
    defty2(c6_literal_cell,   Q("C6::LITERAL-CELL"));
    defty2(c6_stack_cell,     Q("C6::STACK-CELL"));

    defobject(ty_ptr_classd, list(Qptr, Qclass_description) );

    // 5 Data and Control Flow
    defty2(function_designator, Q("EXT:FUNCTION-DESIGNATOR"));

    define_ptr_ty(function);

    // 7 Objects
    define_ptr_ty(class);

    // 11 Symbols
    define_ptr_ty(symbol);

    // 12 Numbers
    define_ptr_ty2(fixnum,       Qfixnum);
    define_ptr_ty2(single_float, Qsingle_float);
    define_ptr_ty2(double_float, Qdouble_float);

    // 14 Conses
    define_ptr_ty(cons);
    define_ptr_ty(list);
    define_ptr_ty(null);

    // 17 Seqeunces
    define_ptr_ty2(sequence_index, ty_sequence_index);

    // 49 Internals
    define_ptr_ty2(value_cell,  Qvalue_cell);
    define_ptr_ty2(closed_cell, Qclosed_cell);
} // build_tys


// add_ftype
static void add_ftype(Val name, Val ll, Val v)
{
    set_fun_info(name, Qftype, list(Qfunction, ll, v));
} // add_ftype

// build_accessors
static void build_accessors()
{
    Val Qref = Q("EXT:REF");
    #define defref(mp_name, mp_class, mp_slot) \
        set_fun_info(mp_name, Qref, list(mp_class, mp_slot))

    // 10 Symbols
    defref(Q("SYMBOL-NAME"),    Qsymbol, Q("NAME"));
    defref(Q("SYMBOL-PACKAGE"), Qsymbol, Q("PACKAGE"));

    // 11 Pcakges
    defref(Q("PACKAGE-SHADOWING-SYMBOL"), Qpackage, Q("SHADOWING-SYMBOLS"));
    defref(Q("PACKAGE-USE-LIST"), Qpackage, Q("USE-LIST"));
    defref(Q("PACKAGE-USED-BY-LIST"), Qpackage, Q("USED-BY-LIST"));

    // 14 Conses
    defref(Q("CAR"),   Qlist, Q("CAR"));
    defref(Q("CDR"),   Qlist, Q("CDR"));
    defref(Q("FIRST"), Qlist, Q("CAR"));
    defref(Q("REST"),  Qlist, Q("CDR"));

    defref(intern_setf_cell(Q("CAR")),   Qcons, Q("CAR"));
    defref(intern_setf_cell(Q("CDR")),   Qcons, Q("CDR"));
    defref(intern_setf_cell(Q("FIRST")), Qcons, Q("CAR"));
    defref(intern_setf_cell(Q("REST")),  Qcons, Q("CDR"));

    // 18 Hash Tables
    defref(Q("HASH-TABLE-REHASH-SIZE"), Qhash_table, Q("REHASH-SIZE"));
    defref(Q("HASH-TABLE-TEST"), Qhash_table, Q("TEST"));

    // 23 Reader
    defref(Qreadtable, Qreadtable, Q("CASE"));
} // build_accessors


// build_ftypes
static void build_ftypes()
{
    Val Qfile_error             = Q("FILE-ERROR");
    Val Qftype                  = Q("FTYPE");
    Val Qpackage_error          = Q("PACKAGE-ERROR");
    Val Qstream_error           = Q("STREAM-ERROR");
    Val Qvalues                 = Q("VALUES");

    #include "./cm_bt_ftype_cl.inc"
    #include "./cm_bt_ftypes.inc"

    // #x-reader
    {
        Val ftype = list(Qfunction,
            list(Qstream, Qcharacter, list(Qor, Qfixnum, Qnull)),
            list(Qvalues, Q("&OPTIONAL"), t) );

        char16 wsz[20];
            ::lstrcpyW(wsz, L"#X-reader");

        for (
            const char16* pwsz = L"#()'*+-.:=ABCOPRSX\\|";
            0 != *pwsz;
            pwsz++ )
        {
            wsz[1] = *pwsz;
            Val fname = intern(wsz, PACKAGE_si);
            set_fun_info(fname, Qftype, ftype);
        } // for char
    }
} // build_ftypes


// build_predicates
void build_predicates()
{
    #define defpredicate(mp_name, mp_type) \
        set_fun_info(mp_name, Qtype_predicate, mp_type);

    // 10 Symbols
    defpredicate(Q("SYMBOLP"), Qsymbol);

    // 11 Packages
    defpredicate(Q("PACKAGEP"), Qpackage);

    // 12 Numbers
    defpredicate(Q("COMPLEXP"),         Qcomplex);
    defpredicate(Q("FLOATP"),           Qfloat);
    defpredicate(Q("INTEGERP"),         Qinteger);
    defpredicate(Q("NUMBERP"),          Qnumber);
    defpredicate(Q("RANDAM-STATE-P"),   Q("RANDOM-STATE"));
    defpredicate(Q("RATIONALP"),        Qrational);
    defpredicate(Q("REALP"),            Qreal);

    // 13 Characters
    defpredicate(Q("CHARACTERP"), Qcharacter);

    // 14 Conses
    //defpredicate(Q("ATOM"),  Q("ATOM"));  atom = (not cons)
    defpredicate(Q("CONSP"), Qcons);
    defpredicate(Q("LISTP"), Qlist);

    // 15 Arrays
    defpredicate(Q("ARRAYP"), Qarray);
    defpredicate(Q("BIT-VECTOR-P"), Qbit_vector);
    defpredicate(Q("SIMPLE-BIT-VECTOR-P"), Qsimple_bit_vector);
    defpredicate(Q("SIMPLE-VECTOR-P"), Qsimple_vector);
    defpredicate(Q("VECTORP"), Qvector);

    // 16 Strings
    defpredicate(Q("SIMPLE-STRING-P"), Qsimple_string);
    defpredicate(Q("STRINGP"), Qstring);

    // 18 Hash Tables
    defpredicate(Q("HASH-TABLE-P"), Qhash_table);

    // 19 Filenames
    defpredicate(Q("PATHNAMEP"), Qpathname);

    // 20 Streams
    defpredicate(Q("STREAMP"), Qstream);

    // 23 Reader
    defpredicate(Q("READTABLEP"), Qreadtable);
} // build_predicates


//////////////////////////////////////////////////////////////////////
//
// BuildForCompiler
//
void BuildForCompiler()
{
    PACKAGE_c6 = make_package(
        list(intern(L"COMPILER", PACKAGE_keyword),
             intern(L"C6", PACKAGE_keyword),
             intern(L"C", PACKAGE_keyword),
             intern(L"XC", PACKAGE_keyword) ),
        Fixnum::Encode(1003),
        Fixnum::Encode(1003) );

    use_package(PACKAGE_cl,  PACKAGE_c6);
    use_package(PACKAGE_ext, PACKAGE_c6);

    deftype(Q("C6::FORM"), t);

    deftlv(Q("C6::*SESSION*"), nil);
    deftlv(Q("C6::*TARGET*"), nil);

    deftlv(Q("C6::*SITUATION*"), nil);
    deftlv(Q("C6::*PROCESSING-MODE*"), nil);
    deftlv(Q("C6::*OPTIMIZATION*"), nil);


    deftlv(Q("C6::*SETTINGS*"), nil);
        Q(":SESSION");
        Q(":DEBUG");


    defconstant(L"C6::+EVAL-TARGET+",
        intern(car(PACKAGE_target->Decode<Package>()->m_names),
               PACKAGE_keyword ) );

    defobject(val_anonymous,
        list(intern(L"ANONYMOUS", PACKAGE_keyword)) );


    // We use L_list_free_marker and L_list_removed_marker as alternate
    // key for gethash for NewLiteral function.
    defobject(QQfree_slot_marker_key,    list(QQfree_slot_marker));
    defobject(QQremoved_slot_marker_key, list(QQremoved_slot_marker));

    // For parsing (atom x) to (typep x 'atom).
    defobject(QQquote_atom,   list(Qquote, Q("ATOM")));

    Q("C::CALL-MACRO-EXPANDER");
    Q("C::PARSE-MACRO");
    Q("SI:CLASSD");

    Q("EXT:REQUIRED");
    Qtype_predicate = Q("EXT:TYPE-PREDICATE");
    Q("EXT:UNSPECIFIED");

    // Support function
    Q(".FUNCALL");
    Q(".MAKE-CLOSURE");
    Q(".MAKE-CLOSED-CELL");
    Q(".VALUES*");

    Q(".GO");
    Q(".RETURN-FROM");
    Q(".THROW");

    Q("SUBCLASSP");

    build_tys();
    build_ftypes();
    build_predicates();
    build_accessors();
} // BuildForCompiler

} // Boot
