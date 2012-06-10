#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - initialization
// genesis/gs_init.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_init.cpp#31 $
//
// Description:
//  Installs following C implemented lisp function for genesis.
//
#include "./gs_init.h"

#include "../big/big_lisp.h"

namespace Compiler
{
    Val compile_form(Val);
} // Compiler

namespace Genesis
{

#define Qop_1P Q1P
#define Qop_1_ Q1_
#define Qop_GE QGE


//////////////////////////////////////////////////////////////////////
//
// 3 Evaluation and Compilation
//

// macro-function
Val C_constantp(MiniThread* p)
{
    if (Fixnum::Encode(1) == p->m_n)
    {
        p->mv_value[1] = nil;
    }

    return constantp(p->mv_value[0], p->mv_value[1]) ? t : nil;
} // C_constantp

// macro-function
Val C_macro_function(MiniThread* p)
{
    if (Fixnum::Encode(1) == p->m_n)
    {
        p->mv_value[1] = nil;
    }

    return macro_function(p->mv_value[0], p->mv_value[1]);
} // C_macro_function

// macroexpand-1
Val C_macroexpand_1(MiniThread* p)
{
    if (Fixnum::Encode(1) == p->m_n)
    {
        p->mv_value[1] = nil;
    }

    return macroexpand_1(p->mv_value[0], p->mv_value[1]);
} // C_macroexpand_1

// (setf macro-function)
Val C_setf_macro_function(MiniThread* p)
{
    if (Fixnum::Encode(2) == p->m_n)
    {
        p->mv_value[2] = nil;
    }

    return setf_macro_function(
        p->mv_value[0], p->mv_value[1], p->mv_value[2] );
} // C_setf_macro_function

Val GS_call_macro_expander(Val fn, Val form, Val env)
{
    Val expansion = funcall(fn, form, env);
    MiniThread::Get()->mv_value[1] = nil;
    return expansion;
} // GS_call_macro_expander


//////////////////////////////////////////////////////////////////////
//
// 5 Data and Control Flow
//

Val C_eql(Val x, Val y)
    { return eql(x, y) ? t : nil; }

Val C_equal(Val x, Val y)
    { return equal(x, y) ? t : nil; }

//////////////////////////////////////////////////////////////////////
//
// 07 Objects
//

// C_find_class
Val C_find_class(MiniThread* p)
{
    switch (Fixnum::Decode_(p->m_n))
    {
    case 1:
        p->mv_value[1] = t;
    case 2:
        p->mv_value[2] = nil;
    }
    return find_class(p->mv_value[0], p->mv_value[1], p->mv_value[2]);
} // C_find_class

// C_subst_in_function
Val C_subst_in_function(MiniThread* p)
{
    if (Fixnum::Encode(3) == p->m_n) p->mv_value[3] = nil;
    return subst_in_function(
        p->mv_value[0], p->mv_value[1], p->mv_value[2], p->mv_value[3] );
} // C_subst_in_function


//////////////////////////////////////////////////////////////////////
//
// 09 Conditions
//
void __declspec(noreturn) thread_error_hook(MiniThread* p)
{
    p->m_fn = Qerror->Decode<Symbol>()->m_function;

    if (nil == p->m_fn)
    {
        C_error(p);
    }
    ::CallLisp(p);
} // thread_error_hook


//////////////////////////////////////////////////////////////////////
//
// 10 Symbols
//
Val C_make_symbol(Val name) { return make_symbol(name); }

//////////////////////////////////////////////////////////////////////
//
// 11 Packages
//
Val C_intern(MiniThread* p)
{
    Val sym; Val status;
    if (Fixnum::Encode(1) == p->m_n)
    {
        sym = intern(p->mv_value[0], TLV(ApackageA), &status);
    }
    else
    {
        sym = intern(p->mv_value[0], p->mv_value[1], &status);
    }
    p->mv_value[1] = status;
    p->m_n = Fixnum::Encode(2);
    return sym;
} // C_intern


//////////////////////////////////////////////////////////////////////
//
// 12 Numbers
//

// C_decode_float32
Val C_decode_float32(Val x)
{
    Thread* p = Thread::Get();
    return decode_float32(x, &p->mv_value[1], &p->mv_value[2]);
} // C_decode_float32

// C_decode_float64
Val C_decode_float64(Val x)
{
    Thread* p = Thread::Get();
    return decode_float64(x, &p->mv_value[1], &p->mv_value[2]);
} // C_decode_float64

// C_logbitp
Val C_logbitp(Val k, Val n)
    { return logbitp(k, n) ? t : nil; }


Val C_minusp(Val x) { return minusp(x) ? t : nil; }
Val C_plusp(Val x)  { return plusp(x) ? t : nil; }
Val C_zerop(Val x)  { return zerop(x) ? t : nil; }


Val C_eq(Val x, Val y) { return num_eq(x, y) ? t : nil; }
Val C_ne(Val x, Val y) { return num_eq(x, y) ? nil : t; }

Val C_lt(Val x, Val y) { return C_cmp(x, y) < 0  ? t : nil; }
Val C_le(Val x, Val y) { return C_cmp(x, y) <= 0 ? t : nil; }

Val C_gt(Val x, Val y) { return C_cmp(x, y) > 0  ? t : nil; }
Val C_ge(Val x, Val y) { return C_cmp(x, y) >= 0 ? t : nil; }


//////////////////////////////////////////////////////////////////////
//
// 14 Conses
//

// Z_cons
Val Z_cons(Val a, Val b)
{
    return MiniThread::Get()->GcFence(cons(a, b));
} // Z_cons

// last
Val GS_last(MiniThread* p)
{
    if (Fixnum::Encode(1) == p->m_n)
    {
        p->mv_value[1] = Fixnum::Encode(1);
    }
    return last(p->mv_value[0], p->mv_value[1]);
} // GS_last


//////////////////////////////////////////////////////////////////////
//
// 15 Arrays
//

// replace_vector
Val C_replace_vector(MiniThread* p)
{
    switch (Fixnum::Decode_(p->m_n))
    {
    case 2: p->mv_value[2] = Fixnum::Encode(0);
    case 3: p->mv_value[3] = nil;
    case 4: p->mv_value[4] = Fixnum::Encode(0);
    case 5: p->mv_value[5] = nil;
    case 6: break;
    default: CAN_NOT_HAPPEN();
    } // switch n
    return replace_vector(
        p->mv_value[0], p->mv_value[1],
        p->mv_value[2], p->mv_value[3],
        p->mv_value[4], p->mv_value[5] );
} // C_replace_vector

//////////////////////////////////////////////////////////////////////
//
// 16 Strings
//
Val Zallocate_string(Val a)
{
    return MiniThread::Get()->GcFence(allocate_string(a));
} // Zallocate_string

//////////////////////////////////////////////////////////////////////
//
// 18 Hash Tables
//
Val C_gethash_eq(MiniThread* p)
{
    if (Fixnum::Encode(2) == p->m_n)
    {
        p->mv_value[2] = nil;
    }
    Val present;
    Val val = gethash(
        p->mv_value[0], p->mv_value[1], p->mv_value[2], &present );
    p->m_n = Fixnum::Encode(2);
    p->mv_value[1] = present;
    return val;
} // C_get_hash_eq

Val C_setf_gethash_eq(MiniThread* p)
{
    return setf_gethash(
        p->mv_value[0], p->mv_value[1], p->mv_value[2]);
} // C_get_hash_eq


//////////////////////////////////////////////////////////////////////
//
// 25 Environment
//
Val gs_encode_universal_time(MiniThread* p)
{
    if (Fixnum::Encode(6) == p->m_n)
    {
        p->mv_value[6] = nil;
    }

    return encode_universal_time(
        p->mv_value[0], p->mv_value[1], p->mv_value[2], // s n h
        p->mv_value[3], p->mv_value[4], p->mv_value[5], // d m y
        p->mv_value[6] );                               // z
} // gs_encode_universal_time


Val gs_room(MiniThread* p)
{
    return room(Fixnum::Encode(0) == p->m_n ? Kdefault : p->mv_value[0]);
} // gs_room


//////////////////////////////////////////////////////////////////////
//
// 49 Internals
//

// Zallocate_binobj
Val Zallocate_binobj(Val classd)
{
    MiniThread* p = MiniThread::Get();
    return p->GcFence(p->AllocBinObj(classd));
} // Zallocate_binobj


// Zallocate_binvec
Val Zallocate_binvec(Val classd, Val length)
{
    MiniThread* p = MiniThread::Get();
    return p->GcFence(p->AllocBinVec(classd, length));
} // Zallocate_binvec


// Zallocate_funobj
//  size    byte size of function. It must be multiple of Host::FunObj_Align.
Val Zallocate_funobj(Val classd, Val size)
{
    MiniThread* p = MiniThread::Get();
    return p->GcFence(p->AllocFunction(classd, Fixnum::Decode_(size)));
} // Zallocate_funobj


// Zallocate_instance
Val Zallocate_instance(Val classd)
{
    MiniThread* p = MiniThread::Get();
    return p->GcFence(p->AllocInstance(classd));
} // Zallocate_instance


// Zallocate_record
Val Zallocate_record(Val classd)
{
    MiniThread* p = MiniThread::Get();
    return p->GcFence(p->AllocRecord(classd));
} // Zallocate_record


// Zallocate_vector
Val Zallocate_vector(Val classd, Val length)
{
    MiniThread* p = MiniThread::Get();
    return p->GcFence(p->AllocVector(classd, length));
} // Zallocate_vector


//////////////////////////////////////////////////////////////////////
//
// 50 Extensions - TLV
//
Val C_Zdeftlv(Val name, Val init, Val docstr, Val initp)
{
    deftlv(name, init, initp, docstr);
    Val tlvrec = gethash(name, VAR(Avalue_tableA));
    setf_variable_information(t, name, Kspecial, TLV(AenvironmentA));
    setf_variable_information(tlvrec, name, Qtlv, TLV(AenvironmentA));
    return name;
} // C_Zdeftlv

//////////////////////////////////////////////////////////////////////
//
// Initializer::prepare
//
void Initializer::prepare()
{
} // Initializer::prepare

//////////////////////////////////////////////////////////////////////
//
// Initializer::init_all
//
void Initializer::init_all()
{
    init_03_Evaluation_and_Compilation();
    init_04_Types_and_Classes();
    init_05_Data_and_Control_Flow();
    init_06_Iteration();
    init_07_Objects();
    init_08_Structures();
    init_09_Conditions();
    init_10_Symbols();
    init_11_Packages();
    init_12_Numbers();
    init_13_Characters();
    init_14_Conses();
    init_15_Arrays();
    init_16_Strings();
    init_17_Sequences();
    init_18_Hash_Tables();
    init_19_Filenames();
    init_20_Files();
    init_21_Streams();
    init_22_Printer();
    init_23_Reader();
    init_24_System_Construction();
    init_25_Environment();

    init_49_Internals();
    init_50_Extensions();

    init_Platform();
    init_Platform_Target();
} // Initializer::init_all


//////////////////////////////////////////////////////////////////////
//
// Install Alias
//
void Initializer::install_alias(Val alias, Val real)
{
    if (fboundp(alias)) return;

    if (fboundp(real))
    {
        // BUGBUG: GF doesn't have m_name.
        Val copy = copy_function(fdefinition(real));
        copy->Decode<NativeCodeFunction>()->m_name = alias;
        setf_fdefinition(copy, alias);
    } // if

    // check setf
    {
        Val setf = find_setf_cell(real);
        if (nil != setf && nil != value_cell_value(setf))
        {
            Val copy = copy_function(value_cell_value(setf));
            copy->Decode<NativeCodeFunction>()->m_name = list(Qsetf, alias);
            setf = intern_setf_cell(alias);
            setf_value_cell_value(copy, setf);
        }
    }
} // Initializer::install_alias


//////////////////////////////////////////////////////////////////////
//
// Install Function
//
Val Initializer::install_function(
    int             fGc,
    Val             fname,
    int             iMin,
    int             iMax,
    const char16*   pwszProc,
    int             cVals )
{
    // If arch builder doesn't install function, we install C-version.
    if (fboundp(fname)) return fdefinition(fname);

    Val fun = make_wrapper(fGc, fname, iMin, iMax, pwszProc, cVals);
    return setf_fdefinition(fun, fname);
} // Initializer::install_function


//////////////////////////////////////////////////////////////////////
//
// Install Macro Expander
//
Val Initializer::install_macro_expander(Val name, const char16* pwszProc)
{
    Val fun = macro_function(name);
    if (nil != fun) return fun;

    Val expander = make_wrapper(
        0, list(Qmacro_function, name), 2, 2, pwszProc, 1);

    return setf_macro_function(expander, name);
} // Initializer::x86_init_macro_expander


void Initializer::init_03_Evaluation_and_Compilation()
{
    install_function(
        Qconstantp, 1, 2,
        L"C_constantp" );

    INSTALL_MACRO_EXPANDER(lambda);

    install_function(
        Qmacroexpand_1, 1, 2,
        L"C_macroexpand_1", 2 );

    install_function(
        Qmacro_function, 1, 2,
        L"C_macro_function" );

    install_function(
        list(Qsetf, Qmacro_function), 2, 3,
        L"C_setf_macro_function" );

    install_function(Qeval, 1, 1, L"eval", -1);

    install_function(
        Q("C::COMPILE-FORM"), 1, 1,
        L"compile_form", 4 );

    install_function(Qc6_call_macro_expander, 3, 3,
        L"GS_call_macro_expander", 2 );

    install_function(QDmake_closed_cell, 1, 1,
        L"Dmake_closed_cell" );

    install_function(QDmake_closure, 0, -1,
        L"Dmake_closure" );

    install_function(Q("INTERN-FOREIGN-ENTRY"), 2, 2,
        L"intern_dll_entry" );

    install_function(Q("REGISTER-CALLER"), 2, 2,
        L"register_caller" );
} // Initializer::init_03_Evaluation_and_Compilation

void Initializer::init_04_Types_and_Classes() {}

void Initializer::init_05_Data_and_Control_Flow()
{
    INSTALL_MACRO_EXPANDER(and);
    INSTALL_MACRO_EXPANDER(cond);
    INSTALL_MACRO_EXPANDER(defconstant);
    INSTALL_MACRO_EXPANDER(defun);
    INSTALL_MACRO_EXPANDER(defvar);
    INSTALL_MACRO_EXPANDER(multiple_value_bind);
    INSTALL_MACRO_EXPANDER(multiple_value_list);
    //INSTALL_MACRO_EXPANDER(multiple_value_setq);
    INSTALL_MACRO_EXPANDER(nth_value);
    INSTALL_MACRO_EXPANDER(or);
    INSTALL_MACRO_EXPANDER(prog1);
    INSTALL_MACRO_EXPANDER(psetq);
    INSTALL_MACRO_EXPANDER(return);
    INSTALL_MACRO_EXPANDER(setf);
    INSTALL_MACRO_EXPANDER(unless);
    INSTALL_MACRO_EXPANDER(when);

    // (%defconstant name value doc-string)
    install_function(Q("%DEFCONSTANT"), 3, 3, L"Pdefconstant");

    // (%defun fname funobj)
    install_function(Q("%DEFUN"), 3, 3, L"Pdefun");

    // (%defvar name value doc-string initp)
    install_function(Q("%DEFVAR"), 4, 4, L"Pdefvar");

    install_function(Qeql, 2, 2, L"C_eql");
    install_function(Qequal, 2, 2, L"C_equal");
    install_function(Qfuncall, 1, -1, L"C_funcall", -1);
    install_function(Qvalues_list, 1, 1, L"values_list", -1);

    install_function(
        Q("MAKE-UNDEFINED-FUNCTION-FUNCTION"), 1, 1,
        L"make_undefined_function_function" );

    install_function(
        Q("UPDATE-CALLERS"), 2, 2,
        L"update_callers" );


    install_function(list(Qsetf, Qfdefinition), 2, 2, L"setf_fdefinition");
} // Initializer::init_05_Data_and_Control_Flow

void Initializer::init_06_Iteration()
{
    INSTALL_MACRO_EXPANDER(do);
    INSTALL_MACRO_EXPANDER(doA);
    INSTALL_MACRO_EXPANDER(dolist);
    INSTALL_MACRO_EXPANDER(dotimes);
    INSTALL_MACRO_EXPANDER(loop);
} // Initializer::init_06_Iteration

void Initializer::init_07_Objects()
{
    install_function(Qfind_class, 1, 3, L"C_find_class");

    install_function(QDallocate_funcallable_instance, 1, 1,
        L"allocate_funcallable_instance" );

    install_function(Qfuncallable_instance_function, 1, 1,
        L"funcallable_instance_function" );

    install_function(Qset_funcallable_instance_function, 2, 2,
        L"set_funcallable_instance_function" );

    install_function(Qsubst_in_function, 3, 4,
        L"C_subst_in_function" );
} // Initializer::init_07_Objects

void Initializer::init_08_Structures()
{
    install_function(Q(".ALLOCATE-STRUCTURE"), 1, 1, L"Zallocate_record");
} // Initializer::init_08_Structures()


//////////////////////////////////////////////////////////////////////
//
//  Initializer::init_09_Conditions
//
void Initializer::init_09_Conditions()
{
    // Note: To avoid making undefined-function-function for error,
    // we install function error at first.
    // install_function(Qerror, GsLisp::C_error, 1, -1);

    install_function(Q("THREAD-ERROR-HOOK"), 1, -1, L"thread_error_hook");

    install_function(Qerror, 1, -1, L"C_error");
    install_function(Qinvoke_debugger, 1, 1, L"invoke_debugger");

    INSTALL_MACRO_EXPANDER(assert);
    INSTALL_MACRO_EXPANDER(check_type);
} // Initializer::init_09_Conditions

void Initializer::init_10_Symbols()
{
    install_function(Qboundp, 1, 1, L"C_boundp");
    install_function(Qsymbol_value, 1, 1, L"symbol_value");
    install_function(Qmake_symbol, 1, 1, L"C_make_symbol");
    install_function(Q("!MAKE-SYMBOL"), 1, 1, L"C_make_symbol");

    install_function(
        list(Qsetf, Qsymbol_function), 2, 2,
        L"setf_symbol_function" );

    install_function(
        list(Qsetf, Qsymbol_value), 2, 2,
        L"setf_symbol_value" );
} // Initializer::init_10_Symbols

void Initializer::init_11_Packages()
{
    INSTALL_MACRO_EXPANDER(in_package);
    install_function(Qintern, 1, 2, L"C_intern", 2);
    INSTALL_FUNCTION(find_package, 1, 1);

    install_function(Q("%IN-PACKAGE"), 1, 1, L"Zin_package");
} // Initializer::init_11_Packages

void Initializer::init_12_Numbers()
{
    INSTALL_MACRO_EXPANDER(decf);
    INSTALL_MACRO_EXPANDER(incf);

    INSTALL_FUNCTION(op_1P, 1, 1);
    INSTALL_FUNCTION(op_1_, 1, 1);
    install_function(Qop_GE, 2, 2, L"C_ge");

    install_function(1, Q("COERCE/BIGNUM/FLOAT64"), 1, 1,
        L"coerce_bignum_float64" );

    install_function(1, Q("COERCE/BIGNUM/FLOAT32"), 1, 1,
        L"coerce_bignum_float32" );

    install_function(Q("FLOAT32-TRUNCATE"), 1, 1,
        L"float32_truncate" );

    install_function(Q("FLOAT64-TRUNCATE"), 1, 1,
        L"float64_truncate" );

    install_function(Q("TRUNCATE/2"), 2, 2, L"truncate_2", 2);

    install_function(Q("MINUSP"), 1, 1, L"C_minusp");
    install_function(Q("PLUSP"),  1, 1, L"C_plusp");
    install_function(Q("ZEROP"),  1, 1, L"C_zerop");

    install_function(1, Q("SI::+/2"), 2, 2, L"RTL_add");
    install_function(1, Q("SI:://2"), 2, 2, L"RTL_div");
    install_function(1, Q("SI::*/2"), 2, 2, L"RTL_mul");
    install_function(1, Q("SI::-/2"), 2, 2, L"RTL_sub");
    install_function(1, Q("SI::-/1"), 1, 1, L"RTL_neg");

    install_function(1, Q("ASH"),          2, 2, L"RTL_ash");
    install_function(1, Q("SI::GCD/2"),    2, 2, L"RTL_gcd");
    install_function(Q("LOGBITP"),      2, 2, L"C_logbitp");
    install_function(1, Q("SI::LOGAND/2"), 2, 2, L"RTL_logand");
    install_function(1, Q("SI::LOGIOR/2"), 2, 2, L"RTL_logior");
    install_function(1, Q("SI::LOGXOR/2"), 2, 2, L"RTL_logxor");

    install_function(Q("SI::</2"),  2, 2, L"C_lt");
    install_function(Q("SI::<=/2"), 2, 2, L"C_le");
    install_function(Q("SI::>/2"),  2, 2, L"C_gt");
    install_function(Q("SI::>=/2"), 2, 2, L"C_ge");

    install_function(Q("SI::=/2"),  2, 2, L"C_eq");
    install_function(Q("SI::/=/2"), 2, 2, L"C_ne");
} // Initializer::init_12_Numbers

void Initializer::init_13_Characters() {}

void Initializer::init_14_Conses()
{
    install_function(1, Qcons,  2,  2, L"Z_cons");
    install_function(Qlast,  1,  2, L"GS_last");
    install_function(Qlist,  0, -1, L"C_list");
    install_function(QlistA, 0, -1, L"C_listA");
    install_function(Qnconc, 0, -1, L"C_nconc");

    INSTALL_MACRO_EXPANDER(pop);
    INSTALL_MACRO_EXPANDER(push);

    INSTALL_ALIAS(null,  not);

    INSTALL_ALIAS(rest,  cdr);

    INSTALL_ALIAS(first,  car);
    INSTALL_ALIAS(second, cadr);
    INSTALL_ALIAS(third,  caddr);
    INSTALL_ALIAS(fourth, cadddr);
} // Initializer::init_14_Conses

void Initializer::init_15_Arrays()
{
    install_function(Q("SBIT/2"), 2, 2,
        L"sbit" );

    install_function(list(Qsetf, Q("SBIT/2")), 3, 3,
        L"setf_sbit" );

    install_function(Q(".REPLACE-VECTOR"), 2, 6,
        L"C_replace_vector" );

    install_function(Q("BIT-NOT/2"), 5, 5, L"bit_not");

    install_function(Q("BIT-AND/3"),   7, 7, L"bit_and");
    install_function(Q("BIT-ANDC1/3"), 7, 7, L"bit_andc1");
    install_function(Q("BIT-ANDC2/3"), 7, 7, L"bit_andc2");
    install_function(Q("BIT-EQV/3"),   7, 7, L"bit_eqv");
    install_function(Q("BIT-IOR/3"),   7, 7, L"bit_ior");
    install_function(Q("BIT-NAND/3"),  7, 7, L"bit_nand");
    install_function(Q("BIT-NOR/3"),   7, 7, L"bit_nor");
    install_function(Q("BIT-ORC1/3"),  7, 7, L"bit_orc1");
    install_function(Q("BIT-ORC2/3"),  7, 7, L"bit_orc2");
    install_function(Q("BIT-XOR/3"),   7, 7, L"bit_xor");
} // init_15_Arrays

void Initializer::init_16_Strings()
{
    // for caar, ... cddddr
    install_function(1, Q(".ALLOCATE-STRING"), 1, 1,
        L"Zallocate_string" );

    // for mapcar => intern => find-symbol
    install_function(Q("CL:STRING"), 1, 1, L"to_string");
} // Initializer::init_16_Strings

void Initializer::init_17_Sequences()
{
    INSTALL_FUNCTION(nreverse, 1, 1);
} // Initializer::init_17_Sequences

void Initializer::init_18_Hash_Tables()
{
    install_function(QgethashSeq, 2, 3,
        L"C_gethash_eq", 2 );

    install_function(list(Qsetf, QgethashSeq), 3, 4,
        L"C_setf_gethash_eq" );

    // for mapcar => intern => find-symbol
    install_function(Qsxhash,  1, 1, L"sxhash");

    install_function(Q("SXHASH/EQ"),  1, 1, L"sxhash_eq");
    install_function(Q("SXHASH/EQL"), 1, 1, L"sxhash_eql");
} // Initializer::init_18_Hash_Tables

void Initializer::init_19_Filenames() {}
void Initializer::init_20_Files() {}


void Initializer::init_22_Printer()
{
    // (format stream control arg*)
    Qformat->Decode<Symbol>()->m_function =
        install_function(Q(".FORMAT"), 2, -1, L"C_format");
} // Initializer::init_22_Printer

void Initializer::init_23_Reader() {}

void Initializer::init_24_System_Construction()
{
    install_function(Qload, 1, -1, L"C_load");
} // Initializer::init_24_System_Construction

//////////////////////////////////////////////////////////////////////
//
//  Initializer::init_25_Environment
//
void Initializer::init_25_Environment()
{
    INSTALL_FUNCTION(disassemble, 1, 1);
    INSTALL_FUNCTION(inspect, 1, 1);

    install_function(Qroom, 0, 1, L"gs_room");

    INSTALL_MACRO_EXPANDER(time);
    install_function(1, Q("TIME-IT"), 1, 1, L"time_it", -1);

    install_function(Qget_universal_time, 0, 0, L"get_universal_time");

    install_function(Qget_decoded_time, 0, 0, L"get_decoded_time", 9);

    install_function(Q(".GET-PROCESS-TIMES"), 0, 0, L"get_process_times", 3);
    install_function(Q(".GET-THREAD-TIMES"),  0, 0, L"get_thread_times", 3);

    #if _WIN32
        install_function(Q(".GET-COMPUTER-NAME"), 1, 1, L"get_computer_name");
    #endif // _WIN32

    INSTALL_FUNCTION(software_version, 0, 0);
} // Initializer::init_25_Environment

void Initializer::init_49_Internals()
{
    install_function(Q("STATIC-OBJECT-P"), 1, 1, L"C_static_object_p");

    install_function(Q("INTERN-SETF-CELL"), 1 ,1, L"intern_setf_cell");

    INSTALL_MACRO_EXPANDER(ref);

    install_function(Q("EXIT-PROCESS"), 1, 1, L"exit_process");

    install_function(1, Q(".ALLOCATE-BINOBJ"),   1, 1, L"Zallocate_binobj");
    install_function(1, Q(".ALLOCATE-BINVEC"),   2, 2, L"Zallocate_binvec");
    install_function(1, Q(".ALLOCATE-FUNOBJ"),   2, 2, L"Zallocate_funobj");
    install_function(1, Q(".ALLOCATE-INSTANCE"), 1, 1, L"Zallocate_instance");
    install_function(1, Q(".ALLOCATE-RECORD"),   1, 1, L"Zallocate_record");
    install_function(1, Q(".ALLOCATE-VECTOR"),   2, 2, L"Zallocate_vector");
    install_function(1, Q(".ALLOCATE-WEAKOBJ"),  1, 1, L"allocate_weakobj");

    install_function(Q("INTERNAL-SAVE-IMAGE"), 1, 1, L"internal_save_image");

    INSTALL_MACRO_EXPANDER(without_garbage_collection);
} // Initializer::init_49_Internals

void Initializer::init_50_Extensions()
{
    install_function(Q("%DEFTLV"), 4, 4, L"C_Zdeftlv");

    install_function(Q("EXT:MAKE-WEAK-VECTOR"), 2, 2,  L"make_weak_vector");

    install_function(Qlock_latch,   2, 2, L"lock_latch");
    install_function(Qunlock_latch, 1, 1, L"unlock_latch");

    install_macro_expander(Qwith_latch, L"expand_with_latch");

    // for debug
    install_function(Q("BACKTRACE"), 0, 0, L"backtrace");
    install_function(Q("GET-FRAME"), 1, 1, L"get_frame");

    // Genesis
    install_function(Q("COMMAND-LOOP"), 0, 0, L"command_loop");
    install_function(Qstart_application, 1, 1, L"genesis_toplevel");
} // Initializer::init_50_Extensions

} // Genesis
