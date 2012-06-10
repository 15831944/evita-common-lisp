#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - Build 04 Types and Classes
// boot/bt_build_04_typs.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/boot/bt_build_04_type.cpp#24 $
//
#include "../mini/mini_lisp.h"

namespace Boot
{

using namespace MiniLisp;

Val make_bignum(const Bigit*, uint);


#define defcvar(mp_name, mp_val) \
    Val mp_name; Defobject(mp_name, mp_val)


Val ty_sequence_index;


// deftype
Val deftype(Val name, Val typespec)
{
    Environment* pEnv = VAR(Aruntime_environmentA)->Decode<Environment>();
    setf_gethash(typespec, name, pEnv->m_types);
    return name;
} // deftype


// make_ty_signed_byte
static Val make_ty_signed_byte(uint n)
{
    Bigit rgLower[3], rgUpper[3];
    uint cLower, cUpper;

    if (n <= sizeof(Bigit) * 8)
    {
        cLower = 1;
        rgLower[0] = static_cast<SignedBigit>(-1) << (n-1);

        cUpper = 1;
        rgUpper[0] = (static_cast<SignedBigit>(1) << (n-1)) - 1;
    }
    #if SIZEOF_VAL == 4
    else if (n == 64)
    {
        cLower = 2;
        rgLower[0] = 0;
        rgLower[1] = 1u << 31;

        cUpper = 2;
        rgUpper[0] = static_cast<Bigit>(-1);
        rgUpper[1] = (1u << 31) - 1;
    }
    #endif // SIZEOF_VAL == 4
    else
    {
        CAN_NOT_HAPPEN();
    }

    return list(
        Qinteger,
        make_bignum(rgLower, cLower),
        make_bignum(rgUpper, cUpper) );
} // make_ty_signed_byte


// make_ty_unsigned_byte
static Val make_ty_unsigned_byte(uint n)
{
    Bigit rgBigit[3];
    uint cBigits;

    if (n < sizeof(Bigit) * 8)
    {
        cBigits = 1;
        rgBigit[0] = (static_cast<Bigit>(1) << n) - 1;
    }
    #if SIZEOF_VAL == 4
    else if (n == 32)
    {
        cBigits = 2;
        rgBigit[0] = static_cast<Bigit>(-1);
        rgBigit[1] = 0;
    }
    else if (n == 64)
    {
        cBigits = 3;
        rgBigit[0] = static_cast<Bigit>(-1);
        rgBigit[1] = static_cast<Bigit>(-1);
        rgBigit[2] = 0;
    }
    #elif SIZEOF_VAL == 8
    else if (n == 64)
    {
        cBigits = 2;
        rgBigit[0] = static_cast<Bigit>(-1);
        rgBigit[1] = 0;
    }
    #endif // SIZEOF_VAL
    else
    {
        CAN_NOT_HAPPEN();
    }

    return list(Qinteger, Fixnum::Encode(0), make_bignum(rgBigit, cBigits));
} // make_ty_unsigned_byte


// build_04_Types
void build_04_Types()
{
    Val zero = Fixnum::Encode(0);
    Val one  = Fixnum::Encode(1);

    // bounding index
    // bounding index designator
    // extended function designator
    // external format designator
    // file position designator
    // interval designator
    // list designator
    // logical host desginator
    // pathname designator
    // readtable designator
    // restart designator
    // spreadable argument list designator
    // stream designator
    // stream variable designator

    // Known Types
    defobject(ty_eql_0,   list(Qeql, Fixnum::Encode(0)));
    defobject(ty_eql_1,   list(Qeql, Fixnum::Encode(1)));
    defobject(ty_eql_M1,  list(Qeql, Fixnum::Encode(-1)));

    defobject(ty_fixnum_, list(Qinteger,
        Fixnum::Encode(Fixnum::MostNegative),
        Fixnum::Encode(Fixnum::MostPositive) ) );


    // for array type specifier.
    defobject(list_QA,    list(Q("*")));

    Defobject(
        ty_sequence_index,
        list(Qinteger, zero, Fixnum::Encode(Array::TotalSizeLimit-1)) );

    // [A]
    deftype(Q("ARRAY-RANK"),
        list(Qinteger, zero, Fixnum::Encode(Array::RankLimit-1)) );

    deftype(Q("ARRAY-TOTAL-SIZE"), ty_sequence_index);

    deftype(Q("ATOM"), list(Qnot, Qcons));

    // [B]
    deftype(Q("BASE-CHAR"),   Qcharacter);
    deftype(Q("BASE-STRING"), Qstring);
    deftype(Q("BIT"), list(Qinteger, zero, Fixnum::Encode(1)) );
    deftype(Q("BOOLEAN"), list(Qmember, t, nil));
    deftype(Q("BYTESPEC"), Qcons);

    // [C]
    {
        Val ft = list(Qfunction, list(t, Qenvironment), t);
        deftype(Q("COMPILER-MACRO-FUNCTION"), ft);
        deftype(Q("MACRO-FUNCTION"), ft);
    }

    deftype(Q("EXT:CHARACTER-CODE"),
        list(Qinteger, Fixnum::Encode(0), Fixnum::Encode(0xffff)) );

    deftype(Q("EXT:CHARACTER-DESIGNATOR"),
        list(Qor, Qcharacter, Qstring) );

    deftype(Q("EXT:CLASS-DESIGNATOR"),
        list(Qor, Q("CLASS"), Qsymbol) );

    deftype(Q("EXT:CONDITION-DESIGNATOR"),
        list(Qor, Qcondition, Qstring, Qsymbol) );

    // [E]
    deftype(Q("EXT:EXTERNAL-FORMAT"), t);

    // [F]
    deftype(Q("FORM"), t);
    deftype(Q("EXT:FORMAT-CONTROL"), list(Qor, Qstring, Qfunction));
    deftype(Q("EXT:FUNCTION-DESIGNATOR"), list(Qor, Qfunction, Qsymbol));

    deftype(
        Q("EXT:FUNCTION-NAME"),
        list(Qor,
                Qsymbol,
                list(Qcons,
                        list(Qeql,  Q("SETF")),
                        list(Qcons, Qsymbol, Qnull) )) );

    // [I]
    deftype(Q("INPUT-STREAM-DESIGNATOR"),
        list(Qor, Qstream, Qnull, ty_eql_t) );

    // [K]
    deftype(Q("KEYWORD"), list(Q("SATISFIES"), Q("KEYWORDP")));

    // [O]
    deftype(Q("OUTPUT-STREAM-DESIGNATOR"),
        list(Qor, Qstream, Qnull, ty_eql_t) );

    // [P]
    deftype(Q("EXT:PACKAGE-DESIGNATOR"),
        list(Qor, Qpackage, Q("EXT:STRING-DESIGNATOR")) );

    deftype(Q("EXT:PATHNAME-DESIGNATOR"),
        list(Qor, Qpathname, Qstring, Qstream) );

    deftype(Q("EXT:PATHNAME-HOST-DESIGNATOR"),
        list(Qstring, Qnull, Q(":UNSPECIFIC"), Q("BASIC-HOST")) );

    deftype(Q("POSITIVE-FIXNUM"),
        list(Qinteger, one, Fixnum::Encode(Fixnum::MostPositive)) );

    deftype(Q("PROPER-LIST"), Qlist);

    // [R]
    deftype(Q("RESTART-DESIGNATOR"),
        list(Qor, Q("RESTART"), list(Qand, Qsymbol, list(Qnot, Qnull))) );

    // [S]
    deftype(Q("SEQUENCE"), list(Qor, Qlist, Qvector));

    deftype(Q("EXT:SEQUENCE-END"),
        list(Qor, ty_sequence_index, Qnull) );

    deftype(Q("EXT:SEQUENCE-INDEX"), ty_sequence_index);

    defcvar(ty_simple_array_A_1A,
        list(Q("SIMPLE-ARRAY"), Q("*"), list(Q("*"))) );

    deftype(Q("SIMPLE-BASE-STRING"),   Qsimple_string);

    deftype(Q("SIGNED-BYTE"), Qinteger);

    defcvar(ty_signed_byte_8_,  make_ty_signed_byte( 8));
    defcvar(ty_signed_byte_16_, make_ty_signed_byte(16));
    defcvar(ty_signed_byte_32_, make_ty_signed_byte(32));
    defcvar(ty_signed_byte_64_, make_ty_signed_byte(64));

    defobject(val_i32_min, second(ty_signed_byte_32_));
    defobject(val_i32_max,  third(ty_signed_byte_32_));

    deftype(Q("STANDARD-CHAR-P"),
        list(Qand,
            Qcharacter,
            list(Q("SATISFIES"), Q("STANDARD-CHAR-P")) ) );

    deftype(Q("EXT:STREAM-DESIGNATOR"),
        list(Qor, Qstream, Qnull, list(Qeql, t)) );

    deftype(Q("EXT:STRING-DESIGNATOR"),
        list(Qor, Qstring, Qsymbol, Qcharacter) );

    // [T]
    deftype(Q("TEST-1-FUNCTION"),
        list(Qor, Qsymbol, list(Qfunction, list(t, QArest, t), t)) );

    deftype(Q("TEST-2-FUNCTION"),
        list(Qor, Qsymbol, list(Qfunction, list(t, t, QArest, t), t)) );

    deftype(Q("TIME-ZONE"),
        list(Qrational, Fixnum::Encode(-24), Fixnum::Encode(24)) );

    deftype(Q("EXT:TYPE-SPECIFIER"),
        list(Qor, Qsymbol, Qcons, Qclass) );

    // [U]
    defcvar(ty_unsigned_byte_,    list(Qinteger, zero, Q("*")));
    defcvar(ty_unsigned_byte_8_,  make_ty_unsigned_byte( 8));
    defcvar(ty_unsigned_byte_16_, make_ty_unsigned_byte(16));
    defcvar(ty_unsigned_byte_32_, make_ty_unsigned_byte(32));
    defcvar(ty_unsigned_byte_64_, make_ty_unsigned_byte(64));

    defobject(val_u32_min, second(ty_unsigned_byte_32_));
    defobject(val_u32_max,  third(ty_unsigned_byte_32_));

    deftype(Q("UNSIGNED-BYTE"), ty_unsigned_byte_);
} // build_04_Types

} // Boot
