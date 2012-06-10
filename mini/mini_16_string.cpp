#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 3 Evaluation and Compilation
// genesis/geneis_lisp.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_16_string.cpp#7 $
//
#include "./mini_lisp.h"

namespace CommonLisp
{

// make_string
Val make_string(
    LPCWSTR         pwchString,
    size_t          cwchString )
{
    Val str = make_string(Fixnum::Encode(cwchString));

    ::CopyMemory(
        str->Decode<SimpleString>()->m_rgwchElement,
        pwchString,
        cwchString * sizeof(char16) );

    return str;
} // make_string


// schar
Val schar(Val s, Val i)
{
    ASSERT(cmp_xx(i, length(s)) < 0);
    SimpleString* pString = s->Decode<SimpleString>();
    return Character::Encode(pString->GetElements()[Fixnum::Decode_(i)]);
} // schar


// setf schar
Val setf_schar(Val c, Val s, Val i)
{
    ASSERT(cmp_xx(i, length(s)) < 0);
    SimpleString* pString = s->Decode<SimpleString>();

    pString->GetElements()[Fixnum::Decode_(i)] = 
        Character::ToCode(c);

    return c;
} // setf_schar

} // CommonLisp

namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// Get Hash Code
//
Val hash_string(Val str)
{
    const SimpleString* pString = str->Decode<SimpleString>();
    const char16* pwchStart = pString->m_rgwchElement;
    const char16* pwchEnd   = pwchStart + Fixnum::Decode_(pString->m_length);

    UInt nHashCode = 0;
    for (
        const char16* pwchRunner = pwchStart;
        pwchRunner < pwchEnd;
        pwchRunner++ )
    {
        nHashCode ^= *pwchRunner;
        nHashCode = ((nHashCode & 0xFFFF) << 8) | (nHashCode >> 16);
    } // for

    nHashCode %= HashTable::MaxHashCode;

    return Fixnum::Encode(static_cast<Int>(nHashCode));
} // hash_string


// Allocate simple string
Val allocate_string(Val length)
{
    if (Fixnum::Encode(0) == length) return QQnull_string;
    Val str = MiniThread::Get()->AllocBinVec(
        CLASSD_simple_string, 
        length );

    return str;
} // allocate_string


//////////////////////////////////////////////////////////////////////
//
// Compare
//
int
string_cs_cmp(Val str1, Val str2)
{
    const SimpleString* pString1 = str1->Decode<SimpleString>();
    const char16* pwchStart1 = pString1->m_rgwchElement;
    const char16* pwchEnd1   =
        pwchStart1 + Fixnum::Decode_(pString1->m_length);

    const SimpleString* pString2 = str2->Decode<SimpleString>();
    const char16* pwchStart2 = pString2->m_rgwchElement;
    const char16* pwchEnd2   =
        pwchStart2 + Fixnum::Decode_(pString2->m_length);

    const char16* pwchRunner2 = pwchStart2;
    for (
        const char16* pwchRunner1 = pwchStart1;
        pwchRunner1 < pwchEnd1;
        pwchRunner1++ )
    {
        if (pwchEnd2 == pwchRunner2)
        {
            // str is shorter than this.
            return 1;
        }

        int iDiff = *pwchRunner1 - *pwchRunner2;
        if (0 != iDiff)
        {
            return iDiff;
        }

        pwchRunner2++;
    } // for

    if (pwchEnd2 == pwchRunner2)
    {
        return 0;
    }

    // str is longer than this.
    return -1;
} // string_cs_cmp


//////////////////////////////////////////////////////////////////////
//
// Compare
//
int
string_ci_cmp(Val str1, Val str2)
{
    const SimpleString* pString1 = str1->Decode<SimpleString>();
    const char16* pwchStart1 = pString1->m_rgwchElement;
    const char16* pwchEnd1   =
        pwchStart1 + Fixnum::Decode_(pString1->m_length);

    const SimpleString* pString2 = str2->Decode<SimpleString>();
    const char16* pwchStart2 = pString2->m_rgwchElement;
    const char16* pwchEnd2   =
        pwchStart2 + Fixnum::Decode_(pString2->m_length);

    const char16* pwchRunner2 = pwchStart2;
    for (
        const char16* pwchRunner1 = pwchStart1;
        pwchRunner1 < pwchEnd1;
        pwchRunner1++ )
    {
        if (pwchEnd2 == pwchRunner2)
        {
            // str is shorter than this.
            return 1;
        }

        int iDiff = *pwchRunner1 - *pwchRunner2;
        if (0 != iDiff)
        {
            iDiff = static_cast<int>(
                char_upcase(Character::Encode(*pwchRunner1)) -
                char_upcase(Character::Encode(*pwchRunner2)) );

            if (0 != iDiff) return iDiff;
        } // if

        pwchRunner2++;
    } // for

    if (pwchEnd2 == pwchRunner2)
    {
        return 0;
    }

    // str is longer than this.
    return -1;
} // string_ci_cmp

// string_data
Val string_data(Val str, Val* out_offset)
{
    Val offset = Fixnum::Encode(0);
    Val runner = str;
    while (! simple_string_p(runner))
    {
        if (runner->Is<Array>())
        {
            offset = add_xx(offset, runner->Decode<Array>()->m_offset);
            runner = runner->Decode<Array>()->m_displaced_to;
        }
        else if (runner->Is<String>())
        {
            offset = add_xx(offset, runner->Decode<Vector>()->m_offset);
            runner = runner->Decode<Vector>()->m_displaced_to;
        }
        else
        {
            error(L"Broken string ~S", str);
        }
    } // while

    *out_offset = offset;

    return runner;
} // string_data


// to_string - Common Lisp function string.
Val to_string(Val x)
{
    if (simple_string_p(x)) return x;
    if (symbolp(x)) return x->Decode<Symbol>()->m_name;

    if (characterp(x))
    {
        Val s = make_string(1);
        s->Decode<SimpleString>()->GetElements()[0] =
            Character::ToCode(x);
        return s;
    }

    error(make_type_error(x, list(Qor, Qstring, Qsymbol, Qcharacter)));
} // to_string

} // MiniLisp
