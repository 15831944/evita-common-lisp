#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - minilisp - type system - 04 values
// compiler/ty/ty_04_values.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ty/ty_04_values.cpp#4 $
//
#include "./ty_defs.h"

namespace Compiler
{

// BUGBUG: NYI: keyword types.
class tyConsValues
{
    protected: Val m_reqs;
    protected: Val m_opts;
    protected: Val m_rest;

    public: tyConsValues() :
        m_reqs(nil),
        m_opts(nil),
        m_rest(nil) {}

    public: Val Unparse()
    {
        Val ty = nil;
        if (nil != m_rest)
        {
            ty = list(QArest, m_rest);
        }

        if (nil != m_opts)
        {
            ty = nconc(cons(QAoptional, nreverse_list(m_opts)), m_rest);
        }

        ty = nconc(nreverse_list(m_reqs), ty);
        if (nil != ty && nil == cdr(ty))
        {
            return first(ty);
        }
        else
        {
            return cons(Qvalues, ty);
        }
    } // Unparse

    public: Val AddReq(Val ty)
        { ASSERT(nil != ty); m_reqs = cons(ty, m_reqs); return ty; }

    public: Val AddOpt(Val ty)
        { ASSERT(nil != ty); m_opts = cons(ty, m_opts); return ty; }

    public: Val AddRest(Val ty)
        { ASSERT(nil == m_rest); ASSERT(nil != ty); m_rest = ty; return ty; }
}; // tyConsValues

//////////////////////////////////////////////////////////////////////
//
// ty_and_valeus_values
//
//      (values A B) and (values &rest C)       => (values A&C B&C)
//      (values A B) and (values &optional C)   => (values A&C)
//
Ty ty_and_values_values(Val ty1, Val ty2)
{
    tyIterator oIter1(ty1);
    tyIterator oIter2(ty2);

    tyConsValues oTy3;

    for (;;)
    {
        tyIterator::State eState1 = oIter1.Next();
        tyIterator::State eState2 = oIter2.Next();

        if (tyIterator::State_End == eState1 &&
            tyIterator::State_End == eState2 )
        {
            // There is no component in both ty1 and ty2.
            break;
        }

        Val ty3 = ty_and(oIter1.GetTy(), oIter2.GetTy());
        if (nil == ty3)
        {
            return nil;
        }

        if (tyIterator::State_Req == eState1 ||
            tyIterator::State_Req == eState2 )
        {
            oTy3.AddReq(ty3);
        }
        else if (tyIterator::State_Opt == eState1 ||
                 tyIterator::State_Opt == eState2 )
        {
            oTy3.AddOpt(ty3);
        }
        else if (tyIterator::State_Rest == eState1 &&
                 tyIterator::State_Rest == eState2 )
        {
            oTy3.AddRest(ty3);
            break;
        }
        else
        {
            break;
        }
    } // for

    return oTy3.Unparse();
} // ty_and_values_values

} // Compiler
