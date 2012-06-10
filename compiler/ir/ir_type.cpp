#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - operand
// ir_operand.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_type.cpp#3 $
//
#include "./ir_defs.h"
#include "./ir_fns.h"
#include "./ir_type.h"

namespace Compiler
{

// ir_get_ty_arity
void ir_get_ty_arity(Ty ty, Arity* out_pArity)
{
    tyIterator oIter(ty);

    out_pArity->m_nMin = 0;
    out_pArity->m_nMax = 0;
    out_pArity->m_fRest = false;

    oIter.Next();

    for (;;)
    {
        switch (oIter.GetState())
        {
        case tyIterator::State_Atom:
            out_pArity->m_nMin = 1;
            out_pArity->m_nMax = 1;
            return;

        case tyIterator::State_End:
            return;

        case tyIterator::State_Req:
            do
            {
                out_pArity->m_nMin += 1;
                out_pArity->m_nMax += 1;
            } while (tyIterator::State_Req == oIter.Next());
            break;

        case tyIterator::State_Opt:
            do
            {
                out_pArity->m_nMax += 1;
            } while (tyIterator::State_Opt == oIter.Next());
            break;

        case tyIterator::State_Rest:
            out_pArity->m_fRest = true;
            return;

        default:
            CAN_NOT_HAPPEN();
        } // switch state
    } // for
} // ir_get_ty_arity

} // Compiler
