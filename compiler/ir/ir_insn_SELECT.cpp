#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction cmp
// ir/instruction/ir_insn_cmp.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_SELECT.cpp#4 $
//
#include "./ir_instruction.h"

#include "./ir_fns.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// SelectInsn::Simplify
//
bool SelectInsn::Simplify()
{
    bool fChanged = simplifyBx();
    return Instruction::Simplify() || fChanged;
} // SelectInsn::Simplify


//////////////////////////////////////////////////////////////////////
//
// SelectInsn::SimplifyOutputAux
//
Operand* SelectInsn::SimplifyOutputAux() const
{
    if (GetSx() == Bool_True)
    {
        return GetSy();
    }

    if (GetSx() == Bool_False)
    {
        return GetSz();
    }

    if (GetSx() == GetSy())
    {
        return GetSx();
    }

    return GetOutput();
} // SelectInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// SelectInsn::UpdateTy
//
bool SelectInsn::UpdateTy()
{
    Ty ty = ty_or(GetSy()->GetTy(), GetSz()->GetTy());
    if (ty_equal(ty, m_ty)) return false;
    m_ty = ty;
    return true;
} // SelectInsn::UpdateTy

} // Compiler
