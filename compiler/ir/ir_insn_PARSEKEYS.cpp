#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction CHECKKEYS
// ir/instruction/ir_insn_CHECKKEYS.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_PARSEKEYS.cpp#3 $
//
#include "./ir_instruction.h"

#include "./ir_fns.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// KeySuppliedInsn::KeySuppliedInsn
//
KeySuppliedInsn::KeySuppliedInsn(
    Bool*       pBd,
    Register*   pRx,
    Val         key ) :
        Two_Operands_Instruction(pRx, NewLiteral(key), ty_bool, pBd) {}

//////////////////////////////////////////////////////////////////////
//
// KeyValInsn::KeyValInsn
//
KeyValInsn::KeyValInsn(
    Register*   pRd,
    Values*     pVx,
    Val         key ) :
        Two_Operands_Instruction(pVx, NewLiteral(key), ty_t, pRd) {}


//////////////////////////////////////////////////////////////////////
//
// ParseKeysInsn::ParseKeysInsn
//
ParseKeysInsn::ParseKeysInsn(
    Values*     pVd,
    Register*   pRx,
    Val         allow,
    Val         keys ) :
    Three_Operands_Instruction(pRx, NewLiteral(allow), NewLiteral(keys))
{
    ASSERT(NULL != pVd);
    ASSERT(NULL != pRx);
    ASSERT(QAkey == allow || QAallow_other_keys == allow);
    m_ty = ty_values_rest_t;
    m_pOutput = pVd;
} // ParseKeysInsn::ParseKeysInsn

} // Compiler
