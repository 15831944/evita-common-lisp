#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - OPENBIND instruction
// ir/instruction/ir_insn_OPENBIND.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_OPENBLOCK.cpp#3 $
//
#include "./ir_instruction.h"
//#include "./ir_defs.h"

//#include "./ir_bblock.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// OpenBlockInsn::IsUseless
//
bool
OpenBlockInsn::IsUseless() const
{
    foreach (Output::EnumUseSite, oEnum, GetOutput())
    {
        if (! oEnum.Get()->GetInstruction()->Is<CloseInsn>())
        {
            return false;
        }
    } // for each use site
    return true;
} // OpenBlockInsn::IsUseless

} // Compiler
