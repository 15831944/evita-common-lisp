//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - ir - IR Assembler
// compiler/ir/ir_asm.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/arch/x86x64/compiler/x86x64_cg_target.h 19 2006-10-24 11:10:21 yosi $
//
#if !defined(INCLUDE_compiler_ir_ir_asm_h)
#define INCLUDE_compiler_ir_ir_asm_h

#include "./ir_bblock.h"

namespace Compiler
{

class BBlockAsm : public BBlock
{
    public: void        Branch(Bool*, BBlock*, BBlock*);
    public: void        Jump(BBlock*);
    public: void        Load(Register*, Register*);
    public: PhiInsn*    Phi(Ty, Output*);
    public: void        Select(Ty, Register*, Bool*, Operand*, Operand*);
    public: void        Slot(Ty, Register*, Val, Val, Register*);
    public: void        Sub(Ty, Register*, Register*, Int);
}; // BBlockAsm

} // Compiler

#endif //!defined(INCLUDE_compiler_ir_ir_asm_h)
