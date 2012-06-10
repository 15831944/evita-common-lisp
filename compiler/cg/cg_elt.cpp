#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Expand ELT and SLOT
// compiler/cg/cg_elt.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_upvar.cpp#6 $
//
#include "./cg_defs.h"

#include "../ir/ir_pass.h"

namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// PassExpandEltAndSlot
//
class PassExpandEltAndSlot : public FunctionPass
{
    public: PassExpandEltAndSlot() : FunctionPass(L"CG-ELTSLOT") {}

    virtual void process_function(Function* pFun)
    {
        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* pBBlock = oEnum.Get();
            foreach (BBlock::EnumInsn, oEnum, pBBlock)
            {
                Instruction* pInsn = oEnum.Get();
                switch (pInsn->GetOpcode())
                {
                case IrOp_ELT: process_ELT(pInsn); break;
                } // switch opcode
            } // for each insn
        } // for each bblock
    } // process_function

    static void process_ELT(Instruction* pElt)
    {
        Ty ty = pElt->GetSx()->GetTy();

        uint ofs = 0;
        if (ty_array == ty)
        {
            ofs = offsetof(Array, mv_dimension) - Array::Tag;
        }
        else
        {
            ofs = offsetof(SimpleVector, mv_element) - SimpleVector::Tag;
        }

        if (0 != ofs)
        {
            Register* pRy = new Register();

            ir_insert_insn(
                new AddInsn(ty_int, pRy, pElt->GetSy(), NewInteger(ofs)),
                pElt );

            pElt->GetOperandBox(1)->Replace(pRy);
        }
    } // process_ELT
}; // PassExpandEltAndSlot

} // namespace

//////////////////////////////////////////////////////////////////////
//
// cg_pass_expand_elt_and_slot
//
void cg_pass_expand_elt_and_slot()
{
    PassExpandEltAndSlot oPass;
        oPass.Run();
} // cg_pass_expand_elt_and_slot

} // Compiler
