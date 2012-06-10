#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Closure Finalizer
// compiler/cg/cg_upvar.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_bool.cpp#1 $
//
// Description:
//  This file contains pass which makes sure bool output is just before
//  user.
//
#include "./cg_defs.h"

#include "../ir/ir_dfa.h"
#include "../ir/ir_pass.h"


namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// Pass Ensure Bool
//
class PassEnsureBool : public FunctionPass
{
    public: PassEnsureBool() : FunctionPass(L"CG-BOOL") {}

    virtual void process_function(Function* pFun)
    {
        html_log_format(2, L"<h2>process ~S</h2>~%", pFun);

        foreach (Function::EnumBBlock, oEnumBB, pFun)
        {
            BBlock* pBBlock = oEnumBB.Get();
            BBlock::EnumInsn oEnum(pBBlock);
            while (! oEnum.AtEnd())
            {
                Instruction* pInsn = oEnum.Get();
                    oEnum.Next();

                Bool* pBd = pInsn->GetBd();
                if (NULL != pBd) process_bool(pInsn);
            } // for each insn
        } // for each bblock
    } // process_function

    static void process_bool(Instruction* pInsn)
    {
        Bool::EnumUseSite oEnum(pInsn->GetBd());
        while (! oEnum.AtEnd())
        {
            OperandBox* pBox = oEnum.Get();
            Instruction* pUser = pBox->GetInstruction();
                oEnum.Next();

            if (pInsn->GetNext() == pUser) continue;

            Bool* pBx = ir_insert_insn(pInsn->Clone(), pUser)->GetBd();
            pBox->Replace(pBx);
        } // for each user

        unless (pInsn->GetBd()->HasUseSite())
        {
            ir_remove_insn(pInsn);
        }
    } // process_bool
}; // PassEnsureBool

} // namespace

//////////////////////////////////////////////////////////////////////
//
// cg_pass_ensure_bool
//
void cg_pass_ensure_bool()
{
    PassEnsureBool oPass;
        oPass.Run();
} // cg_pass_ensure_bool

} // Compiler
