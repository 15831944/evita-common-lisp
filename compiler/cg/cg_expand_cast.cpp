#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Expand RUNTIMECAST instruction
// compiler/cg/cg_expand_cast.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_expand_cast.cpp#8 $
//
// Description:
//  This pass expands RUNTIMECAST instructions into TYPEP, BRANCH and
//  TRAPIF.
//
#include "./cg_defs.h"

#include "../ir/ir_defs.h"
#include "../ir/ir_function.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

namespace
{

// is_foreign_type
//  We don't want to TYPEP for foreign-class.
static bool is_foreign_type(Val ty)
{
    if (symbolp(ty))
    {
        Val klass = find_class(ty, nil, TLV(AenvironmentA));
        if (class_of(klass) == CLASS_foreign_class)
        {
            return true;
        }
    } // if symbol

    return false;
} // is_foreign_type


//////////////////////////////////////////////////////////////////////
//
// ExpandCastPass
//
class ExpandCastPass : public FunctionPass
{
    public: ExpandCastPass() : FunctionPass(L"CG-CAST") {}

    protected: virtual void process_function(Function* pFun)
    {
        html_log_format(2, L"<h2>Process ~S</h2>~%", pFun);

        bool fChanged = false;
        WorkList_<Instruction> oTasks;
        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* pBBlock = oEnum.Get();
            foreach (BBlock::EnumInsn, oEnum, pBBlock)
            {
                Instruction* pInsn = oEnum.Get();
                switch (pInsn->GetOpcode())
                {
                case IrOp_RUNTIMECAST:
                    oTasks.Push(pInsn);
                    fChanged = true;
                    break;
                } // switch opcode
            } // for each insn
        } // for each bblock

        while (! oTasks.IsEmpty())
        {
            Instruction* pCast = oTasks.Pop();
            process_RUNTIMECAST(pCast);
            ir_remove_insn(pCast);
        } // for each task

        if (fChanged) ir_remove_useless_instructions(pFun);
    } // process_function

    // process_RUNTIMECAST
    void process_RUNTIMECAST(Instruction* pInsn)
    {
        RuntimeCastInsn* pCast = pInsn->StaticCast<RuntimeCastInsn>();

        Ty ty = ty_expand(pCast->GetTy());
        if (pCast->HasAttr(RuntimeCastInsn::Attr_Simple))
        {
            ty = weaken_type(ty);
        }

        if (ty_t == ty ||
            NULL == pCast->GetRx() ||
            pCast->HasAttr(RuntimeCastInsn::Attr_Nop) ||
            is_foreign_type(ty) )
        {
            ir_replace_all_users(pCast->GetSx(), pCast->GetRd());
            return;
        }

        Function* pFun = pInsn->GetBBlock()->GetParent();

        BBlock* pCont  = ir_split_bblock_after(pCast);
        BBlock* pError = pFun->AppendBBlock(NewBBlock());

        // Error block
        {
            Instruction* pRefInsn = pError->AppendInsn(
                new UnreachableInsn() );

            Values* pVz = new Values;

            ir_insert_insn(
                new ValuesInsn(pVz,
                    pCast->GetRx(), NewLiteral(pCast->GetTy()) ),
                pRefInsn );

            TrapIfInsn* pTrapIf = new TrapIfInsn(Bool_True, Qtype_error, pVz);
                pTrapIf->SetFrame(pCast->GetFrame());

            ir_insert_insn(pTrapIf, pRefInsn);

            foreach (Frame::Enum, oEnum, pCast->GetFrame())
            {
                Frame* pFrame = oEnum.Get();
                if (pFrame->GetDfn() == NULL)  continue;
                if (pFrame->GetOwner() != pFun) continue;
                ir_insert_insn(new UseInsn(pFrame), pRefInsn);
            } // for each frame
        } // error block

        Bool* pBx = new Bool();
        Register* pRx = pCast->GetRx();

        ir_insert_insn(
            new TypepInsn(pBx, pRx, ty),
            pCast );

        ir_insert_insn(
            new BranchInsn(
                    pBx,
                    pCont, pError ),
            pCast );

        ir_insert_insn(
            new SigmaInsn(pCast->GetTy(), pCast->GetRd(), pCast->GetRx()),
            pCont->GetFirstInsn() );
    } // process_RUNTIMECAST

    // weaken_type
    static Ty weaken_type(Ty ty)
    {
        ty = ty_expand(ty);
        if (! consp(ty)) return ty;

        Val op = first(ty);
        if (Qinteger == op) return Qinteger;
        return ty;
    } // weaken_type
}; // ExpandCastPass

} // namespace

//////////////////////////////////////////////////////////////////////
//
// Code Generator Entry Point
//
void
cg_pass_expand_cast()
{
    ExpandCastPass oPass;
    oPass.Run();
} // cg_pass_expand_cast

} // Compiler
