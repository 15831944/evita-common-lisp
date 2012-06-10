#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - verify
// ir/ir_verify.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_verify.cpp#5 $
//

#include "./ir_function.h"

#include "../cm/cm_fns.h"
#include "../cm/cm_module.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// BBlock::Verify
//
bool
BBlock::Verify() const
{
    bool fCorrect = true;

    foreach (BBlock::EnumInsn, oEnum, const_cast<BBlock*>(this))
    {
        if (! oEnum.Get()->Verify())
        {
            fCorrect = false;
        }
    } // for each bblock

    if (! HasInsn())
    {
        html_log_format(0, L"~S has no instruction.~:%", this);
        fCorrect = false;
    }
    else if (! GetLastInsn()->HasTerminalAttr())
    {
        html_log_format(0, L"~S doesn't end with last instruction.~:%", this);
        fCorrect = false;
    }

    if (IsExitBBlock())
    {
        foreach (BBlock::EnumInEdge, oEnum, this)
        {
            Edge* pEdge = oEnum.Get();
            switch (pEdge->GetKind())
            {
            case CfgEdge::Kind_Pseudo:
            case CfgEdge::Kind_Exit:
                break;
            default:
                html_log_format(0,
                    L"Exit block ~S has non-exit edge.~:%", this );
                fCorrect = false;
                break;
            } // switch
        } // for each in-edge

        if (HasOutEdge())
        {
            html_log_format(0,
                L"Exit block ~S has an out-edge.~:%", this );
            fCorrect = false;
        }
    } // if

    return fCorrect;
} // BBlock::Verify


//////////////////////////////////////////////////////////////////////
//
// Function::Verify
//
bool
Function::Verify() const
{
    bool fCorrect = true;

    foreach (Function::EnumBBlock, oEnum, const_cast<Function*>(this))
    {
        if (! oEnum.Get()->Verify())
        {
            fCorrect = false;
        }
    } // for each bblock

    return fCorrect;
} // Function::Verify


//////////////////////////////////////////////////////////////////////
//
// Function::Verify
//
bool Instruction::Verify() const
{
    if (NULL != m_pOutput->GetExtension<void*>())
    {
        html_log_format(0, L"~S ~S: output has extension.~:%",
            GetBBlock(),
            this );
        return false;
    }

    if (Obj_Void != m_pOutput)
    {
        if (NULL == m_pOutput->GetDfn())
        {
            Register* pRd = GetRd();

            if (NULL == pRd || pRd->IsSSA())
            {
                // Non-SSA register may not be associated to instruction.
                html_log_format(0, L"~S ~S: m_pOutput->GetDfn()==NULL~:%",
                    GetBBlock(),
                    this );
                return false;
            }
        }
    } // if

    if (HasTerminalAttr())
    {
        if (GetBBlock()->GetLastInsn() != this)
        {
            html_log_format(0, L"~S ~S: not the last instruction.~:%",
                GetBBlock(),
                this );
            return false;
        }
    } // if

    return true;
} // Instruction::Verify


// EntryInsn::Verify
bool EntryInsn::Verify() const
{
    if (! Instruction::Verify()) return false;

    if (GetLx() != Knone && GetLx() != t)
    {
        html_log_format(0, L"~S ~S: Bad operand ~S.~:%",
            GetBBlock(),
            this,
            GetSx() );
        return false;
    }

    return true;
} // EntryInsn::Verify


// PrologueInsn::Verify
bool PrologueInsn::Verify() const
{
    if (! Instruction::Verify()) return false;

    if (GetLy() != Knone &&
        GetLy() != Kstack &&
        GetLy() != Kheap )
    {
        html_log_format(0, L"~S:~S: Bad operand ~S.~:%",
            GetBBlock(),
            this,
            GetSy() );
        return false;
    }

    return true;
} // PrologueInsn::Verify


// PhiInsn::Verify
//  o PHI instruction must be placed before other instructions.
bool PhiInsn::Verify() const
{
    if (! Instruction::Verify()) return false;
    if (! (GetBBlock()->GetFirstInsn() == this || GetPrev()->Is<PhiInsn>()))
    {
        html_log_format(0, L"~S:~S can't be placed before PHI.~:%",
            GetBBlock(), GetPrev() );
        return false;
    }
    return true;
} // PhiInsn::Verify


// ProjectInsn::Verify
bool ProjectInsn::Verify() const
{
    if (! Instruction::Verify()) return false;
    if (NULL == GetVx())
    {
        html_log_format(0, L"~S:~S: Second Operand must be %vx.~:%",
            GetBBlock(), this );
        return false;
    }
    return true;
} // ProjectInsn::Verify


//////////////////////////////////////////////////////////////////////
//
// cm_verity
//
bool
cm_verify(Module* pModule)
{
    bool fCorrect = true;
    foreach (Module::EnumFunction, oEnum, pModule)
    {
        if (! oEnum.Get()->Verify())
        {
            fCorrect = false;
        }
    } // for each function
    return fCorrect;
} // cm_verify

} // Compiler
