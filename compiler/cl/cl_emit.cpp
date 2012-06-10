#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - Emitter
// cl_emit.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_emit.cpp#41 $
//
#include "./cl_defs.h"
#include "../cm/cm_base.h"
#include "../ir/ir_instruction.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ClParser::emitBind
//
// Description:
//  Emits variable binding and activates it.
// Note:
//  We can't use this for LET. We bind variables of LET in parallel.
//
OpenBindInsn*
ClParser::emitBind(
    VarDef*         pVarDef,
    Operand*        pSx,
    OpenBindInsn*   pOpenBind )
{
    if (! pVarDef->IsSpecial())
    {
        Variable* pVar = pVarDef->GetVar();
            pVar->SetTy(pVarDef->GetTy());

        emitInsn(new VarDefInsn(new Register(), pVar, pSx));
    }
    else
    {
        if (NULL == pOpenBind)
        {
            BindFrame* pFrame = new BindFrame(GetFunction());

            pOpenBind = new OpenBindInsn(pFrame);
            emitInsn(pOpenBind);

            pushFrame(pFrame);
        } // if no OPENBIND

        // BUGBUG: The first argument to AddBind  must be value_cell or
        // tlv_record instead of symbol.
        Val cell = intern_value_cell(pVarDef->GetName());
        pOpenBind->AddBind(cell, pSx);
    } // if

    activateVarDcl(pVarDef);

    return pOpenBind;
} // ClParser::emitBind


//////////////////////////////////////////////////////////////////////
//
// ClParser::emitBool
//
Register* ClParser::emitBool(Bool* pBx)
{
    Register* pRd = new Register();
    emitInsn(new SelectInsn(pRd, pBx, Obj_True, Obj_Nil));
    return pRd;
} // ClParser::emitBool


//////////////////////////////////////////////////////////////////////
//
// is_setf
//
static bool is_setf(Operand* pSx)
{
    switch (pSx->GetKind())
    {
    case Operand::Kind_Literal:
        return setf_cell_p(pSx->StaticCast<Literal>()->GetDatum());

    case Operand::Kind_Function:
        // Local function must return the first argument.
        return setf_cell_p(pSx->StaticCast<Function>()->GetName());

    default:
        return false;
    } // switch operand
} // is_setf


//////////////////////////////////////////////////////////////////////
//
// ClParser::emitCall
//
Operand*
ClParser::emitCall(
    const Expect*   pExpect,
    Val             form,
    Callee*         pCallee,
    Values*         pVargs )
{
    CallInsn* pCall = new CallInsn(ty_void, Obj_Void, pCallee->m_pSx, pVargs);
        pCall->SetForm(form);
        pCall->SetFrame(m_pFrame);
        pCall->SetNotInline(pCallee->m_fNotInline);

    emitInsn(pCall);

    Ty value_ty = ty_get_function_value(pCallee->m_ty);
    Ty expected_ty = pExpect->Type;

    // BUGBUG: Should we check setf here? Arguments of setf function
    // are variable instead of literal.
    if (is_setf(pCallee->m_pSx))
    {
        value_ty = ty_get_primary(pVargs->GetTy());
    }

    if (nil == value_ty) return emitUnreachable();

    Arity oExpected;
        ir_get_ty_arity(expected_ty, &oExpected);

    if (! oExpected.IsFixed())
    {
        Arity oFunValue;
            ir_get_ty_arity(value_ty, &oFunValue);

        pCall->SetTy(value_ty);

        if (oFunValue.IsFixed() && oFunValue.GetMin() == 1)
        {
            return pCall->SetOutput(new Register());
        }
        else
        {
            // A user may be multiple-value-call or RET.
            return pCall->SetOutput(new Values());
        }
    } // if

    switch (oExpected.GetMin())
    {
    case 0:
        return Obj_Void;

    case 1:
    {
        TyValues::Enum oEnum(value_ty);

        if (oEnum.AtEnd())
        {
            // Callee returns no value.
            return emitCast(pExpect, form, Obj_Nil);
        }

        Ty ty = oEnum.Get();

        Output* pSd = NewOutput(ty);

        if (Obj_Void == pSd) pSd = new Register();

        pSd->SetForm(form);

        pCall->SetOutput(pSd);
        pCall->SetTy(ty);

        return emitCast(pExpect, form, pSd);
    } // 1

    default:
    {
        Values* pVx = new Values();
        pCall->SetOutput(pVx);
        pCall->SetTy(value_ty);

        ValuesInsn* pValues = new ValuesInsn(
            new Values(),
            Obj_Nil,
            oExpected.GetMin() );

        bool fWarnExtraValue = false;
        uint nNth = 0;
        Register* pRn = NULL;

        TyValues::Enum oEnumValTy(value_ty);
        TyValues::Enum oEnumExpTy(expected_ty);

        foreach (Instruction::EnumInput, oEnum, pValues)
        {
            Operand* pSx;

            if (oEnumValTy.AtEnd())
            {
                if (! fWarnExtraValue)
                {
                    fWarnExtraValue = true;
                    style_warn(L"Use extra value of function ~S.",
                        ir_get_callee_name(pCallee->m_pSx) );
                }

                pSx = Obj_Nil;
            }
            else
            {
                Ty val_ty = oEnumValTy.Get();

                Register* pR1 = new Register();
                    emitInsn(new ProjectInsn(val_ty, pR1, pVx, nNth));

                if (oEnumValTy.IsRequired() || 0 == nNth)
                {
                    pSx = pR1;
                }
                else
                {
                    if (NULL == pRn)
                    {
                        pRn = new Register();
                        emitInsn(new CountInsn(pRn, pVx));
                    }

                    Bool* pBx = new Bool();
                    emitInsn(new GtInsn(pBx, pRn, NewLiteral(nNth)));

                    Register* pR2 = new Register();
                    emitInsn(new SelectInsn(pR2, pBx, pR1, Obj_Nil));

                    pSx = pR2;
                }

                oEnumValTy.Next();
            } // if

            Expect oExpect(oEnumExpTy.Get());
            oEnum.GetBox()->SetOperand(emitCast(&oExpect, form, pSx));

            oEnumExpTy.Next();

            nNth += 1;
        } // for each elty

        emitInsn(pValues);

        {
            Instruction* pRefInsn = pCall->GetNext();

            while (pRefInsn->Is<ProjectInsn>())
                { pRefInsn = pRefInsn->GetNext(); }

            Values::EnumUseSite oEnum(pVx);
            while (! oEnum.AtEnd())
            {
                Instruction* pInsn = oEnum.Get()->GetInstruction();
                    oEnum.Next();

                if (pInsn != pRefInsn) ir_move_insn(pInsn, pRefInsn);
            } // for each PROJECT
        }

        return pValues->GetVd();
    } // default
    } // switch expected
} // ClParser::emitCall


Operand*
ClParser::emitCall(
    const Expect*   pExpect,
    Val             form,
    Ty              ty,
    Val             fname,
    Operand*        pSx )
{
    Values* pVy = new Values();
    emitInsn(new ValuesInsn(pVy, pSx));
    Output* pSd = NewOutput(ty);
        pSd->SetForm(form);
    emitInsn(new CallInsn(ty, pSd, NewLiteral(fname), pVy));
    return emitCast(pExpect, form, pSd);
} // ClParser::emitCall


Operand*
ClParser::emitCall(
    const Expect*   pExpect,
    Val             form,
    Ty              ty,
    Val             fname,
    Operand*        pSx,
    Operand*        pSy )
{
    Values* pVy = new Values();
    emitInsn(new ValuesInsn(pVy, pSx, pSy));
    Output* pSd = NewOutput(ty);
        pSd->SetForm(form);
    emitInsn(new CallInsn(ty, pSd, NewLiteral(fname), pVy));
    return emitCast(pExpect, form, pSd);
} // ClParser::emitCall


////////////////////////////////////////////////////////////
//
//  ClParser::emitCast
//
Operand*
ClParser::emitCast(const Expect* pExpect, Val form, Operand* pSx)
{
    if (Obj_Unreachable == pSx) return useless_form(form);

    if (pExpect->Type == ty_void) return Obj_Void;
    if (pExpect->Type == ty_unspecified)  return pSx;

    switch (pSx->GetKind())
    {
    case Operand::Kind_Function:
    case Operand::Kind_Literal:
        return pSx;

    case Operand::Kind_Register:
        break;

    default:
        warn(L"ClParser::emitCast: unexpected operand: ~D",
            Fixnum::Encode(pSx->GetKind()) );
        return pSx;
    } // switch operand

    Register* pRx = pSx->StaticCast<Register>();

    Ty expected_ty = ty_get_primary(ty_expand(pExpect->Type));

    if (Subtypep_Yes == ty_subtypep(pRx->GetTy(), expected_ty))
    {
        return pRx;
    }

    if (nil == ty_and(pRx->GetTy(), expected_ty))
    {
        if (pRx->GetTy() != ty_int)
        {
            unexpected_type(pExpect, pRx->GetTy(), form);
            return pRx;
        }
    }

#if 0
    if (! option_check_type())
    {
        pRx->GetDfn()->SetTy(expected_ty);
        return pRx;
    }
#endif

    uint rgfAttr = 0;
    switch (pExpect->Context)
    {
    case ExpectContext_Argument:
        rgfAttr |= RuntimeCastInsn::Attr_Nop;
        break;

    case ExpectContext_Callee:
        return pRx;
    } // switch context

    Register* pRd = emitRuntimeCast(expected_ty, pRx, rgfAttr);
        pRd->SetForm(form);

    return pRd;
} // ClParser::emitCast


//////////////////////////////////////////////////////////////////////
//
// ClParser::emitInsn
//
Instruction*
ClParser::emitInsn(Instruction* pInsn)
{
    return GetContext()->GetCurr()->AppendInsn(pInsn);
} // ClParser::emitInsn


////////////////////////////////////////////////////////////
//
//  ClParser::emitLinkage
//
Operand*
ClParser::emitLinkage(Operand* pSx)
{
    if (Obj_Unreachable == pSx)
    {
        return GetContext()->SetUnreachable();
    }

    if (pSx->Is<Function>())
    {
        Function* pFun = pSx->StaticCast<Function>();
        if (pFun->HasUpVar() || pFun->MayBeClosure())
        {
            mergeUpVars(pFun);

            Values* pVx = new Values();
            emitInsn(new ValuesInsn(pVx));

            Register* pRd = new Register();
            emitInsn(new ClosureInsn(Qfunction, pRd, pFun, pVx));
            return emitLinkage(pRd);
        } // if
    } // if function

    Context::Linkage eLinkage = GetContext()->GetLinkage();
    switch (eLinkage)
    {
    case Context::Linkage_Jump:
        emitSucc();
        return pSx;

    case Context::Linkage_Next:
        return pSx;

    case Context::Linkage_Phi:
    {
        PhiInsn* pPhiInsn = GetContext()->GetSucc()->GetFirstInsn()->
            StaticCast<PhiInsn>();

        pPhiInsn->AddInput(GetContext()->GetCurr(), pSx);

        emitSucc();

        return pSx;
    } // Context::Linkage_Phi

    case Context::Linkage_Return:
        return emitReturn(pSx);

    case Context::Linkage_Unreachable:
        return Obj_Unreachable;
    } // switch eLinkage

    CAN_NOT_HAPPEN();
} // ClParser::emitLinkage


//////////////////////////////////////////////////////////////////////
//
// ClParser::emitRestoreValues
//
//  Called by:
//      ClParser::parse_multiple_value_prog1
//      ClParser::parse_unwind_protect
//
Operand*
ClParser::emitRestoreValues(Operand* pSx)
{
    Values* pVx = pSx->DynamicCast<Values>();
        if (NULL == pVx) return pSx;

    Instruction* pInsn = pVx->GetDfn();
    if (pInsn->Is<ValuesInsn>() || pInsn->Is<ValuesAInsn>())
    {
        // Move VALUES instruction to end of current bblock.
        ir_move_insn(
            pInsn,
            GetContext()->GetCurr()->GetAnchorInsn() );
        return pVx;
    }

    CAN_NOT_HAPPEN();
} // ClParser::emitRestoreValues


//////////////////////////////////////////////////////////////////////
//
//  ClParser::emitReturn
//
//  Description:
//   Emits RET instruction.
//
Operand*
ClParser::emitReturn(Operand* pSx)
{
    Operand* pSave = emitWeakSaveValues(pSx);
    emitUnwinds(NULL);
    Operand* pRestore = emitWeakRestoreValues(pSave);
    emitInsn(new RetInsn(pRestore));
    GetContext()->SetUnreachable();
    return Obj_Unreachable;
} // ClParser::emitReturn


//////////////////////////////////////////////////////////////////////
//
// ClParser::emitRuntimeCheck
//
//  Note: ty should be expanded.
Register*
ClParser::emitRuntimeCast(Ty ty, Register* pRx, uint rgfAttr)
{
    ty = ty_expand(ty);

    if (ty == nil)
    {
        warn(L"Type confilict.");
    }

    if (Subtypep_Yes == ty_subtypep(pRx->GetTy(), ty))
        { return pRx->StaticCast<Register>(); }

    if (option_no_type_check())     rgfAttr |= RuntimeCastInsn::Attr_Nop;
    if (option_simple_type_check()) rgfAttr |= RuntimeCastInsn::Attr_Simple;

    Register* pRd = new Register();
    emitInsn(new RuntimeCastInsn(ty, pRd, pRx, m_pFrame, rgfAttr));

    return pRd;
} // ClParser::emitRuntimeCheck


//////////////////////////////////////////////////////////////////////
//
// ClParser::emitSaveValues
//
//  Called by:
//      ClParser::parse_multiple_value_prog1
//      ClParser::parse_unwind_protect
//
Operand*
ClParser::emitSaveValues(Operand* pSx)
{
    Values* pVx = pSx->DynamicCast<Values>();
        if (NULL == pVx) return pSx;

    {
        Operand* pSd = pVx->Simplify();
        if (! pSd->Is<Values>()) return pSd;
    }

    Instruction* pInsn = pVx->GetDfn();
    switch (pInsn->GetOpcode())
    {
    case IrOp_CALL:
    {
        // if pInsn is CALL instruction and we know number
        // of values of callee, we use PROJECT instruction.
        Arity oArity;
            ir_get_ty_arity(pInsn->GetTy(), &oArity);
        if (oArity.IsFixed())
        {
            Values* pVd = new Values();
            ValuesInsn* pValues = new ValuesInsn(pVd);
            ir_insert_insn(pValues, pInsn->GetNext());

            uint iNth = 0;
            foreach (EnumTy, oEnum, pInsn->GetTy())
            {
                Register* pRx = new Register();
                ir_insert_insn(
                    new ProjectInsn(oEnum.Get(), pRx, pVx, iNth),
                    pValues );

                pValues->InsertBefore(pRx, iNth);
                iNth += 1;
            } // for each type
            return pVd;
        }
        break;;
    } // call
    case IrOp_VALUES:
        return pVx;

    case IrOp_MVRESTORE:
    {
        Instruction* pMvSave = pInsn->GetRx()->GetDfn();

        Register* pRx = new Register();
        ir_insert_insn(
            new CallInsn(ty_list, pRx, NewLiteral(Qlist), pMvSave->GetVx()),
            pMvSave );

        Values* pVd = new Values();
        ir_insert_insn(new ValuesAInsn(pVd, pRx), pMvSave);

        return pVd;
    } // restore

    } // switch opcode

    Register* pRx = new Register();
    ir_insert_insn(
        new CallInsn(ty_list, pRx, NewLiteral(Qlist), pVx),
        pVx->GetDfn()->GetNext() );

    Values* pVd = new Values();
    emitInsn(new ValuesAInsn(pVd, pRx));
    return pVd;
} // ClParser::emitSaveValues


//////////////////////////////////////////////////////////////////////
//
//  ClParser::emitSucc
//
Operand*
ClParser::emitSucc()
{
    BBlock* rSucc = GetContext()->GetSucc();
    emitInsn(new JumpInsn(rSucc));
    GetContext()->SetCurr(rSucc);
    return Obj_Unreachable;
} // ClParser::Context::Linkage


//////////////////////////////////////////////////////////////////////
//
// ClParsre::emitUnreachable
//
Operand*
ClParser::emitUnreachable()
{
    foreach (Frame::Enum, oEnum, m_pFrame)
    {
        Frame* pFrame = oEnum.Get();
        if (pFrame->GetOwner() != GetFunction())
        {
            break;
        }

        emitInsn(new UseInsn(pFrame));
    } // for each frame

    emitInsn(new UnreachableInsn());
    return Obj_Unreachable;
} // ClParser::emitUnreachable


//////////////////////////////////////////////////////////////////////
//
// ClParser::emitUnwind
//
void
ClParser::emitUnwind(Frame* pFrame)
{
    if (pFrame->GetOwner() == GetFunction())
    {
        emitInsn(new CloseInsn(pFrame));

        // For finally frame, we call cleanup function.
        if (pFrame->Is<FinallyFrame>())
        {
            FinallyFrame* pFinFrame = pFrame->StaticCast<FinallyFrame>();
            Values* pVx = new Values();
            emitInsn(new ValuesInsn(pVx));
            emitInsn(new CallInsn(pFinFrame->GetFinally(), pVx));
        }
    }
} // ClParser::emitUnwind


//////////////////////////////////////////////////////////////////////
//
// ClParser::emitUnwinds
//
// Note:
//  fAbort is true when CALL instruction doesn't return.
//
void
ClParser::emitUnwinds(Frame* pTo)
{
    foreach (Frame::Enum, oEnum, m_pFrame)
    {
        Frame* pFrame = oEnum.Get();
        if (pFrame == pTo)
        {
            break;
        }
        emitUnwind(pFrame);
    } // for each frame
} // ClParser::emitUnwinds


//////////////////////////////////////////////////////////////////////
//
// ClParser::emitWeakRestoreValues
//
Operand*
ClParser::emitWeakRestoreValues(Operand* pSx)
{
    {
        Values* pVx = pSx->DynamicCast<Values>();
        if (NULL != pVx)
        {
            // Move VALUES/VALUES* instruction to end of current bblock.
            ASSERT(
                pVx->GetDfn()->Is<ValuesInsn>() ||
                pVx->GetDfn()->Is<ValuesAInsn>() );

            ir_move_insn(
                pVx->GetDfn(),
                GetContext()->GetCurr()->GetAnchorInsn() );
            return pVx;
        } // if
    }

    Register* pRx = pSx->DynamicCast<Register>();
    if (NULL == pRx) return pSx;

    Instruction* pMvSave = pRx->GetDfn()->DynamicCast<MvSaveInsn>();
    if (NULL == pMvSave) return pSx;

    Values* pVy = pMvSave->GetVx();
    if (NULL == pVy) return pMvSave->GetSx();

    if (GetContext()->GetCurr()->GetLastInsn() == pMvSave)
    {
        return pVy;
    }

    Values* pVd = new Values();
    emitInsn(new MvRestoreInsn(pVy->GetTy(), pVd, pRx));
    return pVd;
} // ClParser::emitWeakRestoreValues


//////////////////////////////////////////////////////////////////////
//
// ClParser::emitWeakSaveValues
//
Operand*
ClParser::emitWeakSaveValues(Operand* pSx)
{
    Values* pVx = pSx->DynamicCast<Values>();
    if (NULL == pVx) return pSx;

    Instruction* pDfn = pVx->GetDfn();

    switch (pDfn->GetOpcode())
    {
    case IrOp_MVRESTORE:
    {
        Register* pRx = pDfn->GetRx();
        return pRx;
    } // mvrestore

    case IrOp_VALUES:
    case IrOp_VALUESA:
        return pVx;
    } // switch opcode

    Register* pRd = new Register();
    emitInsn(new MvSaveInsn(pRd, pVx));
    return pRd;
} // ClParser::emitWeakSaveValues

} // Compiler
