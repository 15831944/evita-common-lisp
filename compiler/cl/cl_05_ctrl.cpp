#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - 5 Data and Control Flow
// cl_05_control.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_05_ctrl.cpp#14 $
//
// Description:
//  This file contains following parsers:
//      cl:block
//      cl:catch
//      cl:go
//      cl:if
//      cl:progn
//      cl:return
//      cl:return-from
//      cl:tagbody
//      cl:throw
//      cl:unwind-protect
//
#include "./cl_defs.h"
#include "../cm/cm_base.h"
#include "../ir/ir_bblock.h"
#include "../ir/ir_instruction.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ClParser::parseBlockAux
//
// Called by:
//  ClParser::parse_block
//  ClParser::parse_lambda
//
//
//    pCurr                          BBlock from return form.
//      +------------------+          +------------------+
//      | ...              |          | ...              |
//      | JMP rLocalXp |              | JMP rLocalXp     |
//      +-----------o------+          +-------o----------+
//                  |                         |
//                  |   +---------------------+
//    rLocalXp  |   |
//      +-----------o---o------------------------------+
//      | PHI %rd <= (pCurr %r1) (from return %r2) ... |
//      | ...                                          |
//      +----------------------------------------------+
//
Operand*
ClParser::parseBlockAux(const Expect* pExpect, Val name, Val forms)
{
    BlockFrame* pBlockFrame = new BlockFrame(
        GetFunction(),
        name,
        pExpect,
        newBBlock(),
        newBBlock() );

    emitInsn(new OpenBlockInsn(
        pBlockFrame,
        NewLiteral(name),
        pBlockFrame->m_pNonlocalXp ) );

    CfgEdge* rNonlocalEdge =
        ir_get_cfg_edge(GetContext()->GetCurr(), pBlockFrame->m_pNonlocalXp);

    rNonlocalEdge->SetKind(CfgEdge::Kind_Nonlocal);

    BBlock* pSucc = GetContext()->SetContinue();

    {
        BBlock* pSucc = newBBlock();
        emitInsn(new JumpInsn(pSucc));
        GetContext()->SetCurrSucc(pSucc, pSucc);
    }

    PhiInsn* pPhi = NULL;
    if (ty_void != pExpect->Type)
    {
        pPhi = new PhiInsn(pExpect->Type, NewOutput(pExpect->Type));
        pBlockFrame->m_pLocalXp->AppendInsn(pPhi);
    }

    Operand* pSx;
    {
        pushFrame(pBlockFrame);
        pSx = parseForms(pExpect, forms);
        popFrame();
    } // pSx

    ////////////////////////////////////////////////////////////
    //
    // Local Exit Point
    //
    if (Obj_Unreachable != pSx)
    {
        emitInsn(new JumpInsn(pBlockFrame->m_pLocalXp));

        if (NULL != pPhi)
        {
            pPhi->AddInput(GetContext()->GetCurr(),pSx);
        }
    } // if not unreachable

    ////////////////////////////////////////////////////////////
    //
    // Nonlocal Exit Point
    //
    if (NULL == pBlockFrame->m_pVar)
    {
        ir_remove_insn(pBlockFrame->GetDfn());
        ir_remove_bblock(pBlockFrame->m_pNonlocalXp);

        if (! pBlockFrame->m_pLocalXp->HasInEdge())
        {
            ir_remove_bblock(pBlockFrame->m_pLocalXp);
        }
    }
    else
    {
        if (NULL == pPhi)
        {
            pBlockFrame->m_pNonlocalXp->AppendInsn(
                new JumpInsn(pBlockFrame->m_pLocalXp) );
        }
        else
        {
            Output* pSd = NewOutput(pExpect->Type);

            pBlockFrame->m_pNonlocalXp->AppendInsn(
                new NonlocalInsn(pExpect->Type, pSd) );

            pBlockFrame->m_pNonlocalXp->AppendInsn(
                new JumpInsn(pBlockFrame->m_pLocalXp) );

            pPhi->AddInput(pBlockFrame->m_pNonlocalXp, pSd);
        } // if phi
    } // if

    ////////////////////////////////////////////////////////////
    //
    // Close block frame
    //
    if (! pBlockFrame->m_pLocalXp->HasInEdge()) return Obj_Unreachable;

    GetContext()->SetCurrSucc(pBlockFrame->m_pLocalXp, pSucc);

    if (NULL == pPhi)
    {
        emitUnwind(pBlockFrame);
        return emitLinkage(Obj_Void);
    }
    else
    {
        Operand* pSave = emitWeakSaveValues(pPhi->SimplifyOutput());
        emitUnwind(pBlockFrame);
        return emitLinkage(emitWeakRestoreValues(pSave));
    }
} // ClParser::parseBlockAux


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_block
//
define_special_operator(block)
{
    CHECK_SYNTAX(2, MaxFormLength, "(block name form*)");

    Val name = second(form);
    if (! symbolp(name))
    {
        warn(L"Block name must be a symbol: ~S", name);
        return NewLiteral(nil);
    }

    return parseBlockAux(pExpect, name, cddr(form));
} // ClParser::parse_block


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_catch
//
define_special_operator(catch)
{
    CHECK_SYNTAX(2, MaxFormLength, "(catch tag form*)");

    Operand* pTag;
    {
        ExpectArgument oExpect(Qcatch, 0, ty_t);
        pTag = parseForm1(&oExpect, second(form));
            if (Obj_Unreachable == pTag) return useless_form(form);
    } // pTag

    CatchFrame* pCatchFrame = new CatchFrame(
        GetFunction(),
        pTag,
        pExpect,
        newBBlock(),
        newBBlock() );

    emitInsn(new OpenCatchInsn(pCatchFrame, pTag, pCatchFrame->m_pNonlocalXp));

    ir_get_cfg_edge(GetContext()->GetCurr(), pCatchFrame->m_pNonlocalXp)->
        SetKind(CfgEdge::Kind_Nonlocal);

    BBlock* pSucc = GetContext()->SetContinue();

    {
        BBlock* pSucc = newBBlock();
        emitInsn(new JumpInsn(pSucc));
        GetContext()->SetCurrSucc(pSucc, pSucc);
    }

    PhiInsn* pPhi = NULL;

    if (ty_void != pExpect->Type)
    {
        pPhi = new PhiInsn(pExpect->Type, NewOutput(pExpect->Type));
        pCatchFrame->m_pLocalXp->AppendInsn(pPhi);
    }

    Operand* pSx;
    {
        pushFrame(pCatchFrame);

        pSx = parseForms(pExpect, cddr(form));

        popFrame();
    } // pSx

    if (Obj_Unreachable != pSx)
    {
        emitInsn(new JumpInsn(pCatchFrame->m_pLocalXp));

        if (NULL != pPhi)
        {
            pPhi->AddInput(GetContext()->GetCurr(),pSx);
        }
    } // if not unreachable

    if (NULL == pPhi)
    {
        pCatchFrame->m_pNonlocalXp->AppendInsn(
            new JumpInsn(pCatchFrame->m_pLocalXp) );
    }
    else
    {
        Output* pSd = NewOutput(pExpect->Type);

        pCatchFrame->m_pNonlocalXp->AppendInsn(
            new NonlocalInsn(pExpect->Type, pSd) );

        pCatchFrame->m_pNonlocalXp->AppendInsn(
            new JumpInsn(pCatchFrame->m_pLocalXp) );

        pPhi->AddInput(pCatchFrame->m_pNonlocalXp, pSd);
    } // if phi

    GetContext()->SetCurrSucc(pCatchFrame->m_pLocalXp, pSucc);

    if (NULL == pPhi)
    {
        emitUnwind(pCatchFrame);
        return emitLinkage(Obj_Void);
    }
    else
    {
        Operand* pSave = emitWeakSaveValues(pPhi->SimplifyOutput());
        emitUnwind(pCatchFrame);
        return emitLinkage(emitWeakRestoreValues(pSave));
    }
} // ClParser::parse_catch


//////////////////////////////////////////////////////////////////////
//
//  ClParser::parse_go
//
define_special_operator(go)
{
    ASSERT(0 != pExpect->Type);

    CHECK_SYNTAX(2, 2, "(go tag)");

    Val name = second(form);
    TagbodyFrame::Tag* pTag = NULL;
    foreach (Frame::Enum, oEnum, m_pFrame)
    {
        TagbodyFrame* pFrame = oEnum.Get()->DynamicCast<TagbodyFrame>();
        if (NULL != pFrame)
        {
            pTag = pFrame->FindTag(name);
            if (NULL != pTag)
            {
                break;
            }
        }
    } // for each frame

    if (NULL == pTag)
    {
        warn(L"No such tag: ~S.", name);
    }
    else
    {
        TagbodyFrame* pFrame = pTag->GetFrame(); 
        pTag->Use();

        if (pTag->GetFrame()->GetOwner() == GetFunction())
        {
            // Local go
            if (! pTag->IsDefined())
            {
                // Forward jump
                emitUnwinds(pFrame);
            }
            else
            {
                // Backward jump can make infinite loop. So, all open
                // loop should be used here.
                foreach (Frame::Enum, oEnum, m_pFrame)
                {
                    Frame* pFrame = oEnum.Get();
                    if (pFrame->GetOwner() == GetFunction())
                    {
                        emitInsn(new UseInsn(pFrame));
                    }
                } // for each frame
            }

            emitInsn(new JumpInsn(pTag->GetBBlock()));
        }
        else
        {
            if (NULL == pTag->m_pVar)
            {
                pTag->m_pVar = newVariable(
                    make_symbol(format(nil, L"~A", pTag->GetName())) );

                Register* pRd = new Register();

                Instruction* pRef = pFrame->GetDfn()->GetNext();

                ir_insert_insn(
                    new TagInsn(pRd, pFrame, pTag->GetBBlock()),
                    pRef );

                ir_insert_insn(
                    new VarDefInsn(new Register(), pTag->m_pVar, pRd),
                    pRef );

                ir_get_cfg_edge(pRef->GetBBlock(), pTag->GetBBlock())->
                    SetKind(CfgEdge::Kind_Nonlocal);
            } // if no var

            Register* pRd;
            {
                Register* pCell = internVarCell(pTag->m_pVar);
                Register* pPtr = new Register();
                emitInsn(
                    new SlotInsn(
                        ty_ptr_t, pPtr,
                        NewLiteral(Qclosed_cell),
                        NewLiteral(Qvalue), 
                        pCell ));

                pRd = new Register();
                emitInsn(new LoadInsn(pRd, pPtr));
            } // pRd

            emitInsn(new GoInsn(pRd, m_pFrame));
            emitUnreachable();
        } // if
    } // if no tag

    return emitLinkage(Obj_Unreachable);
} // ClParser::parse_go


//////////////////////////////////////////////////////////////////////
//
//  ClParser::parse_if
//
define_special_operator(if)
{
    CHECK_SYNTAX(3, 4, "(if test then [else])");

    Val test_form = cadr(form);
    Val then_form = caddr(form);
    Val else_form = cadddr(form);
    bool fElse = nil != cdddr(form);

    Operand* pTest;
    {
        ExpectArgument oExpect(Qif, 0, ty_t);
        pTest = parseForm1(&oExpect, test_form);
        if (Obj_Unreachable == pTest)
        {
            unreachable_form(then_form);
            if (fElse) unreachable_form(else_form);
            return Obj_Unreachable;
        }
    } // pTest

    if (pTest->Is<Literal>())
    {
        if (pTest->StaticCast<Literal>()->GetDatum() != nil)
        {
            Operand* pOut = parseForm(pExpect, then_form);
            if (fElse) unreachable_form(else_form);
            return pOut;
        }
        else
        {
            unreachable_form(then_form);

            if (fElse)
            {
                return parseForm(pExpect, else_form);
            }
            else if (ty_void != pExpect->Type)
            {
                return parseLiteral(pExpect, nil);
            }
            else
            {
                return emitLinkage(Obj_Void);
            }
        }
    } // if test is literal

    Bool* pBx = new Bool();

    emitInsn(new NeInsn(pBx, pTest, NewLiteral(nil)));

    if (! fElse && ty_void == pExpect->Type)
    {
        // then only
        BBlock* pThen = newBBlock();
        BBlock* pSucc = GetContext()->GetSucc();
        BBlock* pJoin = pSucc;

        if (GetContext()->GetCurr() == pJoin)
        {
            ASSERT(Context::Linkage_Next == GetContext()->GetLinkage());
            pJoin = GetContext()->SetSucc(newBBlock());
            pSucc = pJoin;
        }
        else if (pJoin->IsExitBBlock())
        {
            ASSERT(Context::Linkage_Return == GetContext()->GetLinkage());
            pJoin = GetContext()->SetSucc(newBBlock());
        }

        emitInsn(new BranchInsn(pBx, pThen, pJoin));

        GetContext()->SetCurr(pThen);
        parseForm(pExpect, then_form);

        GetContext()->SetCurrSucc(pJoin, pSucc);
        return emitLinkage(Obj_Void);
    } // if then only

    // then and else
    {
        BBlock* pThen = newBBlock();
        BBlock* pElse = newBBlock();
        BBlock* pSucc = startPhiContext(pExpect->Type);
        BBlock* pJoin = GetContext()->GetSucc();

        emitInsn(new BranchInsn(pBx, pThen, pElse));

        GetContext()->SetCurrSucc(pThen, pJoin);
        parseForm(pExpect, then_form);

        GetContext()->SetCurrSucc(pElse, pJoin);
        parseForm(pExpect, else_form);

        GetContext()->SetCurrSucc(pJoin, pSucc);

        if (pJoin->HasInEdge())
        {
            return endPhiContext(pExpect->Type);
        }
        else
        {
            return GetContext()->SetUnreachable();
        } // if
    } // then and else
} // ClParser::parse_if


//////////////////////////////////////////////////////////////////////
//
//  ClParser::parse_progn
//
define_special_operator(progn)
{
    CHECK_SYNTAX(1, MaxFormLength, "(progn form*)");
    return parseForms(pExpect, cdr(form));
} // progn


//////////////////////////////////////////////////////////////////////
//
// ClParser::parseReturnAux
//
//  1. Look block frame up
//  2. Add Phi input oeprand
//  3. Jump to local exit point
//
Operand*
ClParser::parseReturnAux(const Expect*, Val name, Val value_forms, Val form)
{
    BlockFrame* pFrame = NULL;

    foreach (Frame::Enum, oEnum, m_pFrame)
    {
        BlockFrame* pRunner = oEnum.Get()->DynamicCast<BlockFrame>();
        if (NULL != pRunner && pRunner->GetName() == name)
        {
            pFrame = pRunner;
            break;
        }
    } // for each frame

    if (NULL == pFrame)
    {
        warn(L"No such block called ~S.", name);
        return GetContext()->SetUnreachable();
    }

    Operand* pSx = Obj_Void;

    if (ty_void != pFrame->m_pExpect->Type || nil != value_forms)
    {
        pSx = parseForm1(pFrame->m_pExpect, first(value_forms));
        if (Obj_Unreachable == pSx)
        {
            return useless_form(form);
        }
    } // if

    if (pFrame->GetOwner() == GetFunction())
    {
        // Local control transfer
        Operand* pSave = NULL;

        if (Obj_Void != pSx && pFrame->m_pLocalXp->HasInsn())
        {
            PhiInsn* pPhiInsn =
                pFrame->m_pLocalXp->GetFirstInsn()->DynamicCast<PhiInsn>();

            if (NULL != pPhiInsn)
            {
                pSave = emitWeakSaveValues(pSx);
                emitUnwinds(pFrame);
                Operand* pRestore = emitWeakRestoreValues(pSave);
                pPhiInsn->AddInput(GetContext()->GetCurr(), pRestore);
            }
        } // if

        if (NULL == pSave) emitUnwinds(pFrame);

        emitInsn(new JumpInsn(pFrame->m_pLocalXp));
    }
    else
    {
        // Nonlocal control transfer
        if (NULL == pFrame->m_pVar)
        {
            pFrame->m_pVar = newVariable(
                make_symbol(symbol_name(pFrame->GetName())) );

            Instruction* pRef = pFrame->GetDfn()->GetNext();

            Register* pRx = new Register();

            ir_insert_insn(
                new FrameInsn(ty_fixnum, pRx, pFrame, 0),
                pRef );

            ir_insert_insn(
                new VarDefInsn(new Register(), pFrame->m_pVar, pRx),
                pRef );
        } // if no var

        Values* pVx;
        {
            switch (pSx->GetKind())
            {
            case Operand::Kind_Values:
                pVx = pSx->StaticCast<Values>();
                break;

            case Operand::Kind_Void:
                pVx = new Values();
                emitInsn(new ValuesInsn(pVx));
                break;

            default:
                pVx = new Values();
                emitInsn(new ValuesInsn(pVx, pSx));
                break;
            } // switch operand
        } // pVx

        Register* pRd;
        {
            Register* pCell = internVarCell(pFrame->m_pVar);
            Register* pPtr = new Register();
            emitInsn(
                new SlotInsn(
                    ty_ptr_t, pPtr,
                    NewLiteral(Qclosed_cell),
                    NewLiteral(Qvalue), 
                    pCell ));

            pRd = new Register();
            emitInsn(new LoadInsn(pRd, pPtr));
        } // pRd

        emitInsn(new ReturnFromInsn(pRd, pVx, m_pFrame));
        emitUnreachable();
    } // if local

    return emitLinkage(Obj_Unreachable);
} // ClParser::parseReturnAux


//////////////////////////////////////////////////////////////////////
//
//  ClParser::parse_return
//
define_parser(return)
{
    CHECK_SYNTAX(1, 2, "(return [form])");
    return parseReturnAux(pExpect, nil, cdr(form), form);
} // ClParser::parse_return_from


//////////////////////////////////////////////////////////////////////
//
//  ClParser::parse_return_from
//
define_special_operator(return_from)
{
    CHECK_SYNTAX(2, 3, "(return-from name [form])");

    Val name = second(form);
    if (! symbolp(name))
    {
        if (! symbolp(name))
        {
            warn(L"Block name must be a symbol: ~S", name);
            return NewLiteral(nil);
        }
    }

    return parseReturnAux(pExpect, name, cddr(form), form);
} // ClParser::parse_return_from


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_tagbody
//
define_special_operator(tagbody)
{
    CHECK_SYNTAX(1, MaxFormLength, "(tagbody {tag|statement}*)");

    TagbodyFrame* pFrame = new TagbodyFrame(GetFunction());

    // collect tags
    {
        for (Val runner = cdr(form); nil != runner; runner = cdr(runner))
        {
            Val statement = car(runner);
            if (integerp(statement) || symbolp(statement))
            {
                TagbodyFrame::Tag* pTag = pFrame->FindTag(statement);
                if (NULL == pTag)
                {
                    pFrame->AddTag(new TagbodyFrame::Tag(
                        pFrame,
                        statement,
                        newBBlock() ) );
                }
            }
            else if (consp(statement))
            {
                // nothing to do
            }
            else
            {
                ignore_form(statement);
            }
        } // for each form
    }

    emitInsn(new OpenTagbodyInsn(pFrame));

    BBlock* pSucc = GetContext()->SetContinue();

    {
        BBlock* pSucc = newBBlock();
        emitInsn(new JumpInsn(pSucc));
        GetContext()->SetCurrSucc(pSucc, pSucc);
    }

    // Parse statements
    {
        pushFrame(pFrame);

        Expect oExpect(ty_void);
            oExpect.Name    = Qtagbody;

        for (Val runner = cdr(form); nil != runner; runner = cdr(runner))
        {
            Val statement = car(runner);

            if (integerp(statement) || symbolp(statement))
            {
                TagbodyFrame::Tag* pTag = pFrame->FindTag(statement);
                    ASSERT(NULL != pTag);

                if (ExpectContext_Unreachable != oExpect.Context)
                {
                    emitInsn(new JumpInsn(pTag->GetBBlock()));
                }

                GetContext()->SetCurr(pTag->GetBBlock());
                GetContext()->SetSucc(pTag->GetBBlock());

                pTag->Def();

                oExpect.Context = ExpectContext_Value;
                oExpect.Type    = ty_void;
            }
            else if (consp(statement))
            {
                Operand* pSx = parseForm1(&oExpect, statement);
                if (Obj_Unreachable == pSx)
                {
                    oExpect.Context = ExpectContext_Unreachable;
                }
            }
        } // for each form

        popFrame();
    }

    // Check tag usage
    {
        foreach (TagbodyFrame::EnumTag, oEnum, pFrame)
        {
            TagbodyFrame::Tag* pTag = oEnum.Get();
            if (! pTag->IsUsed())
            {
                Val name = pTag->GetName();
                if (! symbolp(name) || nil != symbol_package(name))
                {
                    style_warn(L"Tag ~S isn't used.", name);
                }
            } // if
        } // for each tag
    } // check usage

    if (0 == pFrame->GetCount())
    {
        ir_remove_insn(pFrame->GetDfn());
    }
    else if (NULL != GetContext()->GetCurr())
    {
        emitUnwind(pFrame);
    }

    GetContext()->RestoreSucc(pSucc);

    if (ty_void == pExpect->Type)
    {
        return emitLinkage(Obj_Void);
    }
    else
    {
        return emitLinkage(NewLiteral(nil));
    }
} // ClParser::parse_tagbody


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_throw
//
define_special_operator(throw)
{
    ASSERT(0 != pExpect->Type);

    CHECK_SYNTAX(3, 3, "(throw tag form)");

    Operand* pTag;
    {
        ExpectArgument oExpect(Qcatch, 0, ty_t);
        pTag = parseForm1(&oExpect, second(form));
        if (Obj_Unreachable == pTag)
        {
            return useless_form(form);
        }
    } // pTag

    // Local Throw?
    if (pTag->Is<Literal>())
    {
        Val tag = pTag->StaticCast<Literal>()->GetDatum();

        CatchFrame* pFrame = NULL;

        foreach (Frame::Enum, oEnum, m_pFrame)
        {
            CatchFrame* pRunner = oEnum.Get()->DynamicCast<CatchFrame>();
            if (NULL != pRunner)
            {
                Literal* pLx = pRunner->m_pTag->DynamicCast<Literal>();
                if (NULL != pLx && pLx->GetDatum() == tag)
                {
                    pFrame = pRunner;
                }
                break;
            }
        } // for each frame

        if (NULL != pFrame && pFrame->GetOwner() == GetFunction())
        {
            Operand* pSx = parseForm1(pFrame->m_pExpect, third(form));
            if (Obj_Unreachable == pSx)
            {
                return useless_form(form);
            }

            PhiInsn* pPhiInsn = pFrame->m_pLocalXp->GetFirstInsn()->
                DynamicCast<PhiInsn>();

            if (NULL == pPhiInsn)
            {
                emitUnwinds(pFrame);
            }
            else
            {
                Operand* pSave = emitWeakSaveValues(pSx);

                emitUnwinds(pFrame);

                pPhiInsn->AddInput(
                    GetContext()->GetCurr(),
                    emitWeakRestoreValues(pSave) );
            }

            emitInsn(new JumpInsn(pFrame->m_pLocalXp));

            return GetContext()->SetUnreachable();
        } // if local catch
    } // if literal tag

    // Nonlocal throw

    Operand* pSx;
    {
        ExpectArgument oExpect(Qcatch, 1, ty_values_rest_t);
        pSx = parseForm1(&oExpect, third(form));
        if (Obj_Unreachable == pSx)
        {
            return useless_form(form);
        }
    } // pSx

    Values* pVy = pSx->DynamicCast<Values>();

    if (NULL == pVy)
    {
        pVy = new Values();
        emitInsn(new ValuesInsn(pVy, pSx));
    }

    emitInsn(new ThrowInsn(pTag, pVy));
    emitUnreachable();
    return emitLinkage(Obj_Unreachable);
} // ClParser::parse_throw


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_unwind_protect
//
//  o (unwind-protect protected) => protected
//  o (unwind-protect protected form...)
//      => (let ((#:save (multiple-value-list protected)))
//            form...
//            (values-list #:save) )
//
define_special_operator(unwind_protect)
{
    CHECK_SYNTAX(1, MaxFormLength, "(unwind-protect protected form*)");
    if (nil == cddr(form))
    {
        // No cleanup forms.
        return parseForm(pExpect, second(form));
    }

    Function* pFinFun = NewFunction(Kfinally, Function::Kind_Finally);
    FinallyFrame* pFinFrame = new FinallyFrame(GetFunction(), pFinFun);

    {
        Values* pVx = new Values();
        emitInsn(new ValuesInsn(pVx));
        emitInsn(new OpenFinallyInsn(pFinFrame, pFinFun, pVx));
    }

    Operand* pSx;
    {
        // Parse protected-form.
        pushFrame(pFinFrame);
        pSx = parseForm1(pExpect, second(form));
        popFrame();
    } // pSx

    // Parse finally function.
    {
        LexEnv oLexEnv(LexEnv::Kind_lambda, pFinFun);
            oLexEnv.SetTy(ty_void);

        pFinFun->SetTy(list(Qfunction, nil, Qunspecified));

        LambdaList oLambdaList;

        processLambda(
            val_anonymous,
            pFinFun,
            &oLexEnv,
            &oLambdaList,
            cddr(form) );
    }

    mergeUpVars(pFinFun);

    if (Obj_Unreachable != pSx)
    {
        Operand* pSave = emitSaveValues(pSx);
        emitUnwind(pFinFrame);
        pSx = emitRestoreValues(pSave);
    }

    return emitLinkage(pSx);
} // ClParser::unwind_protect

} // Compiler
