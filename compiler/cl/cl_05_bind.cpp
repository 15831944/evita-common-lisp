#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - main
// gs_main.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_05_bind.cpp#16 $
//
// Description:
//  This file contains following parsers:
//      cl:flet
//      cl:labels
//      cl:funciton
//      cl:labels
//      cl:let
//      cl:let*
//      cl:macrolet
//      cl:setq
//
#include "./cl_defs.h"
#include "../cm/cm_base.h"
#include "../ir/ir_instruction.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// flet
//  Description:
//    o Activate function binding step by step.
//    o Activate free declarations
//    o Parse forms
//
define_special_operator(flet)
{
    CHECK_SYNTAX(2, MaxFormLength, "(flet (binding*) decl* form*)");

    Context::BindingScope oBindScope(
        LexEnv::Kind_flet, GetContext(), pExpect->Type );

    parseFunctionBindings(form);

    ASSERT(oBindScope.GetLexEnv() == GetLexEnv());

    Val forms = parse_declarations(cddr(form));

    deactivateLexEnv();

    // process function bindings
    foreach (LexEnv::EnumFun, oEnum, GetLexEnv())
    {
        if (FunDef* pFunDef = oEnum.Get()->DynamicCast<FunDef>())
        {
            processLambda(
                pFunDef->GetName(),
                pFunDef->GetFunction(),
                &pFunDef->m_oLexEnv,
                &pFunDef->m_oLambdaList,
                pFunDef->m_forms );

            pFunDef->SetTy(pFunDef->GetFunction()->GetTy());

            ASSERT(oBindScope.GetLexEnv() == GetLexEnv());
        } // if
    } // for each fun

    activateLexEnv();

    Operand* pSx = parseForms(pExpect, forms);

    closeLexEnv();

    return pSx;
} // flet


//////////////////////////////////////////////////////////////////////
//
//  function
//
define_special_operator(function)
{
    CHECK_SYNTAX(2, 2, "(function fname)");

    if (ty_void == pExpect->Type)
    {
        return ignore_form(form);
    }

    Val fname = second(form);

    if (symbolp(fname) || function_name_p(fname))
    {
        Val name = symbolp(fname) ? fname : intern_setf_cell(second(fname));
        FunDcl* pFunDcl = getFunDcl(name);
        switch (pFunDcl->GetKind())
        {
        case FunDcl::Kind_Undef:
            if (! fboundp(fname))
            {
                style_warn(L"Using undefined function ~S", fname);
            }
            // FALLTHROUGH

        case FunDcl::Kind_Function:
        {
            FunDcl* pRunner = pFunDcl;
            for (;;)
            {
                if (pRunner->Is<FunDef>())
                {
                    FunDef* pFunDef = pRunner->DynamicCast<FunDef>();
                    markFunUse(pFunDef);
                    return emitLinkage(pFunDef->GetFunction());
                }

                if (pRunner->Is<FunPcl>())
                {
                    FunPcl* pFunPcl = pRunner->StaticCast<FunPcl>();
                    Register* pRx = new Register();

                    if (name == fname)
                    {
                        // (function name)
                        emitInsn(
                            new SlotInsn(ty_make_ptr(pFunDcl->GetTy()), pRx,
                                NewLiteral(Qsymbol),
                                NewLiteral(Qfunction),
                                NewLiteral(name) ) );
                    }
                    else
                    {
                        // (function (setf name))
                        emitInsn(
                            new SlotInsn(ty_make_ptr(pFunDcl->GetTy()), pRx,
                                NewLiteral(Qsetf_cell),
                                NewLiteral(Qfunction),
                                NewLiteral(name) ) );
                    } // if

                    Register* pRd = new Register();
                    emitInsn(new LoadInsn(pRd, pRx));

                    if (option_check_undefined_function(pFunPcl))
                    {
                        Bool* pBx = new Bool();
                        emitInsn(new EqInsn(pBx, pRd, NewLiteral(nil)));

                        Values* pVx = new Values();
                        emitInsn(new ValuesInsn(pVx, NewLiteral(fname)));

                        TrapIfInsn* pTrapIf =
                                new TrapIfInsn(pBx, Qundefined_function, pVx);
                            pTrapIf->SetFrame(m_pFrame);
                        emitInsn(pTrapIf);
                    } // if

                    pRd->SetForm(fname);
                    return emitLinkage(pRd);
                } // if

                pRunner = pRunner->GetOuter();
            } // for
        } // FunDcl::Kind_Function

        case FunDcl::Kind_SpecialOperator:
            warn(L"Can't use special operator ~S as function.", fname);
            return emitLinkage(NewLiteral(nil));

        case FunDcl::Kind_Macro:
            warn(L"Can't use macro ~S as function.", fname);
            return emitLinkage(NewLiteral(nil));

        default:
            CAN_NOT_HAPPEN();
        } // switch kind
    }
    else if (consp(fname) && Qlambda == car(fname))
    {
        // 3.1.3 Lambda Expressions
        Expect oExpect(ty_values_rest_t);
        {
            Ty ty = ty_expand(pExpect->Type);
            html_log_format(3, L"parse_function: expected_ty=~W~:%", ty);
            if (consp(ty) && Qfunction == car(ty))
            {
                // BUGBUG: NYI: We should extract parameter types.
                oExpect.Type = third(ty);
            }
        }

        return emitLinkage(parse_lambda(&oExpect, fname));
    }
    else
    {
        warn(L"Syntax error: ~S", form);
        return emitLinkage(NewLiteral(nil));
    }
} // function

namespace
{
// findUpVar
static Instruction*
findUpVar(Function* pFun, Variable* pVar)
{
    foreach (Function::EnumUpVarSite, oEnum, pFun)
    {
        if (oEnum.Get()->GetOperand() == pVar)
        {
            return oEnum.Get()->GetInstruction();
        }
    } // for each upvar
    return NULL;
} // findUpVar


// insertUpVar
static Instruction*
insertUpVar(Function* pFun, Variable* pVar)
{
    ASSERT(NULL == findUpVar(pFun, pVar));
    return ir_insert_insn(
        new UpVarDefInsn(new Register(pVar), pVar),
        pFun->GetEntryInsn()->GetNext() );
} // insertUpVar

} // namespace


//////////////////////////////////////////////////////////////////////
//
// labels
//  Description:
//    o Activate function binding at once.
//    o Activate free declarations
//    o Parse forms
//
define_special_operator(labels)
{
    CHECK_SYNTAX(2, MaxFormLength, "(labels (binding*) decl* form*)");

    Context::BindingScope oBindScope(
        LexEnv::Kind_labels, GetContext(), pExpect->Type );

    parseFunctionBindings(form);

    Val forms = parse_declarations(cddr(form));

    deactivateLexEnv();

    // activate fundef
    foreach (LexEnv::EnumFun, oEnum, GetLexEnv())
    {
        FunDef* pFunDef = oEnum.Get()->DynamicCast<FunDef>();
            if (NULL == pFunDef) continue;

        ASSERT(oBindScope.GetLexEnv() == GetLexEnv());

        activateFunDcl(pFunDef);

        pFunDef->GetFunction()->SetMayBeClosure(true);
    } // for each fun

    // process function bindings
    foreach (LexEnv::EnumFun, oEnum, GetLexEnv())
    {
        FunDef* pFunDef = oEnum.Get()->DynamicCast<FunDef>();
            if (NULL == pFunDef) continue;

        processLambda(
            pFunDef->GetName(),
            pFunDef->GetFunction(),
            &pFunDef->m_oLexEnv,
            &pFunDef->m_oLambdaList,
            pFunDef->m_forms );

        pFunDef->SetTy(pFunDef->GetFunction()->GetTy());
    } // for each fun

    // Fix upvars
    {
        WorkList_<Function> oWorkList;

        foreach (LexEnv::EnumFun, oEnum, GetLexEnv())
        {
            FunDef* pFunDef = oEnum.Get()->DynamicCast<FunDef>();
            if (NULL == pFunDef)
            {
                continue;
            }

            Function* pCallee = pFunDef->GetFunction();

            pCallee->SetMayBeClosure(false);

            if (! pCallee->HasUpVar())
            {
                continue;
            }

            oWorkList.Push(pCallee);
        } // for each fundecl

        while (! oWorkList.IsEmpty())
        {
            Function* pCallee = oWorkList.Pop();

            foreach (Function::EnumCaller, oEnum, pCallee)
            {
                Function* pCaller = oEnum.GetNode();
                foreach (Function::EnumUpVar, oEnum, pCallee)
                {
                    Variable* pVar = oEnum.Get();

                    if (pVar->GetOwner() != pCaller &&
                        NULL == findUpVar(pCaller, pVar) )
                    {
                        insertUpVar(pCaller,pVar);

                        html_log_format(3, L"labels: ~S: add upvar ~S to ~S~%",
                            pCallee,
                            pVar,
                            pCaller );

                        foreach (Function::EnumCaller, oEnum,pCallee)
                        {
                            Function* pCaller = oEnum.GetNode();
                            if (! oWorkList.Has(pCaller))
                            {
                                oWorkList.Push(pCaller);
                            }
                        } // for each caller of caller
                    } // if upvar
                } // for each upvar
            } // for each caller
        } // while changed
    } // fix upvars

    activateFreeDcls();

    Operand* pSx = parseForms(pExpect, forms);

    closeLexEnv();

    return pSx;
} // labels


//////////////////////////////////////////////////////////////////////
//
//  ClParser::parse_let
//  Description:
//    o Activate variable AFTER processing all initforms.
//    o Activate all declarations.
//    o Parse forms.
//
//      OPENBIND %frame <= (symx %sx) (symy %sy) ...
//      ... forms ...
//      CLOSE %frame
//
define_special_operator(let)
{
    CHECK_SYNTAX(2, MaxFormLength, "(let (binding*) decl* form*)");

    Context::BindingScope oBindScope(
        LexEnv::Kind_let, GetContext(), pExpect->Type );

    parseVariableBindings(second(form));

    Val forms = parse_declarations(cddr(form));

    deactivateLexEnv();

    OpenBindInsn* pOpenBind = NULL;

    // parse initforms
    foreach (LexEnv::EnumVar, oEnum, GetLexEnv())
    {
        VarDef* pVarDef = oEnum.Get()->DynamicCast<VarDef>();
        if (NULL == pVarDef)
        {
            continue;
        }

        Expect oExpect(pVarDef);
        Operand* pSx = parseForm1(&oExpect, pVarDef->GetInitForm());

        if (Obj_Unreachable == pSx)
        {
            return GetContext()->SetUnreachable();
        }

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
            }

            Val cell = intern_value_cell(pVarDef->GetName());
            pOpenBind->AddBind(cell, pSx);
        } // if
    } // for each vardef

    return parseLetAux(pExpect, forms, pOpenBind);
} // ClParser::parse_let


//////////////////////////////////////////////////////////////////////
//
// ClParser::parseLetAux
//  Called by:
//      parse_let
//      parse_multiple_value
//
Operand* ClParser::parseLetAux(
    const Expect*   pExpect,
    Val             forms,
    OpenBindInsn*   pOpenBind )
{
    activateLexEnv();

    Operand* pSx;

    if (NULL == pOpenBind)
    {
        pSx = parseForms(pExpect, forms);
    }
    else
    {
        emitInsn(pOpenBind);

        Frame* pFrame = pOpenBind->GetOutput()->StaticCast<Frame>();

        BBlock* pSucc = GetContext()->SetContinue();

        pushFrame(pFrame);

        pSx = parseForms(pExpect, forms);

        popFrame();

        if (Obj_Unreachable != pSx)
        {
            Operand* pSave = emitWeakSaveValues(pSx);
            emitUnwind(pFrame);
            pSx = emitWeakRestoreValues(pSave);
        }

        GetContext()->RestoreSucc(pSucc);
        emitLinkage(pSx);
    } // if

    closeLexEnv();

    return pSx;
} // ClParser::parseLetAux


//////////////////////////////////////////////////////////////////////
//
//  ClParser::parse_let*
//
//  Description:
//    o Activates variable binding step by step.
//
define_special_operator(letA)
{
    CHECK_SYNTAX(2, MaxFormLength, "(let* (binding*) decl* form*");

    Context::BindingScope oBindScope(
        LexEnv::Kind_let, GetContext(), pExpect->Type );

    parseVariableBindings(second(form));

    Val forms = parse_declarations(cddr(form));

    deactivateLexEnv();

    Frame* pCurpFrame = m_pFrame;

    // parse initform
    foreach (LexEnv::EnumVar, oEnum, GetLexEnv())
    {
        VarDef* pVarDef = oEnum.Get()->DynamicCast<VarDef>();
        if (NULL == pVarDef)
        {
            continue;
        }

        Expect oExpect(pVarDef);
        Operand* pSx = parseForm1(&oExpect, pVarDef->GetInitForm());
        if (Obj_Unreachable == pSx)
        {
            return GetContext()->SetUnreachable();
        }

        emitBind(pVarDef, pSx);
    } // for each vardef

    activateFreeDcls();

    Operand* pSx;
    if (m_pFrame == pCurpFrame)
    {
        // No special variable binding
        pSx = parseForms(pExpect, forms);
    }
    else
    {
        BBlock* pSucc = GetContext()->SetContinue();
        pSx = parseForms(pExpect, forms);

        if (Obj_Unreachable != pSx)
        {
            Operand* pSave = emitWeakSaveValues(pSx);

            while (m_pFrame != pCurpFrame)
            {
                emitUnwind(popFrame());
            } // while

            pSx = emitWeakRestoreValues(pSave);
        } // if

        GetContext()->RestoreSucc(pSucc);
        emitLinkage(pSx);
    } // if frame

    closeLexEnv();

    return pSx;
} // ClParser::parse_letA


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_macrolet
//
define_special_operator(macrolet)
{
    CHECK_SYNTAX(2, MaxFormLength, "(macrolet (bind*) decl* form*)");

    Context::BindingScope oBindScope(
        LexEnv::Kind_macrolet, GetContext(), pExpect->Type );

    deactivateLexEnv();

    foreach (EnumList, oEnum, second(form))
    {
        Val binding = oEnum.Get();
        Operand* pSx = check_syntax(form, 2, MaxFormLength,
            L"(name lambda-list decl* form*)" );
        if (NULL != pSx) continue;

        Val name = first(binding);
        if (! symbolp(name))
        {
            warn(L"Macro name ~S must be a symbol.", name);
            continue;
        }

        Val expander = funcall(Qc6_parse_macro,
            name,               // name
            second(binding),    // lambda-list
            cddr(binding) );    // body
        if (nil == expander)
        {
            warn(L"Failed to compile macro ~S.", name);
            continue;
        }
        addFunDcl(new FunDcl(
            FunDcl::Kind_Macro,
            name,
            expander ) );
    } // for each binding

    Val forms = parse_declarations(cddr(form));

    activateFreeDcls();

    Operand* pSx = parseForms(pExpect, forms);

    closeLexEnv();

    return pSx;
} // macrolet


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_setq
//
// Description:
//  Parses setq form.
//
//      lexical variable:
//          %cell -- From cl-vardef
//          %sx   -- From value-form
//        SLOT <ptr t> %r1 <= <closed-cell> 'si::value %cell
//        STORE %r2 %sx
//
//      constant:
//        error "Atempt to alter constant [name]."
//
//      special variable:
//        SLOT <ptr t> %r1 <= value-cell :value {cell}
//        STORE %r1 %sx
//
//      thread-local variable:
//        STORE {tlvrec} %sx
//
//      symbol macro:
//        parse (setf [expansion] [value-form])
//
define_special_operator(setq)
{
    CHECK_SYNTAX(1, MaxFormLength, "(setq {var val}*)");

    Operand* pSy = NewLiteral(nil);

    Val runner = cdr(form);

    while (consp(runner))
    {
        Val name = car(runner);
            runner = cdr(runner);

            if (! symbolp(name))
            {
                warn(L"Expected variable name: ~S", name);
                name = make_symbol(make_string(L"err"));
            }

            if (nil == runner)
            {
                warn(L"Missing value form for ~S.", name);
            }

            if (! consp(runner))
            {
                break;
            }

        Val value_form = car(runner);
            runner = cdr(runner);

        VarDcl* pVarDcl = getVarDcl(name);

        if (VarDcl::Kind_Constant == pVarDcl->GetKind())
        {
            warn(L"Atempt to alter constant ~S.", name);
            pSy = Obj_Unreachable;
            continue;
        }

        if (pSy == Obj_Unreachable)
        {
            unreachable_form(value_form);
            continue;
        }

        switch (pVarDcl->GetKind())
        {
        case VarDcl::Kind_Lexical:
        {
            ExpectAssign oExpect(pVarDcl->GetTy(), name);
            pSy = parseForm1(&oExpect, value_form);
            if (Obj_Unreachable == pSy) break;

            markVarUse(pVarDcl);

            Variable* pVar = pVarDcl->GetVar();
            Register* pCell = internVarCell(pVar);
            Register* pPtr = new Register(pVar);

            emitInsn(
                new SlotInsn(
                    ty_make_ptr(pVarDcl->GetTy()),
                    pPtr,
                    NewLiteral(Qclosed_cell),
                    NewLiteral(Qvalue), 
                    pCell ) );

            emitInsn(new StoreInsn(pPtr, pSy));
            break;
        } // VarDcl::Kind_Lexical

        case VarDcl::Kind_Special:
            goto parse_special;

        case VarDcl::Kind_SymbolMacro:
        {
            Expect oExpect(nil == runner ? pExpect->Type : ty_void);

            Val expansion = pVarDcl->GetDatum();
            if (symbolp(expansion))
            {
                pSy = parseForm1(
                    &oExpect,
                    list(Qsetq, expansion, value_form) );
            }
            else
            {
                pSy = parseForm1(
                    &oExpect,
                    list(Qsetf, expansion, value_form) );
            } // if
            break;
        } // VarDcl::Kind_SymbolMacro

        case VarDcl::Kind_Undef:
            if (! boundp(name))
            {
                style_warn(L"Using undefined variable: ~S", name);
            }
            goto parse_special;

        parse_special:
        {
            ExpectAssign oExpect(pVarDcl->GetTy(), name);
            pSy = parseForm1(&oExpect, value_form);
            if (Obj_Unreachable == pSy) break;

            Register* pRx = new Register();
            {
                Val datum = intern_value_cell(name);

                if (value_cell_p(datum))
                {
                    emitInsn(
                        new SlotInsn(
                            ty_make_ptr(pVarDcl->GetTy()),
                            pRx,
                            NewLiteral(Qvalue_cell),
                            NewLiteral(Qvalue), 
                            NewLiteral(datum) ) );
                }
                else if (tlv_record_p(datum))
                {
                    emitInsn(
                        new TlvInsn(
                            ty_make_ptr(pVarDcl->GetTy()),
                            pRx,
                            datum ) );
                }
                else
                {
                    CAN_NOT_HAPPEN();
                }
            } // pSx

            emitInsn(new StoreInsn(pRx, pSy));
            break;
        } // parse_special

        default:
            CAN_NOT_HAPPEN();
        } // switch kind
    } // while

    if (nil != runner) warn(L"Malformed form: ~S", form);

    ASSERT(NULL != pSy);
    return emitLinkage(emitCast(pExpect, form, pSy));
} // setq


//////////////////////////////////////////////////////////////////////
//
// ClParser::computeFunctionName
//
Val
ClParser::computeFunctionName(Val flet, Val fname)
{
    LexEnv* pRunner = GetLexEnv();
    for (;;)
    {
        pRunner = pRunner->GetOuter();
        if (NULL == pRunner)
        {
            return fname;
        }

        if (pRunner->GetKind() == LexEnv::Kind_lambda)
        {
            Val name = pRunner->GetOwner()->GetName();

            if (consp(name) && (Qflet == car(name) || Qlabels == car(name)))
            {
                return append(name, list(fname));
            }

            if (val_anonymous == name)
            {
                name = nil;
            }

            return list(flet, name, fname);
        }
    } // for
} // ClParser::computeFunctionName


//////////////////////////////////////////////////////////////////////
//
// ClParser::parseFunctionBindings
//
//  Called by:
//      ClParser::parse_flet
//      ClParser::parse_labels
//
void
ClParser::parseFunctionBindings(Val form)
{
    Val bindings = second(form);

    // parse binding
    if (minusp_xx(safe_list_length(bindings)))
    {
        warn(L"Malformed function bindings: ~S", bindings);
    }
    else
    {
        for (
            Val runner = bindings;
            nil != runner;
            runner = cdr(runner) )
        {
            Val binding = car(runner);

            // (fname lambda-list decl* form*)
            if (cmp_xx(safe_list_length(binding), 2) < 0)
            {
                warn(L"Malformed function binding: ~S", binding);
                continue;
            }

            Val fname = car(binding);
            if (! function_name_p(fname))
            {
                warn(L"Invalid function name ~S.", fname);
                fname = make_symbol(make_string(L"err"));
            }

            Function* pFun = NewFunction(
                computeFunctionName(car(form), fname) );

            Val name = symbolp(fname) ?
                fname :
                intern_setf_cell(second(fname));

            FunDef* pFunDef = new FunDef(name, pFun);

            {
                Context oContext(pFun, &pFunDef->m_oLexEnv);
                ContextScope oScope(this, &oContext);

                pFunDef->m_forms = parseLambdaList(
                    &pFunDef->m_oLexEnv,
                    binding,
                    &pFunDef->m_oLambdaList );
            }

            pFunDef->SetTy(pFun->GetTy());

            addFunDcl(pFunDef);
        } // for each binding
    } // if
} // ClParser::parseFunctionBindings


//////////////////////////////////////////////////////////////////////
//
// ClParser::parseVariableBindings
//
//  Called by:
//      ClParser::parse_let
//      ClParser::parse_letA
//
void
ClParser::parseVariableBindings(Val bindings)
{
    if (minusp_xx(safe_list_length(bindings)))
    {
        warn(L"Malformed bindings: ~S", bindings);
        return;
    }

    for (Val runner = bindings; nil != runner; runner = cdr(runner))
    {
        Val binding = car(runner);
        Val name = nil;
        Val initform = nil;

        if (symbolp(binding))
        {
            name = binding;
        }
        else if (consp(binding))
        {
            name = car(binding);
            if (nil == cdr(binding))
            {
                // nothing to do
            }
            else if (consp(cdr(binding)))
            {
                initform = cadr(binding);
                if (nil != cddr(binding))
                {
                    malformed_binding(binding);
                }
            }
            else
            {
                malformed_binding(binding);
            }
        } // if

        if (! symbolp(name))
        {
            malformed_binding(binding);
            name = make_symbol(make_string(L"err"));
        }

        addVarDcl(makeVarDef(name, initform));
    } // for each binding
} // ClParser::parseVariableBindings

} // Compiler
