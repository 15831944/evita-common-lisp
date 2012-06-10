#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 3 Evaluation and Compilation
// compiler/cl/cl_03_eval.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_03_eval.cpp#23 $
//
//      cl:eval-wehn
//      cl:locally
//      cl:load_time_value
//      cl:quote
//      cl:symbol-macrolet
//      cl:the
//
#include "./cl_defs.h"
#include "../cm/cm_base.h"
#include "../ir/ir_instruction.h"

namespace Compiler
{

Val
ClParser::callMacroExpander(Val expander, Val form)
{
    // FIXME: 2007-03-13: We must pass bindings established by macrolet
    // and symbol-macrolet.
    Val env = nil;
    Val expansion = funcall(Qc6_call_macro_expander, expander, form, env);

    Val macro_error = MiniThread::Get()->mv_value[1];

    if (nil != macro_error)
    {
        warn(L"~A", macro_error);
        return nil;
    }

    if (form == expansion)
    {
        // Avoid infinie macro expansion.
        warn(L"Macro ~S expanded into self.");
        return nil;
    }

    return expansion;
} // ClParser::callMacroExpander


//////////////////////////////////////////////////////////////////////
//
//  ClParser::parseCompoundForm
//
Operand*
ClParser::parseCompoundForm(const Expect* pExpect, Val form)
{
    ToplevelFormScope oToplevelForm(this);

    Val name = car(form);

    // TODO yosi@msn.com 2008-06-21 We should use parser to handle
    // declare exprssion instead of here.
    if (Qdeclare == name)
    {
        warn(L"Can't use ~S as form.", form);
        return Obj_Unreachable;
    }

    // TODO yosi@msn.com 2008-06-21 We should use parser to handle
    // unspecified exprssion instead of here.
    // TODO yosi@msn.com 2008-06-21 Need to clarify specification of
    // unspecified form.
    if (Qunspecified == name)
    {
        return emitLinkage(NewLiteral(nil));
    }

    FunDcl* pFunDcl = getFunDcl(name);

    switch (pFunDcl->GetKind())
    {
    case FunDcl::Kind_Undef:
        if (! fboundp(name))
        {
            style_warn(L"Using undefined function ~S", name);
        }
        // FALLTHROUGH

    case FunDcl::Kind_Function:
        return parseFunctionForm(pExpect, form, pFunDcl);

    case FunDcl::Kind_Macro:
    {
        oToplevelForm.StillToplevel();

        ParserT pfn = s_oParserTable.Get(name);
        if (NULL != pfn)
        {
            return (this->*pfn)(pExpect, form);
        }

        // BUGBUG: pass environment
        Val expansion = callMacroExpander(pFunDcl->GetDatum(), form);
        return parseForm(pExpect, expansion);
    } // FunDcl::Kind_Macro

    case FunDcl::Kind_SpecialOperator:
    {
        ParserT pfn = s_oParserTable.Get(name);
        if (NULL == pfn)
        {
            return parseForm(pExpect, list(Qerror,
                make_string(L"NYI: special-operator ~S"), name ) );
        }

        if (Qeval_when == name ||
            Qlocally   == name ||
            Qmacrolet  == name ||
            Qprogn     == name )
        {
            oToplevelForm.StillToplevel();
        }

        return (this->*pfn)(pExpect, form);
    } // FunDcl::Kind_SpecialOperator

    default:
        CAN_NOT_HAPPEN();
    } // switch kind
} // parseCompoundForm


//////////////////////////////////////////////////////////////////////
//
// parse_cons
//
//  3.1.2.1.2 Conses as Forms
//
Operand*
ClParser::parse_cons(const Expect* pExpect, Val form)
{
    Session::Get()->RememberSource(form);

    Val name = car(form);

    if (symbolp(name))
    {
        return parseCompoundForm(pExpect, form);
    }
    else if (consp(name) && Qlambda == car(name))
    {
        // 3.1.2.1.24 Lambda Forms
        Function* pFun = parse_lambda(pExpect, name);
        Callee oCallee;
            oCallee.m_ty  = pFun->GetTy();
            oCallee.m_pSx = pFun;

        Operand* pSx = parseArgs(form, &oCallee, cdr(form));
            if (Obj_Unreachable == pSx) return useless_form(form);

        Values* pArgs = pSx->StaticCast<Values>();
        return emitLinkage(emitCall(pExpect, form, &oCallee, pArgs));
    }
    else
    {
        warn(L"Invalid cons form: ~S", form);
        return parseLiteral(pExpect, nil);
    }
} // ClParser::parse_cons


//////////////////////////////////////////////////////////////////////
//
// parseForm
//
Operand*
ClParser::parseForm(const Expect* pExpect, Val form)
{
    if (ExpectContext_Unreachable == pExpect->Context) 
    {
        return unreachable_form(form);
    }

    if (symbolp(form))
    {
        return parseSymbol(pExpect, form);
    }
    else if (consp(form))
    {
        return parse_cons(pExpect, form);
    }
    else
    {
        return parseLiteral(pExpect, form);
    }
} // ClParser::parseForm


//////////////////////////////////////////////////////////////////////
//
// parseForm1
//
Operand*
ClParser::parseForm1(const Expect* pExpect, Val form)
{
    Context* pContext = GetContext();
    BBlock* pSucc = pContext->SetContinue();
    Operand* pSx = parseForm(pExpect, form);
    pContext->RestoreSucc(pSucc);
    return pSx;
} // ClParser::parseForm1


//////////////////////////////////////////////////////////////////////
//
// parseForm2
//
bool
ClParser::parseForm2(
    const Expect*   pExpect,
    Ty              expected_ty2,
    Val             forms,
    Operand**       out_pSx,
    Operand**       out_pSy )
{
    ASSERT(NULL != out_pSx);
    ASSERT(NULL != out_pSy);

    Context* pContext = GetContext();
    BBlock* pSucc = pContext->SetContinue();

    *out_pSx = parseForm(pExpect, first(forms));
    if (Obj_Unreachable == *out_pSx)
    {
        unreachable_form(second(forms));
        return false;
    }

    Expect oExpect = *pExpect;
        oExpect.Type = expected_ty2;
        oExpect.Nth  = add_xx(oExpect.Nth, 1);

    *out_pSy = parseForm(pExpect, second(forms));
    if (Obj_Unreachable == *out_pSy)
    {
        return false;
    }

    pContext->RestoreSucc(pSucc);
    return true;
} // ClParser::parseForm1


//////////////////////////////////////////////////////////////////////
//
// parseForms
//
Operand*
ClParser::parseForms(const Expect* pExpect, Val forms)
{
    if (nil == forms) return parseLiteral(pExpect, nil);

    BBlock* pSucc = GetContext()->SetContinue();

    Expect oExpect(ty_void);
    for (;;)
    {
        if (! consp(forms))
        {
            warn(L"Dotted forms.");
            return parseLiteral(pExpect, nil);
        }

        if (nil == cdr(forms))
        {
            GetContext()->RestoreSucc(pSucc);
            if (oExpect.Context == ExpectContext_Unreachable)
                { pExpect = &oExpect; }
            return parseForm(pExpect, car(forms));
        }

        if (Obj_Unreachable == parseForm(&oExpect, car(forms)))
        {
            oExpect.Context = ExpectContext_Unreachable;
        }

        forms = cdr(forms);
    }  // for
} // ClParser::parseForms


// parseLiteral
Operand*
ClParser::parseLiteral(const Expect* pExpect, Val form)
{
    if (ty_void == pExpect->Type)
    {
        style_warn(L"Ignore literal: ~S", form);
        return emitLinkage(Obj_Void);
    }

    if (! ty_typep(form, pExpect->Type))
    {
        unexpected_type(pExpect, type_of(form), form);
    }

    Operand* pSx = NewLiteral(form);
    return emitLinkage(pSx);
} // ClParser::parseLiteral


//////////////////////////////////////////////////////////////////////
//
// parseSymbol
//
//  Description:
//   Parses symbol and returns register or literal.
//
//  Lexical variable:
//      %cell -- from VARDEF si:closed-cell %cell <= name %sx
//      ...
//      SLOT (ptr t) %r0 <= si:closed-cell si:value %cell
//      LOAD t <= %r0
//
Operand*
ClParser::parseSymbol(const Expect* pExpect, Val symbol)
{
    if (nil == symbol || keywordp(symbol))
    {
        return parseLiteral(pExpect, symbol);
    }

    if (ty_void == pExpect->Type)
    {
        if (nil != symbol_package(symbol))
        {
            style_warn(L"Ignore symbol: ~S", symbol);
        }
        return emitLinkage(Obj_Void);
    }

    VarDcl* pVarDcl = getVarDcl(symbol);

    switch (pVarDcl->GetKind())
    {
    case VarDcl::Kind_Lexical:
    {
        Variable* pVar = pVarDcl->GetVar();

        Register* pCell = internVarCell(pVar);
        Register* pRx = new Register(pVar);

        emitInsn(
            new SlotInsn(
                ty_make_ptr(pVarDcl->GetTy()), pRx,
                NewLiteral(Qclosed_cell),
                NewLiteral(Qvalue), 
                pCell ));

        Register* pRd = new Register(pVar);
        emitInsn(new LoadInsn(pRd, pRx));

        markVarUse(pVarDcl);

        pRd->SetForm(symbol);

        return emitLinkage(emitCast(pExpect, symbol, pRd));
    } // VarDcl::Kind_Lexical

    case VarDcl::Kind_Constant:
        return parseLiteral(pExpect, pVarDcl->GetDatum());

    case VarDcl::Kind_Special:
        goto parse_special;

    case VarDcl::Kind_SymbolMacro:
        return parseForm(pExpect, pVarDcl->GetDatum());

    case VarDcl::Kind_Undef:
        if (! boundp(symbol))
        {
            style_warn(L"Using undefined variable: ~S", symbol);
        }
        goto parse_special;

    parse_special:
    {
        // For special variable:
        //    SLOT (ptr t) %r0 <= value-cell :value {cell}
        //    LOAD t %r1 <= %r0
        //    EQ bool %b2 <= %r1 ..unbound..
        //    TRAP-IF %b2 unbound-variable (:name {symbol})
        //
        // For TLV:
        //    LOAD t %r1 <= {tlvrec}
        //    EQ bool %b2 <= %r1 ..unbound..
        //    TRAP-IF %b2 unbound-variable (:name {symbol})

        Register* pRx = new Register();
        {
            Val datum = intern_value_cell(symbol);

            if (value_cell_p(datum))
            {
                emitInsn(
                    new SlotInsn(ty_make_ptr(pVarDcl->GetTy()), pRx,
                        NewLiteral(Qvalue_cell),
                        NewLiteral(Qvalue),
                        NewLiteral(datum)) );
            }
            else if (tlv_record_p(datum))
            {
                emitInsn(
                    new TlvInsn(ty_make_ptr(pVarDcl->GetTy()), pRx,
                        datum ) );
            }
            else
            {
                CAN_NOT_HAPPEN();
            }
        } // pRx

        Register* pRd = new Register();
        emitInsn(new LoadInsn(pRd, pRx));

        if (option_check_unbound_variable(symbol))
        {
            Bool* pBx = new Bool();
            emitInsn(new EqInsn(pBx, pRd, NewLiteral(QQunbound_marker)));

            Values* pVx = new Values();
            emitInsn(new ValuesInsn(pVx, NewLiteral(symbol)));

            TrapIfInsn* pTrapIf = new TrapIfInsn(pBx, Qunbound_variable, pVx);
                pTrapIf->SetFrame(m_pFrame);
            emitInsn(pTrapIf);
        }

        pRd->SetForm(symbol);

        return emitLinkage(emitCast(pExpect, symbol, pRd));
    } // parse_special

    default:
        CAN_NOT_HAPPEN();
        // NOTREACHED
    } // switch kind
} // ClParser::parseSymbol


//////////////////////////////////////////////////////////////////////
//
// eval_when
//
// This parser uses following variables:
//      si::*situation*
//      si::*processing-mode*   when *situation* = compile-file
//
define_special_operator(eval_when)
{
    CHECK_SYNTAX(2, MaxFormLength, "(eval-when (situation*) form*)");

    enum Action
    {
        Action_Compile              = 0,
        Action_Discard              = 1,
        Action_Eval                 = 2,
        Action_EvalAndCompile       = 3,
        Action_Mask                 = 3,

        Action_CompileTimeToo       = 4,

        Action_Compile_Too          = Action_CompileTimeToo | Action_Compile,
        Action_Discard_Too          = Action_CompileTimeToo | Action_Discard,
        Action_Eval_Too             = Action_CompileTimeToo | Action_Eval,
        Action_EvalAndCompile_Too   = Action_CompileTimeToo |
                                            Action_EvalAndCompile,
    }; // Action

    enum Situation
    {
        State_Compile           = 1 << 0,
        State_Load              = 1 << 1,
        State_Execute           = 1 << 2,
        State_CompileTimeToo    = 1 << 3,
    }; // Situation

    static const Action k_rgnEvalWhen[16] =
    {
                                    //    CT  LT  EX CTT    Next
        Action_Discard,             //  0 no  no  no  no
        Action_Eval,                //  1 yes no  no  no
        Action_Compile,             //  2 no  yes no  no
        Action_EvalAndCompile_Too,  //  3 yes yes no  no    CTT     *
        Action_Discard,             //  4 no  no  yes no
        Action_Eval,                //  5 yes no  yes no
        Action_Compile,             //  6 no  yes no  no
        Action_EvalAndCompile_Too,  //  7 yes yes yes no    CTT     *
        Action_Discard_Too,         //  8 no  no  no  yes   CTT
        Action_Eval_Too,            //  9 yes no  no  yes   CTT
        Action_Compile,             // 10 no  yes no  yes           *
        Action_EvalAndCompile_Too,  // 11 yes yes no  yes   CTT
        Action_Eval_Too,            // 12 no  no  yes yes   CTT
        Action_EvalAndCompile_Too,  // 13 yes no  yes yes   CTT
        Action_Compile_Too,         // 14 no  yes yes yes   CTT
        Action_EvalAndCompile_Too   // 15 yes yes yes yes   CTT
    }; // k_rgnEvalWhen

    UINT nState = 0;
    for (Val runner = second(form); ! endp(runner); runner = cdr(runner))
    {
        Val situation = car(runner);

        if (Kcompile_toplevel == situation)
        {
            nState |= State_Compile;
        }
        else if (Kload_toplevel == situation)
        {
            nState |= State_Load;
        }
        else if (Kexecute == situation)
        {
            nState |= State_Execute;
        }
        else if (Qcompile == situation)
        {
            nState |= State_Compile;
        }
        else if (Qload == situation)
        {
            nState |= State_Load;
        }
        else if (Qeval == situation)
        {
            nState |= State_Execute;
        }
        else
        {
            warn(L"Unknown situation: ~S", situation);
        }
    } // for

    Action eAction;
    Val    next_mode;
    {
        if (m_fToplevelForm && Qcompile_file == TLV(c6_AsituationA))
        {
            if (TLV(c6_Aprocessing_modeA) == Kcompile_time_too)
            {
                nState |= State_CompileTimeToo;
            }

            eAction = static_cast<Action>(
                k_rgnEvalWhen[nState] & Action_Mask );

            next_mode = k_rgnEvalWhen[nState] & Action_CompileTimeToo ?
                Kcompile_time_too :
                nil;
        }
        else
        {
            eAction = nState & State_Execute ?
                Action_Compile :
                Action_Discard;

            next_mode = TLV(c6_Aprocessing_modeA);
        } // if
    } // eAction, next_mode

    ////////////////////////////////////////////////////////////
    //
    // Does Action
    //
    BindFrameScope oLet(2);
        oLet.Bind(TLV_c6_AsituationA, nil);
        oLet.Bind(TLV_c6_Aprocessing_modeA, next_mode);

    switch (eAction)
    {
    case Action_Compile:
    action_compile:
        return parseForms(pExpect, cddr(form));

    case Action_Discard:
        return ty_void == pExpect->Type ?
            Obj_Void :
            parseLiteral(pExpect, nil);

    case Action_Eval:
    {
        Val val = funcall(Qeval, cons(Qprogn, cddr(form)));
        return ty_void == pExpect->Type ?
            Obj_Void :
            parseLiteral(pExpect, val);
    } // Action_Eval

    case Action_EvalAndCompile:
        funcall(Qeval, cons(Qprogn, cddr(form)));
        goto action_compile;

    default:
        CAN_NOT_HAPPEN();
    } // switch eAction
} // eval_when


//////////////////////////////////////////////////////////////////////
//
// locally
//
define_special_operator(locally)
{
    Context::BindingScope oLocally(
            LexEnv::Kind_locally, GetContext(), pExpect->Type );
    Val forms = parse_declarations(cdr(form));
    Expect oExpect = *pExpect;
        oExpect.Type = GetLexEnv()->GetTy();
    return parseForms(&oExpect, forms);
} // locally


//////////////////////////////////////////////////////////////////////
//
// load_time_value
//
// See 3.2.2 Minimal Compilation
//
//  compile         -- evalutate form
//  compile-form    -- arrange form to be evaluated at load time.
//
define_special_operator(load_time_value)
{
    CHECK_SYNTAX(2, 3, "(load-time-value form [read-only-p])");

    Val read_only_p = third(form);
    unless (read_only_p == nil || read_only_p == t)
    {
        ExpectArgument oExpect(Qload_time_value, 1, Qboolean);
        unexpected_type(&oExpect, type_of(read_only_p), read_only_p);
    }

    if (ty_void == pExpect->Type)
    {
        ignore_form(form);
        return emitLinkage(Obj_Void);
    }

    Val delayed = second(form);

    // Evaluate the first form as of 3.2.2 "Minimal Compilation".
    if (Qcompile_file != TLV(c6_AsituationA))
    {
        Val val = funcall(Qeval, delayed);
        return emitLinkage(NewLiteral(val));
    }

    Val cookie;
    Function* pFun;
    {
        Val funtab = m_funtab;
        Val vartab = m_vartab;

        m_ltv_counter = add_xx(m_ltv_counter, Fixnum::Encode(1));

        {
            char16 wsz[30];
                ::wsprintfW(wsz, L"ltv%u", Fixnum::Decode_(m_ltv_counter));
            cookie = make_symbol(wsz);
        } // cookie

        pFun = run(list(Qload_time_value, m_ltv_counter), ty_t, delayed);

        m_funtab = funtab;
        m_vartab = vartab;
    } // pFun

    Register* pRd = new Register;
        pRd->SetStorage(Register::Storage_LoadTimeValue);
        pRd->SetForm(form);
    emitInsn(new LoadTimeValueInsn(pRd, cookie, pFun, read_only_p));
    return emitLinkage(pRd);
} // load_time_value


//////////////////////////////////////////////////////////////////////
//
// quote
//
define_special_operator(quote)
{
    CHECK_SYNTAX(2, 2, "(quote datum)");
    return parseLiteral(pExpect, cadr(form));
} // quote


//////////////////////////////////////////////////////////////////////
//
// symbol_macrolet
//
define_special_operator(symbol_macrolet)
{
    CHECK_SYNTAX(2, MaxFormLength, "(symbol-macrolet (binding*) decl* form*)");

    Context::BindingScope oBindScope(
        LexEnv::Kind_symbol_macrolet,
        GetContext(),
        pExpect->Type );

    Val bindings = second(form);

    if (minusp_xx(safe_list_length(bindings)))
    {
        warn(L"Malformed bindings: ~S", bindings);
    }
    else
    {
        for (Val runner = bindings; nil != runner; runner = cdr(runner))
        {
            Val binding = car(runner);

            if (safe_list_length(binding) != Fixnum::Encode(2))
            {
                warn(L"Malformed binding: ~S", binding);
                continue;
            }

            Val name = first(binding);
            Val expansion = second(binding);

            if (! symbolp(name))
            {
                warn(L"Expected symbol instead of ~S.", name);
                continue;
            }

            VarDcl* pVarDcl = getVarDcl(name);
            switch (pVarDcl->GetKind())
            {
            case VarDcl::Kind_Constant:
                warn(L"Can't use constant ~S as symbol-macro.", name);
                continue;

            case VarDcl::Kind_Special:
                warn(L"Can't use special variable ~S as symbol-macro.", name);
                continue;
            } // switch kind

            addVarDcl(new VarDcl(VarDcl::Kind_SymbolMacro, name, expansion));
        } // for each binding
    } // if

    Val forms = parse_declarations(cddr(form));

    Operand* pSx = parseForms(pExpect, forms);

    return pSx;
} // symbol_macrolet


//////////////////////////////////////////////////////////////////////
//
// the
//
define_special_operator(the)
{
    CHECK_SYNTAX(3, 3, "(the type form)");

    if (ty_void == pExpect->Type)
    {
        return parseForm(pExpect, third(form));
    }

    Val the_ty = second(form);

    ExpectOperand oExpect(Qthe, 0, ty_and(the_ty, pExpect->Type));
    return parseForm(&oExpect, third(form));
} // the

} // Compiler
