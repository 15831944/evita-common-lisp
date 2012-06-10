#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 3 Evaluation and Compilation
// compiler/cl/cl_03_call.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_03_call.cpp#15 $
//
#include "./cl_defs.h"
#include "../cm/cm_base.h"
#include "../ir/ir_instruction.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ClParser::parseArgs
//
Operand*
ClParser::parseArgs(Val form, Callee* pCallee, Val args)
{
    // BUGBUG: We must allocate vector for keeping arguments
    Operand* rgpArg[100];
    UINT cArgs = 0;

    bool fApply = car(form) == Qapply;

    ExpectArgument oExpect(ir_get_callee_name(pCallee->m_pSx), 0, ty_t);

    tyArgsIterator oArgs(pCallee->m_ty);

    for (Val runner = args; ! endp(runner); runner = cdr(runner))
    {
        if (tyIterator::State_End == oArgs.GetState())
        {
            warn(L"Too many arguments for ~S.", oExpect.Name);
            break;
        }

        if (fApply && cdr(runner) == nil)
        {
            oExpect.Type = Qlist;
            oExpect.Nth = Fixnum::Encode(cArgs);
        }
        else
        {
            switch (oArgs.GetState())
            {
            case tyIterator::State_KeyName:
                oExpect.Context = ExpectContext_KeyName;
                oExpect.Type    = ty_symbol;
                break;

            case tyIterator::State_KeyVal:
                break;

            default:
                oExpect.Type = oArgs.GetTy();
                oExpect.Nth = Fixnum::Encode(cArgs);
                break;
            } // switch state
        } // if

        #if 0
        {
            log_format(3, L"Function ~S: arg[~D] ~S<br>~%",
                oExpect.Name,
                Fixnum::Encode(cArgs),
                oExpect.Type );
        }
        #endif

        Operand* pSx = parseForm1(&oExpect, car(runner));
        if (Obj_Unreachable == pSx)
        {
            oExpect.Context = ExpectContext_Unreachable;
            return parseForms(&oExpect, cdr(runner));
        }

        if (lengthof(rgpArg) == cArgs)
        {
            warn(L"Run out argument storage: ~S", form);
        }
        else
        {
            rgpArg[cArgs] = pSx;
        }

        cArgs += 1;

        if (oArgs.GetState() == tyIterator::State_KeyName)
        {
            if (! pSx->Is<Literal>())
            {
                oExpect.Context = ExpectContext_Argument;
            }
            else
            {
                oExpect.Context = ExpectContext_KeyVal;
                Val key = pSx->StaticCast<Literal>()->GetDatum();
                oExpect.Nth     = key;
                oExpect.Type    = oArgs.GetKeyTy(key);

                if (nil == oExpect.Type)
                {
                    warn(L"Function ~S doesn't take keyword ~S.",
                        oExpect.Name,
                        key );

                    oExpect.Type = ty_t;
                }
            }
        } // if

        oArgs.Next();
    } // for each arg

    Values* pVx = new Values();
    if (fApply)
    {
        emitInsn(new ValuesAInsn(pVx, rgpArg, cArgs));
    }
    else
    {
        switch (oArgs.GetState())
        {
        case tyIterator::State_Req:
            warn(L"Too few argument for ~S.", oExpect.Name);
            break;

        case tyIterator::State_KeyVal:
            warn(L"Odd number of keyword arguments.");
            break;
        } // switch state

        emitInsn(new ValuesInsn(pVx, rgpArg, cArgs));
    }

    return pVx;
} // ClParser::parseArgs


//////////////////////////////////////////////////////////////////////
//
// ClParser::parseCall
//
//  Called by:
//      ClParser::parse_apply
//      ClParser::parse_funcall
//
Operand*
ClParser::parseCall(const Expect* pExpect, Val form, Callee* pCallee, Val args)
{
    // BUGBUG: NYI: check compiler-macro and parser
    Operand* pSx = parseArgs(form, pCallee, args);
        if (Obj_Unreachable == pSx) return useless_form(form);

    Values* pArgs = pSx->StaticCast<Values>();
    return emitLinkage(emitCall(pExpect, form, pCallee, pArgs));
} // ClParser::parseCall


//////////////////////////////////////////////////////////////////////
//
// ClParser::parseFunctionFormAux
//
Operand*
ClParser::parseFunctionForm(const Expect* pExpect, Val form, FunDcl* pFunDcl)
{
    FunPcl* pFunPcl = pFunDcl->DynamicCast<FunPcl>();
    if (NULL != pFunPcl &&
        ! pFunPcl->GetFlag(FunDcl::Flag_NotInline) )
    {
        // compiler-macro
        {
            Val expander = cdr(assq(Kcompiler_macro, pFunPcl->m_alist));
            if (nil != expander)
            {
                Val expansion = callMacroExpander(expander, form);
                if (nil != expansion && form != expansion)
                {
                    return parseForm(pExpect, expansion);
                }
            }
        }

        // parser
        {
            ParserT pfn = s_oParserTable.Get(pFunPcl->GetName());
            if (NULL != pfn)
            {
                return (this->*pfn)(pExpect, form);
            }
        }

        // type-predicate
        {
            Val type = cdr(assq(Qtype_predicate, pFunPcl->m_alist));
            if (nil != type)
            {
                return parseTypePredicate(pExpect, form, type);
            }
        }
    } // if

    return emitLinkage(parseFunctionFormAux(pExpect, form, pFunDcl));
} // ClParser::parseFunctionForm


//////////////////////////////////////////////////////////////////////
//
// ClParser::parseFunctionFormAux
//
Operand*
ClParser::parseFunctionFormAux(
    const Expect*   pExpect,
    Val             form,
    FunDcl*         pFunDcl )
{
    Callee oCallee;
        oCallee.m_ty  = pFunDcl->GetTy();
        oCallee.m_fNotInline = pFunDcl->GetFlag(FunDcl::Flag_NotInline);
    {
        FunDcl* pRunner = pFunDcl;
        for (;;)
        {
            if (pRunner->Is<FunDef>())
            {
                FunDef* pFunDef = pRunner->StaticCast<FunDef>();

                oCallee.m_pSx = pFunDef->GetFunction();

                mergeUpVars(pFunDef->GetFunction());
                markFunUse(pFunDcl);
                break;
            }

            if (pRunner->Is<FunPcl>())
            {
                FunPcl* pFunPcl = pRunner->StaticCast<FunPcl>();
                oCallee.m_pSx = NewLiteral(pFunPcl->GetName());
                break;
            }

            pRunner = pRunner->GetOuter();
        } // for
    } // oCallee

    Operand* pSx = parseArgs(form, &oCallee, cdr(form));

    if (Obj_Unreachable == pSx) return useless_form(form);

    Values* pArgs = pSx->DynamicCast<Values>();
    if (NULL == pArgs) return emitLinkage(Obj_Unreachable);
    return emitCall(pExpect, form, &oCallee, pArgs);
} // ClParser::parseFunctionFormAux


static bool
is_well_known_callee(Val name)
{
    if (setf_cell_p(name))
    {
        name = setf_cell_name(name);
    }

    Val package = symbol_package(name);
    return package == PACKAGE_cl ||
           package == PACKAGE_si ||
           package == PACKAGE_clos;
} // is_well_known_callee


//////////////////////////////////////////////////////////////////////
//
// ClParser::parseCallee
//
//  Called by:
//      ClParser::parse_apply
//      ClParser::parse_funcall
//      ClParser::parse_multiple_value_call
//
Operand*
ClParser::parseCallee(Val form, Callee* pCallee)
{
    ASSERT(NULL != pCallee);

    if (safe_list_length(form) == Fixnum::Encode(2))
    {
        Val fname = second(form);

        if (Qquote == first(form) && symbolp(fname))
        {
            Val name = fname;
            FunPcl* pFunPcl = getFunPcl(name);
            switch (pFunPcl->GetKind())
            {
            case FunPcl::Kind_Function:
                pCallee->m_ty = pFunPcl->GetTy();

                pCallee->m_fNotInline =
                    pFunPcl->GetFlag(FunPcl::Flag_NotInline);

                return pCallee->m_pSx = NewLiteral(name);

            case FunPcl::Kind_Undef:
                return pCallee->m_pSx = NewLiteral(name);

            default:
                pCallee->m_pSx = NewLiteral(name);
                return not_function(name);
            } // switch kind
        } // quote

        if (Qfunction == car(form) && function_name_p(fname))
        {
            Val name = fname;
            if (consp(fname))
            {
                name = intern_setf_cell(second(fname));
            }

            FunDcl* pFunDcl = getFunDcl(name);

            pCallee->m_ty = pFunDcl->GetTy();

            pCallee->m_fNotInline =
                pFunDcl->GetFlag(FunDcl::Flag_NotInline);

            if (pFunDcl->Is<FunPcl>() &&
                pFunDcl->GetKind() == FunDcl::Kind_Function)
            {
                if (is_well_known_callee(name))
                {
                    return pCallee->m_pSx = NewLiteral(name);
                }
            }
        } // function
    } // if

    Expect oExpect(ExpectContext_Callee, ty_function_designator);
    return pCallee->m_pSx = parseForm1(&oExpect, form); 
} // ClParser::parseCallee

} // Compiler
