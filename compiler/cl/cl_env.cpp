#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - Context
// cl_Context.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_env.cpp#14 $
//
// Description:
//  This file contains environment related methods:
//      activateFunDcl
//      activateFreeDcls
//      activateVarDcl
//      activateLexEnv
//      addFunDcl
//      addVarDcl
//      closeLexEnv
//      getFunDcl
//      getVarDcl
//      isSpecialVariable
//      markFunUse
//      markVarUse
//      newVariable
//
#include "./cl_defs.h"
#include "../cm/cm_base.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ClParser::activateFunDcl
//
//  For flet
//
ClParser::FunDcl*
ClParser::activateFunDcl(FunDcl* pFunDcl)
{
    Val outer = gethash(pFunDcl->GetName(), m_funtab);

    if (nil == outer)
    {
        pFunDcl->SetOuter(NULL);
    }
    else
    {
        FunDcl* pOuter = outer->StaticCast<FunDcl>();
        if (pOuter->GetLexEnv() == pFunDcl->GetLexEnv())
        {
            // There is multiple declarations for same name.
            pOuter = pOuter->GetOuter();
        }
        pFunDcl->SetOuter(pOuter);
    } // if

    setf_gethash(Fixnum::Encode(pFunDcl), pFunDcl->GetName(), m_funtab);

    return pFunDcl;
} // ClParser::activateFunDcl


//////////////////////////////////////////////////////////////////////
//
// activateFreeDcls
//
void
ClParser::activateFreeDcls()
{
    LexEnv* pLexEnv = GetLexEnv();

    // restore variable declarations
    foreach (LexEnv::EnumFun, oEnum, pLexEnv)
    {
        FunDcl* pFunDcl = oEnum.Get();
        if (! pFunDcl->Is<FunDef>())
        {
            activateFunDcl(pFunDcl);
        }
    } // for each FunDcl

    // restore variable declarations
    foreach (LexEnv::EnumVar, oEnum, pLexEnv)
    {
        VarDcl* pVarDcl = oEnum.Get();
        if (! pVarDcl->Is<VarDef>())
        {
            activateVarDcl(pVarDcl);
        }
    } // for each VarDcl
} // ClParser::activateFreeDcls


//////////////////////////////////////////////////////////////////////
//
// activateLexEnv
//
void
ClParser::activateLexEnv()
{
    LexEnv* pLexEnv = GetLexEnv();

    // restore variable declarations
    foreach (LexEnv::EnumFun, oEnum, pLexEnv)
    {
        activateFunDcl(oEnum.Get());
    } // for each FunDcl

    // restore variable declarations
    foreach (LexEnv::EnumVar, oEnum, pLexEnv)
    {
        activateVarDcl(oEnum.Get());
    } // for each VarDcl
} // ClParser::activateLexEnv


//////////////////////////////////////////////////////////////////////
//
// ClParser::activateVarDcl
//
//  For let*
//
ClParser::VarDcl*
ClParser::activateVarDcl(VarDcl* pVarDcl)
{
    Val outer = gethash(pVarDcl->GetName(), m_vartab);

    if (nil == outer)
    {
        pVarDcl->SetOuter(NULL);
    }
    else
    {
        VarDcl* pOuter = outer->StaticCast<VarDcl>();
        if (pOuter->GetLexEnv() == pVarDcl->GetLexEnv())
        {
            // There is multiple declarations for same name.
            pOuter = pOuter->GetOuter();
        }

        pVarDcl->SetOuter(pOuter);
    } // if

    setf_gethash(Fixnum::Encode(pVarDcl), pVarDcl->GetName(), m_vartab);

    return pVarDcl;
} // ClParser::activateVarDcl


//////////////////////////////////////////////////////////////////////
//
// addFunDcl
//
ClParser::FunDcl*
ClParser::addFunDcl(FunDcl* pFunDcl)
{
    LexEnv* pLexEnv = GetLexEnv();
    pLexEnv->Add(pFunDcl);
    pFunDcl->SetLexEnv(pLexEnv);
    return activateFunDcl(pFunDcl);
} // ClParser::addFunDcl


//////////////////////////////////////////////////////////////////////
//
// addVarDcl
//
ClParser::VarDcl*
ClParser::addVarDcl(VarDcl* pVarDcl)
{
    LexEnv* pLexEnv = GetLexEnv();
    pLexEnv->Add(pVarDcl);
    pVarDcl->SetLexEnv(pLexEnv);
    return activateVarDcl(pVarDcl);
} // ClParser::addVarDcl

enum ExitKind
{
    ExitKind_Normal,
    ExitKind_Void,
    ExitKind_NoReturn,
}; // ExitKind

//////////////////////////////////////////////////////////////////////
//
// classify_exit
//  Returns one of following values based on function exit:
//      Exit_KindNormal     function returns normally
//      Exit_NoReturn       function doesn't return.
//      Exit_Void           all call sites ignore function value.
//
//  If pFun has no call sites, pFun is not used function or value of
//  FLET/LABLES.
static ExitKind classify_exit(Function* pFun)
{
    if (pFun->HasUseSite())          return ExitKind_Normal;
    if (! pFun->HasCallSite())       return ExitKind_Normal;

    ExitKind eExitKind = ExitKind_Void;

    foreach (Function::EnumCallSite, oEnum, pFun)
    {
        Instruction* pCall = oEnum.Get()->GetInstruction();

        if (pCall->GetOutput() != Obj_Void)
        {
            eExitKind = ExitKind_Normal;
            break;
        }
    } // for each call site

    foreach (BBlock::EnumInEdge, oEnum,  pFun->GetExitBB())
    {
        BBlock* pBBlock = oEnum.Get()->GetFrom();

        RetInsn* pRet = pBBlock->GetLastInsn()->
            DynamicCast<RetInsn>();

        if (NULL != pRet) return eExitKind;
    } // for each pred

    return ExitKind_NoReturn;
} // classify_exit


// optimize_exit
//  1. Inserts UNREACHABLE if function doesn't return normally.
//  2. If all call sites ignores value of function, we set void to
//     operand of all RET instructions.
static void optimize_exit(Function* pFun)
{
    switch (classify_exit(pFun))
    {
    case ExitKind_NoReturn:
        ty_set_function_value(pFun->GetTy(), nil);

        foreach (Function::EnumCallSite, oEnum, pFun)
        {
            CallInsn* pCall = oEnum.Get()->GetInstruction()->
                StaticCast<CallInsn>();

            Instruction* pLast = pCall->GetBBlock()->GetLastInsn();

            // Remove insns between CALL and JUMP.
            {
                Instruction* pRunner = pCall->GetNext();
                while (pRunner != pLast)
                {
                    Instruction* pNext = pRunner->GetNext();
                    ir_remove_insn(pRunner);
                    pRunner = pNext;
                } // for each insn
            }

            foreach (Frame::Enum, oEnum, pCall->GetFrame())
            {
                Frame* pFrame = oEnum.Get();
                if (pFrame->GetOwner() != pFun) continue;
                ir_insert_insn(new UseInsn(pFrame), pLast);
            } // for each frame

            ir_replace_insn(new UnreachableInsn(), pLast);
        } // for each call site
        break;

    case ExitKind_Void:
        ty_set_function_value(pFun->GetTy(), ty_void);

        foreach (BBlock::EnumInEdge, oEnum,  pFun->GetExitBB())
        {
            BBlock* pBBlock = oEnum.Get()->GetFrom();

            RetInsn* pRet = pBBlock->GetLastInsn()->
                DynamicCast<RetInsn>();

            if (NULL == pRet) continue;

            pRet->GetOperandBox(0)->Replace(Obj_Void);
        } // for each pred
        break;
    } // ExitKind_Void
} // optimize_exit


//////////////////////////////////////////////////////////////////////
//
// closeLexEnv
//
// Note: Optimize_VARDEF don't remove VARDEF since VARDEF+SLOT+LOAD is
// used by PHI or RET.
//
// Note: If we don't call Optimize_VARDEF here, code size is increased.
//
void
ClParser::closeLexEnv()
{
    LexEnv* pLexEnv = GetLexEnv();

    foreach (LexEnv::EnumFun, oEnum, pLexEnv)
    {
        FunDef* pFunDef = oEnum.Get()->DynamicCast<FunDef>();

        if (NULL == pFunDef) continue;

        if (FunDef::Usage_None == pFunDef->GetUsage())
        {
            Val fname = pFunDef->GetName();
            Val name = fname;
            if (setf_cell_p(fname))
            {
                name = setf_cell_name(fname);
                fname = list(Qsetf, name);
            }

            if (nil != symbol_package(name))
            {
                style_warn(L"Function ~S isn't used.", fname);
            }

            continue;
        }

        optimize_exit(pFunDef->GetFunction());
    } // for each FunDcl

    foreach (LexEnv::EnumVar, oEnum, pLexEnv)
    {
        VarDef* pVarDef = oEnum.Get()->DynamicCast<VarDef>();

        if (NULL != pVarDef && VarDef::Kind_Lexical == pVarDef->GetKind())
        {
            if (VarDef::Usage_None == pVarDef->GetUsage())
            {
                Val name = pVarDef->GetName();
                if (nil != symbol_package(name))
                {
                    style_warn(L"Variable ~S isn't used.", name);
                }
            }
        }
    } // for each VarDcl

    deactivateLexEnv();
} // ClParser::closeLexEnv


//////////////////////////////////////////////////////////////////////
//
// deactivateLexEnv
//
void
ClParser::deactivateLexEnv()
{
    LexEnv* pLexEnv = GetLexEnv();

    // restore variable declarations
    {
        Val htb = m_funtab;

        foreach (LexEnv::EnumFun, oEnum, pLexEnv)
        {
            FunDcl* pFunDcl = oEnum.Get();
            Val fundcl = Fixnum::Encode(pFunDcl->GetOuter());
            if (Fixnum::Encode(0) == fundcl)
            {
                fundcl = nil;
            }
            setf_gethash(fundcl, pFunDcl->GetName(), htb);
        } // for each FunDcl
    }

    // restore variable declarations
    {
        Val htb = m_vartab;

        foreach (LexEnv::EnumVar, oEnum, pLexEnv)
        {
            VarDcl* pVarDcl = oEnum.Get();
            Val vardcl = Fixnum::Encode(pVarDcl->GetOuter());
            if (Fixnum::Encode(0) == vardcl)
            {
                vardcl = nil;
            }
            setf_gethash(vardcl, pVarDcl->GetName(), htb);
        } // for each VarDcl
    }
} // ClParser::deactivateLexEnv


//////////////////////////////////////////////////////////////////////
//
// getFunDcl
//
ClParser::FunDcl*
ClParser::getFunDcl(Val name)
{
    ASSERT(symbolp(name) || setf_cell_p(name));

    {
        Val fundcl = gethash(name, m_funtab);

        if (nil != fundcl) return fundcl->StaticCast<FunDcl>();
    }

    return getFunPcl(name);
} // ClParser::getFunDcl


//////////////////////////////////////////////////////////////////////
//
// getFunPcl
//
ClParser::FunPcl*
ClParser::getFunPcl(Val name)
{
    ASSERT(symbolp(name) || setf_cell_p(name));

    // Search in lexical function table.
    {
        Val fundcl = gethash(name, m_funtab);
        if (nil != fundcl)
        {
            FunDcl* pFunDcl = fundcl->StaticCast<FunDcl>();
            do
            {
                if (pFunDcl->Is<FunPcl>())
                {
                    return pFunDcl->StaticCast<FunPcl>();
                }

                pFunDcl = pFunDcl->GetOuter();
            } while (NULL != pFunDcl);
        }
    }

    Val env = TLV(AenvironmentA);
    do
    {
        Environment* pEnv = env->Decode<Environment>();

        with_shared_latch(pEnv->m_latch);

        Val frob = gethash(name, pEnv->m_functions);
        if (nil != frob) return getFunPclAux(name, frob);

        env = pEnv->m_outer;
    } while (nil != env);

    return getFunPclAux(name, nil);
} // ClParser::getFunPcl


//////////////////////////////////////////////////////////////////////
//
// ClParser::getFunPclAux
//
ClParser::FunPcl*
ClParser::getFunPclAux(Val name, Val frob)
{
    FunPcl::Kind eKind;
    Val datum;
    Val funty;
    Val alist = cdr(frob);
    if (Kfunction == car(frob))
    {
        funty = cdr(assq(Qftype, alist));
        if (nil == funty)
        {
            funty = symbolp(name) ? ty_function : ty_setf_function;
        }
        else
        {
            funty = ty_expand_funty(funty);
        }

        eKind = FunPcl::Kind_Function;
        datum = alist;
    }
    else if (Kmacro == car(frob))
    {
        eKind = FunPcl::Kind_Macro;
        datum = cdr(assq(Kmacro, alist));
        funty = nil;
    }
    else if (Kspecial_operator == car(frob))
    {
        eKind = FunPcl::Kind_SpecialOperator;
        datum = cdr(assq(Kspecial_operator, alist));
        funty = nil;
    }
    else
    {
        // undefined function or compiler-macro
        eKind  = FunPcl::Kind_Undef;
        datum  = nil;
        funty = symbolp(name) ? ty_function : ty_setf_function;
    }

    FunPcl* pFunPcl = new FunPcl(eKind, name, datum, funty);

    if (nil != cdr(assq(Qnotinline, alist)))
    {
        pFunPcl->SetFlags(FunPcl::Flag_NotInline);
    }

    pFunPcl->m_alist = alist;

    // Put into m_funtab
    {
        Val fundcl = gethash(name, m_funtab);
        if (nil == fundcl)
        {
            setf_gethash(Fixnum::Encode(pFunPcl), name, m_funtab);
        }
        else
        {
            FunDcl* pFunDcl = fundcl->StaticCast<FunDcl>();
            while (NULL != pFunDcl->GetOuter())
            {
                pFunDcl = pFunDcl->GetOuter();
            } // while

            pFunDcl->SetOuter(pFunPcl);
        }
    }

    return pFunPcl;
} // ClParser::getFunPclAux


//////////////////////////////////////////////////////////////////////
//
// getVarDcl
//
ClParser::VarDcl*
ClParser::getVarDcl(Val name)
{
    ASSERT(symbolp(name));

    {
        Val vardcl = gethash(name, m_vartab);

        if (nil != vardcl)
        {
            return vardcl->StaticCast<VarDcl>();
        }
    }

    {
        Val env = TLV(AenvironmentA);
        do
        {
            Environment* pEnv = env->Decode<Environment>();

            with_shared_latch(pEnv->m_latch);

            Val frob = gethash(name, pEnv->m_variables);
            if (nil != frob) return getVarDclAux(name, frob);
            env = pEnv->m_outer;
        } while (nil != env);
    }

    return getVarDclAux(name, nil);
} // ClParser::getVarDcl

ClParser::VarDcl*
ClParser::getVarDclAux(Val name, Val frob)
{
    Val varty = ty_t;
    VarDcl::Kind eKind;
    Val datum;
    {
        if (nil == frob)
        {
            eKind = VarDcl::Kind_Undef;
            datum = nil;
        }
        else if (Kconstant == car(frob))
        {
            eKind = VarDcl::Kind_Constant;
            datum = cdr(assq(Kconstant, cdr(frob)));
            varty = type_of(datum);
        }
        else if (Kspecial == car(frob))
        {
            eKind = VarDcl::Kind_Special;
            datum = cdr(assq(Qtlv, cdr(frob)));
            Val pair = assq(Qtype, cdr(frob));
            if (nil != pair) varty = cdr(pair);
        }
        else if (Ksymbol_macro == car(frob))
        {
            eKind = VarDcl::Kind_SymbolMacro;
            datum = cdr(assq(Ksymbol_macro, cdr(frob)));
        }
        else
        {
            error(L"Broken environment for ~S", name);
            // NOTREACHED
        }
    } // eKind

    VarPcl* pVarPcl = new VarPcl(eKind, name, datum, varty);
    setf_gethash(Fixnum::Encode(pVarPcl), name, m_vartab);
    return pVarPcl;
} // ClParser::getVarDclAux


//////////////////////////////////////////////////////////////////////
//
// ClParser::isSpecialVariable
//
bool
ClParser::isSpecialVariable(Val name)
{
    for (
        VarDcl* pRunner = getVarDcl(name);
        NULL != pRunner;
        pRunner = pRunner->GetOuter() )
    {
        if (! pRunner->Is<VarDef>() && pRunner->IsSpecial())
        {
            return true;
        }
    } // for

    return false;
} // ClParser::isSpecialVariable


//////////////////////////////////////////////////////////////////////
//
// ClParser::mapFunUse
//
void
ClParser::markFunUse(FunDcl* pFunDcl)
{
    switch (pFunDcl->GetUsage())
    {
    case FunDcl::Usage_None:
    case FunDcl::Usage_Ignorable:
    case FunDcl::Usage_Used:
        break;

    case FunDcl::Usage_Ignore:
    {
        Val fname = pFunDcl->GetName();
        if (setf_cell_p(fname))
        {
            fname = list(Qsetf, value_cell_value(fname));
        }
        style_warn(L"Use ignored function ~S.", fname);
        break;
    } // FunDcl::Usage_Ignore

    default:
        CAN_NOT_HAPPEN();
    } // switch usage

    pFunDcl->SetUsage(FunDcl::Usage_Used);

    for (
        FunDcl* pRunner = pFunDcl;
        NULL != pRunner;
        pRunner = pRunner->GetOuter() )
    {
        if (pRunner->Is<FunDef>())
        {
            pRunner->SetUsage(FunDcl::Usage_Used);
            break;
        }
    } // for
} // ClParser::markFunUse


//////////////////////////////////////////////////////////////////////
//
// ClParser::marVarUse
//
void
ClParser::markVarUse(VarDcl* pVarDcl)
{
    switch (pVarDcl->GetUsage())
    {
    case VarDcl::Usage_None:
    case VarDcl::Usage_Ignorable:
    case VarDcl::Usage_Used:
        break;

    case VarDcl::Usage_Ignore:
        style_warn(L"Use ignored variable ~S.", pVarDcl->GetName());
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch usage

    pVarDcl->SetUsage(VarDcl::Usage_Used);

    for (
        VarDcl* pRunner = pVarDcl;
        NULL != pRunner;
        pRunner = pRunner->GetOuter() )
    {
        if (pRunner->Is<VarDef>())
        {
            pRunner->SetUsage(VarDcl::Usage_Used);
            break;
        }
    } // for
} // ClParser::markVarUse


//////////////////////////////////////////////////////////////////////
//
// ClParser::newVariable
//
Variable*
ClParser::newVariable(Val name)
{
    Variable* pVar = new Variable(name);
    return pVar;
} // ClParser::newVariable

} // Compiler
