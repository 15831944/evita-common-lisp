#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 5 Data and Control Flow
// compiler/cl/cl_05_data.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_05_data.cpp#11 $
// Description:
//  This file contains following parsers:
//      apply
//      eq
//      eql
//      function
//      not
//
#include "./cl_defs.h"

namespace Compiler
{

static bool
is_unportable_eq(Operand* pSx)
{
    Literal* pLx = pSx->DynamicCast<Literal>();
    if (NULL == pLx)
    {
        return false;
    }

    Val datum = pLx->GetDatum();

    return fixnump(datum) || characterp(datum);
} // is_unportable_eq


static bool
is_eq(Operand* pSx)
{
    Literal* pLx = pSx->DynamicCast<Literal>();
    if (NULL == pLx)
    {
        // We don't know actual type of pSx.
        return false;
    }

    Val x = pLx->GetDatum();

    if (fixnump(x)) return true;

    return ! numberp(pLx->GetDatum());
} // is_eq


// apply
define_parser(apply)
{
    CHECK_SYNTAX(3, MaxFormLength, "(apply fn form+)");

    Callee oCallee;
    if (Obj_Unreachable == parseCallee(second(form), &oCallee))
    {
        return useless_form(form);
    }

    return parseCall(pExpect, form, &oCallee, cddr(form));
} // apply


// eq
define_parser(eq)
{
    CHECK_SYNTAX(3, 3, "(eq x y)");

    Operand* pSx;
    Operand* pSy;
    ExpectArgument oExpect(Qeq, 0, ty_t);
    if (! parseForm2(&oExpect, ty_t, cdr(form), &pSx, &pSy))
    {
        return useless_form(form);
    }

    if (is_unportable_eq(pSx) || is_unportable_eq(pSy))
    {
        unportable_form(form);
    }

    return parseEqAux(pExpect, form, pSx, pSy);
} // eq


//////////////////////////////////////////////////////////////////////
//
// ClParser::parseEqAux
//
Operand*
ClParser::parseEqAux(
    const Expect*   pExpect,
    Val             form,
    Operand*        pSx,
    Operand*        pSy )
{
    if (ty_void == pExpect->Type)
    {
        useless_form(form);
        return emitLinkage(Obj_Void);
    }

    if (pSx->Is<Literal>() && pSy->Is<Literal>())
    {
        bool fEq =
            pSx->StaticCast<Literal>()->GetDatum() ==
            pSy->StaticCast<Literal>()->GetDatum();
        return emitLinkage(NewLiteral(fEq ? t : nil));
    }

    Bool* pBx = new Bool();
    emitInsn(new EqInsn(pBx, pSx, pSy));
    return emitLinkage(emitBool(pBx));
} // ClParser::parseEqAux


//////////////////////////////////////////////////////////////////////
//
// eql
//
define_parser(eql)
{
    CHECK_SYNTAX(3, 3, "(eql x y)");

    Operand* pSx;
    Operand* pSy;
    {
        ExpectArgument oExpect(Qeql, 0, ty_t);

        if (! parseForm2(&oExpect, ty_t, cdr(form), &pSx, &pSy))
        {
            return useless_form(form);
        }
    } // pSx, pSy

    if (is_eq(pSx) || is_eq(pSy))
    {
        return parseEqAux(pExpect, form, pSx, pSy);
    }
    else
    {
        return emitLinkage(
            parseFunctionFormAux(pExpect, form, getFunDcl(Qeql)) );
    }
} // eql


//////////////////////////////////////////////////////////////////////
//
// funcall
//
define_parser(funcall)
{
    CHECK_SYNTAX(2, MaxFormLength, "(funcall fn form*)");

    Callee oCallee;
    if (Obj_Unreachable == parseCallee(second(form), &oCallee))
    {
        return useless_form(form);
    }

    if (! oCallee.m_fNotInline && oCallee.m_pSx->Is<Literal>())
    {
        // BUGBUG: NYI: Call compiler-macro
        {
            Val name = oCallee.m_pSx->StaticCast<Literal>()->GetDatum();
            ParserT pfn = s_oParserTable.Get(name);
            if (NULL != pfn)
            {
                if (symbolp(name)) form = cdr(form);
                return (this->*pfn)(pExpect, form);
            }
        }
    } // if

    return parseCall(pExpect, form, &oCallee, cddr(form));
} // funcall


//////////////////////////////////////////////////////////////////////
//
// not
//
define_parser(not)
{
    CHECK_SYNTAX(2, 2, "(not x)");

    if (ty_void == pExpect->Type) return ignore_args(form);

    ExpectArgument oExpect(Qnot, 0, ty_t);
    Operand* pSx = parseForm1(&oExpect, second(form));
    if (Obj_Unreachable == pSx) return useless_form(form);

    Bool* pBx = new Bool();
    emitInsn(new EqInsn(pBx, pSx, Obj_Nil));

    Register* pRd = new Register();
    emitInsn(new SelectInsn(pRd, pBx, NewLiteral(t), Obj_Nil));

    return emitLinkage(pRd);
} // not

} // Compiler
