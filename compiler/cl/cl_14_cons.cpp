#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 14 Conses
// compiler/cl/cl_14_cons.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_14_cons.cpp#11 $
// Description:
//  This file contains following parsers:
//      atom
//      endp
//      list
//
#include "./cl_defs.h"

namespace Compiler
{

// atom = (typep x 'atom)
define_parser(atom)
{
    CHECK_SYNTAX(2, 2, "(atom x)");
    return parseForm(pExpect, list(Qtypep, second(form), QQquote_atom));
} // atom


// endp
//  RUNTIMECAST list %r1 <= %rx
//  EQ bool %b2  <= %r1 nil
//  SELECT t %r3 <= %b2 t nil
define_parser(endp)
{
    CHECK_SYNTAX(2, 2, "(endp x)");
    if (ty_void == pExpect->Type) return ignore_args(form);

    ExpectArgument oExpect(Qendp, 0, ty_t);
    Operand* pSx = parseForm1(&oExpect, second(form));
    if (Obj_Unreachable == pSx) return useless_form(form);

    Operand* pSd;
    Register* pRx = pSx->DynamicCast<Register>();
    if (NULL != pRx)
    {
        Register* pR1 = new Register;
            emitInsn(new RuntimeCastInsn(ty_list, pR1, pRx, m_pFrame));
        Bool* pB2 = new Bool;
            emitInsn(new EqInsn(pB2, pR1, Obj_Nil));
        pSd = emitBool(pB2);
    }
    else if (pSx == Obj_Nil)
    {
        pSd = Obj_True;
    }
    else
    {
        pSd = Obj_Nil;
    }

    return emitLinkage(emitCast(pExpect, form, pSd));
} // endp


// list
define_parser(list)
{
    CHECK_SYNTAX(1, MaxFormLength, "(list x*)");
    if (nil == cdr(form)) return parseLiteral(pExpect, nil);
    if (ty_void == pExpect->Type) return ignore_args(form);
    Operand* pSx = parseFunctionFormAux(pExpect, form, getFunDcl(Qlist));
    Output* pSd = pSx->ToOutput();
    if (NULL != pSd && pSd->GetDfn()->Is<CallInsn>())
    {
        if (pSd->GetTy() == ty_list) pSd->GetDfn()->SetTy(ty_cons);
    }
    return emitLinkage(emitCast(pExpect, form, pSx));
} // list

} // Compiler
