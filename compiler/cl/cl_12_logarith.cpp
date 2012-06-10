#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 12 Numbers - Arithmetic
// compiler/cl/cl_12_arith.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_12_logarith.cpp#2 $
// Description:
//  This file contains following parsers:
//      atom
//
#include "./cl_defs.h"

namespace Compiler
{

// 12.2.62 logand, logeqv, logior, logxor
define_parser(logand) { return parse_logxor(pExpect, form); }
define_parser(logeqv) { return parse_logxor(pExpect, form); }
define_parser(logior) { return parse_logxor(pExpect, form); }
define_parser(logxor)
{
    struct Desc
    {
        const char16*   m_pwsz;
        Val             m_identity;
        Val             m_op2;
    }; // Desc

    Val op = first(form);

    Desc oDesc;
    {
        if (op == Qlogand)
        {
            oDesc.m_pwsz     = L"(logand integer*)";
            oDesc.m_identity = Fixnum::Encode(-1);
            oDesc.m_op2      = QlogandS2;
        }
        else if (op == Qlogeqv)
        {
            oDesc.m_pwsz     = L"(logeqv integer*)";
            oDesc.m_identity = Fixnum::Encode(-1);
            oDesc.m_op2      = QlogeqvS2;
        }
        else if (op == Qlogior)
        {
            oDesc.m_pwsz     = L"(logior integer*)";
            oDesc.m_identity = Fixnum::Encode(0);
            oDesc.m_op2      = QlogiorS2;
        }
        else if (op == Qlogxor)
        {
            oDesc.m_pwsz     = L"(logxor integer*)";
            oDesc.m_identity = Fixnum::Encode(0);
            oDesc.m_op2      = QlogxorS2;
        }
        else
        {
            CAN_NOT_HAPPEN();
        }
    } // oDesc

    {
        Operand* pSx = check_syntax(form, 1, MaxFormLength, oDesc.m_pwsz);
        if (NULL != pSx) return pSx;
    }

    Val args = rest(form);

    uint cArgs = static_cast<uint>(Fixnum::Decode_(
        length(args) ) );

    if (0 == cArgs)
    {
        return emitLinkage(emitCast(pExpect, form, oDesc.m_identity));
    } // if no args

    ExpectArgument oExpect(op, 0, ty_integer);
    Operand* pSx = parseForm1(&oExpect, first(args));
        if (Obj_Unreachable == pSx) return useless_form(form);

    Literal* pOp = NewLiteral(oDesc.m_op2);

    switch (Fixnum::Decode_(length(args)))
    {
    case 1: // (logand x)
        return emitLinkage(emitCast(pExpect, form, pSx));

    case 2: // (logand x y)
    {
        oExpect.Nth = Fixnum::Encode(1);
        Operand* pSy = parseForm1(&oExpect, second(args));
            if (Obj_Unreachable == pSy) return useless_form(form);
        Values* pVy = new Values();
            emitInsn(new ValuesInsn(pVy, pSx, pSy));
        Register* pRd = new Register();
            emitInsn(new CallInsn(ty_integer, pRd, pOp, pVy));
        return emitLinkage(emitCast(pExpect, form, pRd));
    } // 2

    default:
    {
        foreach (EnumList, oEnum, rest(args))
        {
            oExpect.Nth = add(oExpect.Nth, 1);
            Operand* pSy = parseForm1(&oExpect, oEnum.Get());
                if (Obj_Unreachable == pSy) return useless_form(form);
            Values* pVy = new Values();
                emitInsn(new ValuesInsn(pVy, pSx, pSy));
            Register* pRd = new Register();
                emitInsn(new CallInsn(ty_integer, pRd, pOp, pVy));
            pSx = pRd;
        } // for each arg
        return emitLinkage(emitCast(pExpect, form, pSx));
    } // default
    } // switch arity
} // logxor


// 12.2.68 logandc1, logandc2, lognand, lognor, logorc1, logorc2
define_parser(logandc1) { return parse_logorc2(pExpect, form); }
define_parser(logandc2) { return parse_logorc2(pExpect, form); }
define_parser(lognand)  { return parse_logorc2(pExpect, form); }
define_parser(lognor)   { return parse_logorc2(pExpect, form); }
define_parser(logorc1)  { return parse_logorc2(pExpect, form); }
define_parser(logorc2)
{
    Val op = first(form);

    {
        const char16* const pwsz =
            op == Qlogandc1 ? L"(logandc1 integer1 integer2)" :
            op == Qlogandc2 ? L"(logandc2 integer1 integer2)" :
            op == Qlognand  ? L"(lognand integer1 integer2)" :
            op == Qlognor   ? L"(lognor integer1 integer2)" :
            op == Qlogorc1  ? L"(logorc1 integer1 integer2)" :
            op == Qlogorc2  ? L"(logorc2 integer1 integer2)" : L"";
        Operand* pSx = check_syntax(form, 3, 3, pwsz);
        if (NULL != pSx) return pSx;
    }

    Operand* pSx;
    Operand* pSy;
    ExpectArgument oExpect(op, 0, ty_integer);
    if (! parseForm2(&oExpect, ty_integer, cdr(form), &pSx, &pSy))
        { return useless_form(form); }

    Literal* pOp = NewLiteral(op);

    Values* pVy = new Values();
        emitInsn(new ValuesInsn(pVy, pSx, pSy));
    Register* pRd = new Register();
        emitInsn(new CallInsn(ty_integer, pRd, pOp, pVy));
    return emitLinkage(emitCast(pExpect, form, pRd));
} // logorc2


// 12.2.68 lognot
define_parser(lognot)
{
    CHECK_SYNTAX(2, 2, "(lognot integer)");

    ExpectArgument oExpect(Qlognot, 0, ty_integer);
    Operand* pSx = parseForm1(&oExpect, second(form));
        if (Obj_Unreachable == pSx) return useless_form(form);

    Literal* pOp = NewLiteral(QlogxorS2);

    Values* pVy = new Values();
        emitInsn(new ValuesInsn(pVy, pSx, NewLiteral(Fixnum::Encode(-1))));
    Register* pRd = new Register();
        emitInsn(new CallInsn(ty_integer, pRd, pOp, pVy));
    return emitLinkage(emitCast(pExpect, form, pRd));
} // lognot

} // Compiler
