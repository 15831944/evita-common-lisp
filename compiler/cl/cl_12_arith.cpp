#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 12 Numbers - Arithmetic
// compiler/cl/cl_12_arith.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_12_arith.cpp#10 $
// Description:
//  This file contains following parsers:
//      atom
//
#include "./cl_defs.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// 12.5.24 *
//      (*)         => 1
//      (* a)       => a
//      (* a b)     => ADD r1 <= a b
//      (* a b c)   => ADD r1 <= a b, ADD r2 <= r1 c
define_parser(Mul)
{
    CHECK_SYNTAX(1, MaxFormLength, "(* number*)");

    if (ty_void == pExpect->Type) return ignore_args(form);

    Val args = cdr(form);

    switch (Fixnum::Decode_(length(args)))
    {
    case 0:
        return parseLiteral(pExpect, Fixnum::Encode(1));

    case 1:
    {
        ExpectArgument oExpect(QMul, 0, ty_number);
        Operand* pSx = parseForm1(&oExpect, first(args));
        return emitLinkage(emitCast(pExpect, form, pSx));
    } // 1

    case 2:
    {
        Operand* pSx;
        Operand* pSy;
        ExpectArgument oExpect(QMul, 0, ty_number);
        if (! parseForm2(&oExpect, ty_number, args, &pSx, &pSy))
        {
            return useless_form(form);
        }

        Operand* pSd = emitCall(pExpect, form,
            ty_number, QMul2, pSx, pSy );

        return emitLinkage(pSd);
    } // 2

    default:
    {
        // BUGBUG: How do we handle operand 0?

        ExpectArgument oExpect(QMul, 0, ty_number);

        Operand* pSx = NewLiteral(Fixnum::Encode(1));

        foreach (EnumList, oEnum, args)
        {
            Val arg = oEnum.Get();

            Operand* pSy = parseForm1(&oExpect, arg);
            if (Obj_Unreachable == pSy) return useless_form(form);

            if (pSy->Eq(Fixnum::Encode(1))) continue;

            if (pSx->Eq(Fixnum::Encode(1)))
            {
                pSx = pSy;
            }
            else
            {
                pSx = emitCall(&oExpect, form, 
                    ty_number, QMul2, pSx, pSy );
            }
        } // for each arg

        return emitLinkage(pSx);
    } // default
    } // switch arity
} // Mul


//////////////////////////////////////////////////////////////////////
//
// 12.5.25 +
//      (+)         => 0
//      (+ a)       => a
//      (+ a b)     => ADD r1 <= a b
//      (+ a b c)   => ADD r1 <= a b, ADD r2 <= r1 c
define_parser(Add)
{
    CHECK_SYNTAX(1, MaxFormLength, "(+ number*)");

    if (ty_void == pExpect->Type) return ignore_args(form);

    Val args = cdr(form);

    switch (Fixnum::Decode_(length(args)))
    {
    case 0:
        return parseLiteral(pExpect, Fixnum::Encode(1));

    case 1:
    {
        ExpectArgument oExpect(QAdd, 0, ty_number);
        Operand* pSx = parseForm1(&oExpect, first(args));
        return emitLinkage(emitCast(pExpect, form, pSx));
    } // 1

    case 2:
    {
        Operand* pSx;
        Operand* pSy;
        ExpectArgument oExpect(QAdd, 0, ty_number);
        if (! parseForm2(&oExpect, ty_number, args, &pSx, &pSy))
        {
            return useless_form(form);
        }

        Operand* pSd = emitCall(pExpect, form,
            ty_number, QAdd2, pSx, pSy );

        return emitLinkage(pSd);
    } // 2

    default:
    {
        ExpectArgument oExpect(QAdd, 0, ty_number);

        Operand* pSx = NewLiteral(Fixnum::Encode(0));

        foreach (EnumList, oEnum, args)
        {
            Val arg = oEnum.Get();

            Operand* pSy = parseForm1(&oExpect, arg);
            if (Obj_Unreachable == pSy) return useless_form(form);

            if (pSy->Eq(Fixnum::Encode(0))) continue;

            if (pSx->Eq(Fixnum::Encode(0)))
            {
                pSx = pSy;
            }
            else
            {
                pSx = emitCall(&oExpect, form, ty_number, QAdd2, pSx, pSy);
            }
        } // for each arg

        return emitLinkage(pSx);
    } // default
    } // switch arity
} // Add


//////////////////////////////////////////////////////////////////////
//
// 12.5.26 -
//      (- a)       => (- 0 a)
//      (- a b)     => SUB r1 <= a b
//      (- a b c)   => SUB r1 <= a b, SUB r2 <= r1 c
define_parser(Sub)
{
    CHECK_SYNTAX(2, MaxFormLength, "(- number number*)");

    if (ty_void == pExpect->Type) return ignore_args(form);

    Val args = cdr(form);

    ExpectArgument oExpect(QSub, 0, ty_number);
    Operand* pSx = parseForm1(&oExpect, first(args));
        if (Obj_Unreachable == pSx) return useless_form(form);

    args = cdr(args);

    switch (Fixnum::Decode_(length(args)))
    {
    case 0:
        return emitLinkage(emitCall(&oExpect, form, 
            ty_number, Q_S1, pSx) );

    case 1:
    {
        oExpect.Nth = Fixnum::Encode(1);
        Operand* pSy = parseForm1(&oExpect, first(args));
            if (Obj_Unreachable == pSy) return useless_form(form);

        Operand* pSd = emitCall(pExpect, form,
            ty_number, QSub2, pSx, pSy );

        return emitLinkage(pSd);
    } // 1

    default:
    {
        foreach (EnumList, oEnum, args)
        {
            Val arg = oEnum.Get();

            oExpect.Nth = add_xx(oExpect.Nth, 1);

            Operand* pSy = parseForm1(&oExpect, arg);
            if (Obj_Unreachable == pSy) return useless_form(form);

            if (pSy->Eq(Fixnum::Encode(0))) continue;

            pSx = emitCall(&oExpect, form, 
                ty_number, QSub2, pSx, pSy );
        } // for each arg

        return emitLinkage(pSx);
    } // default
    } // switch arity
} // Sub


//////////////////////////////////////////////////////////////////////
//
// 12.5.27 /
//      (/ a)       => (/ 1 a)
//      (/ a b)     => DIV r1 <= a b
//      (/ a b c)   => MUL r1 <= b c,  DIV r2 <= a r1
define_parser(Div)
{
    CHECK_SYNTAX(2, MaxFormLength, "(/ number number*)");

    if (ty_void == pExpect->Type) return ignore_args(form);

    Val args = cdr(form);

    ExpectArgument oExpect(QDiv, 0, ty_number);
    Operand* pSx = parseForm1(&oExpect, first(args));
        if (Obj_Unreachable == pSx) return useless_form(form);

    args = cdr(args);

    switch (Fixnum::Decode_(length(args)))
    {
    case 0:
        return emitLinkage(emitCall(&oExpect, form, 
            ty_number, QDiv2,
                NewLiteral(Fixnum::Encode(1)),
                pSx ) );

    case 1:
    {
        oExpect.Nth = Fixnum::Encode(1);
        Operand* pSy = parseForm1(&oExpect, first(args));
            if (Obj_Unreachable == pSy) return useless_form(form);

        Operand* pSd = emitCall(pExpect, form,
            ty_number, QDiv2, pSx, pSy );

        return emitLinkage(pSd);
    } // 1

    default:
    {
        Operand* pS1 = pSx;

        Operand* pSx = parseForm1(&oExpect, pop(args));
            if (Obj_Unreachable == pSx) return useless_form(form);

        foreach (EnumList, oEnum, args)
        {
            Val arg = oEnum.Get();

            oExpect.Nth = add_xx(oExpect.Nth, 1);

            Operand* pSy = parseForm1(&oExpect, arg);
            if (Obj_Unreachable == pSy) return useless_form(form);

            if (pSy->Eq(Fixnum::Encode(1))) continue;

            pSx = emitCall(&oExpect, form, 
                ty_number, QMul2, pSx, pSy );
        } // for each arg

        Operand* pSd = emitCall(pExpect, form,
            ty_number, QDiv2, pS1, pSx );

        return emitLinkage(pSd);
    } // default
    } // switch arity
} // Div


//////////////////////////////////////////////////////////////////////
//
// 12.2.28 1-
//
define_parser(1Minus)
{
    CHECK_SYNTAX(2, 2, "(1+ number)");

    if (ty_void == pExpect->Type) return ignore_args(form);

    Operand* pSx;
    {
        ExpectArgument oExpect(Q1Plus, 0, ty_number);
        pSx = parseForm1(&oExpect, second(form));
        if (pSx->Is<Literal>())
        {
            return emitLinkage(emitCast(pExpect, form, sub(
                    pSx->StaticCast<Literal>()->GetDatum(),
                    Fixnum::Encode(1) )) );
        } // if
    } // pSx

    return emitLinkage(emitCall(pExpect, form, 
        ty_number, QSub2, pSx, Fixnum::Encode(1) ) );
} // 1Minus


//////////////////////////////////////////////////////////////////////
//
// 12.2.28 1+
//
define_parser(1Plus)
{
    CHECK_SYNTAX(2, 2, "(1+ number)");

    if (ty_void == pExpect->Type) return ignore_args(form);

    Operand* pSx;
    {
        ExpectArgument oExpect(Q1Plus, 0, ty_number);
        pSx = parseForm1(&oExpect, second(form));
        if (pSx->Is<Literal>())
        {
            return emitLinkage(emitCast(pExpect, form, add(
                    pSx->StaticCast<Literal>()->GetDatum(),
                    Fixnum::Encode(1) )) );
        } // if
    } // pSx

    return emitLinkage(emitCall(pExpect, form, 
        ty_number, QAdd2, pSx, Fixnum::Encode(1) ) );
} // 1Plus

} // Compiler
