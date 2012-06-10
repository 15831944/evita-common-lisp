#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 12 Numbers - Compare
// compiler/cl/cl_12_cmp.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_12_cmp.cpp#6 $
// Description:
//  This file contains following parsers:
//      atom
//
#include "./cl_defs.h"

namespace Compiler
{

// parse_cmp
Operand* ClParser::parse_cmp(const Expect* pExpect, Val form,
    const char16*   pwsz,
    Ty              opty,
    Val             name2 )
{
    {
        Operand* pSx = check_syntax(form, 2, MaxFormLength, pwsz);
        if (NULL != pSx) return pSx;
    }

    if (ty_void == pExpect->Type) return ignore_args(form);

    Val args = cdr(form);

    uint cArgs = static_cast<uint>(Fixnum::Decode_(
        length(args) ) );

    ExpectArgument oExpect(car(form), 0, opty);
    Operand* pSx = parseForm1(&oExpect, first(args));
        if (Obj_Unreachable == pSx) return useless_form(form);

    switch (cArgs)
    {
    case 1:
        return emitLinkage(emitCast(pExpect, form, pSx));

    case 2:
    {
        oExpect.Nth = Fixnum::Encode(1);
        Operand* pSy = parseForm1(&oExpect, second(args));
            if (Obj_Unreachable == pSy) return useless_form(form);

        Operand* pSd = emitCall(pExpect, form, ty_t, name2, pSx, pSy);

        return emitLinkage(pSd);
    } // 2
#if 0
    case 3:
    {
        Operand* pSy = parseForm1(&oExpect, second(args));
            if (Obj_Unreachable == pSy) return useless_form(form);

        Operand* pSz = parseForm1(&oExpect, third(args));
            if (Obj_Unreachable == pSz) return useless_form(form);

        Operand* pR1 = emitCall(pExpect, form, ty_t, pOp, pSx, pSy);

        BBlock* pFirst = GetContext()->GetCurr();
        BBlock* pCont  = newBBlock();
        BBlock* pJoin  = newBBlock();

        if (GetContext()->GetSucc() == pFirst)
            { GetContext()->SetSucc(pJoin); }

        Bool* pB2 = new Bool();
            emitInsn(new NeInsn(pB2, pR1, NewLiteral(nil)));
            emitInsn(new BranchInsn(pB2, pCont, pJoin));

        // pCont
        GetContext()->SetCurr(pCont);
        Operand* pR2 = emitCall(pExpect, form, ty_t, pOp, pSy, pSz);
        Bool* pB3 = new Bool();
            emitInsn(new NeInsn(pB3, pR2, NewLiteral(nil)));
        Register* pR3 = new Register();
            emitInsn(new SelectInsn(pR3, pB3, pSy, NewLiteral(nil)));
            emitInsn(new JumpInsn(pJoin));

        // pJoin
        GetContext()->SetCurr(pJoin);
        Register* pR4 = new Register();
            PhiInsn* pPhi = new PhiInsn(ty_t, pR4);
            emitInsn(pPhi);

        pPhi->AddInput(pFirst, NewLiteral(nil));
        pPhi->AddInput(pCont,  pR3);

        return emitLinkage(emitCast(pExpect, form, pR4));
    } // 3
#endif
    } // switch arity

    if (QNe2 == name2 || QCharNe2 == name2 || QCharCiNe2 == name2)
    {
        ValuesInsn* pArgs = new ValuesInsn(
            new Values(),
            NewLiteral(nil), cArgs );

        Instruction::EnumInput oEnumArg(pArgs);

        oEnumArg.GetBox()->Replace(pSx);

        foreach (EnumList, oEnum, rest(args))
        {
            oExpect.Nth = add(oExpect.Nth, 1);
            Operand* pSy = parseForm1(&oExpect, oEnum.Get());
                if (Obj_Unreachable == pSy) return useless_form(form);

            oEnumArg.Next();
            oEnumArg.GetBox()->Replace(pSy);
        } // for each arg

        emitInsn(pArgs);

        Register* pRd = new Register();
            emitInsn(new CallInsn(ty_t, pRd, NewLiteral(QNe), pArgs->GetVd()));

        return emitLinkage(emitCast(pExpect, form, pRd));
    }
    else
    {
        Register* pRd = new Register();
        PhiInsn* pPhi = new PhiInsn(ty_t, pRd);

        BBlock* pJoin = newBBlock();

        BBlock* pCurr = GetContext()->GetCurr();
        BBlock* pSucc = GetContext()->SetContinue();

        Literal* pNil = NewLiteral(nil);

        foreach (EnumList, oEnum, rest(args))
        {
            GetContext()->SetCurrSucc(pCurr, pCurr);

            BBlock* pNext = NewBBlock();
            InsertBBlock(pNext, pJoin);

            Operand* pSy = parseForm1(&oExpect, oEnum.Get());
                if (Obj_Unreachable == pSy) return useless_form(form);

            pCurr = GetContext()->GetCurr();

            Operand* pR1 = emitCall(pExpect, form, ty_t, name2, pSx, pSy);

            Bool* pBx = new Bool();
                emitInsn(new NeInsn(pBx, pR1, pNil));

            if (rest(oEnum.GetList()) == nil)
            {
                Register* pR2 = new Register();
                emitInsn(new SelectInsn(pR2, pBx, pSx, pNil));
                emitInsn(new JumpInsn(pJoin));
                pPhi->AddInput(pCurr, pR2);

                GetContext()->SetCurrSucc(pJoin, pSucc);
                emitInsn(pPhi);
                return emitLinkage(emitCast(pExpect, form, pPhi->GetRd()));
            } // if

            emitInsn(new BranchInsn(pBx, pNext, pJoin));

            pPhi->AddInput(pCurr, pNil);

            pSx = pSy;
            pCurr = pNext;
        } // for each arg

        CAN_NOT_HAPPEN();
    } // if
} // ClParser::parse_cmp


#define define_cmp_parser(mp_name, mp_opty, mp_str) \
    define_parser(mp_name) \
        { return parse_cmp(pExpect, form, mp_str, ty_##mp_opty, Q##mp_name##2); }

// 12.2.15 =, /=, <, >, <=, >=
define_cmp_parser(Eq, number, L"=")
define_cmp_parser(Ne, number, L"/=")
define_cmp_parser(Ge, real,   L">=")
define_cmp_parser(Gt, real,   L">")
define_cmp_parser(Le, real,   L"<=")
define_cmp_parser(Lt, real,   L"<")

// 13.2.5 char=, char/=, char<, char>, char<=, char>=
define_cmp_parser(CharEq, character, L"char=")
define_cmp_parser(CharNe, character, L"char/=")
define_cmp_parser(CharGe, character, L"char>=")
define_cmp_parser(CharGt, character, L"char>")
define_cmp_parser(CharLe, character, L"char<=")
define_cmp_parser(CharLt, character, L"char<")

// 13.2.5 char-equal, char-not-equal, char-lessp, char-not-greaterp,
//  char-greaterp, char-not-lessp
define_cmp_parser(CharCiEq, character, L"char-equal")
define_cmp_parser(CharCiNe, character, L"char-not-equal")
define_cmp_parser(CharCiGe, character, L"char-not-lessp")
define_cmp_parser(CharCiGt, character, L"char-greaterp")
define_cmp_parser(CharCiLe, character, L"char-not-greaterp")
define_cmp_parser(CharCiLt, character, L"char-lessp")

//////////////////////////////////////////////////////////////////////
//
// 12.2.16 max, min
//      VALUES v1 <- sx sy
//      CALL   r2 <- #'> v1
//      NE     b3 <- r2 'nil
//      SELECT r4 <- b3 sx sy
define_parser(max) { return parse_min(pExpect, form); }
define_parser(min)
{
    char16 const* const k_wszSyntax = car(form) == Qmax ?
        L"(max real real*)" : L"(min real real*)";
    {
        Operand* pSx = check_syntax(form, 2, MaxFormLength, k_wszSyntax);
        if (NULL != pSx) return pSx;
    }

    Val args = cdr(form);

    uint cArgs = static_cast<uint>(Fixnum::Decode_(
        length(args) ) );

    ExpectArgument oExpect(car(form), 0, ty_real);
    Operand* pSx = parseForm1(&oExpect, first(args));
        if (Obj_Unreachable == pSx) return useless_form(form);

    switch (cArgs)
    {
    case 1: // (max real)
        return emitLinkage(emitCast(pExpect, form, pSx));

    case 2: // (max real real)
    {
        oExpect.Nth = Fixnum::Encode(1);
        Operand* pSy = parseForm1(&oExpect, second(args));
            if (Obj_Unreachable == pSy) return useless_form(form);

        Operand* pOp = NewLiteral(first(form) == Qmax ? QGt2 : QLt2);

        Values* pV1 = new Values();
            emitInsn(new ValuesInsn(pV1, pSx, pSy));
        Register* pR2 = new Register();
            emitInsn(new CallInsn(ty_t, pR2, pOp, pV1));
        Bool* pB3 = new Bool();
            emitInsn(new NeInsn(pB3, pR2, NewLiteral(nil)));
        Register* pR4 = new Register();
            emitInsn(new SelectInsn(pR4, pB3, pSx, pSy));
        return emitLinkage(emitCast(pExpect, form, pR4));
    } // 2

    default:
    {
        ValuesInsn* pArgs = new ValuesInsn(
            new Values(),
            NewLiteral(nil), cArgs );

        Instruction::EnumInput oEnumArg(pArgs);

        oEnumArg.GetBox()->Replace(pSx);

        foreach (EnumList, oEnum, rest(args))
        {
            oExpect.Nth = add(oExpect.Nth, 1);
            Operand* pSy = parseForm1(&oExpect, oEnum.Get());
                if (Obj_Unreachable == pSy) return useless_form(form);

            oEnumArg.Next();
            oEnumArg.GetBox()->Replace(pSy);
        } // for each arg

        emitInsn(pArgs);

        Literal* pOp = NewLiteral(first(form));   // max/min
        Register* pRd = new Register();
            emitInsn(new CallInsn(ty_real, pRd, pOp, pArgs->GetVd()));
        return emitLinkage(emitCast(pExpect, form, pRd));
    } // default
    } // switch cArgs
} // ClParser::parse_max

// 12.2.18 minusp
define_parser(minusp)
{
    CHECK_SYNTAX(2, 2, "(minusp number");
    ExpectArgument oExpect(Qminusp, 0, ty_real);
    Operand* pSx = parseForm1(&oExpect, second(form));
    if (Obj_Unreachable == pSx) return useless_form(form);
    return emitLinkage(emitCall(pExpect, form, ty_t, QLt2,
        pSx, Fixnum::Encode(0) ) );
} // minusp

// 12.2.18 plusp
define_parser(plusp)
{
    CHECK_SYNTAX(2, 2, "(plusp number");
    ExpectArgument oExpect(Qplusp, 0, ty_real);
    Operand* pSx = parseForm1(&oExpect, second(form));
    if (Obj_Unreachable == pSx) return useless_form(form);
    return emitLinkage(emitCall(pExpect, form, ty_t, QGt2,
        pSx, Fixnum::Encode(0) ) );
} // plusp

// 12.2.18 zerop
define_parser(zerop)
{
    CHECK_SYNTAX(2, 2, "(zerop number");
    ExpectArgument oExpect(Qzerop, 0, ty_number);
    Operand* pSx = parseForm1(&oExpect, second(form));
    if (Obj_Unreachable == pSx) return useless_form(form);
    return emitLinkage(emitCall(pExpect, form, ty_t, QEq2,
        pSx, Fixnum::Encode(0) ) );
} // zerop

} // Compiler
