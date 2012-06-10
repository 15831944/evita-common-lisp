#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction cmp
// ir/instruction/ir_insn_cmp.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_cmp.cpp#15 $
//
#include "./ir_instruction.h"

#include "./ir_fns.h"

#include "../../big/big_lisp.h"


namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// propagate_not
//
static bool propagate_not(Bool* pBx, Bool* pBold)
{
    unless (pBx->HasUseSite()) return false;

    if (pBx == Bool_True)
    {
        ir_replace_all_users(Bool_False, pBold);
        return true;
    }

    if (pBx == Bool_False)
    {
        ir_replace_all_users(Bool_True, pBold);
        return true;
    }

    Bool::EnumUseSite oEnum(pBold);
    while (! oEnum.AtEnd())
    {
        OperandBox* pBox = oEnum.Get();
        Instruction* pUser = pBox->GetInstruction();
            oEnum.Next();

        switch (pUser->GetOpcode())
        {
        case IrOp_BRANCH:
        {
            pBox->Replace(pBx);
            {
                Operand* pTrue  = pUser->GetSy();
                Operand* pFalse = pUser->GetSz();
                pUser->GetOperandBox(1)->SetOperand(pFalse);
                pUser->GetOperandBox(2)->SetOperand(pTrue);
            }
            break;
        } // branch

        case IrOp_SELECT:
        {
            pBox->Replace(pBx);

            Operand* pTrue  = pUser->GetSy();
            Operand* pFalse = pUser->GetSz();
            pUser->GetOperandBox(1)->Replace(pFalse);
            pUser->GetOperandBox(2)->Replace(pTrue);
            break;
        } // select

        case IrOp_TRAPIF:
            ir_replace_insn(
                new TrapIfNotInsn(pBx, pUser->GetLy(), pUser->GetVz()),
                pUser );
            break;

        case IrOp_TRAPIFNOT:
            ir_replace_insn(
                new TrapIfInsn(pBx, pUser->GetLy(), pUser->GetVz()),
                pUser );
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch opcode
    } // for each user

    return true;
} // propage_not


//////////////////////////////////////////////////////////////////////
//
// EqInsn::Simplify
//
//      SELECT t %r13 <= %b12 't 'nil
//      EQ bool %b14 <= %r13 'nil
//  ==>
//      (not %b12)
//
//
//      BOX (integer -2147483648 2147483647) %r51 <- %r509
//      EQ bool %b52 <- %r51 '0
//  ==>
//      EQ bool %b52 <- %r509 0
//
bool EqInsn::Simplify()
{
    if (Instruction::Simplify()) return true;

    Register* pRx = GetRx();
    if (NULL == pRx) return false;

    if (! GetSy()->Is<Literal>()) return false;

    if (GetLy() == nil)
    {
        SelectInsn* pSelect = pRx->GetDfn()->DynamicCast<SelectInsn>();
            if (NULL == pSelect) return false;

        if (! pSelect->GetSz()->Is<Literal>()) return false;
        if (pSelect->GetLz() != nil) return false;
        if (ir_can_nil(pSelect->GetSy())) return false;

        return propagate_not(pSelect->GetBx(), GetBd());
    }
    else if (GetLy() == Fixnum::Encode(0))
    {
         Instruction* pBox = pRx->GetDfn()->DynamicCast<BoxInsn>();
         if (NULL != pBox &&
             pBox->GetRx()->GetClass() == Register::Class_GPR )
         {
            GetOperandBox(0)->Replace(pBox->GetRx());
            GetOperandBox(1)->Replace(NewInteger(0));
            return true;
         } /// if box
    } // if

    return false;
} // EqInsn::Simplify


//////////////////////////////////////////////////////////////////////
//
// EqInsn::SimplifyOutputAux
//
//  [1] %sx == %sy  => true
//  [2] lx == ly    => true
//
Operand* EqInsn::SimplifyOutputAux() const
{
    // [1]
    if (GetSx() == GetSy()) return Bool_True;

    // [2]
    if (GetSx()->Is<Literal>() && GetSy()->Is<Literal>())
        { return GetLx() == GetLy() ? Bool_True : Bool_False; }

    return GetOutput();
} // EqInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// GtInsn::SimplifyOutputAux
//
#define define_simplify(mp_Name, mp_op) \
    Operand* mp_Name##Insn::SimplifyOutputAux() const \
    { \
        if (GetSx()->Is<Literal>() && GetSy()->Is<Literal>()) \
        { \
            return cmp(GetLx(), GetLy()) mp_op 0 ? \
                Bool_True : Bool_False; \
        } \
        return GetOutput(); \
    } // XxInsn::SimplifyOutputAux

define_simplify(Ge, >=)
define_simplify(Gt, >)
define_simplify(Le, <=)
define_simplify(Lt, <)


//////////////////////////////////////////////////////////////////////
//
// NeInsn::Simplify
//
//
//    SELECT %r1 <= %b2 'nil 't
//    NE     %b3 <= %r1 'nil
//  ==>
//    (not %b2)
//
bool NeInsn::Simplify()
{
    if (Instruction::Simplify()) return true;

    Register* pRx = GetRx();
        if (NULL == pRx) return false;

    if (! GetSy()->Is<Literal>()) return false;

    if (GetLy() == nil)
    {
        SelectInsn* pSelect = pRx->GetDfn()->DynamicCast<SelectInsn>();
            if (NULL == pSelect) return false;

        if (! pSelect->GetSy()->Is<Literal>()) return false;
        if (pSelect->GetLy() != nil) return false;
        if (ir_can_nil(pSelect->GetSz())) return false;

        return propagate_not(pSelect->GetBx(), GetBd());
    }
    else if (GetLy() == Fixnum::Encode(0))
    {
         Instruction* pBox = pRx->GetDfn()->DynamicCast<BoxInsn>();
         if (NULL != pBox &&
             pBox->GetRx()->GetClass() == Register::Class_GPR )
         {
            GetOperandBox(0)->Replace(pBox->GetRx());
            GetOperandBox(1)->Replace(NewInteger(0));
            return true;
         } /// if box
    } // if

    return false;
} // NeInsn::Simplify


//////////////////////////////////////////////////////////////////////
//
// NeInsn::SimplifyOutputAux
//
//  For output of if.
//    SELECT %r1 <= %b2 't 'nil
//    NE     %b3 <= %r1 'nil
//  ==>
//    %b2
//
Operand* NeInsn::SimplifyOutputAux() const
{
    if (GetSx() == GetSy()) return Bool_False;

    if (GetSx()->Is<Literal>() && GetSy()->Is<Literal>())
    {
        return GetLx() != GetLy() ? Bool_True : Bool_False;
    }

    if (GetSx()->Is<Register>() &&
        GetSy()->Is<Literal>() &&
        GetLy() == nil &&
        GetRx()->GetDfn()->Is<SelectInsn>() )
    {
        Instruction* pSelect = GetRx()->GetDfn();

        if (pSelect->GetSz()->Is<Literal>() &&
            pSelect->GetLz() == nil &&
            ! ir_can_nil(pSelect->GetSy()) )
        {
            html_log_format(1, L"~S:~S + ~S:~S~:%",
                GetBBlock(), this, pSelect->GetBBlock(), pSelect );

            return pSelect->GetSx();
        }
    }

    return GetOutput();
} // NeInsn::SimplifyOutputAux

#define DEFINE_CMP(mp_Name, mp_Swap) \
    class Op##mp_Name \
    { \
        public: static Instruction* NewInsn(Bool* b, Operand* x, Operand* y) \
            { return new mp_Name##Insn(b, x, y); } \
        public: static Instruction* NewSwapInsn( \
                    Bool* b, Operand* x, Operand* y) \
            { return new mp_Swap##Insn(b, x, y); } \
    }; \
    Instruction* new##mp_Name##Insn( \
        Bool* pBd, Operand* pSx, Operand* pSy ) \
            { return new_cmp_insn<Op##mp_Name>(pBd, pSx, pSy); }


template<class Op_>
Instruction* new_cmp_insn(Bool* pBd, Operand* pSx, Operand* pSy)
{
    if (pSx->Is<Register>())
        { return Op_::NewInsn(pBd, pSx, pSy); }
    else
        { return Op_::NewSwapInsn(pBd, pSy, pSx); }
} // new_cmp_insn

DEFINE_CMP(Eq, Eq)
DEFINE_CMP(Ge, Le)
DEFINE_CMP(Gt, Lt)
DEFINE_CMP(Le, Ge)
DEFINE_CMP(Lt, Gt)
DEFINE_CMP(Ne, Ne)

} // Compiler
