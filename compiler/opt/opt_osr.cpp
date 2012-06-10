#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - opt - Operator Strength Reduction
// compiler/opt/opt_call.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/opt/opt_osr.cpp#21 $
//
//
// Description:
//  This file contains "Self-Tail Call Elimination" pass.
//
#include "./opt_defs.h"

#include "../ir/ir_bblock.h"
#include "../ir/ir_fundb.h"
#include "../ir/ir_instruction.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// Entry of Strength Reduction Table
//
struct Entry
{
    Val m_fname;
    bool (*m_pfn)(Instruction*);

    static const Entry sm_rgoEntry[];
}; // Entry


// apply_float_contagion
//  Note: Foramt of floating-pointer number depends on target machine. We
//  can't convert rational to float here.
//
static void apply_float_contagion(
    Instruction*    pInsn,
    Ty              ty,
    Operand**       inout_pSx,
    Ty              ty1 )
{
    if (ty1 == ty) return;

    Values* pVy = new Values();
    if (ty == ty_double_float)
    {
        ir_insert_insn(
            new ValuesInsn(pVy, *inout_pSx, NewLiteral(VAR(pi))),
            pInsn );
    }
    else
    {
        ir_insert_insn(
            new ValuesInsn(pVy, *inout_pSx),
            pInsn );
    }

    Register* pRx = new Register();
    ir_insert_insn(
        new CallInsn(ty, pRx, NewLiteral(Qfloat), pVy), 
        pInsn );

    *inout_pSx = pRx;
} // apply_float_contagion


// apply_float_contagion
static Ty apply_float_contagion(
    Instruction*    pInsn,
    Ty              ty1,
    Operand**       inout_pSx,
    Ty              ty2,
    Operand**       inout_pSy )
{
    if (ty1 == ty_double_float || ty2 == ty_double_float)
    {
        if (Subtypep_Yes != ty_subtypep(ty1, Qreal)) return nil;
        if (Subtypep_Yes != ty_subtypep(ty2, Qreal)) return nil;

        apply_float_contagion(pInsn, ty_double_float, inout_pSx, ty1);
        apply_float_contagion(pInsn, ty_double_float, inout_pSy, ty2);
        return ty_double_float;
    }

    if (ty1 == ty_single_float || ty2 == ty_single_float)
    {
        if (Subtypep_Yes != ty_subtypep(ty1, Qreal)) return nil;
        if (Subtypep_Yes != ty_subtypep(ty2, Qreal)) return nil;

        apply_float_contagion(pInsn, ty_single_float, inout_pSx, ty1);
        apply_float_contagion(pInsn, ty_single_float, inout_pSy, ty2);
        return ty_single_float;
    }

    return nil;
} // apply_float_contagion


//////////////////////////////////////////////////////////////////////
//
// emit_UNBOX
//
//  Used by:
//      opt_strred
//
//  BOX   ty1 %rd <- %sx
//  UNBOX ty2 %fd <- %rd
//  ==>
//  %sx
//
static Operand* emit_UNBOX(Ty ty, Operand* pSx, Instruction* pRef)
{
    Register* pRx = NULL;

    switch (pSx->GetKind())
    {
    case Operand::Kind_Literal:
    {
        Val val = pSx->StaticCast<Literal>()->GetDatum();

        if (ty == ty_int32)
        {
            if (fixnump(val))
            {
                return NewInteger(Fixnum::Decode_(val));
            }

            if (val->Is<Bignum>() &&
                val->Decode<Bignum>()->m_length == Fixnum::Encode(1) )
            {
                return NewInteger(val->Decode<Bignum>()->m_rgBigit[0]);
            }

            warn(L"Unexpected operand for UNBOX int32: ~S", val);
            return NewLiteral(nil);
        } // int32
        break;
    } // literal

    case Operand::Kind_Register:
    {
        pRx = pSx->StaticCast<Register>();

        if (pRx->GetExtension<Register>())
        {
            return pRx->GetExtension<Register>();
        }

        Instruction* pDfn = pRx->GetDfn();
        while (pDfn->Is<SigmaInsn>())
        {
            pDfn = pDfn->GetRx()->GetDfn();
        } // while
        if (pDfn->Is<BoxInsn>() && pDfn->GetSx()->GetTy() == ty)
        {
            return pDfn->GetSx();
        }
        break;
    } // register

    default:
        warn(L"Unexpected operand for UNBOX: ~S", ty);
        return NewLiteral(nil);
    } // switch operand

    Register* pR2 = new Register(
        NULL,
        ty == ty_float64 ? Register::Class_FPR :
        ty == ty_float32 ? Register::Class_FPR : Register::Class_GPR );

    if (NULL != pRx)
    {
        pRx->SetExtension<Register>(pR2);
        pRef = pRx->GetDfn()->GetNext();
        while (pRef->Is<PhiInsn>())
        {
            pRef = pRef->GetNext();
        }
    } // if

    ir_insert_insn(new UnboxInsn(ty, pR2, pSx), pRef);

    return pR2;
} // emit_UNBOX


// ensure_arg1
static Operand* ensure_arg1(Instruction* p, Ty ty)
{
    ValuesInsn* pArgs = p->GetVy()->GetDfn()->DynamicCast<ValuesInsn>();
        if (NULL != pArgs) return pArgs->GetSx();

    Register* pRx = new Register();
    ir_insert_insn(new ProjectInsn(ty, pRx, p->GetVy(), 0), p);
    return pRx;
} // ensure_arg1


// ensure_args2
static void ensure_args2(
    Instruction*    p,
    Ty              ty1,
    Operand**       inout_pSx,
    Ty              ty2,
    Operand**       inout_pSy )
{
    if (NULL != *inout_pSx)
    {
        // We've already compute arguments.
        ASSERT(NULL != *inout_pSy);
        return;
    }

    Values* pVy = p->GetVy();

    ValuesInsn* pArgs = pVy->GetDfn()->DynamicCast<ValuesInsn>();
    if (NULL != pArgs)
    {
        Operand* pSx = pArgs->GetSx(); *inout_pSx = pSx;
        Operand* pSy = pArgs->GetSy(); *inout_pSy = pSy;

        if (! ty_equal(pSx->GetTy(), ty1))
        {
            warn(L"opt-OSR: arg[0]: %r~D ~S != ~S",
                pSx->Is<Register>() ? 
                    pSx->StaticCast<Register>()->GetName() :
                    Fixnum::Encode(-1),
                pSx->GetTy(), ty1 );
        }

        if (! ty_equal(pSy->GetTy(), ty2))
        {
            warn(L"opt-OSR: arg[1]: %r~D ~S != ~S",
                pSy->Is<Register>() ? 
                    pSy->StaticCast<Register>()->GetName() :
                    Fixnum::Encode(-1),
                pSy->GetTy(), ty2 );
        }
    }
    else
    {
        Register* pRx = new Register(); *inout_pSx = pRx;
        Register* pRy = new Register(); *inout_pSy = pRy;

        ir_insert_insn(new ProjectInsn(ty1, pRx, pVy, 0), p);
        ir_insert_insn(new ProjectInsn(ty2, pRy, pVy, 1), p);

        style_warn(L"Strength Reduction on APPLY.");
    } // if
} // ensure_args2


// replace_to_box
//  Replaces instruction with box instructions.
static void replace_to_box(Ty ty, Instruction* pOpInsn, Instruction* pInsn)
{
    ir_insert_insn(pOpInsn, pInsn);

    Register* pRd = pInsn->GetRd();

    ir_replace_insn(
        new BoxInsn(ty, pRd, pOpInsn->GetRd()),
        pInsn );
} // replace_to_box


//////////////////////////////////////////////////////////////////////
//
// process_arith2_
//  for process_logand2
//  for process_logior2
//  for process_logxor2
//  for process_Sub2
//  for process_Sub2
//  for process_Add2
//
//  Description:
//    If both operands are same type and machine compatible type, we
//    transforms CALL instruction into ADD/SUB.
//
//      double-float    UNBOX+UNBOX+ADD+BOX
//      single-float    UNBOX+UNBOX+ADD+BOX
//      integer         ADD
//
template<class Op_>
bool process_arith2_(Instruction* p)
{
    Ty argty = p->GetVy()->GetTy();

    Ty ty1 = ty_nth(argty, 0);
    Ty ty2 = ty_nth(argty, 1);
    Ty ty3 = p->GetTy();

    Operand* pSx = NULL;
    Operand* pSy = NULL;
        ensure_args2(p, ty1, &pSx, ty2, &pSy);

    Ty ty = apply_float_contagion(p, ty1, &pSx, ty2, &pSy);
    if (nil != ty) ty1 = ty2 = ty;

    if (ty_double_float == ty1 && ty_double_float == ty2)
    {
        Operand* pFx = emit_UNBOX(ty_float64, pSx, p);
        Operand* pFy = emit_UNBOX(ty_float64, pSy, p);
        Register* pFd = new FpRegister();
        replace_to_box(
            ty_double_float,
            Op_::NewInsn(ty_float64, pFd, pFx, pFy),
            p );
        return true;
    } // if double-float

    if (ty_single_float == ty1 && ty_single_float == ty2)
    {
        Operand* pFx = emit_UNBOX(ty_float32, pSx, p);
        Operand* pFy = emit_UNBOX(ty_float32, pSy, p);
        Register* pFd = new FpRegister();
        replace_to_box(
            ty_single_float,
            Op_::NewInsn(ty_float32, pFd, pFx, pFy),
            p );
        return true;
    } // if single-float

    TyInteger oTy3 = TyInteger::Parse(ty3);
        if (! oTy3.IsValid()) return false;

    TyInteger oTy2 = TyInteger::Parse(ty2);
        if (! oTy2.IsValid()) return false;

    TyInteger oTy1 = TyInteger::Parse(ty1);
        if (! oTy1.IsValid()) return false;

    // fixnum
    if (IsSubtype(oTy1, TyInteger::Fixnum) &&
        IsSubtype(oTy3, TyInteger::Fixnum) &&
        IsSubtype(oTy2, TyInteger::Fixnum) )
    {
        Register* pRd = p->GetRd();
        ir_replace_insn(Op_::NewInsn(ty3, pRd, pSx, pSy), p);
        return true;
    } // fixnum

    // int32
    if (IsSubtype(oTy1, TyInteger::SignedByte32) &&
        IsSubtype(oTy3, TyInteger::SignedByte32) &&
        IsSubtype(oTy2, TyInteger::SignedByte32) )
    {
        Operand* pFx = emit_UNBOX(ty_int32, pSx, p);
        Operand* pFy = emit_UNBOX(ty_int32, pSy, p);
        Register* pFd = new Register();
        replace_to_box(
            ty_signed_byte_32_,
            Op_::NewInsn(ty_int32, pFd, pFx, pFy),
            p );
        return true;
    } // int32

    return false;
} // process_arith2_

#define DEFINE_ARITH2(mp_Name, mp_Op) \
    class Op##mp_Name \
    { \
        public: static Instruction* NewInsn( \
                Ty ty, Register* pRd, Operand* pSx, Operand* pSy ) \
            { return new mp_Op##Insn(ty, pRd, pSx, pSy); } \
    }; \
    static bool process_##mp_Name##2(Instruction* p) \
        { return process_arith2_<Op##mp_Name>(p); }


//////////////////////////////////////////////////////////////////////
//
// process_ash
//
static bool process_ash(Instruction* p)
{
    Ty argty = p->GetVy()->GetTy();
    Ty tyN = ty_nth(argty, 0);
    Ty tyK = ty_nth(argty, 1);

    TyInteger oTyK = TyInteger::Parse(tyK);
    if (! oTyK.IsValid()) return false;
    if (! oTyK.IsSingle()) return false;

    Val k = oTyK.m_lower.Get();
    if (! fixnump(k)) return false;

    // (ash n 0) => n
    if (k == Fixnum::Encode(0))
    {
        ir_replace_all_users(ensure_arg1(p, tyN), p->GetRd());
        return true;
    }

    TyInteger oTyN = TyInteger::Parse(tyN);
    if (! oTyN.IsValid()) return false;
    if (oTyN.IsSingle())
    {
        Val n = oTyN.m_lower.Get();
        ir_replace_all_users(NewLiteral(ash(n, k)), p->GetRd());
        return true;
    }

    Val lower = IntvEdge__MinusInf;
    Val upper = IntvEdge__PlusInf;

    if (oTyN.m_lower.Get() != IntvEdge__MinusInf)
    {
        lower = ash(oTyN.m_lower.Get(), k);
    }

    if (oTyN.m_upper.Get() != IntvEdge__PlusInf)
    {
        upper = ash(oTyN.m_upper.Get(), k);
    }

    TyInteger oTyR = TyInteger(lower, upper);

    if (IsSubtype(oTyN, TyInteger::Fixnum) &&
        IsSubtype(oTyR, TyInteger::Fixnum) )
    {
        Register* pRd = p->GetRd();
        Operand*  pSx = ensure_arg1(p, tyN);
        Ty ty = oTyR.Unparse();
        if (plusp(k))
        {
            ir_replace_insn(
                new ShlInsn(ty, pRd, pSx, NewLiteral(k)),
                p );
        }
        else
        {
            Val k2 = Fixnum::Encode(-Fixnum::Decode_(k));
            ir_replace_insn(
                new ShrInsn(ty, pRd, pSx, NewLiteral(k2)),
                p );
        }

        return true;
    } // fixnum

    return false;
} // process_ash


//////////////////////////////////////////////////////////////////////
//
//  process_cmp2_
//      CALL t  %r1 <- >=/2 sx sy
//      NE bool %b2 <- %r1 nil
//
//  For =/2 and /=/2, if one of operand fixnum and another operand is 
//  a rational, we transform CALL into EQ/NE.
//
template<class Op_>
bool process_cmp2_(Instruction* p)
{
    Ty argty = p->GetVy()->GetTy();
    Ty ty1 = ty_nth(argty, 0);
    Ty ty2 = ty_nth(argty, 1);

    Ty ty;

    if (ty_double_float == ty1 && ty_double_float == ty2)
    {
        ty = ty_float64;
    }
    else if (ty_single_float == ty1 && ty_single_float == ty2)
    {
        ty = ty_float32;
    }
    else
    {
        TyInteger oTy1 = TyInteger::Parse(ty1);
        TyInteger oTy2 = TyInteger::Parse(ty2);
        if (oTy1.IsValid() && oTy2.IsValid())
        {
            if (IsSubtype(oTy1, TyInteger::Fixnum) &&
                IsSubtype(oTy2, TyInteger::Fixnum) )
            {
                ty = nil;
            }
            else if (IsSubtype(oTy1, TyInteger::SignedByte32) &&
                     IsSubtype(oTy2, TyInteger::SignedByte32) )
            {
                ty = ty_int32;
            }
            else
            {
                return false;
            }
        }
        else if (p->GetLx() == QEq2 || p->GetLx() == QNe2)
        {
            // Check equality between rational and fixnum.
            if (Subtypep_Yes == ty_subtypep(ty1, ty_rational) &&
                Subtypep_Yes == ty_subtypep(ty2, ty_fixnum) )
            {
                ty = nil;
            }
            else if (Subtypep_Yes == ty_subtypep(ty1, ty_fixnum) &&
                     Subtypep_Yes == ty_subtypep(ty2, ty_rational) )
            {
                ty = nil;
            }
            else
            {
                return false;
            }
        }
        else
        {
            return false;
        }
    } // if

    bool fChanged = false;

    Operand* pSx = NULL;
    Operand* pSy = NULL;

    foreach (Register::EnumUseSite, oEnum, p->GetRd())
    {
        Instruction* pCmp = oEnum.Get()->GetInstruction();
        switch (pCmp->GetOpcode())
        {
        case IrOp_EQ:
        {
            if (pCmp->GetSx()->Is<Literal>() && pCmp->GetLy() != nil) break;

            ensure_args2(p, ty1, &pSx, ty2, &pSy);

            if (! fChanged && ty != nil)
            {
                pSx = emit_UNBOX(ty, pSx, pCmp);
                pSy = emit_UNBOX(ty, pSy, pCmp);
            }

            Bool* pBd = new Bool();
                ir_insert_insn(Op_::NewNotInsn(pBd, pSx, pSy), pCmp);

            ir_replace_all_users(pBd, pCmp->GetBd());
            fChanged = true;
            break;
        } // eq

        case IrOp_NE:
        {
            if (pCmp->GetSx()->Is<Literal>() && pCmp->GetLy() != nil) break;

            ensure_args2(p, ty1, &pSx, ty2, &pSy);

            if (! fChanged && ty != nil)
            {
                pSx = emit_UNBOX(ty, pSx, pCmp);
                pSy = emit_UNBOX(ty, pSy, pCmp);
            }

            Bool* pBd = new Bool();
                ir_insert_insn(Op_::NewInsn(pBd, pSx, pSy), pCmp);

            ir_replace_all_users(pBd, pCmp->GetBd());
            fChanged = true;
            break;
        } // ne
        } // switch opcode
    } // for each use

    ensure_args2(p, ty1, &pSx, ty2, &pSy);

    if (! fChanged && ty != nil)
    {
        pSx = emit_UNBOX(ty, pSx, p);
        pSy = emit_UNBOX(ty, pSy, p);
    }

    Bool* pBx = new Bool();
        ir_insert_insn(Op_::NewInsn(pBx, pSx, pSy), p);

    ir_replace_insn(
        new SelectInsn(p->GetRd(), pBx, Obj_True, Obj_Nil),
        p );

    return true;
} // process_cmp2


#define DEFINE_CMP2(mp_Name, mp_Not) \
    class Op##mp_Name \
    { \
        public: static Instruction* NewInsn(Bool* b, Operand* x, Operand* y) \
            { return new##mp_Name##Insn(b, x, y); } \
        public: static Instruction* NewNotInsn( \
                    Bool* b, Operand* x, Operand* y) \
            { return new##mp_Not##Insn(b, x, y); } \
    }; \
    static bool process_##mp_Name##2(Instruction* p) \
        { return process_cmp2_<Op##mp_Name>(p); }

DEFINE_ARITH2(logand,   LogAnd)
DEFINE_ARITH2(logior,   LogIor)
DEFINE_ARITH2(logxor,   LogXor)

DEFINE_ARITH2(Add, Add)
DEFINE_ARITH2(Sub, Sub)

DEFINE_CMP2(Ge, Lt)
DEFINE_CMP2(Gt, Le)
DEFINE_CMP2(Le, Gt)
DEFINE_CMP2(Lt, Ge)
DEFINE_CMP2(Eq, Ne)
DEFINE_CMP2(Ne, Ne)


//////////////////////////////////////////////////////////////////////
//
// process_char_cmp2
//
//  If both operands are character, we use integer comparision
//  instruction.
//
//      CALL %rd <- char=/2 %sx %sy
//    =>
//      NE %bd <- %sx %sy
//      SELECT %rd <- %sx nil
//
template<class Op_> bool
process_char_cmp2_(Instruction* p)
{
    Ty argty = p->GetVy()->GetTy();
    if (Qcharacter != ty_nth(argty, 0)) return false;
    if (Qcharacter != ty_nth(argty, 1)) return false;

    Operand* pSx = NULL;
    Operand* pSy = NULL;
        ensure_args2(p, Qcharacter, &pSx, Qcharacter, &pSy);

    Bool* pBx = new Bool();
    ir_insert_insn(Op_::NewInsn(pBx, pSx, pSy), p);
    ir_replace_insn(new SelectInsn(p->GetRd(), pBx, pSx, Obj_Nil), p);

    return true;
} // process_char_cmp2


#define DEFINE_CHAR_CMP2(mp_Name, mp_Not) \
    class OpChar##mp_Name \
    { \
        public: static Instruction* NewInsn(Bool* b, Operand* x, Operand* y) \
            { return new##mp_Name##Insn(b, x, y); } \
    }; \
    static bool process_Char##mp_Name##2(Instruction* p) \
        { return process_char_cmp2_<OpChar##mp_Name>(p); }


DEFINE_CHAR_CMP2(Ge, Lt)
DEFINE_CHAR_CMP2(Gt, Le)
DEFINE_CHAR_CMP2(Le, Gt)
DEFINE_CHAR_CMP2(Lt, Ge)
DEFINE_CHAR_CMP2(Eq, Ne)
DEFINE_CHAR_CMP2(Ne, Ne)


//////////////////////////////////////////////////////////////////////
//
// process_Div2
//
// Description:
//  If both operands are single-float or double-float, we transform CALL
//  instruction into UNBOX+UNBOX+MUL+BOX.
//
static bool process_Div2(Instruction* p)
{
    Ty argty = p->GetVy()->GetTy();

    Ty ty1 = ty_nth(argty, 0);
    Ty ty2 = ty_nth(argty, 1);
    //Ty ty3 = p->GetTy();

    Operand* pSx = NULL;
    Operand* pSy = NULL;
        ensure_args2(p, ty1, &pSx, ty2, &pSy);

    Ty ty = apply_float_contagion(p, ty1, &pSx, ty2, &pSy);
    if (nil != ty) ty1 = ty2 = ty;

    if (ty_double_float == ty1 && ty_double_float == ty2)
    {
        Operand* pFx = emit_UNBOX(ty_float64, pSx, p);
        Operand* pFy = emit_UNBOX(ty_float64, pSy, p);
        Register* pFd = new FpRegister();
        replace_to_box(
            ty_double_float,
            new DivInsn(ty_float64, pFd, pFx, pFy),
            p );

        return true;
    } // if double-float

    if (ty_single_float == ty1 && ty_single_float == ty2)
    {
        Operand* pFx = emit_UNBOX(ty_float32, pSx, p);
        Operand* pFy = emit_UNBOX(ty_float32, pSy, p);
        Register* pFd = new FpRegister();
        replace_to_box(
            ty_single_float,
            new DivInsn(ty_float32, pFd, pFx, pFy),
            p );

        return true;
    } // if single-float

    return false;
} // process_Div2


//////////////////////////////////////////////////////////////////////
//
// process_Mul2
//
// Description:
//  If both operands are single-float or double-float, we transform CALL
//  instruction into UNBOX+UNBOX+MUL+BOX.
//
static bool process_Mul2(Instruction* p)
{
    Ty argty = p->GetVy()->GetTy();

    Ty ty1 = ty_nth(argty, 0);
    Ty ty2 = ty_nth(argty, 1);
    //Ty ty3 = p->GetTy();

    Operand* pSx = NULL;
    Operand* pSy = NULL;
        ensure_args2(p, ty1, &pSx, ty2, &pSy);

    Ty ty = apply_float_contagion(p, ty1, &pSx, ty2, &pSy);
    if (nil != ty) ty1 = ty2 = ty;

    if (ty_double_float == ty1 && ty_double_float == ty2)
    {
        Operand* pFx = emit_UNBOX(ty_float64, pSx, p);
        Operand* pFy = emit_UNBOX(ty_float64, pSy, p);
        Register* pFd = new FpRegister();
        replace_to_box(
            ty_double_float,
            new MulInsn(ty_float64, pFd, pFx, pFy),
            p );
        return true;
    } // if double-float

    if (ty_single_float == ty1 && ty_single_float == ty2)
    {
        Operand* pFx = emit_UNBOX(ty_float32, pSx, p);
        Operand* pFy = emit_UNBOX(ty_float32, pSy, p);
        Register* pFd = new FpRegister();
        replace_to_box(
            ty_single_float,
            new MulInsn(ty_float32, pFd, pFx, pFy),
            p );
        return true;
    } // if single-float

    return false;
} // process_Mul2


//////////////////////////////////////////////////////////////////////
//
// process_Sub1
//  CALL single-float %rd <- %sx
//      =>
//      NEG float32 %f1 <- %sx
//      BOX single-float %rd <- %f1
//
static bool process_Sub1(Instruction* pCall)
{
    Ty ty = pCall->GetTy();

    Ty argty = pCall->GetVy()->GetTy();

    Operand*  pSx = ensure_arg1(pCall, ty_nth(argty, 0));

    if (pSx->Is<Literal>())
    {
        Val val = pSx->StaticCast<Literal>()->GetDatum();
        val = funcall(Qcoerce, val, ty);
        val = funcall(QSub1, val);
        ir_replace_all_users(NewLiteral(val), pCall->GetRd());
        return true;
    } // if literal

    if (ty == ty_double_float)
    {
        Register* pFx = emit_UNBOX(ty_float64, pSx, pCall)->
                            StaticCast<Register>();
        Register* pFd = new FpRegister();
        replace_to_box(
            ty_double_float,
            new NegInsn(ty_float64, pFd, pFx),
            pCall );
        return true;
    } // double-float

    if (ty == ty_single_float)
    {
        Register* pFx = emit_UNBOX(ty_float32, pSx, pCall)->
                            StaticCast<Register>();
        Register* pFd = new FpRegister();
        replace_to_box(
            ty_single_float,
            new NegInsn(ty_float32, pFd, pFx),
            pCall );
        return true;
    } // single-float

    return false;
} // process_Sub1


// is_eq_able
static bool is_eq_able(Ty ty)
{
    if (nil == ty_and(ty, ty_number)) return true;
    if (ty_subtypep(ty, ty_fixnum)) return true;
    return false;
} // is_eq_able


//////////////////////////////////////////////////////////////////////
//
// process_eql
//  Rewrite to EQ.
//
static bool process_eql(Instruction* p)
{
    Ty argty = p->GetVy()->GetTy();

    Ty ty1 = ty_nth(argty, 0);
    Ty ty2 = ty_nth(argty, 1);

    Operand* pSx = NULL;
    Operand* pSy = NULL;

    if (ty1 == ty_double_float && ty2 == ty_double_float)
    {
        ensure_args2(p, ty1, &pSx, ty2, &pSy);
        pSx = emit_UNBOX(ty_float64, pSx, p);
        pSy = emit_UNBOX(ty_float64, pSy, p);
        goto reduction;
    } // single-float

    if (ty1 == ty_single_float && ty2 == ty_single_float)
    {
        ensure_args2(p, ty1, &pSx, ty2, &pSy);
        pSx = emit_UNBOX(ty_float32, pSx, p);
        pSy = emit_UNBOX(ty_float32, pSy, p);
        goto reduction;
    } // single-float

    // FIXME 2007-03-18: If both operands are numeric types and they are
    // different type, the value of EQL is false. We should replace all
    // use site with literal NIL.

    unless (is_eq_able(ty1) && is_eq_able(ty2)) return false;

    ensure_args2(p, ty1, &pSx, ty2, &pSy);

    if (! pSx->Is<Register>())
    {
        swap(pSx, pSy);
        swap(ty1, ty2);
    }

  reduction:
    Bool* pBx = new Bool();
        ir_insert_insn(new EqInsn(pBx, pSx, pSy), p);

    ir_replace_insn(
        new SelectInsn(p->GetRd(), pBx, Obj_True, Obj_Nil),
        p );

    return true;
} // process_eql


// process_lengthSvector
static bool process_lengthSvector(Instruction* p)
{
    Operand* pSx = ensure_arg1(p, ty_vector);
    Register* pR1 = new Register();
        ir_insert_insn(new SlotInsn(ty_ptr_sequence_index,
            pR1, NewLiteral(Qdata_vector), NewLiteral(Qlength), pSx ), p );
    ir_replace_insn(new LoadInsn(p->GetRd(), pR1), p);
    return true;
} // process_lengthSvector


//////////////////////////////////////////////////////////////////////
//
// process_length
//  vector  -> SLOT+LOAD
//  list    -> length/list
//
static bool process_length(Instruction* p)
{
    Ty argty = p->GetVy()->GetTy();

    Ty ty1 = ty_nth(argty, 0);

    if (Subtypep_Yes == ty_subtypep(ty1, ty_vector))
    {
        process_lengthSvector(p);
        return true;
    } // if vector

    if (Subtypep_Yes == ty_subtypep(ty1, ty_list))
    {
        p->GetOperandBox(0)->Replace(NewLiteral(QlengthSlist));
        return true;
    } // if list

    return false;
} // process_length


//////////////////////////////////////////////////////////////////////
//
// Function Database
//
// Functions in list must not have side-effects.
//
const Entry Entry::sm_rgoEntry[] =
{
    // Symbols
    { QEq2,             process_Eq2 },
    { QNe2,             process_Ne2 },
    { QGe2,             process_Ge2 },
    { QGt2,             process_Gt2 },
    { QLe2,             process_Le2 },
    { QLt2,             process_Lt2 },

    { QAdd2,            process_Add2 },
    { QDiv2,            process_Div2 },
    { QMul2,            process_Mul2 },
    { QSub2,            process_Sub2 },
    { QSub1,            process_Sub1 },

    // [A]
    { Qash,             process_ash },

    // [C]
    { QCharEq2,         process_CharEq2 },
    { QCharNe2,         process_CharNe2 },
    { QCharGe2,         process_CharGe2 },
    { QCharGt2,         process_CharGt2 },
    { QCharLe2,         process_CharLe2 },
    { QCharLt2,         process_CharLt2 },

    // [E]
    { Qeql,             process_eql },

    // [L]
    { Qlength,          process_length },
    { QlengthSvector,   process_lengthSvector },
    { QlogandS2,        process_logand2 },
    { QlogiorS2,        process_logior2 },
    { QlogxorS2,        process_logxor2 },
}; // Entry::sm_rgoEntry

typedef FunDb_<Entry> FunDb;


//////////////////////////////////////////////////////////////////////
//
// Accessor Entry
//
struct AccessorEntry
{
    Val m_fname;
    Val m_class;
    Val m_slot;

    static const AccessorEntry sm_rgoEntry[];
}; // AccessorEntry

const AccessorEntry AccessorEntry::sm_rgoEntry[] =
{
    { Qsymbol_name,    Qsymbol, Qname },
    { Qsymbol_package, Qsymbol, Qpackage },

    // 11 Pcakges
    { Qpackage_shadowing_symbol,    Qpackage, Qshadowing_symbols },
    { Qpackage_use_list,            Qpackage, Quse_list },
    { Qpackage_used_by_list,        Qpackage, Qused_by_list },

    // 14 Conses
    { Qcar,   Qlist, Qcar },
    { Qcdr,   Qlist, Qcdr },
    { Qfirst, Qlist, Qcar },
    { Qrest,  Qlist, Qcdr },

    { SETF_car,   Qcons, Qcar },
    { SETF_cdr,   Qcons, Qcdr },
    { SETF_first, Qcons, Qcar },
    { SETF_rest,  Qcons, Qcdr },

    // 18 Hash Tables
    { Qhash_table_rehash_size,  Qhash_table, Qrehash_size },
    { Qhash_table_test,         Qhash_table, Qtest },

    // 23 Reader
    { Qreadtable, Qreadtable, Qcase },
}; // AccessorEntry::sm_rgoEntry

typedef FunDb_<AccessorEntry> AccessorDb;


//////////////////////////////////////////////////////////////////////
//
// Call Optimizer
//
class PassStrengthReduction : public FunctionPass
{
    public: PassStrengthReduction() : FunctionPass(L"OPT-OSR") {}

    // enable_RUNTIMECAST
    static void enable_RUNTIMECAST(Register* pRx)
    {
        RuntimeCastInsn* pCast = pRx->GetDfn()->
            DynamicCast<RuntimeCastInsn>();

        if (NULL == pCast) return;

        pCast->ClearAttr(RuntimeCastInsn::Attr_Nop);
    } // enable_RUNTIMECAST

    // find_slot
    static Val find_slot(Val class_name, Val slot_name)
    {
        if (class_name == Qlist) class_name = Qcons;

        Val klass = find_class(class_name, nil);
            if (nil == klass) return nil;

        foreach (EnumList, oEnum, klass->Decode<Class>()->m_slots)
        {
            Val eslotd = oEnum.Get();
            if (eslotd->Decode<EffectiveSlotD>()->m_name == slot_name)
                { return eslotd; }
        } // for each eslotd

        return nil;
    } // find_slot

    // get_arg
    static OperandBox* get_arg(Instruction* pCall, uint nNth)
    {
        ValuesInsn* pArgs = pCall->GetVy()->GetDfn()->
            DynamicCast<ValuesInsn>();

        if (NULL == pArgs) return NULL;

        return pArgs->GetOperandBox(nNth);
    } // get_arg

    // process_function
    virtual void process_function(Function* pFun)
    {
        html_log_format(2, L"<h2>Process ~S</h2>~%", pFun);

        ir_number_instructions(pFun);

        bool fChanged = false;
        foreach (Function::EnumBBlock, oEnumBB, pFun)
        {
            BBlock* pBBlock = oEnumBB.Get();

            BBlock::EnumInsn oEnum(pBBlock);
            while (! oEnum.AtEnd())
            {
                Instruction* pInsn = oEnum.Get();
                    oEnum.Next();
                if (process_instruction(pInsn)) fChanged = true;
            } // for each insn
        } // for each bblock

        if (fChanged) ir_remove_useless_instructions(pFun);
    } // process_function

    // process_instruction
    bool process_instruction(Instruction* pInsn)
    {
        CallInsn* pCall = pInsn->DynamicCast<CallInsn>();
            if (NULL == pCall) return false;
            if (pCall->IsNotInline()) return false;
            if (! pCall->GetVy()->GetDfn()->Is<ValuesInsn>()) return false;

        Literal* pCallee = pCall->GetSx()->DynamicCast<Literal>();
            if (NULL == pCallee) return false;

        Val fname = pCallee->GetDatum();

        // Accessor
        {
            const AccessorEntry* pEntry = AccessorDb::Get(fname);
            if (NULL != pEntry)
            {
                if (symbolp(fname))
                {
                    return process_reader(pCall, pEntry);
                }
                else
                {
                    return process_writer(pCall, pEntry);
                }
            } // if
        } // if

        // Name based reduction.
        if (pCall->GetRd() != NULL)
        {
            const Entry* p = FunDb::Get(fname);
            if (NULL != p)
            {
                pCall->GetVy()->GetDfn()->UpdateTy();
                pCall->UpdateTy();

                html_log_format(3, L"<h3>try reduction ~S:~S</h3>~S~:%",
                    pCall->GetBBlock(), pCall, pCall->GetVy()->GetDfn() );

                return p->m_pfn(pInsn);
            }
        }

        return false;
    } // process_instruction

    // process_reader
    bool process_reader(CallInsn* pCall, const AccessorEntry* pEntry)
    {
        if (NULL == pCall->GetRd())
        {
            ir_remove_insn(pCall);
            return true;
        }

        OperandBox* pBox = get_arg(pCall, 0);
            if (NULL == pBox) return false;

        Register* pRx = pBox->GetOperand()->DynamicCast<Register>();
            if (NULL == pRx) return false;

        Val klass = pEntry->m_class;

        html_log_format(3, L"<h3>try reduction: ~S:~S ~S ~W as ~W</h3>~%",
            pCall->GetBBlock(), pCall, pRx, pRx->GetTy(), klass );

        if (! proof_ty(pBox, klass)) return false;

        Val slot = pEntry->m_slot;
        Val eslotd = find_slot(klass, slot);
            if (nil == eslotd) return false;

        Val slot_ty = ty_expand(
            eslotd->Decode<EffectiveSlotD>()->m_type );

        Register* pR0 = new Register();

        ir_insert_insn(
            new SlotInsn(ty_make_ptr(slot_ty), pR0,
                NewLiteral(klass),
                NewLiteral(slot),
                pRx ),
            pCall );

        ir_replace_insn(new LoadInsn(pCall->GetRd(), pR0), pCall);

        enable_RUNTIMECAST(pRx);

        return true;
    } // process_reader

    // process_writer
    bool process_writer(CallInsn* pCall, const AccessorEntry* pEntry)
    {
        OperandBox* pBox = get_arg(pCall, 1);
            if (NULL == pBox) return false;

        Register* pRx = pBox->GetOperand()->DynamicCast<Register>();
            if (NULL == pRx) return false;

        Val klass = pEntry->m_class;

        html_log_format(3, L"<h3>try reduction: ~S:~S ~S ~W as ~W</h3>~%",
            pCall->GetBBlock(), pCall, pRx, pRx->GetTy(), klass );

        if (! proof_ty(pBox, klass)) return false;

        Val slot = pEntry->m_slot;
        Val eslotd = find_slot(klass, slot);
            if (nil == eslotd) return false;

        //format(t, L"; SR: ~S~%", pCall->GetForm());

        Val slot_ty = ty_expand(
            eslotd->Decode<EffectiveSlotD>()->m_type );

        Register* pR0 = new Register();

        ir_insert_insn(
            new SlotInsn(ty_make_ptr(slot_ty), pR0,
                NewLiteral(klass),
                NewLiteral(slot),
                pRx ),
            pCall );

        enable_RUNTIMECAST(pRx);

        Operand* pSx = pCall->GetVy()->GetDfn()->GetSx();

        ir_replace_insn(new StoreInsn(pR0, pSx), pCall);

        if (NULL != pCall->GetRd())
        {
            ir_replace_all_users(pSx, pCall->GetRd());
        }

        return true;
    } // process_writer

    // proof_ty
    static bool proof_ty(OperandBox* pBox, Ty ty)
    {
        Operand* pSx = pBox->GetOperand();
        return Subtypep_Yes == ty_subtypep(pSx->GetTy(), ty);
    } // proof_ty
}; // PassStrengthReduction

} // namespace

AccessorDb AccessorDb::sm_oFunDb;
FunDb FunDb::sm_oFunDb;

//////////////////////////////////////////////////////////////////////
//
// optimize_strength_reduction
//
void optimize_strength_reduction()
{
    PassStrengthReduction oPass;
        oPass.Run();
} // optimize_strength_reduction

} // Compiler
