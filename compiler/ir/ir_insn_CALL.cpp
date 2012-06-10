#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction CALL
// ir/instruction/ir_insn_CALL.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_CALL.cpp#27 $
//
#include "./ir_instruction.h"

#include "./ir_defs.h"
#include "./ir_fns.h"
#include "./ir_function.h"
#include "./ir_fundb.h"

namespace Compiler
{

namespace
{

// is_setf
//  Returns true if pSx is setf function name.
static bool is_setf(Operand* pSx)
{
    switch (pSx->GetKind())
    {
    case Operand::Kind_Literal:
        return setf_cell_p(pSx->StaticCast<Literal>()->GetDatum());

    case Operand::Kind_Function:
        // Local function must return the first argument.
        return setf_cell_p(pSx->StaticCast<Function>()->GetName());

    default:
        return false;
    } // switch operand
} // is_setf


//////////////////////////////////////////////////////////////////////
//
// Flag
//
enum Flag
{
    Flag_NoSideEffect = 1 << 0,
}; // Flag


//////////////////////////////////////////////////////////////////////
//
// FunDb Entry
//
struct Entry
{
    Val     m_fname;
    Ty (*m_pfnType)(const Instruction*);
    uint    m_nFlags;

    bool IsNoSideEffect() const
        { return 0 != (m_nFlags & Flag_NoSideEffect); }

    static const Entry sm_rgoEntry[];
}; // Entry


//////////////////////////////////////////////////////////////////////
//
// compute_ty_None
//  We have no idea about value of function
static Ty compute_ty_None(const Instruction* pCall)
    { return pCall->GetTy(); }


//////////////////////////////////////////////////////////////////////
//
// compute_ty_Arg0 - arg0 is result-type.
//
static Ty compute_ty_Arg0(const Instruction* pCall)
{
    const ValuesInsn* pArgs = pCall->GetVy()->GetDfn()->
            DynamicCast<ValuesInsn>();
        if (NULL == pArgs) return 0;

    if (! pArgs->GetSx()->Is<Literal>()) return 0;

    return pArgs->GetLx();
} // make_instance


#if 0
// BUGBUG: We fall into the inifinite-loop at opt-TYPE.
static Ty compute_ty_val0(const Instruction* pCall)
    { return ty_and(ty_get_primary(pCall->GetVy()->GetTy()), pCall->GetTy()); }
#else
static Ty compute_ty_val0(const Instruction* pCall)
{
    Ty ty = ty_get_primary(pCall->GetVy()->GetTy());
    if (ty == ty_t) ty = pCall->GetTy();
    return ty;
} // compute_ty_val0
#endif


// compute_ty_ash
static Ty compute_ty_ash(const Instruction* pCall)
{
    Ty argty = pCall->GetVy()->GetTy();

    TyInteger oTyN = TyInteger::Parse(ty_nth(argty, 0));
    if (! oTyN.IsValid()) return 0;
    if (! oTyN.IsClose()) return 0;

    TyInteger oTyK = TyInteger::Parse(ty_nth(argty, 1));
    if (! oTyK.IsValid()) return 0;
    if (! oTyK.IsClose()) return 0;

    Val lower;
    {
        Val k = oTyK.m_lower.Get();
        Int iK = fixnump(k) ? Fixnum::Decode_(k) : -999;
        if (iK >= -128 && iK <= 128)
        {
            lower = ash(oTyN.m_lower.Get(), iK);
        }
        else
        {
            lower = Interval__OpenLower;
        }
    } // lower

    Val upper;
    {
        Val k = oTyK.m_upper.Get();
        Int iK = fixnump(k) ? Fixnum::Decode_(k) : -999;
        if (iK >= -128 && iK <= 128)
        {
            upper = ash(oTyN.m_upper.Get(), iK);
        }
        else
        {
            upper = Interval__OpenUpper;
        }
    } // upper

    TyInteger oTy(lower, upper);
    return oTy.Unparse();
} // compute_ty_ash


// compute_ty_coerce
static Ty compute_ty_coerce(const Instruction* pCall)
{
    ValuesInsn* pArgs = pCall->GetVy()->GetDfn()->DynamicCast<ValuesInsn>();
        if (NULL == pArgs) return pCall->GetTy();
    Operand* pSy = pArgs->GetSy();
    if (! pSy->Is<Literal>()) return pCall->GetTy();
    return ty_expand(pArgs->GetLy());
} // compute_ty_coerce


// compute_ty_float
static Ty compute_ty_float(const Instruction* pCall)
{
    Ty ty = pCall->GetVy()->GetTy();
    Arity oArity;
        ir_get_ty_arity(ty, &oArity);

    if (oArity.GetMax() == 1)
    {
        return ty_single_float;
    }

    Ty syty = ty_nth(ty, 1);

    if (Subtypep_Yes == ty_subtypep(syty, ty_double_float))
    {
        return ty_double_float;
    }

    if (Subtypep_Yes == ty_subtypep(syty, ty_single_float))
    {
        return ty_single_float;
    }

    return ty_float;
} // compute_ty_float


// compute_ty_logandS2
static Ty compute_ty_logandS2(const Instruction* pCall)
{
    TyInteger oTy1 = TyInteger::Parse(ty_nth(pCall->GetVy()->GetTy(), 0));
        if (! oTy1.IsValid()) return 0;

    TyInteger oTy2 = TyInteger::Parse(ty_nth(pCall->GetVy()->GetTy(), 1));
        if (! oTy2.IsValid()) return 0;

    TyInteger oTy = oTy1 & oTy2;
    return oTy.Unparse();
} // compute_ty_logandS2


// compute_ty_logeqvS2
static Ty compute_ty_logeqvS2(const Instruction* pCall)
{
    TyInteger oTy1 = TyInteger::Parse(ty_nth(pCall->GetVy()->GetTy(), 0));
        if (! oTy1.IsValid()) return 0;

    TyInteger oTy2 = TyInteger::Parse(ty_nth(pCall->GetVy()->GetTy(), 1));
        if (! oTy2.IsValid()) return 0;

    TyInteger oTy = ~(oTy1 ^ oTy2);
    return oTy.Unparse();
} // compute_ty_logeqvS2


// compute_ty_logiorS2
static Ty compute_ty_logiorS2(const Instruction* pCall)
{
    TyInteger oTy1 = TyInteger::Parse(ty_nth(pCall->GetVy()->GetTy(), 0));
        if (! oTy1.IsValid()) return 0;

    TyInteger oTy2 = TyInteger::Parse(ty_nth(pCall->GetVy()->GetTy(), 1));
        if (! oTy2.IsValid()) return 0;

    TyInteger oTy = oTy1 | oTy2;
    return oTy.Unparse();
} // compute_ty_logiorS2


// compute_ty_logxorS2
static Ty compute_ty_logxorS2(const Instruction* pCall)
{
    TyInteger oTy1 = TyInteger::Parse(ty_nth(pCall->GetVy()->GetTy(), 0));
        if (! oTy1.IsValid()) return 0;

    TyInteger oTy2 = TyInteger::Parse(ty_nth(pCall->GetVy()->GetTy(), 1));
        if (! oTy2.IsValid()) return 0;

    TyInteger oTy = oTy1 ^ oTy2;
    return oTy.Unparse();
} // compute_ty_logxorS2


// compute_ty_mod
//  FIXME 2007-03-19: We should compute type of MOD more precisely.
static Ty compute_ty_mod(const Instruction* pCall)
{
    return ty_nth(pCall->GetVy()->GetTy(), 1);
} // compute_ty_mod


// compute_ty_rem
//  FIXME 2007-03-19: We should compute type of REM more precisely.
static Ty compute_ty_rem(const Instruction* pCall)
{
    return ty_nth(pCall->GetVy()->GetTy(), 1);
} // compute_ty_rem


//////////////////////////////////////////////////////////////////////
//
// is_ty_arith2
//
static bool is_ty_arith2(Ty ty1, Ty ty2, Ty ty3)
{
    return Subtypep_Yes == ty_subtypep(ty1, ty3) &&
           Subtypep_Yes == ty_subtypep(ty2, ty3);
} // is_ty_arith2


//////////////////////////////////////////////////////////////////////
//
// compute_ty_arith2
//
typedef Interval (*IntTyOpT)(const Interval&, const Interval&);

static Ty compute_ty_arith2(
    const Instruction*  pCall,
    IntTyOpT            pfn )
{
    ASSERT(NULL != pfn);

    Ty ty1 = ty_nth(pCall->GetVy()->GetTy(), 0);
    Ty ty2 = ty_nth(pCall->GetVy()->GetTy(), 1);

    if (is_ty_arith2(ty1, ty2, ty_double_float)) return ty_double_float;
    if (is_ty_arith2(ty1, ty2, ty_single_float)) return ty_single_float;

    TyInteger oTy1 = TyInteger::Parse(ty1);
    TyInteger oTy2 = TyInteger::Parse(ty2);

    if (oTy1.IsValid() && oTy2.IsValid())
    {
        TyInteger oTy = pfn(oTy1, oTy2);
        Val ty = oTy.Unparse();
        if (nil == ty)
        {
            warn(L"BB~D: ~S ~S ~S -> nil",
                pCall->GetBBlock()->GetName(),
                pCall->GetLx(),
                oTy1.Unparse(),
                oTy2.Unparse() );
        }
        return ty;
    } // if

    if (is_ty_arith2(ty1, ty2, ty_rational)) return ty_rational;
    if (is_ty_arith2(ty1, ty2, ty_real))     return ty_real;
    return 0;
} // compute_ty_arith2


// compute_ty_Add2
static Ty compute_ty_Add2(const Instruction* pCall)
    { return compute_ty_arith2(pCall, &operator+); }


// compute_ty_Div2
Ty compute_ty_Div2(const Instruction* pCall)
{
    Ty ty1 = ty_nth(pCall->GetVy()->GetTy(), 0);
    Ty ty2 = ty_nth(pCall->GetVy()->GetTy(), 1);

    if (is_ty_arith2(ty1, ty2, ty_double_float)) return ty_double_float;
    if (is_ty_arith2(ty1, ty2, ty_single_float)) return ty_single_float;
    if (is_ty_arith2(ty1, ty2, ty_rational))     return ty_rational;
    if (is_ty_arith2(ty1, ty2, ty_real))         return ty_real;
    return ty_number;
} // compute_ty_Div2

// compute_ty_Mul2
Ty compute_ty_Mul2(const Instruction* pCall)
{
    Ty ty1 = ty_nth(pCall->GetVy()->GetTy(), 0);
    Ty ty2 = ty_nth(pCall->GetVy()->GetTy(), 1);

    if (is_ty_arith2(ty1, ty2, ty_double_float)) return ty_double_float;
    if (is_ty_arith2(ty1, ty2, ty_single_float)) return ty_single_float;
    if (is_ty_arith2(ty1, ty2, ty_integer))      return ty_integer;
    if (is_ty_arith2(ty1, ty2, ty_real))         return ty_real;
    return ty_number;
} // compute_ty_Mul2


// compute_ty_Sub2
static Ty compute_ty_Sub2(const Instruction* pCall)
    { return compute_ty_arith2(pCall, &operator-); }


// compute_ty_Sub1
static Ty compute_ty_Sub1(const Instruction* pCall)
{
    Ty ty1 = ty_nth(pCall->GetVy()->GetTy(), 0);

    if (Subtypep_Yes == ty_subtypep(ty1, ty_double_float))
        return ty_double_float;

    if (Subtypep_Yes == ty_subtypep(ty1, ty_single_float))
        return ty_single_float;

    {
        TyInteger oTy1 = TyInteger::Parse(ty1);
        if (oTy1.IsValid())
        {
            TyInteger oZero(Fixnum::Encode(0));
            TyInteger oTy = oZero - oTy1;
            return oTy.Unparse();
        }
    }

    if (Subtypep_Yes == ty_subtypep(ty1, ty_real))
        return ty_real;

    return ty_number;
} // compute_ty_Sub1


//////////////////////////////////////////////////////////////////////
//
// FunDb for CALL instruciton
//

typedef FunDb_<Entry> FunDb;

#define FUNDB_ENTRY(mp_name, mp_flags) \
    { Q##mp_name, compute_Q##mp_name, mp_flags }

#define FUNDB_ENTRY_0_VAL(mp_name, mp_flags) \
    { Q##mp_name, compute_ty_val0, mp_flags }

const Entry Entry::sm_rgoEntry[] =
{
    // [A]
    { Qash,             compute_ty_ash,     Flag_NoSideEffect },

    // [C]
    { Qconcatenate,     compute_ty_Arg0,    Flag_NoSideEffect },
    { Qcoerce,          compute_ty_coerce,  Flag_NoSideEffect },
    FUNDB_ENTRY_0_VAL(copy_seq, Flag_NoSideEffect),

    // [D]
    FUNDB_ENTRY_0_VAL(delete, 0),
    FUNDB_ENTRY_0_VAL(delete_duplicates, 0),
    FUNDB_ENTRY_0_VAL(delete_if, 0),
    FUNDB_ENTRY_0_VAL(delete_if_not, 0),

    // [F]
    FUNDB_ENTRY_0_VAL(fill, 0),
    { Qfloat, compute_ty_float, Flag_NoSideEffect },

    // [I]
    { Qidentity,    compute_ty_Arg0,        Flag_NoSideEffect },

    // [L]
    { Qlength,      compute_ty_None,        Flag_NoSideEffect },
    { QlogandS2,    compute_ty_logandS2,    Flag_NoSideEffect },
    { QlogeqvS2,    compute_ty_logeqvS2,    Flag_NoSideEffect },
    { QlogiorS2,    compute_ty_logiorS2,    Flag_NoSideEffect },
    { Qlognot,      compute_ty_None,        Flag_NoSideEffect },
    { QlogxorS2,    compute_ty_logxorS2,    Flag_NoSideEffect },

    // [M]
    { Qmake_instance,   compute_ty_Arg0, Flag_NoSideEffect },
    { Qmake_sequence,   compute_ty_Arg0, Flag_NoSideEffect},
    { Qmap,             compute_ty_Arg0, 0 },
    FUNDB_ENTRY_0_VAL(map_into, 0),
    { Qmerge,           compute_ty_Arg0, 0 },
    { Qmod,             compute_ty_mod, Flag_NoSideEffect },

    // [N]
    FUNDB_ENTRY_0_VAL(nreverse, 0),
    FUNDB_ENTRY_0_VAL(nsubstitute, 0),
    FUNDB_ENTRY_0_VAL(nsubstitute_if, 0),
    FUNDB_ENTRY_0_VAL(nsubstitute_if_not, 0),

    // [R]
    { Qrem, compute_ty_rem, Flag_NoSideEffect },
    FUNDB_ENTRY_0_VAL(remove, Flag_NoSideEffect),
    FUNDB_ENTRY_0_VAL(remove_duplicates, Flag_NoSideEffect),
    FUNDB_ENTRY_0_VAL(remove_if, Flag_NoSideEffect),
    FUNDB_ENTRY_0_VAL(remove_if_not, Flag_NoSideEffect),
    FUNDB_ENTRY_0_VAL(replace, 0),
    FUNDB_ENTRY_0_VAL(reverse, Flag_NoSideEffect),

    // [S]
    FUNDB_ENTRY_0_VAL(stable_sort, 0),
    FUNDB_ENTRY_0_VAL(sort, 0),
    FUNDB_ENTRY_0_VAL(subseq, Flag_NoSideEffect),
    FUNDB_ENTRY_0_VAL(substitute, Flag_NoSideEffect),
    FUNDB_ENTRY_0_VAL(substitute_if, Flag_NoSideEffect),
    FUNDB_ENTRY_0_VAL(substitute_if_not, Flag_NoSideEffect),

    // Symbol
    { QEq,      compute_ty_None, Flag_NoSideEffect },
    { QNe,      compute_ty_None, Flag_NoSideEffect },
    { QGe,      compute_ty_None, Flag_NoSideEffect },
    { QGt,      compute_ty_None, Flag_NoSideEffect },
    { QLe,      compute_ty_None, Flag_NoSideEffect },
    { QLt,      compute_ty_None, Flag_NoSideEffect },

    { QAdd,     compute_ty_None, Flag_NoSideEffect },
    { QDiv,     compute_ty_None, Flag_NoSideEffect },
    { QMul,     compute_ty_None, Flag_NoSideEffect },
    { QSub,     compute_ty_None, Flag_NoSideEffect },

    { QEq2,     compute_ty_None, Flag_NoSideEffect },
    { QNe2,     compute_ty_None, Flag_NoSideEffect },
    { QGe2,     compute_ty_None, Flag_NoSideEffect },
    { QGt2,     compute_ty_None, Flag_NoSideEffect },
    { QLe2,     compute_ty_None, Flag_NoSideEffect },
    { QLt2,     compute_ty_None, Flag_NoSideEffect },

    { Q_S1,     compute_ty_Sub1,    Flag_NoSideEffect },
    { QDiv2,    compute_ty_Div2,    Flag_NoSideEffect },
    { QSub2,    compute_ty_Sub2,    Flag_NoSideEffect },
    { QMul2,    compute_ty_Mul2,    Flag_NoSideEffect },
    { QAdd2,    compute_ty_Add2,    Flag_NoSideEffect },
}; // Entry::sm_rgoEntry

} // namespace

FunDb FunDb::sm_oFunDb;

//////////////////////////////////////////////////////////////////////
//
// Instruction::HtmlPrint
//
void
CallInsn::HtmlPrint(Val stream, bool fDef) const
{
    Instruction::HtmlPrint(stream, fDef);
    if (m_fNotInline)
    {
        html_format(stream, L" notinline");
    }
} // CallInsn::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
// CallInsn::IsPineed
//
bool
CallInsn::IsPinned() const
{
    Literal* pCallee = GetSx()->DynamicCast<Literal>();
        if (NULL == pCallee) return true;

    const Entry* p = FunDb::Get(pCallee->GetDatum());
        if (NULL == p) return true;
    return ! p->IsNoSideEffect();
} // CallInsn::IsPinned


//////////////////////////////////////////////////////////////////////
//
// CallInsn::UpdateTy
//
Operand* CallInsn::SimplifyOutputAux() const
{
    if (m_fNotInline) return m_pOutput;
    if (Obj_Void == m_pOutput) return m_pOutput;
    if (! GetSx()->Is<Literal>()) return m_pOutput;

    ValuesInsn* pArgs = GetVy()->GetDfn()->DynamicCast<ValuesInsn>();
    if (NULL == pArgs) return m_pOutput;

    Val callee = GetLx();

    if (Qidentity == callee) return pArgs->GetSx();

    return m_pOutput;
} // CallInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// CallInsn::UpdateTy
//
bool
CallInsn::UpdateTy()
{
    if (Obj_Void == m_pOutput) return false;

    if (is_setf(GetSx()))
    {
        Val ty = ty_get_primary(GetVy()->GetTy());
        if (ty_equal(m_ty, ty)) return false;
        m_ty = ty;
        return true;
    } // if setf

    // Named callee
    {
        Literal* pCallee = GetSx()->DynamicCast<Literal>();
        if (NULL != pCallee)
        {
            const Entry* p = FunDb::Get(pCallee->GetDatum());
            if (NULL != p)
            {
                Ty ty = p->m_pfnType(this);
                if (0 != ty && ! ty_equal(m_ty, ty))
                    { m_ty = ty; return true; }
            } // if
        } // if
    }

    return false;
} // CallInsn::UpdateTy

} // Compiler
