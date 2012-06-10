#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - instruction Arithmetic
// ir/instruction/ir_insn_arith.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_insn_arith.cpp#7 $
//
#include "./ir_instruction.h"

#include "./ir_fns.h"

namespace Compiler
{

// ir_make_real
Operand* ir_make_real(Ty ty, Int iVal)
{
    if (ty_int32 == ty || ty_uint32 == ty ||
        ty_int16 == ty || ty_uint16 == ty ||
        ty_int8  == ty || ty_uint8  == ty ||
        ty_int64 == ty || ty_uint64 == ty )
    {
        return NewInteger(iVal);
    }

    if (ty_float32 == ty || ty_float64 == ty)
    {
        error(L"NYI: ir_make_real: float");
    }

    if (ty_subtypep(ty, Qdouble_float))
    {
        error(L"NYI: ir_make_real: float");
    }

    if (ty_subtypep(ty, Qsingle_float))
    {
        error(L"NYI: ir_make_real: single float");
    }

    // (complex double-float)
    // (complex single-float)

    return NewLiteral(iVal);
} // ir_make_real


// ir_operand_equal
bool ir_operand_equal(Operand* pSx, Int iVal)
{
    switch (pSx->GetKind())
    {
    case Operand::Kind_Literal:
        return pSx->StaticCast<Literal>()->GetDatum() == Fixnum::Encode(iVal);

    case Operand::Kind_Integer:
        return pSx->StaticCast<Integer>()->GetValue() == iVal;

    default:
        return false;
    } // switch kind
} // ir_operand_equal


//////////////////////////////////////////////////////////////////////
//
// AddInsn::SimplifyOutputAux
//
//      (+ x 0) = x
//
Operand* AddInsn::SimplifyOutputAux() const
{
    if (ir_operand_equal(GetSx(), 0))
    {
        return GetSy();
    }

    if (ir_operand_equal(GetSy(), 0))
    {
        return GetSx();
    }

    return GetOutput();
} // AddInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// DivInsn::SimplifyOutputAux
//
Operand* DivInsn::SimplifyOutputAux() const
{
    return GetOutput();
} // DivInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// LogAndInsn::SimplifyOutputAux
//
//      (logand x  0) = 0
//      (logand x -1) = x
//
Operand* LogAndInsn::SimplifyOutputAux() const
{
    if (ir_operand_equal(GetSx(), -1))
    {
        return GetSy();
    }

    if (ir_operand_equal(GetSy(), -1))
    {
        return GetSx();
    }

    if (ir_operand_equal(GetSx(), 0) || ir_operand_equal(GetSy(), 0))
    {
        return ir_make_real(GetTy(), 0);
    }

    return GetOutput();
} // LogAndInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// LogIorInsn::SimplifyOutputAux
//
//      (logior x  0) = x
//      (logior x -1) = -1
//
Operand* LogIorInsn::SimplifyOutputAux() const
{
    if (ir_operand_equal(GetSx(), 0))
    {
        return GetSy();
    }

    if (ir_operand_equal(GetSy(), 0))
    {
        return GetSx();
    }

    if (ir_operand_equal(GetSx(), -1) || ir_operand_equal(GetSy(), -1))
    {
        return ir_make_real(GetTy(), -1);
    }

    return GetOutput();
} // LogIorInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// LogXorInsn::SimplifyOutputAux
//
//      (logxor x 0) = x
//
Operand* LogXorInsn::SimplifyOutputAux() const
{
    if (ir_operand_equal(GetSx(), 0))
    {
        return GetSy();
    }

    if (ir_operand_equal(GetSy(), 0))
    {
        return GetSx();
    }

    return GetOutput();
} // LogXorInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// MulInsn::SimplifyOutputAux
//
//      (mul x 0) = 0 (of type-of x)
//      (mul x 1) = x
//
Operand* MulInsn::SimplifyOutputAux() const
{
    if (ir_operand_equal(GetSx(), 1))
    {
        return GetSy();
    }

    if (ir_operand_equal(GetSy(), 1))
    {
        return GetSx();
    }

    if (ir_operand_equal(GetSx(), 0))
    {
        return ir_make_real(GetSy()->GetTy(), 0);
    }

    if (ir_operand_equal(GetSy(), 0))
    {
        return ir_make_real(GetSx()->GetTy(), 0);
    }

    return GetOutput();
} // MulInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// ShlInsn::SimplifyOutputAux
//
//      (ash 0 n) = 0
//      (ash x 0) = x
//
Operand* ShlInsn::SimplifyOutputAux() const
{
    if (ir_operand_equal(GetSx(), 0)) return ir_make_real(GetTy(), 0);
    if (ir_operand_equal(GetSy(), 0)) return GetSx();
    return GetOutput();
} // ShlInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// ShrInsn::SimplifyOutputAux
//
//      (ash 0 n) = 0
//      (ash x 0) = x
//
Operand* ShrInsn::SimplifyOutputAux() const
{
    if (ir_operand_equal(GetSx(), 0)) return ir_make_real(GetTy(), 0);
    if (ir_operand_equal(GetSy(), 0)) return GetSx();
    return GetOutput();
} // ShrInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// SubInsn::SimplifyOutputAux
//
//      (- x 0) = x
//
Operand* SubInsn::SimplifyOutputAux() const
{
    if (ir_operand_equal(GetSy(), 0)) return GetSx();
    return GetOutput();
} // SubInsn::SimplifyOutputAux


//////////////////////////////////////////////////////////////////////
//
// TruncateInsn::SimplifyOutputAux
//
//      (truncate x 1) = x
//
Operand* TruncateInsn::SimplifyOutputAux() const
{
    if (ir_operand_equal(GetSy(), 1))
    {
        return GetSx();
    }

    return GetOutput();
} // TruncateInsn::SimplifyOutputAux

} // Compiler
