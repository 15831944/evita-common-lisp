#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 14 Conses
// compiler/cl/cl_14_cons.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_15_array.cpp#3 $
// Description:
//  This file contains following parsers:
//      atom
//      endp
//      list
//
#include "./cl_defs.h"

namespace Compiler
{

// 15.2.28 svref
define_parser(svref)
{
    CHECK_SYNTAX(3, 3, "(svref vector index)");

    Val args = cdr(form);

    Operand* pSArray;
    {
        ExpectOperand oExpect(Qsvref, 0, ty_simple_vector);
        pSArray = parseForm1(&oExpect, pop(args));
        if (Obj_Unreachable == pSArray) return useless_form(form);
    } // pSArray

    Operand* pSIndex;
    {
        ExpectOperand oExpect(Qsvref, 1, ty_sequence_index);
        pSIndex = parseForm1(&oExpect, pop(args));
        if (Obj_Unreachable == pSIndex) return useless_form(form);
    } // pSIndex

    if (pSArray->Is<Register>())
    {
        pSArray = emitRuntimeCast(
            ty_simple_vector, 
            pSArray->StaticCast<Register>() );
    } // if

    if (pSIndex->Is<Register>())
    {
        pSIndex = emitRuntimeCast(
            ty_sequence_index, 
            pSIndex->StaticCast<Register>() );
    } // if

    Register* pRIndex = newRegister(form);
        emitInsn(new BoundInsn(pRIndex, pSArray, pSIndex,
            m_pFrame,
            option_check_index() ? 0 : BoundInsn::Attr_Nop ) );
    Register* pRElt = newRegister(form);
        emitInsn(new EltInsn(ty_ptr_t, pRElt, pSArray, pRIndex));
    Register* pRd = newRegister(form);
        emitInsn(new LoadInsn(pRd, pRElt));
    return emitLinkage(emitCast(pExpect, form, pRd));
} // svref


// 15.2.28 (setf svref)
define_parser_setf(svref)
{
    CHECK_SYNTAX(5, 5, "(funcall #'(setf svref) val vector index)");

    Val args = cddr(form);

    Operand* pSValue;
    {
        ExpectOperand oExpect(Qsvref, 0, ty_t);
        pSValue = parseForm1(&oExpect, pop(args));
        if (Obj_Unreachable == pSValue) return useless_form(form);
    } // pSValue

    Operand* pSArray;
    {
        ExpectOperand oExpect(Qsvref, 1, ty_simple_vector);
        pSArray = parseForm1(&oExpect, pop(args));
        if (Obj_Unreachable == pSArray) return useless_form(form);
    } // pSArray

    Operand* pSIndex;
    {
        ExpectOperand oExpect(Qsvref, 2, ty_fixnum);
        pSIndex = parseForm1(&oExpect, pop(args));
        if (Obj_Unreachable == pSIndex) return useless_form(form);
    } // pSIndex

    switch (pSArray->GetKind())
    {
    case Operand::Kind_Register:
        pSArray = emitRuntimeCast(
            ty_simple_vector, 
            pSArray->StaticCast<Register>() );
        break;

    case Operand::Kind_Literal:
        warn(L"Can't alter ~S.", pSArray->StaticCast<Literal>()->GetDatum());
        return emitLinkage(Obj_Unreachable);

        break;

    default:
        return CanNotHappen();
    } // switch operand

    if (pSIndex->Is<Register>())
    {
        pSIndex = emitRuntimeCast(
            ty_sequence_index, 
            pSIndex->StaticCast<Register>() );
    } // if

    Register* pRIndex = newRegister(form);
        emitInsn(new BoundInsn(pRIndex, pSArray, pSIndex,
            m_pFrame,
            option_check_index() ? 0 : BoundInsn::Attr_Nop ) );
    Register* pRElt = newRegister(form);
        emitInsn(new EltInsn(ty_ptr_t, pRElt, pSArray, pRIndex));
        emitInsn(new StoreInsn(pRElt, pSValue));
    return emitLinkage(emitCast(pExpect, form, pSValue));
} // setf svref

} // Compiler
