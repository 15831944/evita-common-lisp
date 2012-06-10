#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 15 Arrays
// gs_15_array.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_15_array.cpp#5 $
//
// Description:
//  This file contains following functions:
//
#include "./gs_lisp.h"

namespace MiniLisp
{

// setf_sbit
Val setf_sbit(Val bit, Val bitvec, Val index)
{
    check_type(bitvec, simple_bit_vector);

    SimpleBitVector* pBitVec = bitvec->Decode<SimpleBitVector>();

    if (cmp_xx(index, pBitVec->m_length) >= 0)
    {
        error(Qvector_index_error, Kvector, bitvec, Kdatum, index);
    } // if

    Int iIndex = Fixnum::Decode_(index);

    const uint k_BITS = sizeof(Arch::BitEltT) * 8;
    Int iWordIndex = iIndex / k_BITS;
    Int iBitIndex  = iIndex % k_BITS;

    if (Fixnum::Encode(0) == bit)
    {
        pBitVec->GetElements()[iWordIndex] &=
            ~Kernel::Arch::k_rgfBit[iBitIndex];
    }
    else if (Fixnum::Encode(1) == bit)
    {
        pBitVec->GetElements()[iWordIndex] |= 
            Kernel::Arch::k_rgfBit[iBitIndex];
    }
    else
    {
        error(make_type_error(bit, Qbit));
    }

    return bit;
} // setf_sbit

} // MiniLisp
