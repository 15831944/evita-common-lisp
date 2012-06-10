#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 49 Internals - Thread Local Variable
// mini/mini_49_tlv.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_49_tlv.cpp#3 $
//
#include "./mini_lisp.h"

namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// Make Tlv Record
//
Val make_tlv_record(
    Val name,
    Val index,
    Val val )
{
    ASSERT(symbolp(name));
    ASSERT(fixnump(index));

    Val cell = MiniThread::Get()->AllocRecord(CLASSD_tlv_record);
    TlvRecord* pCell = cell->Decode<TlvRecord>();
        pCell->m_name  = name;
        pCell->m_index = index;
        pCell->m_value = val;
    return cell;
} // make_tlv_record


// get_tlv
Val get_tlv(Int iOffset)
{
    return MiniThread::Get()->GetTlv(iOffset);
} // get_Tlv


// set_tlv
Val set_tlv(Int iOffset, Val val)
{
    return MiniThread::Get()->SetTlv(iOffset, val);
} // set_tlv


// deftlv
Int deftlv(Val name, Val initval, Val initp, Val)
{
    Val table = VAR(Avalue_tableA);

    Int iIndex;
    {
        with_exclusive_latch(VAR(Avalue_table_latchA));

        Val tlvrec = gethash(name, table);

        if (nil != tlvrec)
        {
            iIndex = Fixnum::Decode_(tlvrec->Decode<TlvRecord>()->m_index);
        }
        else
        {
            tlvrec = make_tlv_record(name, 0, initval);

            {
                with_exclusive_latch(VAR(Atlv_vector_latchA));

                Val tlv_vector = VAR(Atlv_vectorA);

                iIndex  = Fixnum::Decode_(svref(tlv_vector, Fixnum::Encode(0))) + 1;
                setf_svref(Fixnum::Encode(iIndex), tlv_vector, Fixnum::Encode(0));
                setf_svref(tlvrec, tlv_vector, iIndex);
            } // iIndex

            tlvrec->Decode<TlvRecord>()->m_index = Fixnum::Encode(iIndex);

            setf_gethash(tlvrec, name, table);
        } // if
    }

    Int ofsTlv = MiniThread::ToTlvOffset(iIndex);
    if (nil != initp) set_tlv(ofsTlv, initval);
    return ofsTlv;
} // deftlv

} // MiniLisp
