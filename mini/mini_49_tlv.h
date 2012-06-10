//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 49 Internals - TLV
// mini/mini_49_tlv.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_49_tlv.h#2 $
//
#if !defined(INCLUDE_mini_49_tlv_h)
#define INCLUDE_mini_49_tlv_h

namespace MiniLisp
{
    bool tlv_record_p(Val);
    Val make_tlv_record(Val, Val, Val);
    Val tlv_record_index(Val);
    Val tlv_record_name(Val);
    Val tlv_record_value(Val);
    Val setf_tlv_record_value(Val, Val);

    Int deftlv(Val, Val, Val = t, Val = nil);
} // MiniLisp

#endif //!defined(INCLUDE_mini_49_tlv_h)
