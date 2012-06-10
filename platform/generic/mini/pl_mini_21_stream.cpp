#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - platform - mini - 21 Streams
// platform/generic/mini/pl_mini_21_stream.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/generic/mini/pl_mini_21_stream.cpp#3 $
//
#include "../../../mini/mini_lisp.h"

#include "./pl_mini_21_stream.h"

namespace MiniLisp
{

// StreamImpl::FreshLine
Val StreamImpl::FreshLine(Val stream)
{
    if (nil != StartLineP(stream)) return nil;
    Terpri(stream);
    return t;
} // StreamImpl::FreshLine


// StreamImpl::PeekChar
Val StreamImpl::PeekChar(Val stream)
{
    Val ch = ReadChar(stream);
    if (Keof != ch) UnreadChar(stream, Character::ToCode(ch));
    return ch;
} // StreamImpl::PeekChar


// StreamImpl::ReadBytes
Val StreamImpl::ReadBytes(Val stream, uint8* pbStart, size_t cb)
{
    uint8* pbEnd = pbStart + cb;
    uint8* pbRunner = pbStart;

    while (pbRunner < pbEnd)
    {
        Val datum = ReadByte(stream);
        if (Keof == datum) break;
        *pbRunner++ = static_cast<uint8>(Fixnum::Decode_(datum));
    } // while

    return Fixnum::Encode(pbRunner - pbStart);
} // StreamImpl::ReadBytes


// StreamImpl::ReadCharNoHang
Val StreamImpl::ReadCharNoHang(Val stream)
{
    if (! Listen(stream)) return nil;
    return ReadChar(stream);
} // StreamImpl::ReadCharNoHang


// StreamImpl::StartLineP
Val StreamImpl::StartLineP(Val stream)
    { return LineColumn(stream) == Fixnum::Encode(0) ? t : nil; }

} // MiniLisp
