#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - lisp
// genesis/geneis_lisp.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/big/win_big_21_debug_stream.cpp#4 $
//
// Description:
//  This file contains implementation of
//      realize-instance    console-stream
//      realize-instance    file-stream
//
#include "../../../big/big_lisp.h"

#include "../../generic/mini/pl_mini_21_stream.h"

namespace MiniLisp
{

class DebugOutputStream : public Instance_<Layout::C_debug_output_stream> {};

//////////////////////////////////////////////////////////////////////
//
// Debug Output Stream Implementation
//
class DebugOutputStreamImpl : public StreamImpl
{
    public: virtual void ForceOutput(Val) {}
    public: virtual Val  LineColumn(Val)  { return nil; }
    public: virtual Val  LineNumber(Val)  { return nil; }
    public: virtual Val  OutputWidth(Val) { return nil; }

    public: virtual void WriteString(
        Val,
        const char16*   pwch,
        size_t          cwch )
    {
        if (0 == pwch[cwch])
        {
            ::OutputDebugString(pwch);
        }
        else
        {
            char16 wsz[32+1];
            wsz[lengthof(wsz)-1] = 0;
            while (cwch > lengthof(wsz))
            {
                evcl_memcpy(wsz, pwch, sizeof(char16) * (lengthof(wsz)-1));
                ::OutputDebugString(wsz);
                pwch += lengthof(wsz)-1;
                cwch -= lengthof(wsz)-1;
            } // while
            evcl_memcpy(wsz, pwch, sizeof(char16) * cwch);
            wsz[cwch] = 0;
            ::OutputDebugString(wsz);
        } // if
    } // WriteString
}; // DebugOutputStreamImpl


//////////////////////////////////////////////////////////////////////
//
// realize_instance_debug_output_stream
//
Val realize_instance_debug_output_stream(MiniThread* p)
{
    Val stream = p->mv_value[0];
    stream->Decode<DebugOutputStream>()->m_blob =
        Fixnum::Encode(new DebugOutputStreamImpl());
    return stream;
} // realize_instance_debug_output_stream

} // MiniLisp
