#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 25 Environment - Debugger
// big/big_25_debugger.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/big/x86x64_big_25_debugger.cpp#1 $
//
#include "../../../kernel/ke_thread.h"
#include "../../../big/big_lisp.h"
#include "../kernel/x86x64_ke_gcmap.h"

namespace MiniLisp
{

Val get_frame(Val cookie)
{
    if (! fixnump(cookie)) return nil;
    void* pvCookie = reinterpret_cast<void*>(Fixnum::Decode_(cookie));
    foreach (Thread::EnumFrame, oEnum, Thread::Get())
    {
        FunctionFrame* pRunner = oEnum.Get()->DynamicCast<FunctionFrame>();
        if (NULL == pRunner) continue;
        if (pRunner->m_pvRA != pvCookie) continue;

        Val frob = list(cookie, pRunner->m_fn, pRunner->m_ip);
        Val tail = cddr(frob);
        foreach (Kernel::EnumFrameVar, oEnum, pRunner)
        {
            Kernel::EnumFrameVar::Slot oSlot = oEnum.Get();
            Val tuple = list(oSlot.m_ofs, oSlot.m_name, oSlot.m_datum);
            tail = setf_cdr(list(tuple), tail);
        } // for each var

        return frob;
    } // for each frame
    return nil;
} // get_frame

} // MiniLisp
