#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86x64 - Stack Walker
// arch/x86x64/kernel/gen_ke_frame.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/kernel/x86x64_ke_stack_walker.cpp#11 $
//
// Description:
//  This file contains implementaiton x86/x64 stack walker.
//
#include "./x86x64_ke_layout.h"
#include "./x86x64_ke_thread.h"

#include "../../../kernel/ke_gc.h"


namespace Kernel
{

//////////////////////////////////////////////////////////////////////
//
// Thread::EnumFrame::Get
//
Frame* Thread::EnumFrame::Get() const
{
    ASSERT(! AtEnd()); return m_pRunner;
} // Thread::EnumFrame::Get


//////////////////////////////////////////////////////////////////////
//
// Map Program Counter to Function Object
//  o We don't check m_classd here, since this function is called
//    during GC.
Val map_ra_to_fn(UInt nRA)
{
    uint8* pbRunner = reinterpret_cast<uint8*>(nRA & ~15);

    for (;;)
    {
        FunObj* pFunObj = reinterpret_cast<FunObj*>(pbRunner);

        if (FunObj::Cookie == pFunObj->m_nCookie)
        {
            return pFunObj->Encode();
        }

        pbRunner -= 16;
    } // for
} // map_ra_to_fn


//////////////////////////////////////////////////////////////////////
//
// Computer Caller RA
//
UInt* compute_caller_ra(Val fn, UInt* pRa)
{
    FunObj* pFunObj = Gc::resolveForward(fn)->Decode<FunObj>();

    //DEBUG_PRINTF(L"cbFrame=%u\r\n", pFunObj->GetFrameSize());

    UInt* pRacaller = reinterpret_cast<UInt*>(
        reinterpret_cast<uint8*>(pRa) +
        pFunObj->GetFrameSize() );

    switch (pFunObj->GetFrameType())
    {
    case FunObj::FrameType_Fixed:
        break;

    case FunObj::FrameType_Restify:
        pRacaller = reinterpret_cast<UInt*>(*pRacaller);
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch frame type

    return pRacaller;
} // compute_caller_ra

static uint8*
getCodeVec(Val fn)
{
    FunObj* pFunObj = reinterpret_cast<FunObj*>(
        fn->ToInt() - FunObj::Tag );

    return pFunObj->GetCodeVec();
} // getCodeVec


//////////////////////////////////////////////////////////////////////
//
// Thread::EnumFrame::Next
//
void Thread::EnumFrame::Next()
{
    ASSERT(! AtEnd());

    #if 0
        if (m_pRunner != m_pFunFrame)
        {
            dbg_format(L"~X~X ~C~C~C outer=~X~X~%",
                Fixnum::Encode(reinterpret_cast<Int>(m_pRunner) >> 4),
                Fixnum::Encode(reinterpret_cast<Int>(m_pRunner) & 15),
                Character::Encode(static_cast<char16>((m_pRunner->GetType() >> 24) & 255)),
                Character::Encode(static_cast<char16>((m_pRunner->GetType() >> 16) & 255)),
                Character::Encode(static_cast<char16>((m_pRunner->GetType() >>  8) & 255)),
                Fixnum::Encode(reinterpret_cast<Int>(m_pRunner->GetOuter()) >> 4),
                Fixnum::Encode(reinterpret_cast<Int>(m_pRunner->GetOuter()) & 15) );
        }
        else
        {
            dbg_format(L"~X~X ~C~C~C outer=~X~X ~S~%",
                Fixnum::Encode(reinterpret_cast<Int>(m_pFunFrame->m_pvRA) >> 4),
                Fixnum::Encode(reinterpret_cast<Int>(m_pFunFrame->m_pvRA) & 15),
                Character::Encode(static_cast<char16>((m_pRunner->GetType() >> 24) & 255)),
                Character::Encode(static_cast<char16>((m_pRunner->GetType() >> 16) & 255)),
                Character::Encode(static_cast<char16>((m_pRunner->GetType() >>  8) & 255)),
                Fixnum::Encode(reinterpret_cast<Int>(m_pRunner->GetOuter()) >> 4),
                Fixnum::Encode(reinterpret_cast<Int>(m_pRunner->GetOuter()) & 15),
                m_pFunFrame->m_fn );
        }
    #endif

    if (NULL == m_pFunFrame)
    {
        switch (m_pRunner->GetType())
        {
        default:
            m_pRunner = m_pRunner->GetOuter();
            return;

        case Frame::Type_ToForeign:
        case Frame::Type_ToKernel:
        {
            ToForeignFrame* pFrame =
                reinterpret_cast<ToForeignFrame*>(m_pRunner);

            UInt* pRa = pFrame->GetRaPtr();

            Val fn = map_ra_to_fn(*pRa);

            m_pRunner = m_pRunner->GetOuter();

            m_oFunFrame.m_pOuter     = m_pRunner;
            m_oFunFrame.m_fn         = fn;
            m_oFunFrame.m_pval       = reinterpret_cast<Val*>(pRa + 1);
            m_oFunFrame.m_pvRA       = pRa;
            m_oFunFrame.m_pvCallerRA = compute_caller_ra(fn, pRa);

            m_oFunFrame.m_ip = Fixnum::Encode(
                reinterpret_cast<uint8*>(*pRa) - getCodeVec(fn) );

            m_pFunFrame = &m_oFunFrame;

            if (m_pRunner > m_oFunFrame.m_pvCallerRA)
            {
                m_pRunner = m_pFunFrame;
            }

            return;
        } // ToForeign
        } // switch frame type
    } // if

    if (m_pRunner != &m_oFunFrame)
    {
        m_pRunner = m_pRunner->GetOuter();

        if (m_pRunner > m_oFunFrame.m_pvCallerRA)
        {
            m_oFunFrame.m_pOuter = m_pRunner;
            m_pRunner = &m_oFunFrame;
        }
        return;
    } // if

    m_pRunner = m_oFunFrame.GetOuter();

    // Is From Foreign Frame?
    {
        FromForeignFrame* pFrame = m_pRunner->
            DynamicCast<FromForeignFrame>();

        if (NULL != pFrame)
        {
            if (m_oFunFrame.m_pvCallerRA ==
                reinterpret_cast<UInt*>(m_pRunner) - 1 )
            {
                m_pFunFrame = NULL;
                return;
            }
        }
    }

    // Function Frame
    {
        UInt* pRa = reinterpret_cast<UInt*>(m_oFunFrame.m_pvCallerRA);

        Val fn = map_ra_to_fn(*pRa);
        m_oFunFrame.m_pOuter     = m_pRunner;
        m_oFunFrame.m_fn         = fn;
        m_oFunFrame.m_pval       = reinterpret_cast<Val*>(pRa + 1);
        m_oFunFrame.m_pvRA       = pRa;
        m_oFunFrame.m_pvCallerRA = compute_caller_ra(fn, pRa);

        m_oFunFrame.m_ip = Fixnum::Encode(
            reinterpret_cast<uint8*>(static_cast<Int>(*pRa)) - 
            getCodeVec(fn) );

        if (m_pRunner > m_oFunFrame.m_pvCallerRA)
        {
            m_pRunner = m_pFunFrame;
        }
    }
} // Thread::EnumFrame

} // Kernel
