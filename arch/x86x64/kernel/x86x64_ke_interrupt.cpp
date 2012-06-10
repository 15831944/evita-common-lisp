#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 - Interrupt
// arch/x86/kernel/x86_ke_interrupt.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/kernel/x86x64_ke_interrupt.cpp#5 $
//
// Description:
//  This file contains implementaiton of following Generic frame methods:
//      FinallyFrame::Unwind
//      XferFrame::Transfer
//
#include "./x86x64_ke_interrupt.h"

// #include "../mini/x86x64_mini_lisp.h"

namespace Kernel
{

using namespace MiniLisp;


Val map_ra_to_fn(UInt);
UInt* compute_caller_ra(Val, UInt*);


////////////////////////////////////////////////////////////
//
// map_ip_to_fn
//
static Val map_ip_to_fn(UInt ip)
{
    Area* pArea = Memory::MapToArea(reinterpret_cast<void*>(ip));
    if (NULL == pArea) return nil;
    // BUGBUG: NYI: map_ip_to_fn: Mapping function in obsack.
    if (Area::ScanType_Function != pArea->GetType()) return nil;
    return map_ra_to_fn(ip);
} // map_ip_to_fn


// reroute
static void reroute(
    Thread*     pThread,
    CONTEXT*    pContext,
    Val         fn,
    uint        safe_ofs,
    Val         detour )
{
    X86X64Interrupt::Push(
        pContext, 
        reinterpret_cast<UInt>(
            fn->Decode<FunObj>()->GetCodeVec() + safe_ofs ) );

    X86X64Interrupt::SetIP(pContext, detour->Decode<FunObj>()->GetCodeVec());

    if (! ::SetThreadContext(pThread->m_hThread, pContext))
    {
        Debugger::Fail(L"SetThreadContext: %u", ::GetLastError());
    }
} // reroute


// detour_call
static void detour_call(
    Thread*     pThread,
    CONTEXT*    pContext,
    Val         fn,
    uint        curr_ofs,
    uint        safe_ofs,
    Val         callee )
{
    Val detour = X86X64Interrupt::make_detour_call(
        fn, curr_ofs, safe_ofs, callee );

    reroute(pThread, pContext, fn, safe_ofs, detour);
} // detour_call


// detour_jump
static void detour_jump(
    Thread*     pThread,
    CONTEXT*    pContext,
    Val         fn,
    uint        curr_ofs,
    uint        safe_ofs,
    uint32      gcdesc,
    Val         callee )
{
    Val detour = X86X64Interrupt::make_detour_jump(
        fn, curr_ofs, safe_ofs, gcdesc, callee );

    reroute(pThread, pContext, fn, safe_ofs, detour);
} // detour_jump

// detour_ra
static void detour_ra(Val fn, UInt* pRa, Val callee)
{
    Val detour = X86X64Interrupt::make_detour_ra(
        fn, *pRa, callee );

    *pRa = reinterpret_cast<UInt>(detour->Decode<FunObj>()->GetCodeVec());
} // detour_ra


// Thread::Interrupt
bool Thread::Interrupt(Val callee)
{
    SuspendScope oSuspend(this);

    // BUGBUG: NYI: check without-interrupt

    CONTEXT oContext;
        oContext.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
        if (! ::GetThreadContext(m_hThread, &oContext))
        {
            Debugger::Fail(L"GetThreadContext");
        }

    // BUGBUG: We should have another way to initialize TLS.
    init_tls();

    UInt ip = X86X64Interrupt::GetIP(&oContext);

    Val fn = map_ip_to_fn(ip);
    if (nil == fn)
    {
        UInt* pRa = NULL;
        foreach (Thread::EnumFrame, oEnum, this)
        {
            Frame* pFrame = oEnum.Get();
            UInt* pRaOfWrapper;

            switch (pFrame->GetType())
            {
            case Frame::Type_ToForeign:
                pRaOfWrapper  =
                    pFrame->StaticCast<ToForeignFrame>()->GetRaPtr();
                break;

            case Frame::Type_ToKernel:
                pRaOfWrapper  =
                    pFrame->StaticCast<ToKernelFrame>()->GetRaPtr();
                break;

            default:
                continue;
            } // switch frame

            Val wrapper = map_ra_to_fn(*pRaOfWrapper);

            pRa = compute_caller_ra(wrapper, pRaOfWrapper);

            fn = map_ra_to_fn(*pRa);
            break;
        } // for each frame

        if (nil == fn)
        {
            DEBUG_PRINTF(L"No ToForeign Frame. Why?\r\n");
            return false;
        }

        if (Qstart_application->Decode<Symbol>()->m_function == fn)
        {
            DEBUG_PRINTF(L"start-application\r\n");
            return true;
        }

        DEBUG_PRINTF(L"detour_ra for foreign\r\n");
        detour_ra(fn, pRa, callee);
        return true;
    } // if

    FunObj* pFunObj = fn->Decode<FunObj>();

    uint curr_ofs = static_cast<uint>(
        ip - reinterpret_cast<Int>(pFunObj->GetCodeVec()) );

    // At function entry
    if (0 == curr_ofs)
    {
        // BUGBUG: We can't use detour_call for non-standard call.
        DEBUG_PRINTF(L"detour_call at entry\r\n");
        detour_call(this, &oContext, fn, 0, 0, callee);
        return true;
    }

    GcMap oGcMap(pFunObj->GetGcMap(), pFunObj->GetGcMapSize());

    uint32 cookie = oGcMap.FindAfter(curr_ofs);
    if (0 == cookie)
    {
        // No GC Safe point. (no call, no jump)
        DEBUG_PRINTF(L"detour_ra for no info\r\n");
        UInt* pRa = X86X64Interrupt::GetSP(&oContext);
        detour_ra(fn, pRa, callee);
    }
    else
    {
        uint safe_ofs = oGcMap.GetOffset(cookie);
        const uint32* gcdesc = oGcMap.GetDesc(cookie);
        switch ((*gcdesc >> 1) & 3)
        {
        case GcDesc_General:
            DEBUG_PRINTF(L"detour_jump at %x->%x\r\n", curr_ofs, safe_ofs);
            detour_jump(this, &oContext, fn, curr_ofs, safe_ofs, *gcdesc,
                callee );
            break;

        case GcDesc_StdCall:
            DEBUG_PRINTF(L"detour_call at %x->%x\r\n", curr_ofs, safe_ofs);
            detour_call(this, &oContext, fn, curr_ofs, safe_ofs, callee);
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch gcdesc
    } // if

    return true;
} // Thread::Interrupt

} // Kernel
