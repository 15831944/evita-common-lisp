#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 - Gc
// arch/x86/kernel/x86_ke_gc.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/kernel/x86_ke_gc.cpp#11 $
//
// Description:
//  This file contains implementation of host specific GC methods:
//      prepareFunObj
//      rememberFunObj
//      scanFunObj
//      updateRS_funobj
//      updateThreadStack
//
#include "./x86_ke_gcmap.h"
#include "./x86_ke_layout.h"
#include "./x86_ke_thread.h"

#include "../../../kernel/ke_gc.h"

namespace Kernel
{

void Gc::afterUpdateThread(Thread*) {}


// Update thread service table.
void Gc::beforeUpdateThread(Thread* pThread)
{
    Int* pStart = reinterpret_cast<Int*>(
        pThread->ToInt() - X86::X86Mach::Thread_Lead_Extra );

    Int* pEnd   = reinterpret_cast<Int*>(pThread);

    for (Int* pRunner = pStart; pRunner < pEnd; pRunner++)
    {
        void* pv = reinterpret_cast<void*>(*pRunner);
        if (Memory::IsHeap(pv))
        {
            Val fn = reinterpret_cast<Val>(
                *pRunner - sizeof(FunObj) + FunObj::Tag );

            fn = moveObject(fn);

            *pRunner = reinterpret_cast<Int>(fn->Decode<FunObj>() + 1);
        } // if
    } // for
} // Gc::beforeUpateThread


//////////////////////////////////////////////////////////////////////
//
// Gc::updateFrame
//
bool Gc::updateFrame(Thread* pThread, Frame* pFrameIn)
{
    ASSERT(NULL != pThread);

    FunctionFrame* pFrame = pFrameIn->DynamicCast<FunctionFrame>();
        if (NULL == pFrame) return false;

    pFrame->m_fn = moveObject(pFrame->m_fn);

    FunObj* pFunObj = pFrame->m_fn->Decode<FunObj>();

    UInt nRA = Fixnum::Decode_(pFrame->m_ip);

    *reinterpret_cast<UInt*>(pFrame->m_pvRA) =
        reinterpret_cast<UInt>(pFunObj->GetCodeVec()) + nRA;

    GcMap oGcMap(pFunObj->GetGcMap(), pFunObj->GetCodeSize());

    GC_PRINTF(L"==================================================\n");
    GC_FORMAT(L"~X ~S +~X~%",
        Fixnum::Encode(reinterpret_cast<UInt>(pFrame->m_pvRA)),
        pFrame->m_fn,
        pFrame->m_ip );

    const uint32* pGcDesc = oGcMap.FindByRA(nRA);

    if (NULL == pGcDesc)
    {
        GC_FORMAT(L"  no desc~%");
        return true;
    }

    uint32 nGcDesc = *pGcDesc++;

    GC_FORMAT(L"  GcDesc=~X~X~%",
        Fixnum::Encode(nGcDesc >> 4),
        Fixnum::Encode(nGcDesc & 15) );

    uint fContinue = nGcDesc & 1;
        nGcDesc >>= 1;

    uint cBits;
    {
        const uint kRegs = lengthof(X86::X86Mach::k_rgnGcMapIdx2Reg) - 1;
        switch (nGcDesc & 3)
        {
        case GcDesc_General:
            nGcDesc >>= kRegs + 2;
            cBits = 32 - (kRegs + 2 + 1);
            break;

        case GcDesc_StdCall:
            nGcDesc >>= 2;
            cBits = 32 - 3;
            break;       // 01c

        default:
            CAN_NOT_HAPPEN();
        } // type
    } // cBits

    // Scan live stack slots
    Val* pStack = pFrame->m_pval;
    for (;;)
    {
        while (0 != nGcDesc)
        {
            if (nGcDesc & 1)
            {
                *pStack = moveObject(*pStack);

                #if GC_DEBUG
                {
                    char16 wsz[100];
                        ::wsprintf(wsz, L"*[esp+%3d] %p = %p",
                            (pStack - pFrame->m_pval) * sizeof(Val),
                            pStack, *pStack );
                    ::OutputDebugString(wsz);

                    Gc::Format(L" ~S~%", *pStack);
                }
                #endif // GC_DEBUG

            }
            else
            {
                #if GC_DEBUG
                {
                    char16 wsz[100];
                        ::wsprintf(wsz, L" [esp+%3d] %p = %p\r\n",
                            (pStack - pFrame->m_pval) * sizeof(Val),
                            pStack, *pStack );
                    ::OutputDebugString(wsz);
                }
                #endif // GC_DEBUG
            }

            pStack++;
            nGcDesc >>= 1;
            cBits -= 1;
        } // while

        if (! fContinue) break;

        pStack += cBits;
        nGcDesc = *pGcDesc++;

        GC_FORMAT(L"  GcDesc=~X~X~%",
            Fixnum::Encode(nGcDesc >> 4),
            Fixnum::Encode(nGcDesc & 15) );

        fContinue = nGcDesc & 1;
        nGcDesc >>= 1;
        cBits = 31;
    } // for

    // Scan dynamic rest parameter
    //  Since we have no mark bit in cons cell, we always scan rest parameter
    //  area in stack.
    {
        FunObj* pFunObj = pFrame->m_fn->Decode<FunObj>();
        if (pFunObj->GetFrameType() == FunObj::FrameType_Restify)
        {
            UInt nStart =
                reinterpret_cast<UInt>(pFrame->m_pvRA) +
                pFunObj->GetFrameSize() +
                sizeof(UInt);

            // nStart += Kernel_Arch_Cons_Align - 1;
            // nStart &= ~(Kernel_Arch_Cons_Align - 1);

            Cons* pStart = reinterpret_cast<Cons*>(nStart);

            Cons* pEnd = reinterpret_cast<Cons*>(
                reinterpret_cast<UInt>(pFrame->m_pvCallerRA) &
                ~(Kernel_Arch_Cons_Align - 1) );

            GC_PRINTF(L"Scan restify [%p, %p]\r\n", pStart, pEnd);

            for (Cons* pRunner = pStart; pRunner < pEnd; pRunner++)
            {
                pRunner->m_car = moveObject(pRunner->m_car);

                GC_FORMAT(L"rest: ~X~X ~S~%",
                    Fixnum::Encode(pRunner->ToInt() >> 4),
                    Fixnum::Encode(pRunner->ToInt() & 15),
                    pRunner->m_car );

                pRunner->m_cdr = moveObject(pRunner->m_cdr);
            } // for each cons
        } // if restiy
    } // dynamic list

    return true;
} // Gc::updateFrame


//////////////////////////////////////////////////////////////////////
//
// GcMap::Skip
//
uint GcMap::Skip(uint32 nGcDesc)
{
    switch (nGcDesc & 6)
    {
    case GcDesc_General << 1:
        // -1 for origin of GcMapIdx2Reg
        // +3 for fContinue and DescType.
        return (lengthof(X86::X86Mach::k_rgnGcMapIdx2Reg) - 1) + 3;

    case GcDesc_StdCall << 1:
        return 3;

    default:
        CAN_NOT_HAPPEN();
    } // type
} // GcMap::Skip

} // Kernel
