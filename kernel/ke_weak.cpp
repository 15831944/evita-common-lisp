#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Weak Objects
// kernel/ke_weak.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_weak.cpp#3 $
//
#include "./ke_weak.h"

#include "./ke_layout.h"
#include "./ke_lock.h"
#include "./ke_memory.h"
#include "./ke_thread.h"

namespace Kernel
{

//////////////////////////////////////////////////////////////////////
//
// Weak::Alloc
//  Allocates memory chunk in weak object area.
//
void*  Weak::Alloc(size_t cbObject)
{
    Thread* pThread = Thread::Get();

    LatchLock oLock(
        pThread,
        VAR(Aweak_area_latchA)->Decode<Latch>(),
        Kexclusive );
    
    Area* pArea = VAR(Aweak_areaA)->StaticCast<Area>();
    if (NULL == pArea)
    {
        pArea = Memory::AllocDataArea(
            pThread,
            Area::ScanType_Weak,
            cbObject,
            Area::Age_Min );

        VAR(Aweak_areaA) = Fixnum::Encode(pArea);
    } // if

    for (;;)
    {
        void* pv = pArea->Alloc(cbObject);

        if (NULL != pv)
        {
            return pv;
        } // if

        pArea = Memory::AllocDataArea(
            pThread,
            Area::ScanType_Weak,
            cbObject,
            Area::Age_Min );

        VAR(Aweak_areaA) = Fixnum::Encode(pArea);
    } // for
} // Weak::Alloc

} // Kernel
