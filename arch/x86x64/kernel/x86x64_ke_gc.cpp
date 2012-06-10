#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 - Gc
// arch/x86/kernel/x86_ke_gc.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/kernel/x86x64_ke_gc.cpp#9 $
//
// Description:
//  This file contains implementation of host specific GC methods:
//      prepareFunObj
//      rememberFunObj
//      scanFunObj
//      updateRS_funobj
//      updateThreadStack
//
#include "./x86x64_ke_gcmap.h"
#include "./x86x64_ke_layout.h"
#include "./x86x64_ke_thread.h"

#include "../../../kernel/ke_dll_link.h"
#include "../../../kernel/ke_gc.h"

namespace Kernel
{

//////////////////////////////////////////////////////////////////////
//
// Gc::prepareFunObj
//
//  Description:
//   Updates relative address references
//
void Gc::prepareFunObj(Val newfn, Val oldfn)
{
    FunObj* pOld = oldfn->Decode<FunObj>();

    if (0 == pOld->m_nCookie)
    {
        return;
    }

    FunObj* pNew = newfn->Decode<FunObj>();
    foreach (FunObj::EnumAnnon, oEnum, pOld)
    {
        FunObj::Annon oAnnon = oEnum.Get();
        uint ofs = oAnnon.m_ofs;
        switch (oAnnon.m_eType)
        {
        case FunObj::Annon::Type_NamedCallee:
        case FunObj::Annon::Type_LocalCallee:
        case FunObj::Annon::Type_Callee:
            pNew->PatchCallee(ofs, pOld->FetchCallee(ofs));
            break;

        case FunObj::Annon::Type_Label:
        {
            Int nLabel = pOld->FetchUn(ofs);
                nLabel -= pOld->ToInt();
                nLabel += pNew->ToInt();
            pNew->PatchUn(ofs, static_cast<UInt>(nLabel));
            break;
        } // label

        #if MACH == MACH_x64
        case FunObj::Annon::Type_DllLink:
        {
            DllEntry* pEntry = pOld->FetchDllEntry(ofs);
            pNew->PatchDllEntry(ofs, pEntry);
            break;
        } // dlllink
        #endif // MACH == MACH_x64
        } // switch type
    } // for each annon
} // Gc::prepareFunObj


//////////////////////////////////////////////////////////////////////
//
// Gc::scanFunObj
//
void Gc::scanFunObj(uint nAge, Val fn)
{
    FunObj* pFunObj = fn->Decode<FunObj>();

    ASSERT(Memory::MapToArea(pFunObj)->GetAge() == nAge);

    if (0 == pFunObj->m_nCookie)
    {
        updateCell(nAge, &pFunObj->m_classd);
        updateCell(nAge, &pFunObj->m_name);
        return;
    }

    pFunObj->m_classd = moveObject(pFunObj->m_classd);
    pFunObj->m_name   = moveObject(pFunObj->m_name);

    bool fHasOldToYoung = false;

    Val obj1 = nil;
    foreach (FunObj::EnumAnnon, oEnum, pFunObj)
    {
        FunObj::Annon oAnnon = oEnum.Get();
        uint ofs = oAnnon.m_ofs;

        Val val = nil;

        switch (oAnnon.m_eType)
        {
        case FunObj::Annon::Type_ClosedLit:     // 11
        case FunObj::Annon::Type_LispVal:       // 0
            val = moveObject(pFunObj->FetchVal(ofs));
            pFunObj->PatchVal(ofs, val);
            obj1 = val;
            break;

        case FunObj::Annon::Type_Delta2:        // 6
            // delta2 = obj1 - obj2 => obj2 = obj1 - delta2
            val = reinterpret_cast<Val>(
                    obj1->ToInt() - pFunObj->FetchSn(ofs) );

            val = moveObject(val);

            pFunObj->PatchSn(
                ofs,
                static_cast<Int>(obj1->ToInt() - val->ToInt()) );
            break;

        case FunObj::Annon::Type_DllLink:       // 12
        {
            DllEntry* pEntry = pFunObj->FetchDllEntry(ofs);
            val = moveObject(pEntry->m_proc_info);
            pEntry->m_proc_info = val;
            break;
        } // dlllink

        case FunObj::Annon::Type_LocalCallee:   // 4
        case FunObj::Annon::Type_NamedCallee:   // 5
        case FunObj::Annon::Type_Callee:        // 6
            val = pFunObj->FetchCallee(ofs);
                ASSERT(val->Is<Funcallable>());
            val = moveObject(val);
            pFunObj->PatchCallee(ofs, val);
            break;

        case FunObj::Annon::Type_SymFun:        // 6
        {
            UInt nAddr = pFunObj->FetchUn(ofs);
                nAddr -= offsetof(Symbol, m_function);
            val = FromInt<Symbol>(nAddr)->Encode();
            val = moveObject(val);
            pFunObj->PatchUn(
                ofs,
                static_cast<UInt>(reinterpret_cast<Int>(
                    &val->Decode<Symbol>()->m_function )) );
            break;
        } // symfun

        case FunObj::Annon::Type_ClosedVar:     // 10
        {
            UInt nAddr = pFunObj->FetchUn(ofs);
            if (nAddr <= 0xFFFF)
            {
                val = nil;
            }
            else
            {
                nAddr -= offsetof(ClosedCell, m_value);
                val = FromInt<ClosedCell>(nAddr)->Encode();
                val = moveObject(val);
                pFunObj->PatchUn(
                    ofs,
                    reinterpret_cast<Int>(
                        &val->Decode<ClosedCell>()->m_value ) );
            }
            break;
        } // closedvar

        case FunObj::Annon::Type_SymSetf:       // 7
        {
            UInt nAddr = pFunObj->FetchUn(ofs);
                nAddr -= offsetof(SetfCell, m_function);
            val = FromInt<SetfCell>(nAddr)->Encode();
            val = moveObject(val);
            pFunObj->PatchUn(
                ofs,
                reinterpret_cast<Int>(
                    &val->Decode<SetfCell>()->m_function ) );
            break;
        } // symsetf

        case FunObj::Annon::Type_SymVal:        // 8
        {
            UInt nAddr = pFunObj->FetchUn(ofs);
                nAddr -= offsetof(ValueCell, m_value);
            val = FromInt<ValueCell>(nAddr)->Encode();
            val = moveObject(val);
            pFunObj->PatchUn(
                ofs,
                reinterpret_cast<Int>(
                    &val->Decode<ValueCell>()->m_value ) );
            break;
        } // symval
        } // switch type

        if (nil != val)
        {
            Area* pObjArea = Memory::MapToArea(val);
            if (NULL != pObjArea && pObjArea->GetAge() < nAge)
            {
                fHasOldToYoung = true;
            }
        }
    } // for each annon

    if (fHasOldToYoung)
    {
        rememberFunObj(nAge, fn);
    }
    else
    {
        remember(nAge, &pFunObj->m_classd);
        remember(nAge, &pFunObj->m_name);
    }
} // Gc::scanFunObj


//////////////////////////////////////////////////////////////////////
//
// Gc::updateRS_funobj
//
void Gc::updateRS_funobj(uint nAge, Val fn)
{
    FunObj* pFunObj = fn->Decode<FunObj>();

    ASSERT(Memory::MapToArea(pFunObj)->GetAge() == nAge);

    if (0 != pFunObj->m_nCookie)
    {
        Val obj1 = nil;
        foreach (FunObj::EnumAnnon, oEnum, pFunObj)
        {
            FunObj::Annon oAnnon = oEnum.Get();
            uint ofs = oAnnon.m_ofs;

            Val val = nil;

            switch (oAnnon.m_eType)
            {
            case FunObj::Annon::Type_ClosedLit:     // 11
            case FunObj::Annon::Type_LispVal:       // 0
                val = pFunObj->FetchVal(ofs);
                obj1 = val;
                break;

            case FunObj::Annon::Type_Delta2:        // 6
                // delta2 = obj1 - obj2 => obj2 = obj1 - delta2
                val = reinterpret_cast<Val>(
                        obj1->ToInt() - pFunObj->FetchSn(ofs) );
                break;

            case FunObj::Annon::Type_DllLink:       // 12
            {
                DllEntry* pEntry = pFunObj->FetchDllEntry(ofs);
                val = pEntry->m_proc_info;
                break;
            } // dlllink

            case FunObj::Annon::Type_LocalCallee:   // 4
            case FunObj::Annon::Type_NamedCallee:   // 5
            case FunObj::Annon::Type_Callee:        // 6
                val = pFunObj->FetchCallee(ofs);
                    ASSERT(val->Is<Funcallable>());
                break;

            case FunObj::Annon::Type_SymFun:        // 6
            {
                UInt nAddr = pFunObj->FetchUn(ofs);
                nAddr -= offsetof(Symbol, m_function);
                val = FromInt<Symbol>(nAddr)->Encode();
                break;
            } // symfun

            case FunObj::Annon::Type_ClosedVar:     // 10
            {
                UInt nAddr = pFunObj->FetchUn(ofs);
                if (nAddr <= 0xFFFF)
                {
                    val = nil;
                }
                else
                {
                    nAddr -= offsetof(ClosedCell, m_value);
                    val = FromInt<ClosedCell>(nAddr)->Encode();
                }
                break;
            } // closedvar

            case FunObj::Annon::Type_SymSetf:       // 7
            {
                UInt nAddr = pFunObj->FetchUn(ofs);
                    nAddr -= offsetof(SetfCell, m_function);
                val = FromInt<SetfCell>(nAddr)->Encode();
                break;
            } // symsetf

            case FunObj::Annon::Type_SymVal:        // 8
            {
                UInt nAddr = pFunObj->FetchUn(ofs);
                    nAddr -= offsetof(ValueCell, m_value);
                val = FromInt<ValueCell>(nAddr)->Encode();
                break;
            } // symval
            } // switch type

            if (nil != val && IsOldToYoung(nAge, val))
            {
                rememberFunObj(nAge, fn);
                return;
            }
        } // for each annon
    } // if

    remember(nAge, &pFunObj->m_classd);
    remember(nAge, &pFunObj->m_name);
} // Gc::updareRS_funobj


//////////////////////////////////////////////////////////////////////
//
// FunctionFrame::EnumFrameVar ctor
//
EnumFrameVar::EnumFrameVar(FunctionFrame* pFrame) :
    m_pRunner(pFrame->m_pval),
    m_pStart(pFrame->m_pval)
{
    FunObj* pFunObj = pFrame->m_fn->Decode<FunObj>();
    GcMap oGcMap(pFunObj->GetGcMap(), pFunObj->GetCodeSize());

    m_pGcDesc = oGcMap.FindByRA(Fixnum::Decode_(pFrame->m_ip));
    if (NULL == m_pGcDesc) return;

    m_nGcDesc = *m_pGcDesc++;
    m_fContinue = m_nGcDesc & 1;

    {
        uint nSkip = GcMap::Skip(m_nGcDesc);
        m_nGcDesc >>= nSkip;
        m_cBits = 32 - nSkip;
    } // cBits

    next();
} // FunctionFrame::EnumFrameVar::EnumFrameVar


//////////////////////////////////////////////////////////////////////
//
// FunctionFrame::EnumFrameVar::next
//
EnumFrameVar::Slot EnumFrameVar::Get() const
{
    ASSERT(! AtEnd());
    Int ofs = reinterpret_cast<Int>(m_pRunner) -
              reinterpret_cast<Int>(m_pStart);
    Val name = nil;
    return Slot(Fixnum::Encode(ofs), name, *m_pRunner);
} // EnumFrameVar::Get


//////////////////////////////////////////////////////////////////////
//
// FunctionFrame::EnumFrameVar::next
//
void EnumFrameVar::next()
{
    for (;;)
    {
        while (0 != m_nGcDesc)
        {
            if (m_nGcDesc & 1)
            {
                return;
            }
            m_pRunner++;
            m_nGcDesc >>= 1;
            m_cBits -= 1;
        } // while

        if (! m_fContinue)
        {
            m_pGcDesc = NULL;
            return;
        }

        m_pRunner += m_cBits;
        m_nGcDesc = *m_pGcDesc++;
        m_fContinue = m_nGcDesc & 1;
        m_nGcDesc >>= 1;
        m_cBits = 31;
    } // for
} // FunctionFrame::EnumFrameVar::next


} // Kernel
