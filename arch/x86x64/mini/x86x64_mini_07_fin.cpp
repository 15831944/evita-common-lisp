#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 7 Objects
// arch/x86x64/genesis/x86x64_mini_07_fin.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/mini/x86x64_mini_07_fin.cpp#5 $
//
#include "../../../mini/mini_lisp.h"

#include "../kernel/x86x64_ke_layout.h"
#include "../kernel/x86x64_ke_thread.h"    // SVC_error

namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// subst_in_function
//
// Description:
//  Called by %defmethod to embed a method object into function body for
//  invoking no-next-method-error error with method object.
//
Val subst_in_function(Val newitem, Val olditem, Val fun, Val visits)
{
    if (nil != memq(fun, visits))
    {
        return nil;
    }

    Val delta1 = nil;
    Val subst  = nil;

    FunObj* pFunObj = fun->Decode<FunObj>();
    foreach (FunObj::EnumAnnon, oEnum, pFunObj)
    {
        FunObj::Annon oAnnon = oEnum.Get();
        switch (oAnnon.m_eType)
        {
        case FunObj::Annon::Type_LispVal:
        case FunObj::Annon::Type_ClosedLit:
        case FunObj::Annon::Type_ExitPoint:
            delta1 = pFunObj->FetchVal(oAnnon.m_ofs);
            if (delta1 == olditem)
            {
                pFunObj->PatchVal(oAnnon.m_ofs, newitem);
                delta1 = newitem;
                subst = t;
            }
            else if (functionp(delta1))
            {
                Val subst2 = subst_in_function(
                    newitem,
                    olditem,
                    delta1,
                    cons(fun, visits) );

                if (nil != subst2)
                {
                    subst = t;
                }
            }
            break;

        case FunObj::Annon::Type_LocalCallee:
            delta1 = pFunObj->FetchCallee(oAnnon.m_ofs);
            if (delta1 == olditem)
            {
                pFunObj->PatchCallee(oAnnon.m_ofs, newitem);
                delta1 = newitem;
                subst  = t;
            }
            else
            {
                Val subst2 = subst_in_function(
                    newitem,
                    olditem,
                    delta1,
                    cons(fun, visits) );

                if (nil != subst2)
                {
                    subst = t;
                }
            }
            break;

        case FunObj::Annon::Type_Callee:
        case FunObj::Annon::Type_NamedCallee:
            delta1 = pFunObj->FetchCallee(oAnnon.m_ofs);
            if (delta1 == olditem)
            {
                pFunObj->PatchCallee(oAnnon.m_ofs, newitem);
                delta1 = newitem;
                subst = t;
            }
            break;

        case FunObj::Annon::Type_SymFun:
        {
            UInt ofs = pFunObj->FetchUn(oAnnon.m_ofs);

            Symbol* pItem = FromInt<Symbol>(
                ofs - offsetof(Symbol, m_function) );

            delta1 = pItem->Encode();

            if (delta1 == olditem)
            {
                Symbol* pNew = newitem->Decode<Symbol>();
                pFunObj->PatchUn(
                    oAnnon.m_ofs,
                    reinterpret_cast<UInt>(&pNew->m_function) );
                delta1 = newitem;
                subst = t;
            }
            break;
        } // symfun

        case FunObj::Annon::Type_ClosedVar:
        {
            UInt ofs = pFunObj->FetchUn(oAnnon.m_ofs);

            ClosedCell* pItem = FromInt<ClosedCell>(
                ofs - offsetof(ClosedCell, m_value) );

            delta1 = pItem->Encode();

            if (delta1 == olditem)
            {
                ClosedCell* pNew = newitem->Decode<ClosedCell>();
                pFunObj->PatchUn(oAnnon.m_ofs, &pNew->m_value);

                delta1 = newitem;
                subst = t;
            }
        } // closedvar

        case FunObj::Annon::Type_SymSetf:
        {
            UInt ofs = pFunObj->FetchUn(oAnnon.m_ofs);

            SetfCell* pItem = FromInt<SetfCell>(
                ofs - offsetof(SetfCell, m_function) );

            delta1 = pItem->Encode();

            if (delta1 == olditem)
            {
                SetfCell* pNew = newitem->Decode<SetfCell>();
                pFunObj->PatchUn(oAnnon.m_ofs, &pNew->m_function);

                delta1 = newitem;
                subst = t;
            }
        } // symsetf

        case FunObj::Annon::Type_SymVal:
        {
            UInt ofs = pFunObj->FetchUn(oAnnon.m_ofs);

            ValueCell* pItem = FromInt<ValueCell>(
                ofs - offsetof(ValueCell, m_value) );

            delta1 = pItem->Encode();

            if (delta1 == olditem)
            {
                ValueCell* pNew = newitem->Decode<ValueCell>();
                pFunObj->PatchUn(oAnnon.m_ofs, &pNew->m_value);

                delta1 = newitem;
                subst = t;
            }
        } // symval

        case FunObj::Annon::Type_Delta2:
        {
            Val item = reinterpret_cast<Val>(
                delta1->ToInt() - pFunObj->FetchSn(oAnnon.m_ofs) );

            if (item == olditem)
            {
                pFunObj->PatchUn(
                    oAnnon.m_ofs,
                    delta1->ToInt() - newitem->ToInt() );

                delta1 = newitem;
                subst = t;
            }
            else
            {
                delta1 = item;
            }
            break;
        } // delta2

        // REVIEW: Should we support DllLink?
        } // switch annon
    } // for each annon
    return subst;
} // subst_in_function

} // MiniLisp
