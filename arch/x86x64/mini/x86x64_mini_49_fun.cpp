#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - Kerenel - Layout
// arch/x64/x64_ke_layout.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/mini/x86x64_mini_49_fun.cpp#5 $
//
#include "../../../mini/mini_lisp.h"

#include "../kernel/x86x64_ke_layout.h"
#include "../kernel/x86x64_ke_thread.h"

namespace MiniLisp
{

// allocate_funobj
Val allocate_funobj(
    Val                 classd,
    uint                cbCodeVec,
    uint                cbAnnon,
    uint                cbGcMap,
    FunObj::FrameType   eFrame,
    uint                cbFrame )
{
    uint cbFunObj = sizeof(FuncallableInstance);
        cbFunObj += static_cast<uint>(ROUNDUP(cbCodeVec, 4));
        cbFunObj += cbAnnon;
        cbFunObj += cbGcMap;
        cbFunObj += sizeof(FunObj::Desc);
        cbFunObj = static_cast<uint>(
            ROUNDUP(cbFunObj, Host::FunObj_Align) );

    Val funobj = MiniThread::Get()->AllocFunction(classd, cbFunObj);

    FunObj* pFunObj = funobj->Decode<FunObj>();

    // Populate Desc
    FunObj::Desc* pDesc = pFunObj->GetDesc();

        pDesc->m_cbCodeVec = cbCodeVec;
        pDesc->m_ofsGcMap  = cbFunObj - sizeof(FunObj::Desc) - cbGcMap;
        pDesc->m_ofsAnnon  = pDesc->m_ofsGcMap - cbAnnon;

        pDesc->SetFrame(eFrame, cbFrame);

    // Set NOPs
#if 0
    {
        uint8* pbCode  = pFunObj->GetCodeVec() + cbCodeVec;
        uint8* pbAnnon = reinterpret_cast<uint8*>(pFunObj->GetAnnon());
        while (pbCode < pbAnnon) *pbCode++ = 0x90;  // op_NOP
    }
#else
    {
        uint8* pbCode  = pFunObj->GetCodeVec() + cbCodeVec;
        uint8* pbAnnon = reinterpret_cast<uint8*>(pFunObj->GetAnnon());

        evcl_memset(
            pbCode,
            0x90,
            pbAnnon - pbCode );
    }
#endif
    return funobj;
} // allocate_funobj

} // MiniLisp
