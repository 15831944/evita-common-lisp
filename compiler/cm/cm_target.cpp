#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - main
// cm_main.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_target.cpp#3 $
//
#include "./cm_target.h"

namespace Compiler
{

Target* Target::sm_pTargets;

//////////////////////////////////////////////////////////////////////
//
// Target::Get
//
//  Get compilation target.
//
Target* Target::Get()
{
    Val name = TLV(c6_AtargetA);

    for (
        Target* pRunner = sm_pTargets;
        NULL != pRunner;
        pRunner = pRunner->m_pPrev )
    {
        if (pRunner->m_name == name)
        {
            return pRunner;
        }
    }

    return NULL;
} // Target::Get

} // Compiler
