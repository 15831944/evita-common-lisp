#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - thread
// ke_thread.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_frame.cpp#3 $
//
#include "./ke_frame.h"

#include "./ke_memory.h"
#include "./ke_thread.h"

#include "../mini/mini_lisp.h"

namespace Kernel
{

//////////////////////////////////////////////////////////////////////
//
// BindFrame::Unwind
//
void
BindFrame::Unwind(Thread* pThread)
{
    ASSERT(NULL != pThread);

    foreach (BindFrame::Enum, oEnum, this)
    {
        const BindFrame::Entry* pEntry = oEnum.Get();

        if (value_cell_p(pEntry->m_name))
        {
            setf_value_cell_value(pEntry->m_value, pEntry->m_name);
        }
        else if (fixnump(pEntry->m_name))
        {
            pThread->SetTlv(
                pEntry->m_name->ToInt(),
                pEntry->m_value );
        }
        else
        {
            error(L"broken bind frame: ~S", Fixnum::Encode(this));
        }
    } // for each entry
} // BindFrame::Unwind


//////////////////////////////////////////////////////////////////////
//
// ObStackFrame::Unwind
//
void
ObStackFrame::Unwind(Thread* pThread)
{
    ASSERT(NULL != pThread);

    pThread->m_pObStackArea = m_pArea;
    m_pArea->m_ofsFree      = m_ofsFree;
} // ObStackFrame::Unwind

} // Kernel
