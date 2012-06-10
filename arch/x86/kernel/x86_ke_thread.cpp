#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 - Thread
// arch/x86/kernel/x86_ke_thread.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/kernel/x86_ke_thread.cpp#6 $
//
// Description:
//  This file contains implementaiton of following Generic frame methods:
//      FinallyFrame::Unwind
//      XferFrame::Transfer
//
#include "./x86_ke_thread.h"

#include "./x86_ke_frame.h"
#include "./x86_ke_layout.h"
#include "./x86_ke_mach.h"

#include "../../../mini/mini_lisp.h"

namespace Kernel
{

using namespace X86;

void
install_service_aux(Thread* p, int iOffset, void* pv)
{
    ASSERT(0 == iOffset % 4);

    void** pJump = reinterpret_cast<void**>(p);
    pJump[iOffset / 4] = pv;
} // X86Initializer::install_service


// install_service
void
install_service(Thread* p, int iOffset, Val fun)
{
    if (symbolp(fun))
    {
        fun = symbol_function(fun);
    }
    if (SVC_fixnum_one == iOffset)
    {
        install_service_aux(p, iOffset, fun);
    }
    else
    {
        install_service_aux(
            p,
            iOffset, fun->Decode<FunObj>()->GetCodeVec() );
    }
} // install_service


void
Thread::MachInit()
{
    // Note: every thread MUST install services.

    install_service(this, SVC_alloc_bino_area, Qalloc_bino_area);
    install_service(this, SVC_alloc_code_area, Qalloc_code_area);
    install_service(this, SVC_alloc_cons_area, Qalloc_cons_area);
    install_service(this, SVC_alloc_reco_area, Qalloc_reco_area);

    install_service(this, SVC_fixnum_one,         Fixnum::Encode(1));
    install_service(this, SVC_arity_error,        QDarity_error);
    install_service(this, SVC_not_function,       QDnot_function);
    install_service(this, SVC_type_error,         QDtype_error);
    install_service(this, SVC_undefined_function, QDundefined_function);
    install_service(this, SVC_unbound_variable,   QDunbound_variable);

    install_service(this, SVC_error,              Qthread_error_hook);

    //install_service_aux(this, SVC_stack_alloc, SVC_C_stack_alloc);
} // Thread::MachInit

} // Kernel
