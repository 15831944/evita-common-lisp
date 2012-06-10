//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 Machine Dependent Thread
// arch/kernel/x86_ke_thread.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/kernel/x86_ke_thread.h#2 $
//
#if !defined(INCLUDE_arch_x86_kernel_x86_ke_thread_h)
#define INCLUDE_arch_x86_kernel_x86_ke_thread_h

#include "../../../kernel/ke_thread.h"

namespace X86
{

//////////////////////////////////////////////////////////////////////
//
// Thread Service Code
//
enum SVC
{
    SVC_stack_alloc         = -124,
    SVC_alloc_bino_area     = -120,
    SVC_alloc_code_area     = -116,
    SVC_alloc_cons_area     = -112,
    SVC_alloc_reco_area     = -108,
    SVC_alloc_symb_area     = -104,

    SVC_arity_error         = -100,
    SVC_type_error          = -96,
    SVC_not_function        = -68,
    SVC_unbound_variable    = -64,
    SVC_undefined_function  = -60,
    SVC_error               = -56,  // for referencing error in boot image.

    SVC_fixnum_one          = -4,

    // Temporary assignment. Once we implement them in assembler, we don't
    // need to use thread service.
    SVC_go                  = -52,
    SVC_return_from         = -48,
    SVC_throw               = -44,
}; // SVC

}; // X86

#endif //!defined(INCLUDE_arch_x86_kernel_x86_ke_thread_h)
