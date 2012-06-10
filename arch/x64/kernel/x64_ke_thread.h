//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x64 Machine Dependent Thread
// arch/kernel/x64_ke_thread.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/kernel/x64_ke_thread.h#2 $
//
#if !defined(INCLUDE_arch_x64_kernel_x64_ke_thread_h)
#define INCLUDE_arch_x64_kernel_x64_ke_thread_h

#include "../../../kernel/ke_thread.h"

namespace X64
{

//////////////////////////////////////////////////////////////////////
//
// Thread Service Code
//
enum SVC
{
    SVC_stack_alloc         = -13 * 8,
    SVC_alloc_bino_area     = -12 * 8,
    SVC_alloc_code_area     = -11 * 8,
    SVC_alloc_cons_area     = -10 * 8,
    SVC_alloc_reco_area     = -9 * 8,
    SVC_alloc_symb_area     = -8 * 8,

    SVC_arity_error         = -7 * 8,
    SVC_type_error          = -6 * 8,
    SVC_not_function        = -5 * 8,
    SVC_unbound_variable    = -4 * 8,
    SVC_undefined_function  = -3 * 8,
    SVC_error               = -2 * 8,

    SVC_fixnum_one          = -1 * 8,

    // Temporary assignment. Once we implement them in assembler, we don't
    // need to use thread service.
    SVC_go                  = -14 * 8,
    SVC_return_from         = -15 * 8,
    SVC_throw               = -16 * 8,
}; // SVC

}; // X64

#endif //!defined(INCLUDE_arch_x64_kernel_x64_ke_thread_h)
