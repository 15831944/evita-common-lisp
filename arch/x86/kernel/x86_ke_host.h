//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 host definition
// arch/kernel/x86_ke_host.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/kernel/x86_ke_host.h#2 $
//
#if !defined(INCLUDE_arch_x86_kernel_x86_ke_host_h)
#define INCLUDE_arch_x86_kernel_x86_ke_host_h

#include "./x86_ke_mach.h"

namespace Kernel
{
class Host : public X86::X86Mach {};
Val __fastcall CallLisp(Thread*);
__declspec(noreturn) void __fastcall GoToLisp(Thread*);

} // Kernel


#endif //!defined(INCLUDE_arch_x86_kernel_x86_ke_host_h)
