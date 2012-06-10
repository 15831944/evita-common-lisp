//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x64 host definition
// arch/kernel/x64_ke_host.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/kernel/x64_ke_host.h#2 $
//
#if !defined(INCLUDE_arch_x64_kernel_x64_ke_host_h)
#define INCLUDE_arch_x64_kernel_x64_ke_host_h

#include "./x64_ke_mach.h"

namespace Kernel
{
class Host : public X64::X64Mach {};

} // Kernel

extern "C" Val __fastcall CallLisp(Thread*);

extern "C" void __declspec(noreturn) GoToLisp(Thread*)
    __attribute__((noreturn));

#endif //!defined(INCLUDE_arch_x64_kernel_x64_ke_host_h)
