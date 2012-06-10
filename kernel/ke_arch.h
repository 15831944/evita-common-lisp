//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Archuration
// ke_arch.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_arch.h#3 $
//
#if !defined(INCLUDE_kernel_arch_h)
#define INCLUDE_kernel_arch_h

#define MACH_x86   86
#define MACH_x64   64

#include ke_arch_h

namespace Kernel
{

typedef Arch::OffsetT       OffsetT;
typedef Arch::Bigit         Bigit;
typedef Arch::SignedBigit   SignedBigit;

} // Kernel

#endif //!defined(INCLUDE_kernel_arch_h)
