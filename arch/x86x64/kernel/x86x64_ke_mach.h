//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86x64 Machine Dependent Frame
// arch/kernel/x86x64_ke_mach.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/kernel/x86x64_ke_mach.h#2 $
//
#if !defined(INCLUDE_arch_x86x64_kernel_x86x64_ke_mach_h)
#define INCLUDE_arch_x86x64_kernel_x86x64_ke_mach_h

#if MACH == MACH_x86
    #include "../../x86/kernel/x86_ke_mach.h"
#elif MACH == MACH_x64
    #include "../../x64/kernel/x64_ke_mach.h"
#else
    #error MACH must be either MACH_x86 or MACH_x64
#endif // MACH == MACH_x86


#endif //!defined(INCLUDE_arch_x86x64_kernel_x86x64_ke_mach_h)
