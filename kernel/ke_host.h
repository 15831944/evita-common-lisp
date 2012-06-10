//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Target Machine
// ke_mach.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_host.h#1 $
//
#if !defined(INCLUDE_kernel_host_h)
#define INCLUDE_kernel_host_h

#if MACH == MACH_x86
    #include "../arch/x86/kernel/x86_ke_host.h"

#elif MACH == MACH_x64
    #include "../arch/x64/kernel/x64_ke_host.h"

#else
    #error "Unsupported host machine"
#endif // MACH

#endif //!defined(INCLUDE_kernel_host_h)
