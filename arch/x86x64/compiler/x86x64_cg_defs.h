//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86x64 Machine Dependent Frame
// arch/kernel/x86x64_ke_thread.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_defs.h#2 $
//
#if !defined(INCLUDE_arch_x86x64_compiler_x86x64_cg_defs_h)
#define INCLUDE_arch_x86x64_compiler_x86x64_cg_defs_h

#if MACH == MACH_x86
    #include "../../x86/compiler/x86_cg_defs.h"
    using namespace X86;
#elif MACH == MACH_x64
    #include "../../x64/compiler/x64_cg_defs.h"
    using namespace X64;
#else
    #error MACH must be either MACH_x86 or MACH_x64
#endif // MACH == MACH_x86


#endif //!defined(INCLUDE_arch_x86x64_compiler_x86x64_cg_defs_h)
