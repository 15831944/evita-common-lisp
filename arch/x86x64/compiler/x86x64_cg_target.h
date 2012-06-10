//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - target
// arch/compiler/x86x64_cg_target.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_target.h#2 $
//
#if !defined(INCLUDE_arch_x86x64_compiler_x86x64_cg_target_h)
#define INCLUDE_arch_x86x64_compiler_x86x64_cg_target_h

#if MACH == MACH_x86
    #include "../../x86/compiler/x86_cg_target.h"
    using namespace X86;
#elif MACH == MACH_x64
    #include "../../x64/compiler/x64_cg_target.h"
    using namespace X64;
#else
    #error MACH must be either MACH_x86 or MACH_x64
#endif // MACH == MACH_x86

#endif //!defined(INCLUDE_arch_x86x64_compiler_x86x64_cg_target_h)
