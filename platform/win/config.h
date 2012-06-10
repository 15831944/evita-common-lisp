//////////////////////////////////////////////////////////////////////////////
//
// evcl - root - configuration
// config.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/config.h#6 $
//
#if !defined(INCLUDE_config_h)
#define INCLUDE_config_h

// VC2005 = MSC14

#if _WIN64
    #define MACH            MACH_x64
    #define ke_arch_h       "../arch/x64/kernel/x64_ke_arch.h"
    #define ke_arch_cpp     "../arch/generic/kernel/gen_ke_arch64.cpp"
    #define cg_target_h     "../../arch/x64/compiler/x64_cg_target.h"
    #define cg_opcode_inc   "../../arch/x64/compiler/x64_ir_opcode.inc"

    // MSC14 produces wrong code for reinterpret_cast<Val>(64bit).
    #define HAS_BUG_64BIT_PTR_CONSTANT  1

    // MSC14 doesn't support int128.
    #define HAVE_INT128 0
#elif _WIN32
    #define MACH            MACH_x86
    #define ke_arch_h       "../arch/x86/kernel/x86_ke_arch.h"
    #define ke_arch_cpp     "../arch/generic/kernel/gen_ke_arch32.cpp"
    #define cg_target_h     "../../arch/x86/compiler/x86_cg_target.h"
    #define cg_opcode_inc   "../../arch/x86/compiler/x86_ir_opcode.inc"
#else
    #error "Unsupported architecture."
#endif


// Note: We should use __declspec(thread) instead of TlsGetIndex API since
// VC emits more efficient code. However, it doesn't work on VC2005.
// Internal data _tls_index("tlssup.c") isn't initialized.
#define USE_DECLSPEC_THREAD 0

#if NDEBUG
    #pragma function(memcmp)
    #if _M_IX86
        #pragma function(memcpy)
        #pragma function(memset)
    #endif // _M_IX86
#endif // NDEBG

#if 1
#define evcl_memcmp     memcmp
#define evcl_memcpy     memcpy
#define evcl_memmove    memmove
#define evcl_memset     memset
#endif

#if 0
int evcl_memcmp(const void* src1, const void* src2, size_t count);
void* evcl_memcpy(void* dst, const void* src, size_t count);
void* evcl_memmove(void* dst, const void* src, size_t count);
void* evcl_memset(void* dst, int c, size_t count);
#endif

#endif // !defined(INCLUDE_config_h)
