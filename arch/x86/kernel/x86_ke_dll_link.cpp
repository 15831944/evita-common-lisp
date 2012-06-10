#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 - Machine
// arch/x86/genesis/x86_ke_mach.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/kernel/x86_ke_dll_link.cpp#3 $
//
#if MACH == MACH_x86

#include "../../../kernel/ke_dll_link.h"

#include "../../../kernel/ke_thread.h"

namespace Kernel
{

//////////////////////////////////////////////////////////////////////
//
// DllLinkStab
//
//  [1] Save argments into thread.mv_value
//  [2] Compute address of Link Vector entry
//  [3] Call C_resolver(thread, entry)
//  [4] Restore arguments
//  [5] Transfer control
//
//          +--------------+
//   esp+0  |     ecx      |
//          +--------------+
//   esp+4  |     edx      |
//          +--------------+
//   esp+8  |    callee    |    esp-4
//          +--------------+
//   esp+12 | RA of caller |    esp+0
//          +--------------+
extern "C" Val __declspec(naked) __fastcall
DllLinkStab()
{
    __asm
    {
        // [1] Save C/C++ arguments
        sub esp, 12
        mov [esp],    ecx   // the first argument
        mov [esp+4],  edx   // the second argument

        // [2] Compute address of Link Vector entry
        mov edx, [esp+12]   // eax <- return address of caller
        mov edx, [edx-4]    // eax <- ea of CALL [disp32] instruction

        // [3] Call DllResolve
        mov ecx, ebp
        call DllResolve
        mov [esp+8], eax    // save function entry point

        // [4] Restore arguments
        mov     edx, [esp+4]    // the first argument
        mov     ecx, [esp]      // the second argument
        add     esp, 12

        // [5] Transfer control
        jmp dword ptr [esp-4]   // transfer control to function
    } // __asm
} // DllLinkStab

} // Kernel

#endif // MACH == MACH_x86
