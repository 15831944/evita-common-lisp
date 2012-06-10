#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 Frame
// arch/x86/kernel/gen_ke_frame.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/kernel/x86_ke_frame.cpp#4 $
//
// Description:
//  This file contains implementaiton of following Generic frame methods:
//      FinallyFrame::Unwind
//      XferFrame::Transfer
//
#if MACH == MACH_x86

#include "./x86_ke_frame.h"

#include "./x86_ke_layout.h"
#include "./x86_ke_mach.h"

#include "../../../kernel/ke_thread.h"

namespace Generic
{

using namespace Kernel;

enum
{
    Tag_Function = FunObj::Tag,
}; // enum


typedef X86::FinallyFrame x86FinallyFrame;

//////////////////////////////////////////////////////////////////////
//
// FinallyFrame::Unwind
//
void __declspec(naked) __fastcall
FinallyFrame::Unwind(Thread*)
{
    __asm
    {
        // ecx = pFrame
        // edx = pThread

        // Save callee save register
        push ebp
        push ebx
        push esi
        push edi

        mov ebp, edx
        mov edx, ecx

        // Compute target address
        mov eax, [edx] FinallyFrame.m_fn
        add eax, SIZE FunObj - Tag_Function
        mov [esp-4], eax

        mov ecx, [edx] FinallyFrame.m_n

        // Copy arguments
        lea edi, [ebp+ecx] Thread.mv_value
        lea esi, [edx+ecx] FinallyFrame.mv_arg
        lea ebx, [edx] FinallyFrame.mv_arg[5*4]

     copy_loop:
        cmp esi, ebx
        jl copy_done

        sub edi, 4
        sub esi, 4
        mov eax, [esi]
        mov [edi], eax
        jmp copy_loop
     copy_done:

        // Load arguments for finally procedure
        mov eax, [edx] FinallyFrame.mv_arg[0*4]
        mov ebx, [edx] FinallyFrame.mv_arg[2*4]
        mov esi, [edx] FinallyFrame.mv_arg[3*4]
        mov edi, [edx] FinallyFrame.mv_arg[4*4]
        mov edx, [edx] FinallyFrame.mv_arg[1*4]

        // Call finally
        call dword ptr [esp-4]

        // Restore callee save register
        pop edi
        pop esi
        pop ebx
        pop ebp
        ret
    } // __asm
} // FinallyFrame::Unwind


//////////////////////////////////////////////////////////////////////
//
// TagbodyFrame::Transfer
//
void __declspec(noreturn) __declspec(naked) __fastcall
TagbodyFrame::Transfer(Thread*, Tag*)
{
    __asm
    {
        //  ecx = frame
        //  edx = thread
        mov eax, [esp+4]    // eax <- pTag
        mov ebp, edx        // ebp <- pThread
        mov esp, [ecx] TagbodyFrame.m_sp

        // Compute target address
        mov eax, [eax]
        shr eax, 2
        add eax, [ecx] TagbodyFrame.m_fn
        add eax, SIZE FunObj - Tag_Function
        jmp eax
    }
} // XferFrame::Transfer

//////////////////////////////////////////////////////////////////////
//
// XferFrame::Transfer
//
void __declspec(noreturn) __declspec(naked) __fastcall
XferFrame::Transfer(Thread*)
{
    __asm
    {
        //  ecx = frame
        //  edx = thread
        mov ebp, edx    // ebp <- pThread
        mov esp, [ecx] XferFrame.m_sp

        // Compute target address
        mov eax, [ecx] XferFrame.m_ip
        shr eax, 2
        add eax, [ecx] XferFrame.m_fn
        add eax, SIZE FunObj - Tag_Function
        mov [esp-4], eax

        // Load values
        mov ecx, [ebp] Thread.m_n
        mov eax, [ebp] Thread.mv_value[0*4]
        mov edx, [ebp] Thread.mv_value[1*4]
        mov ebx, [ebp] Thread.mv_value[2*4]
        mov esi, [ebp] Thread.mv_value[3*4]
        mov edi, [ebp] Thread.mv_value[4*4]
        stc

        // Transfer control
        jmp dword ptr [esp-4]
    } // __asm
} // XferFrmae::Transfer

} // Generic

namespace Kernel
{
void unwind_finally_frame(Frame* pFrame)
{
    pFrame->StaticCast<X86::FinallyFrame>()->Unwind(Kernel::Thread::Get());
} // unwind_finally_frame

} // Kernel

#endif // MACH == MACH_x86
