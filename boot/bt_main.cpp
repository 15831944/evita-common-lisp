#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - main
// genesis/main.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/boot/bt_main.cpp 29 2006-10-22 01:40:22 yosi $
//

#include "../kernel/ke_executive.h"
#include "../kernel/ke_memory.h"
#include "../mini/mini_lisp.h"

namespace Boot
{

extern void Build();
extern void Verify();
extern void Write(Thread*);

} // Boot

namespace Kernel
{
extern "C" Val DllLinkStab()
    { return NULL; }
} // Kernel

namespace CommonLisp
{

void __declspec(noreturn)
error(Val cond)
{
    invoke_debugger(cond);
} // error


void __declspec(noreturn)
error(Val datum, Val k1, Val v1)
{
    error(L"Error: ~S", list(datum, k1, v1));
} // error


void __declspec(noreturn)
error(Val datum, Val k1, Val v1, Val k2, Val v2)
{
    error(L"Error: ~S", list(datum, k1, v1, k2, v2));
} // error


Val eval(Val)
{
    error(L"You can't use eval during boot.");
} // eval


void __declspec(noreturn)
invoke_debugger(Val cond)
{
    Val str = format(nil, L"Failed to create build image:~2%~A", cond);

    ::MessageBox(
        NULL,
        str->Decode<SimpleString>()->m_rgwchElement,
        L"Evita Common Lisp Boot Image Maker.",
        MB_ICONERROR );
    ::DebugBreak();
} // invoke_debugger

} // CommonLisp

//#include "../arch/x86/kernel/x86_ke_layout.h"

//////////////////////////////////////////////////////////////////////
//
// Main Entry Point
//
extern "C" int __cdecl
main(int, char**)
{
    Debugger::Printf(L"Start on Windows %d.%d\n",
        WINVER >> 8,
        WINVER & 0xFF );

    // Note: We must initialize nil and Klocked for spinlock.
    {
        // See arch/{arch}/{arch}_ke_mach.h or
        //     arch/generic/gen_ke_arch{bit}.h
        nil = reinterpret_cast<Val>(ADDRESS_OF_NIL);
        Klocked = Fixnum::Encode(1);
    }

    const size_t k_cbHeap = 16 * 1024 * 1024;
    const size_t k_cbThread = sizeof(Kernel::Thread);

    Kernel::Executive::Start(k_cbHeap);

    Kernel::Thread* pThread =
        new(Kernel::Memory::AllocThread(k_cbThread))
            Kernel::Thread();

    pThread->Init(k_cbThread);

    Boot::Build();

    Boot::Verify();
    Boot::Write(pThread);

    close(TLV(Astandard_outputA));

    Debugger::Printf(L"nil = #x%p\n", nil);

    Debugger::Printf(L"End\n");

    if (::IsDebuggerPresent())
    {
        ::MessageBoxW(
                NULL,
                L"Done",
                L"Evita Common Lisp (Boot)",
                MB_ICONINFORMATION );
    } // if

    return EXIT_SUCCESS;
} // main
