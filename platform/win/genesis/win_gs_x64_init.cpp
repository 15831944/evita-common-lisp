#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - initialization
// platform/win/genesis/win_gs_x64_init.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/genesis/win_gs_x64_init.cpp#4 $
//
// Description:
//  Installs following C implemented lisp function for genesis.
//
#include "../../genesis/gs_init.h"

#include "../../arch/x64/x64_asm.h"

namespace Genesis
{

void Initializer::init_Platform_Target()
{
    class Asm : public X64::X64Assembler
    {
        public: void Run()
        {
            // collect-garbage
            //              +---------------------+
            //       $sp-8  |   RA of wrapper     |<----------------+
            //              +---------------------+                 |
            //       $sp+0  |   home of home[0]   |     rcx         |
            //              +---------------------+                 |
            //       $sp+8  |   home of home[1]   |     rdx         |
            //              +---------------------+                 |
            //       $sp+16 |   home of home[2]   |     r8          |
            //              +---------------------+                 |
            //       $sp+24 |   home of home[3]   |     r9          |
            //              +---------------------+                 |
            //       $sp+32 |      $rtcb.m_fp     | <-- $rtcb.m_fp  |
            //              +---------------------+                 |
            //       $sp+40 |      ToKernel       |                 |
            //              +---------------------+                 |
            //       $sp+48 |    sizeof Args    o-+-----------------+
            //              +---------------------+
            //       $sp+56 | RA of lisp function | <-- $sp at entry
            //              +----------------------
            defun("SI::.COLLECT-GARBAGE", 0, -1)

                struct Local
                {
                    int64           m_home[4];  // +0
                    ToKernelFrame   m_oFrame;   // +32
                }; // Frame                     // +56

                frame_type(Fixed, sizeof(Local));

                sub($sp, sizeof(Local));

                // Dump parameters for kernel funciton.
                mov(ea_m_n(),  $rn);

                mov(ea_mv_value(0),  $r0);
                mov(ea_mv_value(1),  $r1);
                mov(ea_mv_value(2),  $r2);
                mov(ea_mv_value(3),  $r3);
                mov(ea_mv_value(4),  $r4);
                mov(ea_mv_value(5),  $r5);
                mov(ea_mv_value(6),  $r6);
                mov(ea_mv_value(7),  $r7);
                mov(ea_mv_value(8),  $r8);
                mov(ea_mv_value(9),  $r9);

                PUSH_TRANSITION_FRAME(ToKernel, $r3);

                mov(rcx, $rtcb);
                call(dlllink(L".", L"big_collect_garbage"));

                POP_TRANSITION_FRAME($r1);

                add($sp, sizeof(Local));

                mov($rn, Fixnum::Encode(2));
                mov($r1, ea_mv_value(1));
                stc();

                ret();
            end_defun()


            // get-internal-real-time
            defun("CL:GET-INTERNAL-REAL-TIME", 0, 0)
                struct Local
                {
                    int64           m_home[4];  // +0
                    ToKernelFrame   m_oFrame;   // +32
                }; // Frame                     // +56

                frame_type(Fixed, sizeof(Local));

                sub($sp, sizeof(Local));

                PUSH_TRANSITION_FRAME(ToKernel, $r3);

                call(dlllink(L"KERNEL32.DLL", L"GetTickCount"));
                shl($r0, Fixnum::TagBits);

                POP_TRANSITION_FRAME($r1);

                add($sp, sizeof(Local));
                ret();
            end_defun() // get-internal-real-time


            // get-internal-run-time
            defun("CL:GET-INTERNAL-RUN-TIME", 0, 0)
                struct Local
                {
                    int64           m_home[5];  // +0

                    ToKernelFrame   m_oFrame;   // +40

                    FILETIME        m_ftUser;   // +64
                    FILETIME        m_ftCreate; // +72
                    FILETIME        m_ftExit;   // +80
                    FILETIME        m_ftKernel; // +88

                    int64           m_pad96;    // +96
                    int64           m_pad104;   // +104
                    int64           m_pad112;   // +112
                }; // Local                     // +120

                frame_type(Fixed, sizeof(Local));
                sub($sp, sizeof(Local));

                PUSH_TRANSITION_FRAME(ToKernel, $r3);

                // Call getProcessTimes
                lea(rax, ea($sp, offsetof(Local, m_ftUser)));
                lea(r9,  ea($sp, offsetof(Local, m_ftKernel)));
                lea(r8,  ea($sp, offsetof(Local, m_ftExit)));
                lea(rdx, ea($sp, offsetof(Local, m_ftCreate)));
                mov(ea($sp, offsetof(Local, m_home[4])), rax);
                or(rcx, -1);    // hProcess
                call(dlllink(L"KERNEL32.DLL", L"GetProcessTimes"));

                mov(eax, ea($sp, offsetof(Local, m_ftUser)+
                                 offsetof(FILETIME, dwLowDateTime) ) );

                mov(edx, ea($sp, offsetof(Local, m_ftUser)+
                                 offsetof(FILETIME, dwHighDateTime) ) );

                // FIXME 2007-03-04: We should use MUL+SHR by division by
                // constant for 10000.
                mov(ecx, 10000);    // 100ns => 1ms
                div(ecx);
                shl($r0, Fixnum::TagBits);

                POP_TRANSITION_FRAME($r1);

                add($sp, sizeof(Local));   // CF=0
                ret();
            end_defun()
        } // Run
    }; // Asm
    Asm oAsm;
    oAsm.Run();
} // Initializer::init_Platform_Target

} // Genesis
