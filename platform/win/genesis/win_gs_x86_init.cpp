#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - initialization
// platform/win/genesis/win_gs_x86_init.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/genesis/win_gs_x86_init.cpp#4 $
//
// Description:
//  Installs following C implemented lisp function for genesis.
//
#include "../../genesis/gs_init.h"

#include "../../arch/x86/x86_asm.h"

namespace Genesis
{

void Initializer::init_Platform_Target()
{
    class Asm : public X86::X86Assembler
    {
        public: void Run()
        {
            // .collect-garbage
            defun("SI::.COLLECT-GARBAGE", 0, -1)
                struct Frame : Kernel::ToForeignFrame {};
                frame_type(Fixed, sizeof(Frame));

                sub($sp, sizeof(Frame));

                mov(ea_m_n(), $rn);
                mov(ea_mv_value(0), $r0);
                mov(ea_mv_value(1), $r1);
                mov(ea_mv_value(2), $r2);
                mov(ea_mv_value(3), $r3);
                mov(ea_mv_value(4), $r4);

                // Push ToKernel Frame
                mov($r0, ea_m_fp());
                mov(ea($sp, offsetof(Frame, m_pOuter)), $r0);
                mov(ea($sp, offsetof(Frame, m_type)),
                    Kernel::Frame::Type_ToKernel );
                mov(ea($sp, offsetof(Frame, m_cbArgs)), 0);
                mov(ea_m_fp(), $sp);

                // Call kernel function
                mov(ecx, $tcb);
                call(dlllink(L".", L"big_collect_garbage"));

                // Pop ToKernel Frame
                mov($rn, ea($sp, offsetof(Frame, m_pOuter)));
                mov(ea_m_fp(), $rn);

                add($sp, sizeof(Frame));

                mov($rn, Fixnum::Encode(2));
                mov($r1, ea_mv_value(1));
                stc();
                ret();
            end_defun()

            // get-internal-real-time
            defun("CL:GET-INTERNAL-REAL-TIME", 0, 0)
                struct Frame : Kernel::ToForeignFrame {};
                frame_type(Fixed, sizeof(Frame));
                sub($sp, sizeof(Frame));

                // Push ToKernel Frame
                mov($r0, ea_m_fp());
                mov(ea($sp, offsetof(Frame, m_pOuter)), $r0);
                mov(ea($sp, offsetof(Frame, m_type)),
                    Kernel::Frame::Type_ToKernel );
                mov(ea($sp, offsetof(Frame, m_cbArgs)), 0);
                mov(ea_m_fp(), $sp);

                // Call GetTickCount
                call(dlllink(L"KERNEL32.DLL", L"GetTickCount"));

                // Convert tick count to fixnum
                and($r0, Fixnum::MostPositive);
                shl($r0, Fixnum::TagBits);

                // Pop ToKernelFrame
                mov($r1, ea($sp, offsetof(Frame, m_pOuter)));
                mov(ea_m_fp(), $r1);

                add($sp, sizeof(Frame));   // CF=0
                ret();
            end_defun()

            // get-internal-run-time
            defun("CL:GET-INTERNAL-RUN-TIME", 0, 0)
                struct Frame : Kernel::ToForeignFrame
                {
                    FILETIME    m_ftUser;
                    FILETIME    m_ftCreate;
                    FILETIME    m_ftExit;
                    FILETIME    m_ftKernel;
                }; // Frame

                frame_type(Fixed, sizeof(Frame));
                sub($sp, sizeof(Frame));

                // Push ToKernel Frame
                mov($r0, ea_m_fp());
                mov(ea($sp, offsetof(Frame, m_pOuter)), $r0);
                mov(ea($sp, offsetof(Frame, m_type)),
                    Kernel::Frame::Type_ToKernel );
                mov(ea($sp, offsetof(Frame, m_cbArgs)), 0);
                mov(ea_m_fp(), $sp);

                // Call getProcessTimes
                lea($r0, ea($sp, offsetof(Frame, m_ftUser)));
                push($r0);      // &m_ftUser
                add($r0, sizeof(FILETIME));
                push($r0);      // &m_ftKernel
                add($r0, sizeof(FILETIME));
                push($r0);      // &m_ftExit
                add($r0, sizeof(FILETIME));
                push($r0);      // &m_ftCreate
                push(-1);       // hProcess
                call(dlllink(L"KERNEL32.DLL", L"GetProcessTimes"));

                mov(edx, ea($sp, offsetof(Frame, m_ftUser) +
                                  offsetof(FILETIME, dwHighDateTime) ));

                mov(eax, ea($sp, offsetof(Frame, m_ftUser) +
                                  offsetof(FILETIME, dwLowDateTime) ));

                // FIXME 2007-03-04: We should use MUL+SHR by division by
                // constant for 10000.
                mov(ecx, 10000);    // 100ns => 1ms
                div(ecx);
                and($r0, Fixnum::MostPositive);
                shl($r0, Fixnum::TagBits);

                // Pop ToKernelFrame
                mov($r1, ea($sp, offsetof(Frame, m_pOuter)));
                mov(ea_m_fp(), $r1);

                add($sp, sizeof(Frame));   // CF=0
                ret();
            end_defun()
        } // Run
    }; // Asm
    Asm oAsm;
    oAsm.Run();
} // Initializer::init_Platform_Target

} // Genesis
