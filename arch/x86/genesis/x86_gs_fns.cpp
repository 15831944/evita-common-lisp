#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - x86 - Utility Functions
// arch/x86/genesis/x86_gs_fns.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/genesis/x86_gs_fns.cpp#9 $
//
#include "../x86_arch.h"
#include "../x86_asm.h"

namespace Genesis
{

using namespace X86;

//////////////////////////////////////////////////////////////////////
//
//  Make C-function wrappering function object.
//
//  Code for variable arity:
//      push Kernel::Frame::Type_ToForegin
//      push [ebp]Context.m_fp
//
//      mov  [ebp]Context.m_fp, esp
//      mov  [ebp]Context.m_n, ecx
//      mov  [ebp]Context.mv_value[0*4], eax
//      mov  [ebp]Context.mv_value[1*4], edx
//      mov  [ebp]Context.mv_value[2*4], ebx
//      mov  [ebp]Context.mv_value[3*4], esi
//      mov  [ebp]Context.mv_value[4*4], edi
//      mov  ecx, ebp        // pointer to Kernel::Thread.
//      call foo
//
//      pop  [ebp]Context.m_fp
//      add  esp, 4         // CF=0
//      ret
//
//  Code for fixed arity:
//      push Kernel::Frame::Type_ToForegin
//      push [ebp]Context.m_fp
//
//      push ebx
//      push esi
//      push edi
//      push [ebp]Context.mv_value[5*4]
//      push [ebp]Context.mv_value[6*4]
//      ...
//      ;;mov edx, edx      // second argument to C
//      mov ecx, eax        // first argument to C
//      call foo
//
//      pop  [ebp]Context.m_fp
//      add  esp, 4         // CF=0
//      ret
//
//              +---------------------+
//      esp-4   |   RA of wrapper     |<----------------+
//              +---------------------+                 |
//      esp+0   |   home of arg[2]    |                 |
//              +---------------------+                 |
//      esp+4   |   home of arg[3]    |                 |
//              +---------------------+                 |
//      esp+8   |   home of arg[4]    |                 |
//              +---------------------+                 |
//      esp+16  |   home of arg[5]    |                 |
//              +---------------------+                 |
//                       ...                            |
//              +---------------------+                 |
//              |      $tcb.m_fp      | <-- $tcb.m_fp   |
//              +---------------------+                 |
//              |      ToForeign      |                 |
//              +---------------------+                 |
//              |    pointer to RA  o-+-----------------+
//              +---------------------+
//              | RA of lisp function | <-- esp at entry
//              +----------------------
//
Val make_wrapper(
    int             fGc,
    Val             name,
    int             iMin,
    int             iMax,
    const char16*   pwszProc,
    int             cVals )
{
    class Asm : X86Assembler
    {
        public: Val Run(
            int             fGc,
            Val             name,
            const char16*   pwszProc,
            int             iMin,
            int             iMax,
            int             cVals )
        {
            if (-1 == iMax)
            {
                iMax = X86::X86Mach::Lambda_Parameters_Limit;
            }

            ASSERT(iMin <= iMax);

            int cRegs = static_cast<uint>(iMax) > reg_arg_limit() ?
                reg_arg_limit() :
                iMax;

            int cbForForeign = 0;

            if (iMin == iMax && iMin > 2)
            {
                cbForForeign = (iMin - 2) * sizeof(Val);
            }

            Defun oDefun(this, name, iMin, iMax);

            int cbFrame = cbForForeign;
                cbFrame += sizeof(ToForeignFrame);

            frame_type(Fixed, cbFrame);

            emit_prologue(&oDefun);

            // Make ToForeign Frame
            {
                push(cbForForeign);
                push(fGc ?
                    Kernel::Frame::Type_ToKernel :
                    Kernel::Frame::Type_ToForeign );
                push(ea_m_fp());
                mov(ea_m_fp(), esp);
            } // ToForeign frame

            if (iMin != iMax)
            {
                // Variable arity
                mov(ea_m_n(), ecx);

                for (int i = 0; i < cRegs; i++)
                {
                    mov(ea_mv_value(i), reg_arg(i));
                } // for iNth

                mov(ecx, ebp);  // argv[0] = pThread
            }
            else
            {
                // Fixed arity
                int i = iMin;

                while (i > cRegs)
                {
                    i -= 1;
                    push(ea_mv_value(i));
                } // while

                while (i > 2)
                {
                    i -= 1;
                    push(reg_arg(i));
                }

                if (iMin >= 1)
                {
                    mov(ecx, eax);  // first argument to C-function
                }
            } // if

            call(dlllink(L".", pwszProc));
            // ForForeign is deallocated by callee.

            // Pop ToForegin Frame
            {
                pop(ea_m_fp());
                add(esp, 8);    // 8=sizeof(ToForeignFrame)-4 // CF=0
            }

            switch (cVals)
            {
            case 0:     // returns no value
                mov(eax, nil);
                stc();
                break;

            case 1:     // returns one value
                break;

            case -1:    // returns unknown number of values
                for (uint i = 1; i < reg_arg_limit(); i++)
                {
                    mov(reg_arg(i), ea_mv_value(i));
                } // for i
                mov(ecx, ea_m_n());
                stc();
                break;

            default:
            {
                uint cRegs = min(static_cast<uint>(cVals), reg_arg_limit());
                for (uint i = 1; i < cRegs; i++)
                {
                    mov(reg_arg(i), ea_mv_value(i));
                } // for i
                mov(ecx, Fixnum::Encode(cVals));
                stc();
                break;
            } // default
            } // switch cVals

            ret();

            emit_epilogue(&oDefun);

            return makeFunObj(&oDefun);
        } // Run
    } oAsm;
    return oAsm.Run(fGc, name, pwszProc, iMin, iMax, cVals);
} // make_wrapper

} // Genesis
