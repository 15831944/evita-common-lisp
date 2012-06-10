#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - x64 - Utility Function
// arch/x64/genesis/x64_gs_fns.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/genesis/x64_gs_fns.cpp#6 $
//
#include "../x64_arch.h"
#include "../x64_asm.h"

#include "../kernel/x64_ke_thread.h"

namespace Genesis
{

using namespace X64;

//////////////////////////////////////////////////////////////////////
//
//  Make C-function wrappering function object.
//
//              +---------------------+
//      rsp-8   |   RA of wrapper     |<----------------+
//              +---------------------+                 |
//      rsp+0   |   home of arg[0]    |     rcx         |
//              +---------------------+                 |
//      rsp+8   |   home of arg[1]    |     rdx         |
//              +---------------------+                 |
//      rsp+16  |   home of arg[2]    |     r8          |
//              +---------------------+                 |
//      rsp+24  |   home of arg[3]    |     r9          |
//              +---------------------+                 |
//                       ...                            |
//              +---------------------+                 |
//              |      $rtcb.m_fp     |<-- $rtcb.m_fp   |
//              +---------------------+                 |
//              |      ToForeign      |                 |
//              +---------------------+                 |
//              |    sizeof args    o-+-----------------+
//              +---------------------+
//              | RA of lisp function | <-- rsp at entry
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
    class Asm : X64Assembler
    {
        public: Val Run(
            int fGc,
            Val name, const char16* pwszProc, int iMin, int iMax,
            int cVals )
        {
            if (-1 == iMax)
            {
                iMax = X64Mach::Lambda_Parameters_Limit;
            }

            ASSERT(iMin <= iMax);

            uint cbForForeign;

            if (iMin == iMax)
            {
                cbForForeign = max(iMax, 4) * 8;
            }
            else
            {
                cbForForeign = 8 * 4;
            }

            if (cbForForeign % 16) cbForForeign += 8;

            uint cbFrame =  cbForForeign;
                 cbFrame += sizeof(ToForeignFrame);
                ASSERT(24 == sizeof(ToForeignFrame));

            Defun oDefun(this, name, iMin, iMax, cbFrame + 8);  // +8 for RA

            emit_prologue(&oDefun);

            // Make ToForeign Frame
            {
                push(cbForForeign);                     // m_cbArgs
                push(fGc ?                              // m_eType
                    Kernel::Frame::Type_ToKernel :
                    Kernel::Frame::Type_ToForeign );
                push(ea_m_fp());                        // m_pOuter
                mov(ea_m_fp(), rsp);
            }

            sub(rsp, cbFrame - sizeof(ToForeignFrame));

            uint cRegs = min(static_cast<uint>(iMax), reg_arg_limit());

            if (iMin != iMax)
            {
                // Variable arity
                mov(ea_m_n(), $rn);

                for (uint i = 0; i < cRegs; i++)
                {
                    mov(ea_mv_value(i), reg_arg(i));
                } // for iNth

                mov(rcx, $rtcb);     // the first C/C++ argument
            }
            else
            {
                // Fixed arity

                // fifth arguments to C-function
                for (uint i = 4; i < cRegs; i++)
                {
                    mov(ea(rsp, i * 8), reg_arg(i));
                } // for i

                for (int i = cRegs; i < iMin; i++)
                {
                    mov(rcx, ea_mv_value(i));
                    mov(ea(rsp, i * 8), rcx);
                } // i

                if (iMin >= 4) mov(r9,  $r3);  // arg[3]
                if (iMin >= 3) mov(r8,  $r2);  // arg[2]
                if (iMin >= 2) mov(rdx, $r1);  // arg[1]
                if (iMin >= 1) mov(rcx, $r0);  // arg[0]
            } // if

            call(dlllink(L".", pwszProc));

            // Pop ToForegin Frame
            {
                mov(rcx, ea(rsp, cbForForeign));
                mov(ea_m_fp(), rcx);
            }

            add(rsp, cbFrame);   // CF=0

            switch (cVals)
            {
            case 0:     // returns no value
                mov($r0, $rnil);
                stc();
                break;

            case 1:     // returns one value
                break;

            case -1:    // returns unknown number of values
                for (uint i = 1; i < reg_arg_limit(); i++)
                    { mov(reg_arg(i), ea_mv_value(i)); }
                mov($rn, ea_m_n());
                stc();
                break;

            default:
            {
                uint iRegs = min(static_cast<uint>(cVals), reg_arg_limit());
                for (uint i = 1; i < iRegs; i++)
                    { mov(reg_arg(i), ea_mv_value(i)); }
                mov($rn, Fixnum::Encode(cVals));
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
