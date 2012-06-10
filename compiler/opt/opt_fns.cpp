#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - opt - SSA to SSI
// compiler/opt/opt_ssa2ssi.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/opt/opt_fns.cpp#2 $
//
//
// Description:
//  This file contains SSA to SSI pass.
//
#include "./opt_defs.h"

#include "../ir/ir_bblock.h"
#include "../ir/ir_instruction.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// opt_eliminate_BOUND
//
bool opt_eliminate_BOUND(
    Instruction*    pStart,
    Register*       pRindex,
    Register*       pRlength )
{
    class Process
    {
        public: static bool Run(
            Instruction*    pStart,
            Register*       pRindex,
            Register*       pRlength )
        {
            Register* pRvector = get_veclen(pRlength);
                if (NULL == pRvector) return false;

            return propagate_veclen(
                pStart,
                pRvector,
                pRindex,
                follow(pRindex) );
        } // Run

        // get_veclen - Returns register contains vector if it is argument
        // of function length.
        static Register* get_veclen(Register* pRx)
        {
            if (NULL == pRx) return NULL;

            CallInsn* pCall = pRx->GetDfn()->DynamicCast<CallInsn>();
                if (NULL == pCall) return NULL;
                if (pCall->IsNotInline()) return NULL;

            Literal* pCallee = pCall->GetSx()->DynamicCast<Literal>();
                if (NULL == pCallee) return NULL;

            unless (pCallee->GetDatum() == Qlength) return NULL;

            ValuesInsn* pArgs = pCall->GetVy()->GetDfn()->
                    DynamicCast<ValuesInsn>();
                if (NULL == pArgs) return NULL;

            return follow(pArgs->GetRx());
        } // get_veclen

        // propagate_veclen
        static bool propagate_veclen(
            Instruction*    pStart,
            Register*       pRvector,
            Register*       pRindex,
            Register*       pRindex0 )
        {
            if (NULL == pRindex) return false;

            html_log_format(3, L"<h4>propagate_veclen: ~S:~S ~S ~S ~S</h4>~%",
                pStart->GetBBlock(), pStart, pRvector, pRindex, pRindex0 );

            bool fChanged = false;

            Register::EnumUseSite oEnum(pRindex);
            while (! oEnum.AtEnd())
            {
                Instruction* pUser = oEnum.Get()->GetInstruction();
                    oEnum.Next();

                unless (ir_dominate_p(pStart, pUser)) continue;

                html_log_format(3, L"~S:~D:~S~:%",
                    pUser->GetBBlock(), pUser->GetIndex(), pUser );

                switch (pUser->GetOpcode())
                {
                case IrOp_BOUND:
                    if (follow(pUser->GetRx()) == pRvector &&
                        follow(pUser->GetRy()) == pRindex0 )
                    {
                        ir_replace_all_users(pUser->GetRy(), pUser->GetRd());
                        fChanged = true;
                    }
                    break;

                case IrOp_RUNTIMECAST:
                case IrOp_SIGMA:
                    propagate_veclen(
                        pStart,
                        pRvector,
                        pUser->GetRd(),
                        pRindex0 );
                    break;
                } // switch opcode
            } // for each user

            return fChanged;
        } // propagate_veclen

        // follow
        static Register* follow(Register* pRx)
        {
            for (;;)
            {
                if (NULL == pRx) return NULL;

                Instruction* pDfn = pRx->GetDfn();
                switch (pDfn->GetOpcode())
                {
                case IrOp_RUNTIMECAST:
                case IrOp_SIGMA:
                {
                    Register* pR2 = pDfn->GetRx();
                    if (NULL == pR2) return pRx;
                    pRx = pR2;
                    break;
                } // runtimecast
                default:
                    return pRx;
                } // switch opcode
            } // for
        } // follow
    }; // Process

    return Process::Run(pStart, pRindex, pRlength);
} // opt_eliminate_BOUND

} // Compiler
