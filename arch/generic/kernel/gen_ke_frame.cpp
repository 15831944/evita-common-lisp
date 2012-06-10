#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Generic Frame
// arch/generic/kernel/gen_ke_frame.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/generic/kernel/gen_ke_frame.cpp#4 $
//
#include "./gen_ke_frame.h"

//#include "../../../kernel/ke_thread.h"
#include "../../../mini/mini_lisp.h"

namespace Generic
{

using namespace Kernel;
using namespace MiniLisp;

namespace
{

// restore_values
static void restore_values(Thread* pThread, XferFrame* pFrame)
{
    ASSERT(NULL != pThread);
    ASSERT(NULL != pFrame);

    pThread->m_n = pFrame->m_n;

    if (cmp_xx(pThread->m_n, lengthof(pFrame->mv_value)) <= 0)
    {
        for (Int i = 0; i < Fixnum::Decode_(pThread->m_n); i++)
        {
            pThread->mv_value[i] = pFrame->mv_value[i];
        } // for i
    }
    else
    {
        Int i = 0;
        foreach (EnumList, oEnum, pFrame->mv_value[0])
        {
            pThread->mv_value[i] = oEnum.Get();
                i += 1;
        } // for list
    } // if
} // retore_values

// save_values
static void save_values(Thread* pThread, XferFrame* pFrame)
{
    ASSERT(NULL != pThread);
    ASSERT(NULL != pFrame);

    pFrame->m_n = pThread->m_n;

    if (cmp_xx(pThread->m_n, lengthof(pFrame->mv_value)) <= 0)
    {
        for (Int i = 0; i < Fixnum::Decode_(pThread->m_n); i++)
        {
            pFrame->mv_value[i] = pThread->mv_value[i];
        } // for i
    }
    else
    {
        Val vals = list(pThread->mv_value[0]);
        Val last = vals;
        for (Int i = 1; i < Fixnum::Decode_(pThread->m_n); i++)
        {
            last = setf_cdr(list(pThread->mv_value[i]), last);
        } // for i
        pFrame->mv_value[0] = vals;
    }
} // save_values

} // namespace

//////////////////////////////////////////////////////////////////////
//
// C_go
//
void __declspec(noreturn) __fastcall
C_go(Val tag)
{
    TagbodyFrame::Tag* pTag = tag->StaticCast<TagbodyFrame::Tag>();
    Thread* pThread = Thread::Get();

    TagbodyFrame* pFrame = NULL;
    foreach (Thread::EnumFrame, oEnum, pThread)
    {
        Frame* pRunner = oEnum.Get();

        if (pRunner->Is<TagbodyFrame>())
        {
            if (pRunner->StaticCast<TagbodyFrame>()->HasTag(pTag))
            {
                pFrame = pRunner->StaticCast<TagbodyFrame>();
                break;
            }
        }
    } // for each frame

    if (NULL == pFrame)
    {
        error(L"Tagbody has gone.");
    }

    if (pFrame->IsAbandoned())
    {
        error(L"Can't transfer control to abandoned exit point.");
    }

    // [1] Intervening exit points are "abandoned" (i.e., their extent ends
    // and it is no longer valid to attempt to transfer control through them).
    foreach (Thread::EnumFrame, oEnum, pThread)
    {
        Frame* pRunner = oEnum.Get();

        if (pRunner == pFrame)
        {
            break;
        }

        switch (pRunner->GetType())
        {
        case Frame::Type_Block:
            pRunner->StaticCast<BlockFrame>()->MakeAbandoned();
            break;

        case Frame::Type_Catch:
            pRunner->StaticCast<CatchFrame>()->MakeAbandoned();
            break;

        case Frame::Type_Tagbody:
            pRunner->StaticCast<TagbodyFrame>()->MakeAbandoned();
            break;
        } // switch frame type
    } // for each frame

    // [2] The cleanup clauses of any intervening unwind-protect clauses are
    // evaluated. 
    // [3] Intervening dynamic bindings of special variables, Block tags,
    // condition handlers, and restarts are undone. 
    foreach (Thread::EnumFrame, oEnum, pThread)
    {
        Frame* pRunner = oEnum.Get();

        if (pRunner == pFrame)
        {
            pThread->SetFP(pFrame);
            pFrame->Transfer(pThread, pTag);
            // NOTREACHED
        }

        switch (pRunner->GetType())
        {
        case Frame::Type_Bind:
            pRunner->StaticCast<BindFrame>()->Unwind(pThread);
            break;

        case Frame::Type_Finally:
            pThread->SetFP(pFrame->m_pOuter);
            pRunner->StaticCast<FinallyFrame>()->Unwind(pThread);
            break;
        } // switch frame type
    } // for each frame
} // C_go


//////////////////////////////////////////////////////////////////////
//
// C_return_from
//
void __declspec(noreturn) __fastcall
C_return_from(Thread* pThread)
{
    ASSERT(NULL != pThread);

    BlockFrame* pFrame  = NULL;
    Frame* pFinally = NULL;

    BlockFrame* pTarget = pThread->m_fn->StaticCast<BlockFrame>();

    foreach (Thread::EnumFrame, oEnum, pThread)
    {
        Frame* pRunner = oEnum.Get();

        if (pRunner == pTarget)
        {
            pFrame = pRunner->StaticCast<BlockFrame>();
            break;
        }

        if (NULL == pFinally)
        {
            if (pRunner->GetType() == Frame::Type_Finally)
            {
                pFinally = pRunner;
            }
        }
    } // for each frame

    if (NULL == pFrame)
    {
        // si::Block-tag-not-seen
        error(L"There is no such block ~S.", pTarget->m_name);
    }

    if (pFrame->IsAbandoned())
    {
        error(L"Can't transfer control to abandoned exit point.");
    }

    if (NULL != pFinally)
    {
        save_values(pThread, pFrame);
    }

    // [1] Intervening exit points are "abandoned" (i.e., their extent ends
    // and it is no longer valid to attempt to transfer control through them).
    foreach (Thread::EnumFrame, oEnum, pThread)
    {
        Frame* pRunner = oEnum.Get();

        if (pRunner == pFrame)
        {
            break;
        }

        switch (pRunner->GetType())
        {
        case Frame::Type_Block:
            pRunner->StaticCast<BlockFrame>()->MakeAbandoned();
            break;

        case Frame::Type_Catch:
            pRunner->StaticCast<CatchFrame>()->MakeAbandoned();
            break;

        case Frame::Type_Tagbody:
            pRunner->StaticCast<TagbodyFrame>()->MakeAbandoned();
            break;
        } // switch frame type
    } // for each frame

    // [2] The cleanup clauses of any intervening unwind-protect clauses are
    // evaluated. 
    // [3] Intervening dynamic bindings of special variables, Block tags,
    // condition handlers, and restarts are undone. 
    foreach (Thread::EnumFrame, oEnum, pThread)
    {
        Frame* pRunner = oEnum.Get();

        if (pRunner == pFrame)
        {
            if (NULL != pFinally)
            {
                restore_values(pThread, pFrame);
            }

            // Exit point sets FP.
            pFrame->Transfer(pThread);
            // NOTREACHED
        }

        switch (pRunner->GetType())
        {
        case Frame::Type_Bind:
            pRunner->StaticCast<BindFrame>()->Unwind(pThread);
            break;

        case Frame::Type_Finally:
            pThread->SetFP(pFrame->m_pOuter);
            pRunner->StaticCast<FinallyFrame>()->Unwind(pThread);
            break;
        } // switch frame type
    } // for each frame
} // C_return_from


//////////////////////////////////////////////////////////////////////
//
// C_throw
//
void __declspec(noreturn) __fastcall
C_throw(Thread* pThread)
{
    ASSERT(NULL != pThread);

    CatchFrame* pFrame = NULL;
    Frame* pFinally = NULL;

    Val tag = pThread->m_fn;

    foreach (Thread::EnumFrame, oEnum, pThread)
    {
        Frame* pRunner = oEnum.Get();

        if (pRunner->GetType() == Frame::Type_Catch &&
            pRunner->StaticCast<CatchFrame>()->m_name == tag )
        {
            pFrame = pRunner->StaticCast<CatchFrame>();
            break;
        }

        if (NULL == pFinally)
        {
            if (pRunner->GetType() == Frame::Type_Finally)
            {
                pFinally = pRunner;
            }
        }
    } // for each frame

    if (NULL == pFrame)
    {
        // si::catch-tag-not-seen
        error(L"There is no catch tag ~S.", tag);
    }

    if (pFrame->IsAbandoned())
    {
        error(L"Can't transfer control to abandoned exit point.");
    }

    if (NULL != pFinally)
    {
        save_values(pThread, pFrame);
    }

    // [1] Intervening exit points are "abandoned" (i.e., their extent ends
    // and it is no longer valid to attempt to transfer control through them).
    foreach (Thread::EnumFrame, oEnum, pThread)
    {
        Frame* pRunner = oEnum.Get();

        if (pRunner == pFrame)
        {
            break;
        }

        switch (pRunner->GetType())
        {
        case Frame::Type_Block:
            pRunner->StaticCast<BlockFrame>()->MakeAbandoned();
            break;

        case Frame::Type_Catch:
            pRunner->StaticCast<CatchFrame>()->MakeAbandoned();
            break;

        case Frame::Type_Tagbody:
            pRunner->StaticCast<TagbodyFrame>()->MakeAbandoned();
            break;
        } // switch frame type
    } // for each frame

    // [2] The cleanup clauses of any intervening unwind-protect clauses are
    // evaluated. 
    // [3] Intervening dynamic bindings of special variables, catch tags,
    // condition handlers, and restarts are undone. 
    foreach (Thread::EnumFrame, oEnum, pThread)
    {
        Frame* pRunner = oEnum.Get();

        if (pRunner == pFrame)
        {
            if (NULL != pFinally)
            {
                restore_values(pThread, pFrame);
            }

            // Exit point sets FP.
            pFrame->Transfer(pThread);
            // NOTREACHED
        }

        switch (pRunner->GetType())
        {
        case Frame::Type_Bind:
            pRunner->StaticCast<BindFrame>()->Unwind(pThread);
            break;

        case Frame::Type_Finally:
            pThread->SetFP(pFrame->m_pOuter);
            pRunner->StaticCast<FinallyFrame>()->Unwind(pThread);
            break;
        } // switch frame type
    } // for each frame
} // C_throw

} // Generic
