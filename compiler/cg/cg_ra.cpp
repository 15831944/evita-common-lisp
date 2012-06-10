#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Register Allocator
// cg/cg_ra.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_ra.cpp#4 $
//
#include "./cg_ra.h"

#include "./cg_target.h"

namespace Compiler
{

namespace
{

struct FrameExt
{
    FrameWorkList m_oChildFrames;
}; // FrameExt


//////////////////////////////////////////////////////////////////////
//
// assign_frame_offset
//
static uint
assign_frame_offset(Frame* pParent, uint ofsParent, uint nLevel = 0)
{
    nLevel += 1;

    html_log_format(2, L"[~D] offset ~D ~S~:%", nLevel, ofsParent, pParent);

    pParent->SetLocation(ofsParent);
    pParent->SetSize(cm_get_target()->ComputeFrameSize(pParent));

    FrameWorkList* pChildList = &pParent->GetExtension<FrameExt>()->
        m_oChildFrames;

    uint ofsChild = ofsParent + static_cast<uint>(pParent->GetSize());
    uint ofsMax = ofsChild;
    foreach (FrameWorkList::Enum, oEnum, pChildList)
    {
        Frame* pChild = oEnum.Get();
        uint ofs = assign_frame_offset(pChild, ofsChild, nLevel);
        if (ofsMax < ofs) ofsMax = ofs;
    } // for each child frame

    return ofsMax;
} // assign_frame_offset


//////////////////////////////////////////////////////////////////////
//
// construct_frame_tree
//
static void
construct_frame_tree(Function* pFun, Frame* pRoot)
{
    foreach (FrameList::Enum, oEnum, &pFun->m_oFrames)
    {
        Frame* pFrame = oEnum.Get();
        if (NULL == pFrame->GetDfn()) continue;

        pFrame->SetExtension(new FrameExt);

        Frame* pParent = pFrame->GetOuter();
        {
            while (NULL != pParent)
            {
                if (NULL != pParent->GetExtension<FrameExt*>())
                {
                    break;
                }
                pParent = pParent->GetOuter();
            } // while
        }

        if (NULL == pParent) pParent = pRoot;

        pParent->GetExtension<FrameExt>()->m_oChildFrames.Push(pFrame);
    } // while
} // construct_frame_tree

} // namespace

//////////////////////////////////////////////////////////////////////
//
// RegisterAllocator::AssignFrameOffset
//
void
BaseRA::AssignFrameOffset(Function* pFun)
{
    Frame oRoot(NULL, t);
        oRoot.SetExtension(new FrameExt);

    construct_frame_tree(pFun, &oRoot);

    uint ofsMax = assign_frame_offset(&oRoot, pFun->GetFrameSize());

    pFun->SetFrameSize(cm_get_target()->ComputeFrameSize(ofsMax));

    html_log_format(2, L"Total frame size is ~D byte.~:%", ofsMax);
} // BaseRA::AssignFrameOffset

} // Compiler
