#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - Dominance Tree
// ir/ir_dom.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_dom.cpp#6 $
//

#include "./ir_defs.h"
#include "./ir_bblock.h"
#include "./ir_function.h"
#include "./ir_fns.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// DomTreeBuilder class
//
template<class DomCFG, class DomBBlock, class DomInfo>
class DomTreeBuilder
{
    //typedef CtrlFlowGraph CFG;

    private: DomCFG*     m_pCFG;
    private: DomBBlock*  m_pEntry;
    private: DomBBlock*  m_pDFSPrev;
    private: uint        m_nDFSName;

    public: DomTreeBuilder() {}

    // Entry Point
    public: void Build(DomCFG* pCFG)
    {
        ASSERT(NULL != pCFG);

        m_pCFG = pCFG;

        m_pEntry = pCFG->GetEntry();

        m_nDFSName = 0;
        m_pDFSPrev = NULL;

        init();
        buildDfsTree(m_pEntry);
        removeUnrechable();
        computeParent();
        m_pEntry->SetParent(NULL);
        computeChildren();
        computeFrontiers();
        uninit();
    } // Build

    // addFrontier
    private: void addFrontier(DomBBlock* pFrontier, DomBBlock* pBBlock)
    {
        ASSERT(NULL != pFrontier);
        ASSERT(NULL != pBBlock);

        if (! pBBlock->GetFrontiers()->Find(pFrontier))
        {
            pBBlock->GetFrontiers()->SetFirst(
                new DomInfo::FrontierList::SLinkSite(
                    pFrontier,
                    pBBlock->GetFrontiers()->GetFirst() ) );
        }
    } // addFrontier

    // buildDfsTree
    private: void buildDfsTree(DomBBlock* pBBlock)
    {
        DomBBlock* pCurr = pBBlock->Extend<DomBBlock>();
        pCurr->SetDFSName(1);

        foreach (DomBBlock::EnumOutEdge, oEnum, pCurr)
        {
            DomBBlock* pSucc = oEnum.Get();
            if (0 == pSucc->GetDFSName())
            {
                buildDfsTree(pSucc);
            }
        } // for each succ

        m_nDFSName += 1;
        pCurr->SetDFSName(m_nDFSName);
        pCurr->SetDFSNext(m_pDFSPrev);
        m_pDFSPrev = pCurr;
    } // buildDfsTree

    // computeChildren
    //  Make children list in postorder.
    private: void computeChildren()
    {
        for (
            DomBBlock* pBBlock = m_pEntry->GetDFSNext();
            NULL != pBBlock;
            pBBlock = pBBlock->GetDFSNext() )
        {
            DomBBlock* pParent = pBBlock->GetParent();

            pParent->GetChildren()->SetFirst(
                new DomInfo::ChildList::SLinkSite(
                    pBBlock,
                    pParent->GetChildren()->GetFirst() ) );
        } // for each bblock
    } // computeChildren

    // computeFrontiers
    //  Loop over bblock which has more than one predecessors.
    private: void computeFrontiers()
    {
        foreach (DomCFG::EnumBBlock, oEnum, m_pCFG)
        {
            DomBBlock* pBBlock = oEnum.Get()->Extend<DomBBlock>();

            if (hasMoreThanOnePreds(pBBlock))
            {
                foreach (DomBBlock::EnumInEdge, oEnum, pBBlock)
                {
                    DomBBlock* pParent = pBBlock->GetParent();

                    for (
                        DomBBlock* pRunner = oEnum.Get();
                        pRunner != pParent;
                        pRunner = pRunner->GetParent() )
                    {
                        addFrontier(pBBlock, pRunner);
                    } // for each ancestor
                } // for each pred edge
            } // if
        } // for each bblock
    } // computeFrontiers

    // computeParent
    //  Computes parent (immediate dominator) for each bblock.
    private: void computeParent()
    {
        m_pEntry->SetParent(m_pEntry);
        m_pEntry->SetDepth(1);

        bool fChanged = true;
        while (fChanged)
        {
            fChanged = false;

            for (
                DomBBlock* pBBlock = m_pEntry->GetDFSNext();
                NULL != pBBlock;
                pBBlock = pBBlock->GetDFSNext() )
            {
                if (computeParentAux(pBBlock))
                {
                    fChanged = true;
                }
            } // for each bblock
        } // while
    } // computeParent

    // computeParentAux
    //  Computes new parent by processed predecessor.
    private: bool computeParentAux(DomBBlock* pCurr)
    {
        ASSERT(NULL != pCurr);

        foreach (DomBBlock::EnumInEdge, oEnum, pCurr)
        {
            DomBBlock* pParent = oEnum.Get();
                if (NULL == pParent->GetParent()) continue;

            foreach (DomBBlock::EnumInEdge, oEnum, pCurr)
            {
                DomBBlock* pPred = oEnum.Get();

                if (pParent != pPred && NULL != pPred->GetParent())
                {
                    pParent = intersect(pPred, pParent);
                }
            } // for each pred

            if (pCurr->GetParent() != pParent)
            {
                pCurr->SetParent(pParent);
                pCurr->SetDepth(pParent->GetDepth() + 1);
                return true;
            }
        } // for each parent

        return false;
    } // computeParentAux

    // hasMoreThanOnePred
    private: static bool hasMoreThanOnePreds(DomBBlock* pBBlock)
    {
        DomBBlock::EnumInEdge oEnum(pBBlock);
        if (oEnum.AtEnd()) return false;
        oEnum.Next();
        return ! oEnum.AtEnd();
    } // hasMoreThanOnePreds

    // init
    //  Allocates dominfo for all bblocks
    private: void init()
    {
        foreach (DomCFG::EnumBBlock, oEnum, m_pCFG)
        {
            DomBBlock* pBBlock = oEnum.Get()->Extend<DomBBlock>();
                pBBlock->SetDFSName(0);
                pBBlock->SetDFSNext(NULL);

            DomInfo* pDomInfo = pBBlock->GetDomInfo();
            if (NULL != pDomInfo)
            {
                pDomInfo->Reset();
            }
            else
            {
                pDomInfo = new DomInfo();
                pBBlock->SetDomInfo(pDomInfo);
            }
        } // for each bblock
    } // init

    // intersect
    private: DomBBlock* intersect(DomBBlock* pFinger1, DomBBlock* pFinger2)
    {
        ASSERT(NULL != pFinger1);
        ASSERT(NULL != pFinger2);

        while (pFinger1 != pFinger2)
        {
            while (pFinger1->GetDFSName() < pFinger2->GetDFSName())
            {
                pFinger1 = pFinger1->GetParent();
            } // while

            while (pFinger2->GetDFSName() < pFinger1->GetDFSName())
            {
                pFinger2 = pFinger2->GetParent();
            } // while
        } // while

        return pFinger1;
    } // intersect

    // remove unreachable bblocks
    private: void removeUnrechable()
    {
        DomCFG::EnumBBlock oEnum(m_pCFG);
        while (! oEnum.AtEnd())
        {
            BBlock* pBBlock = oEnum.Get();
            oEnum.Next();

            if (0 == pBBlock->Extend<DomBBlock>()->GetDFSName())
            {
                ir_remove_bblock(pBBlock);
            }
        } // for each bblock
    } // removeUnrechable

    // uninit
    private: void uninit()
    {
        foreach (DomCFG::EnumBBlock, oEnum, m_pCFG)
        {
            BBlock* pBBlock = oEnum.Get();
            pBBlock->Reset();
        } // for each bblock
    } // uninit
}; // DomTreeBuilder


template<class DomCFG, class DomBBlock, class DomInfo = BBlock::DomInfo>
class DomTreeDumper
{
    public: static void Run(DomCFG* pFun, const char16* pwsz)
    {
        html_log_format(3, L"<h3>~ADominance Tree of ~S</h3>~%",
            pwsz,
            pFun );

        log_format(3, L"<table border='1'>~%");
        log_format(3, L"<thead><tr>~%");
        log_format(3,   L"<th>Depth</th>");
        log_format(3,   L"<th>BBlock</th>");
        log_format(3,   L"<th>Parent</th>");
        log_format(3,   L"<th>Children</th>");
        log_format(3,   L"<th>Frontiers</th>");
        log_format(3, L"</tr></thead>~%");

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            DomBBlock* pBBlock = oEnum.Get()->Extend<DomBBlock>();

            html_log_format(3, L"<tr><td>~S</td>", pBBlock);

            BBlock::DomInfo* pDomInfo = pBBlock->GetDomInfo();

            html_log_format(3, L"<td>~D</td>~%", pDomInfo->GetDepth());

            if (NULL == pDomInfo->GetParent())
            {
                log_format(3, L"<td>-</td>~%");
            }
            else
            {
                html_log_format(3, L"<td>~S</td>~%",
                    pDomInfo->GetParent() );
            } // if

            // Children
            {
                log_format(3, L"<td>{");

                LPCWSTR pwsz = L"";
                foreach (BBlock::DomInfo::EnumChild, oEnum, pDomInfo)
                {
                    BBlock* pChild = oEnum.Get();

                    log_format(3, pwsz);
                    pwsz = L" ";

                    html_log_format(3, L"~S", pChild);
                } // for each child

                log_format(3, L"}</td>~%");
            } // children

            // Frontiers
            {
                log_format(3, L"<td>{");

                LPCWSTR pwsz = L"";
                foreach (BBlock::DomInfo::EnumFrontier, oEnum, pDomInfo)
                {
                    BBlock* pFrontier = oEnum.Get();

                    log_format(3, pwsz);
                    pwsz = L" ";

                    html_log_format(3, L"~S", pFrontier);
                } // for each Frontier

                log_format(3, L"}</td>~%");
            } // frontiers

            log_format(3, L"</tr>~%");
        } // for each bblock

        log_format(3, L"</table>~%");
    } // Run
}; // DomTreeDumper


//////////////////////////////////////////////////////////////////////
//
// DomBBlockBase
//
class DomBBlockBase : public BBlock
{
    protected: typedef BBlock::EnumOutEdge BBEnumOutEdge;
    protected: typedef BBlock::EnumInEdge  BBEnumInEdge;

    private: DomBBlockBase() {}

    public: DomInfo* GetDomInfo() const
        { return m_pDomInfo; }

    public: void SetDomInfo(DomInfo* pDomInfo)
        { m_pDomInfo = pDomInfo; }
}; // DomBBlockBase


//////////////////////////////////////////////////////////////////////
//
// DomBBlock_
//
template<class Base_>
class DomBBlock_ : public Base_
{
    typedef BBlock::DomInfo DomInfo;
    typedef DomBBlock_<Base_> BBlock_;

    private: DomBBlock_() {}

    public: uint GetDFSName() const
        { return static_cast<uint>(m_iWork); }

    public: BBlock_* GetDFSNext() const
        { return reinterpret_cast<BBlock_*>(m_pvWork); }

    public: void SetDFSName(uint nName)
        { m_iWork = nName; }

    public: void SetDFSNext(BBlock_* pNext)
        { m_pvWork = pNext; }

    public: class EnumInEdge : public Base_::BBEnumInEdge
    {
        public: EnumInEdge(BBlock* pBBlock) : BBEnumInEdge(pBBlock) {}

        public: BBlock_* Get() const
            { return BBEnumInEdge::GetNode()->Extend<BBlock_>(); }
    }; // EnumInEdge

    public: class EnumOutEdge : public Base_::BBEnumOutEdge
    {
        public: EnumOutEdge(BBlock* pBBlock) : BBEnumOutEdge(pBBlock) {}

        public: BBlock_* Get() const
            { return BBEnumOutEdge::GetNode()->Extend<BBlock_>(); }
    }; // EnumOutEdge

    ////////////////////////////////////////////////////////////
    //
    // DomInfo accessors shortcut
    //
    public: DomInfo::ChildList* GetChildren() const
        { return GetDomInfo()->GetChildren(); }

    public: uint GetDepth() const { return GetDomInfo()->GetDepth(); }
    public: uint SetDepth(uint n) { return GetDomInfo()->SetDepth(n); }

    public: DomInfo::FrontierList* GetFrontiers() const
        { return GetDomInfo()->GetFrontiers(); }

    public: BBlock_* GetParent() const
        { return GetDomInfo()->GetParent()->Extend<BBlock_>(); }

    public: void SetParent(BBlock_* pBBlock)
        { GetDomInfo()->SetParent(pBBlock); }
}; // BBlock_


typedef DomBBlock_<DomBBlockBase> DomBBlock;

//////////////////////////////////////////////////////////////////////
//
// DomCFG
//
class DomCFG : public Function
{
    private: DomCFG() {}

    public: DomBBlock* GetEntry() const
        { return GetEntryBB()->Extend<DomBBlock>(); }
}; // DomCFG


//////////////////////////////////////////////////////////////////////
//
// PostDomBBlockBase
//
class PostDomBBlockBase : public BBlock
{
    protected: typedef BBlock::EnumOutEdge BBEnumInEdge;
    protected: typedef BBlock::EnumInEdge  BBEnumOutEdge;

    private: PostDomBBlockBase() {}

    public: DomInfo* GetDomInfo() const
        { return m_pPostDomInfo; }

    public: void SetDomInfo(DomInfo* pDomInfo)
        { m_pPostDomInfo = pDomInfo; }
}; // PostDomBBlock


typedef DomBBlock_<PostDomBBlockBase> PostDomBBlock;

//////////////////////////////////////////////////////////////////////
//
// PostDomCFG
//
class PostDomCFG : public Function
{
    private: PostDomCFG() {}

    public: PostDomBBlock* GetEntry() const
        { return GetExitBB()->Extend<PostDomBBlock>(); }
}; // PostDomCFG


//////////////////////////////////////////////////////////////////////
//
// Compute Dominance
//
// Description:
//  Returns false if dominance tree has been computed.
//
bool ComputeDominance(Function* pFun)
{
    DomTreeBuilder<DomCFG, DomBBlock, BBlock::DomInfo> oBuilder;
        oBuilder.Build(pFun->Extend<DomCFG>());

    DomTreeDumper<DomCFG, DomBBlock, BBlock::DomInfo>::Run(
        pFun->Extend<DomCFG>(), L"" );

    return true;
} // ComputeDominance


//////////////////////////////////////////////////////////////////////
//
// Compute Post-Dominance
//
bool ComputePostDominance(Function* pFun)
{
    ir_eliminate_infinite_loop(pFun);

    DomTreeBuilder<PostDomCFG, PostDomBBlock, BBlock::DomInfo> oBuilder;
        oBuilder.Build(pFun->Extend<PostDomCFG>());

    DomTreeDumper<PostDomCFG, PostDomBBlock, BBlock::DomInfo>::Run(
        pFun->Extend<PostDomCFG>(), L"Post-" );

    return true;
} // ComputePostDominance

} // Compiler
