#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - Graph
// ir/ir_graph.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_graph.cpp#5 $
//

#include "./ir_graph.h"

#include "./ir_function.h"
#include "./ir_fns.h"

#include "../cm/cm_module.h"

namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// DFSWaler_
//
// BUGBUG: Should we remove unreachable bblocks?
//
//  Description:
//   Sorts basic blocks by toplogical-sort or depth-first-search (DFS).
//   This function assigns preorder-number and postorder-number to bblocks.
// 
//  See [Robert98], p.68
//
//  References:
//  [Robert98] Robert Morgan,
//  "Building an Optimizing Compiler", Digital Press, 1998
//
template<class Parent_, class Node_>
class DFSWalker_
{
    typedef Graph_<Parent_, Node_> Graph;
    typedef LayoutSite_<Parent_, Node_> LayoutSite;
    typedef PreorderSite_<Node_>        PreorderSite;
    typedef PostorderSite_<Node_>       PostorderSite;

    Graph*  m_pGraph;
    Node_*  m_pPreorder;
    Node_*  m_pPostorder;
    UINT    m_nPreorder;
    UINT    m_nPostorder;

    // DFSWalker_
    public: DFSWalker_(Graph* pGraph) :
        m_pGraph(pGraph),
        m_nPreorder(0),
        m_nPostorder(0)
    {
        ASSERT(NULL != pGraph);

        m_pPreorder  = static_cast<Graph::Preorder*>(m_pGraph)->GetAnchor();
        m_pPostorder = static_cast<Graph::Postorder*>(m_pGraph)->GetAnchor();

        prepare();
        walk(static_cast<Graph::Layout*>(m_pGraph)->GetHead());
        settle();
    } // DFSWalker_

    // prepare
    void prepare()
    {
        foreach (Graph::Layout::Enum, oEnum, m_pGraph)
        {
            oEnum.Get()->SetPreorder(0);
            oEnum.Get()->SetPostorder(0);   // for detecing back edge
        } // for each node
    } // prepare

    // settle
    void settle()
    {
        {
            Node_* pAnchor =
                static_cast<Graph::Preorder*>(m_pGraph)->GetAnchor();

            Node_* pLast = m_pPreorder;

            static_cast<PreorderSite*>(pLast)->SetNext_(pAnchor);
            static_cast<PreorderSite*>(pAnchor)->SetPrev_(pLast);
        }

        {
            Node_* pAnchor =
                static_cast<Graph::Postorder*>(m_pGraph)->GetAnchor();

            Node_* pLast = m_pPostorder;

            static_cast<PostorderSite*>(pLast)->SetNext_(pAnchor);
            static_cast<PostorderSite*>(pAnchor)->SetPrev_(pLast);
        }
    } // settle

    // visited_p
    bool visited_p(Node_* pNode)
        { return 0 != pNode->GetPreorder(); }

    // walk
    void walk(Node_* pCurr)
    {
        ASSERT(NULL != pCurr);
        ASSERT(! visited_p(pCurr));

        m_nPreorder += 1;
        pCurr->SetPreorder(m_nPreorder);

        // Link preorder list
        static_cast<PreorderSite*>(pCurr)->SetPrev_(m_pPreorder);
        static_cast<PreorderSite*>(m_pPreorder)->SetNext_(pCurr);
        m_pPreorder = pCurr;

        // for successors
        foreach (Node_::EnumOutEdge, oEnum, pCurr)
        {
            Node_::Edge* pEdge = oEnum.Get();
            Node_* pSucc = pEdge->GetTo();
            if (! visited_p(pSucc))
            {
                walk(pSucc);
            }
            else
            {
                // If pSucc is on stack, the edge is backward edge.
                //
                // succ.postorder == 0              backward
                // curr.preorder < succ.preorder    forward
                // otherwise                        cross
                pEdge->SetBackward(0 == pSucc->GetPostorder());
            }
        } // for each succ

        m_nPostorder += 1;
        pCurr->SetPostorder(m_nPostorder);

        static_cast<PostorderSite*>(pCurr)->SetPrev_(m_pPostorder);
        static_cast<PostorderSite*>(m_pPostorder)->SetNext_(pCurr);
        m_pPostorder = pCurr;
    } // walk
}; // DFSWalker_

} // namespace

//////////////////////////////////////////////////////////////////////
//
// PrepareTraversal
//
template<class Parent_, class Node_> bool
Graph_<Parent_, Node_>::PrepareTraversal()
{
    if (! IsChanged())
    {
        return false;
    }

    DFSWalker_<Parent_, Node_> oWalker(this);

    return true;
} // Graph_<Parent_, Node_>::PrepareTraversal


//////////////////////////////////////////////////////////////////////
//
// SccWalker
//
template<class Parent_, class Node_>
class SccWalker_
{
    typedef Graph_<Parent_, Node_> Graph;

    uint    m_nSccNum;
    Node_*  m_pStackTop;

    // SccWalker
    public: SccWalker_(Graph* pGraph) :
        m_nSccNum(0),
        m_pStackTop(NULL)
    {
        foreach (Graph::Layout::Enum, oEnum, pGraph)
        {
            oEnum.Get()->ResetScc();
        } // for each node

        dfs(static_cast<Graph::Layout*>(pGraph)->GetHead());
    } // SccWalker

    // dfs
    uint dfs(Node_* pCurr)
    {
        ASSERT(0 == pCurr->m_nSccNum);

        m_nSccNum += 1;
        pCurr->m_nSccNum  = m_nSccNum;
        push(pCurr);

        uint nMin = m_nSccNum;
        foreach (Node_::EnumOutEdge, oEnum, pCurr)
        {
            Node_* pSucc = oEnum.GetNode();

            uint nNum = pSucc->m_nSccNum;
            if (0 == nNum)
            {
                nNum = dfs(pSucc);
            }

            nMin = min(nMin, nNum);
        } // for each succ

        if (pCurr->m_nSccNum == nMin)
        {
            Node_* pSccNext = NULL;
            Node_* pSccId = pCurr;
            for (;;)
            {
                Node_* pTop = pop();

                pTop->m_nSccNum  = static_cast<uint>(-1);
                pTop->m_pSccId   = pSccId;
                pTop->m_pSccNext = pSccNext;

                pSccNext = pTop;

                if (pTop == pCurr) break;
            } // for
        } // if

        return nMin;
    } // dfs

    // push
    void push(Node_* p)
    { 
        p->m_pSccNext = m_pStackTop; 
        m_pStackTop = p;
    } // push

    // pop
    Node_* pop()
    {
        Node_* p = m_pStackTop;
        m_pStackTop = m_pStackTop->m_pSccNext;
        return p;
    } // pop
}; // SccWalker


//////////////////////////////////////////////////////////////////////
//
// PrepareTraversal
//
template<class Parent_, class Node_> bool
Graph_<Parent_, Node_>::PrepareSccTraversal()
{
    if (HasSccInfo())
    {
        return false;
    }

    SccWalker_<Parent_, Node_> oWalker(this);

    return true;
} // Graph_<Parent_, Node_>::PrepareSccTraversal


bool Function::PrepareTraversal() { return Graph::PrepareTraversal(); }
bool Module::PrepareTraversal() { return Graph::PrepareTraversal(); }
bool Module::PrepareSccTraversal() { return Graph::PrepareSccTraversal(); }

} // Compiler
