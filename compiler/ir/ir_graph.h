//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - Graph
// ir/ir_graph.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_graph.h#8 $
//
#if !defined(INCLUDE_compiler_ir_graph_h)
#define INCLUDE_compiler_ir_graph_h

#include "../cm/cm_base.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Layout Site
//
template<class Parent_, class Node_>
class LayoutSite_    :
    public DLinkSite_<LayoutSite_<Parent_, Node_>, Node_>
{
    public: typedef LayoutSite_<Parent_, Node_> LayoutSite;

    protected: Parent_* m_pParent;

    public: LayoutSite_() : m_pParent(NULL) {}

    public: Node_* GetNext() const { return GetNext_(); }
    public: Node_* GetPrev() const { return GetPrev_(); }

    public: Parent_* GetParent() const { return m_pParent; }
    public: void SetParent(Parent_* pParent) { m_pParent = pParent; }
}; // LayoutSite


//////////////////////////////////////////////////////////////////////
//
// PreorderSite_
//
template<class Node_> class PreorderSite_    :
    public DLinkSite_<PreorderSite_<Node_>, Node_>
{
    protected: typedef PreorderSite_<Node_> PreorderSite;
    protected: uint m_nPreorder;
    protected: PreorderSite() : m_nPreorder(0) {}
    public: uint GetPreorder() const { return m_nPreorder; }
    public: uint SetPreorder(uint n) { return m_nPreorder = n; }
}; // PreorderSite_


//////////////////////////////////////////////////////////////////////
//
// PostorderSite_
//
template<class Node_> class PostorderSite_    :
    public DLinkSite_<PostorderSite_<Node_>, Node_>
{
    protected: typedef PostorderSite_<Node_> PostorderSite;
    protected: uint m_nPostorder;
    protected: PostorderSite() : m_nPostorder(0) {}
    public: uint GetPostorder() const { return m_nPostorder; }
    public: uint SetPostorder(uint n) { return m_nPostorder = n; }
}; // PostorderSite_


//////////////////////////////////////////////////////////////////////
//
// LayoutAnchor_
//
template<class Parent_, class Node_>
class LayoutAnchor_ :
    public DLinkAnchor_<LayoutSite_<Parent_, Node_>, Node_> {};


//////////////////////////////////////////////////////////////////////
//
// PreorderAnchor_
//
template<class Node_>
class PreorderAnchor_ :
    public DLinkAnchor_<PreorderSite_<Node_>, Node_>
{
    // nothing
}; // PreorderAnchor_


//////////////////////////////////////////////////////////////////////
//
// PostorderAnchor_
//
template<class Node_>
class PostorderAnchor_ :
    public DLinkAnchor_<PostorderSite_<Node_>, Node_>
{
    // nothing
}; // PostorderAnchor_


//////////////////////////////////////////////////////////////////////
//
// SccComponent_
//
template<class Node_>
class SccComponent_
{
    public: Node_*  m_pSccId;
    public: Node_*  m_pSccNext;
    public: uint    m_nSccNum;

    public: SccComponent_() { ResetScc(); }

    public: void ResetScc(Node_* pSccId = NULL)
    {
        m_pSccId   = pSccId;
        m_pSccNext = NULL;
        m_nSccNum  = 0;
    } // ResetScc

    public: class EnumScc
    {
        protected: Node_* m_pRunner;

        public: EnumScc(Node_* p) :
            m_pRunner(p) {}

        public: bool AtEnd() const
            { return NULL == m_pRunner; }

        public: Node_* Get() const
            { ASSERT(! AtEnd()); return m_pRunner; }

        public: void Next()
            { ASSERT(! AtEnd());
              m_pRunner = m_pRunner->GetSccInfo()->m_pSccNext; }
    }; // EnumScc
}; // SccComponent_


//////////////////////////////////////////////////////////////////////
//
// DomInfo
//
template<class Graph_, class Node_>
class DomInfo_ : public Atom
{
    public: typedef DomInfo_<Graph_, Node_>  DomInfo;
    public: typedef SLinkAnchor_<Node_> ChildList;
    public: typedef SLinkAnchor_<Node_> FrontierList;

    protected: Node_*       m_pParent;
    protected: ChildList    m_oChildren;
    protected: FrontierList m_oFrontiers;
    protected: uint         m_nDepth;

    public: DomInfo_() :
        m_pParent(NULL), m_nDepth(0) {}

    public: ChildList* GetChildren() const
        { return const_cast<ChildList*>(&m_oChildren); }

    public: FrontierList* GetFrontiers() const
        { return const_cast<FrontierList*>(&m_oFrontiers); }

    public: Node_* GetParent() const
        { return m_pParent; }

    public: uint GetDepth()      const { return m_nDepth; }
    public: uint SetDepth(uint nDepth) { return m_nDepth = nDepth; }

    public: void Reset()
    {
        m_pParent = NULL;
        m_oChildren.RemoveAll();
        m_oFrontiers.RemoveAll();
    } // Reset

    public: void SetParent(Node_* pParent)
        { m_pParent = pParent; }

    public: class EnumChild : public ChildList::Enum
    {
        public: EnumChild(DomInfo* p) :
            ChildList::Enum(&p->m_oChildren) {}
    }; // EnumChild

    public: class EnumFrontier : public FrontierList::Enum
    {
        public: EnumFrontier(DomInfo* p) :
            FrontierList::Enum(&p->m_oFrontiers) {}
    }; // EnumFrontier
}; // DomInfo_


//////////////////////////////////////////////////////////////////////
//
// Graph_
//
template<class Parent_, class Node_>
class Graph_ :
    public LayoutAnchor_<Parent_, Node_>,
    public PreorderAnchor_<Node_>,
    public PostorderAnchor_<Node_>
{
    public: typedef Graph_<Parent_, Node_> Graph;
    public: typedef LayoutAnchor_<Parent_, Node_> Layout;
    public: typedef PreorderAnchor_<Node_> Preorder;
    public: typedef PostorderAnchor_<Node_> Postorder;

    // Append
    public: Node_* Append(Node_* pNode)
    {
        pNode->SetParent(static_cast<Parent_*>(this));
        static_cast<Layout*>(this)->Append_(pNode);
        SetChanged();
        return pNode;
    } // Append

    // Insert
    public: Node_* Insert(Node_* pNode, Node_* pRef)
    {
        SetChanged();
        pNode->SetParent(static_cast<Parent_*>(this));
        return static_cast<Layout*>(this)->Insert_(pNode, pRef);
    } // Insert

    // IsChanged
    public: bool IsChanged()
        { return 0 == static_cast<Layout*>(this)->GetHead()->GetPreorder(); }

    // PrepareTraversal
    public: bool PrepareTraversal();
    public: void PrepareEnumLayout() {}
    public: void PrepareEnumPreorder() { PrepareTraversal(); }
    public: void PrepareEnumPostorder() { PrepareTraversal(); }

    // Remove
    public: Node_* Remove(Node_* pNode)
    {
        SetChanged();
        pNode->SetParent(NULL);
        return static_cast<Layout*>(this)->Remove_(pNode);
    } // Remove

    // SetChanged
    public: void SetChanged()
    {
        Node_* p = static_cast<Layout*>(this)->GetHead();
        p->SetPreorder(0);
        p->m_pSccId = NULL;
    } // SetChanged

    // SCC
    public: bool HasSccInfo()
        { return NULL != static_cast<Layout*>(this)->GetHead()->m_pSccId; }

    public: bool PrepareSccTraversal();
}; // Graph_


//////////////////////////////////////////////////////////////////////
//
// Edge Sites
//
template<class Edge_> class EdgeInSite_  :
    public DLinkSite_<EdgeInSite_<Edge_>, Edge_> {};

template<class Edge_> class EdgeOutSite_  :
    public DLinkSite_<EdgeOutSite_<Edge_>, Edge_> {};

template<class Edge_>
class EdgeInAnchor_  :
    public DLinkAnchor_<EdgeInSite_<Edge_>, Edge_> {};

template<class Edge_>
class EdgeOutAnchor_  :
    public DLinkAnchor_<EdgeOutSite_<Edge_>, Edge_> {};


//////////////////////////////////////////////////////////////////////
//
// GraphEdge_
//
template<class Node_, class Edge_>
class GraphEdge_ :
    public EdgeInSite_<Edge_>,
    public EdgeOutSite_<Edge_>
{
    public: typedef EdgeInSite_<Edge_>  EdgeInSite;
    public: typedef EdgeOutSite_<Edge_> EdgeOutSite;

    protected: Node_*  m_pFrom;
    protected: Node_*  m_pTo;

    public: GraphEdge_() :
        m_pFrom(NULL), m_pTo(NULL) {}

    public: GraphEdge_(Node_* pFrom, Node_* pTo) :
        m_pFrom(pFrom), m_pTo(pTo) {}

    public: Node_* GetFrom() const   { return m_pFrom; }
    public: Node_* SetFrom(Node_* p) { return m_pFrom = p; }

    public: Node_* GetTo()   const { return m_pTo; }
    public: Node_* SetTo(Node_* p) { return m_pTo = p; }
}; // GraphEdge_


//////////////////////////////////////////////////////////////////////
//
// GraphNode_ template class
//
//  API:
//      EdgeInAnchor
//      EdgeOutAnchor
//      EnumInEdge
//      EnumOutEdge
//
template<class Parent_, class Node_, class Edge_>
class GraphNode_ :
    public LayoutSite_<Parent_, Node_>,
    public PreorderSite_<Node_>,
    public PostorderSite_<Node_>,
    public SccComponent_<Node_>,
    public EdgeInAnchor_<Edge_>,
    public EdgeOutAnchor_<Edge_>
{
    protected: typedef GraphNode_<Parent_, Node_, Edge_> GraphNode;
    
    public: typedef SccComponent_<Node_> SccInfo;
    public: typedef EdgeInAnchor_<Edge_> EdgeInAnchor;
    public: typedef EdgeOutAnchor_<Edge_> EdgeOutAnchor;
    public: typedef Edge_ Edge;

    public: SccInfo* GetSccInfo() { return static_cast<SccInfo*>(this); }

    public: uint CountInEdge() const
    {
        uint nCount = 0;
        foreach (EnumInEdge, oEnum, this) { nCount += 1; }
        return nCount;
    } // CountInEdge

    // EnumInEdge
    public: class EnumInEdge : public EdgeInAnchor::Enum
    {
        public: EnumInEdge(GraphNode_* pNode) :
            EdgeInAnchor::Enum(
                static_cast<EdgeInAnchor*>(pNode) ) {}

        public: EnumInEdge(const GraphNode* pNode) :
            EdgeInAnchor::Enum(
                static_cast<EdgeInAnchor*>(const_cast<GraphNode*>(pNode)) ) {}

        public: Node_* GetNode() const { return Get()->GetFrom(); }
    }; // EnumInEdge

    // EnumOutEdge
    class EnumOutEdge : public EdgeOutAnchor::Enum
    {
        public: EnumOutEdge(GraphNode* pNode) :
            EdgeOutAnchor::Enum(
                static_cast<EdgeOutAnchor*>(pNode) ) {}

        public: EnumOutEdge(const GraphNode* pNode) :
            EdgeOutAnchor::Enum(
                static_cast<EdgeOutAnchor*>(const_cast<GraphNode*>(pNode)) ) {}

        public: Node_* GetNode() const { return Get()->GetTo(); }
    }; // EnumOutEdge
}; // GraphNode_

} // Compiler

#endif // !defined(INCLUDE_compiler_ir_graph_h)
