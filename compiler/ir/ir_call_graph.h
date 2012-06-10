//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - IR - Call Graph
// ir/ir_call_graph.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_call_graph.h#2 $
//
#if !defined(INCLUDE_compiler_ir_call_graph_h)
#define INCLUDE_compiler_ir_call_graph_h

#include "./ir_graph.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// CgEdge - Call Graph Edge
//
class CgEdge :
    public Object,
    public GraphEdge_<Function, CgEdge>
{
    public: enum Kind
    {
        Kind_Anchor,
        Kind_Normal,
    }; // Kind

    protected: Kind m_eKind;
    public: Kind GetKind() const { return m_eKind; }
    public: Kind SetKind(Kind e) { return m_eKind = e; }

    public: CgEdge() :
        m_eKind(Kind_Anchor) {}

    public: CgEdge(
        Function*   pCaller,
        Function*   pCallee,
        Kind        eKind = Kind_Normal) :
        GraphEdge_<Function, CgEdge>(pCaller, pCallee),
        m_eKind(eKind)
    {
        ASSERT(Kind_Anchor != eKind);
    } // CgEdge

    // We don't need backward edge information.
    void SetBackward(bool) {}
}; // CgEdge

} // Compiler

#endif // !defined(INCLUDE_compiler_ir_call_graph_h)
