//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - IR - Function
// ir/ir_function.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_cfg.h#3 $
//
#if !defined(INCLUDE_compiler_ir_cfg_h)
#define INCLUDE_compiler_ir_cfg_h

#include "./ir_graph.h"

namespace Compiler
{
//////////////////////////////////////////////////////////////////////
//
// Control-Flow Graph Edge
//
class CfgEdge :
    public Object,
    public GraphEdge_<BBlock, CfgEdge>,
    public WithWorkArea
{
    public: enum Kind
    {
        Kind_Anchor,
        Kind_Pseudo,
        Kind_Normal,
        Kind_Nonlocal,
        Kind_Exit,

        Kind_MAX_1,
    }; // Kind

    protected: Kind m_eKind;
        public: Kind GetKind() const { return m_eKind; }
        public: Kind SetKind(Kind e) { return m_eKind = e; }
        public: bool IsNonlocal() const { return Kind_Nonlocal == m_eKind; }
        public: bool IsPseudo()   const { return Kind_Pseudo   == m_eKind; }

    protected: bool m_fBackward;
        public: bool IsBackward() const  { return m_fBackward; }
        public: bool SetBackward(bool f) { return m_fBackward = f; }

    public: CfgEdge() :
        m_eKind(Kind_Anchor), m_fBackward(false) {}

    public: CfgEdge(BBlock* pFrom, BBlock* pTo, Kind eKind = Kind_Normal) :
        GraphEdge_<BBlock, CfgEdge>(pFrom, pTo),
        m_eKind(eKind), m_fBackward(false)
    {
        ASSERT(Kind_Anchor != eKind);
    } // CfgEdge

    ////////////////////////////////////////////////////////////
    //
    // Printer
    //
    virtual void HtmlPrint(Val, bool) const;
}; // CfgEdge

} // Compiler

#endif // !defined(INCLUDE_compiler_ir_cfg_h)
