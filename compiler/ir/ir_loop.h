//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - Data Flow Analysis
// ir/ir_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_loop.h#1 $
//
#if !defined(INCLUDE_compiler_ir_loop_h)
#define INCLUDE_compiler_ir_loop_h

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Loop Tree Node
//
struct LoopInfo :
    public Object,
    public WorkListItem_<LoopInfo>
{
    enum Kind
    {
        Kind_Root,
        Kind_Single,
        Kind_Multiple,
        Kind_Leaf,
    }; // Kind

    LoopInfo*   m_pParent;
    LoopInfo*   m_pChild;
    LoopInfo*   m_pSibling;
    LoopInfo*   m_pGenerator;
    BBlock*     m_pEntry;
    BBlock*     m_pBBlock;
    uint        m_nDepth;
    Kind        m_eKind;

    void AddBody(LoopInfo* pNode)
    {
        ASSERT(NULL == pNode->m_pParent);
        uint nDepth = m_nDepth + 1;
        pNode->m_nDepth   = nDepth;
        pNode->m_pParent  = this;
        pNode->m_pSibling = m_pChild;
        m_pChild = pNode;

        if (Kind_Leaf == pNode->m_eKind) return;

        nDepth += 1;
        pNode->m_nDepth = nDepth;
        foreach (EnumChild, oEnum, pNode)
        {
            oEnum.Get()->m_nDepth = nDepth;
        } // for each body
    } // AddBody

    LoopInfo(BBlock* pBBlock) :
        m_nDepth(0),
        m_eKind(Kind_Leaf),
        m_pBBlock(pBBlock),
        m_pChild(NULL),
        m_pParent(NULL),
        m_pGenerator(NULL),
        m_pSibling(NULL),
        m_pEntry(NULL) {}

    // EnumGenerator
    class EnumGenerator
    {
        LoopInfo* m_pRunner;

        public: EnumGenerator(LoopInfo* pNode) :
            m_pRunner(pNode->m_pGenerator) {}

        public: bool AtEnd() const
            { return NULL == m_pRunner; }

        public: LoopInfo* Get() const
            { ASSERT(! AtEnd()); return m_pRunner; }

        public: void Next()
            { ASSERT(! AtEnd()); m_pRunner = m_pRunner->m_pGenerator; }
    }; // EnumGenerator

    // EnumChild
    class EnumChild
    {
        LoopInfo*   m_pRunner;

        public: EnumChild(LoopInfo* pNode) :
            m_pRunner(pNode->m_pChild) {}

        public: EnumChild(const LoopInfo* pNode) :
            m_pRunner(const_cast<LoopInfo*>(pNode->m_pChild)) {}

        public: bool AtEnd() const
            { return NULL == m_pRunner; }

        public: LoopInfo* Get() const
            { ASSERT(! AtEnd()); return m_pRunner; }

        public: void Next()
            { ASSERT(! AtEnd()); m_pRunner = m_pRunner->m_pSibling; }
    }; // EnumChild
}; // LoopInfo

LoopInfo* ir_compute_loop_tree(Function*);

} // Compiler


#endif // !defined(INCLUDE_compiler_ir_loop_h)
