//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - pre-compiled header
// cm_base_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_bblock.h#7 $
//
#if !defined(INCLUDE_compiler_ir_bblock_h)
#define INCLUDE_compiler_ir_bblock_h

#include "./ir_cfg.h"
#include "./ir_instruction.h"

namespace Compiler
{

struct LoopInfo;

//////////////////////////////////////////////////////////////////////
//
// BBlock
//
class BBlock :
    public Object,
    public GraphNode_<Function, BBlock, CfgEdge>,
    public DLinkAnchor_<Instruction, Instruction, AnchorInsn>,
    public WithWorkArea,
    public WorkListItem_<BBlock>
{
    public: typedef DLinkAnchor_<Instruction, Instruction, AnchorInsn>
        InsnList;

    public: typedef DomInfo_<Function, BBlock> DomInfo;
    public: typedef DLinkAnchor_<Label> LabelList;

    public: enum Kind
    {
        Kind_Anchor = 0,
        Kind_Normal = 1,
    }; // Kind

    protected: uint             m_nName;
    protected: LabelList        m_oLabels;
    protected: DataFlowData*    m_pDFData;
    protected: DomInfo*         m_pDomInfo;
    protected: DomInfo*         m_pPostDomInfo;

    public: BBlock() :
        m_pDomInfo(NULL),
        m_pPostDomInfo(NULL),
        m_pLoopInfo(NULL),
        m_nName(0) {}

    public: BBlock(Kind);

    public: template<class T> T* Extend()
        { return reinterpret_cast<T*>(this); }

    public: Function*   GetFunction() const { return GetParent(); }
    public: Val         GetName() const  { return Fixnum::Encode(m_nName); }

    public: bool IsEntryBBlock() const
        { return InsnList::getHead()->Is<EntryInsn>(); }

    public: bool IsExitBBlock() const
        { return InsnList::getTail()->Is<ExitInsn>(); }

    ////////////////////////////////////////////////////////////
    //
    // Label
    //
    public: Label* AppendLabel(Label* pLabel)
        { return m_oLabels.Append_(pLabel); }

    public: Label* GetLabel() const
        { return m_oLabels.GetHead(); }

    public: class EnumLabel : public LabelList::Enum
    {
        public: EnumLabel(BBlock* p) :
            LabelList::Enum(&p->m_oLabels) {}
    }; // EnumLabel

    ////////////////////////////////////////////////////////////
    //
    // Dominance Tree
    //
    public: bool DoesDominate(const BBlock*) const;
    public: bool DoesPostDominate(const BBlock*) const;

    public: DomInfo* GetDomInfo()     const { return m_pDomInfo; }
    public: DomInfo* GetPostDomInfo() const { return m_pPostDomInfo; }

    public: class EnumChild : public DomInfo::EnumChild
    {
        public: EnumChild(BBlock* p) :
            DomInfo::EnumChild(p->GetDomInfo()) {}
    }; // EnumChild

    public: class EnumFrontier : public DomInfo::EnumFrontier
    {
        public: EnumFrontier(BBlock* p) :
            DomInfo::EnumFrontier(p->GetDomInfo()) {}
    }; // EnumFrontier

    public: class EnumPostChild : public DomInfo::EnumChild
    {
        public: EnumPostChild(BBlock* p) :
            DomInfo::EnumChild(p->GetPostDomInfo()) {}
    }; // EnumChild

    public: class EnumPostFrontier : public DomInfo::EnumFrontier
    {
        public: EnumPostFrontier(BBlock* p) :
            DomInfo::EnumFrontier(p->GetPostDomInfo()) {}
    }; // EnumFrontier

    ////////////////////////////////////////////////////////////
    //
    // Loop Nest Tree
    //
    protected: LoopInfo*   m_pLoopInfo;
        public: LoopInfo* GetLoopInfo() const      { return m_pLoopInfo; }
        public: LoopInfo* SetLoopInfo(LoopInfo* p) { return m_pLoopInfo = p; }

    ////////////////////////////////////////////////////////////
    //
    // Edge related
    //
    public: bool HasOnlyOnePred() const;
    public: bool HasOnlyOneSucc() const;
    public: bool HasMoreThanOnePred() const;
    public: bool HasMoreThanOneSucc() const;

    public: bool HasNonlocalInEdge() const;

    public: bool HasInEdge() const
    {
        return ! static_cast<const BBlock::EdgeInAnchor*>(this)->IsEmpty();
    } // HasInEdge

    public: bool HasOutEdge() const
    {
        return ! static_cast<const BBlock::EdgeOutAnchor*>(this)->IsEmpty();
    } // HasOutEdge

    public: CfgEdge* LinkInEdge(CfgEdge* pEdge)
    {
        ASSERT(this == pEdge->GetTo());
        return static_cast<BBlock::EdgeInAnchor*>(this)->Append_(pEdge);
    } // AddOut

    public: CfgEdge* LinkOutEdge(CfgEdge* pEdge)
    {
        ASSERT(this == pEdge->GetFrom());
        return static_cast<BBlock::EdgeOutAnchor*>(this)->Append_(pEdge);
    } // AddOut

    public: CfgEdge* UnlinkInEdge(CfgEdge* pEdge)
    {
        ASSERT(this == pEdge->GetTo());
        return static_cast<BBlock::EdgeInAnchor*>(this)->Remove_(pEdge);
    } // UnlinkInEdge

    public: CfgEdge* UnlinkOutEdge(CfgEdge* pEdge)
    {
        ASSERT(this == pEdge->GetFrom());
        return static_cast<BBlock::EdgeOutAnchor*>(this)->Remove_(pEdge);
    } // UnlinkOutEdge

    ////////////////////////////////////////////////////////////
    //
    // Instruction related
    //
    public: Instruction* AppendInsn(Instruction*);

    public: Instruction* GetAnchorInsn() const
        { return InsnList::GetAnchor(); }

    public: Instruction* GetFirstInsn() const
        { return InsnList::GetHead(); }

    public: Instruction* GetLastInsn() const
        { return InsnList::GetTail(); }

    public: bool HasInsn() const
        { return ! InsnList::IsEmpty(); }

    // EnumInsn
    public: class EnumInsn : public InsnList::Enum
    {
        public: EnumInsn(BBlock* pBBlock) :
            InsnList::Enum(static_cast<InsnList*>(pBBlock)) {}

        public: EnumInsn() {}
    }; // EnumInsn

    // EnumInsn_Reverse
    public: class EnumInsn_Reverse : public InsnList::Enum_Reverse
    {
        public: EnumInsn_Reverse(BBlock* pBBlock) :
            InsnList::Enum_Reverse(static_cast<InsnList*>(pBBlock)) {}

        public: EnumInsn_Reverse() {}
    }; // EnumInsn

    ////////////////////////////////////////////////////////////
    //
    // Debug related
    //
    public: virtual LPCWSTR GetHtmlClass() const { return L"b"; }
    public: virtual void HtmlPrint(Val, bool) const;
    public: bool Verify() const;
}; // BBlock

} // Compiler

#endif // !defined(INCLUDE_compiler_ir_bblock_h)
