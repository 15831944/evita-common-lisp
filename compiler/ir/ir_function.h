//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - IR - Function
// ir/ir_function.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_function.h#6 $
//
#if !defined(INCLUDE_compiler_ir_function_h)
#define INCLUDE_compiler_ir_function_h

#include "./ir_bblock.h"
#include "./ir_call_graph.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Function
//
class Function :
    public Operand,
    public Graph_<Function, BBlock>,
    public GraphNode_<Module, Function, CgEdge>,
    public WorkListItem_<Function>,
    public WithWorkArea
{
    public: static Operand::Kind GetKind_()
        { return Operand::Kind_Function; }

    public: enum Kind
    {
        Kind_Anchor,
        Kind_Finally,
        Kind_Normal,
        Kind_Toplevel,
        Kind_Template,
    }; // Kind

    protected: Kind m_eKind;
        public: Kind GetFunKind() const { return m_eKind; }

    protected: Val m_name;
        public: Val GetName() const { return m_name; }
        public: Val SetName(Val x)  { return m_name = x; }

    protected: Val m_funobj;
        public: Val  GetFunObj() const { return m_funobj; }
        public: void SetFunObj(Val val) { m_funobj = val; }

    protected: Val m_ty;
        public: virtual Val GetTy() const { return m_ty; }
        public: Val SetTy(Val x)  { return m_ty = x; }

    // Used by LABELS parser. m_fMayBeClosure is true during parseing
    // function definitions in LABELS form.
    protected: bool m_fMayBeClosure;
        public: bool MayBeClosure()   const  { return m_fMayBeClosure; }
        public: bool SetMayBeClosure(bool f) { return m_fMayBeClosure = f; }

    public: Function() :
        Operand(Operand::Kind_Function),
        m_eKind(Kind_Anchor),
        m_cbFrame(0),
        m_cExtraParams(0),
        m_fMayBeClosure(false),
        m_name(nil),
        m_document(nil),
        m_funobj(nil) {}

    public: Function(Val, Kind);

    public: template<class T> T* Extend()
        { return reinterpret_cast<T*>(this); }

    ////////////////////////////////////////////////////////////
    //
    // Basic Information
    //
    protected: uint m_nName;
        public: uint GetNum() const { return m_nName; }

    ////////////////////////////////////////////////////////////
    //
    // Frame for CG
    //
    // BUGBUG: We should think another place to keep size of frame.
    protected: uint m_cbFrame;
        public: uint GetFrameSize() const  { return m_cbFrame; }
        public: uint SetFrameSize(uint cb) { return m_cbFrame = cb; }

    ////////////////////////////////////////////////////////////
    //
    // Frame List
    //
    public: FrameList m_oFrames;
        // Note: Since we don't maintain outer link of frames, m_oFrames
        // includes removed frames.

    ////////////////////////////////////////////////////////////
    //
    // Documentation
    //
    protected: Val m_document;
        public: Val GetDocument() const       { return m_document; }
        public: Val SetDocument(Val document) { return m_document = document; }

    //////////////////////////////////////////////////////////////////////
    //
    // Lambda List
    //
    protected: int  m_iMin;
    protected: int  m_iMax;
    protected: bool m_fRest;

    public: int  GetArityMin() const { return m_iMin + m_cExtraParams; }
    public: int  GetArityMax() const { return m_iMax + m_cExtraParams; }
    public: bool HasRestParam() const     { return m_fRest; }
    public: bool SetRestParam(bool fRest) { return m_fRest = fRest; }

    public: void SetArity(int iMin, int iMax)
    {
        ASSERT(m_iMin <= m_iMax);
        m_iMin = iMin;
        m_iMax = iMax;
    } // SetArity

    public: bool IsStackRestify() const;
    public: bool NeedArity() const;

    public: uint m_cExtraParams;

    ////////////////////////////////////////////////////////////
    //
    // Instruction
    //
    public: EntryInsn*    GetEntryInsn() const;
    public: PrologueInsn* GetPrologueInsn() const;

    ////////////////////////////////////////////////////////////
    //
    // BBlock related
    //
    public: BBlock* GetEntryBB() const;
    public: BBlock* GetExitBB() const;
    public: BBlock* GetStartBB() const;
    public: bool HasNonlocalBlock() const;

    public: bool HasBBlock() const
        { return ! Layout::IsEmpty(); }
    
    public: BBlock* AppendBBlock(BBlock* pBBlock)
        { return Insert(pBBlock, GetExitBB()); }

    void SetCfgChanged() { SetChanged(); }

    ////////////////////////////////////////////////////////////
    //
    // Call Graph Related
    //
    public: void Realize(OperandBox*);
    public: void Unrealize(OperandBox*);

    public: class EnumCallee : public EnumOutEdge
    {
        public: EnumCallee(Function* p) : EnumOutEdge(p) {}
    }; // EnumCallee

    public: class EnumCaller : public EnumInEdge
    {
        public: EnumCaller(Function* p) : EnumInEdge(p) {}
    }; // EnumCaller

    protected: UseSiteList m_oCallSites;
        public: class EnumCallSite : public UseSiteList::Enum
        {
            public: EnumCallSite(Function* p) :
                UseSiteList::Enum(&p->m_oCallSites) {}
        }; // EnumCallSite

    protected: UseSiteList m_oUseSites;
        public: class EnumUseSite : public UseSiteList::Enum
        {
            public: EnumUseSite(Function* p) :
                UseSiteList::Enum(&p->m_oUseSites) {}
        }; // EnumUseSite

    public: bool HasCallee() const
        { return ! static_cast<const EdgeOutAnchor*>(this)->IsEmpty(); }

    public: bool HasCaller() const
        { return ! static_cast<const EdgeInAnchor*>(this)->IsEmpty(); }

    public: bool HasCallSite() const
        { return ! m_oCallSites.IsEmpty(); }

    public: bool HasUseSite() const
        { return ! m_oUseSites.IsEmpty(); }

    public: bool IsClosure() const;

    ////////////////////////////////////////////////////////////
    //
    // CFG related
    //
    Class_Enum_(BBlock, Layout, Function)
    Class_Enum_(BBlock, Preorder, Function)
    Class_Enum_(BBlock, Postorder, Function)

    #define EnumBBlock          EnumBBlock_Layout
    #define EnumBBlock_Reverse  EnumBBlock_Reverse_Layout

    public: bool PrepareTraversal();

    ////////////////////////////////////////////////////////////
    //
    // Dominance Tree
    //
    public: void ComputeDominance();

    ////////////////////////////////////////////////////////////
    //
    // Local Variables
    //
    protected: VarList m_oVariables;
        // Contains all variables defined in this function. This
        // list contains unreferenced variables.

    public: Variable* AddVar(Variable*);
    public: Variable* RemoveVar(Variable*);

    public: class EnumVar : public VarList::Enum
    {
        public: EnumVar(Function* p) :
            VarList::Enum(&p->m_oVariables) {}
    }; // EnumVar

    public: bool HasVar() const
        { return ! m_oVariables.IsEmpty(); }

    ////////////////////////////////////////////////////////////
    //
    // Up-Level Variables
    //
    protected: UseSiteList m_oUpVarSites;
        // Contains list of variable operand box of UPVAR instruction

    public: OperandBox* AddUpVarSite(OperandBox* pBox)
    {
        return m_oUpVarSites.Append_(pBox);
    } // AddUpVarSite

    public: class EnumUpVarSite : public UseSiteList::Enum
    {
        public: EnumUpVarSite(Function* p) :
            UseSiteList::Enum(&p->m_oUpVarSites) {}
    }; // EnumUpVar

    public: class EnumUpVar : public UseSiteList::Enum
    {
        public: EnumUpVar(Function* p) :
            UseSiteList::Enum(&p->m_oUpVarSites) {}

        public: Variable* Get() const
        {
            return UseSiteList::Enum::Get()->GetOperand()->
                StaticCast<Variable>();
        } // Get
    }; // EnumUpVar

    public: bool HasUpVar() const
        { return ! m_oUpVarSites.IsEmpty(); }

    public: OperandBox* RemoveUpVarSite(OperandBox* pBox)
    {
        return m_oUpVarSites.Remove_(pBox);
    } // RemoveUpVarSite

    ////////////////////////////////////////////////////////////
    //
    // Misc
    //
    public: bool Verify() const;

    // EnumReg
    public: class EnumReg
    {
        protected: EnumBBlock       m_oEnumBBlock;
        protected: BBlock::EnumInsn m_oEnumInsn;

        public: EnumReg(Function*);
        public: bool AtEnd() const;
        public: Register* Get() const;
        public: void Next();
    }; // EnumReg

    ////////////////////////////////////////////////////////////
    //
    // Printer
    //
    virtual void HtmlPrint(Val, bool) const;
}; // Function

} // Compiler

#endif // !defined(INCLUDE_compiler_ir_function_h)
