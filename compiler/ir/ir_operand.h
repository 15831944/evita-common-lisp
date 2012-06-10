//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - Operands
// ir_operand.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_operand.h#19 $
//
#if !defined(INCLUDE_compiler_ir_operand_h)
#define INCLUDE_compiler_ir_operand_h

#include "./ir_graph.h"

namespace Compiler
{

class Output;
class Void;

//////////////////////////////////////////////////////////////////////
//
// Operand
//
class Operand :
    public Object
{
    public: enum Kind
    {
        Kind_Anchor,
        Kind_Function,
        Kind_FunLit,        // for OPENxxx to prevent make function closure
        Kind_Label,
        Kind_Literal,
        Kind_Integer,
        Kind_TlvName,
        Kind_Unreachable,
        Kind_Variable,
        Kind_Void,

        Kind_TargetInput1,
        Kind_TargetInput2,
        Kind_TargetInput3,
        Kind_TargetInput4,
        Kind_TargetInput5,
        Kind_TargetInput6,
        Kind_TargetInput7,
        Kind_TargetInput8,
        Kind_TargetInput9,

        // Output
        Kind_Register,
        Kind_Values,
        Kind_Bool,
        Kind_Frame,
        Kind_TargetOutput1,
        Kind_TargetOutput2,
        Kind_TargetOutput3,
        Kind_TargetOutput4,
        Kind_TargetOutput5,
        Kind_TargetOutput6,
        Kind_TargetOutput7,
        Kind_TargetOutput8,
        Kind_TargetOutput9,
    }; // Kind

    protected: Kind m_eKind;
        public: Kind GetKind() const { return m_eKind; }

    public: Operand(Kind eKind) : m_eKind(eKind) {}

    protected: Operand() : m_eKind(Kind_Anchor) {}

    public: virtual bool Equal(const Operand* that) const
        { return this == that; }

    public: virtual bool Eq(Val) const { return false; }

    public: virtual Operand* Simplify() const
        { return const_cast<Operand*>(this); }

    public: virtual void Realize(OperandBox*) {}
    public: virtual void Unrealize(OperandBox*) {}

    public: virtual Ty GetTy() const { return ty_t; }

    public: template<class T> T* DynamicCast()
    {
        if (! Is<T>()) return NULL;
        return reinterpret_cast<T*>(this);
    } // DynamicCast

    public: template<class T> const T* DynamicCast() const
    {
        if (! Is<T>()) return NULL;
        return reinterpret_cast<const T*>(this);
    } // DynamicCast

    public: template<class T> T* StaticCast()
        { ASSERT(Is<T>()); return reinterpret_cast<T*>(this); }

    public: template<class T> const T* StaticCast() const
        { ASSERT(Is<T>()); return reinterpret_cast<const T*>(this); }

    public: template<class T> bool Is() const
        { return GetKind() == T::GetKind_(); }

    public: bool IsOutput() const 
        { return m_eKind >= Kind_Register; }

    public: Output* ToOutput()
        { return IsOutput() ? reinterpret_cast<Output*>(this) : NULL; }
}; // Operand


//////////////////////////////////////////////////////////////////////
//
// Operand Box
//
class OperandBox :
    public Atom,
    public LayoutSite_<Instruction, OperandBox>
{
    public: enum Kind
    {
        Kind_Value,
        Kind_Callee,
    }; // Kind

    protected: Kind m_eKind;
        public: Kind GetKind() const { return m_eKind; }
        public: Kind SetKind(Kind e) { return m_eKind = e; }

    protected: Variable* m_pVar;
        public: Variable* GetVar() const      { return m_pVar; }
        public: Variable* SetVar(Variable* p) { return m_pVar = p; }

    protected: Operand* m_pOperand;

    public: OperandBox(Kind eKind = Kind_Value) : 
        m_eKind(eKind), m_pOperand(NULL), m_pVar(NULL) {}

    public: OperandBox(Operand* pSx) : 
        m_eKind(Kind_Value), m_pOperand(pSx) {}

    public: Operand* GetOperand() const
        { return m_pOperand; }

    public: Operand* SetOperand(Operand*);

    public: void SetInstruction(Instruction* p) { SetParent(p); }
    public: Instruction* GetInstruction() const { return GetParent(); }
    public: Operand* Replace(Operand*);
    public: Register* GetRx() const;
}; // OperandBox


typedef DLinkAnchor_<OperandBox> UseSiteList;


//////////////////////////////////////////////////////////////////////
//
// Output
//
class Output :
    public Operand,
    public UseSiteList,
    public WithWorkArea
{
    public: virtual LPCWSTR GetHtmlClass() const { return L"r"; }
    public: virtual Ty GetTy() const;

    protected: uint         m_nName;
    protected: Instruction* m_pDfnInsn;

    public: Val GetName() const { return Fixnum::Encode(m_nName); }
    public: int GetNum() const {return m_nName; }

    public: virtual Operand* Simplify() const;

    public: Output(Kind eKind, Variable* pVar = NULL) : 
        Operand(eKind),
        m_nName(0),
        m_pDfnInsn(NULL),
        m_form(nil),
        m_fSSA(true),
        m_pVar(pVar) {}

    public: Instruction* GetDfn() const         { return m_pDfnInsn; }
    public: Instruction* SetDfn(Instruction* p) { return m_pDfnInsn = p; }

    ////////////////////////////////////////////////////////////
    //
    // SSA releated
    //
    protected: bool m_fSSA;
        public: bool IsSSA() const { return m_fSSA; }
        public: void MarkNotSSA()  { m_fSSA = false; }

    ////////////////////////////////////////////////////////////
    //
    // Source code information
    //
    protected: Val m_form;
        public: Val GetForm() const { return m_form; }
        public: Val SetForm(Val x) { return m_form = x; }

    // Note: Only register has variable information. To reduce checking
    // whether output is register or not, we have a variable information
    // here instead of class Register.
    protected: Variable* m_pVar;
        public: Variable* GetVar() const      { return m_pVar; }
        public: Variable* SetVar(Variable* p) { return m_pVar = p; }

    ////////////////////////////////////////////////////////////
    //
    // Use Site
    //
    public: bool HasUseSite() const
        { return ! IsEmpty(); }

    public: bool HasOnlyOneUseSite() const
    {
        Enum oEnum(this);
        if (! oEnum.AtEnd()) return false;
        if (! oEnum.AtEnd()) return false;
        return oEnum.AtEnd();
    } // HasOnlyOneUseSite

    public: Instruction* GetUseInsn() const
        { return GetHead()->GetInstruction(); }

    public: Instruction* GetSingleUser() const
    {
        if (IsEmpty()) return NULL;
        if (GetHead()->GetNext() != GetAnchor()) return NULL;
        return GetHead()->GetInstruction();
    } // GetSingleUser

    // Realize
    //  Link use site.
    public: virtual void Realize(OperandBox* pBox)
    {
        if (NULL == pBox->GetVar()) pBox->SetVar(m_pVar);
        Append_(pBox);
    } // Realize

    // Unrealize
    //  Unlink use site.
    public: virtual void Unrealize(OperandBox* pBox)
        { Remove_(pBox); }

    // EnumUseSite
    public: class EnumUseSite : public Enum
        { public: EnumUseSite(Output* p) : Enum(p) {} };
}; // Output


//////////////////////////////////////////////////////////////////////
//
// Bool
//
class Bool :
    public Output
{
    public: static Kind GetKind_() { return Kind_Bool; }
    public: virtual void HtmlPrint(Val, bool) const;

    public: Bool();
    public: Bool(uint nName) : Output(GetKind_()) { m_nName = nName; }
}; // Bool

// BoolConstant -- for %b0 and %b1
class BoolConstant : public Bool
{
    public: virtual void Realize(OperandBox*) {}
    public: virtual void Unrealize(OperandBox*) {}

    public: BoolConstant(uint n) : Bool(n) {}
}; // BoolConstant


//////////////////////////////////////////////////////////////////////
//
// Frame
//
class Frame :
    public Output,
    public DLinkSite_<Frame>,
    public WorkListItem_<Frame>
{
    public: static Kind GetKind_() { return Kind_Frame; }
    public: virtual void HtmlPrint(Val, bool) const;

    protected: Val m_kind;  // block, catch, tagbody, finally
        public: Val GetKind() const { return m_kind; }

    protected: Val m_name;
        public: Val GetName()  const { return m_name; }

    protected: Frame* m_pOuter;
        public: Frame* GetOuter() const  { return m_pOuter; }
        public: Frame* SetOuter(Frame* p){ return m_pOuter = p; }

    protected: Function* m_pOwner;
        public: Function* GetOwner() const      { return m_pOwner; }
        public: Function* SetOwner(Function* p) { return m_pOwner = p; }

    protected: int m_iLocation;
        public: int GetLocation() const   { return m_iLocation; }
        public: int SetLocation(int iLoc) { return m_iLocation = iLoc; }

    protected: uint m_cbFrame;
        public: uint GetSize() const  { return m_cbFrame; }
        public: uint SetSize(uint cb) { return m_cbFrame = cb; }

    protected: Operand* m_pDatum;
        public: Operand* GetDatum() const     { return m_pDatum; }
        public: Operand* SetDatum(Operand* x) { return m_pDatum = x; }

    public: Frame(
        Function*   pOwner = NULL,
        Val         kind   = nil,
        Val         name   = nil);

    public: template<class T> T* DynamicCast()
        { return Is<T>() ? reinterpret_cast<T*>(this) : NULL; }

    public: template<class T> bool Is()
        { return T::GetFrameKind_() == m_kind; }

    public: template<class T> T* StaticCast()
        { ASSERT(Is<T>()); return reinterpret_cast<T*>(this); }

    // For tagbody frame
    public: uint GetCount() const
        { return static_cast<uint>(Fixnum::Decode_(m_name)); }

    public: void IncCount() { m_name = add_xx(m_name, 1); }
    public: void DecCount() { m_name = sub_xx(m_name, 1); }

    // Enum
    public: class Enum
    {
        protected: Frame* m_pRunner;

        public: Enum(Frame* pFrame) :
            m_pRunner(pFrame) {}

        public: bool AtEnd() const
            { return NULL == m_pRunner; }

        public: Frame* Get() const
            { ASSERT(! AtEnd()); return m_pRunner; }

        public: void Next()
            { ASSERT(! AtEnd()); m_pRunner = m_pRunner->GetOuter(); }
    }; // Enum
}; // Frame


//////////////////////////////////////////////////////////////////////
//
// FunLit
//
// Description:
//  Used for establishing Block/Catch/Tagbody frame.
//
class FunLit : public Operand
{
    public: static Kind GetKind_() { return Kind_FunLit; }
    public: virtual void HtmlPrint(Val, bool) const;

    protected: Function* m_pFun;
        public: Function* GetFun() const { return m_pFun; }

    public: FunLit(Function* pFun) :
        Operand(Kind_FunLit), m_pFun(pFun) {}
}; // FunLit


//////////////////////////////////////////////////////////////////////
//
// Integer
//
class Integer : public Operand
{
    public: static Kind GetKind_() { return Kind_Integer; }
    public: virtual void HtmlPrint(Val, bool) const;

    protected: Int m_iVal;

    public: Integer(Int iVal) :
        Operand(Kind_Integer), m_iVal(iVal) {}

    public: Int GetValue() const   { return m_iVal; }

    public: virtual Ty GetTy() const { return ty_int; }
}; // Integer


//////////////////////////////////////////////////////////////////////
//
// Label
//
class Label :
    public Operand,
    public DLinkSite_<Label>
{
    public: static Kind GetKind_() { return Kind_Label; }
    public: virtual void HtmlPrint(Val, bool) const;
    public: virtual void Realize(OperandBox*);
    public: virtual void Unrealize(OperandBox*);

    protected: BBlock* m_pBBlock;

    public: Label(BBlock* pBBlock = NULL) :
        Operand(Kind_Label),
        m_pBBlock(pBBlock) {}

    public: BBlock* GetBBlock() const    { return m_pBBlock; }
    public: BBlock* SetBBlock(BBlock* p) { return m_pBBlock = p; }
}; // Label


//////////////////////////////////////////////////////////////////////
//
// Literal
//
class Literal : public Operand
{
    public: static Kind GetKind_() { return Kind_Literal; }
    public: virtual void HtmlPrint(Val, bool) const;

    public: virtual bool Eq(Val x) const { return m_datum == x; }

    public: virtual bool Equal(const Operand* pSx) const
    {
        if (this == pSx) return true;
        if (! pSx->Is<Literal>()) return false;
        return pSx->StaticCast<Literal>()->GetDatum() == m_datum;
    } // Match

    protected: Ty m_ty;
        public: virtual Ty GetTy() const { return m_ty; }

    protected: Val m_datum;

    protected: Literal(Ty ty, Val datum) :
        Operand(GetKind_()), m_ty(ty), m_datum(datum) {}

    public: Literal(Val datum) :
        Operand(GetKind_()), m_datum(datum)
    {
        if (Fixnum::Encode(0) == datum) m_ty = ty_eql_0;
        else if (Fixnum::Encode(1) == datum) m_ty = ty_eql_1;
        else if (Fixnum::Encode(-1) == datum) m_ty = ty_eql_M1;
        else if (integerp(datum)) 
            { m_ty = list(Qeql, datum); }
        else
            { m_ty = type_of(datum); }
    } // Literal

    public: Val GetDatum() const { return m_datum; }
    public: Val SetDatum(Val datum) { return m_datum = datum; }
}; // Literal


//////////////////////////////////////////////////////////////////////
//
// Register
//
class Register :
    public Output,
    public WorkListItem_<Register>,
    public WithIndex
{
    public: static Kind GetKind_() { return Kind_Register; }
    public: virtual void HtmlPrint(Val, bool) const;

    public: virtual bool Equal(const Operand*) const;

    public: enum Storage
    {
        Storage_Closed,
        Storage_Pseudo,
        Storage_Physical,
        Storage_Virtual,
        Storage_Stack,
        Storage_LoadTimeValue,
    }; // Storage

    protected: Storage      m_eStorage;
    protected: int          m_iStorage;

    ////////////////////////////////////////////////////////////
    //
    // Register Class
    //
    public: enum Class
    {
        Class_GPR,
        Class_FPR,
    }; // Class
    protected: Class m_eClass;
        public: Class GetClass() const { return m_eClass; }

    public: static const int  Unassigned = -1 << 31;

    protected: OperandBox   m_oAnchor;

    ////////////////////////////////////////////////////////////
    //
    // Constructors
    //
    public: Register(Variable* = NULL, Class eClass = Class_GPR);

    ////////////////////////////////////////////////////////////
    //
    // Representable
    //
    protected: Register*    m_pRep;
        public: Register* GetRep() const       { return m_pRep; }
        public: Register* SetRep(Register* p)  { return m_pRep = p; }

    ////////////////////////////////////////////////////////////
    //
    // Storeage related methods
    //
    // GetStorage
    public: Storage GetStorage() const
        { return m_eStorage; }

    // GetLocation
    public: int GetLocation() const
        { return m_iStorage; }

    // IsClosed
    public: bool IsClosed() const
        { return Storage_Closed == m_eStorage; }

    // IsPseudo
    public: bool IsPseudo() const
        { return Storage_Pseudo == m_eStorage; }

    // IsPhysical
    public: bool IsPhysical() const
        { return Storage_Physical == m_eStorage; }

    // IsStackSlot
    public: bool IsStackSlot() const
        { return Storage_Stack == m_eStorage; }

    // IsVirtual
    public: bool IsVirtual() const
        { return Storage_Virtual == m_eStorage; }

    // SetStorage
    public: void SetStorage(Storage eClass, int iLocation = 0)
    {
        ASSERT(Unassigned != iLocation);

        m_eStorage = eClass;
        m_iStorage = iLocation;
    } // SetStorage

    ////////////////////////////////////////////////////////////
    //
    // Work Area
    //

    // Reset
    public: void Reset() { WithWorkArea::Reset(); WithIndex::Reset(); }
    public: void ResetWorkArea() { WithWorkArea::Reset(); }
}; // Regsiter


//////////////////////////////////////////////////////////////////////
//
// FpRegister
//
class FpRegister : public Register
{
    public: FpRegister(Variable* pVar = NULL) :
        Register(pVar, Class_FPR) {}
}; // FpRegister


//////////////////////////////////////////////////////////////////////
//
// Unreachable
//
class Unreachable : public Operand
{
    public: static Kind GetKind_() { return Kind_Unreachable; }
    public: virtual void HtmlPrint(Val, bool) const;

    public: Unreachable() : Operand(Kind_Unreachable) {}
}; // Unreachable


//////////////////////////////////////////////////////////////////////
//
// Values
//
class Values : public Output
{
    public: static Kind GetKind_() { return Kind_Values; }
    public: virtual void HtmlPrint(Val, bool) const;
    public: virtual bool Equal(const Operand*) const;

    public: Values();
}; // Values


//////////////////////////////////////////////////////////////////////
//
// Variable
//
class Variable :
    public Operand,
    public DLinkSite_<Variable>,
    public WithIndex,
    public WithWorkArea
{
    public: static Kind  GetKind_() { return Kind_Variable; }
    public: virtual void HtmlPrint(Val, bool) const;
    public: virtual void Realize(OperandBox*);
    public: virtual void Unrealize(OperandBox*);

    typedef enum Storage
    {
        Storage_Anchor,
        Storage_Heap,
        Storage_Literal,
        Storage_Register,
        Storage_Stack,

        Storage_MAX_1,
    }; // Storage

    protected: Storage m_eStorage;
        public: Storage GetStorage() const    { return m_eStorage; }
        public: Storage SetStorage(Storage e) { return m_eStorage = e; }

    protected: Val m_name;
        public: Val GetName()  const   { return m_name; }

    protected: Function* m_pOwner;
        public: Function* GetOwner() const
            { ASSERT(NULL != m_pOwner); return m_pOwner; }

        public: Function* SetOwner(Function* p)
            { ASSERT(NULL != p); return m_pOwner = p; }

    protected: Instruction* m_pDfnInsn;
        public: Instruction* GetDfn() const         { return m_pDfnInsn; }
        public: Instruction* SetDfn(Instruction* p) { return m_pDfnInsn = p; }
        public: bool HasDfn() const { return NULL != m_pDfnInsn; }

    protected: uint m_nUpVarCount;
        public: uint GetUpVarCount() const { return m_nUpVarCount; }

    protected: Ty m_ty;
        public: virtual Ty GetTy() const { return m_ty; }
        public: Ty SetTy(Ty ty)  { return m_ty = ty; }

    public: Variable(Val name = nil) :
        Operand(Kind_Variable),
        m_name(name),
        m_ty(ty_t),
        m_pDfnInsn(NULL),
        m_pOwner(NULL),
        m_nUpVarCount(0),
        m_eStorage(nil == name ? Storage_Anchor : Storage_Register)
    {
        ASSERT(Kernel::Symbol::Is_(name));
    } // Variable

    public: bool IsSSA() const;

    public: void Reset()
    {
        WithWorkArea::Reset();
        WithIndex::Reset();
    } // Reset
}; // Variable


//////////////////////////////////////////////////////////////////////
//
// Void
//
class Void : public Output
{
    public: static Kind GetKind_() { return Kind_Void; }
    public: virtual void HtmlPrint(Val, bool) const;
    public: Void() : Output(GetKind_()) {}

    public: virtual void Realize(OperandBox*) {}
    public: virtual void Unrealize(OperandBox*) {}
}; // Void


//////////////////////////////////////////////////////////////////////
//
// Operand List
//
typedef DLinkAnchor_<Variable> VarList;
typedef DLinkAnchor_<Frame> FrameList;
typedef WorkList_<Frame> FrameWorkList;

//////////////////////////////////////////////////////////////////////
//
// Singleton Operands
//
extern Unreachable*     Obj_Unreachable;
extern Void*            Obj_Void;
extern BoolConstant*    Bool_True;
extern BoolConstant*    Bool_False;
extern Literal*         Obj_Nil;
extern Literal*         Obj_True;

} // Compiler

#endif //!defined(INCLUDE_compiler_ir_operand_h)
