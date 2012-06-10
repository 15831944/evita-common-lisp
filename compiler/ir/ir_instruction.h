//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR instruction
// compiler/ir/ir_instruction.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_instruction.h#50 $
//
#if !defined(INCLUDE_compiler_ir_instruction_h)
#define INCLUDE_compiler_ir_instruction_h

#include "./ir_operand.h"
#include "./ir_type.h"

namespace Compiler
{

class Instruction;
class Operand;
class OperandBox;
class Output;


extern Integer* NewInteger(Int);
extern Literal* NewLiteral(Val);
extern Output*  NewOutput(Ty);
extern void warn(const char16*, Val);

//////////////////////////////////////////////////////////////////////
//
// Base Instructions
//
enum BaseOpCode
{
    #define DEFIROP(mp_name)    IrOp_ ## mp_name,
    #include "./ir_opcode.inc"
}; // BaseOpCode


//////////////////////////////////////////////////////////////////////
//
// Instruction
//
class Instruction :
    public Object,
    public LayoutSite_<BBlock, Instruction>,
    public SccComponent_<Instruction>,
    public WorkListItem_<Instruction>,
    public WithWorkArea
{
    public: enum Family
    {
        Family_Normal,
        Family_Arithmetic,
        Family_Terminal,
    }; // Family

    public: enum Attr
    {
        // RUNTIMECAST, TYPEP
        Attr_Simple = 1 << 0,   // Does simple type checking

        // BOUND, RUNTIMECAST
        Attr_Nop    = 1 << 1,   // No code emitted

        // ELT
        Attr_Array  = 1 << 2,   // Array reference - ELT
    }; // Attr

    public: typedef SccComponent_<Instruction> SccInfo;
    public: SccInfo* GetSccInfo() { return this; }

    public: Instruction(
                Ty      ty = ty_void,
                Output* pOutput = Obj_Void,
                uint    rgfAttr = 0 ) :
        m_ty(ty),
        m_pOutput(pOutput),
        m_pVar(pOutput->GetVar()),
        m_rgfAttr(rgfAttr),
        m_nIndex(0) {}

    public: virtual Instruction* Clone() const
        { CAN_NOT_HAPPEN(); }

    public: virtual uint          GetOpcode() const = 0;
    public: virtual const char16* GetMnemonic() const = 0;
    public: virtual uint          GetOperandCount() const { return 0; }
    public: virtual OperandBox*   GetOperandBox(uint) const { return NULL; }

    // Identical - returns true if instruction is identical in machine
    // code. This method is used for tail-merging.
    public: virtual bool Identical(const Instruction*) const;

    // m_nAttr
    protected: uint m_rgfAttr;
        public: bool HasAttr(Attr a) const { return 0 != (m_rgfAttr & a); }
        public: uint ClearAttr(Attr a)     { return m_rgfAttr &= ~a; }

    public: virtual uint GetFamily() const 
            { return Family_Normal; }

    public: bool HasTerminalAttr() const
            { return GetFamily() == Family_Terminal; }

    // m_ty -- type of output
    protected: Ty m_ty;
        public: Ty GetTy() const { return m_ty; }
        public: Ty SetTy(Ty ty)  { return m_ty = ty; }

    public: virtual bool UpdateTy() { return false; }

    // m_pVar
    protected: Variable* m_pVar;
        public: Variable* GetVar() const      { return m_pVar; }
        public: Variable* SetVar(Variable* p) { return m_pVar = p; }

    // m_pOutput -- output operand
    protected: Output* m_pOutput;
        public: Output* GetOutput() const { return m_pOutput; }
        public: Output* SetOutput(Output*);

    // m_nIndex -- work area for phase
    protected: uint     m_nIndex;
        public: uint GetIndex() const { return m_nIndex; }
        public: uint SetIndex(uint n) { return m_nIndex = n; }

    // GetBd
    public: Bool* GetBd() const
        { return GetOutput()->DynamicCast<Bool>(); }

    // GetRd
    public: Register* GetRd() const
        { return GetOutput()->DynamicCast<Register>(); }

    // GetVd
    public: Values* GetVd() const
        { return GetOutput()->DynamicCast<Values>(); }

    public: virtual bool IsPinned() const { return false; }
    public: virtual bool IsUseless() const;
    public: virtual void Realize();
    public: virtual void Unrealize();
    public: virtual bool Verify() const;

    ////////////////////////////////////////////////////////////
    //
    // Optimization
    //
    public: Operand* SimplifyOutput() const;

    public: virtual bool Simplify();

    protected: virtual Operand* SimplifyOutputAux() const
        { return m_pOutput; }

    ////////////////////////////////////////////////////////////
    //
    // Utility Functions
    //
    // GetBBlock
    public: BBlock* GetBBlock() const
        { return GetParent(); }

    // GetFunction
    public: Function* GetFunction() const;

    // GetLiteral
    public: Val GetLiteral(uint nIndex) const
    {
        return GetOperand(nIndex)->StaticCast<Literal>()->GetDatum();
    } // GetLiteral

    // GetOperand
    public: Operand* GetOperand(uint nIndex) const
    {
        return GetOperandBox(nIndex)->GetOperand();
    } // GetOperand

    public: Bool* GetBx() const
        { return GetSx()->StaticCast<Bool>(); }

    public: Int GetIx() const
        { return GetSx()->StaticCast<Integer>()->GetValue(); }

    public: Int GetIy() const
        { return GetSy()->StaticCast<Integer>()->GetValue(); }

    public: Int GetIz() const
        { return GetSz()->StaticCast<Integer>()->GetValue(); }

    public: Val GetLx() const
        { return GetSx()->StaticCast<Literal>()->GetDatum(); }

    public: Val GetLy() const
        { return GetSy()->StaticCast<Literal>()->GetDatum(); }

    public: Val GetLz() const
        { return GetSz()->StaticCast<Literal>()->GetDatum(); }

    public: Register* GetRx() const
        { return GetSx()->DynamicCast<Register>(); }

    public: Register* GetRy() const
        { return GetSy()->DynamicCast<Register>(); }

    public: Register* GetRz() const
        { return GetSz()->DynamicCast<Register>(); }

    public: Operand* GetSx() const
        { return GetOperand(0); }

    public: Operand* GetSy() const
        { return GetOperand(1); }

    public: Operand* GetSz() const
        { return GetOperand(2); }

    public: Values* GetVx() const
        { return GetSx()->DynamicCast<Values>(); }

    public: Values* GetVy() const
        { return GetSy()->DynamicCast<Values>(); }

    public: Values* GetVz() const
        { return GetSz()->DynamicCast<Values>(); }

    // EnumInput
    public: class EnumInput
    {
        protected: Instruction* m_pInsn;
        protected: uint  m_nIndex;

        public: EnumInput(Instruction* pInsn) :
            m_pInsn(pInsn),
            m_nIndex(0) {}

        public: EnumInput(const Instruction* pInsn) :
            m_pInsn(const_cast<Instruction*>(pInsn)),
            m_nIndex(0) {}

        public: bool AtEnd() const
        {
            return m_pInsn->GetOperandCount() == m_nIndex;
        } // AtEnd

        public: Operand* Get() const
        {
            return GetBox()->GetOperand();
        } // Get

        public: OperandBox* GetBox() const
        {
            ASSERT(! AtEnd());
            return m_pInsn->GetOperandBox(m_nIndex);
        } // Get

        public: Output* GetOutput() const
        {
            if (! Get()->IsOutput())
            {
                return NULL;
            }

            return reinterpret_cast<Output*>(Get());
        } // GetOutput

        public: Register* GetReg() const
            { return Get()->DynamicCast<Register>(); }

        public: void Next()
        {
            ASSERT(! AtEnd());
            m_nIndex += 1;
        } // Next
    }; // EnumInput

    public: bool IsRealized() const { return NULL != GetBBlock(); }

    public: void ReplaceLabelOperand(BBlock*, BBlock*);

    public: void SetVoid() { m_ty = ty_void; m_pOutput = Obj_Void; }

    ////////////////////////////////////////////////////////////
    //
    // Dynamic Type
    //
    public: template<class T> T* DynamicCast()
        { return Is<T>() ? reinterpret_cast<T*>(this) : NULL; }

    public: template<class T> const T* DynamicCast() const
        { return const_cast<Instruction*>(this)->DynamicCast<T>(); }

    public: template<class T> T* StaticCast()
        { ASSERT(Is<T>()); return reinterpret_cast<T*>(this); }

    public: template<class T> const T* StaticCast() const
        { return const_cast<Instruction*>(this)->StaticCast<T>(); }

    public: template<class T> bool Is() const
        { return GetMnemonic() == T::GetMnemonic_(); }

    ////////////////////////////////////////////////////////////
    //
    // Printer
    //
    public: virtual void HtmlPrint(Val, bool) const;

    ////////////////////////////////////////////////////////////
    //
    // Utility methods
    //
    protected: bool simplifyBx();
}; // Instruction


#define THIS_IS_IR_INSTRUCTION(mp_name, mp_format) \
    THIS_IS_IR_INSTRUCTION2(mp_name, mp_format, #mp_name)

#define THIS_IS_IR_INSTRUCTION2(mp_name, mp_format, mp_rname) \
    public: virtual uint GetOpcode() const \
        { return IrOp_ ## mp_name; } \
    public: static LPCWSTR GetMnemonic_() \
        { return L ## mp_rname; } \
    public: virtual LPCWSTR GetMnemonic() const \
        { return GetMnemonic_(); }


//////////////////////////////////////////////////////////////////////
//
// No Operand Instruction
//
template<class Insn_, class Base_ = Instruction>
class No_Operand_Instruction_ :
    public Base_
{
    typedef No_Operand_Instruction_<Insn_, Base_>
        No_Operand_Instruction;

    public: virtual uint GetOperandCount() const { return 0; }

    public: virtual OperandBox* GetOperandBox(uint) const
    {
        CAN_NOT_HAPPEN();
    } // GetOperandBox

    public: No_Operand_Instruction_(
            Ty ty = ty_void, Output* pOutput = Obj_Void,
            uint rgfAttr = 0) :
        Base_(ty, pOutput, rgfAttr) {}
}; // No_Operand_Instruction_


//////////////////////////////////////////////////////////////////////
//
// One Operand Instruction
//
template<class Insn_, class Base_ = Instruction>
class One_Operand_Instruction_ :
    public Base_
{
    protected: typedef One_Operand_Instruction_<Insn_, Base_>
        One_Operand_Instruction;

    protected: OperandBox m_rgoInputBox[1];

    public: virtual uint GetOperandCount() const { return 1; }

    public: virtual OperandBox* GetOperandBox(uint nIndex) const
    {
        ASSERT(0 == nIndex);
        return const_cast<OperandBox*>(&m_rgoInputBox[0]);
    } // GetOperandBox

    public: One_Operand_Instruction_(
        Operand*    pSx,
        Ty          ty = ty_void,
        Output*     pOutput = Obj_Void,
        uint        rgfAttr = 0 ) :
            Base_(ty, pOutput, rgfAttr )
    {
        ASSERT(NULL != pSx);
        m_rgoInputBox[0].SetOperand(pSx);
    } // One_Operand_Instruction_
}; // One_Operand_Instruction_


//////////////////////////////////////////////////////////////////////
//
// Two Operands Instruction
//
template<class Insn_, class Base_ = Instruction>
class Two_Operands_Instruction_ :
    public Base_
{
    protected: typedef Two_Operands_Instruction_<Insn_, Base_>
        Two_Operands_Instruction;

    protected: OperandBox m_rgoInputBox[2];

    public: virtual uint GetOperandCount() const { return 2; }

    public: virtual OperandBox* GetOperandBox(uint nIndex) const
    {
        ASSERT(nIndex <- 1);
        return const_cast<OperandBox*>(&m_rgoInputBox[nIndex]);
    } // GetOperandBox

    public: Two_Operands_Instruction_(
        Operand*    pSx,
        Operand*    pSy,
        Ty          ty = ty_void,
        Output*     pOutput = Obj_Void,
        uint        rgfAttr = 0 ) :
            Base_(ty, pOutput, rgfAttr)
    {
        ASSERT(NULL != pSx);
        ASSERT(NULL != pSy);

        m_rgoInputBox[0].SetOperand(pSx);
        m_rgoInputBox[1].SetOperand(pSy);
    } // Two_Operands_Instruction_
}; // Two_Operands_Instruction_


//////////////////////////////////////////////////////////////////////
//
// Three Operands Instruction
//
template<class Insn_, class Base_ = Instruction>
class Three_Operands_Instruction_ :
    public Base_
{
    protected: typedef Three_Operands_Instruction_<Insn_, Base_>
        Three_Operands_Instruction;

    protected: OperandBox m_rgoInputBox[3];

    public: virtual uint GetOperandCount() const { return 3; }

    public: virtual OperandBox* GetOperandBox(uint nIndex) const
    {
        ASSERT(nIndex <- 2);
        return const_cast<OperandBox*>(&m_rgoInputBox[nIndex]);
    } // GetOperandBox

    public: Three_Operands_Instruction_(
        Operand*    pSx,
        Operand*    pSy,
        Operand*    pSz,
        Ty          ty = ty_void,
        Output*     pOutput = Obj_Void,
        uint        rgfAttr = 0 ) :
            Base_(ty, pOutput, rgfAttr)
    {
        ASSERT(NULL != pSx);
        ASSERT(NULL != pSy);
        ASSERT(NULL != pSz);

        m_rgoInputBox[0].SetOperand(pSx);
        m_rgoInputBox[1].SetOperand(pSy);
        m_rgoInputBox[2].SetOperand(pSz);
    } // Three_Operands_Instruction_
}; // Three_Operands_Instruction_


//////////////////////////////////////////////////////////////////////
//
// Terminal Instruction Family
//
class TerminalInsnFamily : public Instruction
{
    public: virtual uint GetFamily() const { return Family_Terminal; }

    public: TerminalInsnFamily(Ty ty, Output* pOutput, uint attr) :
        Instruction(ty, pOutput, attr) {}
}; // TerminalInsnFamily


//////////////////////////////////////////////////////////////////////
//
// Arithmetic Instruction Family
//
class ArithmeticInsnFamily : public Instruction
{
    public: virtual uint GetFamily() const { return Family_Arithmetic; }

    public: ArithmeticInsnFamily(Ty ty, Output* pSd, uint attr) :
        Instruction(ty, pSd, attr) {}
}; // ArithmeticInsnFamily


//////////////////////////////////////////////////////////////////////
//
// Anchor Instruction
//
class AnchorInsn : public No_Operand_Instruction_<AnchorInsn>
{
    THIS_IS_IR_INSTRUCTION(ANCHOR, "ANCHOR")

    public: virtual void Realize() { CAN_NOT_HAPPEN(); }
    public: virtual void Unrealize() { CAN_NOT_HAPPEN(); }
}; // AnchorInsn

#define DEFINE_ARITH_INSN(mp_Name, mp_NAME, mp_format) \
class mp_Name : \
    public Two_Operands_Instruction_<mp_Name, ArithmeticInsnFamily> \
{ \
    THIS_IS_IR_INSTRUCTION(mp_NAME, mp_format) \
    public: Operand* SimplifyOutputAux() const; \
    public: virtual Instruction* Clone() const \
        { return new mp_Name(m_ty, NewOutput(m_ty), GetSx(), GetSy()); } \
    public: mp_Name( \
        Ty          ty, \
        Output*     pSd, \
        Operand*    pSx, \
        Operand*    pSy ) : \
            Two_Operands_Instruction(pSx, pSy, ty, pSd) {} \
}; // DEFINE_ARITH_INSN

DEFINE_ARITH_INSN(AddInsn, ADD, "ADD ty %rd <- %sx %sy")
DEFINE_ARITH_INSN(DivInsn, DIV, "DIV ty %rd <- %sx %sy")
DEFINE_ARITH_INSN(MulInsn, MUL, "MUL ty %rd <- %sx %sy")
DEFINE_ARITH_INSN(SubInsn, SUB, "SUB ty %rd <- %sx %sy")
DEFINE_ARITH_INSN(TruncateInsn, TRUNCATE, "TRUNCATE ty %rd <- %sx %sy")

DEFINE_ARITH_INSN(ShlInsn, SHL, "SHL ty %rd <- %sx %sy")
DEFINE_ARITH_INSN(ShrInsn, SHR, "SHR ty %rd <- %sx %sy")

DEFINE_ARITH_INSN(LogAndInsn, LOGAND, "LOGAND ty %rd <- %sx %sy")
DEFINE_ARITH_INSN(LogIorInsn, LOGIOR, "LOGIOR ty %rd <- %sx %sy")
DEFINE_ARITH_INSN(LogXorInsn, LOGXOR, "LOGXOR ty %rd <- %sx %sy")


//////////////////////////////////////////////////////////////////////
//
// BOUND instruction
//
class BoundInsn :
    public Two_Operands_Instruction_<BoundInsn>
{
    THIS_IS_IR_INSTRUCTION(BOUND, "BOUND ty %rd <- %array %index")

    // For expand into CALL
    Frame* m_pFrame;
        public: Frame* GetFrame() const  { return m_pFrame; }

    public: BoundInsn(
        Register*   pRd,
        Operand*    pArray,
        Operand*    pIndex,
        Frame*      pFrame,
        uint        rgfAttr) :
            Two_Operands_Instruction(pArray, pIndex, ty_sequence_index, pRd,
                rgfAttr ),
            m_pFrame(pFrame) {}
}; // BoundInsn


//////////////////////////////////////////////////////////////////////
//
// BOX Instruction
//
//  Note: %sx can be character for supporing non-Unicode platform.
//  Note: Operand type may be lost after expanding RUNTIMECAST.
//
class BoxInsn : public One_Operand_Instruction_<BoxInsn>
{
    THIS_IS_IR_INSTRUCTION(BOX, "BOX ty %rd <- %sx")

    public: Ty m_opty;

    // constructors
    public: BoxInsn(Val ty, Register* pRd, Operand* pSx) :
        One_Operand_Instruction(pSx, ty, pRd), m_opty(pSx->GetTy()) {}
}; // BoxInsn


//////////////////////////////////////////////////////////////////////
//
// BRANCH Instruction
//
class BranchInsn :
    public Three_Operands_Instruction_<BranchInsn, TerminalInsnFamily>

{
    THIS_IS_IR_INSTRUCTION(BRANCH, "BRANCH %bx true false")

    public: virtual bool Simplify() { return simplifyBx(); }

    public: BBlock* GetFalse() const
        { return GetSz()->StaticCast<Label>()->GetBBlock(); }

    public: BBlock* GetTrue() const
        { return GetSy()->StaticCast<Label>()->GetBBlock(); }

    public: BranchInsn(Bool* pBool, Label* pTrue, Label* pFalse) :
        Three_Operands_Instruction(pBool, pTrue, pFalse) {}

    public: BranchInsn(Bool*, BBlock*, BBlock*);
}; // BranchInsn


//////////////////////////////////////////////////////////////////////
//
// CALL instruction
//
// Note: Callee operand of CALL instruction can be closure before
// cg-UPVAR pass. cg-UPVAR splits closure callee into inner function
// and closure.
class CallInsn :
    public Two_Operands_Instruction_<CallInsn>
{
    THIS_IS_IR_INSTRUCTION(CALL, "CALL ty %vd <- callee %vx")

    public: virtual void     HtmlPrint(Val, bool) const;
    public: virtual bool     IsPinned() const;
    public: virtual Operand* SimplifyOutputAux() const;
    public: virtual bool     UpdateTy();

    public: virtual Instruction* Clone() const
    {
        Output* pSd = NewOutput(m_ty);
        CallInsn* pCall = new CallInsn(m_ty, pSd, GetSx(), GetVy());
            pCall->m_form       = m_form;
            pCall->m_pFrame     = m_pFrame;
            pCall->m_fNotInline = m_fNotInline;
        return pCall;
    } // Clone

    public: CallInsn(Ty ty, Output* pVd, Operand* pCallee, Values* pVx) :
        Two_Operands_Instruction(pCallee, pVx, ty, pVd),
        m_form(nil),
        m_pFrame(NULL),
        m_fNotInline(false)
    {
        m_rgoInputBox[0].SetKind(OperandBox::Kind_Callee);
        UpdateTy();
    } // CallInsn

    public: CallInsn(Operand* pCallee, Values* pVx) :
        Two_Operands_Instruction(pCallee, pVx),
        m_form(nil),
        m_pFrame(NULL),
        m_fNotInline(false)
    {
        m_rgoInputBox[0].SetKind(OperandBox::Kind_Callee);
        UpdateTy();
    } // CallInsn

    // Source
    protected: Val m_form;
        public: Val GetForm() const   { return m_form; }
        public: Val SetForm(Val form) { return m_form = form; }

    // For inline
    protected: Frame* m_pFrame;
        public: Frame* GetFrame() const  { return m_pFrame; }
        public: Frame* SetFrame(Frame* p){ return m_pFrame = p; }

    protected: bool m_fNotInline;
        public: bool IsNotInline() const  { return m_fNotInline; }
        public: bool SetNotInline(bool f) { return m_fNotInline = f; }
}; // CallInsn


//////////////////////////////////////////////////////////////////////
//
// CLOSE Instruction
//
class CloseInsn : public One_Operand_Instruction_<CloseInsn>
{
    THIS_IS_IR_INSTRUCTION(CLOSE, "CLOSE %frame")

    public: virtual bool IsUseless() const;

    public: CloseInsn(Frame* pFrame) :
        One_Operand_Instruction(pFrame) {}
}; // CloseInsn


//////////////////////////////////////////////////////////////////////
//
// CLOSURE instruction
//
class ClosureInsn : public Two_Operands_Instruction_<ClosureInsn>
{
    THIS_IS_IR_INSTRUCTION(CLOSURE, "CLOSURE ty %rd <- templ %vx")

    public: virtual Operand* SimplifyOutputAux() const;
    public: ClosureInsn(Val, Register*, Function*, Values*);
}; // ClosureInsn


//////////////////////////////////////////////////////////////////////
//
// COUNT Instruction
//
class CountInsn : public One_Operand_Instruction_<CountInsn>
{
    THIS_IS_IR_INSTRUCTION(COUNT, "COUNT fixnum %rd <- %vx")

    public: virtual Operand* SimplifyOutputAux() const;

    public: CountInsn(Register* pRd, Values* pVx) :
        One_Operand_Instruction(pVx, ty_fixnum, pRd) {}
}; // CountInsn


//////////////////////////////////////////////////////////////////////
//
// FRAME instruction
//
//  This instruction is introduced by cg-Select for expanding OPENxxx
//  and CLOSE.
//
class FrameInsn : public Two_Operands_Instruction_<FrameInsn>
{
    THIS_IS_IR_INSTRUCTION(FRAME, "FRAME ty %rd <- %rx %ry")

    public: FrameInsn(Ty ty, Register* pRd, Frame* pFrame, int ofs) :
        Two_Operands_Instruction(pFrame, NewInteger(ofs), ty, pRd) {}
}; // FrameInsn


//////////////////////////////////////////////////////////////////////
//
// ELT instruction
//
class EltInsn :
    public Two_Operands_Instruction_<EltInsn>
{
    THIS_IS_IR_INSTRUCTION(ELT, "ELT ty %rd <- %array %index")

    public: EltInsn(Ty ty, Register* pRd, Operand* pArray, Operand* pIndex) :
        Two_Operands_Instruction(pArray, pIndex, ty, pRd) {}
}; // EltInsn


//////////////////////////////////////////////////////////////////////
//
// ENTRY Instruction
//
class EntryInsn : public One_Operand_Instruction_<EntryInsn>
{
    THIS_IS_IR_INSTRUCTION(ENTRY, "ENTRY ty %vd <- check")

    public: virtual bool Verify() const;

    public: EntryInsn(Ty ty, Values* pVd, Literal* pCheck) :
        One_Operand_Instruction(pCheck, ty, pVd) {}
}; // EntryInsn


//////////////////////////////////////////////////////////////////////
//
// EQ instruction
//  EQ bool %bd <- %rx %sy
//
class EqInsn : public Two_Operands_Instruction_<EqInsn>
{
    THIS_IS_IR_INSTRUCTION(EQ, "EQ bool %bd <- %sx %sy")

    public: virtual bool Simplify();
    public: virtual Operand* SimplifyOutputAux() const;

    public: virtual Instruction* Clone() const
        { return new EqInsn(new Bool(), GetSx(), GetSy()); }

    public: EqInsn(Bool* pBool, Operand* pSx, Operand* pSy) :
        Two_Operands_Instruction(pSx, pSy, ty_bool, pBool) {}
}; // EqInsn


//////////////////////////////////////////////////////////////////////
//
// EXIT Instruction
//
class ExitInsn : public No_Operand_Instruction_<ExitInsn, TerminalInsnFamily>
{
    THIS_IS_IR_INSTRUCTION(EXIT, "EXIT")
}; // ExitInsn


//////////////////////////////////////////////////////////////////////
//
// GE instruction
//  GE bool %bd <- %rx %sy
//
class GeInsn : public Two_Operands_Instruction_<GeInsn>
{
    THIS_IS_IR_INSTRUCTION(GE, "GE bool %bd <- %sx %sy")

    public: virtual Operand* SimplifyOutputAux() const;

    public: virtual Instruction* Clone() const
        { return new GeInsn(new Bool(), GetSx(), GetSy()); }

    public: GeInsn(Bool* pBool, Operand* pRx, Operand* pRy) :
        Two_Operands_Instruction(pRx, pRy, ty_bool, pBool) {}
}; // GeInsn


//////////////////////////////////////////////////////////////////////
//
// GO Instruction
//
class GoInsn : public One_Operand_Instruction_<GoInsn>
{
    THIS_IS_IR_INSTRUCTION(GO, "GO %rx")

    public: GoInsn(Register* pRx, Frame* pFrame) :
        One_Operand_Instruction(pRx),
        m_pFrame(pFrame) {}

    // For inline
    protected: Frame* m_pFrame;
        public: Frame* GetFrame() const  { return m_pFrame; }
        public: Frame* SetFrame(Frame* p){ return m_pFrame = p; }
}; // GoInsn


//////////////////////////////////////////////////////////////////////
//
// GT instruction
//  GT bool %bd <- %rx %sy
//
class GtInsn : public Two_Operands_Instruction_<GtInsn>
{
    THIS_IS_IR_INSTRUCTION(GT, "GT bool %bd <- %sx %sy")

    public: virtual Operand* SimplifyOutputAux() const;

    public: virtual Instruction* Clone() const
        { return new GtInsn(new Bool(), GetSx(), GetSy()); }

    public: GtInsn(Bool* pBool, Operand* pRx, Operand* pSy) :
        Two_Operands_Instruction(pRx, pSy, ty_bool, pBool) {}
}; // GtInsn


//////////////////////////////////////////////////////////////////////
//
// JUMP Instruction
//
class JumpInsn :
    public One_Operand_Instruction_<JumpInsn, TerminalInsnFamily>
{
    THIS_IS_IR_INSTRUCTION(JUMP, "JUMP label")

    public: BBlock* GetTarget() const
        { return GetSx()->StaticCast<Label>()->GetBBlock(); }

    public: JumpInsn(Label* pLabel) :
        One_Operand_Instruction(pLabel) {}

    public: JumpInsn(BBlock*);
}; // JumpInsn


//////////////////////////////////////////////////////////////////////
//
// KEYSUPPLIED instruction
//
class KeySuppliedInsn : public Two_Operands_Instruction_<KeySuppliedInsn>
{
    THIS_IS_IR_INSTRUCTION(KEYSUPPLIED, "KEYSUPPLIED bool %bd <- %vx key")

    public: virtual Instruction* Clone() const
        { return new KeySuppliedInsn(new Bool(), GetRx(), GetLy()); }

    public: KeySuppliedInsn(Bool*, Register*, Val);
}; // KeySuppliedInsn


//////////////////////////////////////////////////////////////////////
//
// KEYVAL instruction
//
class KeyValInsn : public Two_Operands_Instruction_<KeyValInsn>
{
    THIS_IS_IR_INSTRUCTION(KEYVAL, "KEYVAL ty %rd <- %vx key")

    public: KeyValInsn(Register*, Values*, Val);
}; // KeyValInsn


//////////////////////////////////////////////////////////////////////
//
// LE instruction
//
class LeInsn : public Two_Operands_Instruction_<LeInsn>
{
    THIS_IS_IR_INSTRUCTION(LE, "LE bool %bd <- %sx %sy")

    public: virtual Operand* SimplifyOutputAux() const;

    public: virtual Instruction* Clone() const
        { return new LeInsn(new Bool(), GetSx(), GetSy()); }

    public: LeInsn(Bool* pBool, Operand* pRx, Operand* pSy) :
        Two_Operands_Instruction(pRx, pSy, ty_bool, pBool) {}
}; // LeInsn


//////////////////////////////////////////////////////////////////////
//
// LOAD Instruction
//  LOAD ty %rd <- %rx
//
class LoadInsn : public One_Operand_Instruction_<LoadInsn>
{
    THIS_IS_IR_INSTRUCTION(LOAD, "LOAD ty %rd <- %rx")

    public: LoadInsn(Register*, Register*);
}; // LoadInsn


//////////////////////////////////////////////////////////////////////
//
// LOADTIMEVALUE Instruction
//  LOADTIMEVALUE ty %rd <-
//
class LoadTimeValueInsn : public Two_Operands_Instruction_<LoadTimeValueInsn>
{
    THIS_IS_IR_INSTRUCTION(LOADTIMEVALUE, "LOADTIMEVALUE ty %rd <- cookie")

    protected: Val m_read_only_p;

    public: LoadTimeValueInsn(
            Register*   pRd,
            Val         cookie,
            Function*   pFun,
            Val         read_only_p );
}; // LoadTimeValueInsn


//////////////////////////////////////////////////////////////////////
//
// LT instruction
//  LT bool %bd <- %rx %sy
//
class LtInsn : public Two_Operands_Instruction_<LtInsn>
{
    THIS_IS_IR_INSTRUCTION(LT, "LT bool %bd <- %rx %sy")

    public: virtual Operand* SimplifyOutputAux() const;

    public: virtual Instruction* Clone() const
        { return new LtInsn(new Bool(), GetSx(), GetSy()); }

    public: LtInsn(Bool* pBool, Operand* pRx, Operand* pSy) :
        Two_Operands_Instruction(pRx, pSy, ty_bool, pBool) {}
}; // LtInsn


//////////////////////////////////////////////////////////////////////
//
// MVRESTORE instruction
//
class MvRestoreInsn : public One_Operand_Instruction_<MvRestoreInsn>
{
    THIS_IS_IR_INSTRUCTION(MVRESTORE, "MVRESTORE ty %vd <- %rx")

    public: virtual Operand* SimplifyOutputAux() const;

    public: MvRestoreInsn(Ty ty, Values* pVd, Register* pRx) :
        One_Operand_Instruction(pRx, ty, pVd) {}
}; // MvRestoreInsn


//////////////////////////////////////////////////////////////////////
//
// MVSAVE Instruction
//
class MvSaveInsn : public One_Operand_Instruction_<MvSaveInsn>
{
    THIS_IS_IR_INSTRUCTION(MVSAVE, "MVSAVE ty %rd <- %vx")

    public: MvSaveInsn(Register* pRd, Values* pVx) :
        One_Operand_Instruction(pVx, ty_t, pRd) {}
}; // MvSaveInsn


//////////////////////////////////////////////////////////////////////
//
// NEG Instruction
//  For float negation.
//
class NegInsn : public One_Operand_Instruction_<NegInsn>
{
    THIS_IS_IR_INSTRUCTION(NEG, "NEG ty %rd <- %rx")

    public: NegInsn(Ty ty, Register* pRd, Register* pRx) :
        One_Operand_Instruction(pRx, ty, pRd) {}
}; // NegInsn


//////////////////////////////////////////////////////////////////////
//
// NONLOCAL Instruction
//
class NonlocalInsn : public No_Operand_Instruction_<NonlocalInsn>
{
    THIS_IS_IR_INSTRUCTION(NONLOCAL, "NONLOCAL ty %sd <-")

    NonlocalInsn(Ty ty, Output* pSd) :
        No_Operand_Instruction(ty, pSd) {}
}; // NonlocalInsn


//////////////////////////////////////////////////////////////////////
//
// NE instruction
//  NE bool %bd <- %rx %sy
//
class NeInsn : public Two_Operands_Instruction_<NeInsn>
{
    THIS_IS_IR_INSTRUCTION(NE, "NE bool %bd <- %rx %sy")

    public: virtual bool Simplify();
    public: virtual Operand* SimplifyOutputAux() const;

    public: virtual Instruction* Clone() const
        { return new NeInsn(new Bool(), GetSx(), GetSy()); }

    public: NeInsn(Bool* pBool, Operand* pSx, Operand* pSy) :
        Two_Operands_Instruction(pSx, pSy, ty_bool, pBool) {}
}; // NeInsn


//////////////////////////////////////////////////////////////////////
//
// NTHVALUE instruction
//
class NthValueInsn : public Two_Operands_Instruction_<NthValueInsn>
{
    THIS_IS_IR_INSTRUCTION(NTHVALUE, "NTHVALUE ty %rd <- %sx %vy")

    public: NthValueInsn(Ty ty, Register* pRd, Operand* pNth, Values* pVy) :
        Two_Operands_Instruction(pNth, pVy, ty, pRd) {}
}; // NthValueInsn


//////////////////////////////////////////////////////////////////////
//
// OPENBIND Instruction
//
class OpenBindInsn : public Instruction
{
    THIS_IS_IR_INSTRUCTION(OPENBIND, "OPENBIND t %frame <- (sym %sx)+")

    public: virtual void HtmlPrint(Val, bool) const;

    // OpenBindOperandBox
    class OpenBindOperandBox : public OperandBox
    {
        protected: Val m_cell;
        public: OpenBindOperandBox() : m_cell(nil) {}
        public: Val GetCell() const   { return m_cell; }
        public: Val SetCell(Val cell) { return m_cell = cell; }

        public: Val GetSymbol() const
        {
            if (value_cell_p(m_cell))
            {
                return m_cell->Decode<ValueCell>()->m_name;
            }

            if (tlv_record_p(m_cell))
            {
                return m_cell->Decode<TlvRecord>()->m_name;
            }

            return nil;
        } // GetSymbol
    }; // OpenBindOperandBox

    protected: uint                 m_cAllocs;
    protected: uint                 m_cOperands;
    protected: OpenBindOperandBox** m_prgpOperandBox;

    public: virtual uint GetOperandCount() const
        { return m_cOperands; }

    public: virtual OperandBox* GetOperandBox(uint n) const
        { ASSERT(n < m_cOperands); return m_prgpOperandBox[n]; }

    // OpenBindInsn
    public: OpenBindInsn(Frame* pFrame) :
        m_cAllocs(0),
        m_cOperands(0),
        m_prgpOperandBox(NULL)
    {
        m_ty = Qt;
        m_pOutput = pFrame;
    } // OpenBindInsn

    ////////////////////////////////////////////////////////////
    //
    // Input Operand releated
    //
    public: Operand* AddBind(Val, Operand*);

    public: void RemoveBind(Val);

    public: class EnumInput
    {
        protected: OpenBindOperandBox** m_pRunner;
        protected: OpenBindOperandBox** m_pEnd;

        public: EnumInput(OpenBindInsn* pOpenBind)
            { Reset(pOpenBind); }

        public: EnumInput(const OpenBindInsn* pOpenBind)
            { Reset(const_cast<OpenBindInsn*>(pOpenBind)); }

        public: void Reset(OpenBindInsn* pOpenBind)
        {
            m_pRunner = pOpenBind->m_prgpOperandBox;
            m_pEnd    = pOpenBind->m_prgpOperandBox + pOpenBind->m_cOperands;
        } // Reset

        public: bool AtEnd() const
            { return m_pRunner == m_pEnd; }

        public: Operand* Get() const
            { return GetBox()->GetOperand(); }

        public: OpenBindOperandBox* GetBox() const
            { ASSERT(! AtEnd()); return *m_pRunner; }

        public: void Next()
            { ASSERT(! AtEnd()); m_pRunner++; }
    }; // EnumInput
}; // OpenBindInsn


//////////////////////////////////////////////////////////////////////
//
// OPENBLOCK instruction
//
class OpenBlockInsn : public Two_Operands_Instruction_<OpenBlockInsn>
{
    THIS_IS_IR_INSTRUCTION(OPENBLOCK, "OPENBLOCK %frame <- %sx label")

    public: virtual bool IsUseless() const;

    public: OpenBlockInsn(Frame*, Literal*, BBlock*);
}; // OpenBlockInsn


//////////////////////////////////////////////////////////////////////
//
// OPENCATCH instruction
//
class OpenCatchInsn : public Two_Operands_Instruction_<OpenCatchInsn>
{
    THIS_IS_IR_INSTRUCTION(OPENCATCH, "OPENCATCH %frame <- %sx label")

    public: OpenCatchInsn(Frame*, Operand*, BBlock*);
}; // OpenCatchInsn


//////////////////////////////////////////////////////////////////////
//
// OPENFINALLY instruction
//
class OpenFinallyInsn : public Two_Operands_Instruction_<OpenFinallyInsn>
{
    THIS_IS_IR_INSTRUCTION(OPENFINALLY, "OPENFINALLY %frame <- fn %vx")

    public: OpenFinallyInsn(Frame*, Function*, Values*);
}; // OpenFinallyInsn


//////////////////////////////////////////////////////////////////////
//
// OPENSIMPLE instruction
//
class OpenSimpleI : public One_Operand_Instruction_<OpenSimpleI>
{
    THIS_IS_IR_INSTRUCTION(OPENSIMPLE, "OPENSIMPLE %frame <- code")

    public: OpenSimpleI(Frame* pFrame, int iCode) :
        One_Operand_Instruction(new Integer(iCode))
    {
        m_ty      = Qt;
        m_pOutput = pFrame;
    } // OpenSimpleI
}; // OpenSimpleInsn


//////////////////////////////////////////////////////////////////////
//
// OPENTAGBODY Instruction
//
class OpenTagbodyInsn :
    public No_Operand_Instruction_<OpenTagbodyInsn>
{
    THIS_IS_IR_INSTRUCTION(OPENTAGBODY, "OPENTAGBODY %frame <-")

    public: virtual bool IsUseless() const
        { return 0 == m_pOutput->StaticCast<Frame>()->GetCount(); }

    public: OpenTagbodyInsn(Frame* pFrame) :
        No_Operand_Instruction()
    {
        m_ty = ty_t;
        m_pOutput = pFrame;
    } // OpenTagbodyInsn
}; // OpenTagbodyInsn


//////////////////////////////////////////////////////////////////////
//
// PARSEKEYS instruction
//
class ParseKeysInsn : public Three_Operands_Instruction_<ParseKeysInsn>
{
    THIS_IS_IR_INSTRUCTION(PARSEKEYS, "PARSEKEYS %keys <- %rx allow keys")

    public: ParseKeysInsn(Values*, Register*, Val, Val);
}; // ParseKeysInsn


//////////////////////////////////////////////////////////////////////
//
// PHI Instruction
//
class PhiInsn : public Instruction
{
    THIS_IS_IR_INSTRUCTION(PHI, "PHI ty %rd <- (bblock %sx)+")

    public: virtual bool IsUseless() const;
    public: virtual bool Simplify();
    public: virtual Operand* SimplifyOutputAux() const;
    public: virtual bool UpdateTy();
    public: virtual bool Verify() const;

    // PhiOperandBox
    class PhiOperandBox : public OperandBox
    {
        protected: Label* m_pLabel;
        public: PhiOperandBox() : m_pLabel(NULL) {}
        public: BBlock* GetBBlock() const;
        public: BBlock* SetBBlock(BBlock*);
    }; // PhiOperandBox

    protected: uint             m_cAllocs;
    protected: uint             m_cOperands;
    protected: PhiOperandBox**  m_prgpOperandBox;

    public: virtual uint GetOperandCount() const
        { return m_cOperands; }

    public: virtual OperandBox* GetOperandBox(uint n) const
        { ASSERT(n < m_cOperands); return m_prgpOperandBox[n]; }

    // PhiInsn
    public: PhiInsn(Ty ty, Output* pRd) :
        m_cAllocs(0),
        m_cOperands(0),
        m_prgpOperandBox(NULL),
        Instruction(ty, pRd) {}

    public: PhiInsn(Ty, Output*, const PhiInsn*);

    ////////////////////////////////////////////////////////////
    //
    // Input Operand releated
    //
    public: Operand* AddInput(BBlock*, Operand*);

    public: Operand* GetInput(BBlock* p) const
        { return GetInputBox(p)->GetOperand(); }

    public: PhiOperandBox* GetInputBox(BBlock*) const;
    public: PhiOperandBox* GetInputBox(OperandBox*) const;

    public: void RemoveInput(BBlock*);
    public: void SortInput();

    public: class EnumInput
    {
        protected: PhiOperandBox** m_pRunner;
        protected: PhiOperandBox** m_pEnd;

        public: EnumInput(PhiInsn* pPhi)
            { Reset(pPhi); }

        public: EnumInput(const PhiInsn* pPhi)
            { Reset(const_cast<PhiInsn*>(pPhi)); }

        public: void Reset(PhiInsn* pPhi)
        {
            m_pRunner = pPhi->m_prgpOperandBox;
            m_pEnd    = pPhi->m_prgpOperandBox + pPhi->m_cOperands;
        } // Reset

        public: bool AtEnd() const
            { return m_pRunner == m_pEnd; }

        public: Operand* Get() const
            { return GetBox()->GetOperand(); }

        public: PhiOperandBox* GetBox() const
            { ASSERT(! AtEnd()); return *m_pRunner; }

        public: void Next()
            { ASSERT(! AtEnd()); m_pRunner++; }
    }; // EnumInput

    ////////////////////////////////////////////////////////////
    //
    // Printer
    //
    public: virtual void HtmlPrint(Val, bool) const;
}; // PhiInsn


//////////////////////////////////////////////////////////////////////
//
// PROJECT instruction
//  PROJECT ty %rd <- %vx nth
//
class ProjectInsn : public Two_Operands_Instruction_<ProjectInsn>
{
    THIS_IS_IR_INSTRUCTION(PROJECT, "PROJECT ty %rd <- %vx nth")

    public: virtual Operand* SimplifyOutputAux() const;
    public: virtual bool Verify() const;

    public: ProjectInsn(
        Val         ty,
        Register*   pRd,
        Values*     pVx,
        uint        nNth ) :
            Two_Operands_Instruction(
                pVx, NewLiteral(Fixnum::Encode(nNth)), ty, pRd ) {}
}; // ProjectInsn


//////////////////////////////////////////////////////////////////////
//
// PROLOGUE instruction
//  PROLOGUE ty %vd <- %vx rest
//
class PrologueInsn : public Two_Operands_Instruction_<PrologueInsn>
{
    THIS_IS_IR_INSTRUCTION(PROLOGUE, "PROLOGUE ty %vd <- %vx rest")

    public: virtual bool IsPinned() const { return true; }
    public: virtual bool Verify() const;

    public: PrologueInsn(
        Val         ty,
        Values*     pVd,
        Values*     pVx,
        Literal*    pRest ) :
            Two_Operands_Instruction(pVx, pRest, ty, pVd) {}
}; // PrologueInsn


//////////////////////////////////////////////////////////////////////
//
// RET Instruction
//  RET %sx
//
class RetInsn : public One_Operand_Instruction_<RetInsn, TerminalInsnFamily>
{
    THIS_IS_IR_INSTRUCTION(RET, "RET %sx")

    public: virtual void Realize();
    public: virtual void Unrealize();

    public: RetInsn(Operand* pSx) : One_Operand_Instruction(pSx) {}
}; // RetInsn


//////////////////////////////////////////////////////////////////////
//
// RETURNFROM instruction
//
class ReturnFromInsn : public Two_Operands_Instruction_<ReturnFromInsn>
{
    THIS_IS_IR_INSTRUCTION(RETURNFROM, "RETURNFROM %rx %vy")

    public: ReturnFromInsn(Register* pRx, Values* pVy, Frame* pFrame) :
        Two_Operands_Instruction(pRx, pVy),
        m_pFrame(pFrame) {}

    // For inline
    protected: Frame* m_pFrame;
        public: Frame* xGetFrame() const  { return m_pFrame; }
        public: Frame* xSetFrame(Frame* p){ return m_pFrame = p; }
}; // ReturnFromInsn


//////////////////////////////////////////////////////////////////////
//
// RuntimeCastInsn
//
class RuntimeCastInsn : public Two_Operands_Instruction_<RuntimeCastInsn>
{
    THIS_IS_IR_INSTRUCTION(RUNTIMECAST, "RUNTIMECAST ty %rd <- %rx")

    public: virtual Operand* SimplifyOutputAux() const
    {
        Register* pRx = GetRx();
        if (NULL == pRx) return GetSx();
        Ty ty = pRx->GetTy();
        if (Subtypep_Yes == ty_subtypep(ty, m_ty)) return pRx;
        return m_pOutput;
    } // SimplifyOutputAux

#if 0
    virtual bool UpdateTy()
    {
        Ty ty = GetSx()->GetTy();
        if (Subtypep_Yes == ty_subtypep(ty, m_ty))
            { m_ty = ty; return true; }
        return false;
    } // UpdateTy
#endif

#if 0
    public: virtual bool UpdateTy()
    {
        Ty ty = ty_and(GetLy(), GetSx()->GetTy());
        if (ty_equal(ty, m_ty)) return false;
        m_ty = ty;
        return true;
    } // UpdateTy
#endif

    // For expand into CALL
    Frame* m_pFrame;
        public: Frame* GetFrame() const  { return m_pFrame; }

    // constructor
    public: RuntimeCastInsn(
        Val         ty,
        Register*   pRd,
        Register*   pRx,
        Frame*      pFrame,
        uint        rgfAttr = 0 ) :
            Two_Operands_Instruction(pRx, NewLiteral(ty), ty, pRd, rgfAttr),
            m_pFrame(pFrame)
    {
        if (consp(ty) && Qvalues == first(ty))
        {
            warn(L"RuntimeCast doesn't accept ~S.", ty);
        }
    } // RuntimeCastInsn
}; // RuntimeCastInsn


//////////////////////////////////////////////////////////////////////
//
// SELECT instruction
//
class SelectInsn : public Three_Operands_Instruction_<SelectInsn>
{
    THIS_IS_IR_INSTRUCTION(SELECT, "SELECT ty %rd <- %bx %sy %sz")

    public: virtual bool Simplify();
    public: virtual Operand* SimplifyOutputAux() const;
    public: virtual bool UpdateTy();

    public: SelectInsn(
        Output*     pRd,
        Bool*       pBx,
        Operand*    pSy,
        Operand*    pSz ) :
            Three_Operands_Instruction(pBx, pSy, pSz, ty_t, pRd)
    {
        UpdateTy();
    } // SelectInsn
}; // SelectInsn


//////////////////////////////////////////////////////////////////////
//
// SigmaInsn
//
class SigmaInsn : public One_Operand_Instruction_<SigmaInsn>
{
    THIS_IS_IR_INSTRUCTION(SIGMA, "SIGMA ty %rd <- %sx")

    virtual Operand* SimplifyOutputAux() const
    {
        Register* pRx = GetRx();
        if (NULL == pRx) return GetSx();
        if (Subtypep_Yes == ty_subtypep(pRx->GetTy(), GetTy())) return pRx;
        return m_pOutput;
    } // SimplifyOutputAux

    virtual bool UpdateTy()
    {
        Ty ty = ty_and(GetTy(), GetSx()->GetTy());
        if (ty_equal(ty, GetTy())) return false;
        m_ty = ty;
        return true;
    } // UpdateTy

    // constructors
    public: SigmaInsn(Register* pRd, Register* pRx) :
        One_Operand_Instruction(pRx, pRx->GetTy(), pRd) {}

    public: SigmaInsn(Val ty, Register* pRd, Register* pRx) :
        One_Operand_Instruction(pRx, ty, pRd) {}
}; // SigmaInsn


//////////////////////////////////////////////////////////////////////
//
// SLOT instruction
//  SLOT ty %rd <- class slot-name %sz
//
class SlotInsn : public Three_Operands_Instruction_<SlotInsn>
{
    THIS_IS_IR_INSTRUCTION(SLOT, "SLOT ty %rd <- class slot-name %sz")

    public: SlotInsn(
        Val         ty,
        Output*     pRd,
        Literal*    pClass,
        Literal*    pSlotName,
        Operand*    pSz ) :
            Three_Operands_Instruction(pClass, pSlotName, pSz, ty, pRd) {}
}; // SlotInsn


//////////////////////////////////////////////////////////////////////
//
// STORE instruction
//  STORE %rx %sy
//      %rx     register or effective address
//      %sy     register, literal or integer
//
class StoreInsn : public Two_Operands_Instruction_<StoreInsn>
{
    THIS_IS_IR_INSTRUCTION(STORE, "STORE %rx %sy")

    public: Ty m_opty;

    public: StoreInsn(Ty opty, Register* pRx, Operand* pSy) :
        m_opty(opty),
        Two_Operands_Instruction(pRx, pSy) {}

    public: StoreInsn(Register* pRx, Operand* pSy) :
        m_opty(pRx->GetTy()),
        Two_Operands_Instruction(pRx, pSy) {}
}; // StoreInsn


//////////////////////////////////////////////////////////////////////
//
// TAG instruction
//
class TagInsn : public Two_Operands_Instruction_<TagInsn>
{
    THIS_IS_IR_INSTRUCTION(TAG, "TAG ty %rd <- %frame label")

    public: TagInsn(Register*, Frame*, BBlock*);

    public: void Realize()
    {
        GetSx()->StaticCast<Frame>()->IncCount();
        Two_Operands_Instruction::Realize();
    } // Realize

    public: void Unrealize()
    {
        GetSx()->StaticCast<Frame>()->DecCount();
        Two_Operands_Instruction::Unrealize();
    } // Unrealize
}; // TagInsn


//////////////////////////////////////////////////////////////////////
//
// TBLJMP Instruction
//
class TblJmpInsn : public Instruction
{
    THIS_IS_IR_INSTRUCTION(TBLJMP, "TBLJMP %sx label+")

    protected: OperandBox** m_prgpOperandBox;
    protected: uint         m_cOperands;
    protected: uint         m_cAllocs;

    public: virtual uint GetOperandCount() const { return m_cOperands; }

    public: virtual OperandBox* GetOperandBox(uint nIndex) const
    {
        ASSERT(nIndex < m_cOperands);
        return m_prgpOperandBox[nIndex];
    } // GetOperandBox

    public: TblJmpInsn(
        Register*   pRx,
        BBlock*     prgpBBlock[],
        uint        cBBlocks );
}; // TblJmpInsn


//////////////////////////////////////////////////////////////////////
//
// THROW instruction
//
class ThrowInsn : public Two_Operands_Instruction_<ThrowInsn>
{
    THIS_IS_IR_INSTRUCTION(THROW, "THROW %sx %vy")

    public: ThrowInsn(Operand* pSx, Values* pVy) :
        Two_Operands_Instruction(pSx, pVy) {}
}; // ThrowInsn


//////////////////////////////////////////////////////////////////////
//
// TLV Instruction
//
class TlvInsn : public One_Operand_Instruction_<TlvInsn>
{
    THIS_IS_IR_INSTRUCTION(TLV, "TLV ty %rd <- tlvrec")

    public: TlvInsn(Ty ty, Register* pRd, Val tlvrec) :
        One_Operand_Instruction(NewLiteral(tlvrec), ty, pRd)
            { ASSERT(tlv_record_p(tlvrec)); }
}; // TlvInsn


//////////////////////////////////////////////////////////////////////
//
// TRAPIF instruction
//
class TrapIfInsn : public Three_Operands_Instruction_<TrapIfInsn>
{
    THIS_IS_IR_INSTRUCTION(TRAPIF, "TRAPIF %bx name %vz")

    public: virtual bool Simplify() { return simplifyBx(); }

    public: TrapIfInsn(Bool* pBx, Val name, Values* pVz) :
        Three_Operands_Instruction(pBx, NewLiteral(name), pVz) {}

    // For expand into CALL
    protected: Frame* m_pFrame;
        public: Frame* GetFrame() const  { return m_pFrame; }
        public: Frame* SetFrame(Frame* p){ return m_pFrame = p; }
}; // TrapIfInsn


//////////////////////////////////////////////////////////////////////
//
// TRAPIFNOT instruction
//
class TrapIfNotInsn : public TrapIfInsn
{
    public: TrapIfNotInsn(Bool* pBx, Val name, Values* pVz) :
        TrapIfInsn(pBx, name, pVz) {}

}; // TrapIfNotInsn


//////////////////////////////////////////////////////////////////////
//
// TYPEP instruction
//
class TypepInsn : public Two_Operands_Instruction_<TypepInsn>
{
    THIS_IS_IR_INSTRUCTION(TYPEP, "TYPEP bool %bd <- %sx type")

    public: virtual Instruction* Clone() const
        { return new TypepInsn(new Bool(), GetSx(), GetLy()); }

    public: virtual Operand* SimplifyOutputAux() const;

    public: TypepInsn(Bool* pBd, Operand* pSx, Ty ty) :
        Two_Operands_Instruction(pSx, NewLiteral(ty), ty_bool, pBd)
    {
        if (consp(ty) && Qvalues == first(ty))
        {
            warn(L"Typep doesn't accept ~S.", ty);
        }
    } // TypepInsn
}; // TypepInsn


//////////////////////////////////////////////////////////////////////
//
// UNBOX Instruction
//
class UnboxInsn : public One_Operand_Instruction_<UnboxInsn>
{
    THIS_IS_IR_INSTRUCTION(UNBOX, "UNBOX ty %rd <- %sx")

    // constructors
    public: UnboxInsn(Val ty, Register* pRd, Operand* pRx) :
        One_Operand_Instruction(pRx, ty, pRd) {}
}; // UnboxInsn


//////////////////////////////////////////////////////////////////////
//
// UNREACHABLE Instruction
//
class UnreachableInsn :
    public No_Operand_Instruction_<UnreachableInsn, TerminalInsnFamily>
{
    THIS_IS_IR_INSTRUCTION(UNREACHABLE, "UNREACHABLE")

    public: virtual void Realize();
    public: virtual void Unrealize();
}; // UnreachableInsn


//////////////////////////////////////////////////////////////////////
//
// UPVARDEF Instruction
//
class UpVarDefInsn : public One_Operand_Instruction_<UpVarDefInsn>
{
    THIS_IS_IR_INSTRUCTION(UPVARDEF, "UPVARDEF ty %rd <- var")

    public: virtual bool IsUseless() const
    {
        if (ty_void == m_ty) return false;
        if (GetOutput()->HasUseSite()) return false;

        Variable* pVar = GetSx()->StaticCast<Variable>();
        return 1 == pVar->GetUpVarCount();
    } // IsUseless

    public: UpVarDefInsn(Register* pRd, Variable* pVar) :
        One_Operand_Instruction(pVar, ty_closed_cell, pRd)
            { pRd->SetVar(pVar); }

    public: UpVarDefInsn(Ty ty, Register* pRd, Variable* pVar) :
        One_Operand_Instruction(pVar, ty, pRd)
            { pRd->SetVar(pVar); }
}; // UpVarDefInsn


//////////////////////////////////////////////////////////////////////
//
// USE Instruction
//
class UseInsn : public One_Operand_Instruction_<UseInsn>
{
    THIS_IS_IR_INSTRUCTION(USE, "USE %sx")

    public: virtual bool IsUseless() const
        { return NULL == GetSx()->ToOutput()->GetDfn(); }

    public: UseInsn(Output* pSx) :
        One_Operand_Instruction(pSx) {}
}; // UseInsn


//////////////////////////////////////////////////////////////////////
//
// VALUES instruction
//  VALUES ty %vd <- %sx...
//
class ValuesInsn : public Instruction
{
    THIS_IS_IR_INSTRUCTION(VALUES, "VALUES ty %vd <- %sx*")

    protected: OperandBox** m_prgpOperandBox;
    protected: uint         m_cOperands;
    protected: uint         m_cAllocs;

    public: virtual uint GetOperandCount() const { return m_cOperands; }

    public: virtual OperandBox* GetOperandBox(uint nIndex) const
    {
        ASSERT(nIndex < m_cOperands);
        return m_prgpOperandBox[nIndex];
    } // GetOperandBox

    public: virtual bool UpdateTy();

    public: Operand* InsertBefore(Operand*, uint);

    public: ValuesInsn(Values*, Operand*, Operand*);

    public: ValuesInsn(Values* d, Operand* v[] = NULL, uint c = 0)
        { init(d, v, c); }

    public: ValuesInsn(Values* d, Operand* s, uint c = 1)
        { init(d, s, c); }

    //  For x86-SELECT, lowering VALUES*
    public: ValuesInsn(Values* d , Instruction* s , uint c = 0)
        { init(d, s, c); }

    protected: ValuesInsn::ValuesInsn() {}
    protected: void init(Values*, Operand*[], uint);
    protected: void init(Values*, Operand*, uint = 1);
    protected: void init(Values*, Instruction*, uint = 0);
}; // ValuesInsn


//////////////////////////////////////////////////////////////////////
//
// VALUESA instruction
//  VALUESA ty %vd <- %sx...
//
class ValuesAInsn : public ValuesInsn
{
    THIS_IS_IR_INSTRUCTION2(VALUESA, "VALUES* ty %vd <- %sx*", "VALUES*")

    public: virtual Operand* SimplifyOutputAux() const;
    public: virtual bool UpdateTy();

    public: ValuesAInsn(Values* d, Operand* v[], uint c = 0)
        { init(d, v, c); }

    public: ValuesAInsn(Values* d, Operand* s)
        { init(d, s); }

    //  For ir_split_closure
    public: ValuesAInsn(Values* d, Instruction* s)
        { init(d, s); }
}; // ValuesAInsn


//////////////////////////////////////////////////////////////////////
//
// VARDEF instruction
//
class VarDefInsn : public Two_Operands_Instruction_<VarDefInsn>
{
    THIS_IS_IR_INSTRUCTION(VARDEF, "VARDEF ty %rd <- var %sx")

    public: virtual bool IsUseless() const
    {
        return ! m_pOutput->HasUseSite() &&
               0 == GetSx()->StaticCast<Variable>()->GetUpVarCount();
    } // IsUseless

    public: VarDefInsn(Register*, Variable*, Operand*);
}; // VarDefInsn


Instruction* newEqInsn(Bool*, Operand*, Operand*);
Instruction* newGeInsn(Bool*, Operand*, Operand*);
Instruction* newGtInsn(Bool*, Operand*, Operand*);
Instruction* newLeInsn(Bool*, Operand*, Operand*);
Instruction* newLtInsn(Bool*, Operand*, Operand*);
Instruction* newNeInsn(Bool*, Operand*, Operand*);

} // Compiler

#endif //!defined(INCLUDE_compiler_ir_instruction_h)
