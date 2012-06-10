//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - instruction
// cg/cg_instruction.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_instruction.h#5 $
//
//      CopyInsn
//      FrameRefInsn
//      PhiCopyInsn
//      ReloadInsn
//      SpillInsn
//      SpiltInsn
//      StackDefInsn
//      SwapInsn
//      SwapGetInsn
//      TlvNameInsn
//      VarRefInsn
//      VarSlotInsn
//      VecRefInsn
//
#if !defined(INCLUDE_compiler_cg_instruction_h)
#define INCLUDE_compiler_cg_instruction_h

#include "../ir/ir_instruction.h"
#include "../ir/ir_fns.h"
#include "./cg_operand.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// CopyInsn
//
class CopyInsn : public One_Operand_Instruction_<CopyInsn>
{
    THIS_IS_IR_INSTRUCTION(COPY, "COPY ty %rd <= %sx")

    public: virtual bool Identical(const Instruction* that) const
    {
        return this->GetOutput()->Equal(that->GetOutput()) &&
               this->GetSx()->Equal(that->GetSx());
    } // Identical

    public: virtual bool IsUseless() const
        { return m_pOutput == GetSx(); }

    public: CopyInsn(Output* pSd, Operand* pSx) :
        One_Operand_Instruction(pSx, pSx->GetTy(), pSd) {}

    public: CopyInsn(Val ty, Output* pSd, Operand* pSx) :
        One_Operand_Instruction(pSx, ty, pSd) {}
}; // CopyInsn


//////////////////////////////////////////////////////////////////////
//
// FrameRefInsn
//
class FrameRefInsn : public No_Operand_Instruction_<FrameRefInsn>
{
    THIS_IS_IR_INSTRUCTION(FRAMEREF, "FRAMEREF (ptr t) %rd <=")

    public: virtual void HtmlPrint(Val, bool) const;

    protected: Function* m_pFrameOwner;
        public: Function* GetFrameOwner() const
            { return m_pFrameOwner; }

        public: Function* SetFrameOwner(Function* p)
            { return m_pFrameOwner = p; }

    public: FrameRefInsn(Register* pRd, Function* pFrameOwner) :
        No_Operand_Instruction(ty_ptr_t, pRd),
        m_pFrameOwner(pFrameOwner) {}
}; // FrameRefInsn


//////////////////////////////////////////////////////////////////////
//
// PhiCopyInsn
//
class PhiCopyInsn : public CopyInsn
{
    THIS_IS_IR_INSTRUCTION(PHICOPY, "PHICOPY ty %rd <= %sx")

    public: PhiCopyInsn(Ty ty, Register* pRd, Operand* pSx) : 
        CopyInsn(ty, pRd, pSx) {}
}; // PhiCopyInsn


//////////////////////////////////////////////////////////////////////
//
// RELOAD instruction
//
//  This instruction is introduced by Register Allocator when RA
//  doesn't allocate virtual register to physical register.
//
class ReloadInsn : public CopyInsn
{
    THIS_IS_IR_INSTRUCTION(RELOAD, "RELOAD ty %rd <= %sx")

    public: ReloadInsn(Register* pRd, Register* pRx) :
        CopyInsn(pRd, pRx) {}
}; // ReloadInsn


//////////////////////////////////////////////////////////////////////
//
// SPILL instruction
//
//  This instruction is introduced by Register Allocator when RA
//  doesn't allocate virtual register to physical register.
//
class SpillInsn : public CopyInsn
{
    THIS_IS_IR_INSTRUCTION(SPILL, "SPILL ty %rd <= %sx")

    public: SpillInsn(Val ty, Register* pRd, Register* pRx) :
        CopyInsn(ty, pRd, pRx) {}
}; // SpillInsn


//////////////////////////////////////////////////////////////////////
//
// SPLIT instruction
//
//  This instruction is introduced by Register Allocator when RA
//  doesn't allocate virtual register to physical register.
//
class SplitInsn : public CopyInsn //public One_Operand_Instruction_<SplitInsn>
{
    THIS_IS_IR_INSTRUCTION(SPLIT, "SPLIT ty %rd <= %sx")

    public: SplitInsn(Val ty, Register* pRd, Register* pRx) :
        CopyInsn(ty, pRd, pRx) {}
}; // SplitInsn


//////////////////////////////////////////////////////////////////////
//
// STACKDEF instruction
//
//  This instruction is introduced by cg-UpVar for assigning stack
//  slot in function frame.
//
class StackDefInsn : public Two_Operands_Instruction_<StackDefInsn>
{
    THIS_IS_IR_INSTRUCTION(STACKDEF, "STACKDEF ty %rd <= %rx %ry")

    public: StackDefInsn(Ty ty, Register* pRd, Register* pRx, Register* pRy) :
        Two_Operands_Instruction(pRx, pRy, ty, pRd) {}
}; // StackDefInsn


//////////////////////////////////////////////////////////////////////
//
// STACKVEC instruction
//
class StackVecInsn : public Instruction
{
    THIS_IS_IR_INSTRUCTION(STACKVEC, "STACKVEC ty %rd <= %sx+")

    protected: OperandBox** m_prgpOperandBox;
    protected: uint         m_cOperands;

    public: virtual uint GetOperandCount() const { return m_cOperands; }

    public: virtual OperandBox* GetOperandBox(uint nIndex) const
    {
        ASSERT(nIndex < m_cOperands);
        return m_prgpOperandBox[nIndex];
    } // GetOperandBox

    public: StackVecInsn(Register*, Operand* const*, uint);
}; // StackVecInsn


//////////////////////////////////////////////////////////////////////
//
// SWAP instruction
//
class SwapInsn : public Two_Operands_Instruction_<SwapInsn>
{
    THIS_IS_IR_INSTRUCTION(SWAP, "SWAP ty %qd <= %rx %rd")

    public: SwapInsn(
        Pseudo*     pQd,
        Register*   pRx,
        Register*   pRy ) :
            Two_Operands_Instruction(pRx, pRy, ty_int32, pQd) {}
}; // SwapInsn


//////////////////////////////////////////////////////////////////////
//
// SWAPGET instruction
//
class SwapGetInsn : public Two_Operands_Instruction_<SwapGetInsn>
{
    THIS_IS_IR_INSTRUCTION(SWAPGET, "SWAPGET ty %qd <= %rx %rd")

    public: SwapGetInsn(
        Val         ty,
        Register*   pRd,
        Pseudo*     pQx,
        Register*   pRy ) :
            Two_Operands_Instruction(pQx, pRy, ty, pRd) {}
}; // SwapInsn

//////////////////////////////////////////////////////////////////////
//
// TlvNameInsn
//
class TlvNameInsn : public One_Operand_Instruction_<TlvNameInsn>
{
    THIS_IS_IR_INSTRUCTION(TLVNAME, "TLVNAME fixnum %rd <= tlvrec")

    public: TlvNameInsn(Register* pRd, Val tlvrec) :
        One_Operand_Instruction(NewLiteral(tlvrec), ty_fixnum, pRd)
            { ASSERT(tlv_record_p(tlvrec)); }
}; // TlvNameInsn


//////////////////////////////////////////////////////////////////////
//
// VARREF instruction
//
class VarRefInsn : public Two_Operands_Instruction_<VarRefInsn>
{
    THIS_IS_IR_INSTRUCTION(VARREF, "VARREF ty %rd <= %base %var")

    public: VarRefInsn(
        Val         ty,
        Output*     pRd,
        Register*   pBase,
        Register*    pRy ) :
            Two_Operands_Instruction(pBase, pRy, ty, pRd) {}
}; // VarRefInsn


//////////////////////////////////////////////////////////////////////
//
// VARSLOT instruction
//
class VarSlotInsn : public Two_Operands_Instruction_<VarSlotInsn>
{
    THIS_IS_IR_INSTRUCTION(VARSLOT, "VARSLOT ty %rd <= %base %var")

    public: VarSlotInsn(
        Val         ty,
        Output*     pRd,
        Register*   pBase,
        Register*   pRy ) :
            Two_Operands_Instruction(pBase, pRy, ty, pRd) {}
}; // VarSlotInsn


//////////////////////////////////////////////////////////////////////
//
// VECREF instruction
//
//  This instruction is introduced by CG-UPVAR pass for accessing
//  closed variables by base register and offset.
//
//  When user of closed variable is called from closure and non-closure,
//  this instruction is used.
//
class VecRefInsn : public One_Operand_Instruction_<VecRefInsn>
{
    THIS_IS_IR_INSTRUCTION(VECREF, "VECREF (ptr ty) %rd <= %vector nth")

    public: virtual void HtmlPrint(Val, bool) const;

    protected: uint m_nNth;
        public: uint GetNth() const { return m_nNth; }

    public: VecRefInsn(
        Val         ty,
        Output*     pRd,
        Register*   pVector,
        uint        nNth ) :
            One_Operand_Instruction(pVector, ty, pRd),
            m_nNth(nNth) {}
}; // VecRefInsn

} // Compiler

#endif // !defined(INCLUDE_compiler_cg_instruction_h)
