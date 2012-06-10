//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86x64x64 Machine Dependent Frame
// arch/kernel/x86x64_ke_thread.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_instruction.h#7 $
//
#if !defined(INCLUDE_arch_x86x64_compiler_x86x64_cg_instruction_h)
#define INCLUDE_arch_x86x64_compiler_x86x64_cg_instruction_h


#include "../../../compiler/cg/cg_instruction.h"

//#include "../X86X64_arch.h"
#include "./x86x64_cg_operand.h"

//#include "../kernel/X86X64_ke_thread.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// x86x64-CMP instruction
//  x86x64-CMP int32 %ef <= %sx %sy
//
//  For lowering, EQ, NE, LE, LT, GE, GT
//
class x86x64CmpInsn : public Two_Operands_Instruction_<x86x64CmpInsn>
{
    THIS_IS_IR_INSTRUCTION(X86X64_CMP, "X86X64_CMP int %ef <= %sx %sy")

    public: Ty m_opty;

    public: x86x64CmpInsn(x86x64Flags* pEf, Operand* pSx, Operand* pSy) :
        Two_Operands_Instruction(pSx, pSy, ty_int, pEf),
        m_opty(t) {}
}; // x86x64CmpInsn


//////////////////////////////////////////////////////////////////////
//
// x86x64-CONVERT instruction
//  x86x64-CONVERT int32 %ef <= %sx %sy
//
//  For lowering, EQ, NE, LE, LT, GE, GT
//
class x86x64ConvertInsn : public One_Operand_Instruction_<x86x64ConvertInsn>
{
    THIS_IS_IR_INSTRUCTION(X86X64_CONVERT, "X86X64_CONVERT ty %rd <= %sx")

    public: Ty m_opty;
    public: x86x64ConvertInsn(Ty ty, Register* pRd, Operand* pSx) :
        One_Operand_Instruction(pSx, ty, pRd), m_opty(pSx->GetTy()) {}
}; // x86x64ConvertInsn


//////////////////////////////////////////////////////////////////////
//
// x86x64Def Instruction
//
//  For expanding OPENxxx and CLOSE in assembler.
//  Instruction selector inserts this instruction around OPENxxx and
//  CLOSE with corresponding USE.
//
//      x86x64_DEF %r1
//      OPEN_BIND %frame <= ...
//      USE %r1
//
//      x86x64_DEF %r2
//      CLOSE %frame_1
//      CLOSE %frame_2
//      CLOSE %frame_3
//      USE %r2
//
class x86x64DefInsn : public No_Operand_Instruction_<x86x64DefInsn>
{
    THIS_IS_IR_INSTRUCTION(X86X64_DEF, "X86X64_DEF ty %rd")

    public: x86x64DefInsn(Register* pRd) :
        No_Operand_Instruction(ty_t, pRd) {}

    public: x86x64DefInsn(Ty ty, Register* pRd) :
        No_Operand_Instruction(ty, pRd) {}
}; // x86x64DefInsn


//////////////////////////////////////////////////////////////////////
//
// x86x64-ENCODE instruction
//  x86x64-ENCODE int32 %ef <= %sx %sy
//
//  For lowering, EQ, NE, LE, LT, GE, GT
//
class x86x64EncodeInsn : public One_Operand_Instruction_<x86x64EncodeInsn>
{
    THIS_IS_IR_INSTRUCTION(X86X64_ENCODE, "X86X64_ENCODE ty %rd <= %sx")

    public: x86x64EncodeInsn(Ty ty, Register* pRd, Operand* pSx) :
        One_Operand_Instruction(pSx, ty, pRd) {}
}; // x86x64EncodeInsn


//////////////////////////////////////////////////////////////////////
//
// x86x64-NEG instruction
//  x86x64-NEG ty %fd <= %fx %fy
//
class x86x64NegInsn : public Two_Operands_Instruction_<x86x64NegInsn>
{
    THIS_IS_IR_INSTRUCTION(X86X64_NEG, "X86X64_NEG ty %fd <= %fx %fy")

    public: x86x64NegInsn(Ty ty, Register* pRd, Register* pRx, Register* pRy) :
        Two_Operands_Instruction(pRx, pRy, ty, pRd) {}
}; // x86x64NegInsn


//////////////////////////////////////////////////////////////////////
//
// x86x64Lea2Insn
//
class x86x64Lea2Insn : public Two_Operands_Instruction_<x86x64Lea2Insn>
{
    THIS_IS_IR_INSTRUCTION(X86X64_LEA2, "X86X64_LEA2 int %rd <= %rx ofs")

    public: x86x64Lea2Insn(Register* pRd, Register* pRx, int ofs) :
        Two_Operands_Instruction(pRx, NewInteger(ofs), ty_int, pRd) {}
}; // x86x64Lea2Insn


//////////////////////////////////////////////////////////////////////
//
// x86x64Lea3Insn
//
class x86x64Lea3Insn : public Three_Operands_Instruction_<x86x64Lea3Insn>
{
    THIS_IS_IR_INSTRUCTION(X86X64_LEA3, "X86X64_LEA3 int %rd <= %rx ofs %ry")

    public: x86x64Lea3Insn(
        Register*   pRd,
        Register*   pRx,
        int         ofs,
        Register*   pRy ) :
            Three_Operands_Instruction(pRx, NewInteger(ofs), pRy,
                ty_int, pRd ) {}
}; // x86x64Lea3Insn


//////////////////////////////////////////////////////////////////////
//
// x86x64ServiceInsn
//
class x86x64ServiceInsn : public Two_Operands_Instruction_<x86x64ServiceInsn>
{
    THIS_IS_IR_INSTRUCTION(X86X64_SERVICE, "X86X64_SERVICE code %vy")

    public: x86x64ServiceInsn(int eService, Values* pVy) :
        Two_Operands_Instruction(NewInteger(eService), pVy) {}
}; // x86x64ServiceInsn


//////////////////////////////////////////////////////////////////////
//
// x86x64-SETCC instruction
//  x86x64-SETCC bool %bd <= tttn %ef
//
//  For lowering, EQ, NE, LE, LT, GE, GT
//
class x86x64SetCcInsn : public Two_Operands_Instruction_<x86x64SetCcInsn>
{
    THIS_IS_IR_INSTRUCTION(X86X64_SETCC, "X86X64_SETCC bool %bd <= tttn %ef")

    public: x86x64SetCcInsn(Bool* pBd, Tttn eTttn, x86x64Flags* pEf) :
        Two_Operands_Instruction(NewInteger(eTttn), pEf, ty_bool, pBd) {}
}; // x86x64SetCcInsn


//////////////////////////////////////////////////////////////////////
//
// x86x64TcbInsn
//
//  o Save values from %rx.
//  o For CLOSE.
//
class x86x64TcbInsn :
    public One_Operand_Instruction_<x86x64TcbInsn>
{
    THIS_IS_IR_INSTRUCTION(X86X64_TCB, "X86X64_TCB ty %rd <= ofs")

    public: x86x64TcbInsn(Ty ty, Register* pRd, size_t ofs) :
        One_Operand_Instruction(NewInteger(ofs), ty, pRd) {}
}; // x86x64TcbInsn


//////////////////////////////////////////////////////////////////////
//
// x86x64-TEST instruction
//  x86x64-TEST int32 %ef <= %sx %sy
//
//  For lowering, EQ, NE, LE, LT, GE, GT
//
class x86x64TestInsn : public Two_Operands_Instruction_<x86x64TestInsn>
{
    THIS_IS_IR_INSTRUCTION(X86X64_TEST, "X86X64_TEST int %ef <= %sx %sy")

    public: x86x64TestInsn(x86x64Flags* pEf, Operand* pSx, Operand* pSy) :
        Two_Operands_Instruction(pSx, pSy, ty_int32, pEf) {}
}; // x86x64TestInsn

} // Compiler

#endif //!defined(INCLUDE_arch_x86x64_compiler_x86x64_cg_instruction_h)
