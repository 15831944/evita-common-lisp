#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - x64 - assembler - instruction
// compiler/cg/x64_cg_asm_insn.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/compiler/x64_cg_asm.cpp#31 $
//
#include "../../x86x64/compiler/x86x64_cg_asm.h"
#include "./x64_cg_instruction.h"
#include "./x64_cg_target.h"

#include "../../../compiler/cm/cm_module.h"
#include "../../../compiler/ir/ir_bblock.h"

#include "../kernel/x64_ke_frame.h"
#include "../kernel/x64_ke_thread.h"

namespace Compiler
{

using namespace X64;

// X64CgAsm
class X64CgAsm : public X86X64CgAsm
{
    protected: AsmCopyReg m_rgoCopyReg[16];

    protected: AsmCopyReg* getCopyReg(Reg iRx)
        { return &m_rgoCopyReg[iRx & 15]; }


    protected: X64CgAsm(const char16* pwsz) : X86X64CgAsm(pwsz) {}

    // cast_to_reg8
    static Reg cast_to_reg8(Register* pRd)
        { return static_cast<Reg>((cast_to_reg(pRd)&15) | Gpr_AL); }

    // cast_to_reg16
    static Reg cast_to_reg16(Register* pRd)
        { return static_cast<Reg>((cast_to_reg(pRd)&15) | Gpr_AX); }

    // cast_to_reg32
    static Reg cast_to_reg32(Register* pRd)
        { return static_cast<Reg>((cast_to_reg(pRd)&15) | Gpr_EAX); }

    // cast_to_reg64
    static Reg cast_to_reg64(Register* pRd)
        { return static_cast<Reg>((cast_to_reg(pRd)&15) | Gpr_RAX); }

    // emit_imm32
    protected: void emit_imm32(Operand*);

    ////////////////////////////////////////////////////////////
    //
    // emit_op
    //
    protected: void emit_op(uint, Reg, Reg, Reg);

    protected: void emit_op(uint op, Reg rd, const Addr* pAddr)
    {
        if (NULL != pAddr->m_pRy)
        {
            emit_op(op, rd, pAddr->m_pRx, pAddr->m_pRy);
        }
        else
        {
            emit_op(op, rd, pAddr->m_pRx);
        }
    } // emit_op

    protected: void emit_op(uint op, Register* pRd, const Addr* pAddr)
    {
        if (NULL != pAddr->m_pRy)
        {
            emit_op(op, cast_to_reg(pRd), pAddr->m_pRx, pAddr->m_pRy);
        }
        else
        {
            emit_op(op, cast_to_reg(pRd), pAddr->m_pRx);
        }
    } // emit_op

    // Note: Gpr_RAX denotes 64bit operation.
    protected: void emit_op(uint op, Reg rb)
        { emit_op(op, Gpr_RAX, rb); }

    protected: void emit_op(uint op, Reg rd, Reg rb)
        { emit_op(op, rd, rb, Gpr_EAX); }

    protected: void emit_op(uint op, Register* pRx)
        { emit_op(op, get_reg_for_rex(pRx)); }

    protected: void emit_op(
        uint        op,
        Register*   pRx,
        Register*   pRy )
    { 
        emit_op(op, get_reg_for_rex(pRx), get_reg_for_rex(pRy));
    } // emit_op

    protected: void emit_op(uint op, Register* pRx, Reg ry)
        { emit_op(op, get_reg_for_rex(pRx), ry); }

    protected: void emit_op(uint op, Reg rx, Register* pRy)
        { emit_op(op, rx, get_reg_for_rex(pRy)); }

    protected: void emit_op(uint op)
        { X86X64CgAsm::emit_op(op); }

    protected: void emit_op(
        uint op,
        Register*   pRd,
        Register*   pRb,
        Register*   pRi )
    {
        emit_op(
            op,
            get_reg_for_rex(pRd),
            get_reg_for_rex(pRb),
            get_reg_for_rex(pRi) );
    } // emit_op

    protected: void emit_op(uint op, Reg rd, Register* pRb, Register* pRi)
        { emit_op(op, rd, get_reg_for_rex(pRb), get_reg_for_rex(pRi)); }

    Reg get_reg_for_rex(Register*);

    ////////////////////////////////////////////////////////////
    //
    // Instructions
    //

    // asm_arith_imm
    void asm_arith_imm(OpExt, Reg, int32);
    void asm_arith_imm(OpExt opext, Register*, int32 imm32);

    // CALL callee
    protected: void asm_CALL(Val callee)
    {
        ASSERT(symbolp(callee));

        emit_op(op_CALL_Jv);
        annotate(Annon_NamedCallee, NewLiteral(callee));
        emit_u32(0);
    } // asm_CALL

    // CMOV
    protected: void asm_CMOV(Tttn tttn, Reg rd, Reg rb, int disp32)
        { emit_op(op_CMOVcc_Gv_Ev + tttn, rd, rb); emit_disp(rd, rb, disp32); }

    protected: void asm_CMOV(Tttn tttn, Register* pRd, Register* pRx)
        { emit_op(op_CMOVcc_Gv_Ev + tttn, pRd, pRx); emit_ea(pRd, pRx); }

    ////////////////////////////////////////////////////////////
    //
    // LEA
    //

    // LEA rd <= [rb+disp]
    protected: void asm_LEA(Reg rd, Reg rb, int disp32)
        { emit_op(op_LEA_Gv_M, rd, rb); emit_disp(rd, rb, disp32); }

    // LEA rd <= [rb+disp]
    protected: void asm_LEA(Register* pRd, Reg rb, int disp32)
        { asm_LEA(cast_to_reg(pRd), rb, disp32); }

    // LEA rd <= [rb+ri+disp]
    protected: void asm_LEA(Register* pRd, Reg rb, Reg ri, Int disp32)
        { asm_LEA(cast_to_reg(pRd), rb, ri, disp32); }

    // LEA rd <= [rb+ri+disp]
    protected: void asm_LEA(Reg rd, Reg rb, Reg ri, Int disp32)
        { emit_op(op_LEA_Gv_M, rd, rb, ri); emit_index(rd, rb, ri, disp32); }

    ////////////////////////////////////////////////////////////
    //
    // MOV
    //

    // MOV rd <= rx
    protected: void asm_MOV(Reg rd, Reg rx)
        { emit_op(op_MOV_Gv_Ev, rd, rx); emit_modrm(Mod_Reg, rd, rx); }

    // MOV rd <= [rb+disp]
    protected: void asm_MOV(Register* pRd, Reg rb, int32 disp32)
        { asm_MOV(cast_to_reg(pRd), rb, disp32); }

    // MOV rd <= lit
    protected: void asm_MOV(Reg rd, Val lit)
        { asm_MOV(rd, NewLiteral(lit)); }

    // MOV rd <= lit
    protected: void asm_MOV(Reg, Operand*);

    // MOV rd <= imm32
    protected: void asm_MOV(Reg rd, int32 imm32)
    {
        emit_op(op_MOV_Ev_Iz, rd);
        emit_modrm(Mod_Reg, opext_MOV_Ev_Iz, rd);
        emit_s32(imm32);
    } // emit_MOV

    // MOV rd <= [rb+disp]
    protected: void asm_MOV(Reg rd, Reg rb, int32 disp32)
        { emit_op(op_MOV_Gv_Ev, rd, rb); emit_disp(rd, rb, disp32); }

    protected: void asm_MOV(Register* pRd, Register* pRb, int disp32)
        { emit_op(op_MOV_Gv_Ev, pRd, pRb); emit_disp(pRd, pRb, disp32); }

    // MOV [rb+disp] <= Iz
    protected: void asm_MOV(Reg, int32, Operand*);

    // MOV [rb+disp] <= rx
    protected: void asm_MOV(Reg rb, int32 disp32, Reg rx)
        { emit_op(op_MOV_Ev_Gv, rx, rb); emit_disp(rx, rb, disp32); }

    // MOV [rb+disp] <= imm32
    protected: void asm_MOV(Reg rb, int32 disp32, int32 imm32)
    {
        emit_op(op_MOV_Ev_Iz, rb);
        emit_disp(opext_MOV_Ev_Iz, rb, disp32);
        emit_s32(imm32);
    } // asm_MOV

    // MOV [rb+disp] <- lit
    protected: void asm_MOV(Reg rb, int32 disp32, Val lit)
        { asm_MOV(rb, disp32, NewLiteral(lit)); }

    private: void asm_MOV(uint, uint) {}
    private: void asm_MOV(uint, Operand*) {}

    // tcb$mv_value
    protected: struct tcb$mv_value
        { uint m_i; tcb$mv_value(int i) : m_i(i) {} };

    // MOV tcb.mv_value[i] <= rx
    protected: void asm_MOV(tcb$mv_value ea, Reg rx)
    { 
        asm_MOV(
            $rtcb, static_cast<int32>(offsetof(Thread, mv_value[ea.m_i])),
            rx );
    } // asm_MOV

    // MOV tcb.mv_value[i] <= Iz
    protected: void asm_MOV(tcb$mv_value ea, Operand* pSy)
    { 
        asm_MOV(
            $rtcb, static_cast<int32>(offsetof(Thread, mv_value[ea.m_i])), 
            pSy );
    } // asm_MOV

    // SHx rx, ry
    protected: void asm_SHx(Instruction*, OpExt, Int);
    protected: void asm_SHx(Instruction*, OpExt);

    ////////////////////////////////////////////////////////////
    //
    // TEST
    //

    // TEST rx, ry
    protected: void asm_TEST(Reg rx, Reg ry)
        { emit_op(op_TEST_Ev_Gv, rx, ry); emit_modrm(Mod_Reg, rx, ry); }

    // TEST Ev, Gv
    protected: void asm_TEST(Register* pRx, Register* pRy)
        { emit_op(op_TEST_Ev_Gv, pRx, pRy); emit_ea(pRx, pRy); }

    // TEST Ev, Iz
    protected: void asm_TEST(Register* pRx, Int iImm)
    {
        if (! is_32bit(iImm))
        {
            asm_invalid_operand(NewInteger(iImm));
            return;
        }

        switch (pRx->GetStorage())
        {
        case Register::Storage_Physical:
        {
            Reg rx = cast_to_reg(pRx);
            if (rx == Gpr_EAX)
            {
                if (iImm >= 0 && iImm <= 255)
                {
                    emit_op(op_TEST_AL_Ib);
                    emit_u8(static_cast<uint8>(iImm));
                }
                else
                {
                    emit_op(op_TEST_eAX_Iz, Gpr_RAX);
                    emit_s32(static_cast<int32>(iImm));
                }
            }
            else if (iImm >= 0 && iImm <= 255)
            {
                Reg rxl =static_cast<Reg>(rx - Gpr_RAX + Gpr_AL);
                emit_op(op_TEST_Eb_Ib, rxl);
                emit_modrm(Mod_Reg, opext_TEST_Eb_Ib, rx);
                emit_u8(static_cast<uint8>(iImm));
            }
            else
            {
                emit_op(op_TEST_Ev_Iz, rx);
                emit_modrm(Mod_Reg, opext_TEST_Ev_Iz, rx);
                emit_s32(static_cast<int32>(iImm));
            }
            break;
        } // physical

        case Register::Storage_Stack:
            if (iImm >= 0 && iImm <= 255)
            {
                emit_op(op_TEST_Eb_Ib);
                emit_ea(opext_TEST_Eb_Ib, pRx);
                emit_u8(static_cast<uint8>(iImm));
            }
            else
            {
                emit_op(op_TEST_Ev_Iz, Gpr_RAX);
                emit_ea(opext_TEST_Ev_Iz, pRx);
                emit_s32(static_cast<int32>(iImm));
            }
            break;

        default:
            asm_invalid_operand(pRx);
            break;
        } // switch storage
    } // asm_TEST

    ////////////////////////////////////////////////////////////
    //
    // XCHG
    //

    // XCHG rx, ry
    protected: void asm_XCHG(Reg rx, Reg ry)
    {
        // We use XCHG eAX, e?X
        if (Gpr_EAX == rx)
        {
            emit_op(op_XCHG_eAX_eAX + (ry & 7), ry);
        }
        else if (Gpr_EAX == ry)
        {
            emit_op(op_XCHG_eAX_eAX + (rx & 7), ry);
        }
        else
        {
            emit_op(op_XCHG_Ev_Gv, rx, ry);
            emit_modrm(Mod_Reg, rx, ry);
        } // if
    } // asm_XCHG

    // XOR rd, rx
    protected: void asm_XOR(Reg rd, Reg rx)
        { emit_op(op_XOR_Gv_Ev, rd, rx); emit_modrm(Mod_Reg, rd, rx); }
}; // X64CgAsm


//////////////////////////////////////////////////////////////////////
//
// X64CgAsmPass
//
class X64CgAsmPass : public X86X64AsmPass<X64Target, X64CgAsm>
{
    public: X64CgAsmPass() : 
        X86X64AsmPass<X64Target, X64CgAsm>(L"X64-ASM") {}

    protected: virtual void process_instruction(Instruction*);

    void process_VALUES_aux(ValuesInsn*);

    ////////////////////////////////////////////////////////////
    //
    // ISA
    //
    static Reg reg_arg(uint k)
        { return static_cast<Reg>(X64Mach::ISA.m_pGprArg->Get(k)); }

    static uint reg_arg_limit()
        { return X64Mach::ISA.m_pGprArg->GetLength(); }

    ////////////////////////////////////////////////////////////
    //
    // Instructions
    //
    typedef void (X64CgAsmPass::*InsnProcT)(Instruction*);
    static const InsnProcT k_rgpInsnProc[IrOp_MAX_1 + 1];

    void process_BOX(Instruction*);
    void process_COPY(Instruction*);
    void process_LOAD(Instruction*);
    void process_NTHVALUE(Instruction*);
    void process_PROLOGUE(Instruction*);
    void process_RET(Instruction*);
    void process_SLOT(Instruction*);
    void process_STACKDEF(Instruction*);

    void process_STORE(Instruction*);
    void process_UNBOX(Instruction*);
    void process_VALUES(Instruction*);

    void process_arith(Instruction*, OpExt);
    void process_addsub(Instruction*, OpExt, Opcode, Opcode);
    void process_muldiv(Instruction*, OpExt, Opcode, Opcode);

    void process_fop_sx(Instruction*, Opcode);
    void process_fop_sy(Instruction*, Opcode);

    void process_ADD(Instruction* p)
        { process_addsub(p,
            opext_ADD_Ev_Iz,
            op_ADDSD_Vsd_Wsd, op_ADDSS_Vss_Wss); }

    void process_DIV(Instruction* p)
        { process_muldiv(p,
            opext_IDIV_Ev,
            op_DIVSD_Vsd_Wsd, op_DIVSS_Vss_Wss); }

    void process_MUL(Instruction* p)
        { process_muldiv(p,
            opext_IMUL_Ev,
            op_MULSD_Vsd_Wsd, op_MULSS_Vss_Wss ); }

    void process_SUB(Instruction* p)
        { process_addsub(p,
            opext_SUB_Ev_Iz,
            op_SUBSD_Vsd_Wsd, op_SUBSS_Vss_Wss); }

    #define DEFPROC_ARITH(mp_NAME, mp_OPCODE) \
        void process_##mp_NAME(Instruction* pInsn) \
        { \
            process_arith(pInsn, opext_##mp_OPCODE##_Ev_Iz); \
        } // DEFPROC_ARITH

    DEFPROC_ARITH(LOGAND, AND)
    DEFPROC_ARITH(LOGIOR, OR)
    DEFPROC_ARITH(LOGXOR, XOR)

    DEFPROC_SAME(PHICOPY, COPY);
    DEFPROC_SAME(RELOAD,  COPY);
    DEFPROC_SAME(SPILL,   COPY);
    DEFPROC_SAME(SPLIT,   COPY);

    void process_X86X64_CMP(Instruction*);
    void process_X86X64_CONVERT(Instruction*);
    void process_X86X64_ENCODE(Instruction*);
    void process_X86X64_NEG(Instruction*);
    void process_X86X64_SERVICE(Instruction*);
    void process_X86X64_TEST(Instruction*);

    // get_load_op
    static Opcode get_load_op(Ty ty)
    {
        if (ty == ty_float64) return op_MOVSD_Vsd_Wsd;
        if (ty == ty_float32) return op_MOVSS_Vss_Wss;

        if (ty == ty_int8)    return op_MOVSX_Gv_Eb;
        if (ty == ty_int16)   return op_MOVSX_Gv_Ew;
        if (ty == ty_int32)   return op_MOVSXD_Gv_Ev;

        return op_MOV_Gv_Ev;
    } // get_load_op
}; // X64CgAsmPass


// Dispatch table
const X64CgAsmPass::InsnProcT
X64CgAsmPass::k_rgpInsnProc[IrOp_MAX_1 + 1] =
{
    #define DEFIROP(mp_name) &X64CgAsmPass::process_##mp_name,
    #include "./x64_cg_opcode.inc"
}; // X64CgAsmPass::k_rgpInsnProc


//////////////////////////////////////////////////////////////////////
//
// asm_arith_imm
//
//      TEST Ev, Gv         -- for CMP_Ev_Iz and imm is zero.
//      ADD  eAX, Iv
//      ADD  Ev, Iv
//      ADD  Ev, Is
//
//  opext = opext_ADD_Ev_Iz
//          opext_OR_Ev_Iz
//          opext_ADC_Ev_Iz
//          opext_SBB_Ev_Iz
//          opext_AND_Ev_Iz
//          opext_SUB_Ev_Iz
//          opext_XOR_Ev_Iz
//          opext_CMP_Ev_Iz
//
void X64CgAsm::asm_arith_imm(OpExt opext, Reg rx, int32 imm32)
{
    if (0 == imm32)
    {
        if (opext_CMP_Ev_Iz == opext)
        {
            asm_TEST(rx, rx);
        }
        else
        {
            warn(L"We must optimize ADD Ev, 0 before assembler.");
        }
    }
    else if (imm32 >= -128 && imm32 <= 127)
    {
        emit_op(op_ADD_Ev_Ib, rx);
        emit_modrm(Mod_Reg, opext, rx);
        emit_s8(static_cast<int8>(imm32));
    }
    else if (Gpr_EAX == rx)
    {
        emit_op(op_ADD_eAX_Iz + opext * 8);
        emit_s32(static_cast<int32>(imm32));
    }
    else
    {
        emit_op(op_ADD_Ev_Iz, rx);
        emit_modrm(Mod_Reg, opext, rx);
        emit_s32(static_cast<int32>(imm32));
    } // if
} // X64CgAsm::asm_arith_imm


// X64CgAsm::asm_arith_imm
void X64CgAsm::asm_arith_imm(OpExt opext, Register* pRx, int32 imm32)
{
    if (pRx->IsPhysical())
    {
        asm_arith_imm(opext, cast_to_reg(pRx), imm32);
    }
    else if (imm32 >= -128 && imm32 <= 127)
    {
        emit_op(op_ADD_Ev_Ib, Gpr_RAX, $sp);
        emit_ea(opext, pRx);
        emit_s8(static_cast<int8>(imm32));
    }
    else
    {
        emit_op(op_ADD_Ev_Iz, Gpr_RAX, $sp);
        emit_ea(opext, pRx);
        emit_s32(static_cast<int32>(imm32));
    } // if
} // X64CgAsm::asm_arith_imm


//////////////////////////////////////////////////////////////////////
//
// X64CgAsm::asm_SHx
//
void X64CgAsm::asm_SHx(Instruction* pInsn, OpExt opext)
{
    unless (pInsn->GetRy()->IsPhysical() &&
            pInsn->GetRy()->GetLocation() == Gpr_ECX )
    {
        asm_invalid_operand(pInsn->GetRy());
        return;
    }

    Opcode op = op_SAR_Ev_CL;
    if (pInsn->GetTy() == ty_uint32 && opext_SAR_Ev_CL == opext)
        { op = op_SHR_Ev_CL; opext = opext_SHR_Ev_CL; }

    emit_op(op, pInsn->GetRd());
    emit_ea(opext, pInsn->GetRx());
} // X64CgAsm::asm_SHx


//////////////////////////////////////////////////////////////////////
//
// X64CgAsm::asm_SHx
//
void X64CgAsm::asm_SHx(Instruction* pInsn, OpExt opext, Int nK)
{
    if (nK <= 0 || nK > sizeof(Val) * 8)
    {
        asm_invalid_operand(pInsn->GetSy());
        return;
    }

    if (1 == nK)
    {
        Opcode op = op_SAR_Ev_1;
        if (pInsn->GetTy() == ty_uint32 && opext_SAR_Ev_1 == opext)
            { op = op_SHR_Ev_1; opext = opext_SHR_Ev_1; }

        emit_op(op, pInsn->GetRd());
        emit_ea(opext, pInsn->GetRx());
    }
    else
    {
        Opcode op = op_SAR_Ev_Ib;
        if (pInsn->GetTy() == ty_uint32 && opext_SAR_Ev_Ib == opext)
            { op = op_SHR_Ev_Ib; opext = opext_SHR_Ev_Ib; }

        emit_op(op, pInsn->GetRd());
        emit_ea(opext, pInsn->GetRx());
        emit_u8(static_cast<uint8>(nK));
    }
} // X64CgAsm::asm_SHx


//////////////////////////////////////////////////////////////////////
//
//  X64CgAsm::emit_imm32
//
void
X64CgAsm::emit_imm32(Operand* pSx)
{
    ASSERT(NULL != pSx);
    switch (pSx->GetKind())
    {
    case Operand::Kind_Literal:
    {
        Val x = pSx->StaticCast<Literal>()->GetDatum();
        if (! is_32bit(x)) CAN_NOT_HAPPEN();
        emit_s32(static_cast<int32>(x->ToInt()));
        break;
    } // Literal

    case Operand::Kind_Integer:
    {
        Int imm64 = pSx->StaticCast<Integer>()->GetValue();
        if (! is_32bit(imm64)) CAN_NOT_HAPPEN();
        emit_s32(static_cast<int32>(imm64));
        break;
    } // Kind_Integer

    case Operand::Kind_Label:   // for Block, Catch, Tagbody frame
        annotate(Annon_ExitPoint, pSx);
        emit_u32(0);
        break;

    case Operand::Kind_TlvName:
        annotate(
            Annon_TlvOffset,
            NewLiteral(pSx->StaticCast<TlvName>()->GetDatum()) );
        emit_u32(0);
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch kind
} // X64CgAsm::emit_imm32


// X64CgAsm::emit_op
void X64CgAsm::emit_op(uint op, Reg rd, Reg rb, Reg ri)
{
    uint rex;
    if (op < 0x48000000)
    {
        rex = Rex_None;
    }
    else
    {
        rex = Rex_W;
        op &= 0xffffff;
    }

    if (rd >= Gpr_RAX && rd <= Gpr_R15)  rex |= Rex_W;

    if ((rd & 15) >= 8)  rex |= Rex_R;
    if ((rb & 15) >= 8)  rex |= Rex_B;
    if ((ri & 15) >= 8)  rex |= Rex_X;

    if (rex != Rex_None || (rd >= Gpr_SPL && rd <= Gpr_DIL))
    {
        // Note: Mandatory prefixes must preceed before REX prefix.
        uint8 prefix = static_cast<uint8>(op >> 16);
        if (prefix == 0x66 || prefix == 0xF2 || prefix == 0xF3)
        {
            emit_u8(prefix);
            op &= 0xFFFF;
        }
        emit_u8(static_cast<uint8>(rex));
    }
    emit_op(op);
} // X64CgAsm::emit_op


// X64CgAsm::get_reg_for_rex
Reg X64CgAsm::get_reg_for_rex(Register* pRx)
{
    switch (pRx->GetStorage())
    {
    case Register::Storage_Physical:
        return static_cast<Reg>(pRx->GetLocation());

    case Register::Storage_Stack:
        return Gpr_RSP;

    case Register::Storage_Pseudo:
    {
        Instruction* pDfn = pRx->GetDfn();
        switch (pDfn->GetOpcode())
        {
        case IrOp_UNBOX:
            return static_cast<Reg>(pDfn->GetRx()->GetLocation());
        } // switch opcode
        break;
    } // pseudo
    } // switch storage

    asm_invalid_operand(pRx);
    return Gpr_RAX;
} // X64CgAsm::get_reg_for_rex


// X64CgAsm::asm_MOV - MOV rd <- lit
void X64CgAsm::asm_MOV(Reg rd, Operand* pSx)
{
    ASSERT(NULL != pSx);
    switch (pSx->GetKind())
    {
    case Operand::Kind_Function:
        emit_op(op_MOV_eAX_Iv + (rd & 7), rd);
        annotate(Annon_Function, pSx);
        emit_u64(0);
        break;

    case Operand::Kind_FunLit:
        emit_op(op_MOV_eAX_Iv + (rd & 7), rd);
        annotate(Annon_Function, pSx->StaticCast<FunLit>()->GetFun());
        emit_u64(0);
        break;

    case Operand::Kind_Integer:
    {
        Integer* pIx = pSx->StaticCast<Integer>();
        Int imm64 = pIx->GetValue();
        if (is_32bit(imm64))
        {
            emit_op(
                op_MOV_Ev_Iz,
                rd,
                rd >= Gpr_RAX && rd <= Gpr_R15 ? Gpr_RAX : Gpr_EAX );

            emit_modrm(Mod_Reg, opext_MOV_Ev_Iz, rd);
            emit_s32(static_cast<int32>(imm64));
        }
        else
        {
            emit_op(op_MOV_eAX_Iv + (rd & 7), rd);
            emit_s64(imm64);
        }
        break;
    } // Kind_Integer

    case Operand::Kind_Literal:
    {
        Val val = pSx->StaticCast<Literal>()->GetDatum();
        if (val == nil)
        {
            asm_MOV(rd, $rnil);
        }
        else if (is_32bit(val))
        {
            emit_op(op_MOV_Ev_Iz, rd);
            emit_modrm(Mod_Reg, opext_MOV_Ev_Iz, rd);
            emit_s32(static_cast<int32>(val->ToInt()));
        }
        else
        {
            emit_op(op_MOV_eAX_Iv + (rd & 7), rd);
            annotate(Annon_LispVal, pSx);
            emit_u64(0);
        }
        break;
    } // Litreal

    case Operand::Kind_Register:
    {
        Register* pRx = pSx->StaticCast<Register>();
        switch (pRx->GetStorage())
        {
        case Register::Storage_Closed:
            emit_op(op_MOV_eAX_Iv + (rd & 7), rd);
            annotate(
                Annon_ClosedLit,
                NewLiteral(Fixnum::Encode(pRx->GetLocation())) );
            emit_u64(0);
            break;

        case Register::Storage_LoadTimeValue:
            emit_op(op_MOV_eAX_Iv + (rd & 7), rd);
            annotate(Annon_LispVal, pRx->GetDfn()->GetSx());
            emit_u64(0);
            break;

        default:
            goto invalid_operand;
        } // switch storage
        break;
    } // Kind_Regiser

    default:
        goto invalid_operand;

    invalid_operand:
        asm_invalid_operand(pSx);
        break;
    } // switch kind
} // X64CgAsm::asm_MOV


// X64CgAsm::asm_MOV - MOV [rb+disp] <- lit
void X64CgAsm::asm_MOV(Reg rb, int32 disp32, Operand* pSx)
{
    ASSERT(NULL != pSx);
    switch (pSx->GetKind())
    {
    case Operand::Kind_Literal:
    {
        Val val = pSx->StaticCast<Literal>()->GetDatum();
        if (nil == val)
        {
            asm_MOV(rb, disp32, $rnil);
        }
        else if (is_32bit(val))
        {
            asm_MOV(rb, disp32, static_cast<int32>(val->ToInt()));
        }
        else
        {
            goto invalid_operand;
        }
        break;
    } // Literal

    case Operand::Kind_Integer:
    {
        Integer* pIx = pSx->StaticCast<Integer>();
        Int imm64 = pIx->GetValue();
        if (is_32bit(imm64))
        {
            emit_op(op_MOV_Ev_Iz, rb);
            emit_disp(opext_MOV_Ev_Iz, rb, disp32);
            emit_s32(static_cast<int32>(imm64));
        }
        else
        {
            goto invalid_operand;
        }
        break;
    } // integer

    case Operand::Kind_Label:   // for Block, Catch, Tagbody frame
    {
        emit_op(op_MOV_Ev_Iz, rb);
        emit_disp(opext_MOV_Ev_Iz, rb, disp32);
        annotate(Annon_ExitPoint, pSx);
        emit_u32(0);
        break;
    } // label

    case Operand::Kind_Register:
    {
        Register* pRx = pSx->StaticCast<Register>();
            ASSERT(pRx->IsPhysical());
        asm_MOV(rb, disp32, cast_to_reg(pRx));
        break;
    } // register

    case Operand::Kind_TlvName:
        emit_op(op_MOV_Ev_Iz, rb);
        emit_disp(opext_MOV_Ev_Iz, rb, disp32);
        annotate(
            Annon_TlvOffset,
            NewLiteral(pSx->StaticCast<TlvName>()->GetDatum()) );
        emit_u32(0);
        break;

    default:
        goto invalid_operand;

    invalid_operand:
        asm_invalid_operand(pSx);
        break;
    } // switch kind
} // X64CgAsm::asm_MOV


//////////////////////////////////////////////////////////////////////
//
//  X64CgAsmPass::process_BOX
//      CALL box-float64
//
// Note: Until we support float32 and float64 arguments and values,
// we generate CALL instruction in assembler instead of cg-SELECT
// pass. cg-RA pass knows BOX instruction is assembled into CALL.
// This is not good for modularity.
//
void X64CgAsmPass::process_BOX(Instruction* pBox)
{
    if (pBox->GetTy() == ty_double_float)
    {
        if (pBox->GetRx()->GetLocation() != $f0)
        {
            // Expect double-float expect xmm0
            asm_invalid_operand(pBox->GetSx());
            return;
        }

        emit_op(op_CALL_Jv);
        annotate(Annon_NamedCallee, NewLiteral(QDbox_float64));
        emit_u32(0);
        return;
    } // double-float

    if (pBox->GetTy() == ty_single_float)
    {
        if (pBox->GetRx()->GetLocation() != $f0)
        {
            // Expect single-float expect xmm0
            asm_invalid_operand(pBox->GetSx());
            return;
        }

        emit_op(op_CALL_Jv);
        annotate(Annon_NamedCallee, NewLiteral(QDbox_float32));
        emit_u32(0);
        return;
    } // single-float

    asm_unsupported(pBox->GetTy());
} //  X64CgAsmPass::process_BOX


//////////////////////////////////////////////////////////////////////
//
//  X64CgAsmPass::process_COPY
//
void X64CgAsmPass::process_COPY(Instruction* pCopy)
{
    Register* pRd = pCopy->GetRd();

    if (pCopy->GetTy() == ty_float32)
    {
        Register* pRx = pCopy->GetRx();

        switch (pRd->GetStorage())
        {
        case Register::Storage_Physical:
            emit_op(op_MOVSS_Vss_Wss, pRd, pRx);
            emit_ea(pRd, pRx);
            return;

        case Register::Storage_Stack:
            emit_op(op_MOVSS_Wss_Vss, pRx, $sp);
            emit_ea(pRx, pRd);
            return;
        } // switch storage

        asm_error_vreg(pRd);
        return;
    } // if float32

    if (pCopy->GetTy() == ty_float64)
    {
        Register* pRx = pCopy->GetRx();

        switch (pRd->GetStorage())
        {
        case Register::Storage_Physical:
            emit_op(op_MOVSD_Vsd_Wsd, pRd, pRx);
            emit_ea(pRd, pRx);
            return;

        case Register::Storage_Stack:
            emit_op(op_MOVSD_Wsd_Vsd, pRx, $sp);
            emit_ea(pRx, pRd);
            return;
        } // switch storage

        asm_error_vreg(pRd);
        return;
    } // if float64

    if (pRd->Is<Physical>() && pCopy->GetTy() == ty_int32)
    {
        Reg rd = static_cast<Reg>(pRd->GetLocation() - Gpr_RAX + Gpr_EAX);
        asm_copy(rd, pCopy->GetSx());
        return;
    }

    asm_copy(pRd, pCopy->GetSx());
} //  X64CgAsmPass::process_COPY


//////////////////////////////////////////////////////////////////////
//
//  X64CgAsmPass::process_LOAD
//
//  LOAD ty %rd <= %sx
//
//      MOV_Gv_Ev %rd <= [%rx]
//      MOV_Gv_Ev %rd <= [disp32]
//
void X64CgAsmPass::process_LOAD(Instruction* pLoad)
{
    Register* pRd = pLoad->GetRd();

    if (pRd->IsPseudo())
    {
        // Load cell from frame.
        return;
    }

    ASSERT(pRd->Is<Physical>());

    Reg rd = static_cast<Reg>(pRd->GetLocation());

    Register* pRx = pLoad->GetRx();

    Addr oAddr;
    {
        switch (pRx->GetStorage())
        {
        case Register::Storage_Pseudo:
            compute_addr(pRx->GetDfn(), &oAddr);
            break;

        case Register::Storage_Physical:
            oAddr.m_eMode = AddrMode_BaseDisp;
            oAddr.m_pRx = pRx;
            oAddr.m_ofs = 0;
            break;

        default:
            asm_invalid_operand(pRx);
            return;
        } // switch storage
    } // oAddr

    Ty ty = pLoad->GetTy();

    switch (oAddr.m_eMode)
    {
    case AddrMode_BaseDisp:
    case AddrMode_BaseIndex:
        if (ty == ty_uint32)
        {
            emit_op(op_MOV_Gv_Ev, cast_to_reg32(pRd), &oAddr);
        }
        else if (ty == ty_uint16)
        {
            emit_op(op_MOVZX_Gv_Ew, cast_to_reg16(pRd), &oAddr);
        }
        else if (ty == ty_uint8)
        {
            emit_op(op_MOVZX_Gv_Eb, cast_to_reg8(pRd), &oAddr);
        }
        else
        {
            emit_op(get_load_op(ty), pRd, &oAddr);
        }
        emit_ea(pRd, &oAddr);
        break;

    case AddrMode_ClosedLit:
        emit_op(op_MOV_eAX_Iv + (rd & 7), rd);
        annotate(Annon_ClosedLit, oAddr.m_pAnnon);
        emit_u64(0);
        break;

    case AddrMode_LiteralCell:
    case AddrMode_StackCell:
        asm_copy(pRd, oAddr.m_pRx);
        break;

    case AddrMode_TlvOffset:
        emit_op(op_MOV_Gv_Ev, rd, $rtcb);
        emit_modrm(Mod_Disp32, pRd, $rtcb);
        annotate(Annon_TlvOffset, oAddr.m_pAnnon);
        emit_u32(0);
        break;

    default:
        asm_invalid_operand(pRx);
        return;
    } // switch addr mode
} // X64CgAsmPass::process_LOAD


//////////////////////////////////////////////////////////////////////
//
// X64CgAsmPass::process_NTHVALUE
//      mov $rfn <= pSx
//      call x86::.nth-value
//
void X64CgAsmPass::process_NTHVALUE(Instruction* pInsn)
{
    Values* pVy = pInsn->GetSy()->StaticCast<Values>();
    if (pVy->GetDfn()->Is<CallInsn>())
    {
        asm_CMOV(tttn_NC, $rn, $rtcb, SVC_fixnum_one);
    }

    asm_copy($rfn, pInsn->GetSx());
    asm_CALL(QDnth_value);
} // X64CgAsmPass::process_NTHVALUE


//////////////////////////////////////////////////////////////////////
//
// X64CgAsmPass::process_PROLOGUE
//
void X64CgAsmPass::process_PROLOGUE(Instruction* pInsn)
{
    ASSERT(NULL != pInsn);
    ASSERT(pInsn->Is<PrologueInsn>());

    // -8 for caller's RA.
    size_t cbFrame = m_oContext.GetFunction()->GetFrameSize() - 8;

    if (Knone != pInsn->GetLy())
    {
        asm_MOV(
            $rtcb, offsetof(Thread, m_fn),
            m_oContext.GetFunction()->GetArityMax() * Fixnum::One );

        Val restify;
        {
            if (Kheap == pInsn->GetLy())
            {
                restify = QDrestify;
            }
            else if (Kstack == pInsn->GetLy())
            {
                restify = QDstack_restify;

                // .statck-restify makes statck aligned.
                cbFrame -= 8;
            }
            else
            {
                CAN_NOT_HAPPEN();
            } // if
        } // restify

        asm_CALL(restify);
    } // if

    // Make function frame
    {
        html_log_format(4, L"PROLOGUE: frame size = ~D~:%", cbFrame);
        if (0 != cbFrame)
        {
            asm_arith_imm(opext_SUB_Ev_Iz, $sp, static_cast<int32>(cbFrame));
        }
    }
} // X64CgAsmPass::process_PROLOGUE


namespace
{
static bool need_CLC(Instruction* pInsn)
{
    Instruction* pFirst = pInsn->GetBBlock()->GetFirstInsn();
    while (pInsn != pFirst)
    {
        pInsn = pInsn->GetPrev();

        switch (pInsn->GetOpcode())
        {
        case IrOp_CLOSURE:
        case IrOp_LOGAND:
        case IrOp_LOGIOR:
        case IrOp_LOGXOR:
        case IrOp_X86X64_TEST:
            return false;

        case IrOp_CALL:
            return ! pInsn->GetSx()->Is<Literal>();

        case IrOp_COPY:
        case IrOp_SELECT:
        case IrOp_LOAD:
        case IrOp_STORE:
        case IrOp_X86X64_SETCC:
            break;

        default:
            return true;
        } // switch opcode
    } // while

    return true;
} // need_CLC

} // namespace


//////////////////////////////////////////////////////////////////////
//
// X64CgAsmPass::process_RET
//
//  1. Set values
//  2. Set value count
//  3. Destroy function frame
//      o ADD ESP, frame_size
//      o MOV ESP, [ESP]    ... for stack-listify
//  4. emit RET
//
void X64CgAsmPass::process_RET(Instruction* pInsn)
{
    ASSERT(NULL != pInsn);

    Operand* pSx = pInsn->GetSx();

    bool fSingleValue = true;

    if (Obj_Void == pSx)
    {
        // nothing to do
    }
    else if (pSx->Is<Register>())
    {
        asm_load($r0, pSx->StaticCast<Register>());
    }
    else if (pSx->Is<Values>())
    {
        fSingleValue = false;
    }
    else
    {
        asm_MOV($r0, pInsn->GetSx());
    } // if

    uint cbFrame = m_oContext.GetFunction()->GetFrameSize();
         cbFrame -= sizeof(Val);

    ASSERT(8 == cbFrame % 16);

    if (! m_oContext.GetFunction()->IsStackRestify())
    {
        if (fSingleValue)
        {
            asm_arith_imm(opext_ADD_Ev_Iz, $sp, cbFrame);
        }
        else
        {
            // Preserve CF
            asm_LEA($sp, $sp, cbFrame);
        }
    }
    else
    {
        if (fSingleValue && need_CLC(pInsn)) emit_op(op_CLC);

        // Restore ESP from RP.
        //   ESP => slot[0]    -+
        //          ...         | = cbFrame
        //          slot[n-1]  -+
        //          RP  o-------+
        //          ...         |
        //          list        |
        //          ...         |
        //          pad         |
        //          RA  <-------+
        asm_MOV($sp, $sp, cbFrame);
    } // if

    emit_op(op_RET);
} // X64CgAsmPass::process_RET


//////////////////////////////////////////////////////////////////////
//
// X64CgAsmPass::process_SLOT
//
void X64CgAsmPass::process_SLOT(Instruction* pSlot)
{
    if (pSlot->GetRd()->IsPseudo()) return;

    Reg rd = cast_to_reg(pSlot->GetRd());

    Addr oAddr;
    switch (compute_slot_addr(pSlot, &oAddr))
    {
    case AddrMode_ClosedVar:
        emit_op(op_MOV_eAX_Iv + (rd & 7), rd);
        annotate(Annon_ClosedVar, oAddr.m_pAnnon);
        emit_u64(0);
        break;

    case AddrMode_SymRef:
        emit_op(op_MOV_eAX_Iv + (rd & 7), rd);
        annotate(oAddr.m_eAnnon, oAddr.m_pAnnon);
        emit_u64(0);
        break;

    default:
        asm_invalid_operand(pSlot->GetSz());
        break;
    } // switch addr mode
} // X64CgAsmPass::process_SLOT


//////////////////////////////////////////////////////////////////////
//
// X64CgAsmPass::process_STORE
//
//      STORE %rx, %sy
//
//  Emit:
//      MOV_Ev_Gv [%rx]    <= %ry
//      MOV_Ev_Iz [%rx]    <= Iy
//      MOV_Ev_Gv [disp32] <= %ry
//      MOV_Ev_Iz [disp32] <= Iy
//
void X64CgAsmPass::process_STORE(Instruction* pInsn)
{
    StoreInsn* pStore = pInsn->StaticCast<StoreInsn>();

    Register* pRx = pStore->GetRx();
    Operand*  pSy = pStore->GetSy();

    Addr oAddr;
    {
        switch (pRx->GetStorage())
        {
        case Register::Storage_Pseudo:
            compute_addr(pRx->GetDfn(), &oAddr);
            break;

        case Register::Storage_Physical:
            oAddr.m_eMode = AddrMode_BaseDisp;
            oAddr.m_pRx = pRx;
            oAddr.m_ofs = 0;
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch storage
    } // oAddr

    Ty opty = ty_get_pointee(pStore->m_opty);

    if (opty == ty_int8 || opty == ty_uint8)
    {
        switch (pSy->GetKind())
        {
        case Operand::Kind_Integer:
            emit_op(op_MOV_Eb_Ib, Gpr_AL, &oAddr);
            emit_ea(opext_MOV_Eb_Ib, &oAddr);
            emit_u8(static_cast<uint8>(pStore->GetIy()));
            return;

        case Operand::Kind_Register:
        {
            Register* pRy = get_physical(pSy);
                if (NULL == pRy) return;
            emit_op(op_MOV_Eb_Gb, cast_to_reg8(pRy), &oAddr);
            emit_ea(pRy, &oAddr);
            return;
        } // register
        } // switch operand

        CAN_NOT_HAPPEN();
    } // int8, uint8

    if (opty == ty_int16 || opty == ty_uint16)
    {
        switch (pSy->GetKind())
        {
        case Operand::Kind_Integer:
            emit_op(op_MOV_Ew_Iw, Gpr_AX, &oAddr);
            emit_ea(opext_MOV_Ew_Iw, &oAddr);
            emit_u16(static_cast<uint16>(pStore->GetIy()));
            return;

        case Operand::Kind_Register:
        {
            Register* pRy = get_physical(pSy);
                if (NULL == pRy) return;
            emit_op(op_MOV_Ew_Gw, cast_to_reg16(pRy), &oAddr);
            emit_ea(pRy, &oAddr);
            return;
        } // register
        } // switch operand

        CAN_NOT_HAPPEN();
    } // int16, uint16

    if (opty == ty_float64)
    {
        Register* pRy = get_physical(pSy);
            if (NULL == pRy) return;
        emit_op(op_MOVSD_Wsd_Vsd, pRy, &oAddr);
        emit_ea(pRy, &oAddr);
        return;
    } // float64

    if (opty == ty_float32)
    {
        Register* pRy = get_physical(pSy);
            if (NULL == pRy) return;
        emit_op(op_MOVSS_Wss_Vss, pRy, &oAddr);
        emit_ea(pRy, &oAddr);
        return;
    } // float32

    if (opty == ty_int32 || opty == ty_uint32)
    {
        switch (pSy->GetKind())
        {
        case Operand::Kind_Integer:
            emit_op(op_MOV_Ev_Iz, Gpr_EAX, &oAddr);
            emit_ea(opext_MOV_Ev_Iz, &oAddr);
            emit_u32(static_cast<uint32>(pStore->GetIy()));
            return;

        case Operand::Kind_Register:
        {
            Register* pRy = get_physical(pSy);
                if (NULL == pRy) return;
            emit_op(op_MOV_Ev_Gv, cast_to_reg32(pRy), &oAddr);
            emit_ea(pRy, &oAddr);
            return;
        } // register
        } // switch operand

        CAN_NOT_HAPPEN();
    } // int32, uint32

    switch (oAddr.m_eMode)
    {
    case AddrMode_BaseDisp:
    case AddrMode_BaseIndex:
        if (pSy->Is<Register>())
        {
            Register* pRy = get_physical(pSy);
                if (NULL == pRy) return;
            emit_op(op_MOV_Ev_Gv, pRy, &oAddr);
            emit_ea(pRy, &oAddr);
        }
        else
        {
          // Set 64bit memory cell with 32bit imm.
            emit_op(op_MOV_Ev_Iz, Gpr_RAX, &oAddr);
            emit_ea(opext_MOV_Ev_Iz, &oAddr);
            emit_imm32(pSy);
        }
        break;

    case AddrMode_StackCell:
        asm_copy(oAddr.m_pRx, pSy);
        break;

    case AddrMode_TlvOffset:
        if (pSy->Is<Register>())
        {
            Reg ry = cast_to_reg(pSy->StaticCast<Register>());

            emit_op(op_MOV_Ev_Gv, ry, $rtcb);
            emit_modrm(Mod_Disp32, ry, $rtcb);
            annotate(Annon_TlvOffset, oAddr.m_pAnnon);
            emit_u32(0);
        }
        else
        {
            emit_op(op_MOV_Ev_Iz, $rtcb);
            emit_modrm(Mod_Disp32, opext_MOV_Ev_Iz, $rtcb);
            annotate(Annon_TlvOffset, oAddr.m_pAnnon);
            emit_u32(0);
            emit_imm32(pSy);
        } // if
        break;

    default:
        asm_invalid_operand(pRx);
        break;
    } // switch addr mode
} // X64CgAsmPass::process_STORE


// X64CgAsmPass::process_VALUES
void X64CgAsmPass::process_VALUES(Instruction* pInsn)
{
    CheckValues oCheckValues(pInsn);

    if (! oCheckValues.IsUsed()) return;

    process_VALUES_aux(pInsn->StaticCast<ValuesInsn>());

    if (oCheckValues.NeedNVals() || oCheckValues.ForRet())
    {
        int cOperands = pInsn->GetOperandCount();
        if (1 == cOperands)
        {
            if (oCheckValues.NeedNVals())
            {
                asm_MOV($rn, Fixnum::One);
            }

            if (oCheckValues.ForRet())
            {
                // BUGBUG: If function uses stack slot, we don't need to
                // emit CLC for RET.
                emit_op(op_CLC);
            }
        }
        else
        {
            if (0 == cOperands)
            {
                if (oCheckValues.NeedNil()) asm_MOV($r0, $rnil);
                asm_XOR($rn, $rn);
            }
            else if (1 != cOperands)
            {
                // BUGBUG: We should annotate number of values.
                asm_MOV($rn, cOperands * Fixnum::One);
            } // if

            if (oCheckValues.ForRet())
            {
                emit_op(op_STC);
            }
        }
    } // if
} // X64CgAsmPass::process_VALUES


//////////////////////////////////////////////////////////////////////
//
// process_VALUES_aux
//
//  Classify copy operation by destination and source:
//
//     dst     src     order
//     --------------------
//     mem     imm32   [1]
//     mem     reg     [1]
//     reg     reg     [2]
//     mem     mem     [3]      Need one reg.
//     mem     imm64   [3]      Need one reg.
//     reg     mem     [3]      We use load-delay-time by reg/imm.
//     reg     imm     [4]
//
void X64CgAsmPass::process_VALUES_aux(ValuesInsn* pValuesInsn)
{
    ASSERT(NULL != pValuesInsn);

    AsmCopyTaskList oAsmCopyTasks;

    // [1] mem <= imm32
    // [1] mem <= reg
    // [2] reg <= reg
    {
        uint nNth = 0;
        foreach (ValuesInsn::EnumInput, oEnum, pValuesInsn)
        {
            Operand* pSx = oEnum.Get();

            if (nNth < reg_arg_limit())
            {
                if (pSx->Is<Register>())
                {
                    Register* pRx = pSx->StaticCast<Register>();

                    if (pRx->IsPhysical())
                    {
                        // [2] reg <= reg
                        Reg iRd = reg_arg(nNth);
                        Reg iRx = static_cast<Reg>(pRx->GetLocation());

                        if (iRd != iRx)
                        {
                            oAsmCopyTasks.Push(new AsmCopyTask(
                                getCopyReg(iRd)->Reset(iRd),
                                getCopyReg(iRx)->Reset(iRx) ) );
                        } // if
                    }
                }
            }
            else
            {
                if (! pSx->Is<Register>())
                {
                    // [1] mem <= imm32
                    if (is_32bit(pSx->StaticCast<Literal>()->GetDatum()))
                    {
                        asm_MOV(tcb$mv_value(nNth), pSx);
                    }
                } // if
                else
                {
                    Register* pRx = pSx->StaticCast<Register>();
                    switch (pRx->GetStorage())
                    {
                    case Register::Storage_Closed:
                    case Register::Storage_LoadTimeValue:
                        // [1] mem <= imm
                        asm_MOV(tcb$mv_value(nNth), pSx);
                        break;

                    case Register::Storage_Physical:
                        // [1] mem <= reg
                        asm_MOV(
                            tcb$mv_value(nNth),
                            static_cast<Reg>(pRx->GetLocation()) );
                        break;
                    } // switch storage
                } // if not register
            } // if nNth < reg_arg_limit

            nNth += 1;
        } // for each input
    } // [1]

    // [2] reg <= reg
    emit_copies(&oAsmCopyTasks);

    // [3] mem <= mem
    // [3] mem <= imm64
    // [3] reg <= mem
    {
        uint nNth = 0;
        foreach (ValuesInsn::EnumInput, oEnum, pValuesInsn)
        {
            Operand*  pSx = oEnum.Get();
            Register* pRx = pSx->DynamicCast<Register>();

            if (nNth < reg_arg_limit())
            {
                if (NULL != pRx && pRx->IsStackSlot())
                {
                    // [3] reg <= mem
                    asm_load(reg_arg(nNth), pRx);
                }
            }
            else if (NULL == pRx)
            {
                // [3] mem <= imm64
                asm_MOV($rn, pSx);
                asm_MOV(tcb$mv_value(nNth), $rn);
            }
            else if (! pRx->IsPhysical())
            {
                // [3] mem <= mem
                //  MOV ecx <= [ESP+n]
                //  MOV [EBP+i] <= ecx
                asm_load($rn, pRx);
                asm_MOV(tcb$mv_value(nNth), $rn);
            } // if
            nNth += 1;
        } // for each input
    } // [3]

    // [4] reg <= imm
    {
        uint nNth = 0;
        foreach (ValuesInsn::EnumInput, oEnum, pValuesInsn)
        {
            if (nNth == reg_arg_limit()) break;

            Operand*  pSx = oEnum.Get();
            Register* pRx = pSx->DynamicCast<Register>();
            if (NULL == pRx)
            {
                // [4] reg <= imm
                asm_MOV(reg_arg(nNth), pSx);
            }
            else
            {
                switch (pRx->GetStorage())
                {
                case Register::Storage_Closed:
                case Register::Storage_LoadTimeValue:
                    // [4] reg <= imm
                    asm_MOV(reg_arg(nNth), pSx);
                    break;
                } // switch storeage
            }
            nNth += 1;
        } // for each input
    } // [4]
} // X64CgAsmPass::process_VALUES_aux


//////////////////////////////////////////////////////////////////////
//
// X64CgAsmPass::process_STACKDEF
//
void X64CgAsmPass::process_STACKDEF(Instruction* pInsn)
{
    Register* pRd = pInsn->GetRd();
        ASSERT(pRd->IsStackSlot());

    Register* pRy = pInsn->GetRy();
        ASSERT(pRy->IsPhysical());

    asm_MOV($sp, pRd->GetLocation(), pRy);
} // X64CgAsmPass::process_STACKDEF


//////////////////////////////////////////////////////////////////////
//
//  X64CgAsmPass::process_UNBOX
//
void X64CgAsmPass::process_UNBOX(Instruction* pUnbox)
{
    Register* pRd = pUnbox->GetRd();
    if (pRd->IsPseudo()) return;

    Register* pRx = get_physical(pUnbox->GetRx());
        if (NULL == pRx) return;

    Ty ty = pUnbox->GetTy();

    if (ty == ty_float64)
    {
        int ofs = offsetof(DoubleFloat, m_dbl) - DoubleFloat::Tag;
        emit_op(op_MOVSD_Vsd_Wsd, pRd, pRx);
        emit_disp(pRd, pRx, ofs);
        return;
    } // float64

    if (ty == ty_float32)
    {
        int ofs = offsetof(SingleFloat, m_flt) - SingleFloat::Tag;
        emit_op(op_MOVSS_Vss_Wss, pRd, pRx);
        emit_disp(pRd, pRx, ofs);
        return;
    } // float64

    asm_unsupported(pUnbox->GetTy());
} //  X64CgAsmPass::process_UNBOX


//////////////////////////////////////////////////////////////////////
//
//  X64CgAsmPass::process_arith
//      CMP Ev, Gv
//      CMP Gv, Ev
//      CMP eAX, Iv
//      TEST Ev, Gv     -- when CMP and Iv == 0
//
void X64CgAsmPass::process_arith(Instruction* pInsn, OpExt opext)
{
    if (opext_CMP_Ev_Iz != opext)
    {
        ASSERT(NULL != pInsn->GetRd());
        ASSERT(pInsn->GetRd()->GetStorage() == pInsn->GetRx()->GetStorage());
        ASSERT(pInsn->GetRd()->GetLocation() == pInsn->GetRx()->GetLocation());
    }

    Register* pRx = pInsn->GetRx();
    Operand*  pSy = pInsn->GetSy();

    switch (pSy->GetKind())
    {
    case Operand::Kind_Register:
    {
        Register* pRy = pSy->StaticCast<Register>();

        switch (pRy->GetStorage())
        {
        case Register::Storage_Physical:
            emit_op(op_ADD_Ev_Gv + opext * 8, pRy, pRx);
            emit_ea(pRy, pRx);
            break;

        case Register::Storage_Stack:
            ASSERT(pRx->IsPhysical());
            emit_op(op_ADD_Gv_Ev + opext * 8, pRx, pRy);
            emit_ea(pRx, pRy);
            break;
        } // switch storage
        break;
    } // Operand::Kind_Register

    case Operand::Kind_Literal:
    {
        if (pInsn->GetLy() == nil)
        {
            emit_op(op_ADD_Ev_Gv + opext * 8, $rnil, pRx);
            emit_ea($rnil, pRx);
            break;
        } // if

        if (pRx->IsPhysical())
        {
            Int imm = pSy->StaticCast<Literal>()->GetDatum()->ToInt();
            if (imm >= -128 && imm <= 127)
            {
                asm_arith_imm(opext, pRx, static_cast<int32>(imm));
                break;
            }
        } // if

        if (pRx->IsPhysical() && Gpr_RAX == pRx->GetLocation())
        {
            emit_op(op_ADD_eAX_Iz + opext * 8, Gpr_RAX);
        }
        else
        {
            emit_op(op_ADD_Ev_Iz, pRx);
            emit_ea(opext, pRx);
        } // if

        emit_imm32(pSy);
        break;
    } // Operand::Kind_Literal

    case Operand::Kind_Integer:
    {
        Int imm = pSy->StaticCast<Integer>()->GetValue();
            ASSERT(is_32bit(imm));
        asm_arith_imm(opext, pRx, static_cast<int32>(imm));
        break;
    } // Operand::Kind_Integer

    default:
        CAN_NOT_HAPPEN();
    } // switch eKind
} // X64CgAsmPass::process_arith


// X64CgAsmPass::process_addsub
void X64CgAsmPass::process_addsub(
    Instruction*    pInsn,
    OpExt           opext,
    Opcode          op64,
    Opcode          op32 )
{
    Ty ty = pInsn->GetTy();

         if (ty == ty_float64) { process_fop_sy(pInsn, op64); }
    else if (ty == ty_float32) { process_fop_sy(pInsn, op32); }
    else                       { process_arith(pInsn, opext);}
} // X64CgAsmPass::process_addsub


// X64CgAsmPass::process_fop_sx
void X64CgAsmPass::process_fop_sx(Instruction* pInsn, Opcode op)
{
    emit_op(op, pInsn->GetRd(), pInsn->GetRx());
    emit_ea(pInsn->GetRd(), pInsn->GetRx());
} // X64CgAsmPass::process_fop_sx


// X64CgAsmPass::process_fop_sy
void X64CgAsmPass::process_fop_sy(Instruction* pInsn, Opcode op)
{
    Register* pRx = pInsn->GetRx();

    if (! pRx->IsPhysical()) { asm_error_phy(pRx); return; }
    if (pRx != pInsn->GetRd()) { asm_error_2op(); return; }

    Register* pFy = pInsn->GetRy();

    emit_op(op, pRx, pFy);
    emit_ea(pRx, pFy);
} // X64CgAsmPass::process_fop


// X64CgAsmPass::process_muldiv
void X64CgAsmPass::process_muldiv(
    Instruction* pInsn, OpExt, Opcode op64, Opcode op32 )
{
    Ty ty = pInsn->GetTy();
         if (ty == ty_float64) { process_fop_sy(pInsn, op64); }
    else if (ty == ty_float32) { process_fop_sy(pInsn, op32); }
    else { asm_unsupported(ty); }
} // X64CgAsmPass::process_muldiv


//////////////////////////////////////////////////////////////////////
//
// X64CgAsmPass::process_X86X64_CMP
//
void X64CgAsmPass::process_X86X64_CMP(Instruction* pInsn)
{
    x86x64CmpInsn* pCmp = pInsn->StaticCast<x86x64CmpInsn>();
    Ty ty = pCmp->GetTy();
         if (ty == ty_float64) { process_fop_sy(pCmp, op_COMISD_Vsd_Wsd); }
    else if (ty == ty_float32) { process_fop_sy(pCmp, op_COMISS_Vss_Wss); }
    else { process_arith(pCmp, opext_CMP_Ev_Iz); }
} // X64CgAsmPass::process_X86X64_CMP


//////////////////////////////////////////////////////////////////////
//
// X64CgAsmPass::process_X86X64_CONVERT
//
void X64CgAsmPass::process_X86X64_CONVERT(Instruction* pInsn)
{
    x86x64ConvertInsn* pConvert = pInsn->StaticCast<x86x64ConvertInsn>();

    Ty ty   = pConvert->GetTy();
    Ty opty = pConvert->m_opty;

    if (ty == ty_float32)
    {
        // FIXME 2007-03-11: CONVERT/float32 How about other int types?
        if (opty == ty_int)
        {
            process_fop_sx(pInsn, op_CVTSI2SS_Vss_Eq);
            return;
        }

        if (opty == ty_float64)
        {
            process_fop_sx(pInsn, op_CVTSD2SS_Vss_Wsd);
            return;
        }
    } // float32

    if (ty == ty_float64)
    {
        // FIXME 2007-03-11: CONVERT/float64 How about other int types?
        if (opty == ty_int)
        {
            process_fop_sx(pInsn, op_CVTSI2SD_Vsd_Eq);
            return;
        }

        if (opty == ty_float32)
        {
            process_fop_sx(pInsn, op_CVTSS2SD_Vsd_Wss);
            return;
        }
    } // float64


    if (ty == ty_int)
    {
        if (opty == ty_float32)
        {
            process_fop_sx(pInsn, op_CVTSS2SI_Gd_Wss);
            return;
        }

        if (opty == ty_float64)
        {
            process_fop_sx(pInsn, op_CVTSD2SI_Gq_Wsd);
            return;
        }
    } // float64

    asm_unsupported(ty, opty);
} // X64CgAsmPass::process_X86X64_CONVERT


//////////////////////////////////////////////////////////////////////
//
// X64CgAsmPass::process_X86X64_ENCODE
//
void X64CgAsmPass::process_X86X64_ENCODE(Instruction* pEncode)
{
    Ty ty = pEncode->GetTy();
    Register* pRx = pEncode->GetRx();
    Register* pRd = pEncode->GetRd();

    if (ty == ty_float32 && NULL != pRx)
    {
        Reg rd = get_reg_for_rex(pRd);
        Reg rx = get_reg_for_rex(pRx);
        emit_op(op_MOVD_Vdq_Ed, rd, rx);
        emit_modrm(Mod_Reg, rd, rx);
        return;
    } // float32, int

    if (ty == ty_float64 && NULL != pRx)
    {
        Reg rd = get_reg_for_rex(pRd);
        Reg rx = get_reg_for_rex(pRx);
        emit_op(op_MOVQ_Vdq_Eq, rd, rx);
        emit_modrm(Mod_Reg, rd, rx);
        return;
    } // float32, int

    asm_unsupported(ty, pEncode->GetSx()->GetTy());
} // X64CgAsmPass::process_X86X64_ENCODE


//////////////////////////////////////////////////////////////////////
//
// X64CgAsmPass::process_NEG
//  float32
//      cmpeqd  %rd, %rd
//      psllq   %rd, 63
//      xorpd   %rd, %rx
void X64CgAsmPass::process_X86X64_NEG(Instruction* pNeg)
{
    Ty ty = pNeg->GetTy();

    if (ty == ty_float32)
    {
        Reg rd = cast_to_reg(pNeg->GetRd());
        Reg rx = cast_to_reg(pNeg->GetRx());
        Reg ry = cast_to_reg(pNeg->GetRy());

        emit_op(op_PCMPEQD_Vdq_Wdq, ry, ry);
        emit_modrm(Mod_Reg, ry, ry);

        emit_op(op_PSLLQ_Udq_Ib, ry, Gpr_EAX);
        emit_modrm(Mod_Reg, opext_PSLLQ_Udq_Ib, ry);
        emit_u8(31);


        if (rd == ry)
        {
            emit_op(op_XORPS_Vps_Wps, rd, rx);
            emit_modrm(Mod_Reg, rd, rx);
        }
        else if (rd == rx)
        {
            emit_op(op_XORPS_Vps_Wps, rd, ry);
            emit_modrm(Mod_Reg, rd, ry);
        }
        else
        {
            emit_op(op_XORPS_Vps_Wps, ry, rx);
            emit_modrm(Mod_Reg, ry, rx);

            emit_op(op_MOVSD_Vsd_Wsd, ry, rx);
            emit_modrm(Mod_Reg, rd, ry);
        }

        return;
    } // float32

    if (ty == ty_float64)
    {
        Reg rd = cast_to_reg(pNeg->GetRd());
        Reg rx = cast_to_reg(pNeg->GetRx());
        Reg ry = cast_to_reg(pNeg->GetRy());

        emit_op(op_PCMPEQD_Vdq_Wdq, ry, ry);
        emit_modrm(Mod_Reg, ry, ry);

        emit_op(op_PSLLQ_Udq_Ib, ry, Gpr_EAX);
        emit_modrm(Mod_Reg, opext_PSLLQ_Udq_Ib, ry);
        emit_u8(63);


        if (rd == ry)
        {
            emit_op(op_XORPD_Vpd_Wpd, rd, rx);
            emit_modrm(Mod_Reg, rd, rx);
        }
        else if (rd == rx)
        {
            emit_op(op_XORPD_Vpd_Wpd, rd, ry);
            emit_modrm(Mod_Reg, rd, ry);
        }
        else
        {
            emit_op(op_XORPD_Vpd_Wpd, ry, rx);
            emit_modrm(Mod_Reg, ry, rx);

            emit_op(op_MOVSD_Vsd_Wsd, ry, rx);
            emit_modrm(Mod_Reg, rd, ry);
        }
        return;
    } // float64

    asm_unsupported(ty, pNeg->GetSx()->GetTy());
} // X64CgAsmPass::process_NEG


//////////////////////////////////////////////////////////////////////
//
// X64CgAsmPass::process_X86X64_SERVICE
//
void X64CgAsmPass::process_X86X64_SERVICE(Instruction* pInsn)
{
    emit_op(op_CALL_Ev);
    emit_disp(
        opext_CALL_Ev,
        $rtcb, 
        pInsn->GetSx()->StaticCast<Integer>()->GetValue() );
} // X64CgAsmPass::process_X86X64_SERVICE


//////////////////////////////////////////////////////////////////////
//
//  x64-TEST int32 %rd <= %sx %sy
//
//  TEST eAX, Iv
//  TEST Ev, Gv
//  TEST Ev, Iv
//
void X64CgAsmPass::process_X86X64_TEST(Instruction* pInsn)
{
    Register* pRx = pInsn->GetSx()->StaticCast<Register>();
    Operand*  pSy = pInsn->GetSy();

    switch (pSy->GetKind())
    {
    case Operand::Kind_Register:
    {
        asm_TEST(pSy->StaticCast<Register>(), pRx);
        break;
    } // Operand::Kind_Register

    case Operand::Kind_Literal:
    {
        Val imm = pSy->StaticCast<Literal>()->GetDatum();
        if (! fixnump(imm))
        {
            asm_invalid_operand(pSy);
            return;
        }

        asm_TEST(pRx, imm->ToInt());
        break;
    } // Operand::Kind_Literal

    case Operand::Kind_Integer:
        asm_TEST(pRx, pSy->StaticCast<Integer>()->GetValue());
        break;

    default:
        asm_invalid_operand(pSy);
        break;
    } // switch eKind
} // X86X64CgAsmPass::process_X86X64_TEST


// X64CgAsmPass::process_function
void X64CgAsmPass::process_instruction(Instruction* pInsn)
{
    pInsn->SetIndex(m_oContext.GetOffset());
    InsnProcT pfn = k_rgpInsnProc[pInsn->GetOpcode()];
    (this->*pfn)(pInsn);
} // X64CgAsmPass::process_instruction


// x64_assemble
void x64_assemble()
{
    X64CgAsmPass oPass;
    oPass.Run();
} // x64_assemble

} // Compiler
