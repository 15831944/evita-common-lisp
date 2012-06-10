#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - x86 - assembler - instruction
// compiler/cg/x86_cg_asm_insn.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/compiler/x86_cg_asm.cpp#32 $
//
#include "../../x86x64/compiler/x86x64_cg_asm.h"
#include "./x86_cg_target.h"
#include "./x86_cg_instruction.h"

#include "../../../compiler/cm/cm_module.h"
#include "../../../compiler/ir/ir_bblock.h"
#include "../kernel/x86_ke_frame.h"
#include "../kernel/x86_ke_thread.h"

namespace Compiler
{

using namespace X86;

// X86CgAsm
class X86CgAsm : public X86X64CgAsm
{
    protected: AsmCopyReg m_rgoCopyReg[8];

    protected: AsmCopyReg* getCopyReg(Reg iRx)
        { return &m_rgoCopyReg[iRx & 7]; }

    protected: X86CgAsm(const char16* pwsz) : X86X64CgAsm(pwsz) {}

    // emit_Iz
    protected: void emit_Iz(Operand*);

    ////////////////////////////////////////////////////////////
    //
    // Instructions
    //

    // asm_arith_imm
    void asm_arith_imm(OpExt opext, Register* pRx, Int i)
        { asm_arith_imm(opext, cast_to_reg(pRx), i); }

    void asm_arith_imm(OpExt, Reg, Int);

    // CALL callee
    protected: void asm_CALL(Val callee)
    {
        ASSERT(symbolp(callee));

        emit_op(op_CALL_Jv);
        annotate(Annon_NamedCallee, NewLiteral(callee));
        emit_u32(0);
    } // asm_CALL

    // CMOV
    protected: void asm_CMOV(Tttn tttn, Reg rd, Reg rb, Int disp32)
        { emit_op(op_CMOVcc_Gv_Ev + tttn); emit_disp(rd, rb, disp32); }

    protected: void asm_CMOV(Tttn tttn, Register* pRd, Register* pRx)
        { emit_op(op_CMOVcc_Gv_Ev + tttn); emit_ea(pRd, pRx); }

    ////////////////////////////////////////////////////////////
    //
    // LEA
    //

    // LEA rd <= [rb+disp]
    protected: void asm_LEA(Reg rd, Reg rb, Int disp32)
        { emit_op(op_LEA_Gv_M); emit_disp(rd, rb, disp32); }

    // LEA rd <= [rb+disp]
    protected: void asm_LEA(Register* pRd, Reg rb, Int disp32)
        { asm_LEA(cast_to_reg(pRd), rb, disp32); }

    // LEA rd <= [rb+ri+disp]
    protected: void asm_LEA(Register* pRd, Reg rb, Reg ri, Int disp32)
        { asm_LEA(cast_to_reg(pRd), rb, ri, disp32); }

    // LEA rd <= [rb+ri+disp]
    protected: void asm_LEA(Reg rd, Reg rb, Reg ri, Int disp32)
        { emit_op(op_LEA_Gv_M); emit_index(rd, rb, ri, disp32); }

    ////////////////////////////////////////////////////////////
    //
    // MOV
    //

    // MOV rd <= rx
    protected: void asm_MOV(Reg rd, Reg rx)
        { emit_op(op_MOV_Gv_Ev); emit_modrm(Mod_Reg, rd, rx); }

    // MOV rd <= [rb+disp]
    protected: void asm_MOV(Register* pRd, Reg rb, Int disp32)
        { asm_MOV(cast_to_reg(pRd), rb, disp32); }

    // MOV rd <= Iz
    protected: void asm_MOV(Reg rd, Val lit)
        { asm_MOV(rd, NewLiteral(lit)); }

    // MOV rd <= Iz
    protected: void asm_MOV(Reg rd, Operand* pSx)
        { emit_op(op_MOV_eAX_Iv + (rd & 7)); emit_Iz(pSx); }

    // MOV rd <= Iz
    protected: void asm_MOV(Reg rd, int32 imm32)
        { emit_op(op_MOV_eAX_Iv + (rd & 7)); emit_s32(imm32); }

    protected: void asm_MOV(uint rd, Operand* pSx)
        { asm_MOV(static_cast<Reg>(rd), pSx); }

    // MOV rd <= [rb+disp]
    protected: void asm_MOV(Reg rd, Reg rb, Int disp32)
        { emit_op(op_MOV_Gv_Ev); emit_disp(rd, rb, disp32); }

    protected: void asm_MOV(Register* pRd, Register* pRb, Int disp32)
        { emit_op(op_MOV_Gv_Ev); emit_disp(pRd, pRb, disp32); }

    // MOV [rb+disp] <= Iz
    protected: void asm_MOV(Reg rb, Int disp32, Operand* pSx)
    {
        emit_op(op_MOV_Ev_Iz);
        emit_disp(opext_MOV_Ev_Iz, rb, disp32);
        emit_Iz(pSx);
    } // emit_MOV

    // MOV [rb+disp] <= rx
    protected: void asm_MOV(Reg rb, Int disp32, Reg rx)
        { emit_op(op_MOV_Ev_Gv); emit_disp(rx, rb, disp32); }

    // MOV [rb+disp] <= imm32
    protected: void asm_MOV(Reg rb, Int disp32, int32 imm32)
    {
        emit_op(op_MOV_Ev_Iz);
        emit_disp(opext_MOV_Ev_Iz, rb, disp32);
        emit_s32(imm32);
    } // asm_MOV

    // MOV [rb+disp] <- lit
    protected: void asm_MOV(Reg rb, Int disp32, Val lit)
        { asm_MOV(rb, disp32, NewLiteral(lit)); }

    private: void asm_MOV(uint, uint) {}

    // tcb$mv_value
    protected: struct tcb$mv_value
        { uint m_i; tcb$mv_value(int i) : m_i(i) {} };

    // MOV tcb.mv_value[i] <= rx
    protected: void asm_MOV(tcb$mv_value ea, Reg rx)
        { asm_MOV($rtcb, offsetof(Thread, mv_value[ea.m_i]), rx); }

    // MOV tcb.mv_value[i] <= Iz
    protected: void asm_MOV(tcb$mv_value ea, Operand* pSy)
        { asm_MOV($rtcb, offsetof(Thread, mv_value[ea.m_i]), pSy); }

    ////////////////////////////////////////////////////////////
    //
    // SHx
    //

    // SHx rx, ry
    protected: void asm_SHx(Instruction*, OpExt, Int);
    protected: void asm_SHx(Instruction*, OpExt);

    ////////////////////////////////////////////////////////////
    //
    // TEST
    //

    // TEST rx, ry
    protected: void asm_TEST(Reg rx, Reg ry)
        { emit_op(op_TEST_Ev_Gv); emit_modrm(Mod_Reg, rx, ry); }

    // TEST Ev, Gv
    protected: void asm_TEST(Register* pRx, Register* pRy)
        { emit_op(op_TEST_Ev_Gv); emit_ea(pRx, pRy); }

    protected: void asm_TEST(Register* pRx, Int iImm)
    {
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
                    emit_op(op_TEST_eAX_Iz);
                    emit_s32(static_cast<int32>(iImm));
                }
            }
            else if ( (iImm >= 0 && iImm <= 255) &&
                     (rx == Gpr_EBX || rx == Gpr_ECX || rx == Gpr_EDX) )
            {
                emit_op(op_TEST_Eb_Ib);
                emit_modrm(Mod_Reg, opext_TEST_Eb_Ib, rx);
                emit_u8(static_cast<uint8>(iImm));
            }
            else
            {
                emit_op(op_TEST_Ev_Iz);
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
                emit_op(op_TEST_Ev_Iz);
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
            emit_op(op_XCHG_eAX_eAX + (ry & 7));
        }
        else if (Gpr_EAX == ry)
        {
            emit_op(op_XCHG_eAX_eAX + (rx & 7));
        }
        else
        {
            emit_op(op_XCHG_Ev_Gv);
            emit_modrm(Mod_Reg, rx, ry);
        } // if
    } // asm_XCHG

    ////////////////////////////////////////////////////////////
    //
    // XOR
    //

    // XOR rd, rx
    protected: void asm_XOR(Reg rd, Reg rx)
        { emit_op(op_XOR_Gv_Ev); emit_modrm(Mod_Reg, rd, rx); }

}; // X86CgAsm


//////////////////////////////////////////////////////////////////////
//
// X86CgAsmPass
//
class X86CgAsmPass : public X86X64AsmPass<X86Target, X86CgAsm>
{
    public: X86CgAsmPass() :
        X86X64AsmPass<X86Target, X86CgAsm>(L"X86-ASM") {}

    protected: virtual void process_instruction(Instruction*);

    void process_VALUES_aux(ValuesInsn*);

    ////////////////////////////////////////////////////////////
    //
    // ISA
    //
    static Reg reg_arg(uint k)
        { return static_cast<Reg>(X86Mach::ISA.m_pGprArg->Get(k)); }

    static uint reg_arg_limit()
        { return X86Mach::ISA.m_pGprArg->GetLength(); }

    ////////////////////////////////////////////////////////////
    //
    // Instructions
    //
    typedef void (X86CgAsmPass::*InsnProcT)(Instruction*);
    static const InsnProcT k_rgpInsnProc[IrOp_MAX_1 + 1];

    void process_BOX(Instruction*);
    void process_COPY(Instruction*);
    void process_LOAD(Instruction*);
    void process_NTHVALUE(Instruction*);
    void process_PROLOGUE(Instruction*);
    void process_RET(Instruction*);
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
            { process_arith(pInsn, opext_##mp_OPCODE##_Ev_Iz); }

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
        if (ty == ty_int16)   return op_MOVSX_Gv_Ew;
        if (ty == ty_int8)    return op_MOVSX_Gv_Eb;
        if (ty == ty_uint16)  return op_MOVZX_Gv_Ew;
        if (ty == ty_uint8)   return op_MOVZX_Gv_Eb;
        return op_MOV_Gv_Ev;
    } // get_load_op
}; // X86CgAsmPass

// Dispatch table
const X86CgAsmPass::InsnProcT
X86CgAsmPass::k_rgpInsnProc[IrOp_MAX_1 + 1] =
{
    #define DEFIROP(mp_name) &X86CgAsmPass::process_##mp_name,
    #include "./x86_cg_opcode.inc"
}; // X86CgAsmPass::k_rgpInsnProc


//////////////////////////////////////////////////////////////////////
//
// X86CgAsm::asm_arith_imm
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
void X86CgAsm::asm_arith_imm(OpExt opext, Reg rx, Int imm)
{
    if (0 == imm)
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
    else if (imm >= -128 && imm <= 127)
    {
        emit_op(op_ADD_Ev_Ib);
        emit_modrm(Mod_Reg, opext, rx);
        emit_s8(static_cast<int8>(imm));
    }
    else if (Gpr_EAX == rx)
    {
        emit_op(op_ADD_eAX_Iz + opext * 8);
        emit_s32(static_cast<int32>(imm));
    }
    else
    {
        emit_op(op_ADD_Ev_Iz);
        emit_modrm(Mod_Reg, opext, rx);
        emit_s32(static_cast<int32>(imm));
    } // if
} // X86CgAsm::asm_arith_imm


//////////////////////////////////////////////////////////////////////
//
// X86CgAsm::asm_SHx
//
void X86CgAsm::asm_SHx(Instruction* pInsn, OpExt opext)
{
    unless (pInsn->GetRy()->IsPhysical() &&
            pInsn->GetRy()->GetLocation() == Gpr_ECX )
    { 
        asm_invalid_operand(pInsn->GetSy());
        return;
    }

    Opcode op = op_SAR_Ev_CL;
    if (pInsn->GetTy() == ty_uint32 && opext_SAR_Ev_CL == opext)
        { op = op_SHR_Ev_CL; opext = opext_SHR_Ev_CL; }

    emit_op(op);
    emit_ea(opext, pInsn->GetRx());
} // X86CgAsm::asm_SHx


//////////////////////////////////////////////////////////////////////
//
// X86CgAsm::asm_SHx
//
void X86CgAsm::asm_SHx(Instruction* pInsn, OpExt opext, Int nK)
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

        emit_op(op);
        emit_ea(opext, pInsn->GetRx());
    }
    else
    {
        Opcode op = op_SAR_Ev_Ib;
        if (pInsn->GetTy() == ty_uint32 && opext_SAR_Ev_Ib == opext)
            { op = op_SHR_Ev_Ib; opext = opext_SHR_Ev_Ib; }

        emit_op(op);
        emit_ea(opext, pInsn->GetRx());
        emit_u8(static_cast<uint8>(nK));
    }
} // X86CgAsm::asm_SHx


//////////////////////////////////////////////////////////////////////
//
//  X86CgAsm::emit_Iz
//
void
X86CgAsm::emit_Iz(Operand* pSx)
{
    ASSERT(NULL != pSx);
    switch (pSx->GetKind())
    {
    case Operand::Kind_Literal:
        annotate(Annon_LispVal, pSx);
        emit_u32(0);
        break;

    case Operand::Kind_Function:
        annotate(Annon_Function, pSx);
        emit_u32(0);
        break;

    case Operand::Kind_FunLit:
        annotate(Annon_Function, pSx->StaticCast<FunLit>()->GetFun());
        emit_u32(0);
        break;

    case Operand::Kind_Integer:
    {
        Integer* pIx = pSx->StaticCast<Integer>();
        emit_u32(static_cast<uint32>(pIx->GetValue()));
        break;
    } // Kind_Integer

    case Operand::Kind_Label:   // for Block, Catch, Tagbody frame
        annotate(Annon_ExitPoint, pSx);
        emit_u32(0);
        break;

    case Operand::Kind_Register:
    {
        Register* pRx = pSx->StaticCast<Register>();
        switch (pRx->GetStorage())
        {
        case Register::Storage_Closed:
            annotate(
                Annon_ClosedLit,
                NewLiteral(Fixnum::Encode(pRx->GetLocation())) );
            emit_u32(0);
            break;

        case Register::Storage_LoadTimeValue:
            annotate(Annon_LispVal, pRx->GetDfn()->GetSx());
            emit_u32(0);
            break;

        default:
            warn(L"Invalid operand");
            break;
        } // switch storage
        break;
    } // Kind_Regiser

    case Operand::Kind_TlvName:
        annotate(
            Annon_TlvOffset,
            NewLiteral(pSx->StaticCast<TlvName>()->GetDatum()) );
        emit_u32(0);
        break;

    default:
        asm_invalid_operand(pSx);
        break;
    } // switch kind
} // X86CgAsm::emit_Iz


//////////////////////////////////////////////////////////////////////
//
//  X86CgAsmPass::process_BOX
//      CALL box-float64
//
void X86CgAsmPass::process_BOX(Instruction* pBox)
{
    if (pBox->GetTy() == ty_double_float)
    {
        if (pBox->GetRx()->GetLocation() != $f0)
        {
            // double-float expect xmm0.
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
            // single-float expect xmm0
            asm_invalid_operand(pBox->GetSx());
            return;
        }

        emit_op(op_CALL_Jv);
        annotate(Annon_NamedCallee, NewLiteral(QDbox_float32));
        emit_u32(0);
        return;
    } // single-float

    asm_unsupported(pBox->GetTy());
} //  X86CgAsmPass::process_BOX


//////////////////////////////////////////////////////////////////////
//
//  X86CgAsmPass::process_COPY
//
void X86CgAsmPass::process_COPY(Instruction* pCopy)
{
    Register* pRd = pCopy->GetRd();

    if (pCopy->GetTy() == ty_float32)
    {
        switch (pRd->GetStorage())
        {
        case Register::Storage_Physical:
            emit_op(op_MOVSS_Vss_Wss);
            emit_ea(pRd, pCopy->GetRx());
            return;

        case Register::Storage_Stack:
            emit_op(op_MOVSS_Wss_Vss);
            emit_ea(pCopy->GetRx(), pRd);
            return;
        } // switch storage

        asm_error_vreg(pRd);
        return;
    } // if float32

    if (pCopy->GetTy() == ty_float64)
    {
        switch (pRd->GetStorage())
        {
        case Register::Storage_Physical:
            emit_op(op_MOVSD_Vsd_Wsd);
            emit_ea(pRd, pCopy->GetRx());
            return;

        case Register::Storage_Stack:
            emit_op(op_MOVSD_Wsd_Vsd);
            emit_ea(pCopy->GetRx(), pRd);
            return;
        } // switch storage

        asm_error_vreg(pRd);
        return;
    } // if float64

    asm_copy(pRd, pCopy->GetSx());
} //  X86CgAsmPass::process_COPY


//////////////////////////////////////////////////////////////////////
//
//  X86CgAsmPass::process_LOAD
//
//  LOAD ty %rd <= %sx
//
//      MOV_Gv_Ev %rd <= [%rx]
//      MOV_Gv_Ev %rd <= [disp32]
//
void X86CgAsmPass::process_LOAD(Instruction* pLoad)
{
    Register* pRd = pLoad->GetRd();

    if (pRd->IsPseudo())
    {
        // Load cell from frame.
        return;
    }

    Register*  pRx = pLoad->GetRx();

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
            break;
        } // switch storage
    } // oAddr

    switch (oAddr.m_eMode)
    {
    case AddrMode_BaseDisp:
    case AddrMode_BaseIndex:
        emit_op(get_load_op(pLoad->GetTy()));
        emit_ea(pRd, &oAddr);
        break;

    case AddrMode_ClosedLit:
        emit_op(op_MOV_Ev_Iz);
        emit_modrm(Mod_Reg, opext_MOV_Ev_Iz, pRd);
        annotate(Annon_ClosedLit, oAddr.m_pAnnon);
        emit_u32(0);
        break;

    case AddrMode_ClosedVar:
    case AddrMode_SymRef:
        emit_op(op_MOV_Gv_Ev);
        emit_modrm(Mod_Disp0, pRd, Rm_Disp32);
        annotate(oAddr.m_eAnnon, oAddr.m_pAnnon);
        emit_u32(0);
        break;

    case AddrMode_LiteralCell:
    case AddrMode_StackCell:
        asm_copy(pRd, oAddr.m_pRx);
        break;

    case AddrMode_TlvOffset:
        emit_op(op_MOV_Gv_Ev);
        emit_modrm(Mod_Disp32, pRd, $rtcb);
        annotate(oAddr.m_eAnnon, oAddr.m_pAnnon);
        emit_u32(0);
        break;

    default:
        warn(L"LOAD has invalid operand.");
        break;
    } // switch addr mode
} // X86CgAsmPass::process_LOAD


//////////////////////////////////////////////////////////////////////
//
// X86CgAsmPass::process_NTHVALUE
//      mov [tcb]mv_value[4] <= $r0
//      mov $r4 <= pSx
//      call x86::.nth-value
//
void X86CgAsmPass::process_NTHVALUE(Instruction* pInsn)
{
    Values* pVy = pInsn->GetSy()->StaticCast<Values>();
    if (pVy->GetDfn()->Is<CallInsn>())
    {
        asm_CMOV(tttn_NC, $rn, $rtcb, SVC_fixnum_one);
    }

    const int k = reg_arg_limit() - 1;
    Reg rk = reg_arg(k);
    asm_MOV(tcb$mv_value(k), rk);
    asm_copy(rk, pInsn->GetSx());
    asm_CALL(QDnth_value);
} // X86CgAsmPass::process_NTHVALUE


//////////////////////////////////////////////////////////////////////
//
// X86CgAsmPass::process_PROLOGUE
//
void X86CgAsmPass::process_PROLOGUE(Instruction* pInsn)
{
    ASSERT(NULL != pInsn);
    ASSERT(pInsn->Is<PrologueInsn>());

    if (Knone != pInsn->GetLy())
    {
        asm_MOV(
            $rtcb, offsetof(Thread, m_fn),
            m_oContext.GetFunction()->GetArityMax() * Fixnum::One );

        Val restify = nil;
        {
            if (Kheap == pInsn->GetLy())
            {
                restify = QDrestify;
            }
            else if (Kstack == pInsn->GetLy())
            {
                restify = QDstack_restify;
            }
            else
            {
                asm_invalid_operand(pInsn->GetSy());
            } // if
        } // restify

        asm_CALL(restify);
    } // if

    // Make function frame
    {
        size_t cbFrame = m_oContext.GetFunction()->GetFrameSize();
               cbFrame -= sizeof(Val);

        html_log_format(4, L"PROLOGUE: frame size = ~D~:%", cbFrame);

        if (0 != cbFrame)
        {
            asm_arith_imm(opext_SUB_Ev_Iz, $sp, cbFrame);
        }
    }
} // X86CgAsmPass::process_PROLOGUE


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
// X86CgAsmPass::process_RET
//
//  1. Set values
//  2. Set value count
//  3. Destroy function frame
//      o ADD ESP, frame_size
//      o MOV ESP, [ESP]    ... for stack-listify
//  4. emit RET
//
void X86CgAsmPass::process_RET(Instruction* pInsn)
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

    if (! m_oContext.GetFunction()->IsStackRestify())
    {
        if (fSingleValue)
        {
            // Cleare CF
            if (0 == cbFrame)
            {
                if (need_CLC(pInsn))
                {
                    emit_op(op_CLC);
                }
            }
            else
            {
                asm_arith_imm(opext_ADD_Ev_Iz, $sp, cbFrame);
            } // if
        }
        else
        {
            // Preserve CF
            if (0 != cbFrame)
            {
                asm_LEA($sp, $sp, cbFrame);
            } // if
        }
    }
    else
    {
        if (fSingleValue && need_CLC(pInsn))
        {
            emit_op(op_CLC);
        }

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
} // X86CgAsmPass::process_RET


//////////////////////////////////////////////////////////////////////
//
// X86CgAsmPass::process_STORE
//
//      STORE %rx, %sy
//
//  Emit:
//      MOV_Ev_Gv [%rx]    <= %ry
//      MOV_Ev_Iz [%rx]    <= Iy
//      MOV_Ev_Gv [disp32] <= %ry
//      MOV_Ev_Iz [disp32] <= Iy
//
void X86CgAsmPass::process_STORE(Instruction* pInsn)
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
            asm_invalid_operand(pRx);
            break;
        } // switch storage
    } // oAddr

    Ty opty = ty_get_pointee(pStore->m_opty);

    if (opty == ty_int8 || opty == ty_uint8)
    {
        switch (pSy->GetKind())
        {
        case Operand::Kind_Integer:
            emit_op(op_MOV_Eb_Ib);
            emit_ea(opext_MOV_Eb_Ib, &oAddr);
            emit_u8(static_cast<uint8>(pStore->GetIy()));
            return;

        case Operand::Kind_Register:
        {
            Register* pRy = get_physical(pSy);
                if (NULL == pRy) return;
            emit_op(op_MOV_Eb_Gb);
            emit_ea(pRy, &oAddr);
            return;
        } // register
        } // switch operand

        asm_invalid_operand(pSy);
    } // int8, uint8

    if (opty == ty_int16 || opty == ty_uint16)
    {
        switch (pSy->GetKind())
        {
        case Operand::Kind_Integer:
            emit_op(op_MOV_Ew_Iw);
            emit_ea(opext_MOV_Ew_Iw, &oAddr);
            emit_u16(static_cast<uint16>(pStore->GetIy()));
            break;

        case Operand::Kind_Register:
        {
            Register* pRy = get_physical(pSy);
                if (NULL == pRy) return;
            emit_op(op_MOV_Ew_Gw);
            emit_ea(pRy, &oAddr);
            break;
        } // register

        default:
            asm_invalid_operand(pSy);
            break;
        } // switch operand

        return;
    } // int16, uint16

    if (opty == ty_float64)
    {
        Register* pRy = get_physical(pSy);
            if (NULL == pRy) return;
        emit_op(op_MOVSD_Wsd_Vsd);
        emit_ea(pRy, &oAddr);
        return;
    } // float64

    if (opty == ty_float32)
    {
        Register* pRy = get_physical(pSy);
            if (NULL == pRy) return;
        emit_op(op_MOVSS_Wss_Vss);
        emit_ea(pRy, &oAddr);
        return;
    } // float32

    switch (oAddr.m_eMode)
    {
    case AddrMode_BaseDisp:
    case AddrMode_BaseIndex:
        if (pSy->Is<Register>())
        {
            Register* pRy = get_physical(pSy);
                if (NULL == pRy) return;
            emit_op(op_MOV_Ev_Gv);
            emit_ea(pRy, &oAddr);
        }
        else
        {
            emit_op(op_MOV_Ev_Iz);
            emit_ea(opext_MOV_Ev_Iz, &oAddr);
            emit_Iz(pSy);
        }
        break;

    case AddrMode_ClosedVar:
    case AddrMode_SymRef:
        if (pSy->Is<Register>())
        {
            Register* pRy = get_physical(pSy);
                if (NULL == pRy) return;
            emit_op(op_MOV_Ev_Gv);
            emit_modrm(Mod_Disp0, pRy, Rm_Disp32);
            annotate(oAddr.m_eAnnon, oAddr.m_pAnnon);
            emit_u32(0);
        }
        else
        {
            emit_op(op_MOV_Ev_Iz);
            emit_modrm(Mod_Disp0, opext_MOV_Ev_Iz, Rm_Disp32);
            annotate(oAddr.m_eAnnon, oAddr.m_pAnnon);
            emit_u32(0);
            emit_Iz(pSy);
        } // if
        break;

    case AddrMode_TlvOffset:
        if (pSy->Is<Register>())
        {
            Register* pRy = get_physical(pSy);
                if (NULL == pRy) return;
            emit_op(op_MOV_Ev_Gv);
            emit_modrm(Mod_Disp32, pRy, $rtcb);
            annotate(Annon_TlvOffset, oAddr.m_pAnnon);
            emit_u32(0);
        }
        else
        {
            emit_op(op_MOV_Ev_Iz);
            emit_modrm(Mod_Disp32, opext_MOV_Ev_Iz, $rtcb);
            annotate(Annon_TlvOffset, oAddr.m_pAnnon);
            emit_u32(0);
            emit_Iz(pSy);
        } // if
        break;

    case AddrMode_StackCell:
        asm_copy(oAddr.m_pRx, pSy);
        break;

    default:
        warn(L"STORE has invalid operand.");
        break;
    } // switch addr mode
} // X86CgAsmPass::process_STORE


// X86CgAsmPass::process_VALUES
void X86CgAsmPass::process_VALUES(Instruction* pInsn)
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
                if (oCheckValues.NeedNil()) asm_MOV($r0, nil);
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
} // X86CgAsmPass::process_VALUES


//////////////////////////////////////////////////////////////////////
//
// process_VALUES_aux
//
//  Classify copy operation by destination and source:
//
//     dst     src     order
//     --------------------
//     mem     imm     [1]
//     mem     reg     [1]
//     reg     reg     [2]
//     mem     mem     [3]      Need one reg.
//     reg     mem     [3]      We use load-delay-time by reg/imm.
//     reg     imm     [4]
//
void X86CgAsmPass::process_VALUES_aux(ValuesInsn* pValuesInsn)
{
    ASSERT(NULL != pValuesInsn);

    AsmCopyTaskList oAsmCopyTasks;

    // [1] mem <= imm
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
                    // [1] mem <= imm
                    asm_MOV(tcb$mv_value(nNth), pSx);
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
    // [3] reg <= mem
    {
        uint nNth = 0;
        foreach (ValuesInsn::EnumInput, oEnum, pValuesInsn)
        {
            Register* pRx = oEnum.Get()->DynamicCast<Register>();

            if (NULL != pRx && pRx->IsStackSlot())
            {
                if (nNth < reg_arg_limit())
                {
                    // [4] reg <= mem
                    asm_load(reg_arg(nNth), pRx);
                }
                else
                {
                    // [3] mem <= mem
                    //  MOV ecx <= [ESP+n]
                    //  MOV [EBP+i] <= ecx
                    asm_load($rn, pRx);
                    asm_MOV(tcb$mv_value(nNth), $rn);
                }
            } // if
            nNth += 1;
        } // for each input
    } // [3]

    // [4] reg <= imm
    {
        uint nNth = 0;
        foreach (ValuesInsn::EnumInput, oEnum, pValuesInsn)
        {
            Operand* pSx = oEnum.Get();

            if (! pSx->Is<Register>())
            {
                // [4] reg <= imm
                asm_MOV(reg_arg(nNth), pSx);
            }
            else
            {
                Register* pRx = pSx->StaticCast<Register>();
                switch (pRx->GetStorage())
                {
                case Register::Storage_Closed:
                case Register::Storage_LoadTimeValue:
                    asm_MOV(reg_arg(nNth), pRx);
                    break;
                } // switch storage
            } // if

            nNth += 1;
            if (nNth == reg_arg_limit()) break;
        } // for each input
    } // [4]
} // X86CgAsmPass::process_VALUES_aux


//////////////////////////////////////////////////////////////////////
//
// X86CgAsmPass::process_STACKDEF
//
void X86CgAsmPass::process_STACKDEF(Instruction* pInsn)
{
    Register* pRd = pInsn->GetRd();
        ASSERT(pRd->IsStackSlot());

    Register* pRy = pInsn->GetRy();

    switch (pRy->GetStorage())
    {
    case Register::Storage_Physical:
        asm_MOV($sp, pRd->GetLocation(), pRy);
        break;

    case Register::Storage_Closed:
    {
        emit_op(op_MOV_Ev_Iz);
        emit_disp(opext_MOV_Ev_Iz, $sp, pRd->GetLocation());
        annotate(
            Annon_ClosedLit,
            NewLiteral(Fixnum::Encode(pRy->GetLocation())) );
        emit_u32(0);
        break;
    } // Storage_Pseudo

    default:
        asm_invalid_operand(pRy);
        break;
    } // switch storage
} // X86CgAsmPass::process_STACKDEF


//////////////////////////////////////////////////////////////////////
//
//  X86CgAsmPass::process_UNBOX
//
void X86CgAsmPass::process_UNBOX(Instruction* pUnbox)
{
    Register* pRd = pUnbox->GetRd();
    if (pRd->IsPseudo()) return;

    Register* pRx = get_physical(pUnbox->GetRx());
        if (NULL == pRx) return;

    Ty ty = pUnbox->GetTy();

    if (ty == ty_float64)
    {
        emit_op(op_MOVSD_Vsd_Wsd);
        emit_disp(
            pRd,
            pRx,
            offsetof(DoubleFloat, m_dbl) - DoubleFloat::Tag );
        return;
    } // float64

    if (ty == ty_float32)
    {
        emit_op(op_MOVSS_Vss_Wss);
        emit_disp(
            pRd,
            pRx,
            offsetof(SingleFloat, m_flt) - SingleFloat::Tag );
        return;
    } // float64

    asm_unsupported(pUnbox->GetTy());
} //  X86CgAsmPass::process_UNBOX


//////////////////////////////////////////////////////////////////////
//
//  X86CgAsmPass::process_arith
//      CMP Ev, Gv
//      CMP Gv, Ev
//      CMP eAX, Iv
//      TEST Ev, Gv     -- when CMP and Iv == 0
//
void X86CgAsmPass::process_arith(Instruction* pInsn, OpExt opext)
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
            emit_op(op_ADD_Ev_Gv + opext * 8);
            emit_ea(pRy, pRx);
            break;

        case Register::Storage_Stack:
            if (NULL == get_physical(pRx)) return;
            emit_op(op_ADD_Gv_Ev + opext * 8);
            emit_ea(pRx, pRy);
            break;

        default:
            asm_invalid_operand(pSy);
            break;
        } // switch storage
        break;
    } // Operand::Kind_Register

    case Operand::Kind_Literal:
    {
        if (pRx->IsPhysical())
        {
            Int imm = pSy->StaticCast<Literal>()->GetDatum()->ToInt();
            if (imm >= -128 && imm <= 127)
            {
                asm_arith_imm(opext, pRx, static_cast<int32>(imm));
                break;
            }
        } // if

        if (pRx->IsPhysical() && $r0 == pRx->GetLocation())
        {
            emit_op(op_ADD_eAX_Iz + opext * 8);
        }
        else
        {
            emit_op(op_ADD_Ev_Iz);
            emit_ea(opext, pRx);
        } // if

        emit_Iz(pSy);
        break;
    } // Operand::Kind_Literal

    case Operand::Kind_Integer:
    {
        Int imm = pSy->StaticCast<Integer>()->GetValue();
        if (pRx->IsPhysical())
        {
            asm_arith_imm(opext, pRx, imm);
        }
        else if (imm >= -128 && imm <= 127)
        {
            emit_op(op_ADD_Ev_Ib);
            emit_ea(opext, pRx);
            emit_s8(static_cast<int8>(imm));
        }
        else
        {
            emit_op(op_ADD_Ev_Iz);
            emit_ea(opext, pRx);
            emit_s32(static_cast<int32>(imm));
        } // if
        break;
    } // Operand::Kind_Integer

    default:
        asm_invalid_operand(pSy);
        break;
    } // switch eKind
} // X86CgAsmPass::process_arith


// X86CgAsmPass::process_addsub
void X86CgAsmPass::process_addsub(
    Instruction*    pInsn,
    OpExt           opext,
    Opcode          op64,
    Opcode          op32 )
{
    Ty ty = pInsn->GetTy();
         if (ty == ty_float64) { process_fop_sy(pInsn, op64); }
    else if (ty == ty_float32) { process_fop_sy(pInsn, op32); }
    else                       { process_arith(pInsn, opext);}
} // X86CgAsmPass::process_addsub


// X86CgAsmPass::process_fop_sx
void X86CgAsmPass::process_fop_sx(Instruction* pInsn, Opcode op)
{
    emit_op(op);
    emit_ea(pInsn->GetRd(), pInsn->GetRx());
} // X86CgAsmPass::process_fop_sx


// X86CgAsmPass::process_fop_sy
void X86CgAsmPass::process_fop_sy(Instruction* pInsn, Opcode op)
{
    emit_op(op);
    emit_ea(pInsn->GetRx(), pInsn->GetRy());
} // X86CgAsmPass::process_fop_sy


// X86CgAsmPass::process_muldiv
void X86CgAsmPass::process_muldiv(
    Instruction* pInsn, OpExt, Opcode op64, Opcode op32 )
{
    Ty ty = pInsn->GetTy();
         if (ty == ty_float64) { process_fop_sy(pInsn, op64); }
    else if (ty == ty_float32) { process_fop_sy(pInsn, op32); }
    else { asm_unsupported(ty); }
} // X86CgAsmPass::process_muldiv


// X86CgAsmPass::process_X86X64_CMP
void X86CgAsmPass::process_X86X64_CMP(Instruction* pInsn)
{
    x86x64CmpInsn* pCmp = pInsn->StaticCast<x86x64CmpInsn>();
    Ty opty = pCmp->m_opty;
    if (opty == ty_float64) { process_fop_sy(pCmp, op_COMISD_Vsd_Wsd); }
    else if (opty == ty_float32) { process_fop_sy(pCmp, op_COMISS_Vss_Wss); }
    else { process_arith(pCmp, opext_CMP_Ev_Iz); }
} // X86CgAsmPass::process_X86X64_CMP


//////////////////////////////////////////////////////////////////////
//
// X86CgAsmPass::process_X86X64_CONVERT
//
void X86CgAsmPass::process_X86X64_CONVERT(Instruction* pInsn)
{
    x86x64ConvertInsn* pConvert = pInsn->StaticCast<x86x64ConvertInsn>();

    Ty ty   = pConvert->GetTy();
    Ty opty = pConvert->m_opty;

    if (ty == ty_float32)
    {
        if (opty == ty_int)
        {
            process_fop_sx(pInsn, op_CVTSI2SS_Vss_Ed);
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
        if (opty == ty_int)
        {
            process_fop_sx(pInsn, op_CVTSI2SD_Vsd_Ed);
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
            process_fop_sx(pInsn, op_CVTSD2SI_Gd_Wsd);
            return;
        }
    } // float64

    asm_unsupported(ty, opty);
} // X86CgAsmPass::process_X86X64_CONVERT


//////////////////////////////////////////////////////////////////////
//
// X86CgAsmPass::process_X86X64_ENCODE
//
void X86CgAsmPass::process_X86X64_ENCODE(Instruction* pEncode)
{
    Ty ty = pEncode->GetTy();
    Register* pRx = pEncode->GetRx();
    Register* pRd = pEncode->GetRd();

    if (ty == ty_float32 && NULL != pRx)
    {
        Reg rd = cast_to_reg(pRd);
        Reg rx = cast_to_reg(pRx);
        emit_op(op_MOVD_Vdq_Ed);
        emit_modrm(Mod_Reg, rd, rx);
        return;
    } // float32, int

    if (ty == ty_float64 && NULL != pRx)
    {
        Reg rd = cast_to_reg(pRd);
        Reg rx = cast_to_reg(pRx);

        emit_op(op_MOVD_Vdq_Ed);
        emit_modrm(Mod_Reg, rd, rx);

        emit_op(op_PSLLQ_Udq_Ib);
        emit_modrm(Mod_Reg, opext_PSLLQ_Udq_Ib, rd);
        emit_u8(32);

        return;
    } // float32, int

    asm_unsupported(ty, pEncode->GetSx()->GetTy());
} // X86CgAsmPass::process_X86X64_ENCODE


//////////////////////////////////////////////////////////////////////
//
// X86CgAsmPass::process_NEG
//  float32
//      cmpeqd  %rd, %rd
//      psllq   %rd, 63
//      xorpd   %rd, %rx
void X86CgAsmPass::process_X86X64_NEG(Instruction* pNeg)
{
    Ty ty = pNeg->GetTy();

    if (ty == ty_float32)
    {
        Reg rd = cast_to_reg(pNeg->GetRd());
        Reg rx = cast_to_reg(pNeg->GetRx());
        Reg ry = cast_to_reg(pNeg->GetRy());

        emit_op(op_PCMPEQD_Vdq_Wdq);
        emit_modrm(Mod_Reg, ry, ry);

        emit_op(op_PSLLQ_Udq_Ib);
        emit_modrm(Mod_Reg, opext_PSLLQ_Udq_Ib, ry);
        emit_u8(31);

        emit_op(op_XORPS_Vps_Wps);

        if (rd == ry)
        {
            emit_modrm(Mod_Reg, rd, rx);
        }
        else if (rd == rx)
        {
            emit_modrm(Mod_Reg, rd, ry);
        }
        else
        {
            emit_modrm(Mod_Reg, ry, rx);

            emit_op(op_MOVSS_Vss_Wss);
            emit_modrm(Mod_Reg, rd, ry);
        }

        return;
    } // float32

    if (ty == ty_float64)
    {
        Reg rd = cast_to_reg(pNeg->GetRd());
        Reg rx = cast_to_reg(pNeg->GetRx());
        Reg ry = cast_to_reg(pNeg->GetRy());

        emit_op(op_PCMPEQD_Vdq_Wdq);
        emit_modrm(Mod_Reg, ry, ry);

        emit_op(op_PSLLQ_Udq_Ib);
        emit_modrm(Mod_Reg, opext_PSLLQ_Udq_Ib, ry);
        emit_u8(63);

        emit_op(op_XORPD_Vpd_Wpd);

        if (rd == ry)
        {
            emit_modrm(Mod_Reg, rd, rx);
        }
        else if (rd == rx)
        {
            emit_modrm(Mod_Reg, rd, ry);
        }
        else
        {
            emit_modrm(Mod_Reg, ry, rx);

            emit_op(op_MOVSD_Vsd_Wsd);
            emit_modrm(Mod_Reg, rd, ry);
        }
        return;
    } // float64

    asm_unsupported(ty, pNeg->GetSx()->GetTy());
} // X86CgAsmPass::process_NEG


//////////////////////////////////////////////////////////////////////
//
// X86CgAsmPass::process_X86X64_SERVICE
//
void X86CgAsmPass::process_X86X64_SERVICE(Instruction* pInsn)
{
    emit_op(op_CALL_Ev);
    emit_disp(
        opext_CALL_Ev,
        $rtcb, 
        pInsn->GetSx()->StaticCast<Integer>()->GetValue() );
} // X86CgAsmPass::process_X86X64_SERVICE


//////////////////////////////////////////////////////////////////////
//
//  x86-CMP int32 %rd <= %sx %sy
//
//  TEST eAX, Iv
//  TEST Ev, Gv
//  TEST Ev, Iv
//
void X86CgAsmPass::process_X86X64_TEST(Instruction* pInsn)
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
} // X86CgAsmPass::process_X86X64_TEST


// X86CgAsmPass::process_function
void X86CgAsmPass::process_instruction(Instruction* pInsn)
{
    pInsn->SetIndex(m_oContext.GetOffset());
    InsnProcT pfn = k_rgpInsnProc[pInsn->GetOpcode()];
    (this->*pfn)(pInsn);
} // X86CgAsmPass::process_instruction


// x86_assemble
void x86_assemble()
{
    X86CgAsmPass oPass;
    oPass.Run();
} // x86_assemble

} // Compiler
