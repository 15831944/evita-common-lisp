#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86x64 - assembler
// cg/x86x64/x86x64_cg_asm.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_asm.cpp#21 $
//
#include "./x86x64_cg_asm.h"

#include "./x86x64_cg_instruction.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
//  X86X64CgAsm::compute_addr
//
//  Called by:
//      X86X64CgAsm::process_LOAD
//      X86X64CgAsm::process_STORE
//
X86X64CgAsm::AddrMode
X86X64CgAsm::compute_addr(Instruction* pInsn, Addr* pAddr)
{
    switch (pInsn->GetOpcode())
    {
    case IrOp_ELT:
        return compute_lea_addr(pInsn, pAddr);

    case IrOp_FRAME:
        if (NULL == m_pRegSP) m_pRegSP = new Physical($sp);
        pAddr->m_pRx = m_pRegSP;
        pAddr->m_ofs = static_cast<int32>(
            pInsn->GetSx()->StaticCast<Frame>()->GetLocation() +
            pInsn->GetSy()->StaticCast<Integer>()->GetValue() );
        return pAddr->m_eMode = AddrMode_BaseDisp;

    case IrOp_SLOT:
        return compute_slot_addr(pInsn, pAddr);

    case IrOp_TLV:
        pAddr->m_eAnnon = Annon_TlvOffset;
        pAddr->m_pAnnon = pInsn->GetSx();
        return pAddr->m_eMode = AddrMode_TlvOffset;

    case IrOp_VARREF: case IrOp_VARSLOT:
        pAddr->m_pRx = pInsn->GetRx();
        pAddr->m_ofs = get_upvar_offset(pInsn->GetRx(), pInsn->GetRy());
        return pAddr->m_eMode = AddrMode_BaseDisp;

    case IrOp_VECREF:
        pAddr->m_pRx = pInsn->GetRx();
        pAddr->m_ofs = pInsn->StaticCast<VecRefInsn>()->GetNth() * sizeof(Val);
        return pAddr->m_eMode = AddrMode_BaseDisp;

    case IrOp_X86X64_TCB:
        if (NULL == m_pRegTCB) m_pRegTCB = new Physical($rtcb);
        pAddr->m_pRx = m_pRegTCB;
        pAddr->m_ofs = static_cast<int32>(
            pInsn->GetSx()->StaticCast<Integer>()->GetValue() );
        return pAddr->m_eMode = AddrMode_BaseDisp;

    case IrOp_X86X64_LEA2:
    case IrOp_X86X64_LEA3:
        return compute_lea_addr(pInsn, pAddr);

    default:
        CAN_NOT_HAPPEN();
    } // switch opcode
} // X86X64CgAsm::compute_addr


//////////////////////////////////////////////////////////////////////
//
// X86X64CgAsm::compute_lea_addr
//
X86X64CgAsm::AddrMode
X86X64CgAsm::compute_lea_addr(Instruction* pLea, Addr* pAddr)
{
    pAddr->m_pRx = pLea->GetRx();

    unless (NULL != pAddr->m_pRx && pAddr->m_pRx->IsPhysical())
    {
        asm_error_phy(pLea->GetSx());
        return pAddr->m_eMode = AddrMode_Error;
    }

    //pAddr->m_ofs = offsetof(SimpleVector, mv_element) - SimpleVector::Tag;
    pAddr->m_ofs = static_cast<int>(pLea->GetIy());

    if (pLea->GetOperandCount() == 2)
    {
        return pAddr->m_eMode = AddrMode_BaseDisp;
    }

    pAddr->m_pRy = pLea->GetRz();

    unless (NULL != pAddr->m_pRy && pAddr->m_pRy->IsPhysical())
    {
        asm_error_phy(pLea->GetSx());
        return pAddr->m_eMode = AddrMode_Error;
    }

    return pAddr->m_eMode = AddrMode_BaseIndex;
} // X86X64CgAsm::compute_lea_addr


//////////////////////////////////////////////////////////////////////
//
// X86X64CgAsm::compute_slot_addr
//
X86X64CgAsm::AddrMode
X86X64CgAsm::compute_slot_addr(Instruction* pInsn, Addr* pAddr)
{
    SlotInsn* pSlot = pInsn->StaticCast<SlotInsn>();

    Val class_name = pSlot->GetLx();
    Val slot_name  = pSlot->GetLy();

    if (class_name == Qlist) class_name = Qcons;

    // NOTE: x86-ensure passs catch this, since code annotations don't
    // support arbitrary offset to object.
    if (pSlot->GetSz()->Is<Literal>())
    {
        Val sz = pSlot->GetLz();

        pAddr->m_pAnnon = pSlot->GetSz();

        if (Qsymbol == class_name && Qfunction == slot_name)
        {
            ASSERT(symbolp(sz));
            pAddr->m_eAnnon = Annon_SymFun;
            return pAddr->m_eMode = AddrMode_SymRef;
        }

        if (Qvalue_cell == class_name && Qvalue == slot_name)
        {
            ASSERT(value_cell_p(sz));
            pAddr->m_eAnnon = Annon_SymVal;
            return pAddr->m_eMode = AddrMode_SymRef;
        }

        if (Qsetf_cell == class_name && Qfunction == slot_name)
        {
            ASSERT(setf_cell_p(sz));
            pAddr->m_eAnnon = Annon_SymSetf;
            return pAddr->m_eMode = AddrMode_SymRef;
        }

        warn(L"SLOT with literal ~S.", sz);
        pAddr->m_ofs = 0;
        return pAddr->m_eMode = AddrMode_BaseDisp;
    } // if

    ASSERT(pSlot->GetSz()->Is<Register>());

    if (pSlot->GetRz()->IsClosed())
    {
        ASSERT(Qvalue == slot_name);

        Register* pCld = pSlot->GetRz();
            ASSERT(pCld->IsClosed());

        pAddr->m_pAnnon = NewLiteral(
            Fixnum::Encode(pCld->GetLocation()) );

        if (Qc6_literal_cell == class_name)
        {
            pAddr->m_eAnnon = Annon_ClosedLit;
            return pAddr->m_eMode = AddrMode_ClosedLit;
        }
        else if (Qclosed_cell == class_name)
        {
            pAddr->m_eAnnon = Annon_ClosedVar;
            return pAddr->m_eMode = AddrMode_ClosedVar;
        }
        else
        {
            CAN_NOT_HAPPEN();
        } // if
    } // closed

    pAddr->m_pRx = pSlot->GetRz();

    if (Qc6_literal_cell == class_name)
    {
        ASSERT(Qvalue == slot_name);
        return pAddr->m_eMode = AddrMode_LiteralCell;
    } // if literal-cell

    if (Qc6_stack_cell == class_name)
    {
        ASSERT(Qvalue == slot_name);
        return pAddr->m_eMode = AddrMode_StackCell;
    } // if stack-cell

    pAddr->m_ofs = 0;

    Val klass = find_class(class_name, nil, TLV(AenvironmentA));
    if (nil == klass)
    {
        warn(L"Class ~S isn't defined.", class_name);
        return pAddr->m_eMode = AddrMode_BaseDisp;
    } // if class not found

    Val instanced = klass->Decode<Class>()->m_instanced;
    if (nil == instanced)
    {
        if (CLASS_built_in_class == class_of(klass))
        {
            foreach (
                EnumList,
                oEnum,
                klass->Decode<BuiltInClass>()->m_direct_slots )
            {
                Val dslotd = oEnum.Get();
                if (dslotd->Decode<DirectSlotD>()->m_name == slot_name)
                {
                    pAddr->m_ofs -= get_tag(klass);
                    return pAddr->m_eMode = AddrMode_BaseDisp;
                }
                pAddr->m_ofs += sizeof(Val);
            } // for each dslotd
        }
        warn(L"Class ~S isn't finalized.", class_name);
    }
    else
    {
        ClassD* pClassD = instanced->Decode<ClassD>();

        foreach (EnumList, oEnum, pClassD->m_slots)
        {
            EffectiveSlotD* pSlotD = oEnum.Get()->Decode<EffectiveSlotD>();

            if (pSlotD->m_name == slot_name)
            {
                pAddr->m_ofs = static_cast<int>(
                    Fixnum::Decode_(pSlotD->m_location) );

                switch (pClassD->m_format->ToInt())
                {
                case ClassD::Format_Structure:
                    pAddr->m_ofs *= sizeof(Val);
                    pAddr->m_ofs += sizeof(Record);
                    pAddr->m_ofs -= StructureObject::Tag;
                    break;

                case ClassD::Format_FuncallableInstance:
                case ClassD::Format_Instance:
                    pAddr->m_ofs *= sizeof(Val);
                    pAddr->m_ofs += offsetof(Storage, mv_element);
                    pAddr->m_ofs -= Storage::Tag;
                    break;

                case ClassD::Format_Foreign:
                    break;

                default:
                    if (instanced != CLASSD_symbol)
                    {
                        pAddr->m_ofs -= static_cast<int>(
                            Fixnum::Decode_(pClassD->m_tag_code) );
                    }
                    break;
                } // switch format

                return pAddr->m_eMode = AddrMode_BaseDisp;
            } // if
        } // for each slotd

        warn(L"Class ~S doesn't have slot ~S.", class_name, slot_name);
    } // if

    pAddr->m_ofs = 0;
    return pAddr->m_eMode = AddrMode_BaseDisp;
} // X86X64CgAsm::compute_slot_addr


uint X86X64CgAsm::compute_annon_size(Function* pFun) const
{
    uint cbAnnon = 0;
    foreach (
        AsmFunction::Enum,
        oEnum,
        pFun->GetExtension<AsmFunction>() )
    {
        AsmAnnon* pAnnon = oEnum.Get();
        cbAnnon += 4;
        if (Annon_Data == pAnnon->GetType())
        {
            cbAnnon += 4;
        }
    } // for each annotation

    return cbAnnon;
} // X86X64CgAsm::compute_annon_size


//////////////////////////////////////////////////////////////////////
//
//  X86X64CgAsm::emit_disp
//
//  [00 reg/opext r/m]             disp == 0, r/m != EBP
//  [00 reg/opext r/m] 24          disp == 0, r/m == EBP
//  [01 reg/opext r/m] disp8       r/m != ESP
//  [01 reg/opext r/m] 24 disp8    r/m == ESP
//  [10 reg/opext r/m] disp32      r/m != ESP
//  [10 reg/opext r/m] 24 disp32   r/m == ESP
//
void
X86X64CgAsm::emit_disp(Reg eRd, Reg eRm, Int iDisp)
{
    if (0 == iDisp && Rm_Disp32 != static_cast<Rm>(eRm & 7))
    {
        emit_modrm(Mod_Disp0, eRd, eRm);
        if (Rm_SIB == static_cast<Rm>(eRm & 7)) emit_u8(0x24);
    }
    else if (iDisp >= -128 && iDisp <= 127)
    {
        emit_modrm(Mod_Disp8, eRd, eRm);
        if (Rm_SIB == static_cast<Rm>(eRm & 7)) emit_u8(0x24);
        emit_s8(static_cast<int8>(iDisp));
    }
    else
    {
        emit_modrm(Mod_Disp32, eRd, eRm);
        if (Rm_SIB == static_cast<Rm>(eRm & 7)) emit_u8(0x24);
        emit_s32(static_cast<int32>(iDisp));
    }
} // X86X64CgAsm::emit_disp


//////////////////////////////////////////////////////////////////////
//
//  X86X64CgAsm::emit_ea
//
void
X86X64CgAsm::emit_ea(Reg eRd, Register* pRm)
{
    Int iRm = pRm->GetLocation();
    switch (pRm->GetStorage())
    {
    case Register::Storage_Pseudo:
    {
        Instruction* pUnbox = pRm->GetDfn();
        switch (pUnbox->GetOpcode())
        {
        case IrOp_UNBOX:
            if (pUnbox->GetTy() == ty_float64)
            {
                emit_disp(
                    eRd,
                    get_physical(pUnbox->GetRx()),
                    offsetof(DoubleFloat, m_dbl) - DoubleFloat::Tag );
               return;
            }

            if (pUnbox->GetTy() == ty_float32)
            {
                emit_disp(
                    eRd,
                    get_physical(pUnbox->GetRx()),
                    offsetof(SingleFloat, m_flt) - SingleFloat::Tag );
               return;
            }
            break;
        } // switch opcode
        break;
    } // pseudo

    case Register::Storage_Physical:
        emit_modrm(Mod_Reg, eRd, static_cast<Reg>(iRm));
        return;

    case Register::Storage_Stack:
        emit_disp(eRd, $sp, iRm);
        return;
    } // switch storage

    CAN_NOT_HAPPEN();
} // X86X64CgAsm::emit_ea


//////////////////////////////////////////////////////////////////////
//
//  X86X64CgAsm::emit_ea
//
void X86X64CgAsm::emit_ea(Reg rd, const Addr* pAddr)
{
    switch (pAddr->m_eMode)
    {
    case AddrMode_BaseDisp:
        emit_disp(rd, cast_to_reg(pAddr->m_pRx), pAddr->m_ofs);
        break;

    case AddrMode_BaseIndex:
        emit_index(
            rd, 
            cast_to_reg(pAddr->m_pRx), 
            cast_to_reg(pAddr->m_pRy), 
            pAddr->m_ofs );
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch eMode
} // X86X64CgAsm::emit_ea

//////////////////////////////////////////////////////////////////////
//
//  X86X64CgAsm::emit_index
//
void
X86X64CgAsm::emit_index(Reg eRd, Reg eRb, Reg eRi, Int iDisp)
{
    if (0 == iDisp && Rm_Disp32 != static_cast<Rm>(eRb & 7))
    {
        emit_modrm(Mod_Disp0, eRd, Rm_SIB);
        emit_u8(Ss(Scale_1, eRi, eRb));
    }
    else if (iDisp >= -128 && iDisp <= 127)
    {
        emit_modrm(Mod_Disp8, eRd, Rm_SIB);
        emit_u8(Ss(Scale_1, eRi, eRb));
        emit_s8(static_cast<int8>(iDisp));
    }
    else
    {
        emit_modrm(Mod_Disp32, eRd, Rm_SIB);
        emit_u8(Ss(Scale_1, eRi, eRb));
        emit_s32(static_cast<int32>(iDisp));
    }
} // X86X64CgAsm::emit_index


// emit_op
void X86X64CgAsm::emit_op(uint nOp)
{
    if (nOp <= 0xFF)
    {
        emit_u8(static_cast<uint8>(nOp));
    }
    else if (nOp <= 0xFFFF)
    {
        emit_u8(static_cast<uint8>(nOp >> 8));
        emit_u8(static_cast<uint8>(nOp));
    }
    else if (nOp <= 0xFFFFFF)
    {
        emit_u8(static_cast<uint8>(nOp >> 16));
        emit_u8(static_cast<uint8>(nOp >> 8));
        emit_u8(static_cast<uint8>(nOp));
    }
    else
    {
        CAN_NOT_HAPPEN();
    }
} // X86X64CgAsm::emit_op


// X86X64CgAsm::get_physical
Register* X86X64CgAsm::get_physical(Register* pRx)
{
    if (pRx->IsPhysical()) return pRx;
    asm_error_phy(pRx);
    return NULL;
} // X86X64CgAsm::get_physical


// X86X64CgAsm::get_tttn
Tttn X86X64CgAsm::get_tttn(Bool* pBx)
{
    x86x64SetCcInsn* pSetCc = pBx->GetDfn()->StaticCast<x86x64SetCcInsn>();

    Tttn eTttn = static_cast<Tttn>(
        pSetCc->GetSx()->StaticCast<Integer>()->GetValue() );

    return eTttn;
} // X86X64CgAsm::get_tttn


// CheckValues constructor
X86X64CgAsm::CheckValues::CheckValues(Instruction* pInsn) :
    m_nFlags(0)
{
    foreach (Values::Enum, oEnum, pInsn->GetVd())
    {
        OperandBox* pBox = oEnum.Get();
        Instruction* pUser = pBox->GetInstruction();

        switch (pUser->GetOpcode())
        {
        case IrOp_CALL:
        {
            Function* pCallee = pUser->GetSx()->
                DynamicCast<Function>();

            if (NULL == pCallee || pCallee->NeedArity())
            {
                m_nFlags |= Flag_nvals;
            }

            m_nFlags |= Flag_use;
            break;
        } // call

        case IrOp_CLOSURE:
            m_nFlags |= Flag_nvals | Flag_use;
            break;

        case IrOp_COUNT:
            m_nFlags |= Flag_nil0 | Flag_nvals | Flag_use;
            break;

        case IrOp_MVSAVE:
        {
            bool fFound = false;
            Instruction* pRunner = pUser;
            do
            {
                pRunner = pRunner->GetNext();
                if (pRunner->Is<MvRestoreInsn>())
                {
                    CheckValues oCheck(pRunner);
                    m_nFlags |= oCheck.m_nFlags | Flag_use;
                    fFound = true;
                    break;
                }
            } while (pRunner != pUser);
            if (! fFound) warn(L"CheckValues: broken MVSAVE.");
            break;
        } // mvsave

        case IrOp_NTHVALUE:
            m_nFlags |= Flag_nil0 | Flag_nvals | Flag_use;
            break;

        case IrOp_OPENFINALLY:
            break;

        case IrOp_PHI:
        {
            CheckValues oCheckValues(pUser);
            m_nFlags |= oCheckValues.m_nFlags | Flag_use;
            break;
        } // phi

        case IrOp_PROJECT:
            m_nFlags |= Flag_nil0 | Flag_use;
            break;

        case IrOp_RET:
            m_nFlags |= Flag_ret | Flag_nil0 | Flag_use;
            break;

        case IrOp_RETURNFROM:
        case IrOp_THROW:
            m_nFlags |= Flag_nvals | Flag_use;
            break;

        case IrOp_X86X64_SERVICE:
            m_nFlags |= Flag_nvals | Flag_use;
            break;

        default:
            warn(L"CheckValues: doesn't expect ~A.",
                make_string(pUser->GetMnemonic()) );
            break;
        } // switch opcode
    } // for each
} // X86X64CgAsm::CheckValues::CheckValues


// asm_error_2op
void X86X64CgAsm::asm_error_2op()
{
    asm_error(L"Not two operands operation.");
} // X86X64CgAsm::asm_error_2op


void X86X64CgAsm::asm_error(
    const char16*   pwsz,
    Val             param1,
    Val             param2 )
{
    char16 wsz[200];
        ::wsprintf(wsz, L"cg-asm: BB%u:%u:%ls: %ls",
            Fixnum::Decode_(m_pInsn->GetBBlock()->GetName()),
            m_pInsn->GetIndex(),
            m_pInsn->GetMnemonic(),
            pwsz );
    warn(wsz, param1, param2);
} // X86X64CgAsm::asm_error


// asm_error_phy
void X86X64CgAsm::asm_error_phy(
    Operand*        pSx )
{
    switch (pSx->GetKind())
    {
    case Operand::Kind_Register:
        asm_error(L"%r~D must be physical.",
            pSx->StaticCast<Register>()->GetName() );
        break;

    case Operand::Kind_Literal:
        asm_error(L"~S must be physical.",
            pSx->StaticCast<Literal>()->GetDatum() );
        break;

    default:
        asm_error(L"Expect physical regiser.");
        break;
    } // switch operand
} // X86X64CgAsm::asm_error_phy


// asm_error_vreg
void X86X64CgAsm::asm_error_vreg(Register* pRx)
{
    asm_error(L"RA doesn't allocate %r~D.", pRx->GetName());
} // X86X64CgAsm::asm_error_vreg


// asm_invalid_operand
void X86X64CgAsm::asm_invalid_operand(Operand*)
{
    asm_error(L"Invalid operand.");
} // X86X64CgAsm::asm_invalid_operand


// asm_unsupported
void X86X64CgAsm::asm_unsupported(Val ty)
{
    asm_error(L"~S isn't supported.", ty);
} // X86X64CgAsm::asm_unsupported


// asm_unsupported
void X86X64CgAsm::asm_unsupported(Val ty1, Val ty2)
{
    asm_error(L"~S and ~S isn't supported.", ty1, ty2);
} // X86X64CgAsm::asm_unsupported

} // Compiler
