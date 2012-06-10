#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x86 - Remote Call
// arch/x86/kernel/x86_ke_remote_call.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/kernel/x64_ke_interrupt.cpp#7 $
//
// Description:
//  This file contains implementaiton of following Generic frame methods:
//      FinallyFrame::Unwind
//      XferFrame::Transfer
//
#include "../../x86x64/kernel/x86x64_ke_interrupt.h"

namespace MiniLisp
{
    Val copy_code_annotation(Val, Val, FunObj::Annon);
}

namespace Kernel
{

using namespace MiniLisp;
using namespace X64;

static const Reg k_rgnSave[] = {
    Gpr_EAX, Gpr_ECX, Gpr_EDX, Gpr_EBX,                   Gpr_ESI, Gpr_EDI,
    Gpr_R8,  Gpr_R9,  Gpr_R10, Gpr_R11, Gpr_R12, Gpr_R13, Gpr_R14, Gpr_R15
}; // k_rgnSave

static int const REX_W = 0x48;

// make_detour_call
Val X86X64Interrupt::make_detour_call(
    Val     fn,
    uint    curr_ofs,
    uint    safe_ofs,
    Val     callee )
{
    // 0000 83 EC 04            SUB ESP, 4  ; make function frame
    // ---- GC Desc: StdCall
    // 0003 E8 xx xx xx xx      CALL list ; save arguments into list
    // 0008 89 04 24            MOV [ESP], EAX  ; store it
    // 000B 33 C9               XOR ECX, ECX
    // ---- GC Desc: StdCall [sp+0]
    // 000D E8 xx xx xx xx      CALL keyboard-interrupt
    // 0012 8B 04 24            MOV EAX, [ESP]  ; restore arguments
    // 0015 83 C4 04            ADD ESP, 4
    // 0018 B9 04 00 00 00      MOV ECX, '1
    // 001D E9 xx xx xx xx      JMP values-list
    // 0022
    uint cbCodeVec = safe_ofs - curr_ofs + 0x22;
    uint cbGcMap = (1 + 1 + 2 + 2) * 4;
    uint cbAnnon = sizeof(FunObj::Annon) * 3;
    uint cbFrame = 4 + 4;

    foreach (FunObj::EnumAnnon, oEnum, fn->Decode<FunObj>())
    {
        FunObj::Annon oAnnon = oEnum.Get();
        if (oAnnon.m_ofs >= safe_ofs) break;
        if (oAnnon.m_ofs >= curr_ofs) cbAnnon += 4;
    } // for each annon

    Val detour = allocate_funobj(
        CLASSD_native_code_function,
        cbCodeVec,
        cbAnnon,
        cbGcMap,
        FunObj::FrameType_Fixed,
        cbFrame );

    FunObj* pFunObj = detour->Decode<FunObj>();

    uint8* pbCodeVec = pFunObj->GetCodeVec();

    ::memcpy(
        pbCodeVec,
        fn->Decode<FunObj>()->GetCodeVec() + curr_ofs,
        safe_ofs - curr_ofs );

    FunObj::Annon* pAnnon = pFunObj->GetAnnon();

    foreach (FunObj::EnumAnnon, oEnum, fn->Decode<FunObj>())
    {
        FunObj::Annon oAnnon = oEnum.Get();
        if (oAnnon.m_ofs >= safe_ofs) break;
        if (oAnnon.m_ofs >= curr_ofs) copy_code_annotation(detour, fn, oAnnon);
    } // for each annon

    uint8* pbCode = pbCodeVec + safe_ofs - curr_ofs;

    pFunObj->GetGcMap()[0] = 1;
    pFunObj->GetGcMap()[1] = 2;

    *pbCode++ = op_SUB_Ev_Ib;
        *pbCode++ = ModRm(Mod_Reg, opext_SUB_Ev_Ib, $sp);
        *pbCode++ = sizeof(Val);

    // Populate GC Map
    {
        pFunObj->GetGcMap()[2] =
            (4 << 16) |
            static_cast<uint32>(pbCode - pFunObj->GetCodeVec());
        // StdCall: [sp+0]
        pFunObj->GetGcMap()[4] = (GcDesc_StdCall << 1);
    }

    *pbCode++ = op_CALL_Jv;
        *pAnnon++ = set_callee(pFunObj, pbCode, Qlist);
        pbCode += 4;

    *pbCode++ = op_MOV_Ev_Gv;
        *pbCode++ = ModRm(Mod_Disp0, $r0, $sp);
        *pbCode++ = 0x24;

    *pbCode++ = op_XOR_Gv_Ev;
        *pbCode++ = ModRm(Mod_Reg, $rn, $rn);

    // Populate GC Map
    {
        pFunObj->GetGcMap()[3] =
            (5 << 16) |
            static_cast<uint32>(pbCode - pFunObj->GetCodeVec());
        // StdCall: [sp+0]
        pFunObj->GetGcMap()[5] = (GcDesc_StdCall << 1) | 8;
    }

    *pbCode++ = op_CALL_Jv;
        *pAnnon++ = set_callee(pFunObj, pbCode, callee);
        pbCode += 4;

    *pbCode++ = op_MOV_Gv_Ev;
        *pbCode++ = ModRm(Mod_Disp0, $r0, $sp);
        *pbCode++ = 0x24;

    *pbCode++ = op_ADD_Ev_Ib;
        *pbCode++ = ModRm(Mod_Reg, opext_ADD_Ev_Ib, $sp);
        *pbCode++ = sizeof(Val);

    *pbCode++ = op_MOV_eAX_Iv + ($rn - $r0);
        *reinterpret_cast<uint32*>(pbCode) = Fixnum::One;
            pbCode += 4;

    *pbCode++ = op_JMP_Jv;
        *pAnnon++ = set_callee(pFunObj, pbCode, Qvalues);
        pbCode += 4;

    ASSERT(static_cast<uint>(pbCode - pbCodeVec) == cbCodeVec);

    detour->Decode<FunObj>()->m_name = nil;
    detour->Decode<FunObj>()->m_nCookie = FunObj::Cookie;

    return detour;
} // X86X64Interrupt::make_detour_call


// make_detour_jump
Val X86X64Interrupt::make_detour_jump(
    Val     fn,
    uint    curr_ofs,
    uint    safe_ofs,
    uint32  gcdesc,
    Val     callee )
{
    // 0000 9C                  PUSHF   ; for Jcc
    // 0001 83 EC 19            SUB ESP, 24
    // 0004 89 44 24 00         MOV [ESP+0], EAX
    // 0008 89 54 24 04         MOV [ESP+4], EDX
    // 000C 89 5C 24 08         MOV [ESP+8], EBX
    // 0010 89 74 24 0C         MOV [ESP+12], ESI
    // 0014 89 7C 24 10         MOV [ESP+16], EDI
    // 0018 89 4C 24 14         MOV [ESP+20], ECX
    // 001C 33 C9               XOR ECX, ECX
    // ---- GC Desc:
    // 001E E8 xx xx xx xx      CALL {callee}
    // 0023 8B 44 24 00         MOV EAX, [ESP+0]
    // 0027 8B 54 24 04         MOV EDX, [ESP+4]
    // 002B 8B 5C 24 08         MOV EBX, [ESP+8]
    // 002F 8B 74 24 0C         MOV ESI, [ESP+12]
    // 0033 8B 7C 24 10         MOV EDI, [ESP+16]
    // 0037 8B 4C 24 14         MOV ECX, [ESP+20]
    // 003B 83 C4 18            ADD ESP, 24
    // 003E 9D                  POPF
    // 003F C3                  RET
    // 0040
    uint cbCodeVec = safe_ofs - curr_ofs + 0x40;
    uint cbAnnon = sizeof(FunObj::Annon) * 1;
    uint cbGcMap = 4 + 8 + 4;
    uint cbFrame = 4 + 24 + 4;

    foreach (FunObj::EnumAnnon, oEnum, fn->Decode<FunObj>())
    {
        FunObj::Annon oAnnon = oEnum.Get();
        if (oAnnon.m_ofs >= safe_ofs) break;
        if (oAnnon.m_ofs >= curr_ofs) cbAnnon += 4;
    } // for each annon

    Val detour = allocate_funobj(
        CLASSD_native_code_function,
        cbCodeVec,
        cbAnnon,
        cbGcMap,
        FunObj::FrameType_Fixed,
        cbFrame );

    FunObj* pFunObj = detour->Decode<FunObj>();

    uint8* pbCodeVec = pFunObj->GetCodeVec();

    ::memcpy(
        pbCodeVec,
        fn->Decode<FunObj>()->GetCodeVec() + curr_ofs,
        safe_ofs - curr_ofs );

    FunObj::Annon* pAnnon = pFunObj->GetAnnon();

    foreach (FunObj::EnumAnnon, oEnum, fn->Decode<FunObj>())
    {
        FunObj::Annon oAnnon = oEnum.Get();
        if (oAnnon.m_ofs >= safe_ofs) break;
        if (oAnnon.m_ofs >= curr_ofs) copy_code_annotation(detour, fn, oAnnon);
    } // for each annon

    pFunObj->GetGcMap()[0] = 1;
    pFunObj->GetGcMap()[1] = 1;

    uint8* pbCode = pbCodeVec + safe_ofs - curr_ofs;

    {
        *pbCode++ = op_PUSHFD;

        *pbCode++ = op_SUB_Ev_Ib;
            *pbCode++ = ModRm(Mod_Reg, opext_SUB_Ev_Ib, $sp);
            *pbCode++ = sizeof(Val) * lengthof(k_rgnSave);

        for (int i = 0; i < lengthof(k_rgnSave); i++)
        {
            *pbCode++ = op_MOV_Ev_Gv;
                *pbCode++ = ModRm(Mod_Disp8, k_rgnSave[i], $sp);
                *pbCode++ = static_cast<uint8>(sizeof(Val) * i);
        } // for i

        *pbCode++ = op_XOR_Gv_Ev;
            *pbCode++ = ModRm(Mod_Reg, $rn, $rn);

        // Populate GC Map
        {
            pFunObj->GetGcMap()[2] =
                (3 << 16) |
                static_cast<uint32>(pbCode - pFunObj->GetCodeVec());

            // StdCall: [sp+n], ..., [sp+m]
            uint32 word = GcDesc_StdCall << 1;
                word |= gcdesc >> (3 + 6);
            pFunObj->GetGcMap()[3] = (GcDesc_StdCall << 1) | 8;
        }
        *pbCode = op_CALL_Jv;
            *pAnnon++ = set_callee(pFunObj, pbCode, callee);
            pbCode += 4;

        for (int i = 0; i < lengthof(k_rgnSave); i++)
        {
            *pbCode++ = op_MOV_Gv_Ev;
                *pbCode++ = ModRm(Mod_Disp8, k_rgnSave[i], $sp);
                *pbCode++ = static_cast<uint8>(sizeof(Val) * i);
        } // for i

        *pbCode++ = op_ADD_Ev_Ib;
            *pbCode++ = ModRm(Mod_Reg, opext_ADD_Ev_Ib, $sp);
            *pbCode++ = sizeof(Val) * lengthof(k_rgnSave);

        *pbCode++ = op_POPFD;
        *pbCode++ = op_RET;
    } // code
    ASSERT(static_cast<uint>(pbCode - pbCodeVec) == cbCodeVec);

    detour->Decode<FunObj>()->m_name    = nil;
    detour->Decode<FunObj>()->m_nCookie = FunObj::Cookie;

    return detour;
} // X86X64Interrupt::make_detour_jump



// make_detour_ra
Val X86X64Interrupt::make_detour_ra(Val fn, UInt ip, Val callee)
{
    // 0000 48 0F 43 4D F8     CMOVNB    RCX, $tcb.SVC_fixnum_one
    // 0005 48 83 EC 10        SUB       RSP, 16
    // 0009 48 89 04 24        MOV       [RSP], RAX
    // 000D 48 B8 01 0B 24 F0  MOV       RAX, 'FN
    // 0013 03 00 00 00
    // 0017 48 05 34 12 00 00  ADD       RAX, #x1234
    // 001D 48 89 44 24 08     MOV       [RSP+8], RAX
    // 0022 48 8B 04 24        MOV       RAX, [RSP]
    // 0026 E8 F5 D2 12 00     CALL      #'LIST
    // 002B 48 89 04 24        MOV       [RSP], RAX
    // 002F 48 33 C9           XOR       RCX, RCX
    // 0032 E8 69 00 00 00     CALL      #'CALLEE
    // 0037 48 8B 04 24        MOV       RAX, [RSP]
    // 003B 48 83 C4 08        ADD       RSP, 8
    // 003F 48 C7 C1 08 00 00  MOV       RCX, '1
    // 0045 00
    // 0046 E9 6E 49 D8 FF     JMP       #'VALUES-LIST
    // 004B

    uint cbCodeVec = 0x4B;
    uint cbAnnon   = sizeof(FunObj::Annon) * 3;
    uint cbGcMap   = 4 + 4 + 4 + 4;
    uint cbFrame   = sizeof(Val) * 2;

    Val detour = allocate_funobj(
        CLASSD_native_code_function,
        cbCodeVec,
        cbAnnon,
        cbGcMap,
        FunObj::FrameType_Fixed,
        cbFrame );

    FunObj* pFunObj = detour->Decode<FunObj>();

    pFunObj->GetGcMap()[0] = 1;
    pFunObj->GetGcMap()[1] = 2;

    FunObj::Annon* pAnnon = pFunObj->GetAnnon();

    uint8* pbCodeVec = pFunObj->GetCodeVec();
    uint8* pbCode = pbCodeVec;
    {
        //  0000 48 0F 43 4D F8     CMOVNB RCX, $tcb.SVC_fixnum_one
        *pbCode++ = REX_W;
        *pbCode++ = static_cast<uint8>(op_CMOVNC_Gv_Ev >> 8);
        *pbCode++ = static_cast<uint8>(op_CMOVNC_Gv_Ev & 0xFF);
            *pbCode++ = ModRm(Mod_Disp8, $rn, $rtcb);
            *pbCode++ = (0 - 8) & 0xFF;

        //  0005 48 83 E9 10        SUB       RSP, 16
        *pbCode++ = REX_W;
        *pbCode++ = op_SUB_Ev_Ib;
            *pbCode++ = ModRm(Mod_Reg, opext_SUB_Ev_Ib, $sp);
            *pbCode++ = 16;

        //  0009 48 89 14 24        MOV       [RSP], RAX
        *pbCode++ = REX_W;
        *pbCode++ = op_MOV_Ev_Gv;
            *pbCode++ = ModRm(Mod_Disp0, $r0, $sp);
            *pbCode++ = 0x24;

        //  000D 48 B8 xx xx xx xx  MOV       RAX, {fn}
        //  0013 xx xx xx xx
        *pbCode++ = REX_W;
        *pbCode++ = 0xB8;
            *reinterpret_cast<Val*>(pbCode) = fn;
            pbCode += sizeof(Val);

        //  0017 48 05 xx xx xx xx  ADD       RAX, {rel}
        *pbCode++ = REX_W;
        *pbCode++ = 0x05;
            *reinterpret_cast<int32*>(pbCode) = static_cast<int32>(
                ip - fn->ToInt() );
            pbCode += sizeof(int32);

        //  001D 48 89 04 24        MOV       [RSP+8], RAX
        *pbCode++ = REX_W;
        *pbCode++ = op_MOV_Ev_Gv;
            *pbCode++ = ModRm(Mod_Disp8, $r0, $sp);
            *pbCode++ = 0x24;
            *pbCode++ = 8;

        //  0022 48 8B 04 24        MOV       RAX, [RSP]
        *pbCode++ = REX_W;
        *pbCode++ = op_MOV_Gv_Ev;
            *pbCode++ = ModRm(Mod_Disp0, $r0, $sp);
            *pbCode++ = 0x24;

        //  0026 E8 52 1C F7 FF     CALL      #'LIST
        *pbCode++ = op_CALL_Jv;
            *pAnnon++ = set_callee(pFunObj, pbCode, Qlist);
            pbCode += 4;

        //  002B 48 89 14 24        MOV       [RSP], RAX
        *pbCode++ = REX_W;
        *pbCode++ = op_MOV_Ev_Gv;
            *pbCode++ = ModRm(Mod_Disp0, $r0, $sp);
            *pbCode++ = 0x24;

        //  002F 48 33 C9           XOR       RCX, RCX
        *pbCode++ = REX_W;
        *pbCode++ = op_XOR_Gv_Ev;
            *pbCode++ = ModRm(Mod_Reg, $rn, $rn);

        // Populate GC Map
        {
            pFunObj->GetGcMap()[2] =
                (3 << 16) |
                static_cast<uint32>(pbCode - pFunObj->GetCodeVec());
            // StdCall: [sp+0]
            pFunObj->GetGcMap()[3] = (GcDesc_StdCall << 1);
        }

        //  0032 E8 F6 93 FF FF     CALL      #'CALLEE
        *pbCode++ = op_CALL_Jv;
            *pAnnon++ = set_callee(pFunObj, pbCode, callee);
            pbCode += 4;

        //  0037 48 8B 04 24        MOV       RAX, [RSP]
        *pbCode++ = REX_W;
        *pbCode++ = op_MOV_Gv_Ev;
            *pbCode++ = ModRm(Mod_Disp0, $r0, $sp);
            *pbCode++ = 0x24;

        //  003B 48 83 C4 10        ADD       RCX, 8
        *pbCode++ = REX_W;
        *pbCode++ = op_ADD_Ev_Ib;
            *pbCode++ = ModRm(Mod_Reg, opext_ADD_Ev_Ib, $sp);
            *pbCode++ = 8;

        //  003F 48 C7 C1 08 00 00  MOV       RCX, '1
        //  0045 00
        *pbCode++ = REX_W;
        *pbCode++ = op_MOV_Ev_Iz;
            *pbCode++ = ModRm(Mod_Reg, opext_MOV_Ev_Iz, $rn);
            *reinterpret_cast<int32*>(pbCode) = Fixnum::One;
            pbCode += sizeof(int32);

        // 0046 E9 xx xx xx xx  JMP values-list
        *pbCode++ = op_JMP_Jv;
            *pAnnon++ = set_callee(pFunObj, pbCode, Qvalues_list);
            pbCode += 4;
    } // code

    ASSERT(static_cast<uint>(pbCode - pbCodeVec) == cbCodeVec);

    pFunObj->m_name    = Qquote;
    pFunObj->m_nCookie = FunObj::Cookie;

    return detour;
} // X86X64Interrupt::make_detour_ra


// GetIP
UInt X86X64Interrupt::GetIP(CONTEXT* pContext)
{
    return pContext->Rip;
} // GetIP

// GetSP
UInt* X86X64Interrupt::GetSP(CONTEXT* pContext)
{
    return reinterpret_cast<UInt*>(static_cast<UInt>(pContext->Rsp));
} // GetSP

// Push
void X86X64Interrupt::Push(CONTEXT* pContext, UInt datum)
{
    pContext->Rsp -= sizeof(Val);
    reinterpret_cast<UInt*>(static_cast<UInt>(pContext->Rsp))[0] = datum;
}  // X86X64Interrupt::Push

// SetIP
void X86X64Interrupt::SetIP(CONTEXT* pContext, uint8* ip)
{
    pContext->Rip = static_cast<DWORD>(reinterpret_cast<UInt>(ip));
} // SetIP

} // Kernel
