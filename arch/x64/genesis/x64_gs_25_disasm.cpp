#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 25 Environment - Disassembler
// arch/x64/x64_gs_25_disasm.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/genesis/x64_gs_25_disasm.cpp#16 $
//
#include "../x64_arch.h"
#include "../x64_opcode.h"

#include "../../../genesis/gs_lisp.h"

#include "../kernel/x64_ke_gcmap.h"
#include "../kernel/x64_ke_layout.h"
#include "../kernel/x64_ke_mach.h"
#include "../kernel/x64_ke_thread.h"

#include "../../../kernel/ke_dll_link.h"

#include "../../x86x64/genesis/x86x64_gs_25_disasm.h"

namespace Genesis
{

using namespace X64;

static LPCWSTR k_rgpwszTCB[256];


//////////////////////////////////////////////////////////////////////
//
// X64Disassembler
//
class X64Disassembler : public X86X64Disassembler
{
    static Format sm_oFormat_DQ;

    enum Rex
    {
        Rex_W = 0x08,
        Rex_R = 0x04,
        Rex_X = 0x02,
        Rex_B = 0x01,
    }; // Rex

    uint m_nRex;

    public: X64Disassembler(Val thing) :
        X86X64Disassembler(thing) {}

    // disasm_gcmap
    virtual void disasm_gcmap(uint ofs)
    {
        if (m_oGcMapEnum.AtEnd()) return;
        if (m_oGcMapEnum.GetOffset() != ofs) return;

        const uint32* p = m_oGcMapEnum.GetDesc();
            m_oGcMapEnum.Next();

        format(m_stream, L";  ----");

        for (;;)
        {
            format(m_stream, L" ~X~X",
                Fixnum::Encode(*p >> 4),
                Fixnum::Encode(*p & 15) );
            if (0 == (*p & 1)) break;
            p++;
        } // for

        format(m_stream, L"~%");
    } // disasm_gcmap

    // decode_insn
    virtual const Format* decode_insn(Context* pContext)
    {
        m_nAdrSiz   = 64;
        m_nOpdSiz   = 32;
        m_rgfPrefix = 0;
        m_nModRm    = NoModRm;
        m_nRex      = 0;

        if (pContext->FetchAnnon() == FunObj::Annon::Type_Label)
        {
            return &sm_oFormat_DB;
        }

        uint nOpcode = decode_opcode(pContext);
        if (m_nRex & Rex_W)
        {
            const Format* pFormat = get_format(pContext, 0x48000000 | nOpcode);
            if (&sm_oFormat_DB != pFormat) return pFormat;
        }

        return get_format(pContext, nOpcode);
    } // decode_insn

    // decode_opcode
    //  Parses prefixes and returns Opcode.
    uint decode_opcode(Context* pContext)
    {
        for (;;)
        {
            uint nOpcode = pContext->ReadU8();
            switch (nOpcode)
            {
            case 0x26: m_rgfPrefix |= Prefix_ES; break;
            case 0x2E: m_rgfPrefix |= Prefix_CS; break;
            case 0x36: m_rgfPrefix |= Prefix_SS; break;
            case 0x3E: m_rgfPrefix |= Prefix_DS; break;
            case 0x64: m_rgfPrefix |= Prefix_FS; break;
            case 0x65: m_rgfPrefix |= Prefix_GS; break;
            case 0x66: m_nOpdSiz = 16; break;
            case 0x67: m_nAdrSiz = 32; break;
            case 0xF0: m_rgfPrefix |= Prefix_LOCK; break;
            case 0xF2: m_rgfPrefix |= Prefix_REPNE; break;
            case 0xF3: m_rgfPrefix |= Prefix_REP; break;
            default:
                if (nOpcode >= 0x40 && nOpcode <= 0x4F)
                {
                    m_nRex = nOpcode;
                    nOpcode = pContext->ReadU8();
                }

                // Check mandatory prefix
                if (nOpcode == 0x0F)
                {
                    if (16 == m_nOpdSiz)
                    {
                        m_nOpdSiz = 32;
                        nOpcode = 0x660F00 | pContext->ReadU8();
                    }
                    else if (m_rgfPrefix & Prefix_REPNE)
                    {
                        m_rgfPrefix &= ~Prefix_REPNE;
                        nOpcode = 0xF20F00 | pContext->ReadU8();
                    }
                    else if (m_rgfPrefix & Prefix_REP)
                    {
                        m_rgfPrefix &= ~Prefix_REP;
                        nOpcode = 0xF30F00 | pContext->ReadU8();
                    }
                } // if

                if (m_nRex & Rex_W) m_nOpdSiz = 64;

                return nOpcode;
            } // switch u8
        } // for
    } // decode_opcode

    // disasm_operands
    virtual Val disasm_operands(Context* pContext, const Format* pFormat)
    {
        Val operands = nil;
        for (uint i = 0; i < pFormat->m_cOperands; i++)
        {
            operands = cons(
                disasm_operand(pContext, pFormat->m_rgeOperand[i]),
                operands );
        } // for
        return nreverse_list(operands);
    } // disasm_operands

    // disasm_disp32
    Val disasm_disp32(Context* pContext)
    {
        switch (pContext->GetAnnon())
        {
        case FunObj::Annon::Type_Label:
        {
            int iVal = pContext->ReadS32();
            char16 wsz[20];
                ::wsprintfW(wsz, L"L%04X", iVal - pContext->GetStart());
            return intern(wsz, TLV(ApackageA));
        } // Type_Label

        case FunObj::Annon::Type_TlvOffset:
        {
            Int iOffset = pContext->ReadS32();

            if (iOffset >= offsetof(Thread, mv_tlv) &&
                iOffset < sizeof(Thread) )
            {
                Int iIndex = MiniThread::ToTlvIndex(iOffset);

                Val tlvrec = svref(VAR(Atlv_vectorA), iIndex );

                return list(Qtlv, tlv_record_name(tlvrec));
            } // if

            return Fixnum::Encode(iOffset);
        } // Type_TlvOffset

        default:
            return make_int(pContext->ReadS32());
        } // switch annon
    } // disasm_disp32

    // disasm_Ev
    Val disasm_Ev(Context* pContext, RegClass eRegClass, uint nOpdSiz)
    {
        Val ea = disasm_Ev_aux(pContext, eRegClass, nOpdSiz);
        if (consp(ea) && Q(":RBP") == car(ea))
        {
            Int iDisp = 0;
            Val extra = nil;

            if (fixnump(second(ea)))
            {
                iDisp = Fixnum::Decode_(second(ea));
            }
            else if (fixnump(third(ea)))
            {
                iDisp = Fixnum::Decode_(third(ea));
                extra = second(ea);
            } // if

            if (0 == iDisp % 4)
            {
                if (iDisp < static_cast<Int>(offsetof(Thread, mv_stat)))
                {
                    LPCWSTR pwsz = k_rgpwszTCB[(iDisp + 128) / 8];
                    if (NULL != pwsz)
                    {
                        ea = intern(pwsz, TLV(ApackageA));
                        if (nil != extra)
                        {
                            ea = list(ea, extra);
                        }
                    } // if
                }
                else
                {
                    iDisp -= offsetof(Thread, mv_stat);

                    UINT nType = static_cast<UINT>(iDisp)
                        / sizeof(Thread::ObjectStat);

                    char16 wsz[60];

                    if (0 == iDisp % sizeof(Thread::ObjectStat))
                    {
                        ::wsprintfW(wsz, L"[RBP] thread.mv_stat[%d].m_size",
                            nType );
                    }
                    else
                    {
                        ::wsprintfW(wsz, L"[RBP] thread.mv_stat[%d].m_count",
                            nType );
                    }
                    ea = intern(wsz, TLV(ApackageA));
                } // if iDisp
            } // if
        } // if ebp

        return ea;
    } // disasm_Ev

    // disasm_Ev_aux
    Val disasm_Ev_aux(Context* pContext, RegClass eRegClass, uint nOpdSiz)
    {
        ensure_modrm(pContext);
        Mod nMod = static_cast<Mod>(m_nModRm & 0xC0);
        int nRm  = ldb(3, 0, m_nModRm);
        switch (nMod)
        {
        case Mod_Disp0:
            switch (nRm)
            {
            case Rm_SIB:    // [base+index]
                return disasm_opdsiz(disasm_sib(pContext, nMod), nOpdSiz);
            case Rm_Disp32: // [rip+disp32]
                return disasm_opdsiz(disasm_rip_disp32(pContext), nOpdSiz);
            default:
                return list(disasm_reg(
                    pContext,
                    RegClass_Gpr,
                    m_nAdrSiz,
                    nRm,
                    m_nRex & Rex_B ));
            } // switch nRm

        case Mod_Disp8:
            if (Rm_SIB == nRm)
            {   // [base+index+disp8]
                Val sib = disasm_sib(pContext, nMod);
                Val disp = disasm_s8(pContext);
                return disasm_opdsiz(nconc(sib, list(disp)), nOpdSiz);
            }
            else
            {   // [base+disp8]
                Val reg  = disasm_reg(
                    pContext,
                    RegClass_Gpr,
                    m_nAdrSiz,
                    nRm,
                    m_nRex & Rex_B );
                Val disp = disasm_s8(pContext);
                return disasm_opdsiz(list(reg, disp), nOpdSiz);
            } // if

        case Mod_Disp32:
            if (Rm_SIB == nRm)
            {   // [base+index+disp32]
                Val sib = disasm_sib(pContext, nMod);
                Val disp = disasm_disp32(pContext);
                return disasm_opdsiz(nconc(sib, list(disp)), nOpdSiz);
            }
            else
            {   // [base+disp32]
                Val reg  = disasm_reg(
                    pContext,
                    RegClass_Gpr,
                    m_nAdrSiz,
                    nRm,
                    m_nRex & Rex_B );
                Val disp = disasm_disp32(pContext);
                return disasm_opdsiz(list(reg, disp), nOpdSiz);
            }

        case Mod_Reg:
            return disasm_reg(
                    pContext,
                    eRegClass,
                    nOpdSiz,
                    nRm,
                    m_nRex & Rex_B );
        } // switch mod

        CAN_NOT_HAPPEN();
    } // disasm_Ev_aux

    // disasm_rip_disp32
    Val disasm_rip_disp32(Context* pContext)
    {
        FunObj::Annon::Type eAnnon = pContext->GetAnnon();
        uint ofs = pContext->GetOffset();
        int32 disp32 = pContext->ReadS32();
        switch (eAnnon)
        {
        case FunObj::Annon::Type_DllLink:
        {
            DllEntry* pEntry = m_pFunObj->FetchDllEntry(ofs);
            DllProcInfo* pProc = pEntry->m_proc_info->Decode<DllProcInfo>();
            DllFileInfo* pFile = pProc->m_file_info->Decode<DllFileInfo>();
            return list(Q(":DLL"), pFile->m_filename, pProc->m_proc_name);
        } // FunObj::Annon::Type_DllLink

        default:
        {
            char16 wsz[20];
            uint nRelAddr = ofs + 4 + disp32;
            if (nRelAddr < pContext->GetLength())
            {
                ::wsprintfW(wsz, L"[L%04X]", nRelAddr);
            }
            else
            {
                UInt nAbsAddr = pContext->GetStart() + nRelAddr;
                ::wsprintfW(wsz, L"[#x%lX]", nAbsAddr);
            }
            return intern(wsz, TLV(ApackageA));
        } // default
        } // switch
    } // disasm_rip_disp32

    // disasm_Id
    Val disasm_Id(Context* pContext)
    {
        switch (pContext->GetAnnon())
        {
        case FunObj::Annon::Type_ExitPoint:
        {
            Int iVal = pContext->ReadS32() / Fixnum::One;
            char16 wsz[20];
                ::wsprintfW(wsz, L"L%04X", iVal);
            return intern(wsz, TLV(ApackageA));
        } // Type_ExitPoint

        case FunObj::Annon::Type_TlvOffset:
        {
            int32 iOffset = pContext->ReadS32();

            if (iOffset >= offsetof(Thread, mv_tlv) &&
                iOffset < sizeof(Thread) )
            {
                Int iIndex = MiniThread::ToTlvIndex(iOffset);

                Val tlvrec = svref(VAR(Atlv_vectorA), iIndex);

                return list(Qtlv, tlv_record_name(tlvrec));
            } // if

            return format_s32(iOffset);
        } // Type_TlvOffset

        default:
            return format_s32(pContext->ReadS32());
        } // switch annon
    } // disasm_Id

    // disasm_Iq
    Val disasm_Iq(Context* pContext)
    {
        FunObj::Annon::Type eAnnon = pContext->GetAnnon();
        int64 imm64 = pContext->ReadS64();
        switch (eAnnon)
        {
        case FunObj::Annon::Type_ClosedLit:
        {
            Val val = FromInt<Val_>(imm64);
            return list(Q(":CLIT"), val);
        } // Type_ClosedLit

        case FunObj::Annon::Type_Label:
        {
            char16 wsz[20];
                ::wsprintfW(wsz, L"L%04X", imm64 - pContext->GetStart());
            return intern(wsz, TLV(ApackageA));
        } // Type_Label

        case FunObj::Annon::Type_LispVal:
        {
            Val val = FromInt<Val_>(imm64);
            return list(Qquote, val);
        } // Type_LispVal

        case FunObj::Annon::Type_ClosedVar:
        {
            if (imm64 < 0x1000)
            {
                // A Closure template has an index.
                return FromInt<Val_>(imm64);
            }
            else
            {
                imm64 -= offsetof(ClosedCell, m_value);

                Val cell = reinterpret_cast<ClosedCell*>(imm64)->
                    Encode();

                if (nil == memq(cell, m_cvars))
                {
                    m_cvars = cons(cell, m_cvars);
                }

                return list(Q(":CREF"), cell);
            }
        } // Type_ClosedVar

        case FunObj::Annon::Type_SymFun:
        {
            imm64 -= offsetof(Symbol, m_function);
            return intern(
                format(nil, L"~S.m_function",
                    FromInt<Symbol>(imm64)->Encode() ),
                TLV(ApackageA) );
        } // Type_SymFun

        case FunObj::Annon::Type_SymSetf:
        {
            imm64 -= offsetof(SetfCell, m_function);
            return intern(
                format(nil, L"~S.m_setf",
                    FromInt<SetfCell>(imm64)->m_name ),
                TLV(ApackageA) );
        } // Type_SymSetf

        case FunObj::Annon::Type_SymVal:
        {
            imm64 -= offsetof(ValueCell, m_value);
            return intern(
                format(nil, L"~S.m_value",
                    FromInt<ValueCell>(imm64)->m_name ),
                TLV(ApackageA) );
        } // Type_SymVal

        default:
            return format_s64(imm64);
        } // switch annon
    } // disasm_Iq

    // disasm_Iv
    Val disasm_Iv(Context* pContext)
    {
        switch (m_nOpdSiz)
        {
        case 16: return Fixnum::Encode(pContext->ReadS16());
        case 32: return disasm_Id(pContext);
        case 64: return disasm_Iq(pContext);
        default: CAN_NOT_HAPPEN();
        } // switch m_nOpdSiz
    } // disasm_Iv

    // disasm_Iz
    Val disasm_Iz(Context* pContext)
    {
        switch (m_nOpdSiz)
        {
        case  8: return disasm_s8(pContext);
        case 16: return Fixnum::Encode(pContext->ReadS16());
        case 32: return disasm_Id(pContext);
        case 64: return disasm_Id(pContext);
        default: CAN_NOT_HAPPEN();
        } // switch m_nOpdSiz
    } // disasm_Iz

    // disasm_Jv
    Val disasm_Jv(Context* pContext)
    {
        FunObj::Annon::Type eAnnon = pContext->GetAnnon();

        Int iRel  = pContext->ReadS32();
        Int iAddr = pContext->GetAddress() + iRel;

        switch (eAnnon)
        {
        case FunObj::Annon::Type_LocalCallee:
        case FunObj::Annon::Type_NamedCallee:
        case FunObj::Annon::Type_Callee:
        {
            FunObj* pFunObj = FromInt<FunObj>(iAddr - sizeof(FunObj));
            return pFunObj->Encode();
        } // case

        default:
        {
            char16 wsz[20];

            LPCWSTR pwszFormat = L"#x%016X";
            if (iAddr >= pContext->GetStart() && iAddr < pContext->GetEnd())
            {
                iAddr -= pContext->GetStart();
                pwszFormat = L"L%04X";
            }

            ::wsprintfW(wsz, pwszFormat, iAddr);
            return intern(wsz, TLV(ApackageA));
        } // default
        } // switch
    } // disasm_Jv

    // disasm_opdsiz
    Val disasm_opdsiz(Val ea, uint nOpdSiz)
    {
        switch (nOpdSiz)
        {
        case   8: return cons(Q(":BYTE"), ea);
        case  16: return cons(Q(":WORD"), ea);
        case  32: return cons(Q(":DWORD"), ea);
        case  64: return ea;
        case 128: return cons(Q(":DQWORD"), ea);
        default: CAN_NOT_HAPPEN();
        } // switch opdsiz
    } // disasm_opdsiz

    // disasm_operand
    Val disasm_operand(
        Context*        pContext,
        Format::Operand eOperand )
    {
        switch (eOperand)
        {
        case Format::Operand_1:
            return Fixnum::Encode(1);

        case Format::Operand_Eb:
            return disasm_Ev(pContext, RegClass_Gpr, 8);

        case Format::Operand_Ew:
            return disasm_Ev(pContext, RegClass_Gpr, 16);

        case Format::Operand_Ed:
            return disasm_Ev(pContext, RegClass_Gpr, 32);

        case Format::Operand_Eq:
            return disasm_Ev(pContext, RegClass_Gpr, 64);

        case Format::Operand_Md:
            return disasm_Ev(pContext, RegClass_Mmx, 32);

        case Format::Operand_Wss:
            return disasm_Ev(pContext, RegClass_Xmm, 32);

        case Format::Operand_Wsd:
            return disasm_Ev(pContext, RegClass_Xmm, 64);

        case Format::Operand_Qdq:
            return disasm_Ev(pContext, RegClass_Xmm, 64);

        case Format::Operand_Wdq:
        case Format::Operand_Wpd:
        case Format::Operand_Wps:
            return disasm_Ev(pContext, RegClass_Xmm, 128);

        case Format::Operand_Ev:
        case Format::Operand_M:
        case Format::Operand_Mp:
        case Format::Operand_Ob:
        case Format::Operand_Ov:
            return disasm_Ev(pContext, RegClass_Gpr, m_nOpdSiz);

        case Format::Operand_Mpd:
        case Format::Operand_Mps:
            return disasm_Ev(pContext, RegClass_Mmx, m_nOpdSiz);

        case Format::Operand_Gb:
            return disasm_reg(
                pContext,
                RegClass_Gpr,
                8,
                ldb(3, 3, ensure_modrm(pContext)),
                m_nRex );

        case Format::Operand_EvD64:
        case Format::Operand_EvF64:
            return disasm_Ev(pContext, RegClass_Gpr, 64);

        case Format::Operand_Nq:
        case Format::Operand_Pd:
        case Format::Operand_Pq:
        case Format::Operand_Qd:
        case Format::Operand_Qq:
            return disasm_Ev(pContext, RegClass_Mmx, 64);

        case Format::Operand_Vdq:
        case Format::Operand_Vpd:
        case Format::Operand_Vps:
        case Format::Operand_Vsd:
        case Format::Operand_Vss:
            return disasm_reg(
                pContext,
                RegClass_Xmm,
                128,
                ldb(3, 3, ensure_modrm(pContext)),
                m_nRex & Rex_R );

        case Format::Operand_Udq:
            return disasm_reg(
                pContext,
                RegClass_Xmm,
                128,
                ldb(3, 0, ensure_modrm(pContext)),
                m_nRex & Rex_R );

        case Format::Operand_Gd:
            return disasm_reg(
                pContext,
                RegClass_Gpr,
                32,
                ldb(3, 3, ensure_modrm(pContext)),
                m_nRex & Rex_R );

        case Format::Operand_Gq:
            return disasm_reg(
                pContext,
                RegClass_Gpr,
                64,
                ldb(3, 3, ensure_modrm(pContext)),
                m_nRex & Rex_R );

        case Format::Operand_Gv:
            return disasm_reg(
                pContext,
                RegClass_Gpr,
                m_nOpdSiz,
                ldb(3, 3, ensure_modrm(pContext)),
                m_nRex & Rex_R );

        case Format::Operand_AL:
        case Format::Operand_BL:
        case Format::Operand_CL:
        case Format::Operand_DL:
        case Format::Operand_AH:
        case Format::Operand_BH:
        case Format::Operand_CH:
        case Format::Operand_DH:
            return disasm_reg(
                pContext,
                RegClass_Gpr,
                8,
                eOperand - Format::Operand_AL,
                m_nRex & Rex_B );

        case Format::Operand_DX:    // for IN and OUT instructions.
            return disasm_reg(
                pContext,
                RegClass_Gpr,
                16,
                Gpr_DX,
                m_nRex & Rex_R );

        case Format::Operand_Ib:
            return Fixnum::Encode(pContext->ReadU8());

        case Format::Operand_Is:
            return Fixnum::Encode(pContext->ReadS8());

        case Format::Operand_Iw:
            return Fixnum::Encode(pContext->ReadS16());

        case Format::Operand_Iq:
            return disasm_Iq(pContext);

        case Format::Operand_Iv:
            return disasm_Iv(pContext);

        case Format::Operand_Iz:
            return disasm_Iz(pContext);

        case Format::Operand_Jb:
        {
            Int iAddr = pContext->ReadS8();
                iAddr += pContext->GetAddress();

            char16 wsz[20];
                ::wsprintfW(wsz, L"L%04X", iAddr - pContext->GetStart());
            return intern(wsz, TLV(ApackageA));
        } // Format::Operand_Jb

        case Format::Operand_Jv:
            return disasm_Jv(pContext);

        case Format::Operand_eAX:
        case Format::Operand_eBP:
        case Format::Operand_eBX:
        case Format::Operand_eCX:
        case Format::Operand_eDI:
        case Format::Operand_eDX:
        case Format::Operand_eSI:
        case Format::Operand_eSP:
            return disasm_reg(
                pContext,
                RegClass_Gpr,
                m_nOpdSiz,
                eOperand - Format::Operand_eAX,
                m_nRex & Rex_B );

        case Format::Operand_rAX:
        case Format::Operand_rBP:
        case Format::Operand_rBX:
        case Format::Operand_rCX:
        case Format::Operand_rDI:
        case Format::Operand_rDX:
        case Format::Operand_rSI:
        case Format::Operand_rSP:
            return disasm_reg(
                pContext,
                RegClass_Gpr,
                64,
                eOperand - Format::Operand_rAX,
                m_nRex & Rex_R );

        case Format::Operand_CS:
            return intern(L"CS", PACKAGE_keyword);

        case Format::Operand_DS:
            return intern(L"DS", PACKAGE_keyword);

        case Format::Operand_ES:
            return intern(L"ES", PACKAGE_keyword);

        case Format::Operand_SS:
            return intern(L"SS", PACKAGE_keyword);
        } // switch eOperand

        return nil;
    } // disasm_operand

    // disasm_reg
    Val disasm_reg(Context*, RegClass eRegClass, int nSize, int nIdx, int nRex)
    {
        const char16* pwsz;
        uint cwch;

        switch (eRegClass)
        {
        case RegClass_Gpr:
            switch (nSize)
            {
            case 8:
                if (0 == nRex)
                {
                    pwsz = L"ALCLDLBLAHCHDHBH";
                    cwch = 2;
                }
                else if (0 == (nRex & Rex_R))
                {
                    if (nIdx <= 3)
                    {
                        pwsz = L"ALCLDLBLDL";
                        cwch = 2;
                    }
                    else
                    {
                        pwsz = L"RSPLRBPLRSILRDIL";
                        cwch = 3;
                        nIdx -= 3;
                    }
                }
                else if (nIdx <= 1)
                {
                    pwsz = L"R8LR9L";
                    cwch = 3;
                }
                else
                {
                    pwsz = L"R10LR11LR12LR13LR14LR15L";
                    cwch = 4;
                    nIdx -= 2;
                }
                break;

            case 16:
                if (0 == nRex)
                {
                    pwsz = L"AXCXDXBXSPBPSIDI";
                    cwch = 2;
                }
                else if (nIdx <= 1)
                {
                    pwsz = L"R8WR9W";
                    cwch = 3;
                }
                else
                {
                    pwsz = L"R10WR11WR12WR13WR14WR15W";
                    cwch = 4;
                    nIdx -= 2;
                }
                break;

            case 32:
                if (0 == nRex)
                {
                    pwsz = L"EAXECXEDXEBXESPEBPESIEDI";
                    cwch = 3;
                }
                else if (nIdx <= 1)
                {
                    pwsz = L"R8DR9D";
                    cwch = 3;
                }
                else
                {
                    pwsz = L"R10DR11DR12DR13DR14DR15D";
                    cwch = 4;
                    nIdx -= 2;
                }
                break;

            case 64:
                if (0 == nRex)
                {
                    pwsz = L"RAXRCXRDXRBXRSPRBPRSIRDI";
                    cwch = 3;
                }
                else if (nIdx <= 1)
                {
                    pwsz = L"R8R9";
                    cwch = 2;
                }
                else
                {
                    pwsz = L"R10R11R12R13R14R15";
                    cwch = 3;
                    nIdx -= 2;
                }
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch nSize
            break;

        case RegClass_Fpr:
            pwsz = L"ST0ST1ST2ST3ST4ST5ST6ST7";
            cwch = 3;
            break;

        case RegClass_Mmx:
            pwsz = L"MM0MM1MM2MM3MM4MM5MM6MM7";
            cwch = 3;
            break;

        case RegClass_Xmm:
            if (0 == nRex)
            {
                pwsz = L"XMM0XMM1XMM2XMM3XMM4XMM5XMM6XMM7";
                cwch = 4;
            }
            else if (nIdx <= 1)
            {
                pwsz = L"XMM8XMM9";
                cwch = 4;
            }
            else
            {
                pwsz = L"XMM10XMM11XMM12XMM13XMM14XMM15";
                cwch = 5;
                nIdx -= 2;
            }
            break;

        default:
            CAN_NOT_HAPPEN();
        } // nSize

        StackString_<8> oName;
        oName.Init(pwsz + nIdx * cwch, cwch);
        return intern(oName.Encode(), PACKAGE_keyword);
    } // decode_reg

    // disasm_s8
    Val disasm_s8(Context* pContext)
    {
        return Fixnum::Encode(pContext->ReadS8());
    } // disasm_s8

    // decode_sib
    //   7 6  5 4 3  2 1 0
    //  +----+------+------+
    //  | ss | index| base |
    //  +----+------+------+
    Val disasm_sib(Context* pContext, Mod nMod)
    {
        int sib = pContext->ReadU8();
        int nScale = ldb(2, 6, sib);
        int nIndex = ldb(3, 3, sib);
        int nBase  = ldb(3, 0, sib);
        if (4 != nIndex)
        {
            Val reg = disasm_reg(
                pContext,
                RegClass_Gpr,
                m_nAdrSiz,
                nBase,
                m_nRex & Rex_B );

            Val idx = disasm_sib_index(pContext, nIndex, nScale);

            if (5 != nBase)
            {
                return list(reg, idx);
            }

            switch (nMod)
            {
            case Mod_Disp0:
                return list(disasm_disp32(pContext), idx);
            case Mod_Disp8:
                return list(reg, idx);
            case Mod_Disp32:
                return list(reg, idx);
            default:
                CAN_NOT_HAPPEN();
            } // switch nMod
        }
        else if (5 == nBase)
        {
            // ss 100 101
            return nil;
        }
        else
        {   // ss 100 base
            return list(disasm_reg(
                pContext,
                RegClass_Gpr,
                m_nAdrSiz,
                nBase,
                m_nRex & Rex_B ) );
        }
    } // disasm_sib

    // disasm_sib_index
    Val disasm_sib_index(
        Context*    pContext,
        int         nIndex,
        int         nScale )
    {
        if (0 == nScale)
        {
            return disasm_reg(
                pContext,
                RegClass_Gpr,
                m_nAdrSiz,
                nIndex,
                m_nRex & Rex_X );
        }

        switch (m_nAdrSiz)
        {
        case 32:
        {
            char16 wszIndex[20];
            char16* pwsz = wszIndex;

            if (0 == (m_nRex & Rex_X))
            {
                *pwsz++ = 'E';
                *pwsz++ = L"ACDBBSSD"[nIndex];
                *pwsz++ = L"XXXXPPII"[nIndex];
            }
            else if (nIndex < 2)
            {
                *pwsz++ = 'R';
                *pwsz++ = static_cast<char16>(nIndex + '8');
                *pwsz++ = 'D';
            }
            else
            {
                *pwsz++ = 'R';
                *pwsz++ = '1';
                *pwsz++ = static_cast<char16>(nIndex + '0' - 2);
                *pwsz++ = 'D';
            }
            *pwsz++ = '*';
            *pwsz++ = static_cast<char16>('0' + (1<<nScale));
            *pwsz++ = 0;
            return intern(wszIndex, PACKAGE_keyword);
        } // 32

        case 64:
        {
            char16 wszIndex[20];
            char16* pwsz = wszIndex;

            if (0 == (m_nRex & Rex_X))
            {
                *pwsz++ = 'R';
                *pwsz++ = L"ACDBBSSD"[nIndex];
                *pwsz++ = L"XXXXPPII"[nIndex];
            }
            else if (nIndex < 2)
            {
                *pwsz++ = 'R';
                *pwsz++ = static_cast<char16>(nIndex + '8');
            }
            else
            {
                *pwsz++ = 'R';
                *pwsz++ = '1';
                *pwsz++ = static_cast<char16>(nIndex + '0' - 2);
            }
            *pwsz++ = '*';
            *pwsz++ = static_cast<char16>('0' + (1<<nScale));
            *pwsz++ = 0;
            return intern(wszIndex, PACKAGE_keyword);
        } // 64

        default:
            CAN_NOT_HAPPEN();
        } // switch m_nAdrSiz
    } // disasm_sib_index

    // format_s32
    Val format_s32(int32 iVal)
    {
        if (iVal > -0x1000 && iVal < 0x1000)
        {
            return Fixnum::Encode(iVal);
        }
        else
        {
            return intern(
                format(nil, L"#x~X~X",
                    Fixnum::Encode(iVal >> 4),
                    Fixnum::Encode(iVal & 15) ),
                TLV(ApackageA) );
        }
    } // format_s32

    // format_s64
    Val format_s64(int64 iVal)
    {
        if (iVal > -0x1000 && iVal < 0x1000)
        {
            return make_int(iVal);
        }
        else
        {
            return intern(
                format(nil, L"#x~X", make_int(iVal)),
                TLV(ApackageA) );
        }
    } // format_s64

    // setup_labels
    protected: virtual void setup_labels(Context* pContext)
    {
        do
        {
            const Format* pFormat = decode_insn(pContext);

            for (uint i = 0; i < pFormat->m_cOperands; i++)
            {
                switch (pFormat->m_rgeOperand[i])
                {
                case Format::Operand_Eb:
                case Format::Operand_Ew:
                case Format::Operand_Ed:
                case Format::Operand_Ev:
                case Format::Operand_EvF64:
                case Format::Operand_EvD64:
                case Format::Operand_M:
                case Format::Operand_Mpd:
                case Format::Operand_Mps:
                case Format::Operand_Ob:
                case Format::Operand_Ov:

                // SSE
                case Format::Operand_Vpd:
                case Format::Operand_Vsd:
                case Format::Operand_Vps:
                case Format::Operand_Vss:
                case Format::Operand_Wpd:
                case Format::Operand_Wsd:
                case Format::Operand_Wps:
                case Format::Operand_Wss:
                    setup_labels_modrm(pContext);
                    break;

                case Format::Operand_Ib:
                case Format::Operand_Is:
                    pContext->ReadU8();
                    break;

                case Format::Operand_Iw:
                    pContext->ReadU16();
                    break;

                case Format::Operand_Iq:
                    setup_label_Iq(pContext);
                    break;

                case Format::Operand_Iv:
                    setup_label_Iv(pContext);
                    break;

                case Format::Operand_Iz:
                    setup_label_Iz(pContext);
                    break;

                case Format::Operand_Jb:
                    setup_labels_rip_disp(pContext, pContext->ReadS8());
                    break;

                case Format::Operand_Jv:
                    setup_labels_rip_disp(pContext, pContext->ReadS32());
                    break;
                } // switch eOperand
            } // for i
        } while (! pContext->AtEnd());

        pContext->Rewind();
    } // setup_labels

    // setup_labels_modrm
    void setup_labels_modrm(Context* pContext)
    {
        ensure_modrm(pContext);
        int nMod = static_cast<Mod>(m_nModRm & 0xC0);
        int nRm  = ldb(3, 0, m_nModRm);
        switch (nMod)
        {
        case Mod_Disp0:
            switch (nRm)
            {
            case Rm_SIB: // [base+index]
                pContext->ReadU8();
                break;
            case Rm_Disp32: // [rip+disp32]
                setup_labels_rip_disp(pContext, pContext->ReadS32());
                break;
            } // switch nRm
            break;

        case Mod_Disp8:
            if (Rm_SIB == nRm)
            {   // [base+index+disp8]
                pContext->ReadU8();
                pContext->ReadU8();
            }
            else
            {   // [base+disp8]
                pContext->ReadU8();
            }
            break;

        case Mod_Disp32:
            if (Rm_SIB == nRm)
            {   // [base+index+disp32]
                pContext->ReadU8();
                pContext->ReadU32();
            }
            else
            {   // [base+disp32]
                pContext->ReadU32();
            }
            break;

        case Mod_Reg:
            break;
        } // switch mod
    } // setup_labels_modrm

    // setup_label_Iv
    void setup_label_Iv(Context* pContext)
    {
        switch (m_nOpdSiz)
        {
        case 64: setup_label_Iq(pContext); break;
        case 32: setup_label_Iz(pContext); break;
        case 16: pContext->ReadU16(); break;
        default: CAN_NOT_HAPPEN();
        }
    } // setup_label_Iv

    // setup_label_Iq
    void setup_label_Iq(Context* pContext)
    {
        switch (pContext->GetAnnon())
        {
        case FunObj::Annon::Type_Label:
        {
            uint64 iAddr = pContext->ReadU64() - pContext->GetStart();
            insert_label(pContext, iAddr);
            break;
        } // label

        default:
            pContext->ReadU64();
            break;
        } // switch annon
    } // setup_label_Iq

    // setup_label_Iz
    void setup_label_Iz(Context* pContext)
    {
        switch (pContext->GetAnnon())
        {
        case FunObj::Annon::Type_ExitPoint:
        {
            int32 iAddr = pContext->ReadS32() / Fixnum::One;
            insert_label(pContext, iAddr);
            break;
        } // exit_point

        default:
            pContext->ReadU32();
            break;
        } // switch annon
    } // setup_label_Iz

    // setup_labels_rip_disp
    void setup_labels_rip_disp(Context* pContext, int32 iDiff)
    {
        insert_label(
            pContext,
            pContext->GetOffset() + iDiff );
    } // setup_labels_rip_disp
}; // Disassembler


Format X64Disassembler::sm_oFormat_DQ =
{
    L"DQ",
    0,      // opcode
    1,      // #operands
    Format::Extend_None,
    { Format::Operand_Iq }
}; // sm_oFormat_DQ

// init_tcb
static void init_tcb()
{
    if (NULL != k_rgpwszTCB[(SVC_arity_error + 128) / 8]) return;

    #define INIT_SVC_NAME(x) \
        k_rgpwszTCB[(x + 128) / 8] = L ## #x;

    #define INIT_TCB_FIELD(x) \
        INIT_TCB_FIELD_(offsetof(Thread, x), x)

    #define INIT_TCB_FIELD_(ofs, x) \
        k_rgpwszTCB[(ofs + 128) / 8] = \
            L"[RBP]." L ## #x;

    INIT_SVC_NAME(SVC_alloc_bino_area);
    INIT_SVC_NAME(SVC_alloc_code_area);
    INIT_SVC_NAME(SVC_alloc_cons_area);
    INIT_SVC_NAME(SVC_alloc_reco_area);
    INIT_SVC_NAME(SVC_alloc_symb_area);

    INIT_SVC_NAME(SVC_arity_error);
    INIT_SVC_NAME(SVC_not_function);
    INIT_SVC_NAME(SVC_type_error);
    INIT_SVC_NAME(SVC_unbound_variable);
    INIT_SVC_NAME(SVC_undefined_function);

    INIT_TCB_FIELD_(-8, m_fixnum_1);

    INIT_TCB_FIELD(m_fp);
    INIT_TCB_FIELD(m_n);
    INIT_TCB_FIELD(m_fn);
    INIT_TCB_FIELD(mv_value[0]);
    INIT_TCB_FIELD(mv_value[1]);
    INIT_TCB_FIELD(mv_value[2]);
    INIT_TCB_FIELD(mv_value[3]);
    INIT_TCB_FIELD(mv_value[4]);
    INIT_TCB_FIELD(mv_value[5]);
    INIT_TCB_FIELD(mv_value[6]);
    INIT_TCB_FIELD(mv_value[7]);
    INIT_TCB_FIELD(mv_value[8]);
    INIT_TCB_FIELD(mv_value[9]);
    INIT_TCB_FIELD(mv_value[10]);
    INIT_TCB_FIELD(mv_value[11]);
    INIT_TCB_FIELD(mv_value[12]);
    INIT_TCB_FIELD(mv_value[13]);
    INIT_TCB_FIELD(mv_value[14]);
    INIT_TCB_FIELD(mv_value[15]);
} // init_tcb

} // Genesis

namespace CommonLisp
{

using namespace Genesis;


// disassemble
Val disassemble(Val thing)
{
    init_tcb();

    X64Disassembler oDisassembler(thing);
        oDisassembler.Run();
    return nil;
} // disassemble

} // CommonLisp
