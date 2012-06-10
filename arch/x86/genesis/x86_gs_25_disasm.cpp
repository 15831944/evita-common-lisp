#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 25 Environment - Disassembler
// arch/x86/x86_gs_25_disasm.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/genesis/x86_gs_25_disasm.cpp#17 $
//
#include "../x86_arch.h"
#include "../x86_opcode.h"

#include "../../../genesis/gs_lisp.h"

#include "../kernel/x86_ke_gcmap.h"
#include "../kernel/x86_ke_layout.h"
#include "../kernel/x86_ke_mach.h"
#include "../kernel/x86_ke_thread.h"

#include "../../../kernel/ke_dll_link.h"

#include "../../x86x64/genesis/x86x64_gs_25_disasm.h"

namespace Genesis
{

using namespace X86;

static LPCWSTR k_rgpwszTCB[256];

//////////////////////////////////////////////////////////////////////
//
// X86Disassembler
//
class X86Disassembler : public X86X64Disassembler
{
    public: X86Disassembler(Val thing) :
        X86X64Disassembler(thing) {}

    // disasm_gcmap
    virtual void disasm_gcmap(uint ofs)
    {
        if (m_oGcMapEnum.AtEnd()) return;
        if (m_oGcMapEnum.GetOffset() != ofs) return;

        const uint32* pGcDesc = m_oGcMapEnum.GetDesc();
            m_oGcMapEnum.Next();

        format(m_stream, L";  ----");

        {
            const uint32* p = pGcDesc;

            for (;;)
            {
                format(m_stream, L" ~X~X",
                    Fixnum::Encode(*p >> 4),
                    Fixnum::Encode(*p & 15) );
                if (0 == (*p & 1)) break;
                p++;
            } // for
        }

        const uint32* p = pGcDesc;
        uint gcdesc = *p++;
        uint fCont = gcdesc & 1;
        gcdesc >>= 1;

        const char16* pwsz;
        uint cBits;
        switch (gcdesc & 3)
        {
        case 0:
            pwsz = L" Gen "; gcdesc >>= 8; cBits = 23; break;
        case 1:
            pwsz = L" Call"; gcdesc >>= 2; cBits = 29; break;
        default:
            CAN_NOT_HAPPEN();
        } // switch kind

        write_string(pwsz, m_stream);

        uint ofsStack = 0;
        for (;;)
        {
            while (0 != gcdesc)
            {
                if (gcdesc & 1)
                {
                    format(m_stream, L" [ESP+~D]", Fixnum::Encode(ofsStack));
                }

                gcdesc >>= 1;
                cBits -= 1;
                ofsStack += sizeof(Val);
            } // while

            if (! fCont) break;

            ofsStack += cBits * sizeof(Val);

            gcdesc = *p++;
            fCont = gcdesc & 1;
                gcdesc >>= 1;
            cBits = 31;
        } // for

        format(m_stream, L"~%");
    } // disasm_gcmap

    // decode_insn
    virtual const Format* decode_insn(Context* pContext)
    {
        m_nAdrSiz   = 32;
        m_nOpdSiz   = 32;
        m_rgfPrefix = 0;
        m_nModRm    = NoModRm;

        if (pContext->FetchAnnon() == FunObj::Annon::Type_Label)
        {
            return &sm_oFormat_DD;
        }
        else
        {
            return get_format(pContext, decode_opcode(pContext));
        }
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
            case 0xF0:  // LOCK
                m_rgfPrefix |= Prefix_LOCK;
                break;
            case 0xF2:  // REPNE
                if (0x0F == pContext->ReadU8())
                {
                    return 0xF20F00 | pContext->ReadU8();
                }
                pContext->Unread();
                m_rgfPrefix |= Prefix_REPNE;
                break;
            case 0xF3:
                if (0x0F == pContext->ReadU8())
                {
                    return 0xF30F00 | pContext->ReadU8();
                }
                pContext->Unread();
                m_rgfPrefix |= Prefix_REP;
                break;
            case 0x26:
                m_rgfPrefix |= Prefix_ES;
                break;
            case 0x2E:
                m_rgfPrefix |= Prefix_CS;
                break;
            case 0x36:
                m_rgfPrefix |= Prefix_SS;
                break;
            case 0x3E:
                m_rgfPrefix |= Prefix_DS;
                break;
            case 0x64:
                m_rgfPrefix |= Prefix_FS;
                break;
            case 0x65:
                m_rgfPrefix |= Prefix_GS;
                break;
            case 0x66:  // Operand Size
                if (0x0F == pContext->ReadU8())
                {
                    return 0x660F00 | pContext->ReadU8();
                }
                pContext->Unread();
                m_nOpdSiz = 16;
                break;
            case 0x67:  // Addr Size
                m_nAdrSiz = 32;
                break;
            default:
                return nOpcode;
            } // switch nOpcode
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
        {
            Int iVal = pContext->ReadS32();
            if (iVal == QQchar_min->ToInt()) return QQchar_min;
            return make_int(iVal);
        } // default
        } // switch annon
    } // disasm_disp32

    // disasm_Ev
    Val disasm_Ev(Context* pContext, RegClass eRegClass, uint nOpdSiz)
    {
        Val ea = disasm_Ev_aux(pContext, eRegClass, nOpdSiz);
        if (consp(ea) && Q(":EBP") == car(ea))
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
                    LPCWSTR pwsz = k_rgpwszTCB[(iDisp + 128) / 4];
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
                        ::wsprintfW(wsz, L"[EBP] thread.mv_stat[%d].m_size",
                            nType );
                    }
                    else
                    {
                        ::wsprintfW(wsz, L"[EBP] thread.mv_stat[%d].m_count",
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
        int nRm  = m_nModRm & 0x07;
        switch (nMod)
        {
        case Mod_Disp0:
            switch (nRm)
            {
            case Rm_SIB: // [base+index]
                return disasm_opdsiz(disasm_sib(pContext, nMod), nOpdSiz);
            case Rm_Disp32: // [disp32]
                return disasm_opdsiz(disasm_rm_disp32(pContext), nOpdSiz);
            default:
                return disasm_opdsiz(
                    list(disasm_reg(pContext, RegClass_Gpr, m_nAdrSiz, nRm)),
                    nOpdSiz );
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
                Val reg  = disasm_reg(pContext, RegClass_Gpr, m_nAdrSiz, nRm);
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
                Val reg  = disasm_reg(pContext, RegClass_Gpr, m_nAdrSiz, nRm);
                Val disp = disasm_disp32(pContext);
                return disasm_opdsiz(list(reg, disp), nOpdSiz);
            }

        case Mod_Reg:
            return disasm_reg(pContext, eRegClass, nOpdSiz, nRm);
        } // switch mod

        CAN_NOT_HAPPEN();
    } // disasm_Ev_aux

    // disasm_rm_disp32
    Val disasm_rm_disp32(Context* pContext)
    {
        FunObj::Annon::Type eAnnon = pContext->GetAnnon(); 
        Int ofs = pContext->ReadS32();
        switch (eAnnon)
        {
        case FunObj::Annon::Type_ClosedVar:
        {
            if (ofs < 0x1000)
            {
                // A Closure template has an index.
                return list(Q(":CREF"), Fixnum::Encode(ofs));
            }
            else
            {
                ofs -= offsetof(ClosedCell, m_value);

                Val cell = reinterpret_cast<ClosedCell*>(ofs)->
                    Encode();

                if (nil == memq(cell, m_cvars))
                {
                    m_cvars = cons(cell, m_cvars);
                }

                return list(Q(":CVAR"), cell);
            }
        } // Type_ClosedVar

        case FunObj::Annon::Type_DllLink:
        {
            DllEntry* pEntry = reinterpret_cast<DllEntry*>(ofs);
            DllProcInfo* pProc = pEntry->m_proc_info->Decode<DllProcInfo>();
            DllFileInfo* pFile = pProc->m_file_info->Decode<DllFileInfo>();
            return list(Q(":DLL"), pFile->m_filename, pProc->m_proc_name);
        } // FunObj::Annon::Type_DllLink

        case FunObj::Annon::Type_SymFun:
        {
            ofs -= offsetof(Symbol, m_function);
            return intern(
                format(nil, L"[~S].m_function",
                    FromInt<Symbol>(ofs)->Encode() ),
                TLV(ApackageA) );
        } // Type_SymFun

        case FunObj::Annon::Type_SymSetf:
        {
            ofs -= offsetof(SetfCell, m_function);
            return intern(
                format(nil, L"[~S].m_setf",
                    FromInt<SetfCell>(ofs)->m_name ),
                TLV(ApackageA) );
        } // Type_SymSetf

        case FunObj::Annon::Type_SymVal:
        {
            ofs -= offsetof(ValueCell, m_value);
            return intern(
                format(nil, L"[~S].m_value",
                    FromInt<ValueCell>(ofs)->m_name ),
                TLV(ApackageA) );
        } // Type_SymVal

        default:
        {
            char16 wsz[20];
                ::wsprintfW(wsz, L"[#x%08X]", ofs);
            return intern(wsz, TLV(ApackageA));
        } // default
        } // switch
    } // disasm_rm_disp32

    // disasm_Iz
    Val disasm_Iz(Context* pContext)
    {
        switch (m_nOpdSiz)
        {
        case 8:
            return disasm_s8(pContext);
        case 16:
            return Fixnum::Encode(pContext->ReadS16());
        case 32:
        {
            uint eAnnon = pContext->GetAnnon();
            UInt nVal = pContext->ReadU32();
            switch (eAnnon)
            {
            case FunObj::Annon::Type_ClosedLit:
            {
                Val val = FromInt<Val_>(nVal);
                return list(Q(":CLIT"), val);
            } // Type_ClosedLit

            case FunObj::Annon::Type_DllLink:
            {
                DllEntry* pEntry = reinterpret_cast<DllEntry*>(nVal);
                DllProcInfo* pProc = pEntry->m_proc_info->
                    Decode<DllProcInfo>();
                DllFileInfo* pFile = pProc->m_file_info->
                    Decode<DllFileInfo>();
                return list(Q(":DLL"), pFile->m_filename, pProc->m_proc_name);
            } // FunObj::Annon::Type_DllLink

            case FunObj::Annon::Type_ExitPoint:
            {
                Int iVal = nVal / Fixnum::One;
                char16 wsz[20];
                    ::wsprintfW(wsz, L"L%04X", iVal);
                return intern(wsz, TLV(ApackageA));
            } // Type_ExitPoint

            case FunObj::Annon::Type_Label:
            {
                char16 wsz[20];
                    ::wsprintfW(wsz, L"L%04X", nVal - pContext->GetStart());
                return intern(wsz, TLV(ApackageA));
            } // Type_Label

            case FunObj::Annon::Type_LispVal:
            {
                Val val = FromInt<Val_>(nVal);
                return list(Qquote, val);
            } // Type_LispVal

            case FunObj::Annon::Type_SymFun:
            {
                nVal -= offsetof(Symbol, m_function);
                Val val = FromInt<Symbol>(nVal)->Encode();
                return list(Q(":SYMFUN"), val);
            } // Type_SymFun

            case FunObj::Annon::Type_SymSetf:
            {
                nVal -= offsetof(Symbol, m_function);
                Val val = FromInt<SetfCell>(nVal)->m_name;
                return list(Q(":SYMSETF"), val);
            } // Type_SymSetf

            case FunObj::Annon::Type_SymVal:
            {
                nVal -= offsetof(ValueCell, m_value);
                Val val = FromInt<ValueCell>(nVal)->m_name;
                return list(Q(":SYMVAL"), val);
            } // Type_SymVal

            case FunObj::Annon::Type_TlvOffset:
            {
                Int iOffset = nVal;

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
                return format_s32(nVal);
            } // switch annon
        } // 32

        default:
            CAN_NOT_HAPPEN();
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

            LPCWSTR pwszFormat = L"#%08X";
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
        case  32: return ea;
        case  64: return cons(Q(":QWORD"), ea);
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

        case Format::Operand_Eq:
        case Format::Operand_Wdq:
        case Format::Operand_Wq:
            return disasm_Ev(pContext, RegClass_Xmm, 128);

        case Format::Operand_Ew:
            return disasm_Ev(pContext, RegClass_Gpr, 16);

        case Format::Operand_Ed:
        case Format::Operand_EvD64:
        case Format::Operand_EvF64:
        case Format::Operand_Md:
            return disasm_Ev(pContext, RegClass_Gpr, 32);

        case Format::Operand_Wss:
            return disasm_Ev(pContext, RegClass_Xmm, 32);

        case Format::Operand_Ev:
        case Format::Operand_M:
        case Format::Operand_Mp:
        case Format::Operand_Mpd:
        case Format::Operand_Mps:
            return disasm_Ev(pContext, RegClass_Gpr, m_nOpdSiz);

        case Format::Operand_Gb:
            return disasm_reg(
                pContext,
                RegClass_Gpr,
                8,
                ldb(3, 3, ensure_modrm(pContext)) );

        case Format::Operand_Nq:
        case Format::Operand_Pd:
        case Format::Operand_Pq:
        case Format::Operand_Qd:
        case Format::Operand_Qq:
            return disasm_Ev(pContext, RegClass_Mmx, 64);

        case Format::Operand_Ob:
            return list(Q(":BYTE"), disasm_Iz(pContext));

        case Format::Operand_Ov:
            return list(disasm_Iz(pContext));

        case Format::Operand_Wsd:
            return disasm_Ev(pContext, RegClass_Xmm, 64);

        case Format::Operand_Qdq:

        case Format::Operand_Wpd:
            return disasm_Ev(pContext, RegClass_Xmm, 64);

        case Format::Operand_Wps:
            return disasm_Ev(pContext, RegClass_Xmm, 32);

        case Format::Operand_Vdq:
        case Format::Operand_Vpd:
        case Format::Operand_Vpq:
        case Format::Operand_Vps:
        case Format::Operand_Vq:
        case Format::Operand_Vsd:
        case Format::Operand_Vss:
            return disasm_reg(
                pContext,
                RegClass_Xmm,
                128,
                ldb(3, 3, ensure_modrm(pContext)) );

        case Format::Operand_Udq:
            return disasm_reg(
                pContext,
                RegClass_Xmm,
                128,
                ldb(3, 0, ensure_modrm(pContext)) );

        case Format::Operand_Gd:
        case Format::Operand_Gv:
            return disasm_reg(
                pContext,
                RegClass_Gpr,
                m_nOpdSiz,
                ldb(3, 3, ensure_modrm(pContext)) );

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
                eOperand - Format::Operand_AL );

        case Format::Operand_DX:
            return disasm_reg(pContext, RegClass_Gpr, 16, Gpr_DX);

        case Format::Operand_Ib:
            return Fixnum::Encode(pContext->ReadU8());

        case Format::Operand_Is:
            return Fixnum::Encode(pContext->ReadS8());

        case Format::Operand_Iw:
            return Fixnum::Encode(pContext->ReadS16());

        case Format::Operand_Iv:
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
                32,
                eOperand - Format::Operand_eAX );

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
                32,
                eOperand - Format::Operand_rAX );

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

    // decode_reg
    Val disasm_reg(Context*, RegClass eRegClass, int nSize, int nIdx)
    {
        StackString_<8> oName;

        switch (eRegClass)
        {
        case RegClass_Gpr:
            switch (nSize)
            {
            case 8:
                oName.Init(L"ALCLDLBLAHCHDHBH" + nIdx * 2, 2);
                break;

            case 16:
                oName.Init(L"AXCXDXBXSPBPSIDI" + nIdx * 2, 2);
                break;

            case 32:
                oName.Init(L"EAXECXEDXEBXESPEBPESIEDI" + nIdx * 3, 3);
                break;

            case 64:
                oName.Init(L"MM0MM1MM2MM3MM4MM5MM6MM7" + nIdx * 3, 3);
                break;

            case 128:
                oName.Init(L"XMM0XMM1XMM2XMM3XMM4XMM5XMM6XMM7" + nIdx * 4, 4);
                break;

            default:
                CAN_NOT_HAPPEN();
            } // nSize
            break;

        case RegClass_Fpr:
            oName.Init(L"ST0ST1ST2ST3ST4ST5ST6ST7" + nIdx * 3, 3);
            break;

        case RegClass_Mmx:
            oName.Init(L"MM0MM1MM2MM3MM4MM5MM6MM7" + nIdx * 3, 3);
            break;

        case RegClass_Xmm:
            oName.Init(L"XMM0XMM1XMM2XMM3XMM4XMM5XMM6XMM7" + nIdx * 4, 4);
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch class

        return intern(oName.Encode(), PACKAGE_keyword);
    } // decode_reg

    // disasm_s8
    Val disasm_s8(Context* pContext)
    {
        return Fixnum::Encode(pContext->ReadS8());
    } // disasm_s8

    // format_s32
    Val format_s32(Int iVal)
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
            Val reg = disasm_reg(pContext, RegClass_Gpr, 32, nBase);
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
            return list(disasm_reg(pContext, RegClass_Gpr, 32, nBase));
        }
    } // disasm_sib

    Val disasm_sib_index(
        Context*    pContext,
        int         nIndex,
        int         nScale )
    {
        if (0 == nScale)
        {
            return disasm_reg(pContext, RegClass_Gpr, 32, nIndex);
        }

        switch (m_nAdrSiz)
        {
        case 16:
        {
            char16 wszIndex[5];
                wszIndex[0] = L"ACDBBSSD"[nIndex];
                wszIndex[1] = L"XXXXPPII"[nIndex];
                wszIndex[2] = '*';
                wszIndex[3] = static_cast<char16>('0' + (1<<nScale));
                wszIndex[4] = 0;
            return intern(wszIndex, PACKAGE_keyword);
        } // case

        case 32:
        {
            char16 wszIndex[6];
                wszIndex[0] = 'E';
                wszIndex[1] = L"ACDBBSSD"[nIndex];
                wszIndex[2] = L"XXXXPPII"[nIndex];
                wszIndex[3] = '*';
                wszIndex[4] = static_cast<char16>('0' + (1<<nScale));
                wszIndex[5] = 0;
            return intern(wszIndex, PACKAGE_keyword);
        } // case
        } // switch m_nAdrSiz

        CAN_NOT_HAPPEN();
    } // disasm_sib_index

    // setup_labels
    virtual void setup_labels(Context* pContext)
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

                case Format::Operand_Iv:
                case Format::Operand_Iz:
                    setup_label_disp32(pContext);
                    break;

                case Format::Operand_Jb:
                {
                    int iDiff = pContext->ReadS8();
                    insert_label(
                        pContext,
                        pContext->GetOffset() + iDiff );
                    break;
                } // Format::Operand_Jb

                case Format::Operand_Jv:
                {
                    Int iDiff = setup_label_disp32(pContext);
                    insert_label(
                        pContext,
                        pContext->GetOffset() + iDiff );
                    break;
                } // Format::Operand_Jv
                } // switch eOperand
            } // for i
        } while (! pContext->AtEnd());

        pContext->Rewind();
    } // setup_labels

    void setup_labels_modrm(Context* pContext)
    {
        ensure_modrm(pContext);
        int nMod = ldb(2, 6, m_nModRm);
        int nRm  = ldb(3, 0, m_nModRm);
        switch (nMod)
        {
        case 0: // 00 disp0
            switch (nRm)
            {
            case Rm_SIB: // [base+index]
                pContext->ReadU8();
                break;
            case Rm_Disp32: // [disp32]
                setup_label_disp32(pContext);
                break;
            } // switch nRm
            break;

        case 1: // 01 disp8
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

        case 2: // 10 disp32
            if (Rm_SIB == nRm)
            {   // [base+index+disp32]
                pContext->ReadU8();
                setup_label_disp32(pContext);
            }
            else
            {   // [base+disp32]
                setup_label_disp32(pContext);
            }
            break;

        case 3: // 11 reg
            break;
        } // switch mod
    } // setup_labels_modrm

    // setup_label_disp32
    Int setup_label_disp32(Context* pContext)
    {
        int iAddr;
        switch (pContext->GetAnnon())
        {
        case FunObj::Annon::Type_Label:
            iAddr = pContext->ReadU32() - pContext->GetStart();
            insert_label(pContext, iAddr);
            break;

        case FunObj::Annon::Type_ExitPoint:
            iAddr = pContext->ReadS32() / Fixnum::One;
            insert_label(pContext, iAddr);
            break;

        default:
            iAddr = pContext->ReadU32();
            break;
        } // switch annon

        return iAddr;
    } // setup_label_disp32
}; // X86Disassembler

// init_tcb
static void init_tcb()
{
    if (NULL != k_rgpwszTCB[(SVC_arity_error + 128) / 4]) return;
    #define INIT_SVC_NAME(x) \
        k_rgpwszTCB[(x + 128) / 4] = L ## #x;

    #define INIT_TCB_FIELD(x) \
        INIT_TCB_FIELD_(offsetof(Thread, x), x)

    #define INIT_TCB_FIELD_(ofs, x) \
        k_rgpwszTCB[(ofs + 128) / 4] = \
            L"[EBP]." L ## #x;

    #define INIT_TCB_MV(n) INIT_TCB_FIELD(mv_value[n]);

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

    INIT_TCB_FIELD_(-4, m_fixnum_1);

    INIT_TCB_FIELD(m_fp);
    INIT_TCB_FIELD(m_n);
    INIT_TCB_FIELD(m_fn);

    INIT_TCB_MV(0);  INIT_TCB_MV(1);  INIT_TCB_MV(2);  INIT_TCB_MV(3); 
    INIT_TCB_MV(4);  INIT_TCB_MV(5);  INIT_TCB_MV(6);  INIT_TCB_MV(7); 
    INIT_TCB_MV(8);  INIT_TCB_MV(9);  INIT_TCB_MV(10); INIT_TCB_MV(11); 
    INIT_TCB_MV(12); INIT_TCB_MV(13); INIT_TCB_MV(14); INIT_TCB_MV(15); 
    INIT_TCB_MV(16); INIT_TCB_MV(17); INIT_TCB_MV(18); INIT_TCB_MV(19); 
    INIT_TCB_MV(20); INIT_TCB_MV(21); INIT_TCB_MV(23); INIT_TCB_MV(24); 
    INIT_TCB_MV(24); INIT_TCB_MV(25); INIT_TCB_MV(26); INIT_TCB_MV(27); 
    INIT_TCB_MV(28); INIT_TCB_MV(29); INIT_TCB_MV(30); INIT_TCB_MV(31); 
} // init_tcb

} // Genesis


namespace CommonLisp
{

using namespace Genesis;


Val disassemble(Val thing)
{
    init_tcb();

    X86Disassembler oDisassembler(thing);
        oDisassembler.Run();
    return nil;
} // disassemble

} // CommonLisp
