//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - pre-compiled header
// genesis_lisp.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/genesis/x86x64_gs_25_disasm.h#5 $
//
#if !defined(INCLUDE_arch_x86x64_genesis_x86x64_gs_25_disasm_h)
#define INCLUDE_arch_x86x64_genesis_x86x64_gs_25_disasm_h

#include "../x86x64_opcode.h"
#include "../kernel/x86x64_ke_gcmap.h"
#include "../kernel/x86x64_ke_layout.h"
#include "../../generic/genesis/gen_gs_25_disasm_cics.h"

namespace Genesis
{

//////////////////////////////////////////////////////////////////////
//
// Format
//
struct Format
{
    enum Extend
    {
        Extend_None     = 0,    // One byte opcode
        Extend_ModRm    = 1,    // Opcode with OpExt in ModRm
        Extend_TwoByte  = 2,    // Two byte opcode
    }; // Extend

    enum Operand
    {
        Operand_None    = 0,

        Operand_1,  // SHL Ev, 1

        Operand_Ib, // imm8
        Operand_Iw, // imm16
        Operand_Iq, // imm64
        Operand_Is, // imm8 with sign extend
        Operand_Iv, // imm16, imm32 or imm64
        Operand_Iz, // imm32

        Operand_Jb,
        Operand_Jv,

        Operand_Eb,
        Operand_Ew,
        Operand_Ed,
        Operand_Eq,
        Operand_Ev,

        Operand_Gb,
        Operand_Gd,
        Operand_Gq,
        Operand_Gv,

        Operand_Ob,
        Operand_Ov,

        Operand_M,
        Operand_Md,
        Operand_Mp,
        Operand_Mpd,
        Operand_Mps,
        Operand_Mq,

        Operand_Nq,     // mm

        Operand_Pd,     // mm
        Operand_Pq,     // mm

        Operand_Qd,     // mm or Mq
        Operand_Qq,     // mm or Mq
        Operand_Qdq,

        Operand_Udq,    // ModRm:rm = xmm
        Operand_Upd,    // ModRm:rm = xmm
        Operand_Ups,    // ModRm:rm = xmm

        Operand_Vdq,    // ModRm:reg = xmm
        Operand_Vpd,    // ModRm:reg = xmm
        Operand_Vpq,    // ModRm:reg = xmm
        Operand_Vps,    // ModRm:reg = xmm
        Operand_Vq,     // ModRm:reg = xmm
        Operand_Vsd,    // ModRm:reg = xmm
        Operand_Vss,    // ModRm:reg = xmm

        Operand_Wdq,    // xmm or Mdq
        Operand_Wpd,    // xmm or Mdq
        Operand_Wps,    // xmm or Mdq
        Operand_Wq,     // xmm or Mdq
        Operand_Wsd,    // xmm or Mdq
        Operand_Wss,    // xmm or Mdq

        Operand_eAX,    // 0
        Operand_eCX,    // 1
        Operand_eDX,    // 2
        Operand_eBX,    // 3
        Operand_eSP,    // 4
        Operand_eBP,    // 5
        Operand_eSI,    // 6
        Operand_eDI,    // 7

        Operand_rAX,    // 0    PUSH/POP
        Operand_rCX,    // 1    PUSH/POP
        Operand_rDX,    // 2    PUSH/POP
        Operand_rBX,    // 3    PUSH/POP
        Operand_rSP,    // 4    PUSH/POP
        Operand_rBP,    // 5    PUSH/POP
        Operand_rSI,    // 6    PUSH/POP
        Operand_rDI,    // 7    PUSH/POP

        Operand_AL,     // 0
        Operand_CL,     // 1
        Operand_DL,     // 2
        Operand_BL,     // 3
        Operand_AH,     // 4
        Operand_CH,     // 5
        Operand_DH,     // 6
        Operand_BH,     // 7

        Operand_DX, // For IN, OUT

        Operand_Sw, // reg of modrm selects segment register

        Operand_EvF64,  // CALL/JMP Ev
        Operand_EvD64,  // PUSH Ev

        Operand_CS,
        Operand_DS,
        Operand_ES,
        Operand_SS,
    }; // Oeprand

    const char16*   m_pwszMnemonic;
    uint            m_nOpcode;
    uint            m_cOperands;
    Extend          m_eExtend;
    Operand         m_rgeOperand[3];
}; // Format


//////////////////////////////////////////////////////////////////////
//
// Opcode Table
//
class OpcodeTable
{
    protected: int m_cEntries;
    protected: const Format* m_rgpVector[1033];

    public: OpcodeTable() :
        m_cEntries(0) {}

    public: const Format* Get(uint);
    protected: void populate();
    protected: void put(const Format*);
}; // OpcodeTable


// X86X64Disassembler
class X86X64Disassembler : public CICSDisassembler
{
    protected: enum Prefix
    {
        Prefix_LOCK     = 1<<0,
        Prefix_REPNE    = 1<<1,
        Prefix_REP      = 1<<2,
        Prefix_CS       = 1<<3,
        Prefix_DS       = 1<<4,
        Prefix_ES       = 1<<5,
        Prefix_FS       = 1<<6,
        Prefix_GS       = 1<<7,
        Prefix_SS       = 1<<8,
    }; // Prefix

    protected: enum RegClass
    {
        RegClass_Gpr,   // General Purpose Register
        RegClass_Fpr,   // x87 FPU Regiser
        RegClass_Mmx,   // MMX Register
        RegClass_Xmm,   // XMM Register
    }; // OpdKind

    protected: static const uint NoModRm = 0x100;

    protected: uint     m_rgfPrefix;
    protected: uint     m_nOpdSiz;
    protected: uint     m_nAdrSiz;
    protected: uint     m_nModRm;

    protected: FunObj*  m_pFunObj;
    protected: Val      m_fun;
    protected: Val      m_stream;

    protected: GcMap       m_oGcMap;
    protected: GcMap::Enum m_oGcMapEnum;

    // hasModRm
    protected: bool hasModRm()
        { return NoModRm != m_nModRm; }

    typedef CICSContext Context;

    static OpcodeTable  sm_oOpcodeTable;
    protected: static Format       sm_oFormat_DB;
    static Format       sm_oFormat_DD;

    public: void Run();

    protected: X86X64Disassembler(Val);

    protected: virtual const Format* decode_insn(Context*) = 0;
    protected: virtual Val   disasm_operands(Context*, const Format*) = 0;
    protected: virtual void  setup_labels(Context*) = 0;
    protected: virtual void  disasm_gcmap(uint) = 0;
    protected: Val disasm_opdsiz(Val, uint);

    protected: uint ensure_modrm(Context*);

    const Format*   get_format(Context*, uint);
    void            start();
}; // X86X64Disassembler

} // Genesis

#endif //!defined(INCLUDE_arch_x86x64_genesis_x86x64_gs_25_disasm_h)
