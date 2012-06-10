//////////////////////////////////////////////////////////////////////////////
//
// evcl - x86 Assembler
// arch/x86/x86_asm.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/x86_asm.h#10 $
//
#if !defined(INCLUDE_arch_x86_x86_asm_h)
#define INCLUDE_arch_x86_x86_asm_h

#include "../../mini/mini_lisp.h"
#include "../../kernel/ke_memory.h"

#include "./x86_arch.h"
#include "./kernel/x86_ke_mach.h"
#include "./kernel/x86_ke_layout.h"

namespace X86
{

//////////////////////////////////////////////////////////////////////
//
// X86Assembler
//
class X86Assembler :
    public Kernel::Arch
{
    ////////////////////////////////////////////////////////////
    //
    // X86 Operands
    //
    public: enum Reg
    {
        none = -1,

        eax = Gpr_EAX, ecx = Gpr_ECX, edx = Gpr_EDX, ebx = Gpr_EBX,
        esp = Gpr_ESP, ebp = Gpr_EBP, esi = Gpr_ESI, edi = Gpr_EDI,

        $rn = ecx,

        $r0 = eax, $r1 = edx, $r2 = ebx, $r3 = esi, $r4 = edi,

        $tcb = ebp,
        $sp  = esp,
    }; // Reg

    //    7 6  5 4 3  2 1 0    7 6 5 4 3 2 1 0
    //   +----+------+------+ +---+-----+------+ +----------------+
    //   |mod | reg  |  r/m | | S | idx | base | |  disp8/disp32  |
    //   +----+------+------+ +----------------+ +----------------+
    public: enum Mod
    {
        mod_disp0   = 0x00,
        mod_disp8   = 0x40,
        mod_disp32  = 0x80,
        mod_reg     = 0xC0,
    }; // Mod

    enum Rm
    {
        rm_sib      = 4,
        rm_disp32   = 5,
    }; // Rm


    enum Scale
    {
        scale_none  = 1,
        scale_1     = 0,
        scale_2     = 0x40,
        scale_4     = 0x80,
        scale_8     = 0xC0,
    }; // Scale

    ////////////////////////////////////////////////////////////
    //
    // Condition Code
    //
    enum Tttn
    {
       tttn_O     = 0,          // overflow
       tttn_NO    = 1,          // no overflow
       tttn_B     = 2,          // below (ULT)
         tttn_NAE  = tttn_B,    // not above or equal
       tttn_NB    = 3,          // not below (UGE)
         tttn_AE = tttn_NB,     // above or equal
       tttn_E     = 4,          // equal
         tttn_Z = tttn_E,       // zero
       tttn_NE    = 5,          // not equal
         tttn_NZ = tttn_NE,     // not zero
       tttn_BE    = 6,          // below or equal (ULE)
         tttn_NA = tttn_BE,     // not above
       tttn_NBE   = 7,          // not below or equal (UGT)
         tttn_A = tttn_NBE,     // above
       tttn_S     = 8,          // sign
       tttn_NS    = 9,          // not sign
       tttn_P     = 10,         // parity
         tttn_PE = tttn_P,      // parity even
       tttn_NP    = 11,         // not parity
         ttttn_PO = tttn_NP,    // parity odd
       tttn_L     = 12,         // less than
         tttn_NGE = tttn_L,     // not greater than or equal to
       tttn_GE    = 13,         // greater than or equal to
         tttn_NL = tttn_GE,     // not less than
       tttn_LE    = 14,         // less than or equal to
         tttn_NG = tttn_LE,     // not greater than
       tttn_G     = 15,         // greater than
         tttn_NLE = tttn_G,     // not less than
    }; // Tttn

    public: enum XmmReg
    {
        xmm0,
        xmm1,
        xmm2,
        xmm3,
        xmm4,
        xmm5,
        xmm6,
        xmm7,
    }; // XmmReg

    public: enum CmpPx
    {
        cmppd_eq,
        cmppd_lt,
        cmppd_le,
        cmppd_unord,
        cmppd_neq,
        cmppd_nlt,
        cmppd_nle,
        cmppd_ord,

        cmpps_eq,
        cmpps_lt,
        cmpps_le,
        cmpps_unord,
        cmpps_neq,
        cmpps_nlt,
        cmpps_nle,
        cmpps_ord,
    }; // CmpPx

    ////////////////////////////////////////////////////////////
    //
    // FunObj
    //  Layout of function object.
    //
    class FunObj : public X86::Function
        { friend class X86Assembler; };

    ////////////////////////////////////////////////////////////
    //
    // Context
    //  Layout of thread.
    //
    class Context : public Kernel::Thread
        { friend class X86Assembler; };

    class Area : public Kernel::Area
        { friend class X86Assembler; };

    ////////////////////////////////////////////////////////////
    //
    // Label
    //
    public: class Label
    {
        public: Label*  m_pPrev;
        public: uint    m_nAddr;
        public: uint    m_ofsCode;
        public: Label() : m_nAddr(static_cast<uint>(-1)) {}
    }; // Label

    protected: Label* m_pLastLabel;

    ////////////////////////////////////////////////////////////
    //
    // Ea - Effective Address
    //
    public: class Ea
    {
        public: Reg     m_base;
        public: int32   m_disp;
        public: Reg     m_index;
        public: Scale   m_scale;
        public: Label*  m_pLabel;
    }; // Ea

    public: class Jump
    {
        public: Label*  m_pLabel;
        public: uint    m_ofsCode;
        public: uint    m_nAddr;
        public: uint    m_nOpcode;
    }; // Jump

    public: class Annon
    {
        public: FunObj::Annon::Type m_eType;
        public: Val                 m_value;
        public: uint                m_nAddr;
    }; // Annon

    public: class Call
    {
        public: void*   m_pfn;
        public: uint    m_nAddr;
    }; // Call

    public: class DllLink
    {
        public: Val     m_file;
        public: Val     m_proc;
        public: uint    m_nAddr;
    }; // DllLink

    public: class Defun
    {
        public: Val                 m_classd;
        public: Val                 m_name;
        public: int                 m_iMin;
        public: int                 m_iMax;
        public: Label               m_oArityError;
        public: bool                m_fCheckArity;
        public: uint                m_cbFrame;
        public: FunObj::FrameType   m_eFrame;

        public: Defun(X86Assembler* pAsm, Val name, int iMin, int iMax) :
                m_classd(CLASSD_native_code_function),
                m_name(name),
                m_eFrame(FunObj::FrameType_Fixed),
                m_cbFrame(4),   // include return address
                m_iMin(iMin),
                m_iMax(iMax),
                m_fCheckArity(true)
        {
            pAsm->init();
        } // Defun
    }; // Defun

    static int const Mask_Tag = Val_::Mask_Tag;

    uint    m_nAddr;
    uint    m_ofsCode;
    BYTE    m_rgbCode[1024];

    uint    m_cAnnons;
    Annon   m_rgoAnnon[100];

    uint    m_cJumps;
    Jump    m_rgoJump[100];

    uint    m_cCalls;
    Call    m_rgoCall[100];

    uint    m_cDllLinks;
    DllLink m_rgoDllLink[100];

    Ea m_oEa;

    void init()
    {
        m_ofsCode   = 0;
        m_nAddr     = 0;
        m_cAnnons   = 0;
        m_cCalls    = 0;
        m_cDllLinks = 0;
        m_cJumps    = 0;
        m_pLastLabel = NULL;
    } // init

    ////////////////////////////////////////////////////////////
    //
    // Operation Code
    //
    enum OpCode
    {
        #define DEFFORMAT_0(mp_op, mp_mnemonic) \
            op_##mp_mnemonic = mp_op,

        #define DEFFORMAT_1(mp_op, mp_mnemonic, mp_1) \
            op_##mp_mnemonic##_##mp_1 = mp_op,

        #define DEFFORMAT_EXT_1(mp_op, mp_opext, mp_mnemonic, mp_1) \
            op_##mp_mnemonic##_##mp_1 = mp_op,

        #define DEFFORMAT_2(mp_op, mp_mnemonic, mp_1, mp_2) \
            op_##mp_mnemonic##_##mp_1##_##mp_2 = mp_op,

        #define DEFFORMAT_3(mp_op, mp_mnemonic, mp_1, mp_2, mp_3) \
            op_##mp_mnemonic##_##mp_1##_##mp_2##_##mp_3 = mp_op,

        #define DEFFORMAT_EXT_2(mp_op, mp_opext, mp_mnemonic, mp_1, mp_2) \
            op_##mp_mnemonic##_##mp_1##_##mp_2 = mp_op,

        #define DEFFORMAT_EXT_3(mp_op, mp_opext, mp_mnemonic, \
                                        mp_1, mp_2, mp_3) \
            op_##mp_mnemonic##_##mp_1##_##mp_2##_##mp_3 = mp_op,

        #include "./x86_opcode.inc"
    }; // OpCode

    enum OpExt
    {
        #define DEFFORMAT_EXT_1(mp_op, mp_opext, mp_mnemonic, mp_1) \
            opext_##mp_mnemonic##_##mp_1 = mp_opext,

        #define DEFFORMAT_EXT_2(mp_op, mp_opext, mp_mnemonic, mp_1, mp_2) \
            opext_##mp_mnemonic##_##mp_1##_##mp_2 = mp_opext,

        #define DEFFORMAT_EXT_3(mp_op, mp_opext, mp_mnemonic, \
                mp_1, mp_2) \
            opext_##mp_mnemonic##_##mp_1##_##mp_2##_##mp_3 = mp_opext,

        #include "./x86_opcode.inc"
    }; // Op

    public: uint8 modrm(Mod mod, Reg reg, Reg rm)
        { return static_cast<uint8>(mod | ((reg & 7) << 3) | (rm & 7)); }

    public: uint8 modrm(Mod mod, OpExt ext, Reg rm)
        { return static_cast<uint8>(mod | (ext << 3) | (rm & 7)); }

    public: uint8 modrm(Mod mod, Reg reg, Rm rm)
        { return static_cast<uint8>(mod | ((reg & 7) << 3) | (rm & 7)); }

    public: uint8 modrm(Mod mod, OpExt opext, Rm rm)
        { return static_cast<uint8>(mod | (opext << 3) | (rm & 7)); }

    public: uint8 sib(Scale s, Reg idx, Reg baze)
        { return static_cast<uint8>(s | ((idx & 7) << 3) | (baze & 7)); }

    ////////////////////////////////////////////////////////////
    //
    // Emitter
    //
    protected: void add_annon(FunObj::Annon::Type eType, Val val)
    {
        ASSERT(m_cAnnons < lengthof(m_rgoAnnon));

        Annon* pAnnon = &m_rgoAnnon[m_cAnnons++];
            pAnnon->m_eType = eType;
            pAnnon->m_value = val;
            pAnnon->m_nAddr = m_nAddr;
    } // add_annon

    public: void emit_lit(Val val)
    {
        if (immediate_p(val))
        {
            emit_s32(static_cast<int32>(val->ToInt()));
        }
        else
        {
            add_annon(FunObj::Annon::Type_LispVal, val);
            emit_u32(0);
        }
    } // emit_lit

    public: void emit_op(uint op)
    {
        if (op > 0xFFFF)
        {
            emit_u8(static_cast<uint8>(op >> 16));
        }

        if (op > 0xFF)
        {
            emit_u8(static_cast<uint8>(op >> 8));
        }
        emit_u8(static_cast<uint8>(op));
    } // emit_op

    public: void emit_s8(int8 n)
        { emit_u8(static_cast<uint8>(n)); }

    public: void emit_s16(int16 n)
        { emit_u16(static_cast<uint16>(n)); }

    public: void emit_s32(int32 n)
        { emit_u32(static_cast<uint32>(n)); }


    public: void emit_u16(uint16 n)
    {
        emit_u8(static_cast<uint8>(n & 0xFF));
        emit_u8(static_cast<uint8>(n >> 8));
    } // emit_u16

    public: void emit_u32(uint32 n)
    {
        emit_u8(static_cast<uint8>(n & 0xFF));
        emit_u8(static_cast<uint8>(n >>  8));
        emit_u8(static_cast<uint8>(n >> 16));
        emit_u8(static_cast<uint8>(n >> 24));
    } // emit_u32

    public: void emit_u8(uint8 n)
    {
        ASSERT(m_ofsCode < lengthof(m_rgbCode));
        m_rgbCode[m_ofsCode++] = n;
        m_nAddr += 1;
    } // emit_u8

    #define EMIT_OP_EA(mp_op) \
        emit_op(op_##mp_op); emit_ea(opext_##mp_op, pEa);

    #define EMIT_OP_EXT(mp_op, mp_rm) \
        emit_op(op_##mp_op); emit_u8(modrm(mod_reg, opext_##mp_op, mp_rm));

    public: void emit_ea(OpExt opext, Ea* pEa)
        { emit_ea(static_cast<Reg>(opext), pEa); }

    // emit_ea
    //  0x24 = ss=00 idx=100 base=100 == no index, base=esp
    public: void emit_ea(Reg reg, Ea* pEa)
    {
        if (NULL != pEa->m_pLabel)
        {
            ASSERT(none != pEa->m_base);
            emit_u8(modrm(mod_disp32, reg, pEa->m_base));
            if (esp == pEa->m_base) emit_u8(0x24);
            add_annon(
                FunObj::Annon::Type_Label,
                Fixnum::Encode(pEa->m_pLabel) );
            emit_u32(0);
            return;
        } // if

        if (none == pEa->m_base)
        {
            // [disp32]
            if (none == pEa->m_index)
            {
                emit_u8(modrm(mod_disp0, reg, rm_disp32));
            }
            else
            {
                emit_u8(modrm(mod_disp0, reg, rm_sib));
                emit_u8(sib(pEa->m_scale, pEa->m_index, ebp));
            }
            emit_u32(static_cast<uint32>(pEa->m_disp));
        }
        else if (none == pEa->m_index)
        {
            // [base+disp]
            if (0 == pEa->m_disp && ebp != pEa->m_base)
            {
                emit_u8(modrm(mod_disp0, reg, pEa->m_base));
                if (esp == pEa->m_base) emit_u8(0x24);
            }
            else if (pEa->m_disp >= -128 && pEa->m_disp <= 127)
            {
                emit_u8(modrm(mod_disp8, reg, pEa->m_base));
                if (esp == pEa->m_base) emit_u8(0x24);
                emit_s8(static_cast<int8>(pEa->m_disp));
            }
            else
            {
                emit_u8(modrm(mod_disp32, reg, pEa->m_base));
                if (esp == pEa->m_base) emit_u8(0x24);
                emit_s32(static_cast<int32>(pEa->m_disp));
            }
        }
        else
        {
            // We can't use esp as index.
            ASSERT(esp != pEa->m_index);

            // [base+index+disp]
            if (0 == pEa->m_disp && ebp != pEa->m_base)
            {
                emit_u8(modrm(mod_disp0, reg, rm_sib));
                emit_u8(sib(pEa->m_scale, pEa->m_index, pEa->m_base)); 
            }
            else if (pEa->m_disp >= -128 && pEa->m_disp <= 127)
            {
                emit_u8(modrm(mod_disp8, reg, rm_sib));
                emit_u8(sib(pEa->m_scale, pEa->m_index, pEa->m_base));
                emit_s8(static_cast<int8>(pEa->m_disp));
            }
            else
            {
                emit_u8(modrm(mod_disp32, reg, rm_sib));
                emit_u8(sib(pEa->m_scale, pEa->m_index, pEa->m_base));
                emit_s32(static_cast<int32>(pEa->m_disp));
            }
        } // if
    } // emit_ea

    public: Reg reg_arg(uint i)
        { return static_cast<Reg>(X86::X86Mach::ISA.m_pGprArg->Get(i)); }

    public: uint reg_arg_limit()
        { return X86::X86Mach::ISA.m_pGprArg->m_n; }

    public: Ea* ea(
        Reg     eBase,
        Int     iDisp = 0,
        Reg     eIndex = none,
        Scale   eScale = scale_1 )
    {
        m_oEa.m_base   = eBase;
        m_oEa.m_disp   = static_cast<int32>(iDisp);
        m_oEa.m_index  = eIndex;
        m_oEa.m_scale  = eScale;
        m_oEa.m_pLabel = NULL;
        return &m_oEa;
    } // ea

    public: Ea* ea(
        Reg     eBase,
        Label&  rLabel,
        Int     iDisp = 0,
        Reg     eIndex = none,
        Scale   eScale = scale_1 )
    {
        m_oEa.m_base   = eBase;
        m_oEa.m_disp   = static_cast<int32>(iDisp);
        m_oEa.m_index  = eIndex;
        m_oEa.m_scale  = eScale;
        m_oEa.m_pLabel = &rLabel;
        return &m_oEa;
    } // ea

    public: Ea* ea(void* pv)
    {
        m_oEa.m_base   = none;
        m_oEa.m_disp   = static_cast<int32>(reinterpret_cast<Int>(pv));
        m_oEa.m_index  = none;
        m_oEa.m_scale  = scale_none;
        m_oEa.m_pLabel = NULL;
        return &m_oEa;
    } // ea

    public: Ea* ea_m_fn() { return ea($tcb, offsetof(Context, m_fn)); }
    public: Ea* ea_m_fp() { return ea($tcb, offsetof(Context, m_fp)); }
    public: Ea* ea_m_n()  { return ea($tcb, offsetof(Context, m_n)); }

    public: Ea* ea_mv_value(Reg index)
    {
        return ea($tcb, offsetof(Context, mv_value), index);
    } // ea_mv_value

    public: Ea* ea_mv_value(uint n)
    {
        ASSERT(n < X86::X86Mach::Multiple_Values_Limit);
        return ea($tcb, offsetof(Context, mv_value) + n * sizeof(Val));
    } // ea_mv_value

    public: Ea* ea_mv_count(uint n)
    {
        ASSERT(n < Context::ObjectStat::TypeCode_MAX_1);
        return ea(
            $tcb,
            offsetof(Context, mv_stat) +
                offsetof(Context::ObjectStat, m_count) );
    } // ea_mv_count

    public: Ea* ea_mv_size(uint n)
    {
        ASSERT(n < Context::ObjectStat::TypeCode_MAX_1);
        int32 ofs = offsetof(Context, mv_stat);
            ofs += n * sizeof(Context::ObjectStat);
            ofs += offsetof(Context::ObjectStat, m_size);
        return ea($tcb, ofs);
    } // ea_mv_size

    public: DllLink* dlllink(LPCWSTR pwszFile, LPCWSTR pwszProc)
    {
        DllLink* p = &m_rgoDllLink[m_cDllLinks];
            p->m_file     = make_string(pwszFile);
            p->m_proc     = make_string(pwszProc);
            p->m_nAddr    = m_nAddr;
        m_cDllLinks += 1;
        return p;
    } // dlllink

    ////////////////////////////////////////////////////////////
    //
    // Label
    //
    public: void label(Label& rLabel)
    {
        rLabel.m_pPrev = m_pLastLabel;
        rLabel.m_ofsCode = m_ofsCode;
        rLabel.m_nAddr   = m_nAddr;
        m_pLastLabel = &rLabel;
    } // label

    public: void resolve_jumps();
    public: void shift_labels(uint, uint);

    ////////////////////////////////////////////////////////////
    //
    // Builder
    //
    Val makeFunObj(const Defun*);
    Val installFunObj(const Defun*);

    ////////////////////////////////////////////////////////////
    //
    // Common Instruction Formats
    //

    // asm_arith -- ADD Ev, Gv
    protected: void asm_arith(uint opext, Ea* pEa, Reg ry)
    {
        emit_op(op_ADD_Ev_Gv + opext * 8);
        emit_ea(ry, pEa);
    } // asm_arith

    // asm_arith -- ADD Ev, Iz
    protected: void asm_arith(uint opext, Ea* pEa, int32 imm)
    {
        if (imm >= -128 && imm <= 128)
        {
            // ADD Ev, Ib
            emit_op(0x83);
            emit_ea(static_cast<Reg>(opext), pEa);
            emit_s8(static_cast<int8>(imm));
        }
        else
        {
            // ADD Ev, Iz
            emit_op(0x81);
            emit_ea(static_cast<Reg>(opext), pEa);
            emit_s32(static_cast<int32>(imm));
        }
    } // asm_arith

    // asm_arith -- ADD Gv, Ea
    protected: void asm_arith(uint opext, Reg rd, Ea* pEa)
    {
        emit_op(op_ADD_Gv_Ev + opext * 8);
        emit_ea(rd, pEa);
    } // asm_arith

    // asm_arith -- ADD Ev, Iv
    protected: void asm_arith(uint opext, Reg reg, int32 imm)
    {
        if (imm >= -128 && imm <= 128)
        {
            // ADD Ev, Ib
            emit_op(0x83);
            emit_u8(modrm(mod_reg, static_cast<Reg>(opext), reg));
            emit_s8(static_cast<int8>(imm));
        }
        else if (eax == reg)
        {
            // ADD eAX, Iv
            emit_op(op_ADD_eAX_Iz + opext * 8);
            emit_s32(static_cast<int32>(imm));
        }
        else
        {
            // ADD Ev, Iv
            emit_op(0x81);
            emit_u8(modrm(mod_reg, static_cast<Reg>(opext), reg));
            emit_s32(static_cast<int32>(imm));
        }
    } // asm_arith

    // asm_arith -- ADD Ev, Iv
    protected: void asm_arith(uint opext, Reg reg, Val val)
    {
        if (fixnump(val) &&
            Fixnum::Decode_(val) >= -128 &&
            Fixnum::Decode_(val) <= 127 )
        {
            asm_arith(opext, reg, static_cast<int32>(val->ToInt()));
        }
        else if (eax == reg)
        {
            // ADD eAX, Iv
            emit_op(op_ADD_eAX_Iz + opext * 8);
            emit_lit(val);
        }
        else
        {
            emit_op(op_ADD_Ev_Iz);
            emit_u8(modrm(mod_reg, static_cast<Reg>(opext), reg));
            emit_lit(val);
        }
    } // asm_arith

    // asm_arith -- ADD Gv, Ev
    protected: void asm_arith(uint opext, Reg rd, Reg ry)
    {
        emit_op(op_ADD_Gv_Ev + opext * 8);
        emit_u8(modrm(mod_reg, rd, ry));
    } // asm_arith

    // asm_cmov
    protected: void asm_cmov(uint opcode, Reg rd, Reg rx)
    {
        emit_op(opcode);
        emit_u8(modrm(mod_reg, rd, rx));
    } // asm_cmov

    // asm_Ev - for DIV, IDIV, MUL, IMUL
    protected: void asm_Ev(uint opcode, OpExt opext, Reg rx)
    {
        emit_op(opcode);
        emit_u8(modrm(mod_reg, opext, rx));
    } // asm_Ev

    // asm_Ev - for CALL
    protected: void asm_Ev(uint opcode, OpExt opext, DllLink* pLink)
    {
        emit_op(opcode);
        emit_u8(modrm(mod_disp0, opext, rm_disp32));
        add_annon(FunObj::Annon::Type_DllLink, Fixnum::Encode(pLink));
        emit_u32(0);
    } // asm_Ev

    // asm_Jb
    protected: void asm_Jb(uint opcode, Label& rLabel);

    protected: void asm_Jv(uint opcode, OpExt opext, Ea* pEa)
    {
        emit_op(opcode);
        emit_ea(opext, pEa);
    } // asm_Jv

    protected: void asm_Jv(uint opcode, Val name)
    {
        emit_op(opcode);
        add_annon(FunObj::Annon::Type_NamedCallee, name);
        emit_u32(0);
    } // asm_Jv

    ////////////////////////////////////////////////////////////
    //
    // Mnemonic
    //
    public: void add(Ea* ed, Reg rx) { asm_arith(0, ed, rx); }
    public: void  or(Ea* ed, Reg rx) { asm_arith(1, ed, rx); }
    public: void adc(Ea* ed, Reg rx) { asm_arith(2, ed, rx); }
    public: void sbb(Ea* ed, Reg rx) { asm_arith(3, ed, rx); }
    public: void and(Ea* ed, Reg rx) { asm_arith(4, ed, rx); }
    public: void sub(Ea* ed, Reg rx) { asm_arith(5, ed, rx); }
    public: void xor(Ea* ed, Reg rx) { asm_arith(6, ed, rx); }
    public: void cmp(Ea* ed, Reg rx) { asm_arith(7, ed, rx); }

    public: void add(Reg rd, Ea* ex) { asm_arith(0, rd, ex); }
    public: void  or(Reg rd, Ea* ex) { asm_arith(1, rd, ex); }
    public: void adc(Reg rd, Ea* ex) { asm_arith(2, rd, ex); }
    public: void sbb(Reg rd, Ea* ex) { asm_arith(3, rd, ex); }
    public: void and(Reg rd, Ea* ex) { asm_arith(4, rd, ex); }
    public: void sub(Reg rd, Ea* ex) { asm_arith(5, rd, ex); }
    public: void xor(Reg rd, Ea* ex) { asm_arith(6, rd, ex); }
    public: void cmp(Reg rd, Ea* ex) { asm_arith(7, rd, ex); }

    public: void add(Reg rd, Reg rx) { asm_arith(0, rd, rx); }
    public: void  or(Reg rd, Reg rx) { asm_arith(1, rd, rx); }
    public: void adc(Reg rd, Reg rx) { asm_arith(2, rd, rx); }
    public: void sbb(Reg rd, Reg rx) { asm_arith(3, rd, rx); }
    public: void and(Reg rd, Reg rx) { asm_arith(4, rd, rx); }
    public: void sub(Reg rd, Reg rx) { asm_arith(5, rd, rx); }
    public: void xor(Reg rd, Reg rx) { asm_arith(6, rd, rx); }
    public: void cmp(Reg rd, Reg rx) { asm_arith(7, rd, rx); }

    public: void add(Reg rd, int32 imm) { asm_arith(0, rd, imm); }
    public: void  or(Reg rd, int32 imm) { asm_arith(1, rd, imm); }
    public: void adc(Reg rd, int32 imm) { asm_arith(2, rd, imm); }
    public: void sbb(Reg rd, int32 imm) { asm_arith(3, rd, imm); }
    public: void and(Reg rd, int32 imm) { asm_arith(4, rd, imm); }
    public: void sub(Reg rd, int32 imm) { asm_arith(5, rd, imm); }
    public: void xor(Reg rd, int32 imm) { asm_arith(6, rd, imm); }
    public: void cmp(Reg rd, int32 imm) { asm_arith(7, rd, imm); }

    public: void add(Reg rd, Val val) { asm_arith(0, rd, val); }
    public: void  or(Reg rd, Val val) { asm_arith(1, rd, val); }
    public: void adc(Reg rd, Val val) { asm_arith(2, rd, val); }
    public: void sbb(Reg rd, Val val) { asm_arith(3, rd, val); }
    public: void and(Reg rd, Val val) { asm_arith(4, rd, val); }
    public: void sub(Reg rd, Val val) { asm_arith(5, rd, val); }
    public: void xor(Reg rd, Val val) { asm_arith(6, rd, val); }
    public: void cmp(Reg rd, Val val) { asm_arith(7, rd, val); }

    public: void call(Ea* pEa)  { asm_Jv(op_CALL_Ev, opext_CALL_Ev, pEa); }
    public: void call(Val name) { asm_Jv(op_CALL_Jv, name); }

    public: void call(DllLink* pDllLink)
        { asm_Ev(op_CALL_Ev, opext_CALL_Ev, pDllLink); }

    public: void clc() { emit_op(op_CLC); }

    public: void cmovo(Reg rd, Reg rx)  { asm_cmov(op_CMOVO_Gv_Ev, rd, rx); }
    public: void cmovno(Reg rd, Reg rx) { asm_cmov(op_CMOVNO_Gv_Ev, rd, rx); }
    public: void cmovb(Reg rd, Reg rx)  { asm_cmov(op_CMOVB_Gv_Ev, rd, rx); }
    public: void cmovae(Reg rd, Reg rx) { asm_cmov(op_CMOVAE_Gv_Ev, rd, rx); }
    public: void cmove(Reg rd, Reg rx)  { asm_cmov(op_CMOVE_Gv_Ev, rd, rx); }
    public: void cmovne(Reg rd, Reg rx) { asm_cmov(op_CMOVNE_Gv_Ev, rd, rx); }
    public: void cmovbe(Reg rd, Reg rx) { asm_cmov(op_CMOVBE_Gv_Ev, rd, rx); }
    public: void cmova(Reg rd, Reg rx)  { asm_cmov(op_CMOVA_Gv_Ev, rd, rx); }
    public: void cmovs(Reg rd, Reg rx)  { asm_cmov(op_CMOVS_Gv_Ev, rd, rx); }
    public: void cmovns(Reg rd, Reg rx) { asm_cmov(op_CMOVNS_Gv_Ev, rd, rx); }
    public: void cmovpe(Reg rd, Reg rx) { asm_cmov(op_CMOVPE_Gv_Ev, rd, rx); }
    public: void cmovpo(Reg rd, Reg rx) { asm_cmov(op_CMOVPO_Gv_Ev, rd, rx); }
    public: void cmovl(Reg rd, Reg rx)  { asm_cmov(op_CMOVL_Gv_Ev, rd, rx); }
    public: void cmovge(Reg rd, Reg rx) { asm_cmov(op_CMOVGE_Gv_Ev, rd, rx); }
    public: void cmovle(Reg rd, Reg rx) { asm_cmov(op_CMOVLE_Gv_Ev, rd, rx); }
    public: void cmovg(Reg rd, Reg rx)  { asm_cmov(op_CMOVG_Gv_Ev, rd, rx); }

    public: void cmp(Ea* pEa, int imm)
        { EMIT_OP_EA(CMP_Ev_Iz); emit_s32(imm); }

    public: void cmp(Ea* pEa, Val val)
        { EMIT_OP_EA(CMP_Ev_Iz); emit_lit(val); }

    public: void div(Reg rx)
        { asm_Ev(op_DIV_Ev, opext_DIV_Ev, rx); }

    public: void idiv(Reg rx)
        { asm_Ev(op_IDIV_Ev, opext_IDIV_Ev, rx); }

    public: void mul(Reg rx)
        { asm_Ev(op_MUL_Ev, opext_MUL_Ev, rx); }

    public: void imul(Reg rx)
        { asm_Ev(op_IMUL_Ev, opext_IMUL_Ev, rx); }

    public: void jo(Label& x)       { asm_Jb(op_JO_Jb, x); }
    public: void jno(Label& x)      { asm_Jb(op_JNO_Jb, x); }
    public: void jb(Label& x)       { asm_Jb(op_JB_Jb, x); }
        public: void jc(Label& x)   { asm_Jb(op_JB_Jb, x); }
        public: void jnae(Label& x) { asm_Jb(op_JB_Jb, x); }
    public: void jae(Label& x)      { asm_Jb(op_JAE_Jb, x); }
        public: void jnb(Label& x)  { asm_Jb(op_JAE_Jb, x); }
        public: void jnc(Label& x)  { asm_Jb(op_JAE_Jb, x); }
    public: void je(Label& x)       { asm_Jb(op_JE_Jb, x); }
        public: void jz(Label& x)   { asm_Jb(op_JE_Jb, x); }
    public: void jne(Label& x)      { asm_Jb(op_JNE_Jb, x); }
        public: void jnz(Label& x)  { asm_Jb(op_JNE_Jb, x); }
    public: void jbe(Label& x)      { asm_Jb(op_JBE_Jb, x); }
        public: void jna(Label& x)  { asm_Jb(op_JBE_Jb, x); }
    public: void ja(Label& x)       { asm_Jb(op_JA_Jb, x); }
        public: void jnbe(Label& x) { asm_Jb(op_JA_Jb, x); }
    public: void js(Label& x)       { asm_Jb(op_JS_Jb, x); }
    public: void jns(Label& x)      { asm_Jb(op_JNS_Jb, x); }
    public: void jpe(Label& x)      { asm_Jb(op_JPE_Jb, x); }
    public: void jpo(Label& x)      { asm_Jb(op_JPO_Jb, x); }
    public: void jl(Label& x)       { asm_Jb(op_JL_Jb, x); }
    public: void jge(Label& x)      { asm_Jb(op_JGE_Jb, x); }
    public: void jle(Label& x)      { asm_Jb(op_JLE_Jb, x); }
    public: void jg(Label& x)       { asm_Jb(op_JG_Jb, x); }

    public: void jmp(Label& x)  { asm_Jb(op_JMP_Jb, x); }
    public: void jmp(Ea* pEa)   { asm_Jv(op_JMP_Ev, opext_JMP_Ev, pEa); }
    public: void jmp(Val name)  { asm_Jv(op_JMP_Jv, name); }

    public: void mov(Ea* pEa, int32 imm)
        { EMIT_OP_EA(MOV_Ev_Iz); emit_u32(static_cast<uint32>(imm)); }

    public: void mov(Ea* pEa, Reg rx)
        { emit_op(op_MOV_Ev_Gv); emit_ea(rx, pEa); }

    public: void mov(Ea* pEa, Val val)
        { EMIT_OP_EA(MOV_Ev_Iz); emit_lit(val); }

    public: void mov(Reg rd, Ea* pEa)
        { emit_op(op_MOV_Gv_Ev); emit_ea(rd, pEa); }

    public: void mov(Reg rd, int32 imm)
        { EMIT_OP_EXT(MOV_Ev_Iz, rd); emit_u32(static_cast<uint32>(imm)); }

    public: void mov(Reg rd, Reg rx)
    {
        if (rd != rx)
        {
            emit_op(op_MOV_Gv_Ev); emit_u8(modrm(mod_reg, rd, rx));
        }
    } // mov

    public: void mov(Reg rd, Val val)
        { EMIT_OP_EXT(MOV_Ev_Iz, rd); emit_lit(val); }

    public: void movzx(Reg rd, Ea* pEa)
        { emit_op(op_MOVZX_Gv_Eb); emit_ea(rd, pEa); }

    public: void movzxw(Reg rd, Ea* pEa)
        { emit_op(op_MOVZX_Gv_Ew); emit_ea(rd, pEa); }

    public: void lea(Reg rd, Ea* pEa)
        { emit_op(op_LEA_Gv_M); emit_ea(rd, pEa); }

    public: void pop(Reg rx)    { emit_op(op_POP_rAX + (rx & 7)); }
    public: void push(Reg rx)   { emit_op(op_PUSH_rAX + (rx & 7)); }
    public: void push(int32 imm) { emit_op(op_PUSH_Iz); emit_s32(imm); }

    public: void pop(Ea*  pEa) { EMIT_OP_EA(POP_Ev); }
    public: void push(Ea* pEa) { EMIT_OP_EA(PUSH_Ev); }

    public: void ret() { emit_op(op_RET); }

    public: void sar(Reg rd, int n)
    {
        ASSERT(n >= 0 && n <= 31);
        if (1 == n)
        {
            EMIT_OP_EXT(SAR_Ev_1, rd);
        }
        else
        {
            EMIT_OP_EXT(SAR_Ev_Ib, rd);
            emit_u8(static_cast<uint8>(n));
        }
    } // sar

    public: void shl(Reg rd, int n)
    {
        ASSERT(n >= 0 && n <= 31);
        if (1 == n)
        {
            EMIT_OP_EXT(SHL_Ev_1, rd);
        }
        else
        {
            EMIT_OP_EXT(SHL_Ev_Ib, rd);
            emit_u8(static_cast<uint8>(n));
        }
    } // shl

    public: void shr(Reg rd, int n)
    {
        ASSERT(n >= 0 && n <= 31);
        if (1 == n)
        {
            EMIT_OP_EXT(SHR_Ev_1, rd);
        }
        else
        {
            EMIT_OP_EXT(SHR_Ev_Ib, rd);
            emit_u8(static_cast<uint8>(n));
        }
    } // shr

    public: void stc() { emit_op(op_STC); }

    public: void test(Ea* pEa, Reg ry)
    {
        emit_op(op_TEST_Ev_Gv);
        emit_ea(ry, pEa);
    } // test

    public: void test(Reg rx, Reg ry)
    {
        emit_op(op_TEST_Ev_Gv);
        emit_u8(modrm(mod_reg, rx, ry));
    } // test

    public: void test(Reg reg, int32 imm)
        { EMIT_OP_EXT(TEST_Ev_Iz, reg); emit_s32(imm); }

    public: void test(Ea* pEa, int32 imm)
        { EMIT_OP_EA(TEST_Ev_Iz); emit_s32(imm); }

    ////////////////////////////////////////////////////////////
    //
    // SSE
    //
    public: void emit_ea(Ea* pEa, XmmReg xmm)
        { emit_ea(static_cast<Reg>(xmm), pEa); }

    protected: void emit_xmm(uint opcode, XmmReg x1, Reg r2)
        { emit_op(opcode); emit_u8(modrm(mod_reg, x1, r2)); }

    protected: void emit_xmm(uint opcode, XmmReg x1, XmmReg x2)
        { emit_op(opcode); emit_u8(modrm(mod_reg, x1, x2)); }

    protected: void emit_xmm(uint opcode, XmmReg x1, Ea* pEa)
        { emit_op(opcode); emit_ea(pEa, x1); }

    public: uint8 modrm(Mod mod, XmmReg x1, Reg r2)
        { return static_cast<uint8>(mod | (x1 << 3) | (r2 & 7)); }

    public: uint8 modrm(Mod mod, XmmReg x1, XmmReg x2)
        { return static_cast<uint8>(mod | (x1 << 3) | x2); }

    public: uint8 modrm(Mod mod, OpExt opext, XmmReg x2)
        { return static_cast<uint8>(mod | (opext << 3) | x2); }

    #define DEF_SSE_ASM_2p(mp_name, mp_NAME) \
        DEF_SSE_ASM_(mp_name##pd, mp_NAME##PD, Vpd, Wpd) \
        DEF_SSE_ASM_(mp_name##ps, mp_NAME##PS, Vps, Wps)

    #define DEF_SSE_ASM_2s(mp_name, mp_NAME) \
        DEF_SSE_ASM_(mp_name##sd, mp_NAME##SD, Vsd, Wsd) \
        DEF_SSE_ASM_(mp_name##ss, mp_NAME##SS, Vss, Wss)

    #define DEF_SSE_ASM_4(mp_name, mp_NAME) \
        DEF_SSE_ASM_(mp_name##pd, mp_NAME##PD, Vpd, Wpd) \
        DEF_SSE_ASM_(mp_name##ps, mp_NAME##PS, Vps, Wps) \
        DEF_SSE_ASM_(mp_name##sd, mp_NAME##SD, Vsd, Wsd) \
        DEF_SSE_ASM_(mp_name##ss, mp_NAME##SS, Vss, Wss)

    #define DEF_SSE_ASM_(mp_name, mp_NAME, mp_opd1, mp_opd2) \
        public: void mp_name(XmmReg xmm1, XmmReg xmm2) \
            { emit_xmm( \
                op_ ##mp_NAME ##_ ##mp_opd1 ##_ ##mp_opd2, \
                xmm1, xmm2 ); } \
        public: void mp_name(XmmReg xmm, Reg r32) \
            { emit_xmm( \
                op_ ##mp_NAME ##_ ##mp_opd1 ##_ ##mp_opd2, \
                xmm, r32 ); } \
        public: void mp_name(XmmReg xmm, Ea* m128) \
            { emit_xmm( \
                op_ ##mp_NAME ##_ ##mp_opd1 ##_ ##mp_opd2, \
                xmm, m128 ); }

    DEF_SSE_ASM_2p(and,  AND)
    DEF_SSE_ASM_2p(andn, ANDN)
    DEF_SSE_ASM_2p(or,   OR)
    DEF_SSE_ASM_2p(xor,  XOR)

    DEF_SSE_ASM_4(add, ADD)
    DEF_SSE_ASM_4(div, DIV)
    DEF_SSE_ASM_4(mul, MUL)
    DEF_SSE_ASM_4(sub, SUB)

    DEF_SSE_ASM_4(max, MAX)
    DEF_SSE_ASM_4(min, MIN)

    DEF_SSE_ASM_4(sqrt, SQRT)

    DEF_SSE_ASM_2s(comi,  COMI)
    DEF_SSE_ASM_2s(ucomi, UCOMI)

    #define DEF_SSE_ASM_CMP(mp_name, mp_NAME) \
        public: void cmp##mp_name(XmmReg xmm1, XmmReg xmm2, CmpPx ePred) \
        { \
            emit_op(op_CMP##mp_NAME##_V##mp_name##_W##mp_name##_Ib); \
            emit_u8(modrm(mod_reg, xmm1, xmm2)); \
            emit_u8(static_cast<uint8>(ePred)); \
        } \
        public: void cmp##mp_name(XmmReg xmm, Ea* m128, CmpPx ePred) \
        { \
            emit_op(op_CMP##mp_NAME##_V##mp_name##_W##mp_name##_Ib); \
            emit_ea(m128, xmm); \
            emit_u8(static_cast<uint8>(ePred)); \
        }

    DEF_SSE_ASM_CMP(pd, PD)
    DEF_SSE_ASM_CMP(ps, PS)
    DEF_SSE_ASM_CMP(sd, SD)
    DEF_SSE_ASM_CMP(ss, SS)

    DEF_SSE_ASM_(movapd, MOVAPD, Vpd, Wpd);
    DEF_SSE_ASM_(movd,   MOVD,   Vdq, Ed);
    DEF_SSE_ASM_(movq,   MOVQ,   Vq,  Wq);
    DEF_SSE_ASM_(movsd,  MOVSD,  Vsd, Wsd);
    DEF_SSE_ASM_(movss,  MOVSS,  Vss, Wss);

    public: void movapd(Ea* m128, XmmReg xmm)
        { emit_op(op_MOVAPD_Wpd_Vpd); emit_ea(m128, xmm); }

    public: void movq(Ea* m64, XmmReg xmm)
        { emit_op(op_MOVQ_Wq_Vq); emit_ea(m64, xmm); }

    public: void movsd(Ea* m64, XmmReg xmm)
        { emit_op(op_MOVSD_Wsd_Vsd); emit_ea(m64, xmm); }

    public: void movss(Ea* m32, XmmReg xmm)
        { emit_op(op_MOVSS_Wss_Vss); emit_ea(m32, xmm); }

    ////////////////////////////////////////////////////////////
    //
    // SSE Integer Instructions
    //
    #define DEF_SSE_ASM_Vdq_Wdq(mp_mnemonic, mp_op) \
        public: void mp_mnemonic(XmmReg xmm1, XmmReg xmm2) \
            { \
                emit_op(op_##mp_op##_Vdq_Wdq); \
                emit_u8(modrm(mod_reg, xmm1, xmm2)); \
            }

    #define DEF_SSE_ASM_Udq_Ib(mp_mnemonic, mp_op) \
        public: void mp_mnemonic(XmmReg xmm1, uint8 imm8) \
            { \
                emit_op(op_##mp_op##_Udq_Ib); \
                emit_u8(modrm(mod_reg, opext_##mp_op##_Udq_Ib, xmm1)); \
                emit_u8(imm8); \
            }

    // Shift
    DEF_SSE_ASM_Udq_Ib(pslld,   PSLLD)
    DEF_SSE_ASM_Udq_Ib(pslldq,  PSLLDQ)
    DEF_SSE_ASM_Udq_Ib(psllq,   PSLLQ)
    DEF_SSE_ASM_Udq_Ib(psllw,   PSLLW)

    DEF_SSE_ASM_Udq_Ib(psrad,   PSRAD)
    DEF_SSE_ASM_Udq_Ib(psraw,   PSRAW)

    DEF_SSE_ASM_Udq_Ib(psrld,   PSRLD)
    DEF_SSE_ASM_Udq_Ib(psrldq,  PSRLDQ)
    DEF_SSE_ASM_Udq_Ib(psrlq,   PSRLQ)
    DEF_SSE_ASM_Udq_Ib(psrlw,   PSRLW)

    // MOVD
    DEF_SSE_ASM_Vdq_Wdq(movdqa, MOVDQA)
    DEF_SSE_ASM_Vdq_Wdq(movdqu, MOVDQU)

    // [A]
    DEF_SSE_ASM_Vdq_Wdq(pabsb,      PABSB)
    DEF_SSE_ASM_Vdq_Wdq(pabsd,      PABSD)
    DEF_SSE_ASM_Vdq_Wdq(pabsw,      PABSW)

    DEF_SSE_ASM_Vdq_Wdq(packssdw,   PACKSSDW)
    DEF_SSE_ASM_Vdq_Wdq(packuswb,   PACKUSWB)

    DEF_SSE_ASM_Vdq_Wdq(paddsb,     PADDSB)
    DEF_SSE_ASM_Vdq_Wdq(paddb,      PADDB)
    DEF_SSE_ASM_Vdq_Wdq(paddw,      PADDW)
    DEF_SSE_ASM_Vdq_Wdq(paddsw,     PADDSW)

    DEF_SSE_ASM_Vdq_Wdq(pand,       PAND)
    DEF_SSE_ASM_Vdq_Wdq(pandn,      PANDN)

    DEF_SSE_ASM_Vdq_Wdq(pavgb,      PAVGB)
    DEF_SSE_ASM_Vdq_Wdq(pavgw,      PAVGW)

    // [C]
    DEF_SSE_ASM_Vdq_Wdq(pcksswb,    PCKSSWB)

    DEF_SSE_ASM_Vdq_Wdq(pcmpeqb,    PCMPEQB)
    DEF_SSE_ASM_Vdq_Wdq(pcmpeqd,    PCMPEQD)
    DEF_SSE_ASM_Vdq_Wdq(pcmpeqw,    PCMPEQW)

    DEF_SSE_ASM_Vdq_Wdq(pcmpgtb,    PCMPGTB)
    DEF_SSE_ASM_Vdq_Wdq(pcmpgtd,    PCMPGTD)
    DEF_SSE_ASM_Vdq_Wdq(pcmpgtw,    PCMPGTW)

    // [H]
    DEF_SSE_ASM_Vdq_Wdq(phsubd,     PHSUBD)
    DEF_SSE_ASM_Vdq_Wdq(phsubw,     PHSUBW)
    DEF_SSE_ASM_Vdq_Wdq(phsubsw,    PHSUBSW)

    // [M]
    DEF_SSE_ASM_Vdq_Wdq(pmaddsubsw, PMADDSUBSW)
    DEF_SSE_ASM_Vdq_Wdq(pmaddwd,    PMADDWD)

    DEF_SSE_ASM_Vdq_Wdq(pmaxsw,     PMAXSW)
    DEF_SSE_ASM_Vdq_Wdq(pmaxub,     PMAXUB)

    DEF_SSE_ASM_Vdq_Wdq(pminsw,     PMINSW)
    DEF_SSE_ASM_Vdq_Wdq(pminub,     PMINUB)

    DEF_SSE_ASM_Vdq_Wdq(pmulhrsw,   PMULHRSW)
    DEF_SSE_ASM_Vdq_Wdq(pmulhuw,    PMULHUW)
    DEF_SSE_ASM_Vdq_Wdq(pmulhw,     PMULHW)
    DEF_SSE_ASM_Vdq_Wdq(pmuludq,    PMULUDQ)

    // [O]
    DEF_SSE_ASM_Vdq_Wdq(por,        POR)

    // [S]
    DEF_SSE_ASM_Vdq_Wdq(psadbw,     PSADBW)

    DEF_SSE_ASM_Vdq_Wdq(psignb,     PSIGNB)
    DEF_SSE_ASM_Vdq_Wdq(psignd,     PHSUBD)
    DEF_SSE_ASM_Vdq_Wdq(psignw,     PHSUBW)

    DEF_SSE_ASM_Vdq_Wdq(pslld,      PSLLD)
    DEF_SSE_ASM_Vdq_Wdq(psllw,      PSLLW)
    DEF_SSE_ASM_Vdq_Wdq(psllq,      PSLLQ)

    DEF_SSE_ASM_Vdq_Wdq(psrad,      PSRAD)
    DEF_SSE_ASM_Vdq_Wdq(psraw,      PSRAW)

    DEF_SSE_ASM_Vdq_Wdq(psubb,      PSUBB)
    DEF_SSE_ASM_Vdq_Wdq(psubd,      PSUBD)
    DEF_SSE_ASM_Vdq_Wdq(psubq,      PSUBQ)
    DEF_SSE_ASM_Vdq_Wdq(psubw,      PSUBW)

    DEF_SSE_ASM_Vdq_Wdq(psubsb,     PSUBSB)
    DEF_SSE_ASM_Vdq_Wdq(psubsw,     PSUBSW)

    // [U]
    DEF_SSE_ASM_Vdq_Wdq(punpckhqdq, PUNPCKHQDQ)
    DEF_SSE_ASM_Vdq_Wdq(punpcklqdq, PUNPCKLQDQ)

    DEF_SSE_ASM_Vdq_Wdq(punpckhbw,  PUNPCKHBW)
    DEF_SSE_ASM_Vdq_Wdq(punpckhdq,  PUNPCKHDQ)
    DEF_SSE_ASM_Vdq_Wdq(punpckhwd,  PUNPCKHWD)

    DEF_SSE_ASM_Vdq_Wdq(punpcklbw,  PUNPCKLBW)
    DEF_SSE_ASM_Vdq_Wdq(punpckldq,  PUNPCKLDQ)
    DEF_SSE_ASM_Vdq_Wdq(punpcklwd,  PUNPCKLWD)

    // [X]
    DEF_SSE_ASM_Vdq_Wdq(pxor,       PXOR)

    ////////////////////////////////////////////////////////////
    //
    // Pseudo Instruction
    //
    public: void align(int n)
    {
        while (0 != m_nAddr % n)
        {
            emit_u8(op_NOP);
        } // while
    } // align

    public: void dd(Label& x)
    {
        add_annon(FunObj::Annon::Type_Label, Fixnum::Encode(&x));
        emit_u32(0);
    } // dd

    public: void set_frame_type(
        Defun* pDefun, FunObj::FrameType eFrame, uint cbFrame )
    {
        pDefun->m_eFrame  = eFrame;
        pDefun->m_cbFrame = cbFrame;
    } // set_frame_type

    ////////////////////////////////////////////////////////////
    //
    // Prologue/Epilogue
    //
    protected: void emit_prologue(Defun*);
    protected: void emit_epilogue(Defun*);
}; // Assembler

#define defun(name, min, max) \
    defun_(parse_symbol(L##name), min, max)

#define defun_(name, min, max) \
    { Defun oDefun(this, name, min, max); \
        emit_prologue(&oDefun);

#define end_defun() \
    emit_epilogue(&oDefun); installFunObj(&oDefun); }


#define defun_setf(name, min, max) \
    defun_(list(Qsetf, parse_symbol(name)), min, max)

#define label_def(mp_name) Label mp_name; label(mp_name)

#define frame_type(mp_type, mp_size) \
    set_frame_type(&oDefun, FunObj::FrameType_##mp_type, (mp_size) + 4)

#define OffsetOf(mp_class, mp_slot) \
    ( offsetof(mp_class, mp_slot) - mp_class::Tag )

} // X86

#endif //!defined(INCLUDE_arch_x86_x86_asm_h)
