//////////////////////////////////////////////////////////////////////////////
//
// evcl - x64 Assembler
// arch/x64/x64_asm.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/x64_asm.h#15 $
//
#if !defined(INCLUDE_arch_x64_x64_asm_h)
#define INCLUDE_arch_x64_x64_asm_h

#include "../../mini/mini_lisp.h"
#include "../../kernel/ke_memory.h"

#include "./x64_arch.h"
#include "./kernel/x64_ke_mach.h"
#include "./kernel/x64_ke_layout.h"

namespace X64
{

//////////////////////////////////////////////////////////////////////
//
// X64Assembler
//
class X64Assembler :
    public Kernel::Arch
{
    ////////////////////////////////////////////////////////////
    //
    // X64 Operands
    //
    public: enum Reg
    {
        none = 0xF0,

        // 64-bit registers
        rax = Gpr_RAX, rcx = Gpr_RCX, rdx = Gpr_RDX, rbx = Gpr_RBX,
        rsp = Gpr_RSP, rbp = Gpr_RBP, rsi = Gpr_RSI, rdi = Gpr_RDI,
        r8  = Gpr_R8,  r9  = Gpr_R9,  r10 = Gpr_R10, r11 = Gpr_R11,
        r12 = Gpr_R12, r13 = Gpr_R13, r14 = Gpr_R14, r15 = Gpr_R15,

        // 32-bit registers
        eax  = Gpr_EAX,  ecx  = Gpr_ECX,  edx  = Gpr_EDX,  ebx  = Gpr_EBX,
        esp  = Gpr_ESP,  ebp  = Gpr_EBP,  esi  = Gpr_ESI,  edi  = Gpr_EDI,
        r8d  = Gpr_R8D,  r9d  = Gpr_R9D,  r10d = Gpr_R10D, r11d = Gpr_R11D,
        r12d = Gpr_R12D, r13d = Gpr_R13D, r14d = Gpr_R14D, r15d = Gpr_R15D,

        // 16-bit registers
        ax   = Gpr_AX,   cx   = Gpr_CX,   dx   = Gpr_DX,   bx   = Gpr_BX,
        sp   = Gpr_SP,   bp   = Gpr_BP,   si   = Gpr_SI,   di   = Gpr_DI,
        r8w  = Gpr_R8W,  r9w  = Gpr_R9W,  r10w = Gpr_R10W, r11w = Gpr_R11W,
        r12w = Gpr_R12W, r13w = Gpr_R13W, r14w = Gpr_R14W, r15w = Gpr_R15W,

        // 8-bit registers
        al   = Gpr_AL,   cl   = Gpr_CL,   dl  = Gpr_DL,    bl   = Gpr_BL,
        spl  = Gpr_SPL,  bpl  = Gpr_BPL,  sil = Gpr_SIL,   dil  = Gpr_DIL,
        r8l  = Gpr_R8L,  r9l  = Gpr_R9L,  r10l = Gpr_R10L, r11l = Gpr_R11L,
        r12l = Gpr_R12L, r13l = Gpr_R13L, r14l = Gpr_R14L, r15l = Gpr_R15L,

        $tcb    = rbp,
        $rfn    = r12,
        $rfp    = r15,
        $rtcb   = rbp,  // for backward compatibility
        $rn     = rcx,
        $rnil   = r13,
        $sp     = rsp,

        $r0  = rax, $r1 = rdx, $r2  = rbx, $r3  = rsi,
        $r4  = rdi, $r5 = r8,  $r6  = r9,  $r7  = r10,
        $r8  = r11, $r9 = r14,

        $r0d  = eax,  $r1d = edx,  $r2d  = ebx,  $r3d  = esi,
        $r4d  = edi,  $r5d = r8d,  $r6d  = r9d,  $r7d  = r10w,
        $r8d  = r11d, $r9d = r14d,

        $r0w  = ax,   $r1w = dx,   $r2w  = bx,   $r3w  = si,
        $r4w  = di,   $r5w = r8w,  $r6w  = r9w,  $r7w  = r10w,
        $r8w  = r11w, $r9w = r14w,

        $r0b  = al,   $r1b = dl,   $r2b  = bl,   $r3b  = sil,
        $r4b  = dil,  $r5b = r8l,  $r6b  = r9l,  $r7b  = r10l,
        $r8b  = r11l, $r9b = r14l,
    }; // Reg

    public: enum REX
    {
        REX_WRX     = 0x4E,
        REX_WRXB    = 0x4F,
        REX_WRB     = 0x4D,
        REX_WR      = 0x4C,
        REX_WXB     = 0x4B,
        REX_WX      = 0x4A,
        REX_WB      = 0x49,
        REX_W       = 0x48,
        REX_RXB     = 0x47,
        REX_RX      = 0x46,
        REX_RB      = 0x45,
        REX_R       = 0x44,
        REX_X       = 0x42,
        REX_XB      = 0x43,
        REX_B       = 0x41,
        REX         = 0x40,
    }; // REX

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
        rm_sib      = 4,    // we can't use RSP as index.
        rm_disp32   = 5,    // we can't use disp0 with RBP.
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
    class FunObj : public X64::Function
        { friend class X64Assembler; };

    ////////////////////////////////////////////////////////////
    //
    // Context
    //  Layout of thread.
    //
    class Context : public Kernel::Thread
        { friend class X64Assembler; };

    class Area : public Kernel::Area
        { friend class X64Assembler; };

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
        public: int     m_opdsiz;
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

    typedef FunObj::FrameType FrameType;

    public: class Defun
    {
        public: Val     m_classd;
        public: Val     m_name;
        public: uint    m_cbFrame;
        public: int     m_iMin;
        public: int     m_iMax;
        public: Label   m_oArityError;
        public: bool    m_fCheckArity;
        public: FrameType   m_eFrame;

        public: Defun(
            X64Assembler*       pAsm,
            Val                 name,
            int                 iMin,
            int                 iMax,
            uint                cbFrame = 16,
            FrameType           eFrame = FunObj::FrameType_Fixed ) :
                m_classd(CLASSD_native_code_function),
                m_name(name),
                m_cbFrame(cbFrame),  // including return address
                m_eFrame(eFrame),
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
        #define DEFFORMAT_0(mp_opcode, mp_mnemonic) \
            op_##mp_mnemonic = mp_opcode,

        #define DEFFORMAT_1(mp_opcode, mp_mnemonic, mp_1) \
            op_##mp_mnemonic##_##mp_1 = mp_opcode,

        #define DEFFORMAT_EXT_1(mp_opcode, mp_opext, mp_mnemonic, mp_1) \
            op_##mp_mnemonic##_##mp_1 = mp_opcode,

        #define DEFFORMAT_2(mp_opcode, mp_mnemonic, mp_1, mp_2) \
            op_##mp_mnemonic##_##mp_1##_##mp_2 = mp_opcode,

        #define DEFFORMAT_3(mp_opcode, mp_mnemonic, mp_1, mp_2, mp_3) \
            op_##mp_mnemonic##_##mp_1##_##mp_2##_##mp_3 = mp_opcode,

        #define DEFFORMAT_EXT_2(mp_opcode, mp_opext, mp_mnemonic, mp_1, mp_2) \
            op_##mp_mnemonic##_##mp_1##_##mp_2 = mp_opcode,

        #define DEFFORMAT_EXT_3(mp_opcode, mp_opext, mp_mnemonic, \
                                        mp_1, mp_2, mp_3) \
            op_##mp_mnemonic##_##mp_1##_##mp_2##_##mp_3 = mp_opcode,

        #include "../x86/x86_opcode.inc"
    }; // OpCode

    enum OpExt
    {
        #define DEFFORMAT_EXT_1(mp_opcode, mp_opext, mp_mnemonic, mp_1) \
            opext_##mp_mnemonic##_##mp_1 = mp_opext,

        #define DEFFORMAT_EXT_2(mp_opcode, mp_opext, mp_mnemonic, mp_1, mp_2) \
            opext_##mp_mnemonic##_##mp_1##_##mp_2 = mp_opext,

        #define DEFFORMAT_EXT_3(mp_opcode, mp_opext, mp_mnemonic, \
                mp_1, mp_2) \
            opext_##mp_mnemonic##_##mp_1##_##mp_2##_##mp_3 = mp_opext,

        #include "../x86/x86_opcode.inc"
    }; // Op

    public: static uint8 modrm(Mod mod, Reg reg, Reg rm)
        { return static_cast<uint8>(mod | ((reg & 7) << 3) | (rm & 7)); }

    public: static uint8 modrm(Mod mod, OpExt ext, Reg rm)
        { return static_cast<uint8>(mod | (ext << 3) | (rm & 7)); }

    public: static uint8 modrm(Mod mod, Reg reg, Rm rm)
        { return static_cast<uint8>(mod | ((reg & 7) << 3) | (rm & 7)); }

    public: static uint8 modrm(Mod mod, OpExt reg, Rm rm)
        { return static_cast<uint8>(mod | ((reg & 7) << 3) | (rm & 7)); }

    public: static uint8 sib(Scale s, Reg idx, Reg baze)
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
            emit_u64(0);
        }
    } // emit_lit

    public: void emit_op(uint op)
    {
        if (op > 0xFFFF) emit_u8(static_cast<uint8>(op >> 16));
        if (op > 0xFF)   emit_u8(static_cast<uint8>(op >> 8));
        emit_u8(static_cast<uint8>(op));
    } // emit_op

    protected: void emit_op(uint op, uint rd, uint rx)
    {
        uint rex;
        if (op < 0x48000000)
        {
            rex = REX;
        }
        else
        {
            rex = REX_W;
            op &= 0xffffff;
        }

        uint prefix = op >> 16;
        if (prefix == 0x66 || prefix == 0xF2 || prefix == 0xF2)
        {
            emit_u8(static_cast<uint8>(prefix));
            op &= 0xffff;
        }

        if (rd >= rax) rex |= REX_W;
        if (rd & 8)    rex |= REX_R;
        if (rx & 8)    rex |= REX_B;
        if (REX != rex) emit_u8(static_cast<uint8>(rex));

        if (op > 0xFF) emit_u8(static_cast<uint8>(op >> 8));
        emit_u8(static_cast<uint8>(op));
    } // emit_op

    public: void emit_rex(Ea* pEa)
    {
        uint rex = REX;
        if (pEa->m_opdsiz == 64) rex |= REX_W;
        if (pEa->m_base  & 8)    rex |= REX_B;
        if (pEa->m_index & 8)    rex |= REX_X;
        if (REX != rex) emit_u8(static_cast<uint8>(rex));
    } // emit_rex

    public: void emit_rex(Ea* pEa, Reg rx)
    {
        uint rex = REX;
        if (rx >= rax)        rex |= REX_W;
        if (rx & 8)           rex |= REX_R;
        if (pEa->m_base  & 8) rex |= REX_B;
        if (pEa->m_index & 8) rex |= REX_X;
        if (REX != rex) emit_u8(static_cast<uint8>(rex));
    } // emit_rex

    public: void emit_rex(Reg rd)
    {
        uint rex = REX;
        if (rd >= rax) rex |= REX_W;
        if (rd & 8)    rex |= REX_R;
        if (REX != rex) emit_u8(static_cast<uint8>(rex));
    } // emit_rex

    public: void emit_rex(Reg rd, Reg rx)
    {
        uint rex = REX;
        if (rd >= rax) rex |= REX_W;
        if (rd & 8)    rex |= REX_R;
        if (rx & 8)    rex |= REX_B;
        if (REX != rex) emit_u8(static_cast<uint8>(rex));
    } // emit_rex

    void emit_rex2(Reg rb)
    {
        if (rb >= rax )     emit_rex(rax, rb);
        else if (rb >= eax) emit_rex(eax, rb);
        else if (rb >= ax)  emit_rex(ax,  rb);
        else                emit_rex(al,  rb);
    } // emit_rex2

    public: void emit_s8(int8 n)   { emit_u8(static_cast<uint8>(n)); }
    public: void emit_s16(int16 n) { emit_u16(static_cast<uint16>(n)); }
    public: void emit_s32(int32 n) { emit_u32(static_cast<uint32>(n)); }


    public: void emit_u16(uint16 n)
    {
        emit_u8(static_cast<uint8>(n >> 0));
        emit_u8(static_cast<uint8>(n >> 8));
    } // emit_u16

    public: void emit_u32(uint32 n)
    {
        emit_u8(static_cast<uint8>(n >>  0));
        emit_u8(static_cast<uint8>(n >>  8));
        emit_u8(static_cast<uint8>(n >> 16));
        emit_u8(static_cast<uint8>(n >> 24));
    } // emit_u32

    public: void emit_u64(uint64 n)
    {
        emit_u32(static_cast<uint32>(n));
        emit_u32(static_cast<uint32>(n >> 32));
    } // emit_u32

    public: void emit_u8(uint8 n)
    {
        ASSERT(m_ofsCode < lengthof(m_rgbCode));
        m_rgbCode[m_ofsCode++] = n;
        m_nAddr += 1;
    } // emit_u8

    #define EMIT_OP_EA(mp_op) \
        emit_rex(pEa); emit_op(op_##mp_op); emit_ea(pEa, opext_##mp_op);

    #define EMIT_OP_EXT(mp_op, mp_rm) \
        emit_rex(mp_rm); \
        emit_op(op_##mp_op); emit_u8(modrm(mod_reg, opext_##mp_op, mp_rm));

    public: void emit_ea(Ea* pEa, OpExt opext)
        { emit_ea(pEa, static_cast<Reg>(opext)); }

    // emit_ea
    //  0x24 = ss=00 idx=100 base=100 == no index, base=rsp
    public: void emit_ea(Ea* pEa, Reg rx)
    {
        if (NULL != pEa->m_pLabel)
        {
            // [rip+disp32]
            ASSERT(none != pEa->m_base);
            emit_u8(modrm(mod_disp0, rx, rm_disp32));
            emit_u32(0);

            m_rgoJump[m_cJumps].m_pLabel  = pEa->m_pLabel;
            m_rgoJump[m_cJumps].m_nAddr   = m_nAddr;
            m_rgoJump[m_cJumps].m_ofsCode = m_ofsCode;
            m_rgoJump[m_cJumps].m_nOpcode = op_NOP;
            m_cJumps += 1;

            return;
        } // if

        // Note: In x64, we don't support [disp32+index]. You should use
        // [base+idsp32].
        ASSERT(none != pEa->m_base);

        if (none == pEa->m_index)
        {
            // [base+disp]
            if (0 == pEa->m_disp && rbp != pEa->m_base)
            {
                emit_u8(modrm(mod_disp0, rx, pEa->m_base));
                if (rm_sib == static_cast<Rm>(pEa->m_base & 7)) emit_u8(0x24);
            }
            else if (pEa->m_disp >= -128 && pEa->m_disp <= 127)
            {
                emit_u8(modrm(mod_disp8, rx, pEa->m_base));
                if (rm_sib == static_cast<Rm>(pEa->m_base & 7)) emit_u8(0x24);
                emit_s8(static_cast<int8>(pEa->m_disp));
            }
            else
            {
                emit_u8(modrm(mod_disp32, rx, pEa->m_base));
                if (rm_sib == static_cast<Rm>(pEa->m_base & 7)) emit_u8(0x24);
                emit_s32(static_cast<int32>(pEa->m_disp));
            }
        }
        else
        {
            // [base+index+disp]

            // We can't use rsp as index.
            ASSERT(rsp != pEa->m_index);

            if (0 == pEa->m_disp && rbp != pEa->m_base)
            {
                emit_u8(modrm(mod_disp0, rx, rm_sib));
                emit_u8(sib(pEa->m_scale, pEa->m_index, pEa->m_base)); 
            }
            else if (pEa->m_disp >= -128 && pEa->m_disp <= 127)
            {
                emit_u8(modrm(mod_disp8, rx, rm_sib));
                emit_u8(sib(pEa->m_scale, pEa->m_index, pEa->m_base));
                emit_s8(static_cast<int8>(pEa->m_disp));
            }
            else
            {
                emit_u8(modrm(mod_disp32, rx, rm_sib));
                emit_u8(sib(pEa->m_scale, pEa->m_index, pEa->m_base));
                emit_s32(static_cast<int32>(pEa->m_disp));
            }
        } // if
    } // emit_ea

    public: Reg reg_arg(uint i)
        { return static_cast<Reg>(X64::X64Mach::ISA.m_pGprArg->Get(i)); }

    public: uint reg_arg_limit()
        { return X64::X64Mach::ISA.m_pGprArg->m_n; }

    public: Ea* ea(
        Reg     eBase,
        Int     iDisp  = 0,
        Reg     eIndex = none,
        Scale   eScale = scale_1 )
    {
        ASSERT(iDisp >= (-1ll << 31) && iDisp < (1ll << 31));
        m_oEa.m_base   = eBase;
        m_oEa.m_disp   = static_cast<int32>(iDisp);
        m_oEa.m_index  = eIndex;
        m_oEa.m_scale  = eScale;
        m_oEa.m_pLabel = NULL;
        m_oEa.m_opdsiz = 64;
        return &m_oEa;
    } // ea

    public: Ea* byte_ptr_(
        Reg     eBase,
        Int     iDisp = 0,
        Reg     eIndex = none,
        Scale   eScale = scale_1 )
    {
        ASSERT(iDisp >= (-1ll << 31) && iDisp < (1ll << 31));
        m_oEa.m_base   = eBase;
        m_oEa.m_disp   = static_cast<int32>(iDisp);
        m_oEa.m_index  = eIndex;
        m_oEa.m_scale  = eScale;
        m_oEa.m_pLabel = NULL;
        m_oEa.m_opdsiz = 8;
        return &m_oEa;
    } // byte_ptr_

    public: Ea* dword_ptr_(
        Reg     eBase,
        Int     iDisp  = 0,
        Reg     eIndex = none,
        Scale   eScale = scale_1 )
    {
        ASSERT(iDisp >= (-1ll << 31) && iDisp < (1ll << 31));
        m_oEa.m_base   = eBase;
        m_oEa.m_disp   = static_cast<int32>(iDisp);
        m_oEa.m_index  = eIndex;
        m_oEa.m_scale  = eScale;
        m_oEa.m_pLabel = NULL;
        m_oEa.m_opdsiz = 32;
        return &m_oEa;
    } // dword_ptr_

    public: Ea* word_ptr_(
        Reg     eBase,
        Int     iDisp  = 0,
        Reg     eIndex = none,
        Scale   eScale = scale_1 )
    {
        ASSERT(iDisp >= (-1ll << 31) && iDisp < (1ll << 31));
        m_oEa.m_base   = eBase;
        m_oEa.m_disp   = static_cast<int32>(iDisp);
        m_oEa.m_index  = eIndex;
        m_oEa.m_scale  = eScale;
        m_oEa.m_pLabel = NULL;
        m_oEa.m_opdsiz = 16;
        return &m_oEa;
    } // dword_ptr_

    public: Ea* ea(Label&  rLabel)
    {
        m_oEa.m_pLabel = &rLabel;
        m_oEa.m_opdsiz = 64;
        return &m_oEa;
    } // ea

    public: Ea* ea(void* pv)
    {
        m_oEa.m_base   = none;
        m_oEa.m_disp   = static_cast<int32>(reinterpret_cast<Int>(pv));
        m_oEa.m_index  = none;
        m_oEa.m_scale  = scale_none;
        m_oEa.m_pLabel = NULL;
        m_oEa.m_opdsiz = 64;
        return &m_oEa;
    } // ea

    private: Ea* ea(Reg, Reg) { return 0; }

    public: Ea* ea_m_fn() { return ea($rtcb, offsetof(Context, m_fn)); }
    public: Ea* ea_m_fp() { return ea($rtcb, offsetof(Context, m_fp)); }
    public: Ea* ea_m_n()  { return ea($rtcb, offsetof(Context, m_n)); }

    public: Ea* ea_mv_value(Reg index)
    {
        return ea($rtcb, offsetof(Context, mv_value), index);
    } // ea_mv_value

    public: Ea* ea_mv_value(uint n)
    {
        ASSERT(n < X64Mach::Multiple_Values_Limit);
        return ea($rtcb, offsetof(Context, mv_value) + n * sizeof(Val));
    } // ea_mv_value

    public: Ea* ea_mv_count(uint n)
    {
        ASSERT(n < Context::ObjectStat::TypeCode_MAX_1);
        return ea($rtcb,
            offsetof(Context, mv_stat) +
                offsetof(Context::ObjectStat, m_count) );
    } // ea_mv_count

    public: Ea* ea_mv_size(uint n)
    {
        ASSERT(n < Context::ObjectStat::TypeCode_MAX_1);
        int32 ofs = offsetof(Context, mv_stat);
            ofs += n * sizeof(Context::ObjectStat);
            ofs += offsetof(Context::ObjectStat, m_size);
        return ea($rtcb, ofs);
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
    protected: void asm_arith(uint opext, Ea* pEa, Reg rx)
    {
        emit_rex(pEa, rx);
        emit_op(op_ADD_Ev_Gv + opext * 8);
        emit_ea(pEa, rx);
    } // asm_arith

    // asm_arith -- ADD Ev, Iv
    protected: void asm_arith(uint opext, Ea* pEa, int32 imm)
    {
        if (imm >= -128 && imm <= 128)
        {
            // ADD Ev, Ib
            emit_rex(pEa);
            emit_op(0x83);
            emit_ea(pEa, static_cast<Reg>(opext));
            emit_s8(static_cast<int8>(imm));
        }
        else
        {
            // ADD Ev, Iv
            emit_rex(pEa);
            emit_op(0x81);
            emit_ea(pEa, static_cast<Reg>(opext));
            emit_s32(static_cast<int32>(imm));
        }
    } // asm_arith

    // asm_arith -- ADD Gv, Ea
    protected: void asm_arith(uint opext, Reg rd, Ea* pEa)
    {
        emit_rex(pEa, rd);
        emit_op(op_ADD_Gv_Ev + opext * 8);
        emit_ea(pEa, rd);
    } // asm_arith

    // asm_arith -- ADD Ev, Iv
    protected: void asm_arith(uint opext, Reg rd, int32 imm)
    {
        if (imm >= -128 && imm <= 128)
        {
            // ADD Ev, Ib
            emit_rex2(rd);
            emit_op(0x83);
            emit_u8(modrm(mod_reg, static_cast<Reg>(opext), rd));
            emit_s8(static_cast<int8>(imm));
        }
        else if (rax == rd)
        {
            // ADD eAX, Iv
            emit_u8(REX_W);
            emit_op(op_ADD_eAX_Iz + opext * 8);
            emit_s32(static_cast<int32>(imm));
        }
        else
        {
            // ADD Ev, Iv
            emit_rex2(rd);
            emit_op(0x81);
            emit_u8(modrm(mod_reg, static_cast<Reg>(opext), rd));
            emit_s32(static_cast<int32>(imm));
        }
    } // asm_arith

    // asm_arith -- ADD Ev, Iv
    protected: void asm_arith(uint opext, Reg rd, Val val)
    {
        ASSERT(fixnump(val));
        ASSERT(is_32bit(Fixnum::Decode_(val)));

        if (fixnump(val) &&
            Fixnum::Decode_(val) >= -128 &&
            Fixnum::Decode_(val) <= 127 )
        {
            asm_arith(opext, rd, static_cast<int32>(val->ToInt()));
        }
        else if (rax == rd)
        {
            // ADD eAX, Iz
            emit_u8(REX_W);
            emit_op(op_ADD_eAX_Iz + opext * 8);
            emit_s32(static_cast<int32>(val->ToInt()));
        }
        else
        {
            emit_rex2(rd);
            emit_op(op_ADD_Ev_Iz);
            emit_u8(modrm(mod_reg, static_cast<Reg>(opext), rd));
            emit_s32(static_cast<int32>(val->ToInt()));
        }
    } // asm_arith

    // asm_arith -- ADD Gv, Ev
    protected: void asm_arith(uint opext, Reg rd, Reg rx)
    {
        emit_rex(rd, rx);
        emit_op(op_ADD_Gv_Ev + opext * 8);
        emit_u8(modrm(mod_reg, rd, rx));
    } // asm_arith

    // asm_cmov
    protected: void asm_cmov(uint opcode, Reg rd, Reg rx)
    {
        emit_rex(rd, rx);
        emit_op(opcode);
        emit_u8(modrm(mod_reg, rd, rx));
    } // asm_cmov

    // asm_Ev - for DIV, IDIV, MUL, IMUL
    protected: void asm_Ev(uint opcode, OpExt opext, Reg rx)
    {
        emit_rex(rx);
        emit_op(opcode);
        emit_u8(modrm(mod_reg, opext, rx));
    } // asm_Ev

    // asm_Ev - for CALL and JMP
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
        emit_rex(pEa);
        emit_op(opcode);
        emit_ea(pEa, opext);
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
        { EMIT_OP_EA(CMP_Ev_Iz); ASSERT(is_32bit(val)); emit_lit(val); }

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

    ////////////////////////////////////////////////////////////
    //
    // JMP
    public: void jmp(Label& x)  { asm_Jb(op_JMP_Jb, x); }
    public: void jmp(Ea* pEa)   { asm_Jv(op_JMP_Ev, opext_JMP_Ev, pEa); }
    public: void jmp(Val name)  { asm_Jv(op_JMP_Jv, name); }

    public: void jmp(Reg rx)
    {
        emit_op(op_JMP_Ev, opext_JMP_Ev, rx);
        emit_u8(modrm(mod_reg, opext_JMP_Ev, rx));
    } // jmp

    ////////////////////////////////////////////////////////////
    //
    // MOV
    //
    public: void mov(Ea* pEa, Reg rx)
    {
        uint opcode = op_MOV_Ev_Gv;
        switch (pEa->m_opdsiz)
        {
        case 8:  opcode = op_MOV_Eb_Gb; break;
        case 16: emit_u8(op_OPDSIZ); break;
        case 32: break;
        case 64: break;
        } // switch opdsiz
        emit_rex(pEa, rx); emit_op(op_MOV_Ev_Gv); emit_ea(pEa, rx);
    } // mov

    public: void mov(Ea* pEa, Val val)
        { EMIT_OP_EA(MOV_Ev_Iz); emit_lit(val); }

    public: void mov(Ea* pEa, int32 imm32)
        { EMIT_OP_EA(MOV_Ev_Iz); emit_s32(imm32); }

    public: void mov(Reg rd, Ea* pEa)
        { emit_rex(pEa, rd); emit_op(op_MOV_Gv_Ev); emit_ea(pEa, rd); }

    public: void mov(Reg rd, int64 imm)
    {
        if (is_32bit(imm))
        {
            EMIT_OP_EXT(MOV_Ev_Iz, rd);
            emit_s32(static_cast<int32>(imm));
        }
        else
        {
            emit_rex(rax, rd);
            emit_op(op_MOV_eAX_Iv + (rd & 7));
            emit_u64(imm);
        }
    } // mov

    public: void mov(Reg rd, Reg rx)
    {
        if (rd != rx)
        {
            emit_rex(rd, rx);
            emit_op(op_MOV_Gv_Ev); emit_u8(modrm(mod_reg, rd, rx));
        }
    } // mov

    public: void mov(Reg rd, Val val)
    {
        if (is_32bit(val))
        {
            EMIT_OP_EXT(MOV_Ev_Iz, rd);
            emit_s32(static_cast<int32>(val->ToInt()));
        }
        else
        {
            emit_rex(rax, rd);
            emit_op(op_MOV_eAX_Iv + (rd & 7));
            emit_lit(val);
        }
    } // mov

    public: void movsxd(Reg rd, Ea* pEa)
    {
        emit_rex(pEa, rd);
        emit_op(op_MOVSXD_Gv_Ev);
        emit_ea(pEa, rd);
    } // movsxd

    public: void movzx(Reg rd, Ea* pEa)
    {
        uint rex = REX;
        uint opcode = op_MOVZX_Gv_Ew;
        switch (pEa->m_opdsiz)
        {
        case 8: rex = rd >= rax ? REX_W : REX; opcode = op_MOVZX_Gv_Eb; break;
        case 16: break;
        case 32: rex = REX_W; break;
        default:
            CAN_NOT_HAPPEN();
        } // switch
        if (rd >= r8)            rex |= REX_R;
        if (pEa->m_base  >= r8)  rex |= REX_B;
        if (pEa->m_index >= r8)  rex |= REX_X;
        if (REX != rex) emit_u8(static_cast<uint8>(rex));
        emit_op(opcode);
        emit_ea(pEa, rd);
    } // movzx

    public: void lea(Reg rd, Ea* pEa)
        { emit_rex(pEa, rd); emit_op(op_LEA_Gv_M); emit_ea(pEa, rd); }

    public: void pop(Reg rx)
    {
        if (rx & 8) emit_u8(REX_WB);
        emit_op(op_POP_rAX + (rx & 7));
    } // pop

    public: void push(Reg rx)
    {
        if (rx & 8) emit_u8(REX_WB);
        emit_op(op_PUSH_rAX + (rx & 7));
    } // push

    public: void push(int32 imm) { emit_op(op_PUSH_Iz); emit_s32(imm); }

    public: void pop(Ea*  pEa) { EMIT_OP_EA(POP_Ev); }
    public: void push(Ea* pEa) { EMIT_OP_EA(PUSH_Ev); }

    public: void ret() { emit_op(op_RET); }

    public: void shl(Reg rd, int n)
    {
        ASSERT(n >= 0 && n < sizeof(Val) * 8);

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
        ASSERT(n >= 0 && n < sizeof(Val) * 8);

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

    public: void test(Ea* pEa, Reg rx)
    {
        emit_rex(pEa, rx);
        emit_op(op_TEST_Ev_Gv);
        emit_ea(pEa, rx);
    } // test

    public: void test(Reg rx, Reg ry)
    {
        emit_rex(rx, ry);
        emit_op(rx <= Gpr_R15L ? op_TEST_Eb_Gb : op_TEST_Ev_Gv);
        emit_u8(modrm(mod_reg, rx, ry));
    } // test

    public: void test(Reg rd, int32 imm)
    {
        emit_rex2(rd);
        emit_op(op_TEST_Ev_Iz);
        emit_u8(modrm(mod_reg, opext_TEST_Ev_Iz, rd));
        emit_s32(imm);
    } // test

    public: void test(Ea* pEa, int32 imm)
        { EMIT_OP_EA(TEST_Ev_Iz); emit_s32(imm); }

    ////////////////////////////////////////////////////////////
    //
    // SSE
    //
    public: void emit_ea(Ea* pEa, XmmReg xmm)
        { emit_ea(pEa, static_cast<Reg>(xmm)); }

    protected: void emit_rex(Reg rd, XmmReg xmm)
        { emit_rex(rd, static_cast<Reg>(xmm)); }

    protected: void emit_rex(XmmReg xmm, Reg rx)
        { emit_rex(static_cast<Reg>(xmm), rx); }

    protected: void emit_xmm(uint opcode, XmmReg xmm1, Reg r2)
        { emit_op(opcode); emit_u8(modrm(mod_reg, xmm1, r2)); }

    protected: void emit_xmm(uint opcode, XmmReg xmm1, XmmReg xmm2)
        { emit_op(opcode); emit_u8(modrm(mod_reg, xmm1, xmm2)); }

    protected: void emit_xmm(uint opcode, XmmReg xmm1, Ea* pEa)
        { emit_op(opcode); emit_ea(pEa, xmm1); }

    public: uint8 modrm(Mod mod, Reg r1, XmmReg xmm2)
        { return modrm(mod, r1, static_cast<Reg>(xmm2)); }

    public: uint8 modrm(Mod mod, XmmReg xmm1, Reg r2)
        { return modrm(mod, static_cast<Reg>(xmm1), r2); }

    public: uint8 modrm(Mod mod, XmmReg xmm1, XmmReg xmm2)
        { return static_cast<uint8>(mod | (xmm1 << 3) | xmm2); }

    public: uint8 modrm(Mod mod, OpExt opext, XmmReg xmm2)
        { return static_cast<uint8>(mod | (opext << 3) | xmm2); }

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
        public: void cmp##mp_name(XmmReg xmm, Ea* m64, CmpPx ePred) \
        { \
            emit_op(op_CMP##mp_NAME##_V##mp_name##_W##mp_name##_Ib); \
            emit_ea(m64, xmm); \
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

    public: void movd(Reg r32, XmmReg xmm)
    {
        emit_op(op_MOVD_Ed_Vdq, r32, xmm);
        emit_u8(modrm(mod_reg, r32, xmm));
    } // movd

    public: void movd(XmmReg xmm, Reg r32)
    {
        emit_op(op_MOVD_Vdq_Ed, xmm, r32);
        emit_u8(modrm(mod_reg, xmm, r32));
    } // movd

    public: void movq(Reg r64, XmmReg xmm)
    {
        emit_op(op_MOVQ_Eq_Vdq, r64, xmm);
        emit_u8(modrm(mod_reg, r64, xmm));
    } // movq

    public: void movq(XmmReg xmm, Reg r64)
    {
        emit_op(op_MOVQ_Vdq_Eq, xmm, r64);
        emit_u8(modrm(mod_reg, xmm, r64));
    } // movq

    public: void movsd(Ea* m64, XmmReg xmm)
        { emit_op(op_MOVSD_Wsd_Vsd); emit_ea(m64, xmm); }

    public: void movss(Ea* m32, XmmReg xmm)
        { emit_op(op_MOVSS_Wss_Vss); emit_ea(m32, xmm); }

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

    public: void dq(Label& x)
    {
        add_annon(FunObj::Annon::Type_Label, Fixnum::Encode(&x));
        emit_u64(0);
    } // dd

    public: void set_frame_type(
        Defun* pDefun, FunObj::FrameType eFrame, uint cbFrame )
    {
        pDefun->m_cbFrame = cbFrame;
        pDefun->m_eFrame  = eFrame;
    } // frame_type_

    ////////////////////////////////////////////////////////////
    //
    // Prologue/Epilogue
    //
    protected: void emit_prologue(Defun*);
    protected: void emit_epilogue(Defun*);
}; // Assembler

#define defun(name, min, max) \
    defun_(parse_symbol(L##name), min, max, Fixed, 16)

#define defun_(name, min, max, type, cb) \
    { Defun oDefun(this, name, min, max, cb, FunObj::FrameType_##type); \
        emit_prologue(&oDefun);

#define end_defun() \
    emit_epilogue(&oDefun); installFunObj(&oDefun); }


#define defun_setf(name, min, max) \
    defun_(list(Qsetf, parse_symbol(name)), min, max, Fixed, 16)

#define label_def(mp_name) Label mp_name; label(mp_name)

#define frame_type(mp_type, mp_size) \
    set_frame_type(&oDefun, FunObj::FrameType_##mp_type, (mp_size)+8)

#define OffsetOf(mp_class, mp_slot) \
    ( offsetof(mp_class, mp_slot) - mp_class::Tag )


#define PUSH_TRANSITION_FRAME(mp_ty, mp_1) \
{ \
    mov(mp_1, ea_m_fp()); \
    mov(ea($sp, offsetof(Local, m_oFrame.m_pOuter)), mp_1); \
    ; \
    mov(mp_1, Kernel::Frame::Type_##mp_ty); \
    mov(ea($sp, offsetof(Local, m_oFrame.m_type)), mp_1); \
    ; \
    mov(mp_1, sizeof(reinterpret_cast<Local*>(0)->m_home)); \
    mov(ea($sp, offsetof(Local, m_oFrame.m_cbArgs)), mp_1); \
    ; \
    lea(mp_1, ea($sp, offsetof(Local, m_oFrame))); \
    mov(ea_m_fp(), mp_1); \
} // PUSH_TRANSTION_FRAME


#define POP_TRANSITION_FRAME(mp_1) \
{ \
    mov(mp_1, ea($sp, offsetof(Local, m_oFrame.m_pOuter))); \
    mov(ea_m_fp(), mp_1); \
} // POP_TRANSITION_FRAME

} // X64

#endif //!defined(INCLUDE_arch_x64_x64_asm_h)
