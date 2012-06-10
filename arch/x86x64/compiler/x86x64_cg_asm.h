//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - assembler
// arch/x86x64/compiler/x86x64_cg_asm.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_asm.h#30 $
//
#if !defined(INCLUDE_x86x64_compiler_cg_asm_h)
#define INCLUDE_x86x64_compiler_cg_asm_h

#include "./x86x64_cg_defs.h"

#include "../../../compiler/cg/cg_asm_cics.h"
#include "../../../compiler/cm/cm_session.h"
#include "../kernel/x86x64_ke_mach.h"
#include "../x86x64_opcode.h"
#include "./x86x64_cg_gcmap.h"

namespace MiniLisp
{
    Val allocate_funobj(Val, uint, uint, uint, FunObj::FrameType, uint);
} // MiniLisp

namespace Compiler
{

const Opcode op_MOV_Ew_Gw = static_cast<Opcode>(0x6689);
const Opcode op_MOV_Ew_Iw = static_cast<Opcode>(0x66C7);
const Opcode op_MOV_Gw_Ew = static_cast<Opcode>(0x668B);

const OpExt  opext_MOV_Ew_Iw = static_cast<OpExt>(opext_MOV_Ev_Iz);

//////////////////////////////////////////////////////////////////////
//
// Assembler
//
class X86X64CgAsm : public CgCicsAsmPass
{
    Register* m_pRegSP;
    Register* m_pRegTCB;

    public: X86X64CgAsm(const char16* pwsz) :
        m_pRegSP(NULL),
        m_pRegTCB(NULL),
        CgCicsAsmPass(pwsz) {}

    ////////////////////////////////////////////////////////////
    //
    // Ss
    //       76 543 210
    //      +--+---+---+
    //      |S |idx|bas|
    //      +--+---+---+
    protected: uint8 Ss(Scale eScale, Reg ri, Reg rb)
        { return static_cast<uint8>(eScale | ((ri&7) << 3) | (rb&7)); }

    ////////////////////////////////////////////////////////////
    //
    // Effective Address
    //  Note: We don't use scaled index for perfomance.
    //
    protected: enum AddrMode
    {
        AddrMode_BaseDisp,
        AddrMode_BaseIndex,
        AddrMode_ClosedLit,
        AddrMode_ClosedVar,
        AddrMode_LiteralCell,
        AddrMode_SymRef,
        AddrMode_StackCell,
        AddrMode_TlvOffset,
        AddrMode_Error,
    }; // AddrMode

    protected: struct Addr
    {
        Register*   m_pRx;
        Register*   m_pRy;
        int32       m_ofs;
        AnnonType   m_eAnnon;
        Operand*    m_pAnnon;
        AddrMode    m_eMode;

        Addr() : m_pRy(NULL), m_eMode(AddrMode_Error) {}
    }; // Addr

    protected: AddrMode compute_addr(Instruction*, Addr*);
    protected: AddrMode compute_lea_addr(Instruction*, Addr*);
    protected: AddrMode compute_slot_addr(Instruction*, Addr*);

    ////////////////////////////////////////////////////////////
    //
    // cast_to_reg
    //
    protected: static Reg cast_to_reg(Register* pRx)
    {
        Int iRx = pRx->GetLocation();
            ASSERT(pRx->IsPhysical());
        return static_cast<Reg>(iRx);
    } // cast_to_reg

    protected: static Reg cast_to_reg(OpExt ext)
    {
        ASSERT(ext <= 7);
        return static_cast<Reg>(ext);
    } // cast_to_reg

    // compute size
    protected: uint compute_annon_size(Function*) const;

    ////////////////////////////////////////////////////////////
    //
    // emit_disp
    //
    protected: void emit_disp(Reg, Reg, Int);

    protected: void emit_disp(OpExt ext, Reg r, Int i)
        { emit_disp(cast_to_reg(ext), r, i); }

    protected: void emit_disp(OpExt ext, Register* pRx, Int i)
        { emit_disp(cast_to_reg(ext), pRx, i); }

    protected: void emit_disp(Reg eRd, Register* pRx, Int i)
        { emit_disp(eRd, cast_to_reg(pRx), i); }

    protected: void emit_disp(Register* pRd, Reg r, Int i)
        { emit_disp(cast_to_reg(pRd), r, i); }

    protected: void emit_disp(Register* pRd, Register* pRx, Int i)
        { emit_disp(cast_to_reg(pRd), cast_to_reg(pRx), i); }

#if 0
    protected: void emit_disp(Opext opext, const Addr* pAddr)
        { emit_disp(cast_to_reg(opext), pAddr); }

    protected: void emit_disp(Reg rd, const Addr* pAddr)
        { emit_disp(rd, cast_to_reg(pAddr->m_pRx), pAddr->m_ofs); }

    protected: void emit_disp(Register* pRd, const Addr* pAddr)
        { emit_disp(cast_to_reg(pRd), pAddr); }
#endif

    ////////////////////////////////////////////////////////////
    //
    // emit_ea
    //
    protected: void emit_ea(Reg, Register*);

    protected: void emit_ea(OpExt ext, Register* p)
        { emit_ea(cast_to_reg(ext), p); }

    protected: void emit_ea(Register* pRd, Register* pRx)
        { emit_ea(cast_to_reg(pRd), pRx); }

    protected: void emit_ea(Reg, const Addr*);

    protected: void emit_ea(OpExt opext, const Addr* pAddr)
        { emit_ea(cast_to_reg(opext), pAddr); }

    protected: void emit_ea(Register* pRx, const Addr* pAddr)
        { emit_ea(cast_to_reg(pRx), pAddr); }

    ////////////////////////////////////////////////////////////
    //
    // emit_index
    //
    protected: void emit_index(Reg, Reg, Reg, Int);

#if 0
    protected: void emit_index(OpExt opext, const Addr* pAddr)
        { emit_index(cast_to_reg(opext), pAddr); }

    protected: void emit_index(Register* pRd, const Addr* pAddr)
        { emit_index(cast_to_reg(pRd), pAddr); }

    protected: void emit_index(Reg rd, const Addr* pAddr)
    {
        emit_index(
            rd,
            cast_to_reg(pAddr->m_pRx),
            cast_to_reg(pAddr->m_pRy),
            pAddr->m_ofs );
    } // emit_index
#endif

    ////////////////////////////////////////////////////////////
    //
    // emit_modrm
    //
    protected: void emit_modrm(Mod m, Register* reg, Reg rm)
        { emit_u8(ModRm(m, cast_to_reg(reg), rm)); }

    protected: void emit_modrm(Mod m, Register* reg, Register* rm)
        { emit_u8(ModRm(m, cast_to_reg(reg), cast_to_reg(rm))); }

    protected: void emit_modrm(Mod m, Register* reg, Rm rm)
        { emit_u8(ModRm(m, cast_to_reg(reg), rm)); }

    protected: void emit_modrm(Mod m, Reg r1, Reg eBase)
        { emit_u8(ModRm(m, r1, eBase)); }

    protected: void emit_modrm(Mod m, Reg r1, Rm rm)
        { emit_u8(ModRm(m, r1, rm)); }

    protected: void emit_modrm(Mod m, OpExt e, Register* pBase)
        { emit_u8(ModRm(m, e, cast_to_reg(pBase))); }

    protected: void emit_modrm(Mod m, OpExt e, Reg r2)
        { emit_u8(ModRm(m, e, r2)); }

    protected: void emit_modrm(Mod m, OpExt e, Rm rm)
        { emit_u8(ModRm(m, e, rm)); }

    protected: void emit_sib(Scale s, Reg i, Reg b)
        { emit_u8(static_cast<uint8>(s | ((i & 7) << 3) | (b & 7))); }

    // emit_op
    protected: void emit_op(uint);

    // get_tttn
    protected: static Tttn get_tttn(Bool*);

    // Determin we need to set NVALS and CF.
    protected: class CheckValues
    {
        public: enum Flag
        {
            Flag_nvals  = 1 << 0,
            Flag_nil0   = 1 << 1,
            Flag_ret    = 1 << 2,
            Flag_use    = 1 << 3,
        }; // Flag

        public: uint m_nFlags;
        public: CheckValues(Instruction*);
        public: bool IsUsed()    const { return 0 != m_nFlags; }
        public: uint ForRet()    const { return m_nFlags & Flag_ret; }
        public: uint NeedNVals() const { return m_nFlags & Flag_nvals; }
        public: uint NeedNil()   const { return m_nFlags & Flag_nil0; }
    }; // CheckValues

    ////////////////////////////////////////////////////////////
    //
    // Checkers
    //
    protected: Register* get_physical(Operand* pSx)
        { return get_physical(pSx->DynamicCast<Register>()); }

    protected: Register* get_physical(Register*);

    ////////////////////////////////////////////////////////////
    //
    // Error reporting functions
    //
    protected: void asm_error(const char16*, Val=nil, Val=nil);
    protected: void asm_error_2op();
    protected: void asm_error_phy(Operand*);
    protected: void asm_error_vreg(Register*);
    protected: void asm_invalid_operand(Operand*);
    protected: void asm_unsupported(Val);
    protected: void asm_unsupported(Val, Val);
}; // X86X64CgAsm


//////////////////////////////////////////////////////////////////////
//
// X8X64AsmPass
//
template<class Target_, class Assembler_>
class X86X64AsmPass : public Assembler_
{
    protected: X86X64AsmPass(const char16* pwsz) :
        Assembler_(pwsz) {}

    //////////////////////////////////////////////////////////////////////
    //
    // emit_copies
    //
    // Note: We use Register::GetFlag as m_fUsedByAnother.
    //
    void emit_copies(AsmCopyTaskList* pTemps)
    {
        AsmCopyTaskList oPendings;
        AsmCopyTaskList oReadies;

        foreach (AsmCopyTaskList::Enum, oEnum, pTemps)
        {
            AsmCopyTask* pTask = oEnum.Get();
            pTask->m_pRx->m_nUse += 1;
        } // for each task

        // Pass 2: Set up list work list of initial copies
        {
            while (! pTemps->IsEmpty())
            {
                AsmCopyTask* pTask = pTemps->Pop();
                if (0 == pTask->m_pRd->m_nUse)
                {
                    oReadies.Push(pTask);
                }
                else
                {
                    oPendings.Push(pTask);
                }
            } // while
        } // Pass 2


        // Pass 3: Iterate over the worklist, inserting copies
        {
            AsmCopyTaskList* pReadies  = &oReadies;
            AsmCopyTaskList* pPendings = &oPendings;

            for (;;)
            {
                while (! pReadies->IsEmpty())
                {
                    AsmCopyTask* pTask = pReadies->Pop();
                     AsmCopyReg* pRd = pTask->m_pRd;
                     AsmCopyReg* pRx = pTask->m_pRx;

                    asm_copy(
                        static_cast<Reg>(pRd->m_iReg), 
                        static_cast<Reg>(pRx->m_iReg) );

                    pRx->m_nUse -= 1;

                    ASSERT(pTemps->IsEmpty());
                    while (! pPendings->IsEmpty())
                    {
                        AsmCopyTask* pTask = pPendings->Pop();
                        if (0 == pTask->m_pRd->m_nUse)
                        {
                            pReadies->Push(pTask);
                        }
                        else
                        {
                            pTemps->Push(pTask);
                        }
                    } // while

                    {
                        AsmCopyTaskList* p = pPendings;
                        pPendings = pTemps;
                        pTemps = p;
                    }
                } // while ready

                ASSERT(pTemps->IsEmpty());
                if (pPendings->IsEmpty())
                {
                    // All tasks are finished or no more pending tasks.
                    break;
                }

                // Free %rd since %rd is source of other tasks.
                {
                    AsmCopyTask* pTask = pPendings->Pop();

                     AsmCopyReg* pRd = pTask->m_pRd;
                        ASSERT(pRd->m_nUse >= 1);

                     AsmCopyReg* pRx = pTask->m_pRx;
                        ASSERT(pRx->m_nUse >= 1);

                    asm_XCHG(
                        static_cast<Reg>(pRd->m_iReg), 
                        static_cast<Reg>(pRx->m_iReg) );

                    foreach (AsmCopyTaskList::Enum, oEnum, pPendings)
                    {
                        AsmCopyTask* pPending = oEnum.Get();
                        if (pPending->m_pRx == pRd)
                        {
                            pPending->m_pRx = pRx;
                            pRd->m_nUse -= 1;
                            pRx->m_nUse += 1;
                        } // if
                    } // for each pending

                    ASSERT(pTemps->IsEmpty());
                    while (! pPendings->IsEmpty())
                    {
                        AsmCopyTask* pPending = pPendings->Pop();

                        if (pPending->m_pRd == pPending->m_pRx)
                        {
                            // ignore
                        }
                        else if (0 == pPending->m_pRd->m_nUse)
                        {
                            pReadies->Push(pPending);
                        }
                        else
                        {
                            pTemps->Push(pPending);
                        }
                    } // while

                    {
                        AsmCopyTaskList* p = pPendings;
                        pPendings = pTemps;
                        pTemps = p;
                    }
                } // free %rd
            } // for
        } // Pass 3
    } // emit_copies

    //////////////////////////////////////////////////////////////////////
    //
    // asm_copy
    //
    //      MOV reg, [ESP+n]
    //      MOV [ESP+n], reg
    //      MOV reg, imm
    //      MOV [ESP+n], imm
    //
    protected: void asm_copy(Reg eRd, Operand* pSx)
    {
        if (pSx->Is<Register>())
        {
            asm_load(eRd, pSx->StaticCast<Register>());
        }
        else
        {
            asm_MOV(eRd, pSx);
        } // if
    } // asm_copy

    // asm_copy
    protected: void asm_copy(Reg rd, Reg rx)
        { if (rd != rx) asm_MOV(rd, rx); }

    // asm_copy
    protected: void asm_copy(Register* pRd, Operand* pSx)
    {
        switch (pRd->GetStorage())
        {
        case Register::Storage_Physical:
            asm_copy(cast_to_reg(pRd), pSx);
            break;

        case Register::Storage_Stack:
            if (pSx->Is<Register>())
            {
                Register* pRx = pSx->StaticCast<Register>();
                    ASSERT(pRx->IsPhysical());

                asm_copy(pRd, cast_to_reg(pRx));
            }
            else
            {
                asm_MOV($sp, pRd->GetLocation(), pSx);
            } // if
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch storage
    } // asm_copy

    //////////////////////////////////////////////////////////////////////
    //
    // asm_copy
    //
    //      o MOV reg, reg
    //      o MOV [ESP+disp], reg
    //
    protected: void asm_copy(Register* pRd, Reg eRx)
    {
        switch (pRd->GetStorage())
        {
        case Register::Storage_Physical:
            asm_copy(cast_to_reg(pRd), eRx);
            break;

        case Register::Storage_Stack:
            asm_MOV($sp, pRd->GetLocation(), eRx);
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch storage
    } // asm_copy

    //////////////////////////////////////////////////////////////////////
    //
    // asm_load - Emit load register operand into physical register
    //      MOV reg, reg
    //      or
    //      MOV reg, [ESP+disp]
    protected: void asm_load(Reg rd, Register* pRx)
    {
        ASSERT(NULL != pRx);

        switch (pRx->GetStorage())
        {
        case Register::Storage_Closed:
            asm_MOV(rd, pRx);
            break;

        case Register::Storage_LoadTimeValue:
            asm_MOV(rd, pRx->GetDfn()->GetSx());
            break;

        case Register::Storage_Physical:
            asm_copy(rd, cast_to_reg(pRx));
            break;

        case Register::Storage_Stack:
            asm_MOV(rd, $sp, pRx->GetLocation());
            break;

        default:
            asm_error_phy(pRx);
            break;
        } // storage class
    } // asm_load

    protected: void asm_load(uint rd, Register* pRx)
        { asm_load(static_cast<Reg>(rd), pRx); }

    // asm_store - Emit store with base+disp
    protected: void asm_store(Reg rb, int disp32, Operand* pSy)
    {
        if (pSy->Is<Register>())
        {
            Register* pRy = pSy->StaticCast<Register>();
            asm_MOV(rb, disp32, cast_to_reg(pRy));
        }
        else
        {
            asm_MOV(rb, disp32, pSy);
        }
    } // asm_store

    protected: void asm_store(Register* pRb, int32 disp32, Operand* pSy)
        { asm_store(cast_to_reg(pRb), disp32, pSy); }

    ////////////////////////////////////////////////////////////
    //
    // Frame
    //

    //////////////////////////////////////////////////////////////////////
    //
    // emit_activate_frame
    //
    //  [1] ofsFrame = 0
    //      mov [ebp] thread.m_fp <= esp
    //  [2] ofsFrame != 0
    //      lea eax <= [esp] ofsFrame
    //      mov [ebp] thread.m_fp <= eax
    //
    void emit_activate_frame(Reg rx, size_t ofsFrame)
    {
        // Activate bind frame
        if (0 == ofsFrame)
        {
            asm_MOV($rtcb, offsetof(Kernel::Thread, m_fp), $sp);
        }
        else
        {
            asm_LEA(rx, $sp, static_cast<int32>(ofsFrame));
            asm_MOV($rtcb, offsetof(Kernel::Thread, m_fp), rx);
        } // if
    } // emit_activate_frame


    // emit_open_frame
    int32 emit_open_frame(Frame* pFrame, Reg rx, uint nType)
    {
        int32 ofsFrame = pFrame->GetLocation();

        asm_MOV(rx, $rtcb, offsetof(Thread, m_fp));
        asm_MOV($sp, ofsFrame + offsetof(Kernel::Frame, m_pOuter), rx);
        asm_MOV($sp, ofsFrame + offsetof(Kernel::Frame, m_type), nType);

        return ofsFrame;
    } // emit_open_frame

    ////////////////////////////////////////////////////////////
    //
    // make_function
    //
    // Called by:
    //  CgBaseAsmPass::process_function
    //
    protected: Val make_function(Function* pFun)
    {
        fix_spans(pFun);

        X86X64GcMapPass_<Target_> oGcMapPass(this);

        uint cbCodeVec = m_oContext.GetAddress();
        uint cbGcMap   = oGcMapPass.Compute(pFun);
        uint cbAnnon   = compute_annon_size(pFun);

        Val classd = pFun->IsClosure() ?
            CLASSD_native_code_closure :
            CLASSD_native_code_function;

        Val funobj = allocate_funobj(
            classd,
            cbCodeVec,
            cbAnnon,
            cbGcMap,
            pFun->IsStackRestify() ?
                    FunObj::FrameType_Restify :
                    FunObj::FrameType_Fixed,
            m_oContext.GetFunction()->GetFrameSize() );

        FunObj* pFunObj = funobj->Decode<FunObj>();

        //format(t, L"; Function ~S~%", pFun->GetName());
        //format(t, L";  size = ~D byte~%", Fixnum::Encode(cbFunObj));

        pFunObj->m_name =
            val_anonymous == pFun->GetName() ? nil : pFun->GetName();

        // Populate code vector
        copy_codevec(pFunObj->GetCodeVec());

        {
            uint8* pbCodeVec = pFunObj->GetCodeVec();
            uint nAddr = m_oContext.GetAddress();
            while (0 != nAddr % 4)
            {
                pbCodeVec[nAddr] = op_NOP;
                nAddr += 1;
            }
        }

        // Populate GC Map
        oGcMapPass.Serialize(pFunObj->GetGcMap());

        return funobj;
    } // make_function

    // relize_annon
    FunObj::Annon realize_annon(const AsmAnnon* pAnnon, Val fun)
    {
        UINT nType = pAnnon->GetType();

        switch (nType)
        {
        case Annon_ClosedLit:
        case Annon_ClosedVar:
            fun->Decode<FunObj>()->PatchVal(
                pAnnon->GetAddress(),
                pAnnon->GetOperand()->StaticCast<Literal>()->GetDatum() );
            break;

        case Annon_ExitPoint:
            fun->Decode<FunObj>()->PatchExitPoint(
                pAnnon->GetAddress(),
                Fixnum::Encode(pAnnon->GetOperand()->StaticCast<Label>()->
                    GetBBlock()->GetExtension<AsmBBlock>()->GetAddress() ) );
            break;

        case Annon_Function:
            nType = Annon::Type_LispVal;
            fun->Decode<FunObj>()->PatchVal(
                pAnnon->GetAddress(),
                pAnnon->GetOperand()->StaticCast<Function>()->GetFunObj() );
            break;

        case Annon_LispVal:
            fun->Decode<FunObj>()->PatchVal(
                pAnnon->GetAddress(),
                pAnnon->GetOperand()->StaticCast<Literal>()->GetDatum() );
            break;

        case Annon_LocalCallee:
            fun->Decode<FunObj>()->PatchCallee(
                pAnnon->GetAddress(),
                pAnnon->GetOperand()->StaticCast<Function>()->GetFunObj() );
            break;

        case Annon_NamedCallee:
            fun->Decode<FunObj>()->PatchCallee(
                pAnnon->GetAddress(),
                register_caller(
                    pAnnon->GetOperand()->StaticCast<Literal>()->GetDatum(),
                    fun ) );
            break;

        case Annon_SymFun:
        {
            Val symb = pAnnon->GetOperand()->StaticCast<Literal>()->GetDatum();
            Symbol* pSymb = symb->Decode<Symbol>();
            fun->Decode<FunObj>()->PatchUn(
                pAnnon->GetAddress(),
                static_cast<UInt>(reinterpret_cast<Int>(&pSymb->m_function)) );
            break;
        } // Type_SymFun

        case Annon_SymSetf:
        {
            Val cell = pAnnon->GetOperand()->StaticCast<Literal>()->GetDatum();
            SetfCell* pCell = cell->Decode<SetfCell>();
            fun->Decode<FunObj>()->PatchUn(
                pAnnon->GetAddress(),
                static_cast<UInt>(reinterpret_cast<Int>(&pCell->m_function)) );
            break;
        } // Type_SymSetf

        case Annon_SymVal:
        {
            Val cell = pAnnon->GetOperand()->StaticCast<Literal>()->GetDatum();
            ValueCell* pCell = cell->Decode<ValueCell>();
            fun->Decode<FunObj>()->PatchUn(
                pAnnon->GetAddress(),
                static_cast<UInt>(reinterpret_cast<Int>(&pCell->m_value)) );
            break;
        } // Type_SymVal

        case Annon_TlvOffset:
        {
            Val tlvrec =
                pAnnon->GetOperand()->StaticCast<Literal>()->GetDatum();

            Int iOffset = MiniThread::ToTlvOffset(tlv_record_index(tlvrec));

            fun->Decode<FunObj>()->PatchU32(
                pAnnon->GetAddress(),
                static_cast<uint32>(iOffset) );
            break;
        } // Type_TlvOffset

        default:
            CAN_NOT_HAPPEN();
        } // switch m_eType

        return Annon(
            static_cast<Annon::Type>(nType),
            pAnnon->GetAddress() );
    } // realize_annon

    ////////////////////////////////////////////////////////////
    //
    // realize_function
    //
    // Called by:
    //  X86CgAsmPass::Run
    //
    // Note:
    //  This method must be called when GC disabled.
    //
    void realize_function(Function* pFun)
    {
        ASSERT(NULL != pFun);

        Val funobj = pFun->GetFunObj();
        FunObj* pFunObj = funobj->Decode<FunObj>();

        FunObj::Annon* pnAnnon = pFunObj->GetAnnon();

        FunObj::Annon* pnRunner = pnAnnon;
        foreach (AsmFunction::EnumAnnon, oEnum, pFun)
        {
            AsmAnnon* pAnnon = oEnum.Get();

            *pnRunner++ = realize_annon(pAnnon, funobj);

            ASSERT(Annon_Data != pAnnon->GetType());
        } // for each AsmAnnon

        // Make function object GC aware.
        pFunObj->m_nCookie = FunObj::Cookie;
    } // realize_function

    ////////////////////////////////////////////////////////////
    //
    // IR instruction processors
    //

    //////////////////////////////////////////////////////////////////////
    //
    // process_BRANCH
    //
    //
    //   [1] Next block is false block.
    //      +--------+
    //      | pCurr  |      Jcc pTrue
    //      +--------+
    //      | pFalse |
    //      +--------+
    //
    //   [2] Next block is true block.
    //      +--------+
    //      | pCurr  |      Jcc pFalse
    //      +--------+
    //      | pTrue  |
    //      +--------+
    //
    //   [3] Next block is neither true block or false block
    //      +--------+
    //      | pCurr  |      Jcc pTrue
    //      +--------+      JMP pFalse
    //      | pNext  |
    //      +--------+
    //
    void process_BRANCH(Instruction* pInsn)
    {
        ASSERT(NULL != pInsn);

        Tttn eTttn = get_tttn(pInsn->GetSx()->StaticCast<Bool>());

        BBlock* pTrue  = pInsn->GetSy()->StaticCast<Label>()->GetBBlock();
        BBlock* pFalse = pInsn->GetSz()->StaticCast<Label>()->GetBBlock();

        BBlock* pNext = get_next_bblock(pInsn->GetBBlock());

        if (pNext == pFalse)
        {
            emit_jump(pTrue, op_Jcc_Jv + eTttn, op_Jcc_Jb + eTttn);
        }
        else if (pNext == pTrue)
        {
            eTttn = FlipTttn(eTttn);
            emit_jump(pFalse, op_Jcc_Jv + eTttn, op_Jcc_Jb + eTttn);
        }
        else
        {
            emit_jump(pTrue,  op_Jcc_Jv + eTttn, op_Jcc_Jb + eTttn);
            emit_jump(pFalse, op_JMP_Jv, op_JMP_Jb);
        } // if
    } // process_BRANCH

    //////////////////////////////////////////////////////////////////////
    //
    // process_CALL
    //      Register Callee
    //        TBD
    //      Arguments:
    //        TBD
    //      Callee:
    //        TBD
    //      Output Values:
    //        MOV %rd <= EAX
    void process_CALL(Instruction* pInsn)
    {
        CallInsn* pCall = pInsn->StaticCast<CallInsn>();

        // Callee
        {
            emit_op(op_CALL_Jv);

            switch (pCall->GetSx()->GetKind())
            {
            case Operand::Kind_Function:
                annotate(Annon_LocalCallee, pCall->GetSx());
                emit_u32(0);
                break;

            case Operand::Kind_Literal:
                annotate(Annon_NamedCallee, pCall->GetSx());
                emit_u32(0);
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch kind
        }

        // Output values
        switch (pCall->GetOutput()->GetKind())
        {
        case Operand::Kind_Register:
        {
            Register* pRd = pCall->GetRd();

            if (pRd->IsPhysical())
            {
                if ($r0 != pRd->GetLocation())
                {
                    asm_copy(pRd, $r0);
                } // if
            }
            else
            {
                asm_copy(pRd, $r0);
            } // if
            break;
        } // register

        case Operand::Kind_Values:
            if (need_set_rn(pCall))
            {
                asm_CMOV(tttn_NC, $rn, $rtcb, SVC_fixnum_one);
            }
            break;

        case Operand::Kind_Void:
            // No output
            break;

        default:
            warn(L"CALL instruction has invalid output.");
            break;
        } // switch output
    } // process_CALL



    // need_set_rn
    //  Returns true if output of CALL instruction needs $rn.
    bool need_set_rn(const CallInsn* pCall)
    {
        // Known functions return $rn=1.
        if (pCall->GetSx()->Is<Literal>())
        {
            Val fname = pCall->GetLx();
            if (fname == QDvaluesA) return false;
            if (fname == Qvalues_list) return false;
            if (fname == Qvalues) return false;
        } // literal

        foreach (Values::EnumUseSite, oEnum, pCall->GetVd())
        {
            Instruction* pUser = oEnum.Get()->GetInstruction();
            switch (pUser->GetOpcode())
            {
            case IrOp_CALL:         return true;
            case IrOp_CLOSURE:      return true;
            case IrOp_COUNT:        return true;
            case IrOp_MVSAVE:       break;
            case IrOp_NTHVALUE:     return true;
            case IrOp_OPENFINALLY:  return true;
            case IrOp_PHI:          break;
            case IrOp_PROJECT:      break;
            case IrOp_RET:          break;
            case IrOp_RETURNFROM:   return true;
            case IrOp_THROW:        return true;
            case IrOp_TRAPIF:       return true;
            case IrOp_TRAPIFNOT:    return true;
            default:
                asm_error(L"Unexpected %vx user: ~A",
                    make_string(pUser->GetMnemonic()) );
                break;
            } // switch opcode
        } // for each user

        // Use only by RET.
        return false;
    } // need_set_rn


    // process_COPY
    void process_COPY(Instruction* pCopy)
        { asm_copy(pCopy->GetRd(), pCopy->GetSx()); }

    #if 0
    // process_COUNT
    void process_COUNT(Instruction* pCount)
    {
        ASSERT(pCount->GetRd()->IsPhysical());

        Instruction* pInsn = pCount->GetVx()->GetDfn();
        if (pInsn->Is<CallInsn>())
        {
            asm_CMOV(tttn_NC, $rn, $rtcb, SVC_fixnum_one);
        }
    } // process_COUNT
    #endif

    void process_ELT(Instruction* p) { unsupported(p); }

    //////////////////////////////////////////////////////////////////////
    //
    //  process_ENTRY
    //
    //      min     max
    //      0       any     nothing to emit
    //      n       n       cmp ecx, n; jne arity_error
    //      n       any     cmp ecx, n; jl arity_error
    //      n       m       cmp ecx, n; jl arity_erorr; cmp ecx, m; jgt
    //
    void process_ENTRY(Instruction* pInsn)
    {
        ASSERT(NULL != pInsn);

        if (Knone == pInsn->GetLx()) return;

        if (! pInsn->GetBBlock()->GetFunction()->NeedArity())
        {
            return;
        }

        int iMin = m_oContext.GetFunction()->GetArityMin();
        int iMax = m_oContext.GetFunction()->GetArityMax();

        BBlock* pExitBB = m_oContext.GetFunction()->GetExitBB();

        if (m_oContext.GetFunction()->HasRestParam())
        {
            if (0 != iMin)
            {
                asm_arith_imm(opext_CMP_Ev_Iz, $rn, iMin * Fixnum::One);
                emit_jump(pExitBB, op_JL_Jv, op_JL_Jb);
            }
        }
        else if (iMin == iMax)
        {
            asm_arith_imm(opext_CMP_Ev_Iz, $rn, iMin * Fixnum::One);
            emit_jump(pExitBB, op_JNE_Jv, op_JNE_Jb);
        }
        else
        {
            asm_arith_imm(opext_CMP_Ev_Iz, $rn, iMin * Fixnum::One);
            emit_jump(pExitBB, op_JL_Jv, op_JL_Jb);

            asm_arith_imm(opext_CMP_Ev_Iz, $rn, iMax * Fixnum::One);
            emit_jump(pExitBB, op_JG_Jv, op_JG_Jb);
        }
    } // process_ENTRY

    // process_EXIT
    void process_EXIT(Instruction*)
    {
        Function*  pFun    = m_oContext.GetFunction();
        EntryInsn* pEntryI = pFun->GetEntryInsn();

        if (nil != pEntryI->GetLx() && pFun->NeedArity())
        {
            emit_op(op_CALL_Ev);
            emit_disp(opext_CALL_Ev, $rtcb, SVC_arity_error);
        }
    } // process_EXIT

    //  process_GO
    //
    //      MOV $r0 <- {target}
    //      MOV $rn, '1
    //      CALL $rtcb.SVC_go
    //
    void process_GO(Instruction* pInsn)
    {
        ASSERT(NULL != pInsn);

        asm_copy($r0, pInsn->GetRx());
        asm_MOV($rn, Fixnum::One);

        emit_op(op_CALL_Ev);
        emit_disp(opext_CALL_Ev, $rtcb, SVC_go);
    } // process_GO

    // process_JUMP
    void process_JUMP(Instruction* pInsn)
    {
        if (! is_useless_JUMP(pInsn))
        {
            BBlock* pTargetBB =
                pInsn->GetSx()->StaticCast<Label>()->GetBBlock();

            emit_jump(pTargetBB, op_JMP_Jv, op_JMP_Jb);
        }
    } // process_JUMP

    void process_LOADTIMEVALUE(Instruction* pInsn)
    {
        LoadTimeValueInsn* pLoad = pInsn->StaticCast<LoadTimeValueInsn>();
        Session::Get()->AddLoadTimeValue(
            pLoad->GetLx(),
            Fixnum::Encode(pLoad->GetSy()) );
    } // process_LOADTIMEVALUES

    void process_MVRESTORE(Instruction* pInsn)
      { asm_load($r0, pInsn->GetRx()); }

    // process_PARSEKEYS
    //      MOV $r0 <- {rest}
    //      MOV $r1 <- {keyvec}
    //      MOV $rn <- '2
    //      CALL .check-keys
    //
    void process_PARSEKEYS(Instruction* pParseKeys)
    {
        Val parser;
        {
            if (pParseKeys->GetLy() == QAkey)
            {
                parser = QDcheck_keys;
            }
            else if (pParseKeys->GetLy() == QAallow_other_keys)
            {
                parser = QDparse_keys;
            }
            else
            {
                CAN_NOT_HAPPEN();
            }
        } // parser

        asm_copy($r0, pParseKeys->GetRx());
        asm_copy($r1, pParseKeys->GetSz());
        asm_MOV($rn, Fixnum::One * 2);
        asm_CALL(parser);
    } // process_PARSEKEYS

    // process_PROJECT
    //  Emits cmovnc ecx, fixnum_one if %vx is output of CALL.
    //
    void process_PROJECT(Instruction* pProject)
    {
        ASSERT(pProject->GetRd()->IsPhysical());

        const int cRegArgs = Target_::km_pMach->m_pGprArg->m_n;

        if (pProject->GetLy() < Fixnum::Encode(cRegArgs))
        {
            Int iNth = Fixnum::Decode_(pProject->GetLy());

            asm_copy(
                pProject->GetRd(),
                static_cast<Reg>(Target_::km_pMach->m_pGprArg->m_prgn[iNth]) );
        }
        else
        {   
            size_t ofs = offsetof(Thread, mv_value);
                    ofs += Fixnum::Decode_(pProject->GetLy()) * sizeof(Val);

            asm_MOV(pProject->GetRd(), $rtcb, static_cast<int>(ofs));
        }
    } // process_PROJECT

    // process_SELECT
    void process_SELECT(Instruction* pInsn)
    {
        ASSERT(NULL != pInsn);

        Register*   pRd = pInsn->GetRd();
        Bool*       pBx = pInsn->GetSx()->StaticCast<Bool>();
        Register*   pRy = pInsn->GetRy();
        Register*   pRz = pInsn->GetRz();

        ASSERT(pRd->IsPhysical());
        ASSERT(pRy->IsPhysical());
        ASSERT(pRd->GetLocation() == pRy->GetLocation());

        asm_CMOV(FlipTttn(get_tttn(pBx)), pRd, pRz);
    } // process_SELECT

    // process_SHL
    void process_SHL(Instruction* pShl)
    {
        switch (pShl->GetSy()->GetKind())
        {
        case Operand::Kind_Register:
            asm_SHx(pShl, opext_SHL_Ev_CL);
            break;

        case Operand::Kind_Literal:
        {
            Val k = pShl->GetLy();
            Int nK = fixnump(k) ? Fixnum::Decode_(k) : -1;
            asm_SHx(pShl, opext_SAL_Ev_Ib, nK);
            break;
        } // literal

        case Operand::Kind_Integer:
        {
            asm_SHx(pShl, opext_SAL_Ev_Ib, pShl->GetIy());
            break;
        } // integer

        default:
            CAN_NOT_HAPPEN();
        } // switch operand
    } // process_SHL

    // process_SHR
    void process_SHR(Instruction* pShr)
    {
        switch (pShr->GetSy()->GetKind())
        {
        case Operand::Kind_Register:
            asm_SHx(pShr, opext_SAR_Ev_CL);
            break;

        case Operand::Kind_Literal:
        {
            Val k = pShr->GetLy();
            Int nK = fixnump(k) ? Fixnum::Decode_(k) : -1;
            asm_SHx(pShr, opext_SAR_Ev_Ib, nK);
            break;
        } // literal

        case Operand::Kind_Integer:
        {
            asm_SHx(pShr, opext_SAR_Ev_Ib, pShr->GetIy());
            break;
        } // integer

        default:
            CAN_NOT_HAPPEN();
        } // switch operand
    } // process_SHR

    // process_TAG - Sets %rd to address of tag.
    void process_TAG(Instruction* pInsn)
    {
        Frame* pFrame = pInsn->GetSx()->StaticCast<Frame>();
        uint nIndex = 0;
        foreach (Frame::EnumUseSite, oEnum, pFrame)
        {
            if (oEnum.Get()->GetInstruction() == pInsn) break;
            nIndex += 1;
        } // for each use site

        uint ofs = static_cast<uint>(
            pFrame->GetLocation() +
            offsetof(TagbodyFrame, m_rgoTag[nIndex]) );

        asm_LEA(pInsn->GetRd(), $sp, ofs);
    } // process_TAG

    // process_VARDEF
    void process_VARDEF(Instruction* pVarDef)
    {
        asm_copy(pVarDef->GetRd(), pVarDef->GetSy());
    } // process_VARDEF

    // process_FRAME
    void process_FRAME(Instruction* pInsn)
    {
        Register* pRd = pInsn->GetRd();
        if (pRd->IsPseudo()) return;

        ASSERT(pRd->Is<Physical>());

        Addr oAddr;
        AddrMode eMode = compute_addr(pInsn, &oAddr);
            ASSERT(AddrMode_BaseDisp == eMode);

        asm_LEA(cast_to_reg(pRd), cast_to_reg(oAddr.m_pRx), oAddr.m_ofs);
    } // process_FRAME

    // process_FRAMEDEF
    void process_FRAMEREF(Instruction* pInsn)
    {
        FrameRefInsn* pFrameRef = pInsn->StaticCast<FrameRefInsn>();

        Register* pRd = pFrameRef->GetRd();
        if (pRd->IsPhysical() && pRd->GetLocation() == $sp)
        {
            return;
        }

        Function* pCallee = pFrameRef->GetFunction();
        Function* pCaller = pFrameRef->GetFrameOwner();

        if (pCaller == pCallee)
        {
            asm_copy(pFrameRef->GetRd(), $sp);
        }
        else
        {
            ASSERT(pRd->IsPhysical());
            asm_LEA(pRd, $sp, compute_path_size(pCallee, pCaller));
        } // if
    } // process_FRAMEREF

    // process_SWAP
    void process_SWAP(Instruction* pSwap)
    {
        Register* pRx = pSwap->GetRx();
        Register* pRy = pSwap->GetRy();
        asm_XCHG(cast_to_reg(pRx), cast_to_reg(pRy));
    } // process_SWAP

    void process_RUNTIMECAST(Instruction * p) { unsupported(p); }
    void process_SIGMA(Instruction* p)       { unsupported(p); }

    // process_X86X64_LEA2
    void process_X86X64_LEA2(Instruction* pLea)
    {
        if (pLea->GetRd()->IsPseudo()) return;

        Addr oAddr;
        AddrMode eAddr = compute_lea_addr(pLea, &oAddr);
        switch (eAddr)
        {
        case AddrMode_BaseDisp:
            asm_LEA(pLea->GetRd(), cast_to_reg(oAddr.m_pRx), oAddr.m_ofs);
            break;

        case AddrMode_BaseIndex:
            asm_LEA(pLea->GetRd(),
                cast_to_reg(oAddr.m_pRx),
                cast_to_reg(oAddr.m_pRy),
                oAddr.m_ofs );
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch eAddr
    } // process_X86X64_LEA2

   DEFPROC_SAME(X86X64_LEA3, X86X64_LEA2);
}; // X86X64AsmPass

} // Compiler

#endif //!defined(INCLUDE_x86x64_compiler_cg_asm_h)
