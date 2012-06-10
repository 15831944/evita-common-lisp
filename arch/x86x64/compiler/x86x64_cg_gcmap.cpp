#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - x86x64 - GC Map Construction
// arch/x86x64/compiler/x86x64_cg_gcmap.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_gcmap.cpp#16 $
//
#include "./x86x64_cg_gcmap.h"

#include "./x86x64_cg_asm.h"

#include "../kernel/x86x64_ke_frame.h"

#include "../../../compiler/cm/cm_target.h"
#include "../../../compiler/ir/ir_dfa.h"


//  GCMap is mapping of native instruction to GC annotation.
//
//
//    GC Map                    Segment Table
//    +------------------+      +----------------+
//    |                  |      |  entry list_0  |
//    |  segment table   |      +----------------+
//    |                  |      |  entry list_1  |
//    +------------------+      +----------------+
//    |                  |            ...
//    | entry list  pool |      +----------------+
//    |                  |      |  entry list_n-1|
//    +------------------+      +----------------+
//    |                  |
//    | description pool |
//    |                  |
//    +------------------+
//
//     Entry List           Entry
//    +------------+        +-------------+-----------+
//    |  #entries  |        | Desc Offset | IP offset |
//    +------------+        +-------------+-----------+
//    |   entry_0  |             16bit        16bit
//    +------------+
//    |   entry_1  |        Desc Offset = origin is start of GC Map
//    +------------+        IP offset   = origin is start of 64KB segment,
//          ...                           or low 16 bit of address.
//    +------------+
//    |   entry_n-1|
//    +------------+
//
//     GC Description
//    +----------------------+--+-+     Bit[0] = continue-p
//    |       data           |TT|1|     TT = type of GC description.
//    +----------------------+--+-+         00 = general
//            ....                          01 = stdcall
//    +----------------------+--+-+         10 = extcall
//    |       data              |0|         11 = extcall3
//    +----------------------+--+-+
//


namespace Compiler
{

namespace
{

// PrintableBitVec
class PrintableBitVec : public Atom
{
    const BitVec* m_pBitVec;

    public: PrintableBitVec(const BitVec* bv) : m_pBitVec(bv) {}

    public: virtual void HtmlPrint(Val s, bool) const
    {
        html_format(s, L"#");
        Val comma = Character::Encode('*');
        for (uint i = 0; i < m_pBitVec->GetLength(); i++)
        {
            if (i >= 50) { html_format(s, L"..."); break; }

            if (0 == i % 5)
            {
                write_char(comma, s);
                comma = Character::Encode(' ');
            }

            html_format(s, L"~A", m_pBitVec->IsZero(i) ? L"0" : L"1");
        } // for i
    } // HtmlPrint
}; // PrintableBitVec


// is_gc_type
static bool is_gc_type(Ty ty)
{
    static Ty const kv_non_gc_type[] =
    {
        ty_int8,  ty_int16,  ty_int32,  ty_int64,  ty_int,
        ty_uint8, ty_uint16, ty_uint32, ty_uint64, ty_uint,

        ty_float32,
        ty_float64,
    }; // kv_non_gc_type

    if (consp(ty)) return Qptr != car(ty);

    for (
        const Ty* p = &kv_non_gc_type[0];
        p < &kv_non_gc_type[lengthof(kv_non_gc_type)];
        p++ )
    {
        if (*p == ty) return false;
    } // for each ty

    return true;
} // is_gc_type


#if 0
// has_back_edge
static bool has_back_edge(BBlock* pBBlock)
{
    foreach (BBlock::EnumOutEdge, oEnum, pBBlock)
    {
        if (oEnum.Get()->IsBackward()) return true;
    } // for each egde
    return false;
} // has_back_edge
#endif


//////////////////////////////////////////////////////////////////////
//
// GcMapSubPass
//
class GcMapSubPass : public SubPass
{
    protected: X86X64GcMapPass* m_pPass;

    protected: GcMapSubPass(Pass* pPass, const char16* pwsz ) :
        SubPass(pPass, pwsz) {}

    // [M]
    protected: uint map_to_index(Register* pRx, bool fOutput = false) const
        { return m_pPass->map_to_index(pRx, fOutput); }

    protected: uint map_to_index(uint rx) const
        { return m_pPass->map_to_index(rx); }

    protected: uint map_to_index(Instruction* pInsn, uint nNth) const
        { return m_pPass->map_to_index(pInsn, nNth); }

    // [P]
    void processFrame(Frame* pFrame)
    {
        if (NULL == pFrame) return;

        int ofs = pFrame->GetLocation();

        if (pFrame->GetKind() == Qblock ||
            pFrame->GetKind() == Qcatch )
        {
            processFrameSlot(ofs + offsetof(XferFrame, m_fn));
            processFrameSlot(ofs + offsetof(XferFrame, m_name));
        }
        else if (pFrame->GetKind() == Kcode)
        {
            return;
        }
        else if (pFrame->GetKind() == Kfinally)
        {
            processFrameSlot(ofs + offsetof(FinallyFrame, m_fn));

            uint cArgs = pFrame->GetDfn()->GetVy()->
                GetDfn()->GetOperandCount();

            ofs += offsetof(FinallyFrame, mv_arg);
            for (uint n = 0; n < cArgs; n++)
            {
                processFrameSlot(ofs);
                ofs += sizeof(Val);
            }
        }
        else if (pFrame->GetKind() == Qlet)
        {
            ofs += sizeof(BindFrame);
            foreach (
                OpenBindInsn::EnumInput,
                oEnum,
                pFrame->GetDfn()->StaticCast<OpenBindInsn>() )
            {
                Val cell = oEnum.GetBox()->GetCell();

                if (value_cell_p(cell)) processFrameSlot(ofs);

                ofs += sizeof(Val);

                processFrameSlot(ofs);
                ofs += sizeof(Val);
            } // for each bind
        }
        else if (pFrame->GetKind() == Qtagbody)
        {
            processFrameSlot(ofs + offsetof(TagbodyFrame, m_fn));
        }
        else
        {
            COMPILER_INTERNAL_ERROR();
        }
    } // processFrame

    // processFrameSlot
    void processFrameSlot(int ofs)
        { process_input(ofs / sizeof(Val) + m_pPass->m_pAllRegs->m_n + 1); }

    // process_input, process_last, process_output
    protected: virtual void process_input(int) = 0;
    protected: virtual void process_last(Instruction*, uint) = 0;
    protected: virtual void process_output(int) = 0;

    // process_inputs
    protected: void process_inputs(Instruction* pInsn)
    {
        switch (pInsn->GetOpcode())
        {
        case IrOp_CLOSE:
            processFrame(pInsn->GetSx()->StaticCast<Frame>());
            break;

        case IrOp_USE:
        {
            processFrame(pInsn->GetSx()->DynamicCast<Frame>());
            break;
        } // use
        } // switch opcode

        {
            uint nNth = 0;
            foreach (Instruction::EnumInput, oEnum, pInsn)
            {
                process_input(map_to_index(pInsn, nNth));
                nNth += 1;
            } // for each input operand
        }
    } // process_inputs

    protected: enum Direction { Forward, Backward };

    // process_lasts
    protected: void process_lasts(BBlock* pBBlock, Direction eDir)
    {
        Instruction* pLast = pBBlock->GetLastInsn();
        switch (pLast->GetOpcode())
        {
        case IrOp_BRANCH:
        {
            if (Forward == eDir) process_last(pLast, 0);

            BBlock* pNext = pBBlock->GetNext();
            uint cbInsn = pNext->GetFirstInsn()->GetIndex() -
                          pLast->GetIndex();
            switch (cbInsn)
            {
            case 2: // Jcc_Jb
            case 6: // Jcc_Jv
                break;
            case 2+2: // Jcc_Jb + JMP_Jb
            case 2+5: // Jcc_Jb + JMP_Jv
                process_last(pLast, 2);
                break;
            case 6+2: // Jcc_Jv + JMP_Jb
            case 6+5: // Jcc_Jv + JMP_Jv
                process_last(pLast, 6);
                break;
            default:
                CAN_NOT_HAPPEN();
            } // switch cbInsn

            if (Backward == eDir) process_last(pLast, 0);
            break;
        } // branch

        case IrOp_JUMP:
            if (! is_useless_JUMP(pLast))
            {
                process_last(pLast, 0);
            }
            break;
        } // switch opcode
    } // process_lasts

    // process_outputs
    protected: void process_outputs(Instruction* pInsn)
    {
        switch (pInsn->GetOpcode())
        {
        case IrOp_STORE:
        {
            Register* pRx = pInsn->GetRx();

            if (NULL != pRx && pRx->IsPseudo())
                { process_output(map_to_index(pRx)); }
            break;
        } // store

        default:
            process_output(map_to_index(pInsn->GetRd(), true));
            break;
        } // switch opcode
    } // process_outputs
}; // GcMapSubPass


//////////////////////////////////////////////////////////////////////
//
// ComputePass
//
class ComputePass : public GcMapSubPass
{
    public: ComputePass(Pass* pPass) :
        GcMapSubPass(pPass, L"* Compute Gc Map") {}

    public: static void Run(
        Pass*               pPass,
        X86X64GcMapPass*    pGcMapPass,
        Function*           pFun )
    {
        ComputePass oSubPass(pPass);
            oSubPass.m_pPass = pGcMapPass;
        oSubPass.run(pFun);
    } // Run

    // run
    //  [1] Compute local liveness
    //  [2] Compute global liveness
    void run(Function* pFun)
    {
        // Note: FunObj::GetFrameSize includes RA slot.
        // Howerver, we assign index one for the first
        // register. So, we don't need to substract RA
        // slot.
        uint cBits = m_pPass->m_pAllRegs->m_n;
             cBits +=  pFun->GetFrameSize() / sizeof(Val);

        html_log_format(1, L"nregs=~D, nstacks=~D nbits=~D~:%",
            m_pPass->m_pAllRegs->m_n,
            pFun->GetFrameSize() / sizeof(Val),
            cBits );

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            DataFlowBB* pBBlock = oEnum.Get()->Extend<DataFlowBB>();
            pBBlock->InitDataFlow(Session::Get(), cBits);
            process_bblock(pBBlock);
        } // for each bblock

        SolveBackward(pFun);

        #if _DEBUG
        {
            html_log_format(2, L"<h2>Liveness</h2>~%");

            foreach (Function::EnumBBlock, oEnum, pFun)
            {
                DataFlowBB* pBBlock = oEnum.Get()->Extend<DataFlowBB>();

                html_log_format(2, L"~S {", pBBlock);

                const BitVec* pLiveIn = pBBlock->GetIn();

                for (
                    uint i = 0;
                    i < m_pPass->m_pAllRegs->m_n;
                    i++ )
                {
                    if (pLiveIn->IsOne(i+1))
                    {
                        html_log_format(2, L" ~W",
                            cm_get_target()->GetPhysicalName(
                                m_pPass->m_pAllRegs->m_prgn[i] ) );
                    }
                } // for i

                uint ofs = 0;
                for (
                    uint i = m_pPass->m_pAllRegs->m_n + 1;
                    i < pLiveIn->GetLength();
                    i++ )
                {
                    if (pLiveIn->IsOne(i))
                        { html_log_format(2, L" [sp+~D]", ofs); }
                    ofs += sizeof(Val);
                } // for i

                html_log_format(2, L"}~:%");
            } // for each bblock
        }
        #endif // _DEBUG
    } // run

    DataFlowBB* m_pBBlock;

    // process_bblock
    void process_bblock(DataFlowBB* pBBlock)
    {
        m_pBBlock = pBBlock;

        foreach (BBlock::EnumInsn, oEnum, pBBlock)
        {
            Instruction* pInsn = oEnum.Get();

            switch (pInsn->GetOpcode())
            {
            case IrOp_CALL:
                add_entry(pInsn, 0);
                break;
            } // switch opcode

            process_inputs(pInsn);
            process_outputs(pInsn);
        } // for each bblock

        process_lasts(pBBlock, Forward);
    } // process_bblock

    // process_input
    virtual void process_input(int idx)
        { if (! m_pBBlock->IsKill(idx)) m_pBBlock->SetIn(idx); }

    // process_output
    virtual void process_output(int idx)
        { m_pBBlock->SetKill(idx); }

    // process_last
    virtual void process_last(Instruction* pInsn, uint ofs)
        { add_entry(pInsn, ofs); }

    // add_entry
    void add_entry(Instruction* pInsn, uint ofs)
    {
        uint rip = pInsn->GetIndex() + ofs;

        if (rip > m_pPass->m_cSegments * 64 * 1024)
        {
            m_pPass->m_cSegments += 1;
        }

        html_log_format(1, L"[~D.~D] ~5,'0X: ~S ~S~:%",
            m_pPass->m_cSegments - 1,
            m_pPass->m_cEntries,
            rip,
            pInsn->GetBBlock(),
            pInsn );

        m_pPass->m_cEntries += 1;
    } // process_last
}; // ComputePass


////////////////////////////////////////////////////////////
//
// AssignPass
//
class AssignPass : public GcMapSubPass
{
    // Run - Entry Point
    public: static void Run(
        Pass*               pPass,
        X86X64GcMapPass*    pGcMapPass,
        Function*           pFun )
    {
        AssignPass oSubPass(pPass);
            oSubPass.m_pPass = pGcMapPass;
        oSubPass.run(pFun);
    } // Run

    // AssignPass ctor
    AssignPass(Pass* pPass) :
        GcMapSubPass(pPass, L"*Assign Gc Desc") {}

    BitVec* m_pLive;

    // run
    void run(Function* pFun)
    {
        foreach (Function::EnumBBlock_Reverse, oEnum, pFun)
        {
            DataFlowBB* pBBlock = oEnum.Get()->Extend<DataFlowBB>();
            process_bblock(pBBlock);
        } // for each bblock
    } // run

    // process_bblock
    void process_bblock(DataFlowBB* pBBlock)
    {
        m_pLive = pBBlock->GetOut();

        // Check the last instruction.
        process_lasts(pBBlock, Backward);

        foreach (BBlock::EnumInsn_Reverse, oEnum, pBBlock)
        {
            Instruction* pInsn = oEnum.Get();

            process_outputs(pInsn);
            process_inputs(pInsn);

            switch (pInsn->GetOpcode())
            {
            #if 0
                case IrOp_FRAME:
                case IrOp_LOAD:
                case IrOp_STORE:
                {
                    PrintableBitVec oBitVec(m_pLive);
                    html_log_format(1, L"~5,'0X: ~S ~S ~S~:%",
                        pInsn->GetIndex(),
                        pBBlock,
                        pInsn,
                        &oBitVec );
                    break;
                } // load, store
            #endif

            case IrOp_CALL:
                set_gcmap(pInsn, 0, GcDesc_StdCall);
                break;
            } // switch opcode
        } // for each insn
    } // process_bblock

    // process_input
    virtual void process_input(int idx)
        { m_pLive->SetOne(idx); }

    // process_last
    virtual void process_last(Instruction* pInsn, uint ofs)
        { set_gcmap(pInsn, ofs, GcDesc_General); }

    virtual void process_output(int idx)
        { m_pLive->SetZero(idx); }

    // set_gcmap
    void set_gcmap(
        Instruction*    pInsn,
        uint            ofs,
        GcDescType      eType )
    {
        BitVec* pBitVec = m_pLive;

        pBitVec->SetZero(0);

        GcMapFactory::Entry* pEntry =
            m_pPass->intern(pInsn->GetIndex() + ofs, eType, pBitVec);

        html_log_format(1, L"~5,'0X: ~S ~S ~S~:%",
            pInsn->GetIndex() + ofs,
            pInsn->GetBBlock(),
            pInsn,
            pEntry->m_pDesc );
    } // set_gcmap
}; // AssignPass

} // namespace


// X86X64GcMapPass::map_to_index
uint X86X64GcMapPass::map_to_index(Register* pRx, bool fOutput) const
{
    if (NULL == pRx) return 0;

    switch (pRx->GetStorage())
    {
    case Register::Storage_Physical:
        return map_to_index(pRx->GetLocation());

    case Register::Storage_Stack:
    {
        int idx = pRx->GetLocation() / sizeof(Val);
            idx += m_pAllRegs->m_n + 1;
        return idx;
    } // stack

    case Register::Storage_Pseudo:
    {
        if (fOutput) return 0;

        FrameInsn* pDfn = pRx->GetDfn()->DynamicCast<FrameInsn>();
        if (NULL == pDfn) return 0;

        Val ty = ty_get_pointee(pDfn->GetTy());
        if (! is_gc_type(ty)) return 0;
        if (Qfixnum == ty) return 0;

        Int ofs = pDfn->GetSx()->StaticCast<Frame>()->GetLocation() +
                  pDfn->GetSy()->StaticCast<Integer>()->GetValue();

        int idx = static_cast<int>(ofs) / sizeof(Val);
            idx += m_pAllRegs->m_n + 1;

        return idx;
    } // pseudo

    default:
        return 0;
    } // switch storage
} // X86X64GcMapPass::map_to_index


// X86X64GcMapPass::map_to_index
uint X86X64GcMapPass::map_to_index(uint rx) const
{
    return (rx & (m_pAllRegs->m_n - 1)) + 1;
} // X86X64GcMapPass::map_to_index


// X86X64GcMapPass::map_to_index
//
// Note:
// We don't need to make operands of EQ and NE instruction GC trackable,
// since eq-ness are preserved before and after GC. So, operands of CMP
// instruction aren't GC trackable.
//
uint X86X64GcMapPass::map_to_index(Instruction* pInsn, uint nNth) const
{
    Register* pRx = pInsn->GetOperand(nNth)->DynamicCast<Register>();

    uint nIndex = map_to_index(pRx);
        if (0 == nIndex) return 0;

    switch (pInsn->GetOpcode())
    {
    case IrOp_LOAD:
        if (pRx->IsPseudo()) return nIndex;
        return 0;

    case IrOp_STORE:
        if (! pRx->IsPseudo()) return nIndex;
        return 0;

    case IrOp_RET:
    {
        Val fun_ty = pInsn->GetBBlock()->GetFunction()->GetTy();
        Val ty = ty_get_function_value(fun_ty);
        if (! is_gc_type(ty)) return 0;
        break;
    } // RET

    case IrOp_SELECT:
    case IrOp_COPY: case IrOp_PHICOPY: case IrOp_SIGMA:
    case IrOp_RELOAD: case IrOp_SPILL: case IrOp_SPLIT:
    case IrOp_STACKDEF:
    case IrOp_SWAPGET:
        if (! is_gc_type(pInsn->GetTy())) return 0;
        break;

    case IrOp_ADD: case IrOp_MUL: case IrOp_TRUNCATE: case IrOp_SUB:
    case IrOp_LOGAND: case IrOp_LOGIOR: case IrOp_LOGXOR:
    case IrOp_LOGEQV:
        return 0;

    case IrOp_VALUES:
    {
        foreach (EnumTy, oEnum, pInsn->GetTy())
        {
            Val ty = oEnum.Get();
            if (0 == nNth)
            {
                if (! is_gc_type(ty)) return 0;
                break;
            }
            nNth -= 1;
        } // for each ty
        ASSERT(static_cast<int>(nNth) >= 0);
        break;
    } // VALUES

    case IrOp_VALUESA:
    {
        if (pInsn->GetOperandCount() - 1 == nNth) break;

        foreach (EnumTy, oEnum, pInsn->GetTy())
        {
            Val ty = oEnum.Get();
            if (0 == nNth)
            {
                if (! is_gc_type(ty)) return 0;
                break;
            }
            nNth -= 1;
        } // for each ty
        ASSERT(static_cast<int>(nNth) >= 0);
        break;
    } // VALUESA
    } // switch opcode

    return nIndex;
} // X86X64GcMapPass::map_to_index


//////////////////////////////////////////////////////////////////////
//
// X86X64GcMapPass::intern
//
GcMapFactory::Entry*
X86X64GcMapPass::intern(
    uint            ofsCode,
    GcDescType      eType,
    const BitVec*   pBitVec )
{
    Entry* pEntry = new Entry(ofsCode);

    uint nHashCode = GcDesc::Hash(eType, pBitVec);

    GcDesc* pDesc = m_oHashTable.Get(nHashCode, eType, pBitVec);
    if (NULL == pDesc)
    {
        pDesc = new GcDesc(
            m_ofsDesc,
            eType,
            BitVec::Make(Session::Get(), pBitVec->GetLength())->
                Copy(pBitVec) );

        m_oHashTable.Put(pDesc);

        m_ofsDesc += serialize(NULL, pDesc);
    } // if

    m_oEntries.Prepend_(pEntry);

    pEntry->m_pDesc = pDesc;
    return pEntry;
} // X86X64GcMapPass::intern


// GcMapFactory::GcDesc::HtmlPrint
void GcMapFactory::GcDesc::HtmlPrint(Val s, bool) const
{
    PrintableBitVec oBitVec(m_pBitVec);

    html_format(s, L"[GcDesc ~D ~A ~S]",
        m_ofsDesc,
        GcDesc_StdCall == m_eType ? L"StdCall" :
        GcDesc_General == m_eType ? L"General" : L"Unknown",
        &oBitVec );
} // GcMapFactory::GcDesc::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
// X86X64GcMapPass::Compute
//
uint X86X64GcMapPass::Compute(Function* pFun)
{
    ComputePass::Run(m_pPass, this, pFun);

    m_oHashTable.Init(m_cEntries);

    AssignPass::Run(m_pPass, this, pFun);

    html_log_format(1, L"#Entries=~D, #GcDesc=~D, sizeof(GcDesc Pool)=~D~:%",
        m_cEntries,
        m_oHashTable.GetCount(),
        m_ofsDesc );

    if (0 == m_cEntries) return m_cSegments * 4;

    uint ofs = (m_cEntries + m_cSegments * 2) * 4;

    return m_ofsDesc + ofs;
} // X86X64GcMapPass::Compute


//////////////////////////////////////////////////////////////////////
//
// X86X64GcMapPass::Serialize
//
void X86X64GcMapPass::Serialize(uint32* pStart) const
{
    if (0 == m_cEntries)
    {
        if (NULL != pStart)
        {
            for (uint i = 0; i < m_cSegments; i++) pStart[i] = 0;
        }
        return;
    }

    ASSERT(m_cSegments >= 1);

    uint ofsDesc = m_cEntries + (m_cSegments * 2);
        ofsDesc <<= 2;

    uint32* p = pStart;

    // Serialize entry pool
    {
        uint32* pnList = p;
            p += m_cSegments;

        *pnList++ = static_cast<uint>(p - pStart);

        uint32* pnCount = p;
            p++;

        const uint k_cbSegment = 64 * 1024;
        uint ofsSegment = 0;
        uint cEntries = 0;

        foreach (EnumEntry, oEnum, this)
        {
            const Entry* pEntry = oEnum.Get();

            while (pEntry->m_ofsCode - ofsSegment >= k_cbSegment)
            {
                *pnCount = cEntries;

                pnCount = p;

                *pnList = static_cast<uint32>(p - pStart);
                    pnList++;

                ofsSegment += k_cbSegment;
                cEntries = 0;
            } // while

            cEntries += 1;

            ASSERT(pEntry->m_ofsCode - ofsSegment < k_cbSegment);

            uint word = (pEntry->m_pDesc->m_ofsDesc + ofsDesc) >> 2;
                word <<= 16;
                word |= pEntry->m_ofsCode - ofsSegment;

            *p++ =  word;
        } // for each entry

        if (0 != cEntries)
        {
            *pnCount = cEntries;
        }
    } // entry pool

    // Serialize GC Description
    {
        foreach (EnumEntry_Reverse, oEnum, this)
        {
            const Entry* pEntry = oEnum.Get();
            uint idx = (pEntry->m_pDesc->m_ofsDesc + ofsDesc) / 4;
            if (static_cast<uint>(p - pStart) == idx)
            {
                p += serialize(p, pEntry->m_pDesc) / 4;
            }
        } // for each entry
    } // gc desc
} // X86X64GcMapPass::Serialize


// X86X64GcMapPass::serialize
uint X86X64GcMapPass::serialize(uint32* p, const GcDesc* pDesc) const
{
    const BitVec* bv = pDesc->m_pBitVec;
    uint last = bv->PositionOneFromEnd();
        if (static_cast<uint>(-1) == last) last = 0;

    BitStream oStream(p);

    switch (pDesc->m_eType)
    {
    case GcDesc_General:
        oStream.Emit(0);
        oStream.Emit(pDesc->m_eType & 1);
        oStream.Emit(pDesc->m_eType >> 1);
        for (uint i = 1; i <= m_cReg2Idx; i++)
        {
            if (0 != m_pReg2Idx[i -1])
            {
                oStream.Emit(bv->IsOne(i));
            }
        } // for i
        break;

    case GcDesc_StdCall:
        oStream.Emit(0);
        oStream.Emit(pDesc->m_eType & 1);
        oStream.Emit(pDesc->m_eType >> 1);
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch type

    for (
        uint i = m_cReg2Idx + 1;
        i <= last;
        i++ )
    {
        if (oStream.IsStart())
        {
            oStream.Flush(1);
            oStream.Emit(0);
        }

        oStream.Emit(bv->IsOne(i));
    } // while

    return oStream.Flush(0);
} // X86X64GcMapPass::serialize


//////////////////////////////////////////////////////////////////////
//
// Get
//
GcMapFactory::GcDesc*
GcMapFactory::HashTable::Get(
    uint            nHashCode,
    GcDescType      eType,
    const BitVec*   bv ) const
{
    const Slot* pTop    = m_prgoSlot;
    const Slot* pEnd    = m_prgoSlot + m_cAlloc;
    const Slot* pStart  = pTop + nHashCode % (pEnd - pTop);
    const Slot* pRunner = pStart;
    for (;;)
    {
        if (NULL == pRunner->m_p) return NULL;

        if (pRunner->m_p->m_eType == eType &&
            pRunner->m_p->m_pBitVec->Equal(bv) )
        {
            return pRunner->m_p;
        }

        pRunner++;
        if (pRunner == pEnd) pRunner = pTop;
        ASSERT(pRunner != pStart);
    } // for
} // GcMapFactory::HashTable::Get


//////////////////////////////////////////////////////////////////////
//
// GcMapFactory::HashTable::Init
//
void GcMapFactory::HashTable::Init(uint cEntries)
{
    m_cAlloc = cEntries * 130 / 100;
    m_prgoSlot = new Slot[m_cAlloc];
        ::ZeroMemory(m_prgoSlot, sizeof(Slot) * m_cAlloc);
} // GcMapFactory::HashTable::Init


//////////////////////////////////////////////////////////////////////
//
// Put
//
void GcMapFactory::HashTable::Put(GcDesc* pDesc)
{
    if (m_nCount * 100 > m_cAlloc * 65)
    {
        Slot* prgoSlot    = m_prgoSlot;
        Slot* prgoSlotEnd = m_prgoSlot + m_cAlloc;

        m_cAlloc += m_cAlloc * 30 / 100;
        m_prgoSlot = new Slot[m_cAlloc];
            ::ZeroMemory(m_prgoSlot, sizeof(Slot) * m_cAlloc);

        uint nRest = m_nCount;
        for (
            const Slot* pRunner = prgoSlot;
            pRunner < prgoSlotEnd;
            pRunner++ )
        {
            if (NULL != pRunner->m_p)
            {
                put(pRunner->m_p);
                nRest -= 1;
                if (0 == nRest) break;
            }
        }
    } // if

    put(pDesc);
    m_nCount += 1;
} // void X86X64GcMapPass::Put


// put
void GcMapFactory::HashTable::put(GcDesc* pDesc)
{
    Slot* pTop    = m_prgoSlot;
    Slot* pEnd    = m_prgoSlot + m_cAlloc;
    Slot* pStart  = pTop + pDesc->m_nHashCode % (pEnd - pTop);
    Slot* pRunner = pStart;
    for (;;)
    {
        if (NULL == pRunner->m_p)
        {
            pRunner->m_p = pDesc;
            return;
        }

        pRunner++;
        if (pRunner == pEnd) pRunner = pTop;
        ASSERT(pRunner != pStart);
    } // for
} // GcMapFactory::HashTable::put

} // Compiler
