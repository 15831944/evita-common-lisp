//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86 - ra-ls
// cg/x86/x86_ra_ls.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_ra_ls.h#18 $
//
#if !defined(INCLUDE_arch_x86x64_compiler_cg_ra_ls_h)
#define INCLUDE_arch_x86x64_compiler_cg_ra_ls_h

#include "../../../compiler/cg/cg_ra_ls.h"

namespace Compiler
{

using namespace LinearScanRA;

#if MACH == MACH_x86
enum Flag
{
    Flag_ByteReg = 1,
}; // Flag
#endif MACH == MACH_x86


//////////////////////////////////////////////////////////////////////
//
// CallConv
//  Represents calling convention.
//
struct CallConv
{
    const RegSet*   m_pGprArgs;
    const RegSet*   m_pFprArgs;
    const RegSet*   m_pVolatile;
    int             m_rn;

    CallConv(
        int             rn,
        const RegSet*   pGpr,
        const RegSet*   pFpr,
        const RegSet*   pVol ) :
            m_rn(rn),
            m_pGprArgs(pGpr), m_pFprArgs(pFpr),
            m_pVolatile(pVol) {}
}; // CallConv


//////////////////////////////////////////////////////////////////////
//
// X86X64RegisterAllocator
//
template<class Target_, uint t_cRegs>
class X86X64RegisterAllocator : public RegisterAllocator
{
    CallConv m_oCallConv;

    public: X86X64RegisterAllocator(
        const char16*   pwsz,
        const RegSet*   pGprAlloc,
        const RegSet*   pFprAlloc ) :
            m_oCallConv(
                Target_::km_pMach->m_$rn,
                Target_::km_pMach->m_pGprArg,
                Target_::km_pMach->m_pFprArg,
                Target_::km_pMach->m_pVolatile ),
            RegisterAllocator(
                pwsz,
                Target_::km_pMach,
                pGprAlloc, pFprAlloc )
    {
        ResetMap();
        ResetPosn();
    } // X86X64RegisterAllocator

    ////////////////////////////////////////////////////////////
    //
    // Work Area
    //
    uint m_rgnRegPosn[t_cRegs];

    // GetPosn
    public: virtual uint GetPosn(int rx) const
        { return m_rgnRegPosn[rx % t_cRegs]; }

    // ResetPosn
    public: virtual void ResetPosn()
        { evcl_memset(m_rgnRegPosn, 0, sizeof(m_rgnRegPosn)); }

    // SetPosn
    public: virtual uint SetPosn(int rx, uint nPosn)
        { return m_rgnRegPosn[rx % t_cRegs] = nPosn; }

    ////////////////////////////////////////////////////////////
    //
    // Map for copy propagation
    //
    Operand* m_rgpRegMap[t_cRegs];

    public: virtual Operand* GetMap(int rx) const
        { return m_rgpRegMap[rx % t_cRegs]; }

    public: virtual Operand* SetMap(int rx, Operand* pSx)
        { return m_rgpRegMap[rx % t_cRegs] = pSx; }

    public: virtual void ResetMap()
        { evcl_memset(m_rgpRegMap, 0, sizeof(m_rgpRegMap)); }

    ////////////////////////////////////////////////////////////
    //
    // Fixed Intervals
    //
    LiveInterval* m_rgpFixedIntv[t_cRegs];

    public: virtual LiveInterval* GetFixed(int rx) const
        { return m_rgpFixedIntv[rx % t_cRegs]; }

    public: virtual LiveInterval* SetFixed(int rx, LiveInterval* pIntv)
        { return m_rgpFixedIntv[rx % t_cRegs] = pIntv; }

    ////////////////////////////////////////////////////////////
    //
    // For Copy Propagation
    //
    public: virtual bool CanCopy(Instruction* pInsn, uint nNth) const
    {
        switch (pInsn->GetOpcode())
        {
        case IrOp_ADD: case IrOp_SUB:
        case IrOp_LOGAND: case IrOp_LOGIOR: case IrOp_LOGXOR:
            return nNth != 0;

        case IrOp_SELECT:
            return nNth != 1;
        } // switch
        return true;
    } // CanCopy

    // MapPseudo
    //  Map pseudo register to physical register. This method is called
    //  during building live interval.
    public: virtual Register* MapPseudo(
        Instruction*    pInsn,
        Register*       pRx ) const
    {
        ASSERT(pRx->GetStorage() == Register::Storage_Pseudo);

        switch (pInsn->GetOpcode())
        {
        case IrOp_STORE:
        {
            SlotInsn* pSlot = pRx->GetDfn()->DynamicCast<SlotInsn>();
            if (NULL != pSlot) return pSlot->GetRz();
            break;
        } // store
        } // switch opcode

        return pRx;
    } // MapPseudo

    static bool is_physical(Operand* pSx)
    {
        return pSx->Is<Register>() && 
               pSx->StaticCast<Register>()->IsPhysical();
    } // is_physical

    // NeedPhysical
    bool NeedPhysical(Instruction* pInsn, uint nNth) const
    {
        switch (pInsn->GetOpcode())
        {
        case IrOp_COPY:
        case IrOp_PHICOPY:
            return ! pInsn->GetRd()->IsPhysical();

        case IrOp_ADD:
        case IrOp_SUB:
        case IrOp_LOGAND:
        case IrOp_LOGIOR:
        case IrOp_LOGXOR:
            return 0 == nNth;

        case IrOp_NTHVALUE:
        case IrOp_OPENBIND:
        case IrOp_RET:
        case IrOp_VALUES:
            return false;

        case IrOp_X86X64_CMP:
        case IrOp_X86X64_TEST:
            switch (nNth)
            {
            case 0:
                switch (pInsn->GetSy()->GetKind())
                {
                case Operand::Kind_Register:
                    if (pInsn->GetRx()->GetClass() == Register::Class_FPR)
                    {
                        // The first operand of COMISD must be register.
                        return true;
                    }
                    return ! is_physical(pInsn->GetSy());
                case Operand::Kind_Literal:
                case Operand::Kind_Integer:
                    return false;
                default:
                    CAN_NOT_HAPPEN();
                } // switch operand
            case 1:
                return ! is_physical(pInsn->GetSx());
            default:
                CAN_NOT_HAPPEN();
            } // switch nNth
        } // switch opcode

        return true;
    } // X86RegisterAllocator::NeedPysical

    // NeedPhysicalOutput
    bool NeedPhysicalOutput(Instruction* pInsn) const
    {
        ASSERT(NULL != pInsn);
        switch (pInsn->GetOpcode())
        {
        #if 0
        // Note: Output fuse increase code size.
        case IrOp_ADD:
        case IrOp_SUB:
        case IrOp_LOGAND:
        case IrOp_LOGIOR:
        case IrOp_LOGXOR:
            return ! pInsn->GetSy()->Is<Register>();
        #endif

        case IrOp_COPY:
        case IrOp_PHICOPY:
            if (! pInsn->GetSx()->Is<Register>()) return false;
            break;
        } // switch opcode
        return true;
    } // NeedPhysicalOutput

    // BuildFxiedInterval
    public: virtual void BuildFixedInterval(Function* pFun)
        { FixedIntervalBuilder::Run(this, pFun, &m_oCallConv); }

    // SizeOfSpill
    public: virtual uint SizeOfSpill(Ty ty)
    {
        #if SIZEOF_VAL == 8
            ASSERT(NULL != ty);
            return 1;
        #else
            return ty == ty_float64 ? 2 : 1;
        #endif // SIZEOF_VAL
    } // SizeOfSpill
}; // X86X64RegisterAllocator


//////////////////////////////////////////////////////////////////////
//
// FixedIntervalBuilder
//
class FixedIntervalBuilder
{
    typedef RegisterAllocator RA;

    public: static void Run(
        RA*             pPass,
        Function*       pFun,
        const CallConv* pCallConv)
    {
        FixedIntervalBuilder oBuilder(pPass, pCallConv);
        oBuilder.run(pFun);
    } // Run

    RA* m_pPass;
    uint m_nStart;
    const CallConv* m_pCallConv;

    FixedIntervalBuilder(RA* pPass, const CallConv* pCallConv) :
        m_pPass(pPass),
        m_pCallConv(pCallConv) {}

    // run
    void run(Function* pFun)
    {
        foreach (Function::EnumBBlock_Postorder, oEnum, pFun)
        {
            BBlock* pCurr = oEnum.Get();
            m_nStart = pCurr->GetFirstInsn()->GetIndex() + Posn_LiveIn;
            foreach (BBlock::EnumInsn_Reverse, oEnum, pCurr)
            {
                Instruction* pInsn = oEnum.Get();

                switch (pInsn->GetOpcode())
                {
                case IrOp_BOX:
                    process_BOX(pInsn);
                    continue;

                case IrOp_CALL:
                case IrOp_CLOSURE:
                    process_CALL(pInsn);
                    continue;

                case IrOp_MVRESTORE:
                    process_output(pInsn);
                    use_values_aux(
                        pInsn,
                        1,
                        m_pCallConv->m_pGprArgs->m_n,
                        pInsn->GetIndex() );
                    continue;

                case IrOp_OPENFINALLY:
                    continue;


                #if MACH == MACH_x86
                case IrOp_STORE:
                    process_STORE(pInsn);
                    break;
                #endif // MACH == MACH_x86

                case IrOp_VALUES:
                    process_VALUES(pInsn);
                    continue;
                } // switch opcode

                process_output(pInsn);

                if (pInsn->Is<ProjectInsn>())
                {
                    continue;
                }

                process_inputs(pInsn);
            } // for each instruction
        } // for each bblock
    } // Run

    // add_use_posn
    void add_use_posn(Int iReg, uint nPosn)
    {
        m_pPass->GetFixed(static_cast<int>(iReg))->AddUsePosn(nPosn, true);
    } // add_use_posn

    // get_intv
    LiveInterval* get_intv(int iReg)
        { return m_pPass->GetFixed(iReg); }

    // get_intv
    LiveInterval* get_intv(Register* pRx)
        { return get_intv(pRx->GetLocation()); }

    // is_cross_bblock
    static bool is_cross_bblock(Values* pVd)
    {
        BBlock* pDefBB = pVd->GetDfn()->GetBBlock();
        foreach (Values::EnumUseSite, oEnum, pVd)
        {
            if (oEnum.Get()->GetInstruction()->GetBBlock() != pDefBB)
            {
                return true;
            }
        } // for each use
        return false;
    } // is_cross_bblock

    // is_OpenFinally
    static bool is_OpenFinally(Values* pVd)
    {
        Values::EnumUseSite oEnum(pVd);
        if (oEnum.Get()->GetInstruction()->Is<OpenFinallyInsn>())
        {
            // Used by OPENFINALLY.
            return true;
        }

        return false;
    } // is_OpenFinally

    // process_input
    void process_inputs(Instruction* pInsn)
    {
        uint nPosn = pInsn->GetIndex();

        foreach (Instruction::EnumInput, oEnum, pInsn)
        {
            OperandBox* pBox = oEnum.GetBox();
            Operand* pSx = pBox->GetOperand();

            switch (pSx->GetKind())
            {
            case Operand::Kind_Register:
            {
                Register* pRx = pSx->StaticCast<Register>();
                if (pRx->IsPhysical())
                {
                    LiveInterval* pIntv = get_intv(pRx);

                    pIntv->AddRange(
                        m_nStart,
                        nPosn + Posn_InputEnd );

                    pIntv->AddUsePosn(nPosn + Posn_Input, true);
                } // if
                break;
            } // Kind_Register

            case Operand::Kind_Values:
            {
                if (! pInsn->Is<PhiInsn>())
                {
                    use_values(pInsn, pSx->StaticCast<Values>(), nPosn);
                }
                break;
            } // Kind_Values
            } // switch kind
        } // for each input
    } // process_input

    // process_output
    void process_output(Instruction* pInsn)
    {
        uint nPosn = pInsn->GetIndex();

        switch (pInsn->GetOutput()->GetKind())
        {
        case Output::Kind_Register:
        {
            Register* pRd = pInsn->GetRd();
            if (pRd->IsPhysical())
            {
                LiveInterval* pIntv = get_intv(pRd);

                pIntv->AddRange(
                    nPosn + Posn_Output,
                    nPosn + Posn_OutputEnd );

                pIntv->AddUsePosn(nPosn + Posn_Output, true);
            } // if
            break;
        } // Kind_Register

        case Output::Kind_Values:
        {
            foreach (RegSet::Enum, oEnum, m_pCallConv->m_pGprArgs)
            {
                int iReg = oEnum.Get();
                get_intv(iReg)->AddRange(
                    nPosn + Posn_Output,
                    nPosn + Posn_OutputEnd );

                add_use_posn(iReg, nPosn + Posn_Output);
            } // for i

            get_intv(m_pCallConv->m_rn)->AddRange(
                nPosn + Posn_Output,
                nPosn + Posn_OutputEnd );

            add_use_posn(m_pCallConv->m_rn, nPosn + Posn_Output);
            break;
        } // values
        } // switch kind
    } // process_output

    // process_BOX
    void process_BOX(Instruction* pBox)
    {
        process_output(pBox);
        use_volatile(pBox);

        uint nPosn = pBox->GetIndex();

        int rx = pBox->GetRx()->GetLocation();
        get_intv(rx)->AddRange(m_nStart, nPosn + Posn_InputEnd);
        add_use_posn(rx, nPosn + Posn_Input);
    } // process_BOX

    // process_CALL
    //  Simulates CALL instruction effects in reverse oder.
    //    [1] Add Output range for output registes.
    //    [2] Add Clobber range for volatile registers.
    //    [3] Add Input range for argument registers.
    void process_CALL(Instruction* pInsn)
    {
        uint nPosn = pInsn->GetIndex();

        process_output(pInsn);
        use_volatile(pInsn);

        uint nArgs;
        {
            if (pInsn->GetVy()->GetDfn()->Is<ValuesInsn>())
            {
                nArgs = pInsn->GetVy()->GetDfn()->GetOperandCount();
            }
            else
            {
                nArgs = m_pCallConv->m_pGprArgs->m_n;
            }
        } // nArgs

        foreach (RegSet::Enum, oEnum, m_pCallConv->m_pGprArgs)
        {
            if (0 == nArgs) break;

            nArgs -= 1;

            int iReg = oEnum.Get();

            html_log_format(3, L"~S:~D: ~S => set ~W~:%",
                pInsn->GetBBlock(),
                pInsn->GetIndex(),
                pInsn,
                cm_get_target()->GetPhysicalName(iReg) );

            get_intv(iReg)->AddRange(
                m_nStart,
                nPosn + Posn_InputEnd );

            add_use_posn(iReg, nPosn + Posn_Input);
        } // for i

        get_intv(m_pCallConv->m_rn)->AddRange(m_nStart, nPosn + Posn_InputEnd);
        add_use_posn(m_pCallConv->m_rn, nPosn + Posn_Input);
    } // process_CALL

    #if MACH == MACH_x86
    void process_STORE(Instruction* pInsn)
    {
        StoreInsn* pStore = pInsn->StaticCast<StoreInsn>();

        Ty opty = ty_get_pointee(pStore->m_opty);
            unless (opty == ty_int8 || opty == ty_uint8) return;

        Register* pRy = pStore->GetRy();
            if (NULL == pRy) return;
            unless (pRy->IsVirtual()) return;

        pRy->GetExtension<LiveInterval>()->m_nFlags |= Flag_ByteReg;
    } // process_STORE
    #endif // MACH == MACH_x86

    // process_VALUES
    void process_VALUES(Instruction* pInsn)
    {
        if (! is_OpenFinally(pInsn->GetVd()))
        {
            // Outputs of VALUES
            uint nPosn = pInsn->GetIndex();
            uint nEndPosn = nPosn + Posn_OutputEnd;

            if (is_cross_bblock(pInsn->GetVd()))
            {
                nEndPosn = pInsn->GetBBlock()->GetLastInsn()->
                    GetIndex() + Posn_LiveOut;
            }

            uint n = pInsn->GetOperandCount();
            foreach (RegSet::Enum, oEnum, m_pCallConv->m_pGprArgs)
            {
                if (0 == n) break;
                n -= 1;

                int iReg = oEnum.Get();

                html_log_format(3, L"~S:~D: ~S => set ~W~:%",
                    pInsn->GetBBlock(),
                    pInsn->GetIndex(),
                    pInsn,
                    cm_get_target()->GetPhysicalName(iReg) );

                get_intv(iReg)->AddRange(
                    nPosn + Posn_Output,
                    nEndPosn );

                add_use_posn(iReg, nPosn + Posn_Output);
            } // for i

            get_intv(m_pCallConv->m_rn)->AddRange(
                nPosn + Posn_Output,
                nEndPosn );

            add_use_posn(m_pCallConv->m_rn, nPosn + Posn_Output);
        } // if

        process_inputs(pInsn);
    } // process_VALUES

    // volatile registers
    void use_volatile(Instruction* pInsn)
    {
        uint nPosn = pInsn->GetIndex();

        foreach (RegSet::Enum, oEnum, m_pCallConv->m_pVolatile)
        {
            int iReg = oEnum.Get();

            get_intv(iReg)->AddRange(
                nPosn + Posn_Clobber,
                nPosn + Posn_ClobberEnd );

            add_use_posn(iReg, nPosn + Posn_Clobber);
        } // for i
    } // use_volatile

    // use_values
    void use_values(Instruction* pInsn, Values* pVx, uint nPosn)
    {
        uint n;
        {
            Instruction* pDfn = pVx->GetDfn();
            switch (pDfn->GetOpcode())
            {
            // BUGBUG: Number of values by call instruction.
            case IrOp_VALUES:
                n = pDfn->GetOperandCount();
                break;

            default:
                n = m_pCallConv->m_pGprArgs->m_n;
                break;
            } // switch opcode
        } // n

        use_values_aux(pInsn, 0, n, nPosn);
    } // use_values

    // use_values_aux
    void use_values_aux(Instruction* pInsn, uint nSkip, uint n, uint nPosn)
    {
        uint i = 0;
        foreach (RegSet::Enum, oEnum, m_pCallConv->m_pGprArgs)
        {
            if (i >= n) break;

            if (i >= nSkip)
            {
                int iReg = oEnum.Get();

                html_log_format(3, L"~S:~D: ~S => end ~W~:%",
                    pInsn->GetBBlock(),
                    pInsn->GetIndex(),
                    pInsn,
                    cm_get_target()->GetPhysicalName(iReg) );

                LiveInterval* pIntv = m_pPass->GetFixed(iReg);
                pIntv->AddRange(m_nStart, nPosn + Posn_InputEnd);
                pIntv->AddUsePosn(nPosn + Posn_Input, true);
            }

            i+= 1;
        } // for i

        get_intv(m_pCallConv->m_rn)->AddRange(
            m_nStart,
            nPosn + Posn_InputEnd );

        add_use_posn(m_pCallConv->m_rn, nPosn + Posn_Input);
    } // use_values
}; // FixedIntervalBuilder

} // Compiler

#endif //!defined(INCLUDE_arch_x86x64_compiler_cg_ra_ls_h)
