#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - x86x64 - Expand TYPEP instruction.
// cg/x86x64/x86x64_cg_typep.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_typep.cpp#12 $
//
#include "./x86x64_cg_typep.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// X86X64TypepPass::process_function
//
void
X86X64TypepPass::process_function(Function* pFun)
{
    ASSERT(NULL != pFun);

    html_log_format(2, L"<h2>Process ~S</h2>~%", pFun);

    WorkList_<Instruction> oTasks;

    foreach (Function::EnumBBlock, oEnum, pFun)
    {
        BBlock* pBBlock = oEnum.Get();
        foreach (BBlock::EnumInsn, oEnum, pBBlock)
        {
            Instruction* pInsn = oEnum.Get();

            switch (pInsn->GetOpcode())
            {
            case IrOp_TYPEP:
                oTasks.Push(pInsn);
                break;
            } // switch opcode
        } // for each insn
    } // for each bblock

    while (! oTasks.IsEmpty())
    {
        Instruction* pInsn = oTasks.Pop();
        process_TYPEP(pInsn);
    } // for each task
} // X86X64TypepPass::process_function

// Context
struct Context
{
    Bool*           m_pBd;
    Register*       m_pRx;
    Register*       m_pRclassd;

    Instruction*    m_pUser;

    PhiInsn*        m_pPhi;
    BBlock*         m_pTrue;
    BBlock*         m_pFalse;

    bool isSelect() const { return IrOp_SELECT == m_pUser->GetOpcode(); }

    BBlock* GetFalse() const { return m_pFalse; }
    BBlock* GetTrue() const  { return m_pTrue; }

    Operand* GetFalseVal() const
        { ASSERT(isSelect()); return m_pUser->GetSz(); }

    Operand* GetTrueVal() const
        { ASSERT(isSelect()); return m_pUser->GetSy(); }

    Context() :
        m_pBd(NULL),
        m_pFalse(NULL),
        m_pPhi(NULL),
        m_pRclassd(NULL),
        m_pTrue(NULL) {}
}; // Context


//////////////////////////////////////////////////////////////////////
//
// update_PHIs
//  Updates PHI operands fro TYPEP+BRANCH.
//
static void update_PHIs(
    const Context*  pContext,
    BBlock*         pTo,
    BBlock*         pFrom )
{
    BBlock* pOrig = pContext->m_pUser->GetBBlock();

    foreach (BBlock::EnumInsn, oEnum, pTo)
    {
        PhiInsn* pPhi = oEnum.Get()->DynamicCast<PhiInsn>();
            if (NULL == pPhi) break;

        Operand* pSx = pPhi->GetInput(pOrig);
        pPhi->AddInput(pFrom, pSx);
    } // for each insn
} // update_PHIs


//////////////////////////////////////////////////////////////////////
//
// emit_BranchX
//
//      TYPEP+BRANCH        TYPEP+SELECT
//
//      +-----------+       +-----------+
//      |   TYPEP   |       |  TYPEP    |
//      |   ...     |       |   ...     |
//      |  RefInsn  |       |  RefInsn  |
//      +-----------+       +-----------+
//
//      +-----------+       +-----------+
//      |  BRANCH   |       |  SELECT   |
//      +-----------+       +-----------+
//
//                          +-----------+
//                          |  PHI      |
//                          +-----------+
static void emit_BranchX(Context* pContext, bool fTrue)
{
    ASSERT(NULL != pContext->m_pBd);

    Instruction* pUser = pContext->m_pUser;

    BBlock* pCurr = pUser->GetBBlock();

    BBlock* pCont = ir_split_bblock_after(pUser->GetPrev());

    if (NULL == pContext->m_pTrue)
    {
        switch (pUser->GetOpcode())
        {
        case IrOp_BRANCH:
            pContext->m_pTrue  = pUser->GetSy()->StaticCast<Label>()->
                GetBBlock();

            pContext->m_pFalse = pUser->GetSz()->StaticCast<Label>()->
                GetBBlock();
            break;

        case IrOp_SELECT:
        {
            BBlock* pJoin = ir_split_bblock_after(pUser);

            pContext->m_pPhi = new PhiInsn(
                pUser->GetTy(),
                pUser->GetOutput() );

            pUser->SetOutput(
                NewOutput(pUser->GetTy()) );

            ir_insert_insn(pContext->m_pPhi, pJoin->GetFirstInsn());

            pContext->m_pTrue  = pJoin;
            pContext->m_pFalse = pJoin;
            break;
        } // SELECT

        default:
            CAN_NOT_HAPPEN();
        } // switch opcode
    } // if

    BBlock* pTrue;
    BBlock* pFalse;

    if (fTrue)
    {
        pTrue  = pContext->GetTrue();
        pFalse = pCont;

        if (NULL != pContext->m_pPhi)
        {
            pContext->m_pPhi->AddInput(pCurr, pContext->GetTrueVal());
        }
        else
        {
            update_PHIs(pContext, pTrue, pCurr);
        }
    }
    else
    {
        pTrue  = pCont;
        pFalse = pContext->GetFalse();

        if (NULL != pContext->m_pPhi)
        {
            pContext->m_pPhi->AddInput(pCurr, pContext->GetFalseVal());
        }
        else
        {
            update_PHIs(pContext, pFalse, pCurr);
        }
    } // if

    pCurr->AppendInsn(new BranchInsn(pContext->m_pBd, pTrue, pFalse));

    pContext->m_pBd = NULL;
} // emit_BranchX


//////////////////////////////////////////////////////////////////////
//
// emit_LoadClassD
//
static Register* emit_LoadClassD(Context* pContext, Val layout)
{
    Register* pRclassd = pContext->m_pRclassd;
        if (NULL != pRclassd) return pRclassd;

    Register* pRptr = new Register();

    ir_insert_insn(
        new SlotInsn(ty_ptr_classd, pRptr,
                        NewLiteral(layout), 
                        NewLiteral(Qclassd), 
                        pContext->m_pRx ), 
        pContext->m_pUser );

    pRclassd = new Register();

    ir_insert_insn(
        new LoadInsn(pRclassd, pRptr),
        pContext->m_pUser );

    return pContext->m_pRclassd = pRclassd;
} // emit_LoadClassD

//////////////////////////////////////////////////////////////////////
//
// emit_CmpClassD
//      SLOT (ptr classd) %r10 <= record classd %rx
//      LOAD classd %r11 <= %r10
//      CMP int32 %ef12 <= %r11 {classd}
//      x86SETCC bool  %b13  <= tttn_e %ef12
//
static void emit_CmpClassD(Context* pContext, const TypepCmd* pCmd, Tttn tttn)
{
    pContext->m_pBd = new Bool();

    Register* pRclassd = emit_LoadClassD(pContext, Qrecord);

    x86x64Flags* pEf12 = new x86x64Flags();

    ir_insert_insn(
        new x86x64CmpInsn(pEf12, pRclassd, NewLiteral(pCmd->m_operand)),
        pContext->m_pUser );

    ir_insert_insn(
        new x86x64SetCcInsn(pContext->m_pBd, tttn, pEf12),
        pContext->m_pUser );
} // emit_emit_CmpClassD


//////////////////////////////////////////////////////////////////////
//
// emit_Eq
//      TYPEP bool %b1 <= %r2 null
//    =>
//      x86CMP  int32 %ef11 <= %r10 mask
//      x86SETCC bool %b12  <= tttn_e %ef11
static void emit_Eq(Context* pContext, const TypepCmd* pCmd)
{
    pContext->m_pBd = new Bool();

    x86x64Flags* pEf12 = new x86x64Flags();
    ir_insert_insn(
        new x86x64CmpInsn(pEf12, pContext->m_pRx, NewLiteral(pCmd->m_operand)),
        pContext->m_pUser );

    ir_insert_insn(
        new x86x64SetCcInsn(pContext->m_pBd, tttn_E, pEf12),
        pContext->m_pUser );
} // emit_Eq


//////////////////////////////////////////////////////////////////////
//
// emit_EqTag
//      TYPEP bool %b1 <= %r2 type
//    =>
//      SUB      int32 %r10  <= %r2 sub
//      x86TEST  int32 %ef11 <= %r10 mask
//      x86SETCC bool  %b12  <= tttn_e %ef11
//
static void emit_EqTag(Context* pContext, const TypepCmd* pCmd)
{
    Int iSubMask = pCmd->m_operand->ToInt();
    int iSub  = static_cast<int>(iSubMask >> 8);
    int iMask = static_cast<int>(iSubMask & 255);

    pContext->m_pBd = new Bool();

    Register* pR10;

    if (0 == iSub)
    {
        pR10 = pContext->m_pRx;
    }
    else
    {
        pR10 = new Register();
        ir_insert_insn(
            new SubInsn(ty_int32, pR10,
                pContext->m_pRx, NewInteger(iSub) ),
            pContext->m_pUser );
    } // if sub

    x86x64Flags* pEf12 = new x86x64Flags();
    ir_insert_insn(
        new x86x64TestInsn(pEf12, pR10, NewInteger(iMask)),
        pContext->m_pUser );

    ir_insert_insn(
        new x86x64SetCcInsn(pContext->m_pBd, tttn_E, pEf12),
        pContext->m_pUser );
} // emit_EqTag


//////////////////////////////////////////////////////////////////////
//
// emit_Generic
//      VALUES ty %   vx <= %sx ty
//      CALL   t      %rd <= 'typep %vx
//      CMP    int32  %ef <= %rd 'nil
//      SETCC  bool   %bd <= tttn_E %ef
//
static void emit_Generic(Instruction* pTypep)
{
    Values* pVx = new Values();
    ir_insert_insn(
        new ValuesInsn(pVx, pTypep),
        pTypep );

    Register* pRd = new Register();
    ir_insert_insn(
        new CallInsn(ty_t, pRd, NewLiteral(Qtypep), pVx),
        pTypep );

    x86x64Flags* pEFlags = new x86x64Flags();
    ir_insert_insn(
        new x86x64CmpInsn(pEFlags, pRd, NewLiteral(nil)),
        pTypep );

    ir_replace_insn(
        new x86x64SetCcInsn(pTypep->GetBd(), tttn_NE, pEFlags),
        pTypep );
} // emit_Generic


//////////////////////////////////////////////////////////////////////
//
// emit_FixnumRange
//
//      SUB   uint  %r1  <= %rx <min>
//      CMP   int32 %ef2 <= %r1 <max> - <min>
//      SETCC bool  %bd  <= tttn_NC %ef2
static void emit_FixnumRange(Context* pContext, Instruction* pTypep)
{
    Ty ty = pTypep->GetLy();
    Val min = second(ty);
    Val max = third(ty);

    Register* pR1;
    if (Fixnum::Encode(0) == min)
    {
        pR1 = pContext->m_pRx;
    }
    else
    {
        pR1 = new Register();
        ir_insert_insn(
            new SubInsn(ty_uint, pR1,
                pContext->m_pRx, NewInteger(min->ToInt()) ),
            pContext->m_pUser );
    }

    x86x64Flags* pEf2 = new x86x64Flags();
        ir_insert_insn(
            new x86x64CmpInsn(pEf2,
                pR1, NewInteger(max->ToInt() - min->ToInt()) ),
            pContext->m_pUser );

    pContext->m_pBd = new Bool();
        ir_insert_insn(
            new x86x64SetCcInsn(pContext->m_pBd, tttn_NA, pEf2),
            pContext->m_pUser );
} // emit_FixnumRange


//////////////////////////////////////////////////////////////////////
//
// emit_IsRecord
//      x86TEST  int32 %ef11 <= %r10 Record::Tag
//      x86SETCC bool  %b12  <= tttn_e %ef11
//
static void emit_IsRecord(Context* pContext, const TypepCmd*)
{
    pContext->m_pBd = new Bool();

    Register* pR10 = pContext->m_pRx;

    x86x64Flags* pEf12 = new x86x64Flags();
    ir_insert_insn(
        new x86x64TestInsn(pEf12, pR10, NewInteger(Record::Tag)),
        pContext->m_pUser );

    ir_insert_insn(
        new x86x64SetCcInsn(pContext->m_pBd, tttn_NE, pEf12),
        pContext->m_pUser );
} // emit_IsRecord



//////////////////////////////////////////////////////////////////////
//
// emit_Subclassp
//  VALUES (values t) %v3 <= %rx
//  CALL class %r4 <= class-of %v3
//  VALUES (values class) %v5 <= %r4 {class}
//  CALL t %r6 <= 'subclassp %v5
//  CMP int32 %ef7 <= %r6 'nil
//  SETCC bool %bd <= tttn_NE %ef7
//
static void emit_SlowSubclassp(
    Context* pContext, const TypepCmd*, Val klass )
{

    Values* pV3 = new Values();
        ir_insert_insn(
            new ValuesInsn(pV3, pContext->m_pRx),
            pContext->m_pUser );

    Register* pR4 = new Register();
        ir_insert_insn(
            new CallInsn(ty_class, pR4, NewLiteral(Qclass_of), pV3),
            pContext->m_pUser );

    if (symbolp(klass))
    {
        Val class_name = klass;
        klass = find_class(class_name, nil, TLV(AenvironmentA));
        if (nil == klass)
        {
            warn(L"Class ~S isn't defined.", class_name);
        }
    }

    Values* pV5 = new Values();
        ir_insert_insn(
            new ValuesInsn(pV5, pR4, NewLiteral(klass)),
            pContext->m_pUser );

    Register* pR6 = new Register();
        ir_insert_insn(
            new CallInsn(ty_t, pR6, NewLiteral(Qsubclassp), pV5),
            pContext->m_pUser );

    x86x64Flags* pEf7 = new x86x64Flags();
        ir_insert_insn(
            new x86x64CmpInsn(pEf7, pR6, NewLiteral(nil)),
            pContext->m_pUser );

    pContext->m_pBd = new Bool();
        ir_insert_insn(
            new x86x64SetCcInsn(pContext->m_pBd, tttn_NE, pEf7),
            pContext->m_pUser );
} // emit_SlowSubclassp


//////////////////////////////////////////////////////////////////////
//
// emit_Subclassp
//  SLOT (ptr classd) %r1 <= {layout} classd %rx
//  LOAD classd %r2 <= %r1
//  SLOT (ptr class) %r3 <= classd class %r2
//  LOAD classd %r4 <= %r3
//  VALUES (values class) %v5 <= %r4 {class}
//  CALL t %r6 <= 'subclassp %v5
//  CMP int32 %ef7 <= %r6 'nil
//  SETCC bool %bd <= tttn_NE %ef7
//
static void emit_Subclassp(Context* pContext, const TypepCmd* pCmd, Val klass)
{

    Register* pRclassd = emit_LoadClassD(pContext, pCmd->m_operand);

    Register* pR3 = new Register();
        ir_insert_insn(
            new SlotInsn(ty_make_ptr(Qclass), pR3,
                            NewLiteral(Qclass_description),
                            NewLiteral(Qclass),
                            pRclassd ),
            pContext->m_pUser );

    Register* pR4 = new Register();
        ir_insert_insn(
            new LoadInsn(pR4, pR3),
            pContext->m_pUser );

    if (symbolp(klass)) klass = find_class(klass);

    Values* pV5 = new Values();
        ir_insert_insn(
            new ValuesInsn(pV5, pR4, NewLiteral(klass)),
            pContext->m_pUser );

    Register* pR6 = new Register();
        ir_insert_insn(
            new CallInsn(ty_t, pR6, NewLiteral(Qsubclassp), pV5),
            pContext->m_pUser );

    x86x64Flags* pEf7 = new x86x64Flags();
        ir_insert_insn(
            new x86x64CmpInsn(pEf7, pR6, NewLiteral(nil)),
            pContext->m_pUser );

    pContext->m_pBd = new Bool();
        ir_insert_insn(
            new x86x64SetCcInsn(pContext->m_pBd, tttn_NE, pEf7),
            pContext->m_pUser );
} // emit_Subclassp


static const TypepCmd k_rgoGeneric[] =
{
    { TypepCmd::Op_Generic }
}; // k_rgoGeneric

static const TypepCmd k_rgoFixnumRange[] =
{
    { TypepCmd::Op_EqTag, TYPEP_TAG_OPERAND(Fixnum) },
    { TypepCmd::Op_BranchFalse },
    { TypepCmd::Op_FixnumRange },
    { TypepCmd::Op_End }
}; // k_rgoFixnumRange


static const TypepCmd k_rgoFSC[] =
{
    { TypepCmd::Op_EqTag, TYPEP_TAG_OPERAND(Funcallable) },
    { TypepCmd::Op_BranchFalse },
    { TypepCmd::Op_Subclassp, Qnative_code_function },
    { TypepCmd::Op_End }
}; // k_rgoFSC


static const TypepCmd k_rgoSTD[] =
{
    { TypepCmd::Op_SlowSubclassp, Qinstance },
    { TypepCmd::Op_End }
}; // k_rgoSTD


static const TypepCmd k_rgoSTR[] =
{
    { TypepCmd::Op_IsRecord },
    { TypepCmd::Op_BranchFalse },
    { TypepCmd::Op_Subclassp, Qrecord },
    { TypepCmd::Op_End }
}; // k_rgoSTR


//////////////////////////////////////////////////////////////////////
//
// X86X64TypepPass::process_TYPEP
//
void X86X64TypepPass::process_TYPEP(Instruction* pTypep)
{
    html_log_format(3, L"<h3>Process ~S:~S</h3>~%",
        pTypep->GetBBlock(), pTypep );

    const TypepCmd* pCmd = get_typep_desc(pTypep);

    if (NULL == pCmd)
    {
        // There is no description in table. We use general method.
        pCmd = k_rgoGeneric;

        Val ty = pTypep->GetLy();

        if (consp(ty))
        {
            Val op = first(ty);
            if (op == Qinteger)
            {
                if (fixnump(second(ty)) && fixnump(third(ty)))
                {
                    pCmd = k_rgoFixnumRange;
                }
            }
        }
        else
        {
            Val klass = symbolp(ty) ? find_class(ty, nil) : ty;

            if (classp(klass))
            {
                Val metaclass = class_of(klass);

                if (CLASS_standard_class == metaclass)
                    { pCmd = k_rgoSTD; }
                else if (CLASS_structure_class == metaclass)
                    { pCmd = k_rgoSTR; }
                else if (CLASS_funcallable_standard_class == metaclass)
                    { pCmd = k_rgoFSC; }
            }
        } // if
    } // if

    Context oContext;
        oContext.m_pRx   = pTypep->GetRx();
        oContext.m_pUser = pTypep->GetOutput()->GetUseInsn();

    for (;;)
    {
        switch (pCmd->m_eOpcode)
        {
        case TypepCmd::Op_BranchFalse:
            emit_BranchX(&oContext, false);
            break;

        case TypepCmd::Op_BranchTrue:
            emit_BranchX(&oContext, true);
            break;

        case TypepCmd::Op_End:
            if (NULL != oContext.m_pBd)
            {
                ir_replace_all_users(oContext.m_pBd, pTypep->GetBd());
            }

            ir_remove_insn(pTypep);

            if (NULL != oContext.m_pPhi)
            {
                oContext.m_pUser->GetBBlock()->AppendInsn(
                    new JumpInsn(oContext.m_pPhi->GetBBlock()) );

                oContext.m_pPhi->AddInput(
                    oContext.m_pUser->GetBBlock(),
                    oContext.m_pUser->GetOutput() );
            } // if
            return;

        case TypepCmd::Op_FixnumRange:
            emit_FixnumRange(&oContext, pTypep);
            break;

        case TypepCmd::Op_Generic:
            emit_Generic(pTypep);
            return;

        case TypepCmd::Op_Eq:
            emit_Eq(&oContext, pCmd);
            break;

        case TypepCmd::Op_EqTag:
            emit_EqTag(&oContext, pCmd);
            break;

        case TypepCmd::Op_EqClassD:
            emit_CmpClassD(&oContext, pCmd, tttn_E);
            break;

        case TypepCmd::Op_GeClassD:
            emit_CmpClassD(&oContext, pCmd, tttn_NB);
            break;

        case TypepCmd::Op_IsRecord:
            emit_IsRecord(&oContext, pCmd);
            break;

        case TypepCmd::Op_LeClassD:
            emit_CmpClassD(&oContext, pCmd, tttn_NA);
            break;

        case TypepCmd::Op_Subclassp:
            emit_Subclassp(&oContext, pCmd, pTypep->GetLy());
            break;

        case TypepCmd::Op_SlowSubclassp:
            emit_SlowSubclassp(&oContext, pCmd, pTypep->GetLy());
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch opcode

        pCmd++;
    } // for
} // X86X64TypepPass::process_TYPEP

} // Compiler
