#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - x86x64 - instrution selection
// arch/x86x64/compiler/x8664_cg_select.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_select.cpp#35 $
//
#include "./x86x64_cg_select.h"

#include "./x86x64_cg_instruction.h"
#include "./x86x64_cg_operand.h"
#include "./x86x64_cg_target.h"

#include "../../../compiler/ir/ir_fundb.h"

#include "../kernel/x86x64_ke_frame.h"


namespace Compiler
{

static bool optimize_rest_param(Function*);

namespace
{

struct NativeType
{
    public: enum Kind
    {
        Kind_Signed,
        Kind_Float,
        Kind_Pointer,
        Kind_Unsigned,
        Kind_Void,
    }; // Kind

    Val m_name;
    Kind m_eKind;
    uint m_cBits;
}; // NativeType

static const NativeType
k_rgoNativeType[] =
{
    { ty_int,  NativeType::Kind_Signed, sizeof(Val) * 8 },
    { ty_uint, NativeType::Kind_Unsigned, sizeof(Val) * 8 },

    { ty_int8,  NativeType::Kind_Signed, 8 },
    { ty_uint8, NativeType::Kind_Unsigned, 8 },

    { ty_int16,  NativeType::Kind_Signed, 16 },
    { ty_uint16, NativeType::Kind_Unsigned, 16 },

    { ty_int32,  NativeType::Kind_Signed, 32 },
    { ty_uint32, NativeType::Kind_Unsigned, 32 },

    #if SIZEOF_VAL == 8
    { ty_int64,  NativeType::Kind_Signed, 64 },
    { ty_uint64, NativeType::Kind_Unsigned, 64 },
    #endif // SIZEOF_VAL == 8

    { ty_float32, NativeType::Kind_Float, 32 },
    { ty_float64, NativeType::Kind_Float, 64 },
}; // k_rgoNativeType

static const NativeType* get_native_type(Val name)
{
    for (
        const NativeType* p = &k_rgoNativeType[0];
        p < &k_rgoNativeType[lengthof(k_rgoNativeType)];
        p++ )
    {
        if (p->m_name == name) return p;
    } // for each entry
    return NULL;
} // get_native_type


//////////////////////////////////////////////////////////////////////
//
// Entry of Strength Reduction Table
//
struct Entry
{
    Val m_fname;
    bool (*m_pfn)(Instruction*);

    static const Entry sm_rgoEntry[];
}; // Entry


// ensure_arg1
static Operand* ensure_arg1(Instruction* p, Ty ty)
{
    ValuesInsn* pArgs = p->GetVy()->GetDfn()->DynamicCast<ValuesInsn>();
        if (NULL != pArgs) return pArgs->GetSx();

    Register* pRx = new Register();
    ir_insert_insn(new ProjectInsn(ty, pRx, p->GetVy(), 0), p);
    return pRx;
} // ensure_arg1


#if SIZEOF_VAL == 4
static bool isFixnumCompatible(Ty ty)
{
    return ty == ty_int8  || ty == ty_uint8 ||
           ty == ty_int16 || ty == ty_uint16;
} // isFixnumCompatible

static bool isIntCompatible(Ty ty)
    { return ty == ty_int || ty == ty_int32; }

static bool isUintCompatible(Ty ty)
    { return ty == ty_uint || ty == ty_uint32; }


#elif SIZEOF_VAL == 8
static bool isFixnumCompatible(Ty ty)
{
    return ty == ty_int8  || ty == ty_uint8 ||
           ty == ty_int16 || ty == ty_uint16 ||
           ty == ty_int32 || ty == ty_uint32;
} // isFixnumCompatible

static bool isIntCompatible(Ty ty)
    { return ty == ty_int || ty == ty_int64; }

static bool isUintCompatible(Ty ty)
    { return ty == ty_uint || ty == ty_uint64; }

#endif

// isFloatCompatible
//  Returns true if ty is compatible to converting to float.
static bool isFloatCompatible(Ty ty)
{
    const NativeType* pTy = get_native_type(ty);
    if (NULL == pTy) return false;
    switch (pTy->m_eKind)
    {
    case NativeType::Kind_Signed:
        return true;
    case NativeType::Kind_Unsigned:
        return pTy->m_cBits < sizeof(Val) * 8;
    case NativeType::Kind_Float:
        return true;
    } // switch native type
    return false;
} // isFloatCompatible

// <summmary>
//  Transform CHAR-CODE function call to BOX instruction.
// <code>
//   VALUES %v1 <- %sx
//   CALL %r3 <- #'char-code %v1
//   =>
//   BOX fixnum %r3 <- %sx
// </code>
// </summary>
static bool processCharCode(Instruction* pInsn)
{
    CallInsn* pCall = pInsn->StaticCast<CallInsn>();

    Ty argty = pCall->GetVy()->GetTy();

    if (ty_nth(argty, 0) != Qcharacter) return false;

    Operand* pSx = ensure_arg1(pCall, Qcharacter);

    switch (pSx->GetKind())
    {
    case Operand::Kind_Literal:
    {
        Val code = Fixnum::Encode(Character::ToCode(pCall->GetLx()));
        ir_replace_all_users(NewLiteral(code), pCall->GetRd());
        break;
    } // literal

    case Operand::Kind_Register:
    {
        Integer* pCount = NewInteger(
            Character::ShiftCount - Fixnum::TagBits );

        Register* pR2 = new Register;

        ir_insert_insn(
            new SubInsn(ty_int, pR2, pSx, NewLiteral(QQchar_min)),
            pCall );

        ir_replace_insn(
            new ShrInsn(ty_fixnum, pCall->GetRd(), pR2, pCount),
            pCall );
        break;
    } // register

    default:
        CAN_NOT_HAPPEN();
    } // switch operand

    return true;
} // processCharCode

// process_float_aux
static bool process_float_aux(
    Instruction*    pCall,
    Operand*        pSx,
    Ty              toNaty,
    Ty              toTy,
    Ty              fromNaty,
    Ty              fromTy )
{
    if (Subtypep_Yes == ty_subtypep(pSx->GetTy(), toTy))
    {
        ir_replace_all_users(pSx, pCall->GetRd());
        return true;
    }

    switch (pSx->GetKind())
    {
    case Operand::Kind_Literal:
        pSx = NewLiteral(
            funcall(Qcoerce, pSx->StaticCast<Literal>()->GetDatum(), toTy) );
        ir_replace_all_users(pSx, pCall->GetRd());
        return true;

    case Operand::Kind_Register:
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch operand

    Register* pRx;

    if (isFloatCompatible(pSx->GetTy()))
    {
         pRx = pSx->StaticCast<Register>();
    }
    else if (Subtypep_Yes == ty_subtypep(pSx->GetTy(), ty_fixnum))
    {
        pRx = new Register();
        ir_insert_insn(
            new ShrInsn(ty_int, pRx, pSx, NewInteger(Fixnum::TagBits)),
            pCall );
    }
    else if (Subtypep_Yes == ty_subtypep(pSx->GetTy(), fromTy))
    {
        pRx = new FpRegister();
        ir_insert_insn(
            new UnboxInsn(fromNaty, pRx, pSx),
            pCall );
    }
    else
    {
        return false;
    }

    Register* pFx = new FpRegister();
    ir_insert_insn(
        new x86x64ConvertInsn(toNaty, pFx, pRx),
        pCall );

    ir_replace_insn(
        new BoxInsn(toTy, pCall->GetRd(), pFx),
        pCall );

    return true;
} // process_float


// process_float
static bool process_float(Instruction* pCall)
{
    ValuesInsn* pArgs = pCall->GetVy()->GetDfn()->DynamicCast<ValuesInsn>();
        if (NULL == pArgs) return false;

    switch (pArgs->GetOperandCount())
    {
    case 1:
        return process_float_aux(pCall, pArgs->GetSx(),
            ty_float32, ty_single_float,
            ty_float64, ty_double_float );

    case 2:
        if (pArgs->GetSy()->GetTy() == Qsingle_float)
        {
            return process_float_aux(pCall, pArgs->GetSx(),
                ty_float32, ty_single_float,
                ty_float64, ty_double_float );
        }

        if (pArgs->GetSy()->GetTy() == Qdouble_float)
        {
            return process_float_aux(pCall, pArgs->GetSx(),
                ty_float64, ty_double_float,
                ty_float32, ty_single_float );
        }
        return false;
    default:
        CAN_NOT_HAPPEN();
    } // switch arity
} // process_float


//////////////////////////////////////////////////////////////////////
//
// Function Database
//
typedef FunDb_<Entry> FunDb;

const Entry Entry::sm_rgoEntry[] =
{
    // [C]
    { Qchar_code,   processCharCode },
    { Qchar_int,    processCharCode },

    // [F]
    { Qfloat,       process_float }
}; // Entry::sm_rgoEntry

} // namespace

FunDb FunDb::sm_oFunDb;

//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_function
//
void
X86X64SelectPass::process_function(Function* pFun)
{
    ASSERT(NULL != pFun);

    html_log_format(2, L"<h2>Process ~S</h2>~%", pFun);

    m_pSP = NULL;

    foreach (Function::EnumBBlock, oEnumBB, pFun)
    {
        BBlock* pCurr = oEnumBB.Get();

        BBlock::EnumInsn oEnum(pCurr);
        while (! oEnum.AtEnd())
        {
            Instruction* pInsn = oEnum.Get();
                oEnum.Next();

            process_instruction(pInsn);
        } // for each insn
    } // for each bblock

    ir_remove_useless_instructions(pFun);
} // X86X64SelectPass::process_function


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_compare
//
//      EQ bool %bd <= %sx %sy
//      ==>
//      x86-CMP int32 %rd <= %sx %sy
//      x86-SETCC bool %bd <= tttn %rd
//
void
X86X64SelectPass::process_compare(Instruction* pInsn, Tttn eTttn)
{
    x86x64Flags* pEFlags = new x86x64Flags();

    Operand* pSx = pInsn->GetSx();
    Operand* pSy = pInsn->GetSy();

    if (pSx->Is<Literal>())
    {
        pSx = pInsn->GetSy();
        pSy = pInsn->GetSx();

        switch (eTttn)
        {
        case tttn_GE: eTttn = tttn_LE; break;
        case tttn_G:  eTttn = tttn_L;  break;
        case tttn_LE: eTttn = tttn_GE; break;
        case tttn_L:  eTttn = tttn_G;  break;
            break;
        } // switch eTttn
    } // if

    x86x64CmpInsn* pCmp = new x86x64CmpInsn(pEFlags, pSx, pSy);

    ir_insert_insn(pCmp, pInsn);

    if (pSx->GetTy() == ty_float32 || pSx->GetTy() == ty_float64)
    {
        pCmp->m_opty = pSx->GetTy();

        // COMISS
        //  less than       ZF=0 CF=1
        //  greater than    ZF=0 CF=0
        //  equal           ZF=1 CF=0
        switch (eTttn)
        {
        case tttn_E:  break;                    // ZF=1
        case tttn_NE: break;                    // ZF=0

        case tttn_LE: eTttn = tttn_NA; break;   // ZF=1 or CF=1
        case tttn_G:  eTttn = tttn_A;  break;   // ZF=0 and CF=0

        case tttn_GE: eTttn = tttn_NB; break;   // CF=0
        case tttn_L:  eTttn = tttn_B;  break;   // CF=1

        default:
            warn(L"Unsupoorted float comparision.");
            break;
        } // switch eTttn
    } // if float

    Bool* pBd = pInsn->GetBd();

    ir_replace_insn(
        new x86x64SetCcInsn(pBd, eTttn, pEFlags),
        pInsn );
} // X86X64SelectPass::process_compare


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_BOUND
//      BOUND %r1 <- %r2 %r3
//  ==>
//      SLOT   %r10  <- data-vector length %r3
//      LOAD   %r11  <- %r10
//      CMP    %ef12 <- %r3 %r11
//      SETCC  %b13  <- tttn_AE %ef12
//      TRAPIF %b13  sequence-index-error %r2 %r3
//
void X86X64SelectPass::process_BOUND(Instruction* pInsn)
{
    BoundInsn* pBound = pInsn->StaticCast<BoundInsn>();

    Operand* pSIndex = pBound->GetSy();

    ir_replace_all_users(pSIndex, pBound->GetRd());

    if (pBound->HasAttr(Instruction::Attr_Nop))
    {
        ir_remove_insn(pBound);
        return;
    } // if

    Operand* pSVector = pBound->GetSx();
    Operand* pSLength;

    switch (pSVector->GetKind())
    {
    case Operand::Kind_Register:
    {
        Ty eltty = ty_t;
        Register* pR10 = new Register();
            ir_insert_insn(new SlotInsn(ty_make_ptr(eltty), pR10,
                NewLiteral(Qdata_vector), 
                NewLiteral(Qlength), 
                pSVector ), pBound );
        Register* pR11 = new Register();
            ir_insert_insn(new LoadInsn(pR11, pR10), pBound);
        pSLength = pR11;
        break;
    } // register

    case Operand::Kind_Literal:
    {
        Val len = length(pBound->GetLx());
        pSLength = NewLiteral(len);

        if (pSIndex->Is<Literal>())
        {
            // BUGBUG: We should check index before this pass.
            ir_remove_insn(pBound);
            return;
        }
        break;
    } // literal

    default:
        CAN_NOT_HAPPEN();
    } // switch kind

    Bool* pB13 = new Bool();

    if (pSIndex->Is<Literal>())
    {
        // Trap if length <= index
        x86x64Flags* pEf12 = new x86x64Flags();
            ir_insert_insn(new x86x64CmpInsn(pEf12, pSLength, pSIndex), pBound);

        ir_insert_insn(new x86x64SetCcInsn(pB13, tttn_BE, pEf12), pBound);
    }
    else
    {
        x86x64Flags* pEf12 = new x86x64Flags();
            ir_insert_insn(new x86x64CmpInsn(pEf12, pSIndex, pSLength), pBound);

        // BUGBUG: We should use B instead of AE for detecting out of bound
        // index. This allows removing CLC before RET.
        ir_insert_insn(new x86x64SetCcInsn(pB13, tttn_AE, pEf12), pBound);
    } // if

    Values* pV14 = new Values();
        ir_insert_insn(
            new ValuesInsn(pV14, pSVector, pSLength),
            pBound );

    // BUGBUG: Should we use vector-index-error for aref/1 and
    // row-major-aref.
    TrapIfInsn* pTrapIf = new TrapIfInsn(pB13, Qsequence_index_error, pV14);
        pTrapIf->SetFrame(pBound->GetFrame());

    ir_replace_insn(pTrapIf, pBound);
} // X86X64SelectPass::process_BOUND


//  [1] integer <- character == SHR %rd <- %sx 6(x86) or 5(x64)
//  [2] integer <- int{8,16} == SHL %rd <- %sx 2(x86) or 3(x64)
//  [3] integer <- int{32,64} == CALL .box-int
//  [4] integer <- int{32,64} == CALL .box-uint
static bool process_BOX_integer(BoxInsn* pBox)
{
    Operand* pSx = pBox->GetSx();

    Ty opty = pBox->m_opty;

    if (opty == ty_character)
    {
        switch (pSx->GetKind())
        {
        case Operand::Kind_Literal:
        {
            Val ch = pBox->GetLx();
            if (! characterp(ch))
            {
                warn(L"Expect character: ~S", ch);
            }
            else
            {
                Val code = Fixnum::Encode(Character::ToCode(ch));
                ir_replace_all_users(NewLiteral(code), pBox->GetRd());
            }
            break;
        } // literal

        case Operand::Kind_Register:
        {
            Register* pRx = pSx->StaticCast<Register>();
            Register* pR2 = new Register;
            ir_insert_insn(
                new SubInsn(ty_int, pR2, pRx, NewLiteral(QQchar_min)),
                pBox );

            ASSERT(Character::ShiftCount > Fixnum::TagBits);

            Integer* pCount = NewInteger(
                Character::ShiftCount - Fixnum::TagBits );

            ir_replace_insn(
                new ShrInsn(ty_fixnum, pBox->GetRd(), pR2, pCount),
                pBox );
        } // register

        default:
            warn(L"BOX character expects register or literal.");
        } // switch operand

        return true;
    } // character

    if (isFixnumCompatible(opty))
    {
        Integer* pCount = NewInteger(Fixnum::TagBits);
        ir_replace_insn(
            new ShlInsn(ty_fixnum, pBox->GetRd(), pSx, pCount),
            pBox );
        return true;
    } // fixnum

    if (isIntCompatible(opty))
    {
        Values* pVy = new Values;

        ir_insert_insn(new ValuesInsn(pVy, pSx), pBox);

        ir_replace_insn(
            new CallInsn(ty_integer, pBox->GetRd(),
                            NewLiteral(QDbox_int), pVy ),
            pBox );

        return true;
    } // int{32,64}

    if (isUintCompatible(opty))
    {
        Values* pVy = new Values;

        ir_insert_insn(new ValuesInsn(pVy, pSx), pBox);

        ir_replace_insn(
            new CallInsn(ty_integer, pBox->GetRd(),
                            NewLiteral(QDbox_uint), pVy ),
            pBox );

        return true;
    } // uint{32,64}

    return false;
} // process_BOX_integer


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_BOX
void X86X64SelectPass::process_BOX(Instruction* pInsn)
{
    BoxInsn* pBox = pInsn->StaticCast<BoxInsn>();

    Ty ty = pBox->GetTy();

    if (Subtypep_Yes == ty_subtypep(ty, ty_integer))
    {
        if (process_BOX_integer(pBox)) return;
    }
    else if (ty_single_float == ty)
    {
        Ty opty = pBox->m_opty;
        if (ty_float32 == opty) return;
    }
    else if (ty_double_float == ty)
    {
        Ty opty = pBox->m_opty;
        if (ty_float64 == opty) return;
    }

    warn(L"Unsupport BOX from ~S to ~S.", pBox->m_opty, ty);
} // X86X64SelectPass::process_BOX


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_CALL
//
void X86X64SelectPass::process_CALL(Instruction* pInsn)
{
    CallInsn* pCall = pInsn->StaticCast<CallInsn>();
    unless (pCall->IsNotInline())
    {
        Literal* pCallee = pCall->GetSx()->DynamicCast<Literal>();
        if (NULL != pCallee)
        {
            const Entry* p = FunDb::Get(pCallee->GetDatum());
            if (NULL != p)
            {
                // FIXME 2007-03-20: RUNTIMECAST elimination dosn't update
                // type of VALUES instruction. So, we update type of
                // VALUES instruction here.
                pCall->GetVy()->GetDfn()->UpdateTy();
                if (p->m_pfn(pCall)) return;
            }
        }
    } // inline

    Register* pRd = pCall->GetRd();
    if (NULL != pRd)
    {
        Register* pRx = new Physical($r0, pRd->GetVar());
        pCall->SetOutput(pRx);
        ir_insert_insn(new CopyInsn(pRd, pRx), pCall->GetNext());
    }
} // X86X64SelectPass::process_CALL


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::insertActivateFrame
//  Emits STORE TCB.m_fp %frame for the last OPENxxx instruction.
//
//
void X86X64SelectPass::insertActivateFrame(
    Frame*              pFrame,
    Instruction*        pNext )
{
    // Set Thread.m_fp
    {
        Register* pR1;
        if (pNext->Is<FrameInsn>() &&
            pNext->GetSx() == pFrame &&
            pNext->GetIy() == 0 )
        {
            // FRAME follows OPENBLOCK.
            pR1 = pNext->GetRd();
            pNext = pNext->GetNext();
        }
        else
        {
            pR1 = new Register();
            ir_insert_insn(
                new FrameInsn(ty_ptr_int, pR1, pFrame, 0),
                pNext );
        }

        Pseudo* pQ0 = new Pseudo();
        ir_insert_insn(new x86x64TcbInsn(ty_ptr_int, pQ0,
                offsetof(Kernel::Thread, m_fp) ),
            pNext );
        ir_insert_insn(new StoreInsn(pQ0, pR1), pNext);
    } // set Thread.m_fp
} // X86X64SelectPass::insertActivateFrame


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::insertOpenFrame
//
void X86X64SelectPass::insertOpenFrame(
    Kernel::Frame::Type eFrame,
    Frame*              pFrame,
    Instruction*        pNext )
{
    // Save Frame.m_pOuter
    {
        Pseudo* pQ0 = new Pseudo();
        ir_insert_insn(new x86x64TcbInsn(ty_ptr_int, pQ0,
                offsetof(Kernel::Thread, m_fp) ),
            pNext );

        Register* pR1 = new Register();
        ir_insert_insn(new LoadInsn(pR1, pQ0), pNext);

        Pseudo* pR2 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_int, pR2,
                pFrame, offsetof(Kernel::Frame, m_pOuter) ),
            pNext );

        ir_insert_insn(new StoreInsn(pR2, pR1), pNext);
    }

    // Set Frame.m_eType
    {
        Pseudo * pQ3 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_int, pQ3,
                pFrame, offsetof(Kernel::Frame, m_type) ),
            pNext );

        ir_insert_insn(
            new StoreInsn(pQ3, NewInteger(eFrame)),
            pNext );
    }
} // X86X64SelectPass::insertOpenFrame


// X86X64SelectPass::get_sp
Register* X86X64SelectPass::get_sp(Function* pFun)
{
    if (NULL != m_pSP) return m_pSP;

    Instruction* pPrologue = pFun->GetPrologueInsn();
    Instruction* pLast = pPrologue->GetBBlock()->GetLastInsn();

    for (
        Instruction* pRunner = pPrologue;
        pRunner != pLast;
        pRunner = pRunner->GetNext() )
    {
        if (pRunner->Is<FrameRefInsn>())
        {
            return m_pSP = pRunner->GetRd();
        }
    } // for each insn

    Physical* pSP = new Physical(cm_get_target()->GetSP());
    ir_insert_insn(
        new FrameRefInsn(pSP, pFun),
        pPrologue->GetNext() );
    return m_pSP = pSP;
} // get_sp


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::insertXferFame
//
// Called by:
//  OPENBLOCK
//  OPENCATCH
//
//      ... open frame ...
//
//      // save SP
//      FRAME (ptr fixnum) %q0 <- %frame offsetof(XferFrame, m_sp)
//      STORE %q0 %sp
//
//      // Set frame ownre
//      FRAME (ptr function) %q1 <- %frame offsetof(XferFrame, m_fn)
//      STORE %q1 fun
//
//      // Set exit point
//      FRAME (ptr fixnum) %q2 <- %frame offsetof(XferFrame, m_ip)
//      STORE %q2 label
//
void X86X64SelectPass::insertXferFame(
    Kernel::Frame::Type eFrame,
    Frame*              pFrame,
    Instruction*        pNext )
{
    insertOpenFrame(eFrame, pFrame, pNext);

    Function* pFun = pNext->GetBBlock()->GetFunction();

    // Save SP
    {
        Pseudo* pQ0 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_int, pQ0,
                pFrame, offsetof(XferFrame, m_sp) ),
            pNext );
        ir_insert_insn(new StoreInsn(pQ0, get_sp(pFun)), pNext);
    }

    // Set frame owner function
    {
        Pseudo* pQ1 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_function, pQ1,
                pFrame, offsetof(XferFrame, m_fn) ),
            pNext );
        if (pFun->IsClosure())
        {
            warn(L"Can't use closure ~S in XFER frame.", pFun->GetName());
        }
        ir_insert_insn(new StoreInsn(pQ1, new FunLit(pFun)), pNext);
    }

    // Set Exit Point
    {
        Pseudo* pQ2 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_fixnum, pQ2,
                pFrame, offsetof(XferFrame, m_ip) ),
            pNext );
        ir_insert_insn(new StoreInsn(pQ2, pFrame->GetDfn()->GetSy()), pNext);
    }
} // X86X64SelectPass::insertXferFame


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_CLOSE
//
//  Inserts pop frame sequences if this CLOSE instruction is the last
//  CLOSE instruction of consecutive CLOSE instructions.
//      FRAME (ptr fixnum) %q0 <- %frame m_pOuter
//      LOAD  fixnum       %r1 <- %q0
//      TCB   (ptr fixnum) %q2 <- m_fp
//      STORE                %q2 %r1
//
void X86X64SelectPass::process_CLOSE(Instruction* pClose)
{
    Frame* pFrame = pClose->GetSx()->StaticCast<Frame>();

    if (pFrame->GetKind() == Qlet)
    {
        // Restore bindings
        OpenBindInsn* pOpen = pFrame->GetDfn()->StaticCast<OpenBindInsn>();

        int ofs = sizeof(BindFrame) + offsetof(BindFrame::Entry, m_value);
        foreach (OpenBindInsn::EnumInput, oEnum, pOpen)
        {
            Val cell = oEnum.GetBox()->GetCell();

            Pseudo* pQ0 = new Pseudo();
            ir_insert_insn(
                new FrameInsn(ty_ptr_t, pQ0, pFrame, ofs),
                pClose );

            Register* pR1 = new Register();
            ir_insert_insn(new LoadInsn(pR1, pQ0), pClose);

            Pseudo* pQ2 = new Pseudo();
            if (tlv_record_p(cell))
            {
                ir_insert_insn(new TlvInsn(ty_ptr_t, pQ2, cell), pClose);
            }
            else if (value_cell_p(cell))
            {
                ir_insert_insn(
                    new SlotInsn(ty_ptr_t, pQ2,
                        NewLiteral(Qvalue_cell),
                        NewLiteral(Qvalue),
                        NewLiteral(cell) ),
                    pClose );
            }
            else
            {
                CAN_NOT_HAPPEN();
            }

            ir_insert_insn(new StoreInsn(pQ2, pR1), pClose);

            ofs += sizeof(BindFrame::Entry);
        } // for each bind
    } // if

    if (! pClose->GetNext()->Is<CloseInsn>())
    {
        Pseudo* pQ0 = new Pseudo();
        ir_insert_insn(
            new FrameInsn(ty_ptr_int, pQ0,
                pFrame, offsetof(Kernel::Frame, m_pOuter) ),
            pClose );

        Register* pR1 = new Register();
        ir_insert_insn(new LoadInsn(pR1, pQ0), pClose);

        Pseudo* pQ2 = new Pseudo();
        ir_insert_insn(
            new x86x64TcbInsn(ty_ptr_int, pQ2,
                offsetof(Kernel::Thread, m_fp) ),
            pClose );
        ir_insert_insn(new StoreInsn(pQ2, pR1), pClose);
    } // if
} // // X86X64SelectPass::process_CLOSE


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_CLOSURE
//
//      mov tcb.m_fn <= template
//      call .make-closure
//
void X86X64SelectPass::process_CLOSURE(Instruction* pClosure)
{
    Instruction* pRef = pClosure->GetVy()->GetDfn();

    if (! pRef->Is<ValuesInsn>())
    {
        pRef = pClosure;
    }

    Pseudo* pQ0 = new Pseudo();
    ir_insert_insn(
        new x86x64TcbInsn(ty_ptr_function, pQ0,
                offsetof(Kernel::Thread, m_fn) ),
        pRef );

    ir_insert_insn(new StoreInsn(pQ0, pClosure->GetSx()), pRef);

    Register* pRd = pClosure->GetRd();
    Physical* pR0 = new Physical($r0, pRd->GetVar());

    Instruction* pCall = ir_replace_insn(
        new CallInsn(ty_function, pR0,
            NewLiteral(QDmake_closure), pClosure->GetVy() ),
        pClosure );

    ir_insert_insn(new CopyInsn(pRd, pR0), pCall->GetNext());
} // X86X64SelectPass::process_CLOSURE


//////////////////////////////////////////////////////////////////////
//
// k_rgnShift
//  log_2(n) table
//
static const int k_rgnShift[] =
{       // byte bit
    0,  // 0    0 
    0,  // 1    8
    1,  // 2    16
    0,  // 3    24
    2,  // 4    32
    0,  // 5    40
    0,  // 6    48
    0,  // 7    56
    3,  // 8    64
}; // k_rgnShift


// update_elt_index
static void update_elt_index(Instruction* pElt, uint cbElt)
{
    int iShift = k_rgnShift[cbElt];

    Operand* pSy = pElt->GetSy();
    unless (pSy->GetTy() == ty_int || pSy->GetTy() == ty_uint)
    {
        iShift -= k_rgnShift[sizeof(Val)];
    }

    if (0 == iShift) return;

    switch (pSy->GetKind())
    {
    case Register::Kind_Register:
    {
        Register* pRy = new Register();

        if (iShift < 0)
        {
            ir_insert_insn(
                new ShrInsn(ty_int, pRy, pSy, NewInteger(-iShift)),
                pElt );
        }
        else
        {
            ir_insert_insn(
                new ShlInsn(ty_int, pRy, pSy, NewInteger(iShift)),
                pElt );
        }

        pSy = pRy;
        break;
    } // register

    case Register::Kind_Literal:
    {
        int i = static_cast<int>(Fixnum::Decode_(pElt->GetLy()));
        if (iShift < 0)
        {
            pSy = NewInteger(i >> -iShift);
        }
        else
        {
            pSy = NewInteger(i << iShift);
        }
        break;
    } // Literal

    case Register::Kind_Integer:
    {
        int i = static_cast<int>(pElt->GetIy());
        if (iShift < 0)
        {
            pSy = NewInteger(i >> -iShift);
        }
        else
        {
            pSy = NewInteger(i << iShift);
        }
        break;
    } // Integer

    default:
        warn(L"BOX: Unexpected index operand.");
        break;
    } // switch operand

    pElt->GetOperandBox(1)->Replace(pSy);
} // update_elt_index


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_ELT
//
void X86X64SelectPass::process_ELT(Instruction* pInsn)
{
    EltInsn* pElt = pInsn->StaticCast<EltInsn>();

    Register* pRd = pElt->GetRd();

    if (NULL != pRd->GetSingleUser())
        { pRd->SetStorage(Register::Storage_Pseudo); }

    Register* pRx = pElt->GetRx();
    if (NULL == pRx)
    {
        pRx = new Register();
        ir_insert_insn(new CopyInsn(pRx, pElt->GetSx()), pElt);
    }

    bool fScale = true;
    uint ofs = 0;
    {
        Ty opdty = pRx->GetTy();
        if (ty_array_object == opdty)
        {
            ofs = offsetof(Array, mv_dimension) - Array::Tag;
        }
        else if (Subtypep_Yes == ty_subtypep(opdty, ty_vector))
        {
            ofs = offsetof(SimpleVector, mv_element) - SimpleVector::Tag;
        }
        else if (opdty == ty_bignum)
        {
            ofs = offsetof(Bignum, m_rgBigit) - Bignum::Tag;
        }
        else if (Subtypep_Yes == ty_subtypep(opdty, ty_function))
        {
            update_elt_index(pElt, 8 / 8);
            ofs = sizeof(NativeCodeFunction) - NativeCodeFunction::Tag;
            fScale = false;
        }
        else
        {
            style_warn(L"Unsupported vector type ~S.", opdty);
            ofs = offsetof(SimpleVector, mv_element) - SimpleVector::Tag;
        }
    } // ofs

    Ty ptrty = pElt->GetTy();

    // Index scaling
    if (fScale)
    {
        Ty elty = ty_get_pointee(ptrty);

        if (elty == ty_float64 || elty == ty_int64 || elty == ty_uint64)
        {
            update_elt_index(pElt, 64 / 8);
        }
        else if (elty == ty_float32 || elty == ty_int32 || elty == ty_uint32)
        {
            update_elt_index(pElt, 32 / 8);
        }
        else if (elty == ty_int16 || elty == ty_uint16)
        {
            update_elt_index(pElt, 16 / 8);
        }
        else if (elty == ty_int8 || elty == ty_uint8)
        {
            update_elt_index(pElt, 8 / 8);
        }
    } // if fScale

    Instruction* pLea;

    switch (pElt->GetSy()->GetKind())
    {
    case Operand::Kind_Register:
        pLea = ir_replace_insn(
            new x86x64Lea3Insn(pRd, pRx, ofs, pElt->GetRy()),
            pElt );
        break;

    case Operand::Kind_Literal:
        ofs += static_cast<int>(pElt->GetLy()->ToInt());
        goto base_disp;

    case Operand::Kind_Integer:
        ofs += static_cast<int>(pElt->GetIy());
        goto base_disp;

    base_disp:
        pLea = ir_replace_insn(
            new x86x64Lea2Insn(pRd, pRx, ofs),
            pElt );
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch operand

    pLea->SetTy(ptrty);
} // X86X64SelectPass::process_ELT


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_OPENBIND
//
//      ... open frame ...
//
//      // save value of TLV and name
//      TLV   (ptr t) %q0 <- {tlvrec}
//      LOAD  t       %r1 <- %q0
//      FRAME (ptr t) %q2 <- {frame} {ofs}
//      STORE                %q2 %r1
//      FRAME (ptr t) %q3 <- {frame} {ofs}
//      STORE                %q3 {tlvname}
//
//      // save value of special variable
//      SLOT  (ptr t) %q3 <- value-cell value {vcell}
//      LOAD  t       %r4 <- %q3
//      FRAME (ptr t) %q5 <- {frame} {ofs}
//      STORE                %q5 %r4
//      FRAME (ptr t) %q3 <- {frame} {ofs}
//      STORE                %q3 {vcell}
//
//      ... activate frame ... (if the last OPENxxxx)
//
void X86X64SelectPass::process_OPENBIND(Instruction* pInsn)
{
    OpenBindInsn* pOpen = pInsn->StaticCast<OpenBindInsn>();

    Instruction* pNext = pOpen->GetNext();

    Frame* pFrame = pOpen->GetOutput()->StaticCast<Frame>();

    insertOpenFrame(BindFrame::Type_Bind, pFrame, pNext);

    {
        uint cBinds = pOpen->GetOperandCount();
        Pseudo* pQ0 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_int, pQ0,
                    pFrame, offsetof(BindFrame, m_cbFrame)),
            pNext );
        ir_insert_insn(new StoreInsn(pQ0,
                NewInteger(BindFrame::ComputeSize(cBinds)) ),
            pNext );
    }

    int ofs = sizeof(BindFrame);
    foreach (OpenBindInsn::EnumInput, oEnum, pOpen)
    {
        Operand* pSy = oEnum.Get();

        Val cell = oEnum.GetBox()->GetCell();

        Register* pR0 = new Register();

        if (tlv_record_p(cell))
        {
            pR0->SetStorage(Register::Storage_Pseudo);
            ir_insert_insn(new TlvInsn(ty_ptr_t, pR0, cell), pNext);
        }
        else if (value_cell_p(cell))
        {
            ir_insert_insn(
                new SlotInsn(ty_ptr_t, pR0,
                    NewLiteral(Qvalue_cell),
                    NewLiteral(Qvalue),
                    NewLiteral(cell) ),
                pNext);
        }
        else
        {
            CAN_NOT_HAPPEN();
        } // if

        // Save current value
        {
            Register* pR1 = new Register();
            ir_insert_insn(new LoadInsn(pR1, pR0), pNext);

            Register* pR2 = new Register();
                pR2->SetStorage(Register::Storage_Pseudo);

            ir_insert_insn(new FrameInsn(ty_ptr_t, pR2,
                        pFrame,
                        ofs + offsetof(BindFrame::Entry, m_value) ),
                pNext );

            ir_insert_insn(new StoreInsn(pR2, pR1), pNext);
        } // save current value

        // Set new value
        ir_insert_insn(new StoreInsn(pR0, pSy), pNext);

        // Set name
        {
            Register* pR3 = new Register();
                pR3->SetStorage(Register::Storage_Pseudo);

            ir_insert_insn(new FrameInsn(ty_ptr_t, pR3,
                        pFrame,
                        ofs + offsetof(BindFrame::Entry, m_name) ),
                pNext );

            if (tlv_record_p(cell))
            {
                ir_insert_insn(new StoreInsn(pR3, new TlvName(cell)), pNext);
            }
            else if (value_cell_p(cell))
            {
                ir_insert_insn(new StoreInsn(pR3, NewLiteral(cell)), pNext);
            }
            else
            {
                CAN_NOT_HAPPEN();
            } // if
        } // set name

        ofs += sizeof(BindFrame::Entry);
    } // for each bind

    insertActivateFrame(pFrame, pNext);
} // X86X64SelectPass::process_OPENBIND


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_OPENBLOCK
//
//      ... open xfer frame ...
//
//      // set block name
//      FRAME (ptr symbol) %q0 <- %frame offsetof(BlockFrame, m_name)
//      STORE %q0 '{name}
//
//      ... activate frame ... (if the last OPENxxxx)
//
void X86X64SelectPass::process_OPENBLOCK(Instruction* pInsn)
{
    OpenBlockInsn* pOpen = pInsn->StaticCast<OpenBlockInsn>();

    Instruction* pNext = pOpen->GetNext();

    Frame* pFrame = pOpen->GetOutput()->StaticCast<Frame>();

    insertXferFame(BlockFrame::Type_Block, pFrame, pNext);

    // set block name
    {
        Pseudo* pQ0 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_symbol, pQ0, pFrame,
                offsetof(BlockFrame, m_name) ),
            pNext );
        ir_insert_insn(new StoreInsn(pQ0, pOpen->GetSx()), pNext);
    }

    insertActivateFrame(pFrame, pNext);
} // X86X64SelectPass::process_OPENBLOCK


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_OPENCATCH
//
//      ... open xfer frame ...
//
//      // set catch name
//      FRAME (ptr symbol) %q0 <- %frame offsetof(CatchFrame, m_name)
//      STORE %q0 '{name}
//
//      ... activate frame ... (if the last OPENxxxx)
//
void X86X64SelectPass::process_OPENCATCH(Instruction* pInsn)
{
    OpenCatchInsn* pOpen = pInsn->StaticCast<OpenCatchInsn>();

    Instruction* pNext = pOpen->GetNext();

    Frame* pFrame = pOpen->GetOutput()->StaticCast<Frame>();

    insertXferFame(CatchFrame::Type_Catch, pFrame, pNext);

    // set catch tag
    {
        Pseudo* pQ0 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_t, pQ0, pFrame,
                offsetof(CatchFrame, m_name) ),
            pNext );
        ir_insert_insn(new StoreInsn(pQ0, pOpen->GetSx()), pNext);
    }

    insertActivateFrame(pFrame, pNext);
} // X86X64SelectPass::process_OPENCATCH


void X86X64SelectPass::process_OPENSIMPLE(Instruction* pI)
{
    OpenSimpleI* pOpen = pI->StaticCast<OpenSimpleI>();

    Instruction* pNext = pOpen->GetNext();

    Frame* pFrame = pOpen->GetOutput()->StaticCast<Frame>();

    insertOpenFrame(GcDisableFrame::Type_GcDisable, pFrame, pNext);

    insertActivateFrame(pFrame, pNext);
} // X86X64SelectPass::process_OPENSIMPLE


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_OPENFINALLY
//
//  [1] Open frame
//  [2] Save SP
//  [3] Set finally procedure
//  [4] Set number of arguments
//  [5] Set arguments
//  [6] Activate frame if the last OPENxxxx
//
void X86X64SelectPass::process_OPENFINALLY(Instruction* pInsn)
{
    OpenFinallyInsn* pOpen = pInsn->StaticCast<OpenFinallyInsn>();

    Instruction* pNext = pOpen->GetNext();

    Frame* pFrame = pOpen->GetOutput()->StaticCast<Frame>();

    // [1] Open frame
    insertOpenFrame(FinallyFrame::Type_Finally, pFrame, pNext);


    // [2] Save SP
    {
        Function* pFun = pNext->GetBBlock()->GetFunction();

        Pseudo* pQ0 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_int, pQ0,
                pFrame, offsetof(FinallyFrame, m_sp) ),
            pNext );
        ir_insert_insn(new StoreInsn(pQ0, get_sp(pFun)), pNext);
    }

    // [3] Set finally procedure
    {
        Pseudo* pQ1 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_function, pQ1,
                pFrame, offsetof(FinallyFrame, m_fn) ),
            pNext );
        ir_insert_insn(new StoreInsn(pQ1, pOpen->GetSx()), pNext);
    }

    {
        ValuesInsn* pArgs = pOpen->GetVy()->GetDfn()->
            StaticCast<ValuesInsn>();

        // [4] Set number of arguments
        {
            Pseudo* pQ2 = new Pseudo();
            ir_insert_insn(new FrameInsn(ty_ptr_fixnum, pQ2,
                    pFrame, offsetof(FinallyFrame, m_n) ),
                pNext );
            ir_insert_insn(
                new StoreInsn(pQ2,
                    NewLiteral(Fixnum::Encode(pArgs->GetOperandCount())) ),
                pNext );
        }

        // [5] Set arguments
        int ofs = offsetof(FinallyFrame, mv_arg);
        foreach (ValuesInsn::EnumInput, oEnum, pArgs)
        {
            Pseudo* pQ3 = new Pseudo();
            ir_insert_insn(new FrameInsn(ty_ptr_t, pQ3,pFrame, ofs), pNext);
            ir_insert_insn(new StoreInsn(pQ3, oEnum.Get()), pNext);
            ofs += sizeof(Val);
        } // for each input
    }

    // [6] Activate frame
    insertActivateFrame(pFrame, pNext);
} // X86X64SelectPass::process_OPENFINALLY


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_OPENTAGBODY
//
//  [1] Open frame
//  [2] Save SP
//  [3] Set frame owner
//  [4] Set number of tags
//  [5] Set arguments
//  [6] Activate frame if the last OPENxxxx
//
void X86X64SelectPass::process_OPENTAGBODY(Instruction* pInsn)
{
    OpenTagbodyInsn* pOpen = pInsn->StaticCast<OpenTagbodyInsn>();

    Instruction* pNext = pOpen->GetNext();

    Frame* pFrame = pOpen->GetOutput()->StaticCast<Frame>();

    // [1] Open frame
    insertOpenFrame(TagbodyFrame::Type_Tagbody, pFrame, pNext);

    Function* pFun = pNext->GetBBlock()->GetFunction();

    // [2] Save SP
    {
        Pseudo* pQ0 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_int, pQ0,
                pFrame, offsetof(TagbodyFrame, m_sp) ),
            pNext );
        ir_insert_insn(new StoreInsn(pQ0, get_sp(pFun)), pNext);
    }

    // [3] Set frame owner
    {
        Pseudo* pQ1 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_function, pQ1,
                pFrame, offsetof(TagbodyFrame, m_fn) ),
            pNext );
        if (pFun->IsClosure())
        {
            warn(L"Can't use closure ~S in TAGBODY frame.", pFun->GetName());
        }
        ir_insert_insn(new StoreInsn(pQ1, new FunLit(pFun)), pNext);
    }

    // [4] Set number of tags
    {
        Pseudo* pQ2 = new Pseudo();
        ir_insert_insn(new FrameInsn(ty_ptr_int, pQ2,
                pFrame, offsetof(TagbodyFrame, m_n) ),
            pNext );
        ir_insert_insn(
            new StoreInsn(pQ2,
                NewLiteral(pFrame->GetCount()) ),
            pNext );
    }

    // [5] Set tags
    {
        int ofs = offsetof(TagbodyFrame, m_rgoTag);
        foreach (Frame::EnumUseSite, oEnum, pFrame)
        {
            TagInsn* pTagInsn = oEnum.Get()->GetInstruction()->
                DynamicCast<TagInsn>();

            if (NULL == pTagInsn) continue;

            Pseudo* pQ3 = new Pseudo();
            ir_insert_insn(
                new FrameInsn(ty_ptr_fixnum, pQ3,pFrame, ofs),
                pNext );

            ir_insert_insn(new StoreInsn(pQ3, pTagInsn->GetSy()), pNext);

            ofs += sizeof(Val);
        } // for each input
    }

    // [6] Activate frame
    insertActivateFrame(pFrame, pNext);
} // X86X64SelectPass::process_OPENTAGBODY


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_KEYSUPPLIED
//
void
X86X64SelectPass::process_KEYSUPPLIED(Instruction* pKeySupplied)
{
    ParseKeysInsn* pParseKeys = pKeySupplied->GetRx()->
        GetDfn()->StaticCast<ProjectInsn>()->GetVx()->
        GetDfn()->StaticCast<ParseKeysInsn>();

    Int nNth = -1;
    {
        Val keys = pParseKeys->GetLz();
        Val key = pKeySupplied->GetLy();
        Int cKeys = Fixnum::Decode_(length(keys));
        for (Int nIndex = 0; nIndex < cKeys; nIndex++)
        {
            if (svref(keys, nIndex) == key)
            {
                nNth = nIndex;
                break;
            }
        } // for
        ASSERT(nNth >= 0);
    } // nNth

    Register* pRbits = pKeySupplied->GetRx();

    x86x64Flags* pEFlags = new x86x64Flags();
    ir_insert_insn(
        new x86x64TestInsn(pEFlags, pRbits, NewLiteral(1 << nNth)),
        pKeySupplied );

    Bool* pBd = pKeySupplied->GetBd();
    ir_replace_insn(
        new x86x64SetCcInsn(pBd, tttn_NE, pEFlags),
        pKeySupplied );
} // X86X64SelectPass::process_KEYSUPPLIED


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_KEYVAL
//
void
X86X64SelectPass::process_KEYVAL(Instruction* pKeyVal)
{
    ParseKeysInsn* pParseKeys = pKeyVal->GetVx()->GetDfn()->
        StaticCast<ParseKeysInsn>();

    int nNth = -1;
    {
        Val keys = pParseKeys->GetLz();
        Val key = pKeyVal->GetLy();
        int cKeys = static_cast<int>(Fixnum::Decode_(length(keys)));
        for (int nIndex = 0; nIndex < cKeys; nIndex++)
        {
            if (svref(keys, nIndex) == key)
            {
                nNth = nIndex;
                break;
            }
        } // for
        ASSERT(nNth >= 0);
    } // nNth

    ir_replace_insn(
        new ProjectInsn(pKeyVal->GetTy(), pKeyVal->GetRd(),
            pKeyVal->GetVx(), nNth + 1 ),
        pKeyVal );
} // X86X64SelectPass::process_KEYVAL


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_SHR
//
void X86X64SelectPass::process_SHR(Instruction* pShr)
{
    Ty ty = pShr->GetTy();
    const NativeType* pTy = get_native_type(ty);
    if (NULL == pTy)
    {
        Register* pRd = pShr->GetRd();
        Register* pRx = new Register();

        pShr->SetOutput(pRx);
        pShr->SetTy(ty_int);

        ir_insert_insn(
            new LogAndInsn(ty, pRd, pRx, NewInteger(~Fixnum::TagMask)),
            pShr->GetNext() );
    } // if
} // X86X64SelectPass::process_SHR


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_SLOT
//
//  Owner:
//      VARDEF  stack-cell %r1 <= var %r2
//      SLOT    (ptr t)    %r3 <= stack-cell value %r1
//
//
//      VARREF  literal-cell %r1 <= %r2 %q3
//      SLOT    (ptr t)      %r4 <= literal-cell value %r1
//     ==>
//      VARSLOT (ptr t)      %r4 <= %r2 %q3
//      (VARREF instruction will be removed.)
//
void X86X64SelectPass::process_SLOT(Instruction* pSlot)
{
    Register* pRd = pSlot->GetRd();

    Val klass = pSlot->GetLx();

    if (Qc6_literal_cell == klass || Qc6_stack_cell == klass)
    {
        Instruction* pInsn = pSlot->GetRz()->GetDfn();

        if (pInsn->Is<LoadInsn>())
        {
            pInsn = pInsn->GetRx()->GetDfn();
        }

        if (pInsn->Is<VarRefInsn>())
        {
            VarRefInsn* pVarRef = pInsn->StaticCast<VarRefInsn>();

            ir_replace_insn(
                new VarSlotInsn(pSlot->GetTy(), pRd,
                    pVarRef->GetRx(), pVarRef->GetRy() ),
                pSlot );
        } // if
    }
    else if (Qsymbol == klass)
    {
        if (pSlot->GetSz()->Is<Register>())
        {
            const int align = Symbol::Align - 1;

            Register* pRx = new Register();
            ir_insert_insn(
                new LogAndInsn(ty_int, pRx,
                    pSlot->GetRz(), NewInteger(~align) ),
                pSlot );
            pSlot->GetOperandBox(2)->Replace(pRx);
        }
    }
} // X86X64SelectPass::process_SLOT


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_UNBOX
//
void X86X64SelectPass::process_UNBOX(Instruction* pUnbox)
{
    const NativeType* pTy = get_native_type(pUnbox->GetTy());
    if (NULL == pTy)
    {
        warn(L"UNBOX: Unsupported type ~S.", pUnbox->GetTy());
        return;
    }

    Operand* pSx = pUnbox->GetSx();

    switch (pTy->m_eKind)
    {
    case NativeType::Kind_Signed:
    case NativeType::Kind_Unsigned:
        switch (pSx->GetKind())
        {
        case Operand::Kind_Literal:
        {
            Val x = pSx->StaticCast<Literal>()->GetDatum();
            Int iVal;
            if (fixnump(x))
            {
                iVal = Fixnum::Decode_(x);
            }
            else if (bignump(x))
            {
                iVal = x->Decode<Bignum>()->m_rgBigit[0];
            }
            else
            {
                warn(L"UNBOX: Unsupported operand ~S", x);
                return;
            }

            ir_replace_all_users(NewInteger(iVal), pUnbox->GetRd());
            break;
        } // literal

        case Operand::Kind_Register:
        {
            if (pTy->m_cBits < sizeof(Val) * 8 ||
                Subtypep_Yes == ty_subtypep(pSx->GetTy(), ty_fixnum) )
            {
                ir_replace_insn(
                    new ShrInsn(pUnbox->GetTy(), pUnbox->GetRd(),
                                    pSx, NewInteger(Fixnum::TagBits) ),
                    pUnbox );
            }
            else
            {
                Values* pVy = new Values();
                ir_insert_insn(new ValuesInsn(pVy, pSx), pUnbox);
                ir_replace_insn(
                    new CallInsn(pUnbox->GetTy(), pUnbox->GetRd(),
                            NewLiteral(QDunbox_int), pVy ),
                    pUnbox );
            } // if
            break;
        } // register

        default:
            warn(L"UNBOX: Unexpected operand.");
            return;
        } // switch operand

    case NativeType::Kind_Float:
        break;

    default:
        warn(L"UNBOX: Unsupported type ~S.", pUnbox->GetTy());
        break;
    } // switch ty
} // X86X64SelectPass::process_UNBOX


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_UPVARDEF
//
void X86X64SelectPass::process_UPVARDEF(Instruction* pUpVarDef)
{
    Register* pRd = pUpVarDef->GetRd();
    if (pRd->IsVirtual())
    {
        pRd->SetStorage(Register::Storage_Pseudo);
    }
} // X86X64SelectPass::process_UPVARDEF


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_VALUESA
//
void X86X64SelectPass::process_VALUESA(Instruction* pValuesA)
{
    Val     ty  = pValuesA->GetTy();
    Values* pVd = pValuesA->GetVd();

    Values* pVx = new Values();

    Val callee;
    Val args_ty;

    if (pValuesA->GetOperandCount() == 1)
    {
        callee = Qvalues_list;
        args_ty = Qlist;
    }
    else
    {
        callee = QDvaluesA;
        // BUGBUG: Should be more precise type.
        args_ty = ty_values_rest_t;
    }

    ir_insert_insn(new ValuesInsn(pVx, pValuesA), pValuesA);

    Instruction* pCall = new CallInsn(ty, pVd, NewLiteral(callee), pVx);
    ir_replace_insn(pCall, pValuesA);
} // X86X64SelectPass::process_VALUESA


//////////////////////////////////////////////////////////////////////
//
// X86X64SelectPass::process_VARDEF
//
void X86X64SelectPass::process_VARDEF(Instruction* pVarDef)
{
    Val ty = pVarDef->GetTy();
    if (ty_c6_literal_cell == ty ||
        ty_c6_stack_cell   == ty )
    {
        pVarDef->GetRd()->SetStorage(Register::Storage_Stack);
    }
    else if (ty_closed_cell == ty)
    {
        Values* pVy = new Values();
        ir_insert_insn(new ValuesInsn(pVy, pVarDef->GetSy()), pVarDef);

        Register* pRy = new Register();
        ir_insert_insn(new CallInsn(
                pVarDef->GetSy()->GetTy(), pRy, 
                NewLiteral(QDmake_closed_cell), pVy ),
            pVarDef );

        pVarDef->GetOperandBox(1)->Replace(pRy);
    }
    else
    {
        CAN_NOT_HAPPEN();
    }
} // X86X64SelectPass::process_VARDEF

} // Compiler
