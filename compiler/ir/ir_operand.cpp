#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - operand
// ir_operand.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_operand.cpp#28 $
//
#include "./ir_operand.h"

#include "./ir_fns.h"

#include "../cm/cm_fns.h"
#include "../cm/cm_session.h"
#include "../cm/cm_target.h"

namespace Compiler
{

Unreachable k_oUnreachable;
Unreachable* Obj_Unreachable = &k_oUnreachable;

Void  k_oVoid;
Void* Obj_Void = &k_oVoid;

BoolConstant k_oTrue(1);
BoolConstant* Bool_True = &k_oTrue;

BoolConstant k_oFalse(0);
BoolConstant* Bool_False = &k_oFalse;


class Constant : public Literal
{
    public: Constant(Val ty, Ty x) : Literal(ty, x) {}
}; // Constant

Constant k_oGeneraizedTrue(ty_symbol, Fixnum::Encode(1));
Literal* Obj_True = &k_oGeneraizedTrue;

Constant k_oNil(ty_null, nil);
Literal* Obj_Nil = &k_oNil;


//////////////////////////////////////////////////////////////////////
//
// Register constructor
//
Bool::Bool() :
    Output(GetKind_())
{
    m_nName = Session::NewName();
} // Bool::Bool


//////////////////////////////////////////////////////////////////////
//
//  Bool::HtmlPrint
//
void Bool::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);
    cm_format(stream, L"%b~D", GetName());
    write_string(L"</a>", stream);
} // Bool::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
//  Frame constructor
//
Frame::Frame(
    Function*   pOwner,
    Val         kind,
    Val         name ) :
        Output(Kind_Frame),
        m_pOwner(pOwner),
        m_pOuter(NULL),
        m_iLocation(12345),
        m_cbFrame(0),
        m_kind(kind),
        m_name(name)
{
    if (NULL != m_pOwner)
    {
        // For anchor.
        m_pOwner->m_oFrames.Append_(this);
    }
} // Frame::Frame


//////////////////////////////////////////////////////////////////////
//
//  Frame::HtmlPrint
//
void Frame::HtmlPrint(Val stream, bool) const
{
    Int ip = reinterpret_cast<Int>(this);
    cm_format(stream, L"[~A-Frame ~S ~X~X]",
        GetKind(),
        GetName(),
        Fixnum::Encode(ip >> 4),
        Fixnum::Encode(ip & 15) );
} // Frame::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
//  FunLit::HtmlPrint
//
void FunLit::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);
    cm_format(stream, L"#[FunLit ");
    GetFun()->HtmlPrint(stream, false);
    write_string(L"]</a>", stream);
} // FunLit::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
//  Integer::HtmlPrint
//
void Integer::HtmlPrint(Val stream, bool) const
{
    if (m_iVal < Fixnum::MostNegative || m_iVal > Fixnum::MostPositive)
    {
        cm_format(stream, L"#x~X~X",
            Fixnum::Encode(m_iVal >> 4),
            Fixnum::Encode(m_iVal & 15) );
    }
    else
    {
        cm_format(stream, L"~D", Fixnum::Encode(m_iVal));
    }
} // Integer::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
//  Label::HtmlPrint
//
void Label::HtmlPrint(Val stream, bool) const
{
    GetBBlock()->HtmlPrint(stream, false);
} // Label::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
// Label::Realize
//
void
Label::Realize(OperandBox* pBox)
{
    switch (pBox->GetInstruction()->GetOpcode())
    {
    case IrOp_BRANCH: case IrOp_JUMP:
    case IrOp_OPENBLOCK: case IrOp_OPENCATCH: case IrOp_TAG:
    case IrOp_TBLJMP:
        AddCfgEdge(
            pBox->GetInstruction()->GetBBlock(),
            GetBBlock() );
        break;
    } // switch opcode
} // Label::Realize


//////////////////////////////////////////////////////////////////////
//
//  Literal::HtmlPrint
//
void Literal::HtmlPrint(Val stream, bool) const
{
    if (this == Obj_True)
    {
        cm_format(stream, L"<b>True</b>");
    }
    else
    {
        cm_format(stream, L"<span class='l'>'~S</span>", GetDatum());
    }
} // Literal::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
// JumpInsn::Unrealize
//
//  Description:
//    Removes edge from pCurr to pTarget.
//
void
Label::Unrealize(OperandBox* pBox)
{
    switch (pBox->GetInstruction()->GetOpcode())
    {
    case IrOp_BRANCH: case IrOp_JUMP:
    case IrOp_OPENBLOCK: case IrOp_OPENCATCH: case IrOp_TAG:
    case IrOp_TBLJMP:
        RemoveCfgEdge(
            pBox->GetInstruction()->GetBBlock(),
            GetBBlock() );
        break;
    } // switch opcode
} // Label::Unrealize

//////////////////////////////////////////////////////////////////////
//
// OperandBox::GetRx
//
Register*
OperandBox::GetRx() const
{
    return m_pOperand->DynamicCast<Register>();
} // OperandBox::GetRx


//////////////////////////////////////////////////////////////////////
//
// OperandBox::Replace
//
Operand* OperandBox::Replace(Operand* pSnew)
{
    html_log_format(5, L"~S:~S ~S -> ~S~:%",
        GetInstruction()->GetBBlock(),
        GetInstruction(),
        m_pOperand,
        pSnew );

    // Note: Values operand must be values.
    if (m_pOperand->Is<Values>() && ! pSnew->Is<Values>())
    {
        switch (GetInstruction()->GetOpcode())
        {
        case IrOp_PHI:
        case IrOp_RET:
            break;

        default:
        {
            Values* pVd = new Values();
            ir_insert_insn(
                new ValuesInsn(pVd, pSnew),
                GetInstruction() );
            pSnew = pVd;
            break;
        } // default
        } // switch opcode
    } // if

    if (! GetInstruction()->IsRealized())
    {
        m_pOperand = pSnew;
    }
    else
    {
        m_pOperand->Unrealize(this);
        m_pOperand = pSnew;
        m_pOperand->Realize(this);
    }

#if 0
    // Note: To avoid infinite loop due to PHI with integer range type,
    // we don't propagate type here.
    {
        Register* pRx = pSnew->DynamicCast<Register>();
        if (NULL == pRx || (pRx->IsVirtual() && pRx->IsSSA()))
            { GetInstruction()->UpdateTy(); }
    }
#endif

    if (NULL == m_pVar && pSnew->Is<Register>())
        { m_pVar = pSnew->StaticCast<Register>()->GetVar(); }

    return pSnew;
} // OperandBox::Replace


// Note: Only RET instruction can have void operand.
Operand* OperandBox::SetOperand(Operand* p)
{
    if (NULL == m_pVar && p->Is<Register>())
        { m_pVar = p->StaticCast<Register>()->GetVar(); }

    p = p->Simplify();

    if (NULL == m_pVar && p->Is<Register>())
        { m_pVar = p->StaticCast<Register>()->GetVar(); }

    return m_pOperand = p;
} // OperandBox::SetOperand


//////////////////////////////////////////////////////////////////////
//
// Output::GetTy
//
Val
Output::GetTy() const
{
    //return NULL == GetDfn() ? ty_t : GetDfn()->GetTy();
    return GetDfn()->GetTy();
} // Output::GetTy


Operand* Output::Simplify() const
{
    if (NULL == GetDfn()) return const_cast<Output*>(this);
    return GetDfn()->SimplifyOutput();
} // Output::Simplify


//////////////////////////////////////////////////////////////////////
//
// Register constructor
//
Register::Register(Variable* pVar, Class eClass) :
    Output(Kind_Register, pVar),
    m_eClass(eClass),
    m_eStorage(Storage_Virtual),
    m_iStorage(Unassigned)
{
    m_nName = Session::NewName();
    m_pRep  = this;
} // Register::Register


//////////////////////////////////////////////////////////////////////
//
//  Register::HtmlPrint
//
void Register::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);

    switch (GetStorage())
    {
    case Register::Storage_Closed:
        cm_format(stream, L"%c[~D]_~D",
            Fixnum::Encode(GetLocation()),
            GetName() );
        break;

    case Register::Storage_LoadTimeValue:
        cm_format(stream, L"%ltv~D", GetName());
        break;

    case Register::Storage_Pseudo:
        cm_format(stream, L"%q~D", GetName());
        break;

    case Register::Storage_Physical:
        cm_format(stream, L"~A_~D",
            cm_get_target()->GetPhysicalName(GetLocation()),
            GetName() );
        break;

    case Register::Storage_Stack:
        cm_format(stream, L"%[SP+~D]_~D",
            Fixnum::Encode(GetLocation()),
            GetName() );
        break;

    case Register::Storage_Virtual:
        if (GetClass() == Class_GPR)
            { cm_format(stream, L"%r~D", GetName()); }
        else
            { cm_format(stream, L"%f~D", GetName()); }
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch sc

    if (! IsSSA())
    {
        cm_format(stream, L"*");
    }

    write_string(L"</a>", stream);
} // Register::HtmlPrint


// Register::Match
bool Register::Equal(const Operand* pSx) const
{
    if (this == pSx) return true;

    const Register* pRx = pSx->DynamicCast<Register>();
        if (NULL == pRx) return false;

    if (pRx->GetStorage() != m_eStorage) return false;
    if (pRx->GetClass()   != m_eClass) return false;
    if (Unassigned == m_iStorage) return false;

    switch (m_eStorage)
    {
    case Storage_Physical:
    case Storage_Stack:
        return pRx->GetLocation() == m_iStorage;

    case Storage_Closed:
    case Storage_Pseudo:
    case Storage_Virtual:
        return false;

    default:
        CAN_NOT_HAPPEN();
    } // switch storage
} // Register::Match


//////////////////////////////////////////////////////////////////////
//
//  Unreachable::HtmlPrint
//
void Unreachable::HtmlPrint(Val stream, bool) const
{
    cm_format(stream, L"unreachable");
} // Unreachable::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
// Values constructor
//
Values::Values() :
    Output(Kind_Values)
{
    m_nName = Session::NewName();
} // Values::Value


//////////////////////////////////////////////////////////////////////
//
//  Values::HtmlPrint
//
void Values::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);
    cm_format(stream, L"%v~D</i>", GetName());
    write_string(L"</a>", stream);
} // Values::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
//  Values::Match
//
bool Values::Equal(const Operand* pSx) const
{
    if (this == pSx)
    {
        return true;
    }

    const Values* pVx = pSx->DynamicCast<Values>();
    if (NULL == pVx)
    {
        return false;
    }

    ValuesInsn* pValues1 = GetDfn()->DynamicCast<ValuesInsn>();
    if (NULL == pValues1)
    {
        return false;
    }

    ValuesInsn* pValues2 = pVx->GetDfn()->DynamicCast<ValuesInsn>();
    if (NULL == pValues2)
    {
        return false;
    }

    if (pValues1->GetOperandCount() != pValues2->GetOperandCount())
    {
        return false;
    }

    Instruction::EnumInput oEnum2(pValues2);
    foreach (Instruction::EnumInput, oEnum1, pValues1)
    {
        if (! oEnum1.Get()->Equal(oEnum2.Get()))
        {
            return false;
        }
    } // for each input

    return true;
} // Values::Match


//////////////////////////////////////////////////////////////////////
//
//  Void::HtmlPrint
//
void Void::HtmlPrint(Val stream, bool) const
{
    cm_format(stream, L"void");
} // Void::HtmlPrint


//////////////////////////////////////////////////////////////////////
//
// NewInteger
//
Integer*
NewInteger(Int iVal)
{
    return new Integer(iVal);
} // NewInteger

static bool isHashCompatible(Val x)
{
    switch (x->GetTag4())
    {
    case_Tag_Cons:
    case_Tag_Record:
    case Val_::Tag_Function:
        return false;

    default:
        return true;
    } // switch tag
} // isHashCompatible

//////////////////////////////////////////////////////////////////////
//
// NewLiteral
//
Literal*
NewLiteral(Val x)
{
    if (x == nil) return Obj_Nil;

    Val key = x;

    if (QQfree_slot_marker == key)
    {
        key = QQfree_slot_marker_key;
    }
    else if (QQremoved_slot_marker == key)
    {
        key = QQremoved_slot_marker_key;
    }

    Val htb = Session::Get()->m_littab;

    Val lit = nil;

    if (! isHashCompatible(key))
    {
        Literal* pLiteral = new Literal(x);
        lit = Fixnum::Encode(pLiteral);
    }
    else
    {
        lit = gethash(key, htb);
        if (nil == lit)
        {
            Literal* pLiteral = new Literal(x);
            lit = Fixnum::Encode(pLiteral);
            setf_gethash(lit, key, htb);
        }
    }

    return lit->StaticCast<Literal>();
} // NewLiteral


//////////////////////////////////////////////////////////////////////
//
//  NewOutput
//
Output* 
NewOutput(Val ty)
{
    if (ty_void == ty) return Obj_Void;
    if (ty_bool == ty) return new Bool();

    if (ty_float32 == ty) return new FpRegister();
    if (ty_float64 == ty) return new FpRegister();

    if (ty_int == ty)   return new Register();
    if (ty_int8 == ty)  return new Register();
    if (ty_int16 == ty) return new Register();
    if (ty_int32 == ty) return new Register();
    if (ty_int64 == ty) return new Register();

    if (ty_uint == ty)   return new Register();
    if (ty_uint8 == ty)  return new Register();
    if (ty_uint16 == ty) return new Register();
    if (ty_uint32 == ty) return new Register();
    if (ty_uint64 == ty) return new Register();

    ty = ty_expand(ty);

    if (consp(ty))
    {
        if (Qvalues == car(ty))
        {
            return new Values();
        }

        ty = car(ty);
    }

    if (nil == ty) return Obj_Void;

    if (symbolp(ty)) return new Register();

    error(L"Unsupported type: ~S", ty);
} // NewOutput


//////////////////////////////////////////////////////////////////////
//
// ir_can_nil
//  Returns true if pSx can be nil.
//
bool ir_can_nil(Operand* pSx)
{
    if (pSx->Is<Literal>())
    {
        return pSx->StaticCast<Literal>()->GetDatum() == nil;
    }

    return ty_typep(nil, pSx->GetTy());
} // ir_can_nil


//////////////////////////////////////////////////////////////////////
//
// ir_get_callee_name
//
Val ir_get_callee_name(Operand* pSx)
{
    Val name;
    {
        switch (pSx->GetKind())
        {
        case Operand::Kind_Function:
            name = pSx->StaticCast<Function>()->GetName();
            break;

        case Operand::Kind_Literal:
            name = pSx->StaticCast<Literal>()->GetDatum();
            break;

        case Operand::Kind_Register:
        {
            Register* pRx = pSx->StaticCast<Register>();
            if (NULL != pRx->GetVar())
            {
                name = pRx->GetVar()->GetName();
            }
            else
            {
                name =pRx->GetForm();
            }
            break;
        } // Kind_Register

        default:
            CAN_NOT_HAPPEN();
        } // switch kind
    } // name

    if (setf_cell_p(name))
    {
        name = list(Qsetf, setf_cell_name(name));
    }
    return name;
} // ir_get_callee_name

} // Compiler
