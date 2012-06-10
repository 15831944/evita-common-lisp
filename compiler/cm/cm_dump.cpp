#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - Dump IR
// cm_dump.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_dump.cpp#3 $
//

#include "./cm_session.h"
#include "./cm_fns.h"
#include "./cm_target.h"

namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// Dumper
//
class Dumper
{
    // Run
    public: static void Run(Val stream)
    {
        Dumper oDumper;
        oDumper.run(stream);
    } // Run


    void run(Val stream)
    {
        format(stream, L"(Session~%");

        foreach (Module::EnumFunction, oEnum, Session::Get()->GetModule())
        {
            dump(stream, oEnum.Get());
        } // for each function

        format(stream, L") ; Session~%");
    } // Run

    // dump - Function
    void dump(Val stream, Function* pFun)
    {
        format(stream, L"  (function ~S~%",
            pFun->GetName() );

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            dump(stream, oEnum.Get());
        } // for each bblock

        format(stream, L"  ) ; function ~S~%",
            pFun->GetName() );
    } // dump

    // dump - BBlock
    void dump(Val stream, BBlock* pBBlock)
    {
        format(stream, L"    (bblock ~D~%",
            pBBlock->GetName() );

        foreach (BBlock::EnumInsn, oEnum, pBBlock)
        {
            dump(stream, oEnum.Get());
        } // for each bblock

        format(stream, L"      ) ; bblock ~D~%",
            pBBlock->GetName() );
    } // dump

    // dump - Instruction
    void dump(Val stream, Instruction* pInsn)
    {
        format(stream, L"      (~A",
            intern(pInsn->GetMnemonic(), PACKAGE_keyword) );

        if (Obj_Void != pInsn->GetOutput())
        {
            write_char(' ', stream);
            format(stream, L"~A ", pInsn->GetTy());
            dump(stream, pInsn->GetOutput());
            write_string(L" <=", stream);
        }

        foreach (Instruction::EnumInput, oEnum, pInsn)
        {
            write_char(' ', stream);
            dump(stream, oEnum.Get());
        } // for each operand

        format(stream, L")~%");
    } // dump

    // dump - Operand
    void dump(Val stream, Operand* pOperand)
    {
        switch (pOperand->GetKind())
        {
        case Operand::Kind_Bool:
            format(stream, L"%b~D",
                pOperand->StaticCast<Bool>()->GetName() );
            break;

        case Operand::Kind_Function:
            format(stream, L"#'~S",
                pOperand->StaticCast<Function>()->GetName() );
            break;

        case Operand::Kind_Label:
            format(stream, L"BB~D",
                pOperand->StaticCast<Label>()->GetBBlock()->GetName() );
            break;

        case Operand::Kind_Literal:
            format(stream, L"'~S",
                pOperand->StaticCast<Literal>()->GetDatum() );
            break;

        case Operand::Kind_Register:
        {
            Register* pRx = pOperand->StaticCast<Register>();
            switch (pRx->GetStorage())
            {
            case Register::Storage_Physical:
                format(stream, L"~A_~D",
                    cm_get_target()->GetPhysicalName(pRx->GetLocation()),
                    pRx->GetName() );
                break;

            case Register::Storage_Stack:
                format(stream, L"%[SP+~D]_~D",
                    Fixnum::Encode(pRx->GetLocation()),
                    pRx->GetName() );

            case Register::Storage_Virtual:
                format(stream, L"%r~D",
                    pRx->GetName() );

            default:
                CAN_NOT_HAPPEN();
                // NOTREACHED
            } // switch sc
            break;
        } // Operand::Kind_Register

        case Operand::Kind_Values:
            format(stream, L"%v~D",
                pOperand->StaticCast<Values>()->GetName() );
            break;

        case Operand::Kind_Variable:
            format(stream, L"#<Var ~S>",
                pOperand->StaticCast<Variable>()->GetName() );
            break;

        default:
            CAN_NOT_HAPPEN();
            // NOTREACHED
        } // switch
    } // dump
}; // Dumper

} // namespace


//////////////////////////////////////////////////////////////////////
//
// Dump IR
//
void
text_dump(Val stream)
{
    Dumper::Run(stream);
} // CmLisp::dump

} // Compiler
