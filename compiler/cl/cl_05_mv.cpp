#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 3 Evaluation and Compilation
// compiler/cl/cl_03_call.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_05_mv.cpp#12 $
//
//  Description:
//   This file contains following functions:
//      cl:multiple-value-bind
//      cl:multiple-value-call
//      cl:multiple-value-prog1
//      cl:nth-value
//      cl:values
//
#include "./cl_defs.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_multiple_value_bind
//
define_parser(multiple_value_bind)
{
    CHECK_SYNTAX(3, MaxFormLength,
        "(multiple-value-bind (var*) form decl* form*)" );

    Context::BindingScope oBindScope(
        LexEnv::Kind_let, 
        GetContext(),
        pExpect->Type );

    // (var*)
    Val vars_ty = nil;
    {
        for (Val runner = second(form); ! endp(runner); runner = cdr(runner))
        {
            Val var = car(runner);
            if (! symbolp(var))
            {
                warn(L"Variable name must be a symbol instead of ~S.", var);
                var = make_symbol(L"err");
            }

            addVarDcl(makeVarDef(var));

            vars_ty = cons(t, vars_ty);
        } // for each var

        vars_ty = cons(Qvalues, vars_ty);
    }

    Val forms = parse_declarations(cdddr(form));

    deactivateLexEnv();

    OpenBindInsn* pOpenBind = NULL;

    // Bind variables
    {
        Expect oExpect(ExpectContext_Bind, vars_ty);
            oExpect.Name    = second(form);
        Operand* pSd = parseForm1(&oExpect, third(form));

        if (Obj_Unreachable == pSd)
        {
            return useless_form(form);
        }

        Values* pVx = pSd->DynamicCast<Values>();

        ValuesInsn* pValues = 
            NULL != pVx && pVx->GetDfn()->Is<ValuesInsn>() ?
                pVx->GetDfn()->StaticCast<ValuesInsn>() :
                NULL;

        Arity oArity;
            ir_get_ty_arity(pSd->GetTy(), &oArity);

        Register* pRn = NULL;

        uint nNth = 0;
        foreach (LexEnv::EnumVar, oEnum, GetLexEnv())
        {
            VarDef* pVarDef = oEnum.Get()->DynamicCast<VarDef>();
            if (NULL == pVarDef)
            {
                continue;
            }

            Operand* pSx;
            {
                if (NULL != pValues)
                {
                    if (nNth < pValues->GetOperandCount())
                    {
                        pSx = pValues->GetOperand(nNth);
                    }
                    else
                    {
                        bound_to_extra_value(pVarDef);
                        pSx = NewLiteral(nil);
                    }
                }
                else if (NULL != pVx)
                {
                    if (nNth < oArity.GetMin())
                    {
                        Register* pRd = new Register(pVarDef->GetVar());
                        emitInsn(new ProjectInsn(
                            pVarDef->GetTy(), pRd, pVx, nNth ) );

                        pSx = pRd;
                    }
                    else if (nNth >= oArity.GetMax() && oArity.IsFixed())
                    {
                        bound_to_extra_value(pVarDef);
                        pSx = NewLiteral(nil);
                    }
                    else
                    {
                        style_warn(L"~S bound to missing value.",
                            pVarDef->GetName() );

                        if (NULL == pRn)
                        {
                            pRn = new Register();
                            emitInsn(new CountInsn(pRn, pVx));
                        }

                        Bool* pBx = new Bool();
                        emitInsn(new GtInsn(pBx, pRn, NewLiteral(nNth)));

                        Register* pRx = new Register();
                        emitInsn(new ProjectInsn(
                            pVarDef->GetTy(), pRx, pVx, nNth ) );

                        Register* pRd = new Register(pVarDef->GetVar());
                        emitInsn(new SelectInsn(
                            pRd, pBx, pRx, NewLiteral(nil) ) );

                        pSx = pRd;
                    } // if
                }
                else if (0 == nNth)
                {
                    pSx = pSd;
                }
                else
                {
                    bound_to_extra_value(pVarDef);
                    pSx = NewLiteral(nil);
                }

                nNth += 1;
            } // pSx

            if (! pVarDef->IsSpecial())
            {
                emitInsn(
                    new VarDefInsn(new Register(pVarDef->GetVar()),
                        pVarDef->GetVar(), pSx ) );
            }
            else
            {
                if (NULL == pOpenBind)
                {
                    BindFrame* pFrame = new BindFrame(GetFunction());
                    pOpenBind = new OpenBindInsn(pFrame);
                }
                pOpenBind->AddBind(pVarDef->GetName(), pSx);
            } // if
        } // for each vardef
    } // bind vars

    return parseLetAux(pExpect, forms, pOpenBind);
} // ClParser::parse_multiple_value_bind

//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_multiple_value_call
//  o (multiple-value-call fn) = (funcall fn)
//  o (multiple-vlaue-call fn form1 form2 ...) =
//      (apply fn (nconc (multiple-value-list form1)
//                       (multiple-value-list form2) ...))

define_parser(multiple_value_call)
{
    CHECK_SYNTAX(2, MaxFormLength, "(multiple-value-call fn form*)");

    Val fnForm = second(form);

    switch (Fixnum::Decode_(length(cddr(form))))
    {
    case 0:
        return parseForm(pExpect, list(Qfuncall, fnForm));

    default:
    {
        Val args = list(Qnconc);
        Val tail = args;
        for (Val runner = cddr(form); ! endp(runner); runner = cdr(runner))
        {
            Val frob = list(list(Qmultiple_value_list, car(runner)));
            tail = setf_cdr(frob, tail);
        } // for
        return parseForm(pExpect, list(Qapply, fnForm, args));
    } // default

    case 1:
    {
        Callee oCallee;
        if (Obj_Unreachable == parseCallee(fnForm, &oCallee))
        {
            return useless_form(form);
        }

        // BUGBUG: We have to pass precise type.
        Operand* pSy = parseOpd(
            Qmultiple_value_call,
            ty_values_rest_t,
            1,
            third(form) );

        Values* pArgs;
        {
            if (pSy->Is<Values>())
            {
                pArgs = pSy->StaticCast<Values>();
            }
            else
            {
                pArgs = new Values();
                emitInsn(new ValuesInsn(pArgs, pSy));
            }
        } // pArgs

        return emitLinkage(emitCall(pExpect, form, &oCallee, pArgs));
    } // 1
    } // switch length
} // ClParser::parse_multiple_value_call

//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_multiple_value_prog1
//
//      o (multiple-value-prog1 form1)
//          => form1
//      o (multiple-value-prog1 form1 form...) void
//          => (progn form1 form...)
//      o (multiple-value-prog1 form1 form...) single
//          => (let ((#:save form1)) form... #:save)
//      o (multiple-value-prog1 form1 form...) values
//          => (let ((#:save (multiple-values-list form1)))
//                form...
//                (values-list #:save) )
//
define_special_operator(multiple_value_prog1)
{
    CHECK_SYNTAX(2, MaxFormLength, "(multiple-value-prog1 form1 form*)");

    if (nil == cddr(form))
    {
        return parseForm(pExpect, second(form));
    }

    Operand* pSx = parseForm1(pExpect, second(form));
    if (Obj_Unreachable == pSx)
    {
        return useless_form(form);
    }

    BBlock* pSucc = GetContext()->SetContinue();

    Expect oExpect(Qmultiple_value_prog1, ty_void);
    if (Obj_Unreachable == parseForms(&oExpect, cddr(form)))
    {
        return Obj_Unreachable;
    }

    GetContext()->RestoreSucc(pSucc);

    if (! pSx->Is<Values>())
    {
        return emitLinkage(pSx);
    }
    else
    {
        pSx = emitSaveValues(pSx);
        return emitLinkage(emitRestoreValues(pSx));
    }
} // ClParser::parse_multiple_value_prog1


// ClParser::parse_nth_value
define_special_operator(nth_value)
{
    CHECK_SYNTAX(3, 3, "(nth-value nth form)");

    if (ty_void == pExpect->Type)
    {
        return parseForms(pExpect, cdr(form));
    }

    Operand* pNth = parseOpd(Qnth_value, ty_fixnum, 0, second(form));
        if (Obj_Unreachable == pNth) return useless_form(form);

    Operand* pSx = parseOpd(Qnth_value, ty_values_rest_t, 1, third(form));
        if (Obj_Unreachable == pSx) return useless_form(form);

    switch (pNth->GetKind())
    {
    case Operand::Kind_Register:
        goto emit_nth_value;

    case Operand::Kind_Literal:
    {
        Val nth = pNth->StaticCast<Literal>()->GetDatum();
            ASSERT(fixnump(nth));

        if (minusp_xx(nth))
        {
            warn(L"Invalid index ~D.", nth);
            return emitLinkage(pSx);
        }

        if (! pSx->Is<Values>())
        {
            if (Fixnum::Decode_(nth) > 0)
            {
                pSx = NewLiteral(nil);
            }
        }
        else
        {
            Values* pVx = pSx->StaticCast<Values>();
            Instruction* pInsn = pVx->GetDfn();

            // BUGBUG: NYI: Check known function.
            if (pInsn->Is<ValuesInsn>())
            {
                if (cmp_xx(nth, pInsn->GetOperandCount()) < 0)
                {
                    pSx = pInsn->GetOperand(
                        static_cast<uint>(Fixnum::Decode_(nth)) );
                }
                else
                {
                    pSx = NewLiteral(nil);
                }
            }
            else if (Fixnum::Encode(0) == nth)
            {
                Register* pRd = new Register();
                    emitInsn(new ProjectInsn(pExpect->Type, pRd, pVx, 0));
                pSx = pRd;
            }
            else
            {
                Register* pRn = new Register();
                emitInsn(new CountInsn(pRn, pVx));

                Register* pRy = new Register();
                emitInsn(new ProjectInsn(ty_t, pRy, 
                    pVx,
                    static_cast<uint>(Fixnum::Decode_(nth) )) );

                Bool* pBd = new Bool();
                emitInsn(new GeInsn(pBd, pRn, pNth));

                Register* pRd = new Register();
                emitInsn(new SelectInsn(pRd, pBd, pRy, NewLiteral(nil)));
                pSx = pRd;
            } // if values insn
        } // if values
        break;
    } // Kind_Literal

    emit_nth_value:
    {
        Register* pRx = new Register();
        emitInsn(new NthValueInsn(pExpect->Type, pRx,
            pNth,
            pSx->StaticCast<Values>() ) );
        pSx = pRx;
        break;
    } // emit_nth_value

    default:
        CAN_NOT_HAPPEN();
    } // switch kind

    return emitLinkage(pSx);
} // ClParser::nth_value


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_values
//
//      void        parseForms
//      single      parseForm1 + parseForms
//      multiple    parseForm
//
define_parser(values)
{
    CHECK_SYNTAX(1, MaxFormLength, "(values form*)");

    if (ty_void == pExpect->Type)
    {
        useless_form(form);
        return parseForms(pExpect, rest(form));
    } // if void

    Arity oArity;
        ir_get_ty_arity(pExpect->Type, &oArity);

    uint cOperands;
    {
        if (oArity.IsFixed())
        {
            cOperands = oArity.GetMax();
        }
        else
        {
            cOperands = static_cast<uint>(
                Fixnum::Decode_(length(form)) - 1 );
        }
    } // cOperands

    Operand* rgpOperand[5];
    Operand** prgpOperand = rgpOperand;
    {
        if (cOperands > lengthof(rgpOperand))
        {
            prgpOperand = new Operand*[cOperands];
        }
    } // prgpOperand

    html_log_format(2, L"parse_values: expected_ty=~W~:%", pExpect->Type);
    {
        uint nIndex = 0;
        tyIterator oIter(pExpect->Type);

        Expect oExpect = *pExpect;

        foreach (EnumList, oEnum, cdr(form))
        {
            Val form1 = oEnum.Get();

            Val ty;
            {
                if (oArity.IsExtra(nIndex))
                {
                    ty = ty_void;
                }
                else
                {
                    oIter.Next();
                    ty = oIter.GetTy();
                }
            } // ty

            html_log_format(2, L"parse_values: [~D] ty=~W~:%", nIndex, ty);

            Operand* pSx;
            {
                oExpect.Type = ty;
                oExpect.Nth  = Fixnum::Encode(nIndex);

                pSx = parseForm1(&oExpect, form1);
                if (Obj_Unreachable == pSx)
                {
                    return useless_form(form);
                } // if
            } // pSx

            if (nIndex < cOperands)
            {
                prgpOperand[nIndex] = Obj_Void == pSx ? NewLiteral(nil) : pSx;
                nIndex += 1;
            } // while
        } // for each form

        if (1 == cOperands)
        {
            return emitLinkage(prgpOperand[0]);
        }

        while (nIndex < cOperands)
        {
            prgpOperand[nIndex] = NewLiteral(nil);
            nIndex += 1;
        } // while

        Values* pVd = new Values();
        emitInsn(new ValuesInsn(pVd, prgpOperand, cOperands));
        return emitLinkage(pVd);
    } // multiple
} // ClParser::parse_values

} // Compiler
