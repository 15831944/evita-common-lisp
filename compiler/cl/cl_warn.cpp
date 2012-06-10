#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Common Lisp Parser - Warnings
// compiler/cl/cl_warn.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_warn.cpp#11 $
//
#include "./cl_defs.h"

namespace Compiler
{

// bound_to_extra_value
void
ClParser::bound_to_extra_value(VarDef* pVarDef)
{
    style_warn(L"~S bound to extra value NIL.", pVarDef->GetName());
} // ClParser::bound_to_extra_value


//////////////////////////////////////////////////////////////////////
//
// check_form_syntax
//
Operand*
ClParser::check_syntax(
    Val form,
    int iMin,
    int iMax,
    LPCWSTR pwszSyntax )
{
    ASSERT(iMin >= 0);
    ASSERT(iMin <= iMax);

    Int n = Fixnum::Decode_(safe_list_length(form));

    if (n >= iMin && n <= iMax)
    {
        return NULL;
    }

    if (n <= 0)
    {
        return malformed_form(form);
        #if 0
        return parseForm(expected_ty, list(Qerror,
            list(Qquote, Q("COMPILER::MALFORMED-FORM")),
            Q(":SYNTAX"), make_string(pwszSyntax),
            Q(":FORM"),  list(Qquote, form) ) );
        #endif
    }
    else
    {
        warn(L"Syntax error: ~S, expected ~A", form, make_string(pwszSyntax));
        return Obj_Unreachable;
        #if 0
        return parseForm(expected_ty, list(Qerror,
            list(Qquote, Q("COMPILER::SYNTAX-ERROR")),
            Q(":SYNTAX"), make_string(pwszSyntax),
            Q(":FORM"),  list(Qquote, form) ) );
        #endif
    }
} // ClParser::check_form_syntax


//////////////////////////////////////////////////////////////////////
//
// ignore_args
//
Operand*
ClParser::ignore_args(Val form)
{
    if (Session::Get()->CanContinue())
    {
        style_warn(L"Useless form: ~S", form);
    }

    Expect oExpect(ty_void);
    return parseForms(&oExpect, rest(form));
} // ClParser::ignore_args


//////////////////////////////////////////////////////////////////////
//
// ignore_form
//
Operand*
ClParser::ignore_form(Val form)
{
    style_warn(L"Ignore form: ~S", form);
    return Obj_Void;
} // ClParser::ignore_form


//////////////////////////////////////////////////////////////////////
//
// malbindinged_binding
//
void
ClParser::malformed_binding(Val binding)
{
    warn(L"Malformed binding: ~S", binding);
} // ClParser::malbindinged_binding


//////////////////////////////////////////////////////////////////////
//
// malformed_form
//
Operand*
ClParser::malformed_form(Val form)
{
    warn(L"Malformed form: ~S", form);
    return Obj_Unreachable;
} // ClParser::malformed_form


//////////////////////////////////////////////////////////////////////
//
// not_function
//
Operand*
ClParser::not_function(Val name)
{
    if (setf_cell_p(name))
    {
        name = setf_cell_name(name);
    }
    warn(L"~S is not function.", name);
    return Obj_Unreachable;
} // ClParser::not_function


//////////////////////////////////////////////////////////////////////
//
// unexpected_type
//
void
ClParser::unexpected_type(const Expect* pExpect, Ty ty, Val form)
{
    if (! Session::Get()->CanContinue()) return;

    switch (pExpect->Context)
    {
    case ExpectContext_Argument:
    case ExpectContext_Operand:
        warn(L"The ~:R argument of ~S must be ~S instead of ~S.",
            add_xx(pExpect->Nth, 1),
            pExpect->Name,
            pExpect->Type,
            ty );
        break;

    case ExpectContext_Assign:
    case ExpectContext_Bind:
        warn(L"Variable ~S takes ~S instead of ~S.",
            pExpect->Name,
            pExpect->Type,
            ty );
        break;

    case ExpectContext_KeyName:
        warn(L"Keyword paramenter name must be a symbol instead of ~S.",
            pExpect->Type,
            form );
        break;

    case ExpectContext_KeyVal:
        warn(L"Keyword ~S of ~S must be ~S instead of ~S.",
            pExpect->Nth,
            pExpect->Name,
            pExpect->Type,
            ty );
        break;

    case ExpectContext_Callee:
    case ExpectContext_Unreachable:
    case ExpectContext_Value:
        if (nil == pExpect->Name)
        {
            warn(L"Expect ~S instead of ~S.",
                pExpect->Type,
                ty );
        }
        else if (nil != form)
        {
            warn(L"Value of function ~S must be ~S instead of ~S: ~S",
                pExpect->Name,
                pExpect->Type,
                ty,
                form );
        }
        else
        {
            warn(L"Value of function ~S must be ~S instead of ~S.",
                pExpect->Name,
                pExpect->Type,
                ty );
        }
        break;

    default:
        warn(L"Unexpected context ~D", Fixnum::Encode(pExpect->Context));
        break;
    } // context
} // ClParser::unexpected_type


//////////////////////////////////////////////////////////////////////
//
// unportable_form
//
void
ClParser::unportable_form(Val form)
{
    style_warn(L"Unportable form: ~S", form);
} // ClParser::unportable_form


//////////////////////////////////////////////////////////////////////
//
// unreachable_form
//
Operand*
ClParser::unreachable_form(Val form)
{
    if (Session::Get()->CanContinue())
    {
        style_warn(L"Unreachable form: ~S", form);
    }
    return Obj_Unreachable;
} // ClParser::unreachable_form


//////////////////////////////////////////////////////////////////////
//
// useless_form
//
//  Report useless form. This function called with whole form when one
//  of operand of function is unreachable.
//
//  Example:
//      (eq (return nil) (foo))
//    =>
//      Unreachable form: (foo)
//      Useless form: (eq (return nil) (foo))
//
Operand*
ClParser::useless_form(Val form)
{
    if (Session::Get()->CanContinue())
    {
        style_warn(L"Useless form: ~S", form);
    }
    return Obj_Unreachable;
} // ClParser::useless_form

} // Compiler
