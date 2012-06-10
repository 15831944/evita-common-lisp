#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 3 Evaluation and Compilation
// compiler/cl/cl_03_call.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_03_decl.cpp#9 $
//
// Description:
//  This file contains parser for declare form with following declaration
//  identifiers:
//      declaration     -- signal warning
//      dynamic-extent
//      ftype NYI
//      ignore
//      ignorable
//      inline
//      ext:lambda-name
//      notinline
//      optimize
//      special
//      type
//      values
//
#include "./cl_defs.h"

namespace Compiler
{

#define CHECK_DECL_SYNTAX(min, max, syntax) \
{ \
    if (NULL != check_syntax(decl, min, max, L##syntax))\
        return; \
} // CHECK_DECL_SYNTAX


Val find_type(Val);


//////////////////////////////////////////////////////////////////////
//
// ClParser::parseDclFunName
//
ClParser::FunDcl*
ClParser::parseDclFunName(Val name)
{
    if (symbolp(name))
    {
        // function name
    }
    else if (function_name_p(name))
    {
        name = intern_setf_cell(second(name));
    }
    else
    {
        style_warn(L"Expected function name: ~S", name);
        return NULL;
    }

    FunDcl* pFunDcl = getFunDcl(name);
    if (pFunDcl->GetLexEnv() != GetLexEnv())
    {
        FunDcl* pOuter = pFunDcl;
        pFunDcl = addFunDcl(new FunDcl(
            pFunDcl->GetKind(),
            name,
            pOuter->GetDatum() ) );

        pFunDcl->SetTy(pOuter->GetTy());
    } // if

    return pFunDcl;
} // ClParser::parseDclFunName


//////////////////////////////////////////////////////////////////////
//
// parseDclFunNameOrVarName
//
ClParser::NameDcl*
ClParser::parseDclFunNameOrVarName(Val name)
{
    if (symbolp(name))
    {
        return parseDclVarName(name);
    } // if

    if (safe_list_length(name) == Fixnum::Encode(2) && Qfunction == car(name))
    {
        return parseDclFunName(second(name));
    }

    style_warn(L"Expect variable or function name: ~S", name);
    return NULL;
} // ClParser::parseDclFunNameOrVarName


//////////////////////////////////////////////////////////////////////
//
// ClParser::parseDclVarName
//
ClParser::VarDcl*
ClParser::parseDclVarName(Val name)
{
    if (symbolp(name))
    {
        // variable name
    }
    else
    {
        style_warn(L"Expected variable name: ~S", name);
        return NULL;
    }

    VarDcl* pVarDcl = getVarDcl(name);
    if (pVarDcl->GetLexEnv() != GetLexEnv())
    {
        VarDcl* pOuter = pVarDcl;
        pVarDcl = addVarDcl(new VarDcl(
            pVarDcl->GetKind(),
            name,
            pOuter->GetDatum() ) );

        pVarDcl->SetTy(pOuter->GetTy());
    } // if

    return pVarDcl;
} // ClParser::parseDclVarName


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_declarations
//
Val ClParser::parse_declarations(Val decls_forms, Function* pFun)
{
    Val runner = decls_forms;
    Val doc_string = nil;
    while (! endp(runner))
    {
        Val form = car(runner);

        if (consp(form))
        {
            if (car(form) != Qdeclare)
            {
                break;
            }

            parse_declare(form, pFun);
        }
        else if (stringp(form))
        {
            if (nil != doc_string)
            {
                break;
            }

            if (NULL == pFun)
            {
                break;
            }

            if (nil == cdr(runner))
            {
                break;
            }

            pFun->SetDocument(form);
        }
        else
        {
            break;
        }

        runner = cdr(runner);
    } // while

    return runner;
} // ClParser::parse_declarations


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_declare
//
void ClParser::parse_declare(Val form, Function* pFun)
{
    if (minusp_xx(safe_list_length(form)))
    {
        malformed_form(form);
        return;
    }

    struct Entry
    {
        Val m_name;
        void (ClParser::*m_pfn)(Val, Function*);
    }; // Entry

    static Entry const k_rgoParser[] =
    {
        #define ENTRY(mp_name) \
            { Q ## mp_name, &ClParser::parse_declare_##mp_name }

        #define parse_declare_ignorable parse_declare_ignore
        #define parse_declare_notinline parse_declare_inline

        ENTRY(declaration),
        ENTRY(dynamic_extent),
        ENTRY(ftype),
        ENTRY(ignore),
        ENTRY(ignorable),
        ENTRY(inline),
        ENTRY(lambda_name),
        ENTRY(notinline),
        ENTRY(optimize),
        ENTRY(special),
        ENTRY(type),
        ENTRY(values)
    }; // k_rgoParser

    foreach (EnumList, oEnum, cdr(form))
    {
        Val decl = oEnum.Get();
        if (minusp_xx(safe_list_length(decl)))
        {
            malformed_form(decl);
        }
        else
        {
            const Entry* pEntry = NULL;
            for (
                const Entry* p = &k_rgoParser[0];
                p < &k_rgoParser[lengthof(k_rgoParser)];
                p++ )
            {
                if (p->m_name == car(decl))
                {
                    pEntry = p;
                    break;
                }
            } // for

            if (NULL != pEntry)
            {
                (this->*pEntry->m_pfn)(decl, pFun);
            }
            else if (nil != find_type(car(decl)))
            {
                parse_declare_type(cons(Qdeclare, decl), pFun);
            }
            else
            {
                // BUGBUG: Check type name declaration
                // BUGBUG: Check declaration
                warn(L"Unknown declaration: ~S", decl);
            }
        } // if
    } // for
} // ClParser::parse_declare


// ClParser::parse_declare_declaration
void ClParser::parse_declare_declaration(Val decl, Function*)
{
    warn(L"~S can be allowed to be in proclaim.", decl);
} // ClParser::parse_declare_declaration

// ClParser::parse_declare_dynamic_extent
void ClParser::parse_declare_dynamic_extent(Val decl, Function*)
{
    foreach (EnumList, oEnum, cdr(decl))
    {
        NameDcl* pNameDcl = parseDclFunNameOrVarName(oEnum.Get());
        if (NULL == pNameDcl)
        {
            continue;
        }

        pNameDcl->SetFlags(NameDcl::Flag_DynamicExtent);
    } // for
} // ClParser::parse_declare_dynamic_extent

void ClParser::parse_declare_ftype(Val decl, Function*)
{
    Ty ty = second(decl);
    foreach (EnumList, oEnum, cddr(decl))
    {
        FunDcl* pFunDcl = parseDclFunName(oEnum.Get());
        pFunDcl->SetTy(ty);
    } // for
} // ClParser::parse_declare_ftype


// ClParser::parse_declare_ignore
void ClParser::parse_declare_ignore(Val decl, Function*)
{
    NameDcl::Usage eUsage = Qignore == car(decl) ?
        NameDcl::Usage_Ignore : 
        NameDcl::Usage_Ignorable;

    foreach (EnumList, oEnum, cdr(decl))
    {
        NameDcl* pNameDcl = parseDclFunNameOrVarName(oEnum.Get());
        if (NULL == pNameDcl)
        {
            continue;
        }

        if (pNameDcl->GetUsage() != NameDcl::Usage_None &&
            pNameDcl->GetUsage() != eUsage )
        {
            style_warn(L"~S was declared as ~S.",
                pNameDcl->GetName(),
                pNameDcl->GetUsage() == NameDcl::Usage_Ignore ?
                    Qignore : Qignorable );
        } // if

        pNameDcl->SetUsage(eUsage);
    } // for each name
} // ClParser::parse_declare_ignore


// ClParser::parse_declare_inline
void ClParser::parse_declare_inline(Val decl, Function*)
{
    UINT nSet;
    UINT nClr;
    {
        if (Qinline == car(decl))
        {
            nSet = NameDcl::Flag_Inline;
            nClr = NameDcl::Flag_NotInline;
        }
        else
        {
            nSet = NameDcl::Flag_NotInline;
            nClr = NameDcl::Flag_Inline;
        }
    } // nPositive
    
    foreach (EnumList, oEnum, cdr(decl))
    {
        NameDcl* pNameDcl = parseDclFunName(oEnum.Get());
        if (NULL == pNameDcl)
        {
            continue;
        }

        pNameDcl->SetFlags(nSet);
        pNameDcl->ClrFlags(nClr);
    } // for each name
} // ClParser::parse_declare_inline


// ClParser::parse_declare_ext_lambda_name
//      (ext:lambda-name name)
// Note: we allow multiple ext:lambda-name declaration. The first one
// is effective.
//
void ClParser::parse_declare_lambda_name(Val decl, Function* pFun)
{
    if (NULL == pFun)
    {
        warn(L"Can't use ~S in this context.", decl);
        return;
    }

    CHECK_DECL_SYNTAX(2, 2, "(ext:lambda-name name)");

    if (val_anonymous == pFun->GetName())
    {
        pFun->SetName(second(decl));
    }
} // ClParser::parse_declare_ext_lambda_name


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_declare_optimize
//
void ClParser::parse_declare_optimize(Val decl, Function*)
{
    foreach (EnumList, oEnum, cdr(decl))
    {
        Val spec = oEnum.Get();
        Val name = spec;
        Val val = Fixnum::Encode(3);

        if (symbolp(spec))
        {
            name = spec;
        }
        else if (consp(spec) && nil == cdr(spec))
        {
            name = first(spec);
        }
        else if (consp(spec) && consp(cdr(spec)) && nil == cddr(spec))
        {
            name = first(spec);
            val  = second(spec);
        }
        else
        {
            name = Fixnum::Encode(0);
        }

        if (fixnump(val) &&
            Fixnum::Decode_(val) >= 0 &&
            Fixnum::Decode_(val) <= 3 )
        {
            if (Qcompilation_speed == name)
            {
                Session::Get()->m_oOptimizeQualities.SetCompilationSpeed(
                    Fixnum::Decode_(val) );
                continue;
            }
            else if (Qdebug == name)
            {
                Session::Get()->m_oOptimizeQualities.SetDebug(
                    Fixnum::Decode_(val) );
                continue;
            }
            else if (Qsafety == name)
            {
                Session::Get()->m_oOptimizeQualities.SetSafety(
                    Fixnum::Decode_(val) );
                continue;
            }
            else if (Qspeed == name)
            {
                Session::Get()->m_oOptimizeQualities.SetSpeed(
                    Fixnum::Decode_(val) );
                continue;
            }
            else if (Qspace == name)
            {
                Session::Get()->m_oOptimizeQualities.SetSpace(
                    Fixnum::Decode_(val) );
                continue;
            }
        } // if

        warn(L"Invalid optimize quality ~S.", spec);
    } // for spec
} // ClParser::parse_declare_optimize


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_declare_special
//
void ClParser::parse_declare_special(Val decl, Function*)
{
    foreach (EnumList, oEnum, cdr(decl))
    {
        VarDcl* pVarDcl = parseDclVarName(oEnum.Get());
        pVarDcl->MarkSpecial();
    } // for
} // ClParser::parse_declare_special


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_type
//      (declare (type <type> name ...))
//
void ClParser::parse_declare_type(Val decl, Function*)
{
    Ty ty = ty_expand(second(decl));
    foreach (EnumList, oEnum, cddr(decl))
    {
        VarDcl* pVarDcl = parseDclVarName(oEnum.Get());
        if (NULL != pVarDcl) pVarDcl->SetTy(ty);
    } // for
} // ClParser::parse_declare_type


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_declare_values
//      (declare (values type* [&optional type*] [&rest type]))
//
void ClParser::parse_declare_values(Val decl, Function*)
{
    Val ty;
    if (safe_list_length(decl) == Fixnum::Encode(2))
    {
        ty = ty_expand(second(decl));
    }
    else
    {
        ty = cons(Qvalues, ty_expand_tylist(rest(decl)));
    } // if

    GetLexEnv()->SetTy(ty_and(ty, GetLexEnv()->GetTy()));
} // ClParser::parse_declare_values

} // Compiler
