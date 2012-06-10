#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 4 Classes and Types
// compiler/cl/cl_04_type.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_04_type.cpp#15 $
// Description:
//  This file contains following parsers:
//
#include "./cl_defs.h"

namespace Compiler
{

static bool quote_form_p(Val x)
    { return consp(x) && car(x) == Qquote && consp(cdr(x)) && nil == cddr(x); }

static Val quote(Val x)
    { return list(Qquote, x); }


// get_typep_alt -- get alternate typep form
static Val get_typep_alt(Val form)
{
    Val ty = third(form);
        if (! quote_form_p(ty)) return nil;

    ty = ty_expand(second(ty));
        if (! consp(ty)) return nil;

    if (car(ty) == Qcons)
    {
        Ty carty = ty_t;
        Ty cdrty = ty_t;
        switch (Fixnum::Decode_(safe_list_length(ty)))
        {
        case 1: // (cons)
            break;
        case 2: // (cons x)
            carty = second(ty);
            break;
        case 3:
            carty = second(ty);
            cdrty = third(ty);
            break;
        default:
            warn(L"Invalid type specifier: ~S", ty);
            return nil;
        } // switch len

        if (carty == QA) carty = ty_t;
        if (cdrty == QA) cdrty = ty_t;

        if (carty == ty_t && cdrty == ty_t) return Qcons;   // (cons t t)

        // (let ((x form))
        //   (and (consp x) (typep (car x) 'car) (typep (cdr x) 'cdr)) )
        Val var = make_symbol(L"X");

        if (carty == ty_t)
        {
            return list(Qlet, list(list(var, second(form))), list(Qand,
                list(Qtypep, var, quote(Qcons)),
                list(Qtypep, list(Qcdr, var), quote(cdrty)) ) );
        }

        if (cdrty == ty_t)
        {
            return list(Qlet, list(list(var, second(form))), list(Qand,
                list(Qtypep, var, quote(Qcons)),
                list(Qtypep, list(Qcar, var), quote(carty)) ) );
        }

        return list(Qlet, list(list(var, second(form))), list(Qand,
            list(Qtypep, var, quote(Qcons)),
            list(Qtypep, list(Qcar, var), quote(carty)),
            list(Qtypep, list(Qcdr, var), quote(cdrty)) ) );
    } // cons

    if (car(ty) == Qeql)
    {
        // (typep a '(eql x)) = (eql a 'x)
        return list(Qeql, second(form), quote(second(ty)));
    } // eql

    if (car(ty) == Qnot)
    {
        // (typep a '(not ty)) = (not (typep a 'ty))
        return list(Qnot,
            list(Qtypep, second(form), quote(second(ty))) );
    } // not

    if (car(ty) == Qand || car(ty) == Qor)
    {
        // (typep a '(and ty1 ...))
        //  = (let ((x a)) (and (typep x 'ty1) ...))
        Val var = make_symbol(L"X");
        Val forms = list(car(ty));
        Val tail = forms;
        foreach (EnumList, oEnum, rest(ty))
        {
            Val typep = list(Qtypep, var, quote(oEnum.Get()));
            tail = setf_cdr(list(typep), tail);
        } // for each elt
        return list(Qlet, list(list(var, second(form))), forms);
    }

    if (car(ty) == Qmember)
    {
        // (typep a '(member ...)) = (member a '(...))
        return list(Qmember, second(form), quote(rest(ty)));
    }

    if (car(ty) == Qsatisfies)
    {
        // (typep a '(satisfies fn)) = (values (fn a))
        return list(Qvalues, list(second(ty), second(form)));
    }

    return nil;
} // get_typep_alt


//////////////////////////////////////////////////////////////////////
//
// coerce
//      (coerce x 'character)       => (character x)
//      (coerce x 'double-float)    => (float x 0d0)
//      (coerce x 'single-float)    => (float x)
//
define_parser(coerce)
{
    CHECK_SYNTAX(3, 3, "(coerce x type)");

    Operand* pSx = parseArg(Qtypep, ty_t, 0, second(form));
        if (Obj_Unreachable == pSx) return useless_form(form);

    Operand* pSy = parseArg(Qtypep, ty_t, 1, third(form));
        if (Obj_Unreachable == pSy) return useless_form(form);

    if (ty_void == pExpect->Type) return ignore_form(form);

    if (pSx->Is<Literal>() && pSy->Is<Literal>())
    {
        Val x = funcall(
            Qcoerce,
            pSx->StaticCast<Literal>()->GetDatum(),
            pSy->StaticCast<Literal>()->GetDatum() );
        return parseLiteral(pExpect, x);
    }

    if (pSy->Is<Literal>())
    {
        Ty ty = ty_expand(pSy->StaticCast<Literal>()->GetDatum());
        Ty sxty = pSx->GetTy();

        if (Subtypep_Yes == ty_subtypep(sxty, ty))
        {
            return emitLinkage(emitCast(pExpect, form, pSx));
        }

        if (ty == ty_character)
        {
            // Note: function character expectes character designator
            // instead of string designator. However, character
            // desginator is expressed with satisfies and it doesn't
            // help compiler.
            if (nil == ty_and(sxty, ty_expand(Qstring_designator)))
            {
                ExpectArgument oExpect(Qcoerce, 0, Qstring_designator);
                unexpected_type(&oExpect, sxty, second(form));
            }

            return emitLinkage(emitCall(pExpect, form, ty, Qcharacter, pSx));
        } // character

        if (ty == ty_double_float)
        {
            if (nil == ty_and(sxty, ty_real))
            {
                ExpectArgument oExpect(Qcoerce, 0, ty_real);
                unexpected_type(&oExpect, sxty, second(form));
            }

            return emitLinkage(emitCall(pExpect, form, ty, Qfloat,
                        pSx, symbol_value(Qpi) ) );
        } // double_float

        if (ty == ty_single_float)
        {
            if (nil == ty_and(sxty, ty_real))
            {
                ExpectArgument oExpect(Qcoerce, 0, ty_real);
                unexpected_type(&oExpect, sxty, second(form));
            }

            return emitLinkage(emitCall(pExpect, form, ty, Qfloat, pSx));
        } // single_float
    } // if literal

    return emitLinkage(emitCall(pExpect, form, ty_t, Qcoerce, pSx, pSy));
} // coerce


//////////////////////////////////////////////////////////////////////
//
// typep
//      TYPEP  bool %b1 <= %r2 'type
//      SELECT ty   %r3 <= %b4 't 'nil
//
define_parser(typep)
{
    CHECK_SYNTAX(3, 3, "(typep x type)");

    {
        Val altform = get_typep_alt(form);
        if (nil != altform) return parseForm(pExpect, altform);
    }

    Operand* pSx = parseArg(Qtypep, ty_t, 0, second(form));
        if (Obj_Unreachable == pSx) return useless_form(form);

    Operand* pSy = parseArg(Qtypep, ty_t, 1, third(form));
        if (Obj_Unreachable == pSy) return useless_form(form);

    if (ty_void == pExpect->Type) return ignore_form(form);

    if (pSx->Is<Literal>() && pSy->Is<Literal>())
    {
        Val x = funcall(
            Qtypep,
            pSx->StaticCast<Literal>()->GetDatum(),
            pSy->StaticCast<Literal>()->GetDatum() );
        return parseLiteral(pExpect, x);
    }

    if (pSy->Is<Literal>())
    {
        Val ty = pSy->StaticCast<Literal>()->GetDatum();
        if (classp(ty))
        {
            ty = class_name(ty);
        }
        else
        {
            if (! (consp(ty) || symbolp(ty)))
            {
                warn(L"Invalid type-specifier ~S.", ty);
                return Obj_Nil;
            }

            ty = ty_expand(ty);
        }

        return processTypep(pExpect, form, ty, pSx);
    } // if

    return emitLinkage(emitCall(pExpect, form, ty_t, Qtypep, pSx, pSy));
} // typep


// parseTypePredicate
Operand*
ClParser::parseTypePredicate(const Expect* pExpect, Val form, Ty ty)
{
    {
        Int n = Fixnum::Decode_(safe_list_length(form));
        if (2 != n)
        {
            if (n < 0) return malformed_form(form);

            if (n < 2)
            {
                warn(L"Too few arguments for ~S.", car(form));
                return Obj_Nil;
            }

            {
                warn(L"Too many arguments for ~S.", car(form));
                return Obj_Nil;
            }
        } // if
    }

    Operand* pSx = parseArg(car(form), ty_t, 0, cadr(form));
        if (Obj_Unreachable == pSx) return useless_form(form);
        //if (ty_void == oExpect.Type) return ignore_form(form);
    return processTypep(pExpect, form, ty, pSx);
} // ClParser::parseTypePredicate


//////////////////////////////////////////////////////////////////////
//
// ClParser::processTypep
//
Operand*
ClParser::processTypep(const Expect* pExpect, Val form, Ty ty, Operand* pSx)
{
    if (Qnull == ty)
    {
        Bool* pBx = new Bool();
        emitInsn(new EqInsn(pBx, pSx, Obj_Nil));

        Register* pRd = new Register();
        emitInsn(new SelectInsn(pRd, pBx, Obj_True, Obj_Nil));

        return emitLinkage(pRd);
    }

    if (nil == ty || t == ty)
    {
        useless_form(form);
        return parseLiteral(pExpect, t);
    }

    Bool* pBx = new Bool();
    emitInsn(new TypepInsn(pBx, pSx, ty));

    Operand* pTrue;
    {
        if (! ty_typep(nil, pSx->GetTy()))
        {
            pTrue = pSx;
        }
        else
        {
            pTrue = Obj_True;
        }
    }

    Register* pRd = new Register();
        emitInsn(new SelectInsn(pRd, pBx, pTrue, Obj_Nil));

    return emitLinkage(pRd);
} // ClParser::processTypep

} // Compiler
