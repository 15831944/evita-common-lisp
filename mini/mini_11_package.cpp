#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - lisp
// genesis/geneis_lisp.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_11_package.cpp#5 $
//

#include "./mini_lisp.h"

using namespace MiniLisp;

namespace
{

const Int Rehash_Threshold = 65;    // rehash when 60% filled.
const Int Rehash_Size      = 120;   // grow 20%.

static Val internal_find_package(Val);

static bool package_need_rehash_p(Val);
static Val package_find(Val, Val);
static Val package_rehash(Val);
static void package_remove(Val, Val);


//////////////////////////////////////////////////////////////////////
//
// Find package
//
Val internal_find_package(Val name)
{
    name = to_string(name);

    foreach (EnumList, oEnum, VAR(Aall_packagesA))
    {
        Val package = oEnum.Get();

        foreach (EnumList, oEnum, package_names(package))
        {
            if (0 == string_cs_cmp(oEnum.Get(), name))
            {
                return package;
            }
        } // for each name
    } // for each package

    return nil;
} // internal_find_package


//////////////////////////////////////////////////////////////////////
//
// Need rehash?
//
bool package_need_rehash_p(Val vector)
{
    Int iLength = Fixnum::Decode_(length(vector));
    Int iCount  = Fixnum::Decode_(svref(vector, Fixnum::Encode(0)));
    return iCount * 100 > iLength * 65;
} // package_need_rehash_p


//////////////////////////////////////////////////////////////////////
//
// Find symbol
//
Val package_find(Val vector, Val name)
{
    PackageImpl::EnumAll::Arg oArg(
        vector,
        Fixnum::Decode_(hash_string(name)) );

    foreach (PackageImpl::EnumAll, oEnum, oArg)
    {
        Val present = oEnum.Get();
        if (PackageImpl::Free() == present)
        {
            return present;
        }
        else if (PackageImpl::Removed() != present)
        {
            if (0 == string_cs_cmp(symbol_name(present), name))
            {
                return present;
            }
        } // if
    } // for
    return PackageImpl::Free();
} // package_find


//////////////////////////////////////////////////////////////////////
//
// package_rehash
//  Enlarge current vector to 120%.
//
Val package_rehash(Val oldvec)
{
    ASSERT(simple_vector_p(oldvec));

    Val newvec = make_vector(
        truncate_xx(mul_xx(length(oldvec), Rehash_Size), 100) );
    // Note: We assume make_vector returns zero filled simple-vector
    // instead of nil filled.

    foreach (PackageImpl::Enum, oEnum, oldvec)
    {
        package_put(newvec, oEnum.Get());
    } // for each symbol

    return newvec;
} // package_rehash


//////////////////////////////////////////////////////////////////////
//
// Remove symbol from table
//
void package_remove(Val vector, Val symbol)
{
    ASSERT(simple_vector_p(vector));
    ASSERT(symbolp(symbol));

    PackageImpl::EnumAll::Arg oArg(
        vector,
        symbol_hash_code(symbol) );

    foreach (PackageImpl::EnumAll, oEnum, oArg)
    {
        Val present = oEnum.Get();
        if (symbol == present)
        {
            oEnum.Remove();
            break;
        }
        else if (PackageImpl::Free() == present)
        {
            // symbol isn't in package.
            break;
        }
    } // for each symbol
} // package_remove

} // namespace

namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// Extern
//
Val export_intern(LPCWSTR pwszName, Val package)
{
    StackString oName(pwszName);
    Val name = oName.Encode();

    Package* pPackage = package->Decode<Package>();

    Val xStatus;
    Val symbol = find_symbol(name, package, &xStatus);

    if (Kexternal == xStatus)
    {
        return symbol;
    }

    if (nil == xStatus)
    {
        name = make_string(pwszName);

        symbol = make_symbol(name);
        symbol->Decode<Symbol>()->m_package = package;
    }
    else if (Kinternal == xStatus)
    {
        package_remove(pPackage->m_internal_table, symbol);
    }
    else if (Kinherited == xStatus)
    {
        // nothing to do
    }
    else
    {
        error(L"find_symbol returns unexpected value: ~S", xStatus);
    }

    if (package_need_rehash_p(pPackage->m_external_table))
    {
        pPackage->m_external_table = package_rehash(
            pPackage->m_external_table );
    }

    package_put(pPackage->m_external_table, symbol);

    return symbol;
} // export_intern


//////////////////////////////////////////////////////////////////////
//
// Internal Make Package
//
// Note:
//  This function doesn't register newly created package. Caller should
//  push new package object to si::*all-packages*.
//
Val internal_make_package(
    Val   names,
    Val   external_size,
    Val   internal_size )
{
    if (symbolp(names))
    {
        names = list(symbol_name(names));
    }
    else if (simple_string_p(names))
    {
        names = list(names);
    }
    else if (consp(names))
    {
        Cons  oAnchor;
        Cons* pLast = &oAnchor;

        do
        {
            Val name = car(names);

            if (simple_string_p(name))
            {
                // nothing to do
            }
            else if (symbolp(name))
            {
                name = symbol_name(name);
            }
            else
            {
                error(make_type_error(name, Qstring));
            }

            Val xCons = list(name);
            pLast->m_cdr = xCons;
            pLast = xCons->Decode<Cons>();

            names = cdr(names);
        } while (consp(names));

        names = oAnchor.m_cdr;
    }
    else
    {
        error(make_type_error(names, list(Qor, Qstring, Qlist)));
    }

    if (cmp_xx(external_size, 30) <= 0)
    {
        external_size = Fixnum::Encode(30);
    }

    if (cmp_xx(internal_size, 30) <= 0)
    {
        internal_size = Fixnum::Encode(30);
    }

    Val package =  MiniThread::Get()->AllocRecord(CLASSD_package);

    Package* pPackage = package->Decode<Package>();
        pPackage->m_names   = names;

        pPackage->m_external_table =
            make_vector(external_size);

        pPackage->m_internal_table =
            make_weak_vector(internal_size, Qpackage);

        pPackage->m_use_list            = nil;
        pPackage->m_used_by_list        = nil;
        pPackage->m_shadowing_symbols   = nil;
        pPackage->m_protect             = nil;

        pPackage->m_thread = nil;
        pPackage->m_state  = nil;

    return package;
} // internal_make_package


//////////////////////////////////////////////////////////////////////
//
// Get pretty name of pacakge
//
Val package_pretty_name(Val package)
{
    Package* pPackage = package->Decode<Package>();

    Val name = car(pPackage->m_names);

    if (consp(cdr(pPackage->m_names)))
    {
        name = cadr(pPackage->m_names);
    }

    return name;
} // package_pretty_name


//////////////////////////////////////////////////////////////////////
//
// Put symbol onto table
//
Val package_put(Val vector, Val symbol)
{
    ASSERT(symbolp(symbol));

    PackageImpl::EnumAll::Arg oArg(
        vector,
        symbol_hash_code(symbol) );

    PackageImpl::Slot* pHome = NULL;

    PackageImpl::EnumAll oEnum(oArg);
    while (! oEnum.AtEnd())
    {
        Val present = oEnum.Get();
        if (symbol == present)
        {
            return symbol;
        }

        if (PackageImpl::Free() == present)
        {
            if (NULL == pHome) pHome = oEnum.GetRef();
            break;
        }

        if (PackageImpl::Removed() == present)
        {
            if (NULL == pHome) pHome = oEnum.GetRef();
        }

        oEnum.Next();
    } // for each slot

    ASSERT(NULL != pHome);

    pHome->m_key = symbol;
    oEnum.Add();
    return symbol;
} // package_put

} // MiniLisp

namespace CommonLisp
{

Val find_package(Val name)
{
    if (packagep(name))
    {
        return name;
    }

    with_shared_latch(value_cell_value(VAR_Aall_packages_latchA));
    return internal_find_package(name);
} // find_package


//////////////////////////////////////////////////////////////////////
//
// Find accessible symbol
//
Val find_symbol(
    Val   name,
    Val   package,
    Val*  out_status )
{
    ASSERT(simple_string_p(name));
    ASSERT(packagep(package));
    ASSERT(NULL != out_status);

    Package* pPackage = package->Decode<Package>();

    {
        Val present = package_find(pPackage->m_internal_table, name);

        if (symbolp(present))
        {
            *out_status = Kinternal;
            return present;
        }
    }

    {
        Val present = package_find(pPackage->m_external_table, name);

        if (symbolp(present))
        {
            *out_status = Kexternal;
            return present;
        }
    }

    for (
        Val xRunner = pPackage->m_use_list;
        nil != xRunner;
        xRunner = cdr(xRunner) )
    {
        Package* pInherited = car(xRunner)->Decode<Package>();

        Val present = package_find(pInherited->m_external_table, name);

        if (symbolp(present))
        {
            *out_status = Kinherited;
            return present;
        }
    } // for

    *out_status = nil;
    return nil;
} // find_symbol


//////////////////////////////////////////////////////////////////////
//
// Intern
//
Val intern(LPCWSTR pwszName, Val package)
{
    StackString oName(pwszName);
    Val xStatus;
    return intern(oName.Encode(), package, &xStatus);
} // intern


//////////////////////////////////////////////////////////////////////
//
// Intern
//
Val intern(Val name, Val package, Val* out_status)
{
    ASSERT(NULL != out_status);

    if (! packagep(package))
    {
        Val pkgname = package;
        package = find_package(pkgname);
        if (! packagep(package)) error(L"No such package: ~S", pkgname);
    }

    {
        Val present = find_symbol(name, package, out_status);
        if (nil != *out_status)
        {
            return present;
        }
    }

    if (! name->IsHeap())
    {
        name = make_string(
            name->Decode<SimpleString>()->GetElements(),
            Fixnum::Decode_(length(name)) );
    }

    Val symbol = make_symbol(name);
        symbol->Decode<Symbol>()->m_package = package;

    Package* pPackage = package->Decode<Package>();

    if (PACKAGE_keyword == package)
    {
        if (package_need_rehash_p(pPackage->m_external_table))
        {
            pPackage->m_external_table = package_rehash(
                pPackage->m_external_table );
        }

        package_put(pPackage->m_external_table, symbol);
    }
    else
    {
        if (package_need_rehash_p(pPackage->m_internal_table))
        {
            pPackage->m_internal_table = package_rehash(
                pPackage->m_internal_table );
        }

        package_put(pPackage->m_internal_table, symbol);
    } // if

    return symbol;
} // intern


// make_package
Val make_package(Val name, Val external_size, Val internal_size)
{
    with_exclusive_latch(value_cell_value(VAR_Aall_packages_latchA));

    if (! consp(name))
    {
        if (nil != internal_find_package(name))
        {
            error(L"Package ~S already exists.", name);
        }
    }
    else
    {
        for (Val runner = name; ! endp(runner); runner = cdr(runner))
        {
            Val nickname = car(runner);
            Val found = internal_find_package(nickname);
            if (nil != found)
            {
                error(L"Nickname ~S used by ~S.", nickname, found);
            }
        } // for
    }

    Val package = internal_make_package(name, external_size, internal_size);

    setf_value_cell_value(
        cons(package, value_cell_value(VAR_Aall_packagesA)),
        VAR_Aall_packagesA );

    return package;
} // make_package


// use_package
Val use_package(Val thing, Val package)
{
    Val to_use = find_package(thing);

    if (! packagep(to_use)) error(L"No such package: ~A", thing);

    Package* pPackage = package->Decode<Package>();

    for (
        Val runner = pPackage->m_use_list;
        ! endp(runner);
        runner = cdr(runner) )
    {
        if (car(runner) == to_use)
        {
            return package;
        }
    } // for each used package

     pPackage->m_use_list = cons(to_use, pPackage->m_use_list);

     return package;
} // use_package

} // CommonLisp
