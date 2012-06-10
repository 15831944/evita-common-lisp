#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - main
// gs_main.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_lisp.cpp#3 $
//
#include "./gs_lisp.h"

namespace Genesis
{

//////////////////////////////////////////////////////////////////////
//
// check_syntax
//
//  See Also:
//      ClParser::check_syntax
//
Val
check_syntax(Val form, int iMin, int iMax, LPCWSTR pwszSyntax)
{
    ASSERT(iMin >= 0);
    ASSERT(iMin <= iMax);

    Int n = Fixnum::Decode_(safe_list_length(form));

    if (n >= iMin && n <= iMax)
    {
        return nil;
    }

    if (n <= 0)
    {
        return list(Qerror,
            list(Qquote, parse_symbol(L"COMPILER::MALFORMED-FORM")),
            parse_symbol(L":SYNTAX"), make_string(pwszSyntax),
            parse_symbol(L":FORM"), list(Qquote, form) );
    }
    else
    {
        return list(Qerror,
            list(Qquote, parse_symbol(L"COMPILER::SYNTAX-ERROR")),
            parse_symbol(L":SYNTAX"), make_string(pwszSyntax),
            parse_symbol(L":FORM"), list(Qquote, form) );
    }
} // check_syntax

} // Genesis


namespace MiniLisp
{
// upcase
static void upcase(char16* pwsz)
{
    while (0 != *pwsz)
    {
        if (*pwsz >= 'a' && *pwsz <= 'z')
        {
            *pwsz = static_cast<char16>(*pwsz + 'A' - 'a');
        }
        pwsz++;
    } // while
} // upcase


//////////////////////////////////////////////////////////////////////
//
// ParseSymbol
//
Val parse_symbol(LPCWSTR pwchName, LPCWSTR pwchEnd)
{
    ASSERT(pwchName < pwchEnd);

    LPCWSTR pwchColon = NULL;
    {
        LPCWSTR pwchRunner = pwchName;
        while (pwchRunner < pwchEnd)
        {
            if (':' == *pwchRunner)
            {
                pwchColon = pwchRunner;
                break;
            }
            pwchRunner++;
        } // while
    } // pwchColon

    Val package;

    bool fInternal;

    if (NULL == pwchColon)
    {
        fInternal = true;

        package = TLV(ApackageA);

        if (! packagep(package))
        {
            package = PACKAGE_si;
        }
    }
    else
    {
        WCHAR wszPackageName[100];

        ::CopyMemory(
            wszPackageName,
            pwchName,
            (pwchColon - pwchName) * sizeof(WCHAR) );

        wszPackageName[pwchColon - pwchName] = 0;

        upcase(wszPackageName);

        if (0 == wszPackageName[0])
        {
            package = PACKAGE_keyword;
        }
        else
        {
            StackString oPackageName(wszPackageName);
            package = find_package(oPackageName.Encode());

            if (! packagep(package))
            {
                error(L"No such package: ~S", oPackageName.Encode());
            }
        } // if

        if (':' == pwchColon[1])
        {
            fInternal = true;
            pwchName = pwchColon + 2;
        }
        else
        {
            fInternal = false;
            pwchName = pwchColon + 1;
        }
    }

    StackString oName(pwchName, pwchEnd);
        upcase(oName);

    Val status;
    Val symbol = find_symbol(oName.Encode(), package, &status);

    if (nil != status)
    {
        return symbol;
    }

    if (fInternal)
    {
        return intern(static_cast<Val>(oName), package);
    }
    else
    {
        return export_intern(oName, package);
    }
} // parse_symbol

} // MiniLisp
