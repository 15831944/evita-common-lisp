#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - Bootstrap Compiler - Format
// compiler/cm_format.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_format.cpp#7 $
//
//
//      ~S      Object reference
//      ~:S     Object definition
//      ~W      lisp val
//      ~%      Terminate line
//      ~:%     Terminate line with <br/>
//
#include "./cm_defs.h"

#include "./cm_base.h"
#include "./cm_fns.h"

#include "../../kernel/ke_format.h"

namespace Compiler
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// Formatter
//
class Formatter : public Kernel::Formatter
{
    Val m_stream;

    virtual void print_object(Val obj)
        { cm_print_object(obj, m_stream); }

    virtual void write_char(char16 wch)
        { CommonLisp::write_char(wch, m_stream); }

    virtual void write_string(const char16* pwsz)
        { CommonLisp::write_string(pwsz, m_stream); }

    virtual void write_string(const char16* s, const char16* e)
        { CommonLisp::write_string(s, e - s, m_stream); }

    public: void Run(Val stream, LPCWSTR pwszFormat, va_list args)
    {
        m_stream = stream;
        Kernel::Formatter::Run(pwszFormat, args);
    } // Run
}; // Formatter

}; // namespace



//////////////////////////////////////////////////////////////////////
//
// Format
//
void cm_formatv(Val stream, LPCWSTR pwszFormat, va_list args)
{
    Formatter oFormatter;
        oFormatter.Run(stream, pwszFormat, args);
} // cm_format


void cm_format(Val stream, LPCWSTR pwszFormat, ... )
{
    va_list args;
    va_start(args, pwszFormat);
    cm_formatv(stream, pwszFormat, args);
    va_end(args);
} // cm_format


static UInt print_uint(UInt n, Val s, uint nBase, UInt q)
{
    if (n >= nBase)
    {
        UInt qq = print_uint(n / nBase, s, nBase, q);
        print_uint(n % qq, s, nBase, 1);
        return qq;
    }
    else
    {
        if (n < 10)
        {
            write_char(static_cast<char16>(n + '0'), s);
        }
        else
        {
            write_char(static_cast<char16>(n + 'A' - 10), s);
        }

        return q * nBase;
    }
} // print_uint


// print_int
static void print_int(Val ival, Val s, int nBase)
{
    if (! fixnump(ival))
    {
        cm_print_object(ival, s);
        return;
    }

    if (Fixnum::Encode(0) == ival)
    {
        write_char('0', s);
        return;
    }

    Int n = Fixnum::Decode_(ival);
    if (n < 0)
    {
        write_char('-', s);
        n = -n;
    }

    print_uint(n, s, nBase, 1);
} // print_int

static void write_escaped_string(const char16* pwsz, Val s)
{
    while (0 != *pwsz)
    {
        if ('<' != *pwsz)
        {
            write_char(*pwsz, s);
        }
        else
        {
            write_string(L"&lt;", s);
        }
        pwsz++;
    } // while
} // write_escaped_string


// cm_print_object
void cm_print_object(Val obj, Val stream)
{
    if (nil == obj)
    {
        write_string(L"NIL", stream);
        return;
    }

    if (fixnump(obj))
    {
        print_int(obj, stream, 10);
        return;
    }

    if (symbolp(obj))
    {
        Symbol* p = obj->Decode<Symbol>();

        if (p->m_package == PACKAGE_keyword)
        {
            write_char(':', stream);
        }
        else if (p->m_package == nil)
        {
            write_string(L"#:", stream);
        } // if

        write_escaped_string(
            p->m_name->Decode<SimpleString>()->GetElements(),
            stream );
        return;
    }

    if (simple_string_p(obj))
    {
        if (nil != TLV(Aprint_escapeA)) write_char(0x22, stream);

        write_escaped_string(
            obj->Decode<SimpleString>()->GetElements(),
            stream );

        if (nil != TLV(Aprint_escapeA)) write_char(0x22, stream);
        return;
    }

    if (consp(obj))
    {
        write_string(L"(", stream);
        print_object(car(obj), stream);

        Val runner = obj;

        for (;;)
        {
            runner = cdr(runner);

            if (nil == runner)
            {
                write_char(0x29, stream);
                break;
            }

            if (! consp(runner))
            {
                write_string(L" . ", stream);
                print_object(runner, stream);
                write_string(L")", stream);
                break;
            }

            write_char(' ', stream);
            print_object(car(runner), stream);
        } // for
        return;
    } // cons

    if (characterp(obj))
    {
        char16 wch = Character::ToCode(obj);

        char16 wsz[20];
        if (wch >= 0x21 && wch <= 0x7E)
        {
            if (wch == '<')
            {
                ::lstrcpyW(wsz, L"#\\&lt;");
            }
            else
            {
                ::wsprintf(wsz, L"#\\%c", wch);
            }
        }
        else
        {
            ::wsprintf(wsz, L"#\\u%04X", wch);
        }
        write_string(wsz, stream);
        return;
    } // character

    {
        Class* pClass = class_of(obj)->Decode<Class>();

        write_string(L"#&lt;", stream);

        write_string(
            pClass->m_name->Decode<Symbol>()->
                m_name->Decode<SimpleString>()->GetElements(),
            stream );

        char16 wsz[100];
            ::wsprintfW(wsz, L" %p>", obj);
        write_string(wsz, stream);
    }
} // print_object

} // Compiler
