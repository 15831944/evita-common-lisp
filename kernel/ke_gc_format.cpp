#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Garbage Collector Format
// kernel/ke_gc_format.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_gc_format.cpp#3 $
//
#include "./ke_gc.h"

#include "../mini/mini_lisp.h"

namespace Kernel
{

namespace
{

// GcFormat
class GcFormat
{
    public: static void Run(const char16* pwsz, va_list args)
    {
        for (;;)
        {
            while ('~' != *pwsz)
            {
                if (0 == *pwsz) return;
                write_char(*pwsz);
                pwsz++;
            } // while

            pwsz++;

            char16 wchDirective = *pwsz++;

            switch (wchDirective)
            {
            case '%':
                ::OutputDebugString(L"\r\n");
                break;

            case 'C': case 'c':
            {
                char16 wsz[2];
                    wsz[0] = Character::ToCode(va_arg(args, Val));
                    wsz[1] = 0;
                ::OutputDebugString(wsz);
                break;
            } // c

            case 'D': case 'd':
                print_int(Gc::resolveForward(va_arg(args, Val)), 10);
                break;

            case 'S': case 's':
                print_object(va_arg(args, Val));
                break;

            case 'X': case 'x':
                print_int(Gc::resolveForward(va_arg(args, Val)), 16);
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch directive
        } // for
    } // run

    // print_int
    static void print_int(Val ival, int nBase)
    {
        if (! fixnump(ival))
        {
            print_object(ival);
            return;
        }

        if (Fixnum::Encode(0) == ival)
        {
            write_char('0');
            return;
        }

        Int n = Fixnum::Decode_(ival);
        if (n < 0)
        {
            write_char('-');
            n = -n;
        }

        print_int_aux(n, nBase, 1);
    } // print_int

    static UInt print_int_aux(UInt n, uint nBase, UInt q)
    {
        if (n >= nBase)
        {
            UInt qq = print_int_aux(n / nBase, nBase, q);
            print_int_aux(n % qq, nBase, 1);
            return qq;
        }
        else
        {
            if (n < 10)
            {
                write_char(static_cast<char16>(n + '0'));
            }
            else
            {
                write_char(static_cast<char16>(n + 'A' - 10));
            }

            return q * nBase;
        }
    } // print_int_aux

    // print_object
    static void print_object(Val objin)
    {
        Val obj = Gc::resolveForward(objin);

        if (nil == obj)
        {
            write_string(L"NIL");
            return;
        }

        if (fixnump(obj))
        {
            print_int(obj, 10);
            return;
        }

        if (consp(obj))
        {
            write_string(L"(");
            print_object(car(obj));

            Val runner = obj;

            for (;;)
            {
                runner = Gc::resolveForward(cdr(runner));

                if (nil == runner)
                {
                    write_char(0x29);
                    break;
                }

                if (! consp(runner))
                {
                    write_string(L" . ");
                    print_object(runner);
                    write_string(L")");
                    break;
                }

                write_char(' ');
                print_object(car(runner));
            } // for
            return;
        } // cons

        if (functionp(obj))
        {
            write_string(L"#<Function ");

            Val name = Gc::resolveForward(
                obj->Decode<NativeCodeFunction>()->m_name );

            if (! name->Is<Storage>())
            {
                print_object(name);
            }
            else
            {
                Record* pStorage = name->Decode<Record>();

                print_object(
                    reinterpret_cast<StandardGenericFunction*>(pStorage)->
                        m_name );
            }

            char16 wsz[20];
                ::wsprintf(wsz, L" %p>", obj);
            write_string(wsz);
            return;
        } // function

        if (simple_string_p(obj))
        {
            write_char(0x22);
            write_string(obj->Decode<SimpleString>()->GetElements());
            write_char(0x22);
            return;
        } // simple_string

        if (symbolp(obj))
        {
            Val pkg = Gc::resolveForward(obj->Decode<Symbol>()->m_package);
            if (nil == pkg)
            {
                write_string(L"#:");
            }
            else if (PACKAGE_keyword == pkg)
            {
                write_string(L":");
            }

            Val name = Gc::resolveForward(obj->Decode<Symbol>()->m_name);
            write_string(name->Decode<SimpleString>()->GetElements());
            return;
        } // symbol

        {
            write_string(L"#<");

            Val klass = class_of(obj);
                klass = Gc::resolveForward(klass);

            print_object(klass->Decode<Class>()->m_name);

            char16 wsz[20];
                ::wsprintf(wsz, L" %p>", obj);
            write_string(wsz);
        }
    } // print_obj

    // write_char
    static void write_char(char16 wch)
    {
        char16 wsz[2];
            wsz[0] = wch;
            wsz[1] = 0;
        ::OutputDebugString(wsz);
    } // write_char

    // write_string
    static void write_string(const char16* pwsz)
        { ::OutputDebugString(pwsz); }
}; // GcFormat

} // namespace


// Gc::Format
void Gc::Format(const char16* pwszControl, ...)
{
    va_list args;
    va_start(args, pwszControl);
        GcFormat::Run(pwszControl, args);
    va_end(args);
} // Gc::Format


// Gc::Printf
void Gc::Printf(const char16* pwszFormat, ...)
{
    va_list args;

    char16 wsz[1024];
    {
        va_start(args, pwszFormat);
            ::wvsprintfW(wsz, pwszFormat, args);
        va_end(args);
    } // wsz

    ::OutputDebugString(wsz);
} // Gc::Printf


void dbg_format(const char16* pwsz, ...)
{
    va_list args;
    va_start(args, pwsz);
        GcFormat::Run(pwsz, args);
    va_end(args);
} // dbg_format

} // Kernel
