#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 22 Printer
// genesis_lisp_22_printer.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_22_printer.cpp#7 $
//

#include "./mini_lisp.h"

namespace MiniLisp
{

Val print_integer(Val, Val);
Val print_double_float(Val, Val);
Val print_double_float_complex(Val, Val);
Val print_single_float(Val, Val);
Val print_single_float_complex(Val, Val);
Val print_function(Val, Val);
Val print_record(Val, Val);
Val print_character(Val, Val);
Val print_cons(Val, Val);
Val print_simple_string(Val, Val);


Val print_object_default(Val obj, Val stream)
{
    format(stream, L"#<~A ~X~X>",
        type_of(obj),
        Fixnum::Encode(obj->ToInt() >> 4),
        Fixnum::Encode(obj->ToInt() & 15) );
    return obj;
} // print_object_default


// print character
Val print_character(Val obj, Val stream)
{
    char16 wch = Character::ToCode(obj);

    WCHAR wsz[20];

    wsz[0] = '#';
    wsz[1] = '\\';

    if (wch > 0x20 && wch < 0x7F)
    {
        wsz[2] = wch;
        wsz[3] = 0;
    }
    else
    {
        static char16 xdigit[16+1] = L"0123456789ABCDEF";

        wsz[2] = 'u';
        wsz[3] = xdigit[(wch >> 12) & 15];
        wsz[4] = xdigit[(wch >>  8) & 15];
        wsz[5] = xdigit[(wch >>  4) & 15];
        wsz[6] = xdigit[(wch >>  0) & 15];
        wsz[7] = 0;
    }

    write_string(wsz, stream);
    return obj;
} // print_character

// print cons
Val print_cons(Val obj, Val stream)
{
    write_string(L"(", stream);

    for (;;)
    {
        print_object(car(obj), stream);

        obj = cdr(obj);

        if (nil == obj)
        {
            break;
        }

        if (! consp(obj))
        {
            write_string(L" . ", stream);
            print_object(obj, stream);
            break;
        }

        write_string(L" ", stream);
    } // for

    write_string(L")", stream);
    return obj;
} // print_cons


//////////////////////////////////////////////////////////////////////
//
// print fixnum
//
Val print_fixnum(Val obj, Val stream)
{
    char16  wszDigit[100];
    char16* pwszRunner = &wszDigit[lengthof(wszDigit)];

    *--pwszRunner = 0;

    Int iBase = Fixnum::Decode_( MiniThread::Get()->GetTlv(TLV_Aprint_baseA));

    if (nil !=  MiniThread::Get()->GetTlv(TLV_Aprint_radixA))
    {
        if (10 == iBase)
        {
            *--pwszRunner = '.';
        }
    }

    if (Fixnum::Encode(0) == obj)
    {
        *--pwszRunner = '0';
    }
    else
    {
        Int iVal = Fixnum::Decode_(obj);

        bool fSign = iVal < 0;
        if (fSign)
        {
            iVal = -iVal;
        }

        if (iBase < 10)
        {
            while (0 != iVal)
            {
                Int iMod = iVal % iBase;
                iVal /= iBase;

                *--pwszRunner = static_cast<char16>(iMod + '0');
            } // while
        }
        else
        {
            while (0 != iVal)
            {
                Int iMod = iVal % iBase;
                iVal /= iBase;

                if (iMod < 10)
                {
                    *--pwszRunner = static_cast<char16>(iMod + '0');
                }
                else
                {
                    *--pwszRunner = static_cast<char16>(iMod + 'A' - 10);
                }
            } // while
        }

        if (fSign)
        {
            *--pwszRunner = '-';
        }
    } // if

    if (nil !=  MiniThread::Get()->GetTlv(TLV_Aprint_radixA) && 10 != iBase)
    {
        switch (iBase)
        {
        case 2:
            *--pwszRunner = 'b';
            break;
        case 8:
            *--pwszRunner = 'o';
            break;
        case 16:
            *--pwszRunner = 'x';
            break;
        default:
            *--pwszRunner = 'r';
            if (iBase < 10)
            {
                *--pwszRunner = static_cast<char16>(iBase + '0');
            }
            else
            {
                *--pwszRunner = static_cast<char16>(iBase % 10 + '0');
                *--pwszRunner = static_cast<char16>(iBase / 10 + '0');
            }
            break;
        } // switch iBase

        *--pwszRunner = '#';
    }

    write_string(pwszRunner, stream);
    return obj;
} // print_fixnum

// print_package
Val print_package(Val obj, Val stream)
{
    format(stream, L"#<Package ~A Ext=~D Int=~D>",
        package_pretty_name(obj),
        svref(obj->Decode<Package>()->m_external_table, Fixnum::Encode(0)),
        svref(obj->Decode<Package>()->m_internal_table, Fixnum::Encode(0)) );
    return obj;
} // print_package


// print simple-string
Val print_simple_string(Val obj, Val stream)
{
    if (nil ==  MiniThread::Get()->GetTlv(TLV_Aprint_escapeA))
    {
        write_string(obj, stream);
    }
    else
    {
        write_string(L"\x22", stream);
        write_string(obj, stream);
        write_string(L"\x22", stream);
    }
    return obj;
} // print_simple_string

// print symbol
Val print_symbol(Val obj, Val stream)
{
    if (nil !=  MiniThread::Get()->GetTlv(TLV_Aprint_readablyA) ||
        nil !=  MiniThread::Get()->GetTlv(TLV_Aprint_escapeA) )
    {
        Val package = symbol_package(obj);
        if (PACKAGE_keyword == package)
        {
            write_string(L":", stream);
        }
        else if (nil == package)
        {
            write_string(L"#:", stream);
        }
        else
        {
            Val status;
            Val present = find_symbol(
                symbol_name(obj),
                TLV(ApackageA),
                &status );

            if (present != obj)
            {
                write_string(package_pretty_name(package), stream);

                Val status;
                find_symbol(symbol_name(obj), package, &status);
                if (Kexternal == status)
                {
                    write_string(L":", stream);
                }
                else
                {
                    write_string(L"::", stream);
                }
            }
        } // if
    } // if

    write_string(symbol_name(obj), stream);

    return obj;
} // print_symbol

} // MiniLisp

namespace CommonLisp
{

//////////////////////////////////////////////////////////////////////
//
// Print Object
//
Val print_object(Val obj, Val stream)
{
    stream = ensure_output_stream(stream);

    if (nil != obj && obj->GetTag4() == Null::Tag)
    {
        format(stream, L"#<Bad-Object ~X~X>",
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        return obj;
    }

    if (fixnump(obj))
    {
        print_fixnum(obj, stream);
        return obj;
    }

    if (characterp(obj))
    {
        print_character(obj, stream);
        return obj;
    }

#if 0
    // FIXME: 2007-01-23: Until we identify objects in C-stack, we turn off
    // this safety check. :-<
    if (obj->ToInt() <  Memory::GetStart()->ToInt() ||
        obj->ToInt() >= Memory::GetEnd()->ToInt() )
    {
        format(stream, L"#<Bad-Object ~X~X>",
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        return obj;
    }
#endif

    if (symbolp(obj))
    {
        print_symbol(obj, stream);
    }
    else if (consp(obj))
    {
        print_cons(obj, stream);
    }
    else if (obj->Is<Record>())
    {
        print_record(obj, stream);
    }
    else if (obj->Is<NativeCodeFunction>())
    {
        print_function(obj, stream);
    }
    else
    {
        write_string(L"#<Unprintable>", stream);
    }

    return obj;
} // print_object

} // CommonLisp
