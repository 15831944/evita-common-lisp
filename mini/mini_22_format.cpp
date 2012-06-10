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
// @(#)$Id: //proj/evcl3/mainline/mini/mini_22_format.cpp#9 $
//
#include "./mini_lisp.h"

namespace
{

const char16* k_rgpwszCardinal[11] =
{
    L"zero", L"one",  L"two",   L"three", L"four",
    L"five", L"six",  L"seven", L"eight", L"nine",
    L"ten"
}; // k_rgpwszCardinal

const char16* k_rgpwszOldRoman[11] =
{
    L"0", L"I",  L"II",  L"III",  L"IIII",
    L"V", L"VI", L"VII", L"VIII", L"VIIII",
    L"X"
}; // k_rgpwszOldRoman

const char16* k_rgpwszOrdinal[11] =
{
    L"zeroth", L"first",  L"second",  L"third", L"fourth",
    L"fifth",  L"sixth",  L"seventh", L"eighth", L"ninth",
    L"tenth"
}; // k_rgpwszOrdinal

const char16* k_rgpwszRoman[11] =
{
    L"0", L"I",  L"II",  L"III",  L"IV",
    L"V", L"VI", L"VII", L"VIII", L"IX",
    L"X"
}; // k_rgpwszRoman


//////////////////////////////////////////////////////////////////////
//
// Formatter
//
class Formatter
{
    public: Val Run()
    {
        MiniThread* p = MiniThread::Get();

        #if ! defined(EVCL_BOOT) && ! defined(_DEBUG)
        {
            p->m_fn = Qformat->Decode<Symbol>()->m_function;
            if (p->m_fn->Decode<NativeCodeFunction>()->m_name == Qformat)
            {
                return ::CallLisp(p);
            }
        }
        #endif // ! defined(EVCL_BOOT) && ! defined(_DEBUG)

        if (cmp_xx(p->m_n, 2) < 0)
        {
            error(L"Too few arguments.");
        }

        Val stream  =  p->mv_value[0];
        Val control =  p->mv_value[1];
        Val args = nil;
        {
            Int iNth = Fixnum::Decode_( p->m_n);
            while (iNth >= 3)
            {
                iNth -= 1;
                args = cons(p->mv_value[iNth], args);
            } // while
        } // args
        return main(stream, control, args);
    } // Run

    #if 0
    public: Val Run(Val stream, const char16* pwszControl)
    {
        StackString_<1024> oControl(pwszControl);
        Val args = nil;
        {
            Int iNth = Fixnum::Decode_( MiniThread::Get()->m_n);
            while (iNth >= 1)
            {
                iNth -= 1;
                args = cons( MiniThread::Get()->mv_value[iNth], args);
            } // while
        } // args
        return main(stream, oControl.Encode(), args);
    } // Run
    #endif

    Val main(Val stream, Val xControl, Val args)
    {
        bool fNil = true;

        if (Qt == stream)
        {
            stream =  MiniThread::Get()->GetTlv(TLV_Astandard_outputA);
        }
        else if (nil == stream)
        {
            stream = make_string_output_stream();
            fNil = false;
        }

        format(stream, xControl, args);

        return fNil ? nil : get_output_stream_string(stream);
    } // Run

    protected: struct Param
    {
        bool    m_fAt;
        bool    m_fColon;
        int     m_cParams;
        Val     mv_value[5];

        Param()
        {
            m_fAt     = false;
            m_fColon  = false;
            m_cParams = 0;
        }
    }; // Param

    // format main loop
    void format(Val stream, Val control, Val args)
    {
        check_type(control, simple_string);

        const char16* pwchRunner =
            control->Decode<SimpleString>()->GetElements();

        const char16* pwchEnd = pwchRunner + Fixnum::Decode_(length(control));

        Val runner = args;

        while (pwchRunner < pwchEnd)
        {
            char16 wchChar = *pwchRunner;

            if ('~' != wchChar)
            {
                const char16* pwchStart = pwchRunner;

                while (pwchRunner < pwchEnd)
                {
                    if ('~' == *pwchRunner)
                    {
                        break;
                    }

                    pwchRunner++;
                } // while

                write_string(pwchStart, pwchRunner - pwchStart, stream);

                if (pwchRunner == pwchEnd)
                {
                    break;
                }
            } // if

            pwchRunner++;

            Param oParam;

            // Get parameters
            {
                enum
                {
                    State_None,
                    State_Digit,
                    State_Quote,
                    State_Next,
                } eState = State_None;

                for (;;)
                {
                    if (pwchRunner == pwchEnd)
                    {
                        error(L"control string ends with '~'.");
                    }

                    switch (eState)
                    {
                    case State_None:
                        if (*pwchRunner >= '0' && *pwchRunner <= '9')
                        {
                            oParam.mv_value[oParam.m_cParams] =
                                Fixnum::Encode(*pwchRunner - '0');

                            eState = State_Digit;
                        }
                        else if (0x27 == *pwchRunner)
                        {
                            eState = State_Quote;
                        }
                        else if ('V' == *pwchRunner || 'v' == *pwchRunner)
                        {
                            oParam.mv_value[oParam.m_cParams] =
                                next_arg(&runner);

                            eState = State_Next;
                        }
                        else if (',' == *pwchRunner)
                        {
                            oParam.mv_value[oParam.m_cParams] =
                                QQunbound_marker;

                            oParam.m_cParams += 1;
                        }
                        else
                        {
                            goto done;
                        }
                        break;

                    case State_Digit:
                        if (*pwchRunner >= '0' && *pwchRunner <= '9')
                        {
                            Val param = oParam.mv_value[oParam.m_cParams];

                            oParam.mv_value[oParam.m_cParams] =
                                add_xx(mul_xx(param, 10), *pwchRunner - '0');
                        }
                        else if (',' == *pwchRunner)
                        {
                            oParam.m_cParams += 1;
                            eState = State_None;
                        }
                        else
                        {
                            oParam.m_cParams += 1;
                            goto done;
                        }
                        break;

                    case State_Quote:
                        oParam.mv_value[oParam.m_cParams] =
                            Character::Encode(*pwchRunner);

                        oParam.m_cParams += 1;
                        
                        eState = State_Next;
                        break;

                    case State_Next:
                        if (',' == *pwchRunner)
                        {
                            oParam.m_cParams += 1;
                        }
                        else
                        {
                            goto done;
                        }
                        break;

                    default:
                        error(L"Can't happen!");
                    } // switch eState

                    pwchRunner++;
                } // for

                done: ;
            }

            // Get colon(:) and at-mark(@)
            for (;;)
            {

                if (pwchRunner == pwchEnd)
                {
                    error(L"control string ends with '~'.");
                }

                if (':' == *pwchRunner)
                {
                    oParam.m_fColon = true;
                }
                else if ('@' == *pwchRunner)
                {
                    oParam.m_fAt = true;
                }
                else
                {
                    break;
                }

                pwchRunner++;
            } // for

            switch (*pwchRunner)
            {
            case '%':
                format_chars(stream, &oParam, 0x0A);
                break;

            case 'A': case 'a':
                format_A(stream, next_arg(&runner), &oParam);
                break;

            case 'B': case 'b':
                format_R(stream, next_arg(&runner), &oParam, 2);
                break;

            case 'C': case 'c':
                format_C(stream, next_arg(&runner), &oParam);
                break;

            case 'D': case 'd':
                format_R(stream, next_arg(&runner), &oParam, 10);
                break;

            case 'O': case 'o':
                format_R(stream, next_arg(&runner), &oParam, 8);
                break;

            case 'R': case 'r':
                format_R(stream, next_arg(&runner), &oParam);
                break;

            case 'S': case 's':
                format_S(stream, next_arg(&runner), &oParam);
                break;

            case 'X': case 'x':
                format_R(stream, next_arg(&runner), &oParam, 16);
                break;

            case '|':
                format_chars(stream, &oParam, 0x0C);
                break;

            case '~':
                format_chars(stream, &oParam, '~');
                break;
            } // switch

            pwchRunner++;
        } // while
    } // format

    // next_arg
    Val next_arg(Val* pxRunner)
    {
        Val runner = *pxRunner;

        if (! endp(runner))
        {
            Val xArg = car(runner);
            *pxRunner = cdr(runner);
            return xArg;
        }

        error(L"Insufficent format argument.");
    } // next_arg

    // Format Printer Operations
    void format_A(Val stream, Val val, Param* pParam)
    {
        ASSERT(NULL != pParam);

        BindFrameScope oLet(2);
            oLet.Bind(TLV_Aprint_escapeA,   nil);
            oLet.Bind(TLV_Aprint_readablyA, nil);

        if (nil == val && pParam->m_fColon)
        {
            write_string(L"()", stream);
        }
        else
        {
            print_object(val, stream);
        }
    } // format_a

    // Format Printer Operations
    void format_C(Val stream, Val val, Param* pParam)
    {
        ASSERT(NULL != pParam);

        BindFrameScope oLet(2);
            oLet.Bind(TLV_Aprint_escapeA,   nil);
            oLet.Bind(TLV_Aprint_readablyA, nil);

        if (characterp(val))
        {
            char16 wch = Character::ToCode(val);

            if (pParam->m_fColon)
            {
                Val name = gethash(val, VAR(Achar_name_tableA));
                if (stringp(name))
                {
                    write_string(name, stream);
                }
                else if (wch < 0x20)
                {
                    char16 wsz[20];
                        ::wsprintf(wsz, L"u%04X", wch);
                    write_string(wsz, stream);
                }
                else
                {
                    write_string(&wch, 1, stream);
                }
            }
            else
            {
                write_string(&wch, 1, stream);
            }
        }
        else
        {
            print_object(val, stream);
        }
    } // format_C

    // format chars
    void format_chars(Val stream, Param* pParam, char16 wchChar)
    {
        Int iCount =
            0 == pParam->m_cParams ?
                1 :
                Fixnum::Decode_(pParam->mv_value[0]);

        while (iCount > 0)
        {
            write_string(&wchChar, 1, stream);

            iCount -= 1;
        } // while
    } // format_chars

    // Format Radix Control
    //  ~radix,mincol,padchar,commachar,comma-intervalR
    void format_R(Val stream, Val val, Param* pParam)
    {
        ASSERT(NULL != pParam);
        if (0 != pParam->m_cParams)
        {
            Val radix = pParam->mv_value[0];
            if (! fixnump(radix)) radix = Fixnum::Encode(0);
            Int iRadix = static_cast<int>(Fixnum::Decode_(radix));
            if (iRadix < 2 || iRadix > 36)
            {
                error(make_type_error(
                    pParam->mv_value[0],
                    list(Qinteger, Fixnum::Encode(2), Fixnum::Encode(36)) ) );
            }

            pParam->m_cParams -= 1;

            ::memcpy(
                pParam->mv_value,
                pParam->mv_value + 1,
                sizeof(Val) * pParam->m_cParams );

            format_R(stream, val, pParam, static_cast<int>(iRadix));
            return;
        }

        if (! fixnump(val))
        {
            pParam->m_fAt    = false;
            pParam->m_fColon = false;
            format_R(stream, val, pParam, 10);
            return;
        }

        Int iVal = static_cast<int>(Fixnum::Decode_(val));
        if (iVal < 0 || iVal > 10)
        {
            pParam->m_fAt    = false;
            pParam->m_fColon = false;
            format_R(stream, val, pParam, 10);
            return;
        }

        if (pParam->m_fAt && pParam->m_fColon)
        {
            write_string(k_rgpwszOldRoman[iVal], stream);
        }
        else if (pParam->m_fAt)
        {
            write_string(k_rgpwszRoman[iVal], stream);
        }
        else if (pParam->m_fColon)
        {
            write_string(k_rgpwszOrdinal[iVal], stream);
        }
        else
        {
            write_string(k_rgpwszCardinal[iVal], stream);
        }
    } // format_R

    // Format Radix Control
    //  ~mincol,padchar,commachar,comma-intervalD
    void format_R(Val stream, Val val, Param* pParam, int iRadix)
    {
        ASSERT(NULL != pParam);

        if (! fixnump(val))
        {
            BindFrameScope oLet(4);
                oLet.Bind(TLV_Aprint_escapeA,   nil);
                oLet.Bind(TLV_Aprint_radixA,    nil);
                oLet.Bind(TLV_Aprint_baseA,     Fixnum::Encode(iRadix));
                oLet.Bind(TLV_Aprint_readablyA, nil);

            print_object(val, stream);
        }
        else
        {
            char16  wszDigit[100];
            char16* pwszRunner = &wszDigit[lengthof(wszDigit)];
            *--pwszRunner = 0;

            if (Fixnum::Encode(0) == val)
            {
                *--pwszRunner = '0';
            }
            else
            {
                Int iVal = Fixnum::Decode_(val);

                char16 wchSign = 0;
                if (iVal < 0)
                {
                    wchSign = '-';
                    iVal = -iVal;
                }
                else if (pParam->m_fAt)
                {
                    wchSign = '+';
                }

                if (iRadix < 10)
                {
                    while (0 != iVal)
                    {
                        Int iMod = iVal % iRadix;
                        iVal /= iRadix;
                        *--pwszRunner = static_cast<char16>(iMod + '0');
                    } // while
                }
                else
                {
                    while (0 != iVal)
                    {
                        Int iMod = iVal % iRadix;
                        iVal /= iRadix;
                        if (iMod < 10)
                        {
                            *--pwszRunner = static_cast<char16>(iMod + '0');
                        }
                        else
                        {
                            *--pwszRunner =
                                static_cast<char16>(iMod + 'A' - 10);
                        }
                    } // while
                } // if

                if (0 != wchSign)
                {
                    *--pwszRunner = wchSign;
                }
            } // if

            char16 wchPad = ' ';

            if (pParam->m_cParams >= 2)
            {
                Val pad = pParam->mv_value[1];
                if (QQunbound_marker == pad)
                {
                    // nothing to do
                }
                else if (characterp(pad))
                {
                    wchPad = Character::ToCode(pad);
                }
                else
                {
                    error(make_type_error(pad, Qcharacter));
                }
            } // if

            if (pParam->m_cParams >= 1 && fixnump(pParam->mv_value[0]))
            {
                Int iWidth = Fixnum::Decode_(pParam->mv_value[0]);
                iWidth -= &wszDigit[lengthof(wszDigit) - 1] - pwszRunner;
                while (iWidth > 0)
                {
                    write_char(wchPad, stream);
                    iWidth -= 1;
                } // while
            } // if

            write_string(pwszRunner, stream);
        }
    } // format_R

    // Format Printer Operations
    void format_S(Val stream, Val val, Param* pParam)
    {
        ASSERT(NULL != pParam);

        BindFrameScope oLet(1);
            oLet.Bind(TLV_Aprint_escapeA,  Qt);

        if (nil == val && pParam->m_fColon)
        {
            write_string(L"()", stream);
        }
        else
        {
            print_object(val, stream);
        }
    } // format_s
}; // Formatter

} // namespace

namespace MiniLisp
{

// C_format
Val C_format(Kernel::Thread*)
{
    Formatter oFormatter;
    return oFormatter.Run();
} // Thread::C_format

} // MiniLisp

namespace CommonLisp
{
Val format(Val stream, const char16* pwszControl)
{
    StackString_<1024> oControl(pwszControl);
    values(stream, oControl);
    Formatter oFormatter;
    return oFormatter.Run();
} // format


Val format(Val stream, const char16* pwszControl, Val a)
{
    StackString_<1024> oControl(pwszControl);
    values(stream, oControl, a);
    Formatter oFormatter;
    return oFormatter.Run();
} // format


Val format(Val stream, const char16* pwszControl,
    Val a, Val b )
{
    StackString_<1024> oControl(pwszControl);
    values(stream, oControl, a, b);
    Formatter oFormatter;
    return oFormatter.Run();
} // format


Val format(Val stream, const char16* pwszControl,
    Val a, Val b, Val c )
{
    StackString_<1024> oControl(pwszControl);
    values(stream, oControl, a, b, c);
    Formatter oFormatter;
    return oFormatter.Run();
} // format


Val format(Val stream, const char16* pwszControl,
    Val a, Val b, Val c, Val d )
{
    StackString_<1024> oControl(pwszControl);
    values(stream, oControl, a, b, c, d);
    Formatter oFormatter;
    return oFormatter.Run();
} // format


Val format(Val stream, const char16* pwszControl,
    Val a, Val b, Val c, Val d, Val e )
{
    StackString_<1024> oControl(pwszControl);
    values(stream, oControl, a, b, c, d, e);
    Formatter oFormatter;
    return oFormatter.Run();
} // format


Val format(Val stream, const char16* pwszControl,
    Val a, Val b, Val c, Val d, Val e, Val f )
{
    StackString_<1024> oControl(pwszControl);
    values(stream, oControl, a, b, c, d, e, f);
    Formatter oFormatter;
    return oFormatter.Run();
} // format


Val format(Val stream, const char16* pwszControl,
    Val a, Val b, Val c, Val d, Val e, Val f, Val g )
{
    StackString_<1024> oControl(pwszControl);
    values(stream, oControl, a, b, c, d, e, f, g);
    Formatter oFormatter;
    return oFormatter.Run();
} // format

Val format(Val stream, const char16* pwszControl,
    Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h )
{
    StackString_<1024> oControl(pwszControl);
    values(stream, oControl, a, b, c, d, e, f, g, h);
    Formatter oFormatter;
    return oFormatter.Run();
} // format

} // CommonLisp
