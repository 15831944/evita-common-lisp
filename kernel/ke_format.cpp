#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - format
// kernel/ke_format.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_format.cpp#4 $
//
#include "./ke_format.h"

#include "../mini/mini_lisp.h"


namespace Kernel
{

    // format main loop
void Formatter::Run(
    const char16*   pwchStart,
    const char16*   pwchEnd,
    va_list         args )
{
    const char16* pwchRunner = pwchStart;

    while (pwchRunner < pwchEnd)
    {
        WCHAR wchChar = *pwchRunner;

        if ('~' != wchChar)
        {
            const char16* pwchString = pwchRunner;

            while (pwchRunner < pwchEnd)
            {
                if ('~' == *pwchRunner)
                {
                    break;
                }

                pwchRunner++;
            } // while

            write_string(pwchString, pwchRunner);

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
            format_chars(&oParam, 0x0A);
            break;

        case 'A': case 'a':
            format_A(&oParam, va_arg(args, Val));
            break;

        case 'B': case 'b':
            format_R(&oParam, va_arg(args, Val), 2);
            break;

        case 'C': case 'c':
            format_C(&oParam, va_arg(args, Val));
            break;

        case 'D': case 'd':
            format_R(&oParam, va_arg(args, Val), 10);
            break;

        case 'O': case 'o':
            format_R(&oParam, va_arg(args, Val), 8);
            break;

        case 'R': case 'r':
            format_R(&oParam, va_arg(args, Val), 0);
            break;

        case 'S': case 's':
            format_S(&oParam, va_arg(args, Val));
            break;

        case 'X':
        case 'x':
            format_R(&oParam, va_arg(args, Val), 16);
            break;

        case '|':
            format_chars(&oParam, 0x0C);
            break;

        case '~':
            format_chars(&oParam, '~');
            break;
        } // switch

        pwchRunner++;
    } // while
} // Formatter::Run


// Format Printer Operations
void Formatter::format_A(const Param* pParam, Val val)
{
    ASSERT(NULL != pParam);

    BindFrameScope oLet(2);
        oLet.Bind(TLV_Aprint_escapeA,   nil);
        oLet.Bind(TLV_Aprint_readablyA, nil);

    if (nil == val && pParam->m_fColon)
    {
        write_string(L"()");
    }
    else
    {
        print_object(val);
    }
} // Formatter::format_A


// Formatter::format_C
void Formatter::format_C(const Param* pParam, Val val)
{
    ASSERT(NULL != pParam);

    BindFrameScope oLet(2);
        oLet.Bind(TLV_Aprint_escapeA,   nil);
        oLet.Bind(TLV_Aprint_readablyA, nil);

    if (characterp(val))
    {
        write_char(Character::ToCode(val));
    }
    else
    {
        print_object(val);
    }
} // Formatter::format_C


// Formatter::format_chars
void Formatter::format_chars(const Param* pParam, char16 wch)
{
    Int iCount =
        0 == pParam->m_cParams ?
            1 :
            Fixnum::Decode_(pParam->mv_value[0]);

    while (iCount > 0)
    {
        write_char(wch);

        iCount -= 1;
    } // while
} // Formatter::format_chars


// Format Radix Control
//  ~mincol,padchar,commachar,comma-intervalD
void Formatter::format_R(const Param* pParam, Val val, int iRadix)
{
    ASSERT(NULL != pParam);

    if (! fixnump(val))
    {
        BindFrameScope oLet(4);
            oLet.Bind(TLV_Aprint_escapeA,   nil);
            oLet.Bind(TLV_Aprint_radixA,    nil);
            oLet.Bind(TLV_Aprint_baseA,     Fixnum::Encode(iRadix));
            oLet.Bind(TLV_Aprint_readablyA, nil);

        print_object(val);
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

        WCHAR wchPad = ' ';

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
                write_char(wchPad);
                iWidth -= 1;
            } // while
        } // if

        write_string(pwszRunner);
    }
} // Formatter::format_R


// Format Printer Operations
void Formatter::format_S(const Param* pParam, Val val)
{
    ASSERT(NULL != pParam);

    BindFrameScope oLet(1);
        oLet.Bind(TLV_Aprint_escapeA,  Qt);

    if (nil == val && pParam->m_fColon)
    {
        write_string(L"()");
    }
    else
    {
        print_object(val);
    }
} // Formatter::format_S

} // Kernel
