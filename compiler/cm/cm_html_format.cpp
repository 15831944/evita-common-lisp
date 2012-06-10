#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - Bootstrap Compiler - Format
// compiler/cm_format.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_html_format.cpp#2 $
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

namespace Compiler
{

namespace {

//////////////////////////////////////////////////////////////////////
//
// Formatter
//
class Formatter
{
    public: void Run(Val stream, LPCWSTR pwszFormat, va_list args);

    struct Param
    {
        bool    m_fAtSign;
        bool    m_fColon;
        UINT    m_cParams;
        Val     mv_value[5];

        Param() :
            m_fAtSign(false),
            m_fColon(false),
            m_cParams(0) {}
    }; // Param


    void formatR(
        Val         stream,
        INT_PTR     iValue,
        Param*  pParam,
        UINT        nBase );
}; // Formatter


//////////////////////////////////////////////////////////////////////
//
// Run
//
void
Formatter::Run(Val stream, LPCWSTR pwszFormat, va_list args)
{
    while (0 != *pwszFormat)
    {
        WCHAR wchChar = *pwszFormat++;
        if ('~' != wchChar)
        {
            write_char(wchChar, stream);
            continue;
        }

        wchChar = *pwszFormat++;

        Param oParam;

        for (;;)
        {
            if (wchChar >= '0' && wchChar <= '9')
            {
                int iVal = 0;
                while (wchChar >= '0' && wchChar <= '9')
                {
                    iVal *= 10;
                    iVal += wchChar - '0';
                    wchChar = *pwszFormat++;
                } // while

                oParam.mv_value[oParam.m_cParams] =
                    Fixnum::Encode(iVal);
                oParam.m_cParams += 1;
            }
            else if ('\'' == wchChar)
            {
                oParam.mv_value[oParam.m_cParams] =
                    Character::Encode(*pwszFormat++);
                oParam.m_cParams += 1;

                wchChar = *pwszFormat++;
            }
            else if (',' == wchChar)
            {
                oParam.mv_value[oParam.m_cParams] = nil;
                oParam.m_cParams += 1;
            }

            if (',' != wchChar)
            {
                break;
            }

            wchChar = *pwszFormat++;
        } // for

        for (;;)
        {
            if ('@' == wchChar)
            {
               oParam.m_fAtSign = true;
               wchChar = *pwszFormat++;
            }
            else if (':' == wchChar)
            {
               oParam.m_fColon = true;
               wchChar = *pwszFormat++;
            }
            else
            {
                break;
            } // if
        } // forever

        switch (wchChar)
        {
        case 'A':
        case 'a':
            write_string(va_arg(args, LPCWSTR), stream);
            break;

        case 'B':
        case 'b':
            formatR(stream, va_arg(args, int), &oParam, 2);
            break;

        case 'D':
        case 'd':
            formatR(stream, va_arg(args, int), &oParam, 10);
            break;

        case 'O':
        case 'o':
            formatR(stream, va_arg(args, int), &oParam, 8);
            break;

        case 'S':
        case 's':
        {
            Object* pObject = va_arg(args, Object*);
            if (NULL != pObject)
            {
                pObject->HtmlPrint(stream, oParam.m_fColon);
            }
            else
            {
                write_string(L"null", stream);
            }
            break;
        } // case 'S'

        case 'W':
        case 'w':
            cm_print_object(va_arg(args, Val), stream);
            break;

        case 'X':
        case 'x':
            formatR(stream, va_arg(args, INT_PTR), &oParam, 16);
            break;

        case '%':
        {
            Int iCount = 0 == oParam.m_cParams ?
                1 :
                Fixnum::Decode_(oParam.mv_value[0]);

            while (iCount >= 1)
            {
                if (oParam.m_fColon)
                {
                    write_string(L"<br/>", stream);
                }

                write_char(0x0A, stream);
                iCount -= 1;
            } // while
            break;
        } // '%'

        case '~':
        {
            Int iCount = 0 == oParam.m_cParams ?
                1 :
                Fixnum::Decode_(oParam.mv_value[0]);

            while (iCount >= 1)
            {
                write_char('~', stream);
                iCount -= 1;
            } // while
            break;
        } // '~'

        default:
            CAN_NOT_HAPPEN();
        } // switch wchChar
    } // while
} // Formatter::Run


//////////////////////////////////////////////////////////////////////
//
// Format R
//
void
Formatter::formatR(
    Val         stream,
    INT_PTR     iArg,
    Param*      pParam,
    UINT        nBase )
{
    UINT_PTR nArg;
    BOOL fSign;
    {
        fSign = iArg < 0;
        if (fSign)
        {
            iArg = -iArg;
        }

        nArg = static_cast<UINT_PTR>(iArg);
    } // nArg


    WCHAR  rgwchDigit[100];
    LPWSTR pwchDigit = rgwchDigit;
    if (0 == nArg)
    {
        *pwchDigit++ = '0';
    }
    else
    {
        do
        {
            UINT nDigit = static_cast<UINT>(nArg % nBase);
            if (nDigit < 10)
            {
                nDigit += '0';
            }
            else
            {
                nDigit += 'A' - 10;
            }

            *pwchDigit++ = static_cast<WCHAR>(nDigit);
            nArg /= nBase;
        } while (0 != nArg);
    } // if

    Int iWidth = pParam->m_cParams >= 1 ?
        Fixnum::Decode_(pParam->mv_value[0]) :
        0;

    WCHAR   wchPad = pParam->m_cParams >= 2 ?
        Character::ToCode(pParam->mv_value[1]) :
        ' ';

    UINT cDigits = static_cast<UINT>(pwchDigit - rgwchDigit);
    Int cPads =  iWidth - cDigits;

    if (fSign)
    {
        cPads -= 1;
    }
    else if (pParam->m_fAtSign)
    {
        cPads -= 1;
    } // if

    while (cPads >= 1)
    {
        write_char(wchPad, stream);
        cPads -= 1;
    }

    if (fSign)
    {
        write_char('-', stream);
    }
    else if (pParam->m_fAtSign)
    {
        write_char('+');
    } // if

    while (pwchDigit != rgwchDigit)
    {
        write_char(*--pwchDigit, stream);
    } // while
} // Formatter::formatR

}; // namespace



//////////////////////////////////////////////////////////////////////
//
// Format
//
void html_formatV(Val stream, LPCWSTR pwszFormat, va_list args)
{
    Formatter oFormatter;
        oFormatter.Run(stream, pwszFormat, args);
} // html_format


void html_format(Val stream, LPCWSTR pwszFormat, ... )
{
    va_list args;
    va_start(args, pwszFormat);
    html_formatV(stream, pwszFormat, args);
    va_end(args);
} // Format

} // Compiler
