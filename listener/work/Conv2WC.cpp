#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// CConvertToUnicode class implementation.
//
// Copyright (C) 1996-2003 by Project Vogue.
// Written by Yoshimi "VOGUE" FUNAI. (yosi@msn.com)
//
// @(#)$Id: /proj/evita/shared/Conv2WC.cpp 1 2003-08-30 19:25:00 yosi $
//
#include "./Conv2WC.h"

//////////////////////////////////////////////////////////////////////////////
//
// CConvertToUnicode public methods
//


//////////////////////////////////////////////////////////////////////
//
// CConvertToUnicode constructor
//
CConvertToUnicode::CConvertToUnicode(
    UINT    nCodePage )
{
    // Code Page must be valid.
    m_nCodePage = nCodePage;
    m_eState    = ISO2022_SBC;

} // CConvertToUnicode::CConvertToUnicode


//////////////////////////////////////////////////////////////////////
//
// CConvertToUnicode destructor
//
CConvertToUnicode::~CConvertToUnicode()
{
    // nothing to do

} // CConvertToUnicode::~CConvertToUnicode


//////////////////////////////////////////////////////////////////////
//
// Create CConvertToUnicode object
//
HRESULT
CConvertToUnicode::Create(
    UINT                nCodePage,
    CConvertToUnicode** out_pConverter )
{
    switch (nCodePage)
    {
    case CP_UTF16_LE:
    case CP_UTF16_BE:
    case CP_UTF8:
    case CP_UTF7:
        break;

    case CP_EUC_JP:
        break;

    case CP_ISO2022_JP:
    case CP_ISO2022_JP_0201:
    case CP_ISO2022_JP_0202:
        break;

    default:
    {
        CPINFO oCPInfo;
        if (! ::GetCPInfo(nCodePage, &oCPInfo))
        {
            DWORD dwError = ::GetLastError();
            DEBUG_PRINTF("CConvertStream::init: "
                "GetCPInfo failed(%u)\n", dwError );
            return HRESULT_FROM_WIN32(dwError);
        }
        break;
    } // default
    } // switch nCodePage

    CConvertToUnicode* pConverter = new CConvertToUnicode(nCodePage);
    if (pConverter == NULL) return E_OUTOFMEMORY;

    pConverter->m_eState = ISO2022_SBC;

    *out_pConverter = pConverter;

    return S_OK;

} // CConvertToUnicode::Create


//////////////////////////////////////////////////////////////////////
//
// Convert MBCS to WCS
//
// Arguments:
//  dwFlags     MB_ERR_INVALID_CHARS
//
DWORD
CConvertToUnicode::ConvertEx(
    DWORD   dwFlags,
    LPCSTR  pchMBCS,
    int*    inout_cchMBCS,
    LPWSTR  pwchWCS,
    int*    inout_cwchWCS )
{
    if (inout_cwchWCS == NULL)
    {
        return ERROR_INVALID_PARAMETER;
    }

    if (inout_cchMBCS == NULL)
    {
        return ERROR_INVALID_PARAMETER;
    }

    if (*inout_cchMBCS == 0)
    {
        *inout_cwchWCS = 0;
        return 0;
    }

    ////////////////////////////////////////////////////////////
    //
    // Non-Windows Code Page
    //
    switch (m_nCodePage)
    {
    case CP_UTF8:
        return fromUTF8(
            pchMBCS, inout_cchMBCS,
            pwchWCS, inout_cwchWCS );

    case CP_UTF16_LE:
        return fromUTF16LE(
            pchMBCS, inout_cchMBCS,
            pwchWCS, inout_cwchWCS );

    case CP_UTF16_BE:
        return fromUTF16BE(
            pchMBCS, inout_cchMBCS,
            pwchWCS, inout_cwchWCS );

    case CP_EUC_JP:
        return fromEUC_JP(
            dwFlags,
            pchMBCS,
            inout_cchMBCS,
            pwchWCS,
            inout_cwchWCS );

    case CP_ISO2022_JP:
    case CP_ISO2022_JP_0201:
    case CP_ISO2022_JP_0202:
        return fromISO2022(
            dwFlags,
            pchMBCS,
            inout_cchMBCS,
            pwchWCS,
            inout_cwchWCS );

    // BUGBUG: NYI: UTF-7 for Win9x

    } // switch codePage

    ////////////////////////////////////////////////////////////
    //
    // Use Windows Code Page
    //
    int cchMBCS = *inout_cchMBCS;
    int cwchWCS = *inout_cwchWCS;
    for (;;)
    {
        ASSERT(cchMBCS >= 1);

        int cwchGot = ::MultiByteToWideChar(
            m_nCodePage,
            dwFlags,
            pchMBCS, cchMBCS,
            pwchWCS, cwchWCS );

        if (cwchGot >= 1)
        {
            #if 0
                DEBUG_PRINTF("CConvertToUnicode::Convert: "
                    "MBCS in=%d out=%d\n",
                    *inout_cchMBCS,
                    cchMBCS );
            #endif

            *inout_cchMBCS = cchMBCS;
            *inout_cwchWCS = cwchGot;
            break;
        }

        {
            DWORD dwError = ::GetLastError();
            if (ERROR_NO_UNICODE_TRANSLATION != dwError)
            {
                REPORT_WIN32_ERROR("MultiByteToWideChar", dwError);
                return dwError;
            }

            # if 0
            {
                for (
                    LPCSTR pchScan = pchMBCS;
                    pchScan < pchMBCS + cchMBCS;
                    pchScan++ )
                {
                    if (static_cast<UINT>(*pchScan) >= 0x80)
                    {
                        if (::IsDBCSLeadByteEx(m_nCodePage, *pchScan))
                        {
                            pchScan++;
                        }
                        else
                        {
                            cchMBCS = static_cast<int>(pchScan - pchMBCS);

                            DEBUG_PRINTF("CConvertToUnicode::Convert: "
                                "Found bad leading byte 0x%02X at %d\n",
                                *pchScan & 0xFF,
                                cchMBCS );
                            break;
                        }
                    }
                } // for
            }
            #endif
        }

        cchMBCS -= 1;
        if (cchMBCS == 0)
        {
            *inout_cchMBCS = 0;
            *inout_cwchWCS = 0;
            return 0;
        }
    } // for

    return 0;
} // CConvertToUnicode::Convert

//////////////////////////////////////////////////////////////////////////////
//
// Encoding support functions
//

inline void
jis_to_sjis(
    int&    iFirst,
    int&    iSecond )
{
    int iRow = iFirst < 95 ? 112 : 176;
    int iCell = iFirst & 1 ? (iSecond > 95 ? 32 : 31) : 126;

    iFirst = ((iFirst + 1) >> 1) + iRow;
    iSecond += iCell;
} // jis_to_sjis


//////////////////////////////////////////////////////////////////////////////
//
// EUC-JP
//  CS0: 0x21-0x7E                  (ASCII or JIS Roman)
//  CS1: 0xA1..0xFE 0xA1..0xFE      (JIS-X0208-1990)
//  CS2: 0x8E 0xA1..0xFE            (half-width katakana)
//  CS3: 0x8F 0xA1..0xFE 0xA1..0xFE (JIS X0212-1990)
//
// EUC-CN
//  CS0: 0x21-0x7E                  (ASCII or GB Roman)
//  CS1: 0xA1..0xFE 0xA1..0xFE      (GB 2312-80)
//  CS2: 0x8E 0xA1..0xFE            unused
//  CS3: 0x8F 0xA1..0xFE 0xA1..0xFE unused
//
// EUC-KR
//  CS0: 0x21-0x7E                  (ASCII or KS-Roman)
//  CS1: 0xA1..0xFE 0xA1..0xFE      (KS X 1001:1992)
//  CS2: 0x8E 0xA1..0xFE            unused
//  CS3: 0x8F 0xA1..0xFE 0xA1..0xFE unused
//
// EUC-TW
//  CS0: 0x21-0x7E                  (ASCII or CNS Roman)
//  CS1: 0xA1..0xFE 0xA1..0xFE      (CNS 11643-1992 Plane 1)
//  CS2: 0x8E 0xA1..0xFE            (CNS 11643-1992 Plane 1-7)
//  CS3: 0x8F 0xA1..0xFE 0xA1..0xFE unused

#define EUC_SS2 0x8E
#define EUC_SS3 0x8F

DWORD
CConvertToUnicode::fromEUC_JP(
    DWORD   dwFlags,
    LPCSTR  pchMBCS,
    int*    inout_cchMBCS,
    LPWSTR  pwchWCS,
    int*    inout_cwchWCS )
{
    ASSERT(NULL != pchMBCS);

    int nDesignate = 0;

    DWORD dwError = 0;
    int cchMBCS = *inout_cchMBCS;
    int cwchWCS = *inout_cwchWCS;

    if (cchMBCS == -1)
    {
        cchMBCS = ::lstrlenA(pchMBCS) + 1;
    }
    else if (cchMBCS < 0)
    {
        return ERROR_INVALID_PARAMETER;
    }

    LPCSTR pchScan     = pchMBCS;
    LPCSTR pchScanEnd  = pchMBCS + cchMBCS;
    LPWSTR pwchDest    = pwchWCS;

    if (pwchWCS == NULL)
    {
        if (cwchWCS != 0)
        {
            return ERROR_INVALID_PARAMETER;
        }
        cwchWCS = MAXLONG;
    }

    LPWSTR pwchDestEnd = pwchWCS + cwchWCS;
    int    iFirst;

    enum EUC_StateT
    {
        STATE_EUC_0,
        STATE_EUC_1_2,
        STATE_EUC_2_2,
        STATE_EUC_3_2,
        STATE_EUC_3_3
    };

    EUC_StateT eState = STATE_EUC_0;

    while (pchScan < pchScanEnd)
    {
        int iChar = static_cast<BYTE>(*pchScan++);

        switch (eState)
        {
        case STATE_EUC_0:
            if (iChar <= 0x7F)
            {
                break;
            }
            else if (iChar >= 0xA1 && iChar <= 0xFE)
            {
                eState = STATE_EUC_1_2;
                iFirst = iChar & 0x7F;
                continue;
            }
            else if (iChar == EUC_SS2)
            {
                eState = STATE_EUC_2_2;
                continue;
            }
            else if (iChar == EUC_SS3)
            {
                eState = STATE_EUC_3_2;
                continue;
            }
            else
            {
                --pchScan;
                goto errorIllegalChar;
            }
            break;

        case STATE_EUC_1_2:
        {
            int iSecond;
            if (iChar >= 0xA1 && iChar <= 0xFE)
            {
                iSecond = iChar & 0x7F;
            }
            else
            {
                pchScan -= 2;
                goto errorIllegalChar;
            }

            jis_to_sjis(iFirst, iSecond);

            CHAR rgchSJIS[2];
                rgchSJIS[0] = static_cast<CHAR>(iFirst);
                rgchSJIS[1] = static_cast<CHAR>(iSecond);

            WCHAR wchChar;
            int cwchChar = ::MultiByteToWideChar(
                CP_SJIS,
                dwFlags,
                rgchSJIS, 2,
                &wchChar, 1 );
            if (cwchChar <= 0)
            {
                pchScan -= 2;
                goto errorIllegalChar;
            }

            iChar = wchChar;
            eState = STATE_EUC_0;
            break;
        } // case STATE_EUC_1_2

        case STATE_EUC_2_2:
        // Half-width katakana
        if (iChar >= 0xA1 && iChar <= 0xFE)
        {
            CHAR rgcChar[1];
                rgcChar[0] = static_cast<BYTE>(iChar);

            WCHAR wchChar;
            int cwchChar = ::MultiByteToWideChar(
                    nDesignate,
                    dwFlags,
                    rgcChar, 1,
                    &wchChar, 1 );
            if (cwchChar == 1)
            {
                eState = STATE_EUC_0;
                iChar = wchChar;
                break;
            }
        }

        pchScan -= 2;
        goto errorIllegalChar;

        case STATE_EUC_3_2:
        case STATE_EUC_3_3:
            // BUGBUG: NYI: EUC CS3

        default:
            CAN_NOT_HAPPEN();
            return ERROR_INVALID_PARAMETER;
        }

        if (pwchDest + 1 > pwchDestEnd)
        {
            goto errorInsufficentBuffer;
        }

        if (pwchWCS != NULL)
        {
            *pwchDest = static_cast<WCHAR>(iChar);
        }
        pwchDest++;

    } // while

    if (STATE_EUC_0 != eState)
    {
        goto errorTruncate;
    }
    goto exit;

errorTruncate:
    dwError = ERROR_NO_UNICODE_TRANSLATION;
    goto exit;

errorIllegalChar:
    dwError = ERROR_NO_UNICODE_TRANSLATION;
    goto exit;

errorInsufficentBuffer:
    dwError = ERROR_INSUFFICIENT_BUFFER;
    goto exit;

exit:
    *inout_cchMBCS = pchScan - pchMBCS;
    *inout_cwchWCS = pwchDest - pwchWCS;

    return dwError;
} // CConvertToUnicode::fromEUC_JP

//////////////////////////////////////////////////////////////////////////////
//
// ISO-2022-JP2 (RFC1554)
//
// ISO-2022-1986 = JIS X 0202-1991
// ESC $ (      G0-DESIGNATE MULTIBYTE 94-SET
// ESC $ )      G1-DESIGNATE MULTIBYTE 94-SET
// ESC $ *      G2-DESIGNATE MULTIBYTE 94-SET
// ESC $ +      G3-DESIGNATE MULTIBYTE 94-SET
// ESC $ -      G0-DESIGNATE MULTIBYTE 96-SET
// ESC $ .      G1-DESIGNATE MULTIBYTE 96-SET
// ESC $ /      G2-DESIGNATE MULTIBYTE 96-SET
// ESC (        G0-DESIGNATE 94-SET
// ESC )        G1-DESIGNATE 94-SET
// ESC *        G2-DESIGNATE 94-SET
// ESC +        G3-DESIGNATE 94-SET
// ESC -        G1-DESIGNATE 96-SET
// ESC .        G2-DESIGNATE 96-SET
// ESC /        G3-DESIGNATE 96-SET
//
// Double Byte
// ESC $ ( @    JIS X 0208-1978 (ESC $ @)
// ESC $ ( A    GB2312-1980 (ESC $ A)
// ESC $ ( B    JIS X 0208-Roman (ESC $ B)
// ESC $ ( C    KCS5601-1987
// ESC $ ( D    JIS X 0212-1990
//
// Single Byte
// ESC ( B      ASCII
// ESC ( H      some implementation use this instead of ESC ( J
// ESC ( I      JIS X0202-1984 G1
// ESC ( J      JIS X0201-1976 G0
// ESC . A      ISO-8859-1 G2
// ESC . F      ISO-8859-7 G2 (Greek)
//

// See: ISO-2022 or ECMA-035 document
//  (Character Code Structure and Extension Techniques)
// 4F and 8F are Escape Sqeunce of types nF instead of hexadecimal
//


#define ISO2022_SI      0x0F        // Ctrl+O
#define ISO2022_SO      0x0E        // Ctrl+N
#define ISO2022_ESCAPE  0x1B

DWORD
CConvertToUnicode::fromISO2022(
    DWORD   dwFlags,
    LPCSTR  pchMBCS,
    int*    inout_cchMBCS,
    LPWSTR  pwchWCS,
    int*    inout_cwchWCS )
{
    int nDesignate = 0;

    switch (m_nCodePage)
    {
    case CP_ISO2022_JP:
    case CP_ISO2022_JP_0201:
    case CP_ISO2022_JP_0202:
        nDesignate = CP_SJIS;
        break;

    default:
        return ERROR_INVALID_PARAMETER;
    } // switch m_nCodePage

    DWORD dwError = 0;
    int cchMBCS = *inout_cchMBCS;
    int cwchWCS = *inout_cwchWCS;

    if (cchMBCS == -1)
    {
        cchMBCS = ::lstrlenA(pchMBCS) + 1;
    }
    else if (cchMBCS < 0)
    {
        return ERROR_INVALID_PARAMETER;
    }

    LPCSTR pchScan     = pchMBCS;
    LPCSTR pchScanEnd  = pchMBCS + cchMBCS;
    LPWSTR pwchDest    = pwchWCS;

    if (pwchWCS == NULL)
    {
        if (cwchWCS != 0)
        {
            return ERROR_INVALID_PARAMETER;
        }
        cwchWCS = MAXLONG;
    }

    LPWSTR pwchDestEnd = pwchWCS + cwchWCS;
    int    iFirst;

    while (pchScan < pchScanEnd)
    {
        int iChar = static_cast<BYTE>(*pchScan++);

        switch (m_eState)
        {
        case ISO2022_SBC:
            switch (iChar)
            {
            case ISO2022_ESCAPE:
                m_eState = ISO2022_ESC;
                continue;

            case ISO2022_SO:
                m_eState = ISO2022_SBC_SO;
                continue;
            } // switch iChar
            break;

        case ISO2022_SBC_G1:
        case ISO2022_SBC_SI:
        case ISO2022_SBC_SO:    // BUGBUG: NYI: SBC_SO @ 1999/07/22
            if (iChar == ISO2022_ESCAPE)
            {
                m_eState = ISO2022_ESC;
                continue;
            }
            else if (iChar == ISO2022_SI)
            {
                if (m_eState == ISO2022_SBC_SO)
                {
                    m_eState = ISO2022_SBC;
                    continue;
                }
            }
            else
            {
                // Note: We accept G1 characters. Some programs emit
                // half-width katakana as G1

                iChar |= 0x80;

                CHAR rgcChar[1];
                    rgcChar[0] = static_cast<BYTE>(iChar);

                WCHAR wchChar;
                int cwchChar = ::MultiByteToWideChar(
                        nDesignate,
                        dwFlags,
                        rgcChar, 1,
                        &wchChar, 1 );
                if (cwchChar == 1)
                {
                    iChar = wchChar;
                }
            }
            break;

        case ISO2022_DBC_1:
            if (iChar >= 0x21 && iChar <= 0x7E)
            {
                m_eState = ISO2022_DBC_2;
                iFirst = iChar;
                continue;
            }

            if (iChar == 0x1B)
            {
                m_eState = ISO2022_ESC;
                continue;
            }

            if (iChar <= 0x20)
            {
                m_eState = ISO2022_SBC;
                break;
            }
            else
            {
                --pchScan;
                goto errorIllegalChar;
            }
            break;

        case ISO2022_DBC_2:
        {
            int iSecond;
            if (iChar >= 0x21 && iChar <= 0x7E)
            {
                iSecond = iChar;
            }
            else
            {
                goto errorIllegalChar;
            }

            if (nDesignate == CP_SJIS)
            {
                jis_to_sjis(iFirst, iSecond);
            }
            else
            {
                iFirst |= 0x80;
                iSecond |= 0x80;
            }

            ////////////////////////////////////////////////////////////
            //
            // Convert DBC to WC
            //
            {
                CHAR rgcChar[2];
                    rgcChar[0] = static_cast<BYTE>(iFirst);
                    rgcChar[1] = static_cast<BYTE>(iSecond);

                WCHAR wchChar;
                int cwchChar = ::MultiByteToWideChar(
                    nDesignate,
                    dwFlags,
                    rgcChar, 2,
                    &wchChar, 1 );
                if (cwchChar != 1)
                {
                    if (pwchDest + 2 > pwchDestEnd)
                    {
                        goto errorInsufficentBuffer;
                    }

                    if (pwchWCS != NULL)
                    {
                        pwchDest[0] = static_cast<WCHAR>(iFirst  & 0x7F);
                        pwchDest[1] = static_cast<WCHAR>(iSecond & 0x7F);
                    }
                    pwchDest += 2;
                    continue;
                }
                iChar = wchChar;
            }

            m_eState = ISO2022_DBC_1;
            break;
        } // case ISO2022_DBC_2 

        case ISO2022_ESC:
            if (iChar == '$')                       // ESC $
            {
                m_eState = ISO2022_ESC_4F;
                continue;
            }
            else if (iChar == '(')                  // ESC (
            {
                m_eState = ISO2022_ESC_8F;
                continue;
            }
            else if (iChar == ISO2022_ESCAPE)       // ESC ESC
            {
                break;
            }

            m_eState = ISO2022_SBC;

            if (pwchDest + 2 > pwchDestEnd)
            {
                goto errorInsufficentBuffer;
            }

            if (pwchWCS != NULL)
            {
                pwchDest[0] = static_cast<WCHAR>(ISO2022_ESCAPE);
                pwchDest[1] = static_cast<WCHAR>(iChar);
            }
            pwchDest += 2;
            continue;

        case ISO2022_ESC_4F:    // ESC $
            switch (iChar)
            {
            case '@':                   // ESC $ @ = JIS X 0208-1978
            case 'B':                   // ESC $ B = JIS X 0208-1983
                m_eState = ISO2022_DBC_1;
                nDesignate = CP_SJIS;
                continue;

            case 'A':                   // ESC $ A = GB 2312-1980
                m_eState = ISO2022_DBC_1;
                nDesignate = CP_GB2312;
                continue;

            case '(':                   // ESC $ (
                m_eState = ISO2022_ESC_4F_8F;
                continue;
            } // switch iChar

            m_eState = ISO2022_SBC;

            if (pwchDest + 3 > pwchDestEnd)
            {
                goto errorInsufficentBuffer;
            }

            if (pwchWCS != NULL)
            {
                pwchDest[0] = static_cast<WCHAR>(ISO2022_ESCAPE);
                pwchDest[1] = '$';
                pwchDest[2] = static_cast<WCHAR>(iChar);
            }
            pwchDest += 3;
            continue;

        case ISO2022_ESC_4F_8F:    // ESC $ (
            switch (iChar)
            {
            case '@':               // ESC $ ( @ = JIS X 0208-1978
            case 'B':               // ESC $ ( @ = JIS X 0208-1983
                m_eState = ISO2022_DBC_1;
                nDesignate = CP_SJIS;
                continue;

            case 'A':               // ESC $ ( A = GB 2312-1980
                m_eState = ISO2022_DBC_1;
                nDesignate = CP_GB2312;
                continue;

            case 'C':               // ESC $ ( C = KCS5601-1987
                m_eState = ISO2022_DBC_1;
                nDesignate = CP_HANGUL;
                continue;
            } // switch iChar

            m_eState = ISO2022_SBC;

            if (pwchDest + 4 > pwchDestEnd)
            {
                goto errorInsufficentBuffer;
            }

            if (pwchWCS != NULL)
            {
                pwchDest[0] = static_cast<WCHAR>(ISO2022_ESCAPE);
                pwchDest[1] = '$';
                pwchDest[2] = '$';
                pwchDest[3] = static_cast<WCHAR>(iChar);
            }
            pwchDest += 4;
            continue;

        case ISO2022_ESC_8F:    // ESC (
            switch (iChar)
            {
            case 'B':                           // ESC ( B = ASCII
            case 'J':                           // ESC ( J = JIS X201-1976:G0
                m_eState = ISO2022_SBC;
                nDesignate = CP_SJIS;
                continue;

            case 'I':                           // ESC ( I = JIS X202-1984:G1
                m_eState = ISO2022_SBC_G1;
                nDesignate = CP_SJIS;
                continue;
            } // switch iChar

            m_eState = ISO2022_SBC;

            if (pwchDest + 3 > pwchDestEnd)
            {
                goto errorInsufficentBuffer;
            }

            if (pwchWCS != NULL)
            {
                pwchDest[0] = static_cast<WCHAR>(ISO2022_ESCAPE);
                pwchDest[1] = '(';
                pwchDest[2] = static_cast<WCHAR>(iChar);
            }
            pwchDest += 3;
            continue;

        default:
            CAN_NOT_HAPPEN();
            return ERROR_INVALID_PARAMETER;
        } // switch m_eState

        if (pwchDest + 1 > pwchDestEnd)
        {
            goto errorInsufficentBuffer;
        }

        if (pwchWCS != NULL)
        {
            *pwchDest = static_cast<WCHAR>(iChar);
        }
        pwchDest++;

    } // while

    switch (m_eState)
    {
    case ISO2022_DBC_2:
    case ISO2022_ESC_4F:
    case ISO2022_ESC_8F:
    case ISO2022_ESC_4F_8F:
        goto errorTruncate;
    } // switch m_eState

    goto exit;

errorTruncate:
    dwError = ERROR_NO_UNICODE_TRANSLATION;
    goto exit;

errorIllegalChar:
    dwError = ERROR_NO_UNICODE_TRANSLATION;
    goto exit;

errorInsufficentBuffer:
    dwError = ERROR_INSUFFICIENT_BUFFER;
    goto exit;

exit:
    *inout_cchMBCS = pchScan - pchMBCS;
    *inout_cwchWCS = pwchDest - pwchWCS;

    return dwError;
} // CConvertToUnicode::fromISO2022

//////////////////////////////////////////////////////////////////////////////
//
// UTF8 decoder
//
// Note: UTF8 decoder/encoder
//  We use our own implementation of UTF8 decoder/encoder due to
//   1)  We can't use MB_ERR_INVALID_CHARS in MultiByteToWideChar
//       with CP_UTF8.
//   2)  Win95 doesn't support CP_UTF8.
//

// Note:
//  If you change utf8MB2WC, EvXML is good friend for testing.


// UTF-8
//
// U+0000...U+007F  0xxxxxxx
// U+0080...U+07FF  110yyyyy 10xxxxxx
// U+0800...U+FFFF  1110zzzz 10yyyyyy 10xxxxxx
// U+D800...U+DFFF  11110uuu 10uuzzzz 10yyyyyy 10xxxxxx
//  where uuuuu=wwwww+1
//
const WCHAR k_wchSurrogateHBeg  = 0xD800u;
const WCHAR k_wchSurrogateHEnd  = 0xDBFFu;
const WCHAR k_wchSurrogateLBeg  = 0xDC00u;
const WCHAR k_wchSurrogateLEnd  = 0xDFFFu;
const WCHAR k_wchReplacement    = 0xFFFDu;


//////////////////////////////////////////////////////////////////////
//
// Convert UTF16BE string to Unicdoe
//
//
DWORD
CConvertToUnicode::fromUTF16BE(
    LPCSTR  pchMBCS,
    int*    inout_cchMBCS,
    LPWSTR  pwchWCS,
    int*    inout_cwchWCS )
{
    int cchMBCS = *inout_cchMBCS;
    int cwchWCS = *inout_cwchWCS;

    if (cchMBCS == -1)
    {
        cchMBCS = ::lstrlenW(reinterpret_cast<LPCWSTR>(pchMBCS)) + 1;
    }
    else if (cchMBCS < 0)
    {
        return ERROR_INVALID_PARAMETER;
    }

    int cwchWCS2 = cchMBCS / 2;

    DWORD dwError = cchMBCS & 1 ? ERROR_NO_UNICODE_TRANSLATION : 0;

    if (pwchWCS == NULL)
    {
        if (cwchWCS != 0)
        {
            return ERROR_INVALID_PARAMETER;
        }
    }
    else
    {
        if (cwchWCS2 > cwchWCS)
        {
            cwchWCS2 = cwchWCS;
            dwError  = ERROR_INSUFFICIENT_BUFFER;
        }

        LPCSTR pchScan    = pchMBCS;
        LPCSTR pchScanEnd = pchMBCS + cchMBCS;
        LPSTR  pchDest    = reinterpret_cast<CHAR*>(pwchWCS);
        while (pchScan < pchScanEnd)
        {
            *pchDest++ = pchScan[1];
            *pchDest++ = pchScan[0];

            pchScan += 2;
        }
    }

    *inout_cchMBCS = cwchWCS2 * 2;
    *inout_cwchWCS = cwchWCS2;

    return dwError;
} // CConvertToUnicode::fromUTF16BE


//////////////////////////////////////////////////////////////////////
//
// Convert UTF16LE string to Unicdoe
//
//
DWORD
CConvertToUnicode::fromUTF16LE(
    LPCSTR  pchMBCS,
    int*    inout_cchMBCS,
    LPWSTR  pwchWCS,
    int*    inout_cwchWCS )
{
    int cchMBCS = *inout_cchMBCS;
    int cwchWCS = *inout_cwchWCS;

    if (cchMBCS == -1)
    {
        cchMBCS = ::lstrlenW(reinterpret_cast<LPCWSTR>(pchMBCS)) + 1;
    }
    else if (cchMBCS < 0)
    {
        return ERROR_INVALID_PARAMETER;
    }

    int cwchWCS2 = cchMBCS / 2;

    DWORD dwError = cchMBCS & 1 ? ERROR_NO_UNICODE_TRANSLATION : 0;

    if (pwchWCS == NULL)
    {
        if (cwchWCS != 0)
        {
            return ERROR_INVALID_PARAMETER;
        }
    }
    else
    {
        if (cwchWCS2 > cwchWCS)
        {
            cwchWCS2 = cwchWCS;
            dwError  = ERROR_INSUFFICIENT_BUFFER;
        }

        // PORTING: x86 is little endian machine.
        ::CopyMemory(pwchWCS, pchMBCS, cwchWCS2 * sizeof(WCHAR));
    }

    *inout_cchMBCS = cwchWCS2 * 2;
    *inout_cwchWCS = cwchWCS2;

    return dwError;

} // CConvertToUnicode::fromUTF16LE


//////////////////////////////////////////////////////////////////////
//
// Convert UTF8 string to Unicdoe
//
//
DWORD
CConvertToUnicode::fromUTF8(
    LPCSTR  pchMBCS,
    int*    inout_cchMBCS,
    LPWSTR  pwchWCS,
    int*    inout_cwchWCS )
{
    DWORD dwError = 0;
    int cchMBCS = *inout_cchMBCS;
    int cwchWCS = *inout_cwchWCS;

    if (cchMBCS == -1)
    {
        cchMBCS = ::lstrlenA(pchMBCS) + 1;
    }
    else if (cchMBCS < 0)
    {
        return ERROR_INVALID_PARAMETER;
    }

    LPCSTR pchScan     = pchMBCS;
    LPCSTR pchScanEnd  = pchMBCS + cchMBCS;
    LPWSTR pwchDest    = pwchWCS;

    if (pwchWCS == NULL)
    {
        if (cwchWCS != 0)
        {
            return ERROR_INVALID_PARAMETER;
        }
        cwchWCS = MAXLONG;
    }

    LPWSTR pwchDestEnd = pwchWCS + cwchWCS;
    LPCSTR pchStart;
    for (;;)
    {
        pchStart = pchScan;
        if (pchScan >= pchScanEnd)
        {
            break;
        }

        int iChar = static_cast<BYTE>(*pchScan++);

        if (iChar < 0x80)           // 1 byte
        {
            if (pwchDest > pwchDestEnd)
            {
                goto errorInsufficentBuffer;
            }

            if (pwchWCS != NULL)
            {
                *pwchDest = static_cast<WCHAR>(iChar);
            }
            pwchDest++;
            continue;
        }

        if (iChar < 0xC0)           // illegal
        {
            goto errorIllegalChar;
        }

        DWORD dwChar = iChar;

        if (iChar < 0xE0)           // 2byte
        {
            dwChar &= 0x1F;
            goto get_2byte;
        }

        if (iChar < 0xF0)           // 3byte
        {
            dwChar &= 0x0F;
            goto get_3byte;
        }

        if (iChar < 0xF8)            // 4byte
        {
            dwChar &= 0x07;
            goto get_4byte;
        }

        if (iChar < 0xFC)           // 5byte
        {
            dwChar &= 0x03;
            goto get_5byte;
        }

        dwChar &= 0x01;
        goto get_6byte;

get_6byte:
        if (pchScan >= pchScanEnd) goto errorTruncate;
        iChar = static_cast<BYTE>(*pchScan++);
        dwChar <<= 6;
        dwChar |= iChar & 0x3F;

get_5byte:
        if (pchScan >= pchScanEnd) goto errorTruncate;
        iChar = static_cast<BYTE>(*pchScan++);
        dwChar <<= 6;
        dwChar |= iChar & 0x3F;

get_4byte:
        if (pchScan >= pchScanEnd) goto errorTruncate;
        iChar = static_cast<BYTE>(*pchScan++);
        dwChar <<= 6;
        dwChar |= iChar & 0x3F;

get_3byte:
        if (pchScan >= pchScanEnd) goto errorTruncate;
        iChar = static_cast<BYTE>(*pchScan++);
         dwChar <<= 6;
         dwChar |= iChar & 0x3F;

get_2byte:
        if (pchScan >= pchScanEnd) goto errorTruncate;
        iChar = static_cast<BYTE>(*pchScan++);
         dwChar <<= 6;
         dwChar |= iChar & 0x3F;

//get_1byte:
        if (dwChar > 0x10FFFF)
        {
            dwChar = k_wchReplacement;
        }

        if (dwChar <= 0xFFFF)
        {
            if (pwchDest > pwchDestEnd)
            {
                goto errorInsufficentBuffer;
            }

            if (pwchWCS != NULL)
            {
                *pwchDest = static_cast<WCHAR>(dwChar);
            }
            pwchDest++;
             continue;
         }

         if (dwChar <= 0x10FFFF)
         {
             dwChar -= 0x10000;

            if (pwchDest + 1 > pwchDestEnd)
            {
                goto errorInsufficentBuffer;
            }

            if (pwchWCS != NULL)
            {
                pwchDest[0] = static_cast<WCHAR>(
                    (dwChar >> 10) + k_wchSurrogateHBeg );

                pwchDest[1] = static_cast<WCHAR>(
                    (dwChar & 0x3FF) + k_wchSurrogateLBeg );
            }
            pwchDest += 2;
            continue;
         }
    } // while

    goto exit;

errorTruncate:
    pchScan = pchStart;
    dwError = ERROR_NO_UNICODE_TRANSLATION;
    goto exit;

errorIllegalChar:
    pchScan = pchStart;
    dwError = ERROR_NO_UNICODE_TRANSLATION;
    goto exit;

errorInsufficentBuffer:
    dwError = ERROR_INSUFFICIENT_BUFFER;
    goto exit;

exit:
    *inout_cchMBCS = pchScan - pchMBCS;
    *inout_cwchWCS = pwchDest - pwchWCS;

    return dwError;

} // CConvertToUnicode::fromUTF8
