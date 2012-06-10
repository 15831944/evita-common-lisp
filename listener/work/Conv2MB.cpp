#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// CConvertToMultiByte class implementation.
//
// Copyright (C) 1996-2003 by Project Vogue.
// Written by Yoshimi "VOGUE" FUNAI. (yosi@msn.com)
//
// @(#)$Id: /proj/evita/shared/Conv2MB.cpp 1 2002-07-28 17:37:00 yosi $
//
#include "./Conv2MB.h"


__forceinline void
sjis_to_jis(
    int&    iFirst,
    int&    iSecond )
{
    ASSERT(iFirst >= 0x81 && iFirst <= 0x9F ||
           iFirst >= 0xE0 && iFirst <= 0xFC );

    ASSERT(iSecond >= 0x40 && iSecond <= 0x7E ||
           iSecond >= 0x80 && iSecond <= 0xFC );

    int iAdjust = iSecond < 159;
    int iRow = iFirst < 160 ? 112 : 176;
    int iCell = iAdjust ? (iSecond > 127 ? 32 : 31) : 126;

    iFirst = ((iFirst - iRow) << 1) - iAdjust;
    iSecond -= iCell;
} // sjis_to_jis


//////////////////////////////////////////////////////////////////////////////
//
// EUC-JP
//
//  CS0: 0x21-0x7E
//  CS1: 0xA1..0xFE 0xA1..0xFE      (JIS-X208-1990)
//  CS2: 0x8E 0xA1..0xFE            (half-width katakana)
//  CS3: 0x8F 0xA1..0xFE 0xA1..0xFE
//
DWORD
CConvertToMultiByte::ToEUC_JP(
    LPCWSTR pwchWCS,
    int*    inout_cwchWCS,
    LPSTR   pchMBCS,
    int*    inout_cchMBCS,
    LPCSTR  pchDefaultChar,
    BOOL*   out_fUsedDefaultChar )
{
    int cwchWCS = *inout_cwchWCS;

    if (cwchWCS == -1)
    {
        cwchWCS = ::lstrlenW(pwchWCS) + 1;
    }
    else if (cwchWCS < 0)
    {
        return ERROR_INVALID_PARAMETER;
    }

    LPCWSTR pwchScan    = pwchWCS;
    LPCWSTR pwchScanEnd = pwchScan + cwchWCS;

    int cchMBCS = *inout_cchMBCS;

    LPSTR pchDest = pchMBCS;

    if (pchMBCS == NULL)
    {
        if (cchMBCS != 0)
        {
            return ERROR_INVALID_PARAMETER;
        }
        cchMBCS = MAXLONG;
    }

    LPSTR pchDestEnd = pchDest + cchMBCS;

    if (out_fUsedDefaultChar != NULL)
    {
        *out_fUsedDefaultChar = FALSE;
    }

    DWORD dwError;

    while (pwchScan < pwchScanEnd)
    {
        BOOL  fUsedDefaultChar;
        CHAR  rgchMBCS[2];
        int cchMBCS = ::WideCharToMultiByte(
            CP_SJIS,
            0,
            pwchScan, 1,
            rgchMBCS, 2,
            pchDefaultChar,
            &fUsedDefaultChar );

        if (cchMBCS == 1)
        {
            if (static_cast<BYTE>(rgchMBCS[0]) <= 0x7F)
            {
                if (pchDest + 1 > pchDestEnd)
                {
                    goto errorInsufficentBuffer;
                }

                if (pchMBCS != NULL)
                {
                    pchDest[0] = rgchMBCS[0];
                }

                pchDest += 1;
            }
            else
            {
                if (pchDest + 2 > pchDestEnd)
                {
                    goto errorInsufficentBuffer;
                }

                if (pchMBCS != NULL)
                {
                    pchDest[0] = static_cast<BYTE>(0x8E);
                    pchDest[1] = rgchMBCS[0];
                }

                pchDest += 2;
            }
        }
        else if (cchMBCS == 2)
        {
            int iFirst  = rgchMBCS[0] & 0xFF;
            int iSecond = rgchMBCS[1] & 0xFF;
                sjis_to_jis(iFirst, iSecond);

            if (pchDest + 2 > pchDestEnd)
            {
                goto errorInsufficentBuffer;
            }

            if (pchMBCS != NULL)
            {
                pchDest[0] = static_cast<CHAR>(iFirst  | 0x80);
                pchDest[1] = static_cast<CHAR>(iSecond | 0x80);
            }

            pchDest += 2;
        }
        else
        {
            ASSERT(cchMBCS == 0);
            dwError = ::GetLastError();
            goto exit;
        }

        if (fUsedDefaultChar && out_fUsedDefaultChar != NULL)
        {
            *out_fUsedDefaultChar = TRUE;
        }

        pwchScan++;
    } // while

    dwError = 0;
    goto exit;

errorInsufficentBuffer:
    dwError = ERROR_INSUFFICIENT_BUFFER;
    goto exit;

exit:
    *inout_cchMBCS = pchDest - pchMBCS;
    *inout_cwchWCS = pwchScan - pwchWCS;
    return dwError;
} // CConvertToMultiByte::ToEUC_JP

//////////////////////////////////////////////////////////////////////////////
//
// ISO2022-JP
//
// 1) Starts with ASCII
// 2) Discarage to use JIS X 201-Roamn
// 3) Half-width Katakanas are converted based on code page value:
//      CP_ISO2022_JP       Default character
//      CP_ISO2022_JP_202   "ESC(I"
//      CP_ISO2022_JP_201   "ESC(J" SO
//
// ISO-2022-JP-2 (RFC1554)
//
// 94 chracacter sets
//    reg#  character set      ESC sequence                designated to
//    ------------------------------------------------------------------
//    6     ASCII              ESC 2/8 4/2      ESC ( B    G0
//    42    JIS X 0208-1978    ESC 2/4 4/0      ESC $ @    G0
//    87    JIS X 0208-1983    ESC 2/4 4/2      ESC $ B    G0
//    13    JIS X 0201-Roman   ESC 2/8 4/10     ESC ( J    G0 *1
//    14    JIS X 0201-Kana    ESC 2/8 4/9      ESC ( I    G1 *2
//    58    GB2312-1980        ESC 2/4 4/1      ESC $ A    G0
//    149   KSC5601-1987       ESC 2/4 2/8 4/3  ESC $ ( C  G0
//    159   JIS X 0212-1990    ESC 2/4 2/8 4/4  ESC $ ( D  G0
//
// *1 RFC1554 incorrectly describes JIS X 0201-Roman as ISOREG#14.
// *2 JIS X0201-Kana is not part of ISO-2022-JP
//
// 96 character sets 
//    reg#  character set      ESC sequence                designated to
//    ------------------------------------------------------------------
//    100   ISO8859-1          ESC 2/14 4/1     ESC . A    G2
//    126   ISO8859-7(Greek)   ESC 2/14 4/6     ESC . F    G2
//
//

const CHAR ISO2022_ESCAPE = '\x1B';
const CHAR ISO2022_SO     = '\x0E'; // to G1
const CHAR ISO2022_SI     = '\x0F'; // to G0

DWORD
CConvertToMultiByte::toISO2022_JP(
    LPCWSTR pwchWCS,
    int*    inout_cwchWCS,
    LPSTR   pchMBCS,
    int*    inout_cchMBCS,
    LPCSTR  pchDefaultChar,
    BOOL*   out_fUsedDefaultChar )
{
    int cwchWCS = *inout_cwchWCS;

    if (cwchWCS == -1)
    {
        cwchWCS = ::lstrlenW(pwchWCS) + 1;
    }
    else if (cwchWCS < 0)
    {
        return ERROR_INVALID_PARAMETER;
    }

    LPCWSTR pwchScan    = pwchWCS;
    LPCWSTR pwchScanEnd = pwchScan + cwchWCS;

    int cchMBCS = *inout_cchMBCS;

    LPSTR pchDest = pchMBCS;

    if (pchMBCS == NULL)
    {
        if (cchMBCS != 0)
        {
            return ERROR_INVALID_PARAMETER;
        }
        cchMBCS = MAXLONG;
    }

    CHAR chDefaultChar;
        if (pchDefaultChar == NULL)
        {
            chDefaultChar = '?';    // for half-width katakana
        }
        else
        {
            chDefaultChar = *pchDefaultChar;
            if (static_cast<BYTE>(chDefaultChar) >= 0x80)
            {
                return ERROR_INVALID_PARAMETER;
            }
        }


    LPSTR pchDestEnd = pchDest + cchMBCS;

    if (out_fUsedDefaultChar != NULL)
    {
        *out_fUsedDefaultChar = FALSE;
    }

    DWORD dwError;

    enum ISO2022_StateT
    {
        STATE_ASCII,
        STATE_JISX0202_G1,
        STATE_JISX0201_SO,
        STATE_JISX0208
    }; // enum ISO2022_StateT

    ISO2022_StateT eState = STATE_ASCII;

    while (pwchScan < pwchScanEnd)
    {
        BOOL  fUsedDefaultChar;
        CHAR  rgchMBCS[2];
        int cchMBCS = ::WideCharToMultiByte(
            CP_SJIS,
            0,
            pwchScan, 1,
            rgchMBCS, 2,
            pchDefaultChar,
            &fUsedDefaultChar );

        switch (cchMBCS)
        {
        case 1:
        ////////////////////////////////////////////////////////////
        //
        // Single-byte character
        //
        {
            int iChar = static_cast<BYTE>(rgchMBCS[0]);

            if (iChar < 0xA1 || iChar > 0xDF)
            {
                ////////////////////////////////////////////////////////////
                //
                // ASCII    0x00...0x7F
                // C1       0x80...0x9F
                // G1       0xA0
                // G1       0xE0...0xFF
                //
                if (eState != STATE_ASCII)
                {
                    if (pchDest + 3 > pchDestEnd)
                    {
                        goto errorInsufficentBuffer;
                    }

                    if (pchMBCS != NULL)
                    {
                        pchDest[0] = ISO2022_ESCAPE;
                        pchDest[1] = '(';
                        pchDest[2] = 'B';
                    }

                    pchDest += 3;
                    eState = STATE_ASCII;
                }

                if (pchDest + 1 > pchDestEnd)
                {
                    goto errorInsufficentBuffer;
                }

                if (pchMBCS != NULL)
                {
                    pchDest[0] = rgchMBCS[0];
                }

                pchDest += 1;
            }
            else
            {
                ////////////////////////////////////////////////////////////
                //
                // Halfwidth Katakana
                //
                ASSERT(iChar >= 0xA1 && iChar <= 0xDF);
                switch (m_nCodePage)
                {
                case CP_ISO2022_JP:
                    if (eState != STATE_ASCII)
                    {
                        if (pchDest + 3 > pchDestEnd)
                        {
                            goto errorInsufficentBuffer;
                        }

                        if (pchMBCS != NULL)
                        {
                            pchDest[0] = ISO2022_ESCAPE;
                            pchDest[1] = '(';
                            pchDest[2] = 'B';
                        }

                        pchDest += 3;
                        eState = STATE_ASCII;
                    }

                    if (pchDest + 1 > pchDestEnd)
                    {
                        goto errorInsufficentBuffer;
                    }

                    rgchMBCS[0] = chDefaultChar;
                    fUsedDefaultChar = TRUE;

                    pchDest += 1;
                    break;

                case CP_ISO2022_JP_0202:
                    if (eState != STATE_JISX0202_G1)
                    {
                        if (pchDest + 3 > pchDestEnd)
                        {
                            goto errorInsufficentBuffer;
                        }

                        if (pchMBCS != NULL)
                        {
                            pchDest[0] = ISO2022_ESCAPE;
                            pchDest[1] = '(';
                            pchDest[2] = 'I';
                        }

                        pchDest += 3;
                        eState = STATE_JISX0202_G1;
                    }
                    break;

                case CP_ISO2022_JP_0201:
                    if (eState != STATE_JISX0201_SO)
                    {
                        if (pchDest + 4 > pchDestEnd)
                        {
                            goto errorInsufficentBuffer;
                        }

                        if (pchMBCS != NULL)
                        {
                            pchDest[0] = ISO2022_ESCAPE;
                            pchDest[1] = '(';
                            pchDest[2] = 'J';
                            pchDest[3] = ISO2022_SO;
                        }

                        pchDest += 4;
                        eState = STATE_JISX0201_SO;
                    }
                    break;

                default:
                    CAN_NOT_HAPPEN();
                    return ERROR_INVALID_PARAMETER;
                } // switch m_nCodePage

                if (pchDest + 1 > pchDestEnd)
                {
                    goto errorInsufficentBuffer;
                }

                if (pchMBCS != NULL)
                {
                    pchDest[0] = static_cast<CHAR>(rgchMBCS[0] & 0x7F);
                }

                pchDest += 1;
            }
            break;
        } // casse 1

        case 2:
        ////////////////////////////////////////////////////////////
        //
        // Double byte character
        //
        {
            int iFirst  = rgchMBCS[0] & 0xFF;
            int iSecond = rgchMBCS[1] & 0xFF;
                sjis_to_jis(iFirst, iSecond);

            if (eState != STATE_JISX0208)
            {
                if (pchDest + 3 > pchDestEnd)
                {
                    goto errorInsufficentBuffer;
                }

                if (pchMBCS != NULL)
                {
                    pchDest[0] = ISO2022_ESCAPE;
                    pchDest[1] = '$';
                    pchDest[2] = 'B';
                }

                pchDest += 3;
                eState = STATE_JISX0208;
            }

            if (pchDest + 2 > pchDestEnd)
            {
                goto errorInsufficentBuffer;
            }

            if (pchMBCS != NULL)
            {
                pchDest[0] = static_cast<CHAR>(iFirst);
                pchDest[1] = static_cast<CHAR>(iSecond);
            }

            pchDest += 2;
            break;
        } // case 2

        default:
            ////////////////////////////////////////////////////////////
            //
            // WC to MB failed
            //
            ASSERT(cchMBCS == 0);
            dwError = ::GetLastError();
            goto exit;

        } // switch cchMBCS

        if (fUsedDefaultChar && out_fUsedDefaultChar != NULL)
        {
            *out_fUsedDefaultChar = TRUE;
        }

        pwchScan++;
    } // while

    ////////////////////////////////////////////////////////////
    //
    // Reset state to ASCII
    //
    if (eState != STATE_ASCII)
    {
        if (pchDest + 3 > pchDestEnd)
        {
            goto errorInsufficentBuffer;
        }

        if (pchMBCS != NULL)
        {
            pchDest[0] = ISO2022_ESCAPE;
            pchDest[1] = '(';
            pchDest[2] = 'B';
        }

        pchDest += 3;
    } // if

    dwError = 0;
    goto exit;

errorInsufficentBuffer:
    dwError = ERROR_INSUFFICIENT_BUFFER;
    goto exit;

exit:
    *inout_cchMBCS = pchDest - pchMBCS;
    *inout_cwchWCS = pwchScan - pwchWCS;
    return dwError;
} // CConvertToMultiByte::toISO2022_JP

//////////////////////////////////////////////////////////////////////////////
//
// UTF8
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
CConvertToMultiByte::ToUTF16BE(
    LPCWSTR pwchWCS,
    int*    inout_cwchWCS,
    LPSTR   pchMBCS,
    int*    inout_cchMBCS )
{
    int cwchWCS = *inout_cwchWCS;

    if (cwchWCS == -1)
    {
        cwchWCS = ::lstrlenW(pwchWCS);
    }
    else if (cwchWCS < 0)
    {
        return ERROR_INVALID_PARAMETER;
    }

    int cchMBCS = *inout_cchMBCS;
    int cchMBCS2 = cwchWCS * 2;

    DWORD dwError = 0;

    if (pchMBCS == NULL)
    {
        if (cchMBCS != 0)
        {
            return ERROR_INVALID_PARAMETER;
        }
    }
    else
    {
        if (cchMBCS2 > cchMBCS)
        {
            cchMBCS2 = cchMBCS;
            dwError  = ERROR_INSUFFICIENT_BUFFER;
        }

        LPCSTR pchScan    = reinterpret_cast<LPCSTR>(pwchWCS);
        LPCSTR pchScanEnd = reinterpret_cast<LPCSTR>(pwchWCS + cwchWCS);
        LPSTR  pchDest    = pchMBCS;
        while (pchScan < pchScanEnd)
        {
            *pchDest++ = pchScan[1];
            *pchDest++ = pchScan[0];

            pchScan += 2;
        }
    }

    *inout_cchMBCS = cchMBCS2;
    *inout_cwchWCS = cchMBCS2 / 2;

    return dwError;

} // CConvertToMultiByteTo::UTF16BE


//////////////////////////////////////////////////////////////////////
//
// Convert UTF16LE string to Unicdoe
//
//
DWORD
CConvertToMultiByte::ToUTF16LE(
    LPCWSTR pwchWCS,
    int*    inout_cwchWCS,
    LPSTR   pchMBCS,
    int*    inout_cchMBCS )
{
    int cwchWCS = *inout_cwchWCS;

    if (cwchWCS == -1)
    {
        cwchWCS = ::lstrlenW(pwchWCS);
    }
    else if (cwchWCS < 0)
    {
        return ERROR_INVALID_PARAMETER;
    }

    int cchMBCS = *inout_cchMBCS;
    int cchMBCS2 = cwchWCS * 2;

    DWORD dwError = 0;

    if (pchMBCS == NULL)
    {
        if (cchMBCS != 0)
        {
            return ERROR_INVALID_PARAMETER;
        }
    }
    else
    {
        if (cchMBCS2 > cchMBCS)
        {
            cchMBCS2 = cchMBCS;
            dwError  = ERROR_INSUFFICIENT_BUFFER;
        }

        ::CopyMemory(pchMBCS, pwchWCS, cchMBCS2);
    }

    *inout_cchMBCS = cchMBCS2;
    *inout_cwchWCS = cchMBCS2 / 2;

    return dwError;

} // CConvertToMultiByteTo::UTF16LE


//////////////////////////////////////////////////////////////////////
//
// Convert Unicode To UTF8.
//
//
DWORD
CConvertToMultiByte::ToUTF8(
    LPCWSTR pwchWCS,
    int*    inout_cwchWCS,
    LPSTR   pchMBCS,
    int*    inout_cchMBCS )
{
    int cwchWCS = *inout_cwchWCS;

    if (-1 == cwchWCS)
    {
        cwchWCS = ::lstrlenW(pwchWCS) + 1;
    }
    else if (cwchWCS < 0)
    {
        return ERROR_INVALID_PARAMETER;
    }

    LPCWSTR pwchScan    = pwchWCS;
    LPCWSTR pwchScanEnd = pwchScan + cwchWCS;

    int cchMBCS = *inout_cchMBCS;

    LPSTR pchDest = pchMBCS;

    if (pchMBCS == NULL)
    {
        if (cchMBCS != 0)
        {
            return ERROR_INVALID_PARAMETER;
        }
        cchMBCS = MAXLONG;
    }

    LPSTR pchDestEnd = pchMBCS + cchMBCS;

    DWORD dwError;

    while (pwchScan < pwchScanEnd)
    {
        WCHAR wchChar = *pwchScan++;

        DWORD dwchChar = wchChar;
        if (wchChar >= k_wchSurrogateHBeg && wchChar <= k_wchSurrogateHEnd)
        {
            if (pwchScan >= pwchScanEnd)
            {
                goto errorTruncated;
            }

            wchChar = *pwchScan++;

            if (wchChar >= k_wchSurrogateLBeg && wchChar <= k_wchSurrogateLEnd)
            {
                dwchChar -= k_wchSurrogateHEnd;
                dwchChar <<= 10;
                dwchChar += wchChar - k_wchSurrogateLBeg;
                dwchChar += 0x00100000;
            }
        }

        if (dwchChar <= 0x0000007F)
        {
            if (pchDest >= pchDestEnd)
            {
                goto errorInsufficentBuffer;
            }
            if (pchMBCS != NULL)
            {
                *pchDest++ = static_cast<char>(wchChar);
            }
            else
            {
                pchDest += 1;
            }
            continue;

        }
        else if (dwchChar <= 0x000007FF)
        {
            if (pchDest + 1 >= pchDestEnd)
            {
                goto errorInsufficentBuffer;
            }
            if (pchMBCS != NULL)
            {
                *pchDest++ = static_cast<char>(
                    0xC0 | ((dwchChar >>  6) & 0x3F) );
            }
            else
            {
                pchDest += 2;
                continue;
            }
            goto emit_2byte;

        }
        else if (dwchChar <= 0x0000FFFF)
        {
            if (pchDest + 2 >= pchDestEnd)
            {
                goto errorInsufficentBuffer;
            }
            if (pchMBCS != NULL)
            {
                *pchDest++ = static_cast<char>(
                    0xE0 | ((dwchChar >> 12) & 0x3F) );
            }
            else
            {
                pchDest += 3;
                continue;
            }
            goto emit_3byte;

        }
        else if (dwchChar <= 0x0001FFFF)
        {
            if (pchDest + 3 >= pchDestEnd)
            {
                goto errorInsufficentBuffer;
            }
            if (pchMBCS != NULL)
            {
                *pchDest++ = static_cast<char>(
                    0xF0 | ((dwchChar >> 18) & 0x3F) );
            }
            else
            {
                pchDest += 4;
                continue;
            }
            goto emit_4byte;

        }
        else if (dwchChar <= 0x003FFFFF)
        {
            if (pchDest + 4 >= pchDestEnd)
            {
                goto errorInsufficentBuffer;
            }
            if (pchMBCS != NULL)
            {
                *pchDest++ = static_cast<char>(
                    0xF8 | ((dwchChar >> 24) & 0x3F) );
            }
            else
            {
                pchDest += 5;
                continue;
            }
            goto emit_5byte;

        }
        else if (dwchChar <= 0x7FFFFFFF)
        {
            if (pchDest + 5 >= pchDestEnd)
            {
                goto errorInsufficentBuffer;
            }

            if (pchMBCS != NULL)
            {
                *pchDest++ = static_cast<char>(
                    0xFC | ((dwchChar >> 30) & 0x3F) );
            }
            else
            {
                pchDest += 6;
                continue;
            }
            goto emit_6byte;

        }
        else
        {
            wchChar = k_wchReplacement;
            if (pchDest + 1 > pchDestEnd)
            {
                goto errorInsufficentBuffer;
            }

            if (pchMBCS != NULL)
            {
                *pchDest++ = static_cast<char>(
                    0xC0 | ((dwchChar >>  6) & 0x3F) );
            }
            else
            {
                pchDest += 2;
                continue;
            }
            goto emit_2byte;
        }

emit_6byte:
        *pchDest++ = static_cast<BYTE>(0x80 | ((dwchChar >> 24) & 0x3F));

emit_5byte:
        *pchDest++ = static_cast<BYTE>(0x80 | ((dwchChar >> 18) & 0x3F));

emit_4byte:
        *pchDest++ = static_cast<BYTE>(0x80 | ((dwchChar >> 12) & 0x3F));

emit_3byte:
        *pchDest++ = static_cast<BYTE>(0x80 | ((dwchChar >>  6) & 0x3F));

emit_2byte:
        *pchDest++ = static_cast<BYTE>(0x80 | ((dwchChar >>  0) & 0x3F));
    } // while

    dwError = 0;
    goto exit;

errorInsufficentBuffer:
    pwchScan -= 1;
    dwError = ERROR_INSUFFICIENT_BUFFER;
    goto exit;

errorTruncated:
    pwchScan -= 1;
    dwError = ERROR_INVALID_PARAMETER;
    goto exit;

exit:
    *inout_cchMBCS = pchDest - pchMBCS;
    *inout_cwchWCS = pwchScan - pwchWCS;
    return dwError;
} // CConvertToMultiByteTo::UTF8

//////////////////////////////////////////////////////////////////////////////
//
// CConvertToMultiByte public static methods
//


//////////////////////////////////////////////////////////////////////
//
// Create CConvertToMultiByte object
//
HRESULT
CConvertToMultiByte::Create(
    UINT                    nCodePage,
    CConvertToMultiByte**   out_pConverter )
{
    HRESULT hr;

    ASSERT(NULL != out_pConverter);

    switch (nCodePage)
    {
    case CP_UTF16_LE:
    case CP_UTF16_BE:
    case CP_UTF8:
    case CP_UTF7:
        break;

    default:
    {
        CPINFO oCPInfo;
        if (! ::GetCPInfo(nCodePage, &oCPInfo))
        {
            DWORD dwError = ::GetLastError();
            REPORT_WIN32_ERROR("GetCPInfo", dwError);
            return HRESULT_FROM_WIN32(dwError);
        }
        break;
    } // default
    } // switch nCodepage

    CConvertToMultiByte* pConverter = new CConvertToMultiByte(nCodePage);
    if (NULL == pConverter)
    {
        hr = E_OUTOFMEMORY;
        REPORT_HRESULT("new CConvertToMultiByte", hr);
        return hr;
    }

    *out_pConverter = pConverter;

    return S_OK;

} // CConvertToMultiByte::Create

//////////////////////////////////////////////////////////////////////////////
//
// CConvertToMultiByte public methods
//

//////////////////////////////////////////////////////////////////////
//
// CConvertToMultiByte constructor
//
CConvertToMultiByte::CConvertToMultiByte(
    UINT    nCodePage )
{
    // Code Page must be valid.
    m_nCodePage = nCodePage;

} // CConvertToMultiByte::CConvertToMultiByte


//////////////////////////////////////////////////////////////////////
//
// CConvertToMultiByte destructor
//
CConvertToMultiByte::~CConvertToMultiByte()
{
    // nothing to do

} // CConvertToMultiByte::~CConvertToMultiByte


DWORD
CConvertToMultiByte::ConvertEx(
    DWORD   dwFlags,
    LPCWSTR pwchWCS,
    int*    inout_cwchWCS,
    LPSTR   pchMBCS,
    int*    inout_cchMBCS,
    LPCSTR  pchDefaultChar,
    BOOL*   out_fUsedDefaultChar )
{
    if (NULL == inout_cwchWCS)
    {
        return ERROR_INVALID_PARAMETER;
    }

    if (NULL == inout_cchMBCS)
    {
        return ERROR_INVALID_PARAMETER;
    }

    if (0 == *inout_cwchWCS)
    {
        *inout_cchMBCS = 0;
        return 0;
    }

    switch (m_nCodePage)
    {
    case CP_UTF8:
        ToUTF8(pwchWCS, inout_cwchWCS, pchMBCS, inout_cchMBCS);
        if (out_fUsedDefaultChar != NULL) *out_fUsedDefaultChar = FALSE;
        return 0;

    case CP_UTF16_LE:
        ToUTF16LE(pwchWCS, inout_cwchWCS, pchMBCS, inout_cchMBCS);
        if (out_fUsedDefaultChar != NULL) *out_fUsedDefaultChar = FALSE;
        return 0;

    case CP_UTF16_BE:
        ToUTF16BE(pwchWCS, inout_cwchWCS, pchMBCS, inout_cchMBCS);
        if (out_fUsedDefaultChar != NULL) *out_fUsedDefaultChar = FALSE;
        return 0;

    case CP_EUC_JP:
        return ToEUC_JP(
            pwchWCS, inout_cwchWCS,
            pchMBCS, inout_cchMBCS,
            pchDefaultChar,
            out_fUsedDefaultChar );

    case CP_ISO2022_JP:
    case CP_ISO2022_JP_0201:
    case CP_ISO2022_JP_0202:
        return toISO2022_JP(
            pwchWCS, inout_cwchWCS,
            pchMBCS, inout_cchMBCS,
            pchDefaultChar,
            out_fUsedDefaultChar );

    // BUGBUG: NYI: UTF-7

    } // switch codePage

    ////////////////////////////////////////////////////////////
    //
    // Use Windows Code Page
    //
    int cwchWCS = *inout_cwchWCS;
    int cchMBCS = *inout_cchMBCS;

    for (;;)
    {
        ASSERT(cwchWCS >= 1);

        int cwchGot = ::WideCharToMultiByte(
            m_nCodePage,
            dwFlags,
            pwchWCS, cwchWCS,
            pchMBCS, cchMBCS,
            pchDefaultChar,
            out_fUsedDefaultChar );

        if (cwchGot >= 1)
        {
            *inout_cwchWCS = cwchWCS;
            *inout_cchMBCS = cwchGot;
            break;
        }

        cwchWCS -= 1;
        if (cwchWCS == 0)
        {
            *inout_cwchWCS = 0;
            *inout_cchMBCS = 0;
            return 0;
        }
    } // for

    return 0;

} // CConvertToMultiByte::ConvertEx
