//////////////////////////////////////////////////////////////////////////////
//
// ConvertToUnicode interface declratation.
//
// Copyright (C) 1996-2003 by Project Vogue.
// Written by Yoshimi "VOGUE" FUNAI. (yosi@msn.com)
//
// @(#)$Id: /proj/evita/shared/Conv2WC.h 1 2002-04-27 20:59:00 yosi $
//

#if !defined(INCLUDE_ConvertToUnicode_h)
#define INCLUDE_ConvertToUnicode_h


class CConvertToUnicode
{
public:
    ////////////////////////////////////////////////////////////
    //
    // CConvertToUnicode public methods
    //
    CConvertToUnicode(
        UINT    nCodePage );

    ~CConvertToUnicode();

    static HRESULT Create(
        UINT                nCodePage,
        CConvertToUnicode** out_pConverter );


    DWORD Convert(
        LPCSTR  pchMBCS,
        int*    inout_cchMBCS,
        LPWSTR  out_rgwchWCS,
        int*    inout_cwchWCS )
    {
        return ConvertEx(
            MB_ERR_INVALID_CHARS | MB_PRECOMPOSED,
            pchMBCS, inout_cchMBCS,
            out_rgwchWCS, inout_cwchWCS );
    }

    DWORD ConvertEx(
        DWORD   dwFlags,
        LPCSTR  pchMBCS,
        int*    inout_cchMBCS,
        LPWSTR  out_rgwchWCS,
        int*    inout_cwchWCS );
protected:
    ////////////////////////////////////////////////////////////
    //
    // CConvertToUnicode protected methods
    //
    static DWORD fromEUC_JP(
        DWORD               dwFlags,
        LPCSTR              pchMBCS,
        int*                inout_cchMBCS,
        LPWSTR              pwchWCS,
        int*                inout_cwchWCS );

    DWORD fromISO2022(
        DWORD               dwFlags,
        LPCSTR              pchMBCS,
        int*                inout_cchMBCS,
        LPWSTR              pwchWCS,
        int*                inout_cwchWCS );

    static DWORD fromUTF16BE(
        LPCSTR              pchMBCS,
        int*                inout_cchMBCS,
        LPWSTR              pwchWCS,
        int*                inout_cwchWCS );

    static DWORD fromUTF16LE(
        LPCSTR              pchMBCS,
        int*                inout_cchMBCS,
        LPWSTR              pwchWCS,
        int*                inout_cwchWCS );

    static DWORD fromUTF8(
        LPCSTR              pchMBCS,
        int*                inout_cchMBCS,
        LPWSTR              pwchWCS,
        int*                inout_cwchWCS );

protected:
    ////////////////////////////////////////////////////////////
    //
    // CConvertToUnicode member variables
    //
    UINT    m_nCodePage;

    enum StateT
    {
        ISO2022_SBC,
        ISO2022_SBC_G1,
        ISO2022_SBC_SI,
        ISO2022_SBC_SO,
        ISO2022_DBC_1,
        ISO2022_DBC_2,
        ISO2022_ESC,
        ISO2022_ESC_4F,
        ISO2022_ESC_8F,
        ISO2022_ESC_4F_8F,
    }; // enum StateT

    StateT m_eState;

}; // class CConvertToUnicode


#endif // !defined(INCLUDE_ConvertToUnicode_h)
