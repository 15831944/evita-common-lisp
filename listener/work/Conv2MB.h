//////////////////////////////////////////////////////////////////////////////
//
// ConvertToMultiByte interface declratation.
//
// Copyright (C) 1996-2003 by Project Vogue.
// Written by Yoshimi "VOGUE" FUNAI. (yosi@msn.com)
//
// @(#)$Id: /proj/evita/shared/Conv2MB.h 1 2002-04-27 20:59:00 yosi $
//

#if !defined(INCLUDE_ConvertToMultiByte_h)
#define INCLUDE_ConvertToMultiByte_h


class CConvertToMultiByte
{
public:
    ////////////////////////////////////////////////////////////
    //
    // CConvertToMultiByte public static methods
    //
    static HRESULT Create(
        UINT                nCodePage,
        CConvertToMultiByte** out_pConverter );

    static DWORD ToEUC_JP(
        LPCWSTR             pwchWCS,
        int*                inout_cwchWCS,
        LPSTR               pchMBCS,
        int*                inout_cchMBCS,
        LPCSTR              pchDefaultChar,
        BOOL*               out_fUsedDefaultChar );

    static DWORD ToUTF16BE(
        LPCWSTR             pwchWCS,
        int*                inout_cwchWCS,
        LPSTR               pchMBCS,
        int*                inout_cchMBCS );

    static DWORD ToUTF16LE(
        LPCWSTR             pwchWCS,
        int*                inout_cwchWCS,
        LPSTR               pchMBCS,
        int*                inout_cchMBCS );

    static DWORD ToUTF8(
        LPCWSTR             pwchWCS,
        int*                inout_cwchWCS,
        LPSTR               pchMBCS,
        int*                inout_cchMBCS );

public:
    ////////////////////////////////////////////////////////////
    //
    // CConvertToMultiByte public methods
    //
    CConvertToMultiByte(
        UINT    nCodePage );

    ~CConvertToMultiByte();

    DWORD Convert(
        LPCWSTR             pwchWCS,
        int*                inout_cwchWCS,
        LPSTR               pchMBCS,
        int*                inout_cchMBCS )
    {
        return ConvertEx(
            0,
            pwchWCS, inout_cwchWCS,
            pchMBCS, inout_cchMBCS,
            NULL, NULL );
    }

    DWORD ConvertEx(
        DWORD               dwFlags,
        LPCWSTR             pwchWCS,
        int*                inout_cwchWCS,
        LPSTR               pchMBCS,
        int*                inout_cchMBCS,
        LPCSTR              pchDefaultChar,
        BOOL*               out_fUsedDefaultChar );

protected:
    ////////////////////////////////////////////////////////////
    //
    // CConvertToMultiByte protected methods
    //
    DWORD toISO2022_JP(
        LPCWSTR             pwchWCS,
        int*                inout_cwchWCS,
        LPSTR               pchMBCS,
        int*                inout_cchMBCS,
        LPCSTR              pchDefaultChar,
        BOOL*               out_fUsedDefaultChar );

protected:
    ////////////////////////////////////////////////////////////
    //
    // CConvertToMultiByte member variables
    //
    UINT    m_nCodePage;

}; // class CConvertToMultiByte


#endif // !defined(INCLUDE_ConvertToMultiByte_h)
