#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - FontSet class
// listener/winapp/fontset.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_Style.cpp#2 $
//
#include "./vi_style.h"

#include "./vi_util.h"

StyleValues g_DefaultStyle;

// FontSet::Add
void FontSet::Add(Font* pFont)
{
    ASSERT(NULL != pFont);
    m_rgpFont[m_cFonts] = pFont;
    m_cFonts++;
} // FontSet::Add


//////////////////////////////////////////////////////////////////////
//
// Font ctor
//
Font::Font(HGDIOBJ hFont, const TEXTMETRIC* ptm, const LOGFONT* pLogFont) :
    m_hFont(reinterpret_cast<HFONT>(hFont)),
    m_iDescent(ptm->tmDescent),
    m_iHeight(ptm->tmHeight + ptm->tmExternalLeading),
    m_iWidth(ptm->tmAveCharWidth),
    m_fFixedPitch(ptm->tmPitchAndFamily & TMPF_FIXED_PITCH ? false : true),
    m_wchDefault(ptm->tmDefaultChar),
    m_oLogFont(*pLogFont)
{
    //myCopyMemory(&m_oLogFont, pLogFont, sizeof(m_oLogFont));
} // Font::Font


//////////////////////////////////////////////////////////////////////
//
// Font::Create
//
Font* Font::Create(
    const char16*   pwszFamily,
    int             iHeightPt,
    int             iCharSet,
    uint            rgfFlags )
{
    LOGFONT oLogFont;

    {
        Dc hdc(NULL, ::GetDC(NULL));

        oLogFont.lfHeight = -::MulDiv(
            ::GetDeviceCaps(hdc, LOGPIXELSY), iHeightPt, 72 );
    }

    oLogFont.lfWidth = 0;
    oLogFont.lfEscapement = 0;
    oLogFont.lfOrientation = 0;
    oLogFont.lfWeight = rgfFlags & Flag_Bold ? FW_BOLD : FW_NORMAL;
    oLogFont.lfItalic = rgfFlags & Flag_Italic ? 1 : 0;
    oLogFont.lfUnderline = rgfFlags & Flag_Underline ? 1 : 0;
    oLogFont.lfStrikeOut = rgfFlags & Flag_StrikeOut ? 1 : 0;
    oLogFont.lfCharSet   = static_cast<BYTE>(iCharSet);
    oLogFont.lfOutPrecision = OUT_DEFAULT_PRECIS;
    oLogFont.lfClipPrecision = CLIP_DEFAULT_PRECIS;
    oLogFont.lfQuality = DEFAULT_QUALITY;
    oLogFont.lfPitchAndFamily = DEFAULT_PITCH;
    ::lstrcpyW(oLogFont.lfFaceName, pwszFamily);

    return Create(&oLogFont);
} // Font::Create

// Font::Create
Font* Font::Create(const LOGFONT* pLogFont)
{
    HFONT hFont = ::CreateFontIndirect(pLogFont);

    if (hFont == NULL) return NULL;

    Dc hdc(NULL, ::GetDC(NULL));

    GdiObj oFont(hFont);

    DcSelect oSelectFont(hdc, hFont);

    TEXTMETRIC tm;
    if (! ::GetTextMetrics(hdc, &tm)) return NULL;

    Font* pFont = new Font(oFont.Detach(), &tm, pLogFont);
    return pFont;
} // Font::Create


//////////////////////////////////////////////////////////////////////
//
// Font::GetCharWidth
//
int Font::GetCharWidth(HDC hdc, char16 wch)
{
    if (m_fFixedPitch && wch <= 0x7F)
    {
        return GetWidth();
    }

    DcSelect oSelectFont(hdc, m_hFont);

    SIZE size;
    BOOL fSucceeded = ::GetTextExtentPoint32W(hdc, &wch, 1, &size);
    if (! fSucceeded) return GetWidth();
    return size.cx <= 0 ? GetWidth() : size.cx;
} // Font::GetCharWidth


//////////////////////////////////////////////////////////////////////
//
// Font::GetTextWidth
//
int Font::GetTextWidth(HDC hdc, const char16* pwch, uint cwch)
{
    DcSelect oSelectFont(hdc, m_hFont);

    SIZE size;
    BOOL fSucceeded = ::GetTextExtentPoint32W(
        hdc, pwch, cwch, &size );
    if (! fSucceeded) return GetWidth();
    // Note: Below line may not work if font includes Kanji character and
    // ASCII character.
    return size.cx <= 0 ? GetWidth() : size.cx;
} // Font::GetTextWidth


//////////////////////////////////////////////////////////////////////
//
// Font::HasGlyph
//
#if 1
bool Font::HasGlyph(char16 wch, HDC hdc) const
{
    // Note: The first font in FontSet must satisfiy this invariant.
    if (wch >= 0x20 && wch <= 0x7E) return true;

    WORD  wIndex;
    DWORD cwch;

    if (NULL == hdc)
    {
        Dc dc(NULL, ::GetDC(NULL));
        DcSelect oSelectFont(hdc, m_hFont);
        cwch = ::GetGlyphIndicesW(
            dc, &wch, 1, &wIndex, GGI_MARK_NONEXISTING_GLYPHS );
    }
    else
    {
        DcSelect oSelectFont(hdc, m_hFont);
        cwch = ::GetGlyphIndicesW(
            hdc, &wch, 1, &wIndex, GGI_MARK_NONEXISTING_GLYPHS );
    }

    if (1 != cwch) return false;
    if (0xFFFF == wIndex) return false;
    // Note: GetGlyphIndices for "System Font" returns 31 (tmDefaultChar)
    // for non-supported font.
    if (wIndex == m_wchDefault) return false;
    return true;
} // Font::HasGlyph
#else
bool Font::HasGlyph(char16, HDC) const
    { return true; }
#endif

//////////////////////////////////////////////////////////////////////
//
// FontSet::FindFont
//
Font* FontSet::FindFont(char16 wch) const
{
    Dc dc(NULL, ::GetDC(NULL));
    return FindFont(dc, wch);
} // FontSet::FindFont


//////////////////////////////////////////////////////////////////////
//
// FontSet::FindFont
//
Font* FontSet::FindFont(HDC hdc, char16 wch) const
{
    foreach (EnumFont, oEnum, this)
    {
        Font* pFont = oEnum.Get();
        if (pFont->HasGlyph(wch, hdc)) return pFont;
    } // for each font

    return NULL;
} // FontSet::FindFont

int memhash(const void* pv, size_t cb)
{
    ASSERT(NULL != pv);
    const uint* s = reinterpret_cast<const uint*>(pv);
    const uint* e = s + cb / sizeof(uint);
    uint nHashCode = 0;
    for (const uint* p = s; p < e; p++)
    {
        uint nHigh = nHashCode >>= sizeof(uint) * 8 - 5;
        nHashCode |= *p;
        nHashCode <<= 5;
        nHashCode |= nHigh;
    } // for
    return nHashCode & ((1<<28)-1);
} // memhash

int Font::HashKey(const Key* pKey)
    { return memhash(pKey, sizeof(*pKey)); }

int FontSet::HashKey(const Key* pKey)
{
    const Fonts* p = reinterpret_cast<const Fonts*>(pKey);
    ASSERT(p->m_cFonts >= 1);
    return memhash(p->m_rgpFont, sizeof(Font*) * p->m_cFonts);
} // FontSet::HashKey

template<class Item_, class Key_, int t_N = 31>
class Cache_
{
    private: struct Slot
    {
        Item_*  m_pItem;

        bool HasItem() const
        {
            return NULL != m_pItem && Removed() != m_pItem;
        } // HasItem
    }; // Slot

    private: Slot*  m_prgSlot;
    private: int    m_cAlloc;
    private: int    m_cItems;

    public: Cache_() :
        m_cAlloc(t_N),
        m_cItems(0),
        m_prgSlot(new Slot[t_N])
    {
        ::ZeroMemory(m_prgSlot, sizeof(Slot) * m_cAlloc);
    } // Cache_
        
    private: static Item_* Removed()
        { return reinterpret_cast<Item_*>(1); };

    public: Item_* Get(const Key_* pKey) const
    {
        int iHashCode = Item_::HashKey(pKey);
        const Slot* pTop    = &m_prgSlot[0];
        const Slot* pBottom = &m_prgSlot[m_cAlloc];
        const Slot* pStart  = &m_prgSlot[iHashCode % m_cAlloc];
        const Slot* pRunner = pStart;
        do
        {
            Item_* pItem = pRunner->m_pItem;
            if (NULL == pItem)
            {
                return NULL;
            }

            if (Removed() == pItem)
            {
                // removed
            }
            else if (pItem->EqualKey(pKey))
            {
                return pItem;
            }

            pRunner++;
            if (pRunner == pBottom) pRunner = pTop;
        } while (pRunner != pStart);
        CAN_NOT_HAPPEN();
    } // Get

    public: void Put(Item_* pItem)
    {
        ASSERT(NULL != pItem);

        if (m_cItems * 60 > m_cAlloc * 100)
        {
            rehash();
        }

        int iHashCode = Item_::HashKey(pItem->GetKey());
        Slot* pTop    = &m_prgSlot[0];
        Slot* pBottom = &m_prgSlot[m_cAlloc];
        Slot* pStart  = &m_prgSlot[iHashCode % m_cAlloc];
        Slot* pRunner = pStart;
        Slot* pHome = NULL;
        const Key_* pKey = pItem->GetKey();
        do
        {
            Item_* pPresent = pRunner->m_pItem;
            if (NULL == pPresent)
            {
                if (NULL == pHome) pHome = pRunner;
                pHome->m_pItem = pItem;
                m_cItems += 1;
                return;
            }

            if (Removed() == pPresent)
            {
                if (NULL == pHome) pHome = pRunner;
            }
            else if (pPresent->EqualKey(pKey))
            {
                return;
            }

            pRunner++;
            if (pRunner == pBottom) pRunner = pTop;
        } while (pRunner != pStart);
        CAN_NOT_HAPPEN();
    } // Put

    private: void rehash()
    {
        Slot* prgStart = m_prgSlot;
        int cAllocs = m_cAlloc;
        int cItems  = m_cItems;
        
        m_cAlloc = m_cAlloc * 130 / 100;
        m_cItems  = 0;
        m_prgSlot = new Slot[m_cAlloc];
        ::ZeroMemory(m_prgSlot, sizeof(Slot) * m_cAlloc);

        Slot* prgEnd = prgStart + cAllocs;
        for (Slot* pRunner = prgStart; pRunner < prgEnd; pRunner++)
        {
            if (pRunner->HasItem())
            {
                Put(pRunner->m_pItem);
                cItems -= 1;
                if (0 == cItems) break;
            }
        } // for pRunner
    } // rehash
}; // Cache_

typedef Cache_<Font, Font::Key> FontCache;
typedef Cache_<FontSet, FontSet::Key> FontSetCache;
FontCache* g_pFontCache;
FontSetCache* g_pFontSetCache;


//////////////////////////////////////////////////////////////////////
//
// FontSet::Get
//
FontSet* FontSet::Get(HDC hdc, const StyleValues* pStyle)
{
    Fonts oFonts;
    oFonts.m_cFonts = 0;

    const char16* pwszFamily = pStyle->GetFontFamily();
    while (0 != *pwszFamily)
    {
        LOGFONT oLogFont;
        ::ZeroMemory(&oLogFont, sizeof(oLogFont));

        oLogFont.lfHeight = -::MulDiv(
                ::GetDeviceCaps(hdc, LOGPIXELSY),
                pStyle->m_nFontSize,
                72 );

        oLogFont.lfWidth = 0;
        oLogFont.lfEscapement = 0;
        oLogFont.lfOrientation = 0;

        oLogFont.lfWeight =
            FontWeight_Bold == pStyle->GetFontWeight() ? FW_BOLD : FW_NORMAL;

        oLogFont.lfItalic =
            FontStyle_Italic == pStyle->GetFontStyle() ? 1 : 0;

        oLogFont.lfUnderline =
            TextDecoration_Underline == pStyle->GetDecoration() ? 1 : 0;

        oLogFont.lfStrikeOut     = 0;
        oLogFont.lfCharSet       = ANSI_CHARSET;;
        oLogFont.lfOutPrecision  = OUT_DEFAULT_PRECIS;
        oLogFont.lfClipPrecision = CLIP_DEFAULT_PRECIS;
        oLogFont.lfQuality       = DEFAULT_QUALITY;

        // We use FIXED_PITCH. This makes width of Kanji character is double
        // of width of alphabet character.
        //oLogFont.lfPitchAndFamily = DEFAULT_PITCH;
        oLogFont.lfPitchAndFamily = FIXED_PITCH;

        while (IsWhitespace(*pwszFamily))
        {
            pwszFamily++;
        } // while

        char16* pwsz = oLogFont.lfFaceName;
        while (0 != *pwszFamily) {
            if (',' == *pwszFamily)
            {
                pwszFamily++;
                break;
            }

            *pwsz++ = *pwszFamily++;
        } // while

        if (NULL == g_pFontCache)
        {
            g_pFontCache = new FontCache;
        }

        Font* pFont = g_pFontCache->Get(&oLogFont);
        if (NULL == pFont)
        {
            pFont = Font::Create(&oLogFont);
            g_pFontCache->Put(pFont);
        }

        oFonts.m_rgpFont[oFonts.m_cFonts] = pFont;

        oFonts.m_cFonts += 1;
    } // for

    if (NULL == g_pFontSetCache)
    {
        g_pFontSetCache = new FontSetCache;
    }

    FontSet* pFontSet = g_pFontSetCache->Get(&oFonts);

    if (NULL == pFontSet)
    {
        pFontSet = new FontSet;
        for (int i = 0; i < oFonts.m_cFonts; i++)
        {
            pFontSet->Add(oFonts.m_rgpFont[i]);
        } // for i
        g_pFontSetCache->Put(pFontSet);
    }
    return pFontSet;
} // FontSet::Get
