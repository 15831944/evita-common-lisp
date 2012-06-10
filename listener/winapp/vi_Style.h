//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - edit buffer
// listener/winapp/ed_buffer.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_Style.h#2 $
//
#if !defined(INCLUDE_listener_winapp_visual_fontset_h)
#define INCLUDE_listener_winapp_visual_fontset_h

#include "./vi_defs.h"

#include "./ed_Style.h"

//////////////////////////////////////////////////////////////////////
//
// Font class
//
class Font
{
    public: enum Flag
    {
        Flag_Bold       = 1 << 0,
        Flag_Italic     = 1 << 1,
        Flag_StrikeOut  = 1 << 2,
        Flag_Underline  = 1 << 3,
    }; // Flag

    public: typedef LOGFONT Key;

    private: bool       m_fFixedPitch;
    private: HFONT      m_hFont;
    private: int        m_iDescent;
    private: int        m_iHeight;
    private: int        m_iWidth;
    private: LOGFONT    m_oLogFont;
    private: char16     m_wchDefault;

    Font(HGDIOBJ, const TEXTMETRIC*, const LOGFONT*);

    public: ~Font()
    {
        if (NULL != m_hFont) ::DeleteObject(m_hFont);
    } // ~Font

    public: operator HFONT() const { return m_hFont; }

    // [C]
    public: static Font* Create(const char16*, int, int, uint = 0);
    public: static Font* Create(const LOGFONT*);

    // [E]
    public: bool EqualKey(const Key* pKey) const
    {
        return 0 == ::memcmp(&m_oLogFont, pKey, sizeof(m_oLogFont));
    } // EqualKey

    // [G]
    public: int GetAscent()  const { return m_iHeight - m_iDescent; }
    public: int GetCharWidth(HDC, char16);
    public: int GetDescent() const { return m_iDescent; }
    public: int GetHeight()  const { return m_iHeight; }
    public: const Key* GetKey() const { return &m_oLogFont; }
    public: const LOGFONT* GetLogFont() const { return &m_oLogFont; }
    public: int GetTextWidth(HDC, const char16*, uint);
    public: int GetWidth()  const  { return m_iWidth; }

    // [H]
    public: bool HasGlyph(char16, HDC = NULL) const;

    // [H]
    public: uint Hash() const
        { return static_cast<uint>(reinterpret_cast<UINT_PTR>(this)); }

    public: static int HashKey(const Key*);
}; // Font


//////////////////////////////////////////////////////////////////////
//
// FontSet class
//
struct Fonts
{
    Font*   m_rgpFont[10];
    int     m_cFonts;
}; // Fonts

class FontSet : public Fonts
{
    public: typedef Fonts Key;

    public: FontSet()
    {
        m_cFonts = 0;
    } // FontSet

    // [A]
    public: void Add(Font*);

    // [E]
    public: bool EqualKey(const Key* pFonts) const
    {
        if (pFonts->m_cFonts != m_cFonts) return false;
        ASSERT(m_cFonts < lengthof(m_rgpFont));
        return 0 == ::memcmp(
            pFonts->m_rgpFont,
            m_rgpFont,
            sizeof(m_rgpFont[0]) * m_cFonts );
    } // EqualKey

    // [F]
    public: Font* FindFont(char16) const;
    public: Font* FindFont(HDC, char16) const;

    // [G]
    public: static FontSet* Get(HDC, const StyleValues*);
    public: const Key* GetKey() const { return this; }

    // [H]
    public: static int HashKey(const Key*);

    // [E]
    public: class EnumFont
    {
        Font**  m_pRunner;
        Font**  m_pEnd;

        public: EnumFont(const FontSet* p) :
            m_pRunner(const_cast<Font**>(&p->m_rgpFont[0])),
            m_pEnd(const_cast<Font**>(&p->m_rgpFont[p->m_cFonts])) {}

        public: bool AtEnd() const { return m_pRunner >= m_pEnd; }
        public: Font* Get() const { ASSERT(! AtEnd()); return *m_pRunner; }
        public: void Next() { ASSERT(! AtEnd()); m_pRunner++; }
    }; // EnumFont
}; // FontSet

#endif //!defined(INCLUDE_listener_winapp_visual_fontset_h)
