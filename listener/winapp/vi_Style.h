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
#include "./li_util.h"

namespace gfx {
class FontFace;
class Graphics;
class TextFormat;
class TextLayout;
};

//////////////////////////////////////////////////////////////////////
//
// Font class
//
class Font {
    public: typedef LOGFONT Key;

    private: LOGFONT m_oLogFont;
    private: const base::OwnPtr<gfx::FontFace> font_face_;
    private: float const ascent_;
    private: float const descent_;
    private: float const height_;
    private: float const fixed_width_;

    private: Font(const LOGFONT&);
    public: ~Font();

    public: float ascent() const { return ascent_; }
    public: const gfx::FontFace& font_face() const { return *font_face_; }
    public: float height() const { return height_; }

    public: float font_size() const {
      return m_oLogFont.lfHeight * 96.0f / 72.0f;
    }

    // [C]
    private: float ConvertDesignUnitToDip(int design_unit) const;
    public: static base::OwnPtr<Font> Create(const LOGFONT*);

    // [E]
    public: bool EqualKey(const Key* pKey) const {
      return !::memcmp(&m_oLogFont, pKey, sizeof(m_oLogFont));
    }

    // [G]
    public: float GetCharWidth(char16) const;
    public: float GetDescent() const { return descent_; }
    private: float GetHeight()  const { return height_; }
    public: const Key* GetKey() const { return &m_oLogFont; }
    public: float GetTextWidth(const char16* pwch, uint cwch) const;

    // [H]
    public: bool HasCharacter(char16) const;

    public: uint Hash() const {
      return static_cast<uint>(reinterpret_cast<UINT_PTR>(this));
    }

    public: static int HashKey(const Key*);

    DISALLOW_COPY_AND_ASSIGN(Font);
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
    public: Font* FindFont(const gfx::Graphics&, char16 wch) const {
      return FindFont(wch);
    }

    public: Font* FindFont(char16) const;

    // [G]
    public: static FontSet* Get(const gfx::Graphics&, const StyleValues* p) {
      return Get(p);
    }
    public: static FontSet* Get(const StyleValues*);
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
