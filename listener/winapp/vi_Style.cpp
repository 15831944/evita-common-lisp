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

#include "./gfx_base.h"
#include "./vi_util.h"

#include <vector>

namespace {
std::vector<uint16> GetGlyphIndexs(const gfx::FontFace& font_face, 
                                   const char16* pwch, int cwch) {
  ASSERT(cwch);
  std::vector<uint32> code_points(cwch);
  auto it = code_points.begin();
  for (auto s = pwch; s < pwch + cwch; ++s) {
    *it = *s;
    ++it;
  }
  std::vector<uint16> glyph_indexs(cwch);
  COM_VERIFY(font_face->GetGlyphIndices(
      &code_points[0],
      code_points.size(),
      &glyph_indexs[0]));
    return glyph_indexs;
}

bool IsCacheableChar(char16 wch) {
  return wch >= 0x20 && wch <= 0x7E;
}

bool IsCachableString(const char16* pwch, int cwch) {
  for (auto s = pwch; s < pwch + cwch; ++s) {
    if (!IsCacheableChar(*s))
      return false;
  }
  return true;
}

}

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
// Font
//
static uint CalculateFixedWidth(const gfx::FontFace& font_face) {
  static uint32 code_points[0x7E - 0x20 + 1];
  if (!code_points[0]) {
    for (int ch = ' '; ch <= 0x7E; ++ch) {
      code_points[ch - 0x20] = ch;
    }
  }
  std::vector<uint16> glyph_indexs(lengthof(code_points));
  COM_VERIFY(font_face->GetGlyphIndices(
      &code_points[0],
      lengthof(code_points),
      &glyph_indexs[0]));

  std::vector<DWRITE_GLYPH_METRICS> metrics(glyph_indexs.size());
  COM_VERIFY(font_face->GetDesignGlyphMetrics(
      &glyph_indexs[0], glyph_indexs.size(), &metrics[0], false));

  auto const width = metrics[0].advanceWidth;
  for (const auto metric: metrics) {
    if (width != metric.advanceWidth)
      return 0;
  }
  return width;
}

Font::Font(const LOGFONT& log_font)
    : m_oLogFont(log_font),
      font_face_(*new gfx::FontFace(log_font.lfFaceName)),
      ascent_(Scale(font_face_->metrics().ascent)),
      descent_(Scale(font_face_->metrics().descent)),
      height_(Scale(font_face_->metrics().lineGap) + ascent_ + descent_),
      fixed_width_(Scale(CalculateFixedWidth(*font_face_))) {
}

Font::~Font() {
}

// Font::Create
base::OwnPtr<Font> Font::Create(const LOGFONT* logFont) {
  return std::move(base::OwnPtr<Font>(new Font(*logFont)));
}

float Font::GetCharWidth(char16 wch) const {
  if (IsCacheableChar(wch) && fixed_width_)
    return fixed_width_;
  return GetTextWidth(&wch, 1);
}

float Font::GetTextWidth(const char16* pwch, uint cwch) const {
  if (fixed_width_ && IsCachableString(pwch, cwch))
    return fixed_width_ * cwch;
  auto const glyph_indexs = GetGlyphIndexs(*font_face_, pwch, cwch);
  if (!glyph_indexs.size())
    return 0.0f;
  std::vector<DWRITE_GLYPH_METRICS> metrics(glyph_indexs.size());
  COM_VERIFY((*font_face_)->GetDesignGlyphMetrics(
      &glyph_indexs[0], glyph_indexs.size(), &metrics[0], false));
  auto width = 0;
  for (const auto metric: metrics) {
    width += metric.advanceWidth;
  }
  return Scale(width);
}

bool Font::HasCharacter(char16 wch) const {
  // Note: The first font in FontSet must satisfiy this invariant.
  // TODO(yosi): We don't belive this assumption.l
  if (wch >= 0x20 && wch <= 0x7E)
    return true;

  uint32 code_point = wch;
  uint16 glyph_index;
  COM_VERIFY((*font_face_)->GetGlyphIndices(
      &code_point,
      1,
      &glyph_index));
  return glyph_index;
}

// Convert pt(1/72in) to dip(1/96in)
float Font::Scale(int design_unit) const {
  auto const pt = static_cast<float>(m_oLogFont.lfHeight * design_unit) /
      font_face_->metrics().designUnitsPerEm;
  return pt * 96.0f / 72.0f;
}

//////////////////////////////////////////////////////////////////////
//
// FontSet
//
Font* FontSet::FindFont(char16 wch) const {
  foreach (EnumFont, oEnum, this) {
    auto const pFont = oEnum.Get();
    if (pFont->HasCharacter(wch))
      return pFont;
  }
  return nullptr;
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
FontSet* FontSet::Get(const StyleValues* pStyle)
{
    Fonts oFonts;
    oFonts.m_cFonts = 0;

    const char16* pwszFamily = pStyle->GetFontFamily();
    while (0 != *pwszFamily)
    {
        LOGFONT oLogFont;
        ::ZeroMemory(&oLogFont, sizeof(oLogFont));

        oLogFont.lfHeight = pStyle->m_nFontSize;
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
        if (!pFont) {
            pFont = &Font::Create(&oLogFont).Detach();
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
