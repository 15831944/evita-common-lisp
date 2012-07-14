//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - edit buffer
// listener/winapp/ed_buffer.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_CommandWindow.h#1 $
//
#if !defined(INCLUDE_visual_CommandWindow_h)
#define INCLUDE_visual_CommandWindow_h

#include "./vi_BaseWindow.h"

namespace Command
{
    class KeyBindEntry;
} // Command


class Frame;
class Pane;

//////////////////////////////////////////////////////////////////////
//
// CommandWindow
//
class CommandWindow : public BaseWindow
{
    // [D]
    public: template<class T> T* DynamicCast()
        { return Is<T>() ? static_cast<T*>(this) : NULL; }

    // [G]
    public: virtual const char16* GetClass() const = 0;

    // [I]
    public: virtual bool IsPane() const { return false; }

    public: template<class T> bool Is() const
        { return T::Is_(this); }

    // [M]
    public: virtual Command::KeyBindEntry* MapKey(uint) = 0;
}; // CommandWindow


template<class T, class Parent_ = CommandWindow>
class CommandWindow_ : public Parent_
{
    public: static bool Is_(const CommandWindow* p)
        { return T::GetClass_() == p->GetClass(); }

    public: virtual const char16* GetClass() const override
        { return T::GetClass_(); }
}; // CommandWindow_

#endif //!defined(INCLUDE_visual_CommandWindow_h)
