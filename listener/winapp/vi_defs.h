//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - edit buffer
// listener/winapp/vi_defs.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_defs.h#1 $
//
#if !defined(INCLUDE_listener_winapp_visual_defs_h)
#define INCLUDE_listener_winapp_visual_defs_h

#include "./resource.h"

struct Point : POINT
{
    Point(int xx = 0, int yy = 0)
        { x = xx; y  = yy; }

    Point(POINTS pt)
        { x = pt.x; y = pt.y; }
}; // Point

struct Rect : RECT
{
    Rect(int l = 0, int t = 0, int r = 0, int b = 0)
        { left= l; right= r; top= t; bottom= b; }

    Rect(RECT rc)
        { left = rc.left; right = rc.right; top = rc.top; bottom = rc.bottom; }
}; // Rect

extern HINSTANCE g_hInstance;
extern HINSTANCE g_hResource;


#endif //!defined(INCLUDE_listener_winapp_visual_defs_h)
