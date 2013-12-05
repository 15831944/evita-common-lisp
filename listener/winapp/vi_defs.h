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

struct Point : POINT {
  Point() {
    x = y = 0;
  }

  Point(int x, int y) {
    this->x = x;
    this->y = y;
  }

 Point(POINTS pt) {
   x = pt.x; y = pt.y;
 }
};

struct Rect : RECT {
  Rect() {
    left = right = top = bottom =0;
  }

  Rect(int l, int t, int r, int b) {
    left= l; right= r; top= t; bottom= b;
  }

  Rect(RECT rc) {
    left = rc.left; right = rc.right; top = rc.top; bottom = rc.bottom;
  }

  operator bool() const { return !width() && !height(); }
  int height() const { return bottom - top; }
  int width() const { return right - left; }
};

extern HINSTANCE g_hInstance;
extern HINSTANCE g_hResource;

#endif //!defined(INCLUDE_listener_winapp_visual_defs_h)
