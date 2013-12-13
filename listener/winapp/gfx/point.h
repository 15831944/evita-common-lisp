// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#if !defined(INCLUDE_gfx_point_h)
#define INCLUDE_gfx_point_h

namespace gfx {

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

} // namespace gfx

#endif //!defined(INCLUDE_gfx_point_h)
