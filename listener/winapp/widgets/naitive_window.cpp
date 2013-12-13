#include "precomp.h"
// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#include "widgets/naitive_window.h"

extern HINSTANCE g_hInstance;
extern HINSTANCE g_hResource;

namespace widgets {

namespace {
ATOM s_window_class;
NaitiveWindow* s_creating_window;
}

NaitiveWindow::NaitiveWindow(const Widget& widget)
    : hwnd_(nullptr),
      widget_(const_cast<Widget*>(&widget)) {
}

NaitiveWindow::NaitiveWindow()
    : hwnd_(nullptr), widget_(nullptr) {
}

NaitiveWindow::~NaitiveWindow() {
  #if DEBUG_DESTROY
    DEBUG_PRINTF("%p\n", this);
  #endif
  ASSERT(!hwnd_);
}

bool NaitiveWindow::CreateWindowEx(DWORD dwExStyle, DWORD dwStyle,
                                  HWND parent_hwnd, 
                                  const gfx::Point& left_top,
                                  const gfx::Size& size) {
  ASSERT(!s_creating_window);
  s_creating_window = this;

  if (!s_window_class) {
    WNDCLASSEXW wc;
    wc.cbSize = sizeof(wc);
    wc.style = CS_DBLCLKS | CS_BYTEALIGNCLIENT;
    wc.lpfnWndProc = WindowProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance  = g_hInstance;
    wc.hIcon = ::LoadIconW(g_hResource, MAKEINTRESOURCE(IDI_APPLICATION));
    wc.hCursor = nullptr;
    wc.hbrBackground = (HBRUSH) (COLOR_WINDOW + 1);
    wc.lpszMenuName = nullptr;
    wc.lpszClassName = L"EvitaNaitiveWindow";
    wc.hIconSm  = ::LoadIconW(g_hResource, MAKEINTRESOURCE(IDI_APPLICATION));
    s_window_class = ::RegisterClassExW(&wc);
    ASSERT(s_window_class);
  }

  return ::CreateWindowEx(dwExStyle, MAKEINTATOM(s_window_class), nullptr,
                          dwStyle, left_top.x, left_top.y, size.cx, size.cy,
                          parent_hwnd, nullptr, g_hInstance, 0);
}

void NaitiveWindow::Destroy() {
  ASSERT(IsRealized());
  ::DestroyWindow(hwnd_);
}

NaitiveWindow* NaitiveWindow::MapHwnToNaitiveWindow(HWND const hwnd) {
  ASSERT(hwnd);
  return reinterpret_cast<NaitiveWindow*>(
    static_cast<LONG_PTR>(::GetWindowLongPtrW(hwnd, GWLP_USERDATA)));
}

LRESULT NaitiveWindow::DefWindowProc(UINT message, WPARAM wParam,
                                    LPARAM lParam) {
  return ::DefWindowProc(hwnd_, message, wParam, lParam);
}

LRESULT CALLBACK NaitiveWindow::WindowProc(HWND hwnd, UINT message,
                                           WPARAM wParam, LPARAM  lParam) {

  if (auto const window = s_creating_window) {
    s_creating_window = nullptr;
    window->hwnd_ = hwnd;
    ::SetWindowLongPtrW(hwnd, GWLP_USERDATA,
                        static_cast<LONG>(reinterpret_cast<LONG_PTR>(window)));
    return window->WindowProc(message, wParam, lParam);
  }

  auto const window = MapHwnToNaitiveWindow(hwnd);
  ASSERT(window);
  if (message == WM_NCDESTROY) {
    window->hwnd_ = nullptr;
    window->WindowProc(message, wParam, lParam);
    delete window;
    return 0;
  }

  return window->WindowProc(message, wParam, lParam);
}

LRESULT NaitiveWindow::WindowProc(UINT message, WPARAM wParam,
                                  LPARAM lParam) {
  if (widget_)
    return widget_->WindowProc(message, wParam, lParam);
  return DefWindowProc(message, wParam, lParam);
}

} // namespace widgets
