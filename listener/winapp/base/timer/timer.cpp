#include "precomp.h"
// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)

#include "base/singleton.h"
#include "base/timer/timer.h"
#include "widgets/naitive_window.h"
#include <memory>

namespace base {
namespace impl {

struct TimerEntry : RefCounted<TimerEntry> {
  AbstractTimer* timer;
  uint repeat_interval_ms;
  TimerEntry(AbstractTimer* timer, uint repeat_interval_ms)
      : timer(timer), repeat_interval_ms(repeat_interval_ms) {
  }
};

class TimerController : public Singleton<TimerController> {
  friend class Singleton<TimerController>;

  private: class MessageWindow : public widgets::NaitiveWindow {
    public: MessageWindow() = default;
    public: virtual ~MessageWindow() = default;
    DISALLOW_COPY_AND_ASSIGN(MessageWindow);
  };

  private: std::unique_ptr<MessageWindow> message_window_;

  private: TimerController()
      : message_window_(CreateMessageWindow()) {
  }

  private: UINT_PTR ComputeCookie(AbstractTimer* timer) {
    auto const entry = timer->entry_.get();
    ASSERT(entry);
    auto const cookie = reinterpret_cast<UINT_PTR>(entry);
    return cookie;
  }

  private: static std::unique_ptr<MessageWindow> CreateMessageWindow() {
    std::unique_ptr<MessageWindow> window(new MessageWindow());
    window->CreateWindowEx(0, 0, nullptr, HWND_MESSAGE,
                           gfx::Point(), gfx::Size());
    return std::move(window);
  }

  private: void SetTimer(AbstractTimer* timer,
                         uint next_fire_interval_ms) {
    ::SetTimer(*message_window_,
               ComputeCookie(timer),
               next_fire_interval_ms,
               TimerController::TimerProc);
  }

  public: void StartTimer(AbstractTimer* timer,
                          uint next_fire_interval_ms,
                          uint repeat_interval_ms) {
    if (!timer->entry_) {
      timer->entry_.reset(new TimerEntry(timer, repeat_interval_ms));
      timer->entry_->timer = timer;
    } else {
      timer->entry_->repeat_interval_ms = repeat_interval_ms;
    }
    timer->entry_->AddRef();
    SetTimer(timer, next_fire_interval_ms);
  }

  public: void StopTimer(AbstractTimer* timer) {
    ::KillTimer(*message_window_, ComputeCookie(timer));
    timer->entry_.reset();
  }

  private: static void CALLBACK TimerProc(HWND, UINT, UINT_PTR cookie,
                                          DWORD) {
    auto const entry = reinterpret_cast<TimerEntry*>(cookie);
    if (!entry->timer)
      return;
    entry->timer->Fire();
    // Fire() may remove associated timer object in |entry|.
    auto const timer = entry->timer;
    if (!timer)
      return;
    if (entry->repeat_interval_ms) {
      if (!timer->entry_)
        timer->entry_ = entry;
      instance().SetTimer(timer, entry->repeat_interval_ms);
    } else {
      instance().StopTimer(timer);
    }
  }

  DISALLOW_COPY_AND_ASSIGN(TimerController);
};

AbstractTimer::AbstractTimer() {
}

AbstractTimer::~AbstractTimer() {
  if (!entry_)
    return;
  Stop();
  entry_->timer = nullptr;
}

void AbstractTimer::Start(uint next_fire_interval_ms,
                          uint repeat_interval_ms) {
  TimerController::instance().StartTimer(this, next_fire_interval_ms,
                                         repeat_interval_ms);
}

void AbstractTimer::Stop() {
  TimerController::instance().StopTimer(this);
}

} // namespace impl
} // namespace base
