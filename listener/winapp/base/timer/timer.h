// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#if !defined(INCLUDE_base_timer_timer_h)
#define INCLUDE_base_timer_timer_h

#include "base/ref_counted.h"

namespace base {
namespace impl {

struct TimerEntry;

class AbstractTimer {
  friend class TimerController;
  private: base::scoped_refptr<TimerEntry> entry_;
  protected: AbstractTimer();
  public: virtual ~AbstractTimer();
  public: void Start(uint next_fire_interval_ms, uint repeat_interval_ms);
  public: void Stop();
  protected: virtual void Fire() = 0;
  DISALLOW_COPY_AND_ASSIGN(AbstractTimer);
};

template<class Receiver, class TimerClass>
class Timer : public AbstractTimer {
  public: typedef void (Receiver::*FiredFunction)(TimerClass*);
  private: Receiver* const receiver_;
  private: FiredFunction const function_;
  protected: Timer(Receiver* receiver, FiredFunction function)
      : receiver_(receiver),
        function_(function) {
  }
  private: virtual void Fire() override {
    (receiver_->*function_)(static_cast<TimerClass*>(this));
  }
  DISALLOW_COPY_AND_ASSIGN(Timer);
};

} // namespace impl

template<class Receiver>
class OneShortTimer : public impl::Timer<Receiver, OneShortTimer<Receiver>> {
  public: OneShortTimer(Receiver* receiver, FiredFunction function)
      : Timer(receiver, function) {
  }
  public: void Start(uint interval_ms) { Timer::Start(interval_ms, 0); }
  DISALLOW_COPY_AND_ASSIGN(OneShortTimer);
};

template<class Receiver>
class RepeatingTimer : public impl::Timer<Receiver, RepeatingTimer<Receiver>> {
  public: RepeatingTimer(Receiver* receiver, FiredFunction function)
      : Timer(receiver, function) {
  }
  public: void Start(uint interval_ms) {
    Timer::Start(interval_ms, interval_ms);
  }
  DISALLOW_COPY_AND_ASSIGN(RepeatingTimer);
};

} // namespace base

#endif //!defined(INCLUDE_base_timer_timer_h)
