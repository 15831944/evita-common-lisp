//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers - DoubleFloat
// big/big_12_float.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_12_float.h#6 $
//
#if !defined(INCLUDE_big_12_float_h)
#define INCLUDE_big_12_float_h

namespace MiniLisp
{

class DoubleFloatImpl;
class StackDoubleFloat;

enum FpClass
{
    FpClass_Zero,
    FpClass_Normal,
    FpClass_Subnormal,
    FpClass_Infinity,
    FpClass_NaN,
    FpClass_SNaN,
}; // FpClass

float64 convert_to_float64(Val);
float32 convert_to_float32(Val);

inline bool double_floatp(Val x) { return x->Is<DoubleFloat>(); }
inline bool single_floatp(Val x) { return x->Is<SingleFloat>(); }

//////////////////////////////////////////////////////////////////////
//
// Float_
template<
    class Self_, class Kernel_, class Layout_,
    typename float_, typename uint_, typename int_,
    typename Bignum_ >
class FloatImpl_ : public Kernel_
{
    public: static FpClass Classify(const Layout_*);

    public: static char16 GetMarker()
    {
        if (TLV(Aread_default_float_formatA) == Self_::GetSymbol())
        {
            return 'e';
        }
        else
        {
            return Self_::GetMarker_();
        }
    } // GetMarker

    ////////////////////////////////////////////////////////////
    //
    // Arithmetic
    //
    public: static Val Add(Val x, Val y)
    {
        return Self_::Make(
            x->Decode<Self_>()->Get() +
            y->Decode<Self_>()->Get() );
    } // Add

    public: static Val Div(Val x, Val y)
    {
        return Self_::Make(
            x->Decode<Self_>()->Get() /
            y->Decode<Self_>()->Get() );
    } // Div

    public: static Val Mul(Val x, Val y)
    {
        return Self_::Make(
            x->Decode<Self_>()->Get() *
            y->Decode<Self_>()->Get() );
    } // Mul

    public: static Val Sub(Val x, Val y)
    {
        return Self_::Make(
            x->Decode<Self_>()->Get() -
            y->Decode<Self_>()->Get() );
    } // Sub

    public: static Val Rational(const Layout_*);
    public: static Val ToInt(const Layout_*);

    public: static Val Rational(Val x)
        { return Rational(x->Decode<Self_>()->GetLayout()); }

    public: static void Ceiling(Val, Val, Val*, Val*);
    public: static void Floor(Val, Val, Val*, Val*);
    public: static void Round(Val, Val, Val*, Val*);

    public: static void Truncate(Val x, Val y, Val* out_q, Val* out_r)
    {
        float_ fltX = x->Decode<Self_>()->Get();
        float_ fltY = y->Decode<Self_>()->Get();

        float_ fltQ = Self_::TruncateImpl(fltX / fltY);

        if (NULL != out_q) *out_q = Self_::Make(fltQ);
        if (NULL != out_r) *out_r = Self_::Make(fltX - fltQ * fltY);
    } // Truncate
}; // FloatImpl_


//////////////////////////////////////////////////////////////////////
//
// DoubleFloatImpl
//
class DoubleFloatImpl :
    public FloatImpl_<
        DoubleFloatImpl, Kernel::DoubleFloat,
        Kernel::DoubleFloat::Layout,
        double, uint64, int64,
        BignumInt64 >
{
    public: double Get() const
        { return m_dbl; }

    public: static float64 Get(Val x)
    {
        check_type(x, double_float);
        return x->Decode<DoubleFloat>()->m_dbl;
    } // Get

    public: const Layout* GetLayout() const
        { return reinterpret_cast<const Layout*>(&m_dbl); }

    public: static uint64 GetSignificand(const Layout* p)
    {
        uint64 ull = p->m_nSignificandH;
        ull <<= 32;
        ull |= p->m_nSignificandL;
        return ull;
    } // GetSignificand

    public: static char16 GetMarker_()   { return 'd'; }
    public: static Val GetSymbol()       { return Qdouble_float; }
    public: static const char16* GetTypeName() { return L"Double-Float"; }

    public: static Val Make(Val);

    public: static Val Make(float64 dbl)
    {
        Val x = MiniThread::Get()->AllocBinObj(CLASSD_double_float);
            x->Decode<DoubleFloat>()->m_dbl = dbl;
        return x;
    } // Make

    public: static void SetSignificand(Layout*, Val);

    public: static double TruncateImpl(float64);
}; // DoubleFloatImpl


//////////////////////////////////////////////////////////////////////
//
// StackDoubleFloat
//
class __declspec(align(Kernel_Arch_BinObj_Align)) StackDoubleFloat :
    public Kernel::DoubleFloat
{
    public: StackDoubleFloat(double dbl)
    {
        m_dbl = dbl;
    } // StackDoubleFloat

    public: StackDoubleFloat(Val x)
    {
        ASSERT(! double_float_p(x));
        m_dbl = convert_to_float64(x);
    } // StackDoubleFloat
}; // StackDoubleFloat


#if ! USE_SINGLE_FLOAT_TAG

//////////////////////////////////////////////////////////////////////
//
// SingleFloatImpl (! USE_SINGLE_FLOAT_TAG)
//
class SingleFloatImpl :
    public FloatImpl_<
        SingleFloatImpl, Kernel::SingleFloat,
        Kernel::SingleFloat::Layout,
        float, uint32, int32,
        BignumInt >
{
    public: float32 Get() const
        { return m_flt; }

    public: const Layout* GetLayout() const
        { return reinterpret_cast<const Layout*>(&m_flt); }

    public: static float32 Get(Val x)
    {
        check_type(x, single_float);
        return x->Decode<SingleFloat>()->m_flt;
    } // Get

    public: static uint32 GetSignificand(const Layout* p)
        { return p->m_nSignificand; }

    public: static char16 GetMarker_()   { return 'f'; }
    public: static Val GetSymbol()       { return Qsingle_float; }
    public: static const char16* GetTypeName() { return L"Single-Float"; }

    public: static Val Make(Val);

    public: static Val Make(float32 flt)
    {
        Val x = MiniThread::Get()->AllocBinObj(CLASSD_single_float);
            x->Decode<SingleFloat>()->m_flt = flt;
        return x;
    } // Make

    public: static void SetSignificand(Layout*, Val);

    public: static float TruncateImpl(float32);

}; // SingleFloatImpl

#else

//////////////////////////////////////////////////////////////////////
//
// SingleFloatImpl  (! USE_SINGLE_FLOAT_TAG)
//
class SingleFloatImpl :
    public FloatImpl_<
        SingleFloatImpl, Kernel::SingleFloat,
        Kernel::SingleFloat::Layout,
        float, uint32, int32 >
{
    union f32i64
    {
        uint64  n64;
        float32 f32;
    };

    public: float Get() const
    {
        f32i64 u;
            u.n64 = ToInt() >> ShiftCount;
        return u.f32;
    } // Get

    public: const Layout* GetLayout() const
        { return reinterpret_cast<const Layout*>(&m_flt); }

    public: static uint32 GetSignificand(const Layout* p)
        { return p->m_nSignificand; }

    public: static char16 GetMarker_()   { return 'f'; }
    public: static Val GetSymbol()       { return Qsingle_float; }
    public: static const char16* GetTypeName() { return L"Single-Float"; }

    public: static Val Make(Val);

    public: static Val Make(float flt)
    {
        f32i64 u;
            u.n64 = 0;
            u.f32 = flt;
            u.n64 <<= ShiftCount;
            u.n64 |= SingleFloat::Tag;
        return FromInt<Val_>(u.n64);
    } // Make

    public: static void SetSignificand(Layout*, Val);

    public: static float TruncateImpl(float);
}; // SingleFloatImpl

#endif // ! USE_SINGLE_FLOAT_TAG


//////////////////////////////////////////////////////////////////////
//
// StackSingleFloat
//
class __declspec(align(Kernel_Arch_BinObj_Align)) StackSingleFloat :
    public Kernel::SingleFloat
{
    public: StackSingleFloat(float32 flt)
    {
        m_flt = flt;
    } // StackSingleFloat

    public: StackSingleFloat(Val x)
    {
        ASSERT(! single_float_p(x));
        m_flt = convert_to_float32(x);
    } // StackSingleFloat
}; // StackSingleFloat

} // MiniLisp

#endif //!defined(INCLUDE_big_12_float_h)
