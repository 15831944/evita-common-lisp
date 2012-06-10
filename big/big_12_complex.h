//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers - DoubleFloat
// big/big_12_float.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_12_complex.h#6 $
//
#if !defined(INCLUDE_big_12_complex_h)
#define INCLUDE_big_12_complex_h

namespace MiniLisp
{

class DoubleFloatComplexImpl;
class StackDoubleFloatComplex;

class SingleFloatComplexImpl;
class StackSingleFloatComplex;


template<class Self_, class Kernel_, class Component_>
class Complex_ : public Kernel_
{
    private: static Component_ add(Component_ x, Component_ y)
        { return Self_::Add_(x, y); }

    private: static Component_ div(Component_ x, Component_ y)
        { return Self_::Div_(x, y); }

    private: static Component_ mul(Component_ x, Component_ y)
        { return Self_::Mul_(x, y); }

    private: static Component_ sub(Component_ x, Component_ y)
        { return Self_::Sub_(x, y); }

    // (rx + ry) + (ix + iy)i
    public: static Val Add(Val x, Val y)
    {
        Component_ rx = x->Decode<Self_>()->GetReal();
        Component_ ix = x->Decode<Self_>()->GetImag();

        Component_ ry = y->Decode<Self_>()->GetReal();
        Component_ iy = y->Decode<Self_>()->GetImag();

        return Self_::Make(add(rx, ry), add(ix, iy));
    } // Add

    // (rx x ry + ix x iy) + (ry x rx - rx x ry)i
    // -------------------------------------------
    //          ry x ry + iy x iy
    public: static Val Div(Val x, Val y)
    {
        Component_ rx = x->Decode<Self_>()->GetReal();
        Component_ ix = x->Decode<Self_>()->GetImag();

        Component_ ry = y->Decode<Self_>()->GetReal();
        Component_ iy = y->Decode<Self_>()->GetImag();

        Component_ den = add(mul(ry, ry), mul(iy, iy));

        return Self_::Make(
            div(add(mul(rx, ry), mul(ix, iy)), den),
            div(add(mul(ry, ry), mul(iy, iy)), den) );
    } // Div

    // rx x ry - ix x iy + (rx x iy + ry x ix)i
    public: static Val Mul(Val x, Val y)
    {
        Component_ rx = x->Decode<Self_>()->GetReal();
        Component_ ix = x->Decode<Self_>()->GetImag();

        Component_ ry = y->Decode<Self_>()->GetReal();
        Component_ iy = y->Decode<Self_>()->GetImag();

        return Self_::Make(
            add(mul(rx, ry), mul(ix, iy)),
            add(mul(rx, iy), mul(ry, ix)) );

    } // Mul

    // (rx - ry) + (ix - iy)i
    public: static Val Sub(Val x, Val y)
    {
        Component_ rx = x->Decode<Self_>()->GetReal();
        Component_ ix = x->Decode<Self_>()->GetImag();

        Component_ ry = y->Decode<Self_>()->GetReal();
        Component_ iy = y->Decode<Self_>()->GetImag();

        return Self_::Make(sub(rx, ry), sub(ix, iy));
    } // Sub

    public: static bool Eq(Val x, Val y)
    {
        ASSERT(x->Is<Kernel_>());

        switch (get_type_1(y, CLASSD_number_max, Qnumber))
        {
        case CLASSD_(fixnum):                //[1]
        case CLASSD_(bignum):                //[2]
        case CLASSD_(ratio):                 //[3]
            if (0 != x->Decode<Self_>()->GetImag())
            {
                return false;
            }
            return 0 == cmp(rational(x->Decode<Self_>()->GetReal()),
                            y );

        case CLASSD_(double_float):          //[4]
            if (0 != x->Decode<Self_>()->GetImag())
            {
                return false;
            }
            return x->Decode<Self_>()->GetReal() ==
                   y->Decode<DoubleFloat>()->m_dbl;

        case CLASSD_(single_float):          //[5]
            if (0 != x->Decode<Self_>()->GetImag())
            {
                return false;
            }
            return x->Decode<Self_>()->GetReal() ==
                   y->Decode<SingleFloat>()->m_flt;

        case CLASSD_(double_float_complex):  //[6]
            return
                x->Decode<Self_>()->GetReal() ==
                         y->Decode<DoubleFloatComplex>()->m_dblReal &&
                x->Decode<Self_>()->GetImag() ==
                         y->Decode<DoubleFloatComplex>()->m_dblImag;

        case CLASSD_(rational_complex):      //[7]
            return
                0 == cmp(rational(x->Decode<Self_>()->GetReal()),
                         y->Decode<RationalComplex>()->m_real ) &&
                0 == cmp(rational(x->Decode<Self_>()->GetImag()),
                         y->Decode<RationalComplex>()->m_imag );

        case CLASSD_(single_float_complex):  //[8]
            return
                x->Decode<Self_>()->GetReal() ==
                y->Decode<SingleFloatComplex>()->m_fltReal  &&
                x->Decode<Self_>()->GetImag() ==
                y->Decode<SingleFloatComplex>()->m_fltImag;

        default:
            CAN_NOT_HAPPEN();
        } // switch type
    } // Complex_::Eq
}; // Complex_


class DoubleFloatComplexImpl :
    public Complex_<
        DoubleFloatComplexImpl, Kernel::DoubleFloatComplex, float64 >
{
    public: float64 GetReal() const { return m_dblReal; }
    public: float64 GetImag() const { return m_dblImag; }

    public: static float64 Add_(float64 x, float64 y) { return x + y; }
    public: static float64 Div_(float64 x, float64 y) { return x / y; }
    public: static float64 Mul_(float64 x, float64 y) { return x * y; }
    public: static float64 Sub_(float64 x, float64 y) { return x - y; }

    public: static Val Make(Val x, Val y)
        { return Make(convert_to_float64(x), convert_to_float64(y)); }

    public: static Val Make(float64 dblReal, float64 dblImag)
    {
        Val x = MiniThread::Get()->AllocBinObj(CLASSD_double_float_complex);
            x->Decode<DoubleFloatComplex>()->m_dblReal = dblReal;
            x->Decode<DoubleFloatComplex>()->m_dblImag = dblImag;
        return x;
    } // Make
}; // DoubleFloatComplexImpl


class RationalComplexImpl :
    public Complex_<RationalComplexImpl, Kernel::RationalComplex, Val>
{
    public: Val GetReal() const { return m_real; }
    public: Val GetImag() const { return m_imag; }


    public: static Val Add_(Val x, Val y) { return MiniLisp::add(x, y); }
    public: static Val Div_(Val x, Val y) { return MiniLisp::div(x, y); }
    public: static Val Mul_(Val x, Val y) { return MiniLisp::mul(x, y); }
    public: static Val Sub_(Val x, Val y) { return MiniLisp::sub(x, y); }

    public: static bool Eq(Val, Val);

    public: static Val Make(Val real, Val imag)
    {
        if (Fixnum::Encode(0) == imag)
        {
            return real;
        }

        Val x = MiniThread::Get()->AllocRecord(CLASSD_rational_complex);
            x->Decode<RationalComplex>()->m_real = real;
            x->Decode<RationalComplex>()->m_imag = imag;
        return x;
    } // Make
}; // RationalComplex


//////////////////////////////////////////////////////////////////////
//
// SingleFloatComplexImpl
//
class SingleFloatComplexImpl :
    public Complex_<
        SingleFloatComplexImpl, Kernel::SingleFloatComplex, float32 >
{
    public: float32 GetReal() const { return m_fltReal; }
    public: float32 GetImag() const { return m_fltImag; }

    public: static float32 Add_(float32 x, float32 y) { return x + y; }
    public: static float32 Div_(float32 x, float32 y) { return x / y; }
    public: static float32 Mul_(float32 x, float32 y) { return x * y; }
    public: static float32 Sub_(float32 x, float32 y) { return x - y; }

    public: static Val Make(Val x, Val y)
        { return Make(convert_to_float32(x), convert_to_float32(y)); }

    public: static Val Make(float32 fltReal, float32 fltImag)
    {
        Val x = MiniThread::Get()->AllocBinObj(CLASSD_single_float_complex);
            x->Decode<SingleFloatComplex>()->m_fltReal = fltReal;
            x->Decode<SingleFloatComplex>()->m_fltImag = fltImag;
        return x;
    } // Make
}; // SingleFloatComplexImpl


//////////////////////////////////////////////////////////////////////
//
// StackDoubleFloatComplex
//
class __declspec(align(Kernel_Arch_BinObj_Align)) StackDoubleFloatComplex :
    public Kernel::DoubleFloatComplex
{
    public: StackDoubleFloatComplex(Val x)
    {
        m_classd  = CLASSD_double_float_complex;
        m_dblReal = convert_to_float64(x);
        m_dblImag = 0;
    } // StackDoubleFloatComplex

    public: StackDoubleFloatComplex(Val x, Val y)
    {
        m_classd  = CLASSD_double_float_complex;
        m_dblReal = convert_to_float64(x);
        m_dblImag = convert_to_float64(y);
    } // StackDoubleFloatComplex

    public: StackDoubleFloatComplex(float64 x, float64 y)
    {
        m_classd  = CLASSD_double_float_complex;
        m_dblReal = x;
        m_dblImag = y;
    } // StackDoubleFloatComplex
}; // StackDoubleFloatComplex


//////////////////////////////////////////////////////////////////////
//
// StackRationalComplex
//
class __declspec(align(Kernel_Arch_BinObj_Align)) StackRationalComplex :
    public Kernel::RationalComplex
{
    public: StackRationalComplex(Val x)
    {
        m_classd  = CLASSD_rational_complex;
        m_real = x;
        m_imag = Fixnum::Encode(0);
    } // StackRationalComplex
}; // StackRationalComplex


//////////////////////////////////////////////////////////////////////
//
// StackSingleFloatComplex
//
class __declspec(align(Kernel_Arch_BinObj_Align)) StackSingleFloatComplex :
    public Kernel::SingleFloatComplex
{
    public: StackSingleFloatComplex(Val x)
    {
        m_classd  = CLASSD_single_float_complex;
        m_fltReal = convert_to_float32(x);
        m_fltImag = 0;
    } // StackSingleFloatComplex

    public: StackSingleFloatComplex(Val x, Val y)
    {
        m_classd  = CLASSD_single_float_complex;
        m_fltReal = convert_to_float32(x);
        m_fltImag = convert_to_float32(y);
    } // StackSingleFloatComplex
}; // StackSingleFloatComplex


} // MiniLisp

#endif //!defined(INCLUDE_big_12_complex_h)
