//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers - Ratio
// big/big_12_ratio.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_12_ratio.h#3 $
//
#if !defined(INCLUDE_big_12_ratio_h)
#define INCLUDE_big_12_ratio_h

namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// RatioImpl
//
class RatioImpl : public Kernel::Ratio
{
    public: static Val Add(Val, Val);
    public: static Int Cmp(Val, Val);
    public: static Val Div(Val, Val);
    public: static Val Mul(Val, Val);
    public: static Val Sub(Val, Val);
    public: static float64 ToFloat64(Val);
    public: static float32 ToFloat32(Val);
    public: static Val Make(Val, Val);

    public: static void Ceiling(Val, Val, Val*, Val*);
    public: static void Floor(Val, Val, Val*, Val*);
    public: static void Round(Val, Val, Val*, Val*);
    public: static void Truncate(Val, Val, Val*, Val*);
}; // RatioImpl


//////////////////////////////////////////////////////////////////////
//
// StackRatio
//
class __declspec(align(Kernel_Arch_Record_Align)) StackRatio :
    public Kernel::Ratio
{
    public: StackRatio(Val num, Val den = Fixnum::Encode(1))
    {
        m_classd  = CLASSD_ratio;
        m_num = num;
        m_den = den;
    } // StackRatio
}; // StackRatio

} // MiniLisp

#endif //!defined(INCLUDE_big_12_ratio_h)
