//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - generic 32bit architecture
// arch/generic/ke_arch32.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/generic/kernel/gen_ke_arch64.h#8 $
//
#if !defined(INCLUDE_arch_generic_kernel_arch64_h)
#define INCLUDE_arch_generic_kernel_arch64_h

namespace Kernel
{

#define SIZEOF_VAL      8

// For debugging purpose, we use heap start address is over 4GB and low
// 32bit is over 2GB.
#define Kernel_Heap_Start   0x3F0000000ull
//                            876543210

// Note: If you change this value, you MUST also change value of nil in
// arch/x64/kernel/x64_ke.asm
#define ADDRESS_OF_NIL      0x3F0000082ull
//                            876543210

// For __declspec(align(n))
#define Kernel_Arch_BinObj_Align    16
#define Kernel_Arch_Cons_Align      16
#define Kernel_Arch_FunObj_Align    16
#define Kernel_Arch_Instance_Align  16
#define Kernel_Arch_Record_Align    16
#define Kernel_Arch_ObStack_Align   32

//////////////////////////////////////////////////////////////////////
//
// Arch64
//
//       | xx00        xx01        xx10         xx11
//  -----+--------------------------------------------
//  00xx | fixnum
//  01xx | single-float instance    record      nil
//  10xx |              function
//  11xx | fixnum       record                  cons
//
class Arch64
{
    public: static const size_t BinObj_Align    = Kernel_Arch_BinObj_Align;
    public: static const size_t Cons_Align      = Kernel_Arch_Cons_Align;
    public: static const size_t FunObj_Align    = Kernel_Arch_FunObj_Align;
    public: static const size_t Instance_Align  = Kernel_Arch_Instance_Align;
    public: static const size_t Record_Align    = Kernel_Arch_Record_Align;
    public: static const size_t ObStack_Align   = Kernel_Arch_ObStack_Align;

    public: static const int    Bits_Value      = 64;
    public: static const int    Bits_Tag        = 4;
    public: static const int    Bits_FixnumTag  = 3;

    public: static const ptrdiff_t Param_Start     = Kernel_Heap_Start;
    public: static const size_t    Param_AllocUnit = 64 * 1024;

    public: enum Tag
    {
         Tag_Fixnum         = 0,   // 0000
         Tag_Record         = 1,   // 0001
         Tag_Null           = 2,   // 0010
         Tag_invalid3       = 3,   // 0011

        #if USE_SINGLE_FLOAT_TAG
         Tag_SingleFloat    = 4,   // 0100
        #endif // USE_SINGLE_FLOAT_TAG

         Tag_Instance       = 5,   // 0101
         Tag_6              = 6,   // 0110
         Tag_invalid7       = 7,   // 0101

         Tag_Fixnum1        = 8,   // 1000
         Tag_Function       = 9,   // 1001
         Tag_Cons           = 10,  // 1010
         Tag_11             = 11,  // 1011

         Tag_12             = 12,  // 1100
         Tag_invalid13      = 13,  // 1101
         Tag_14             = 14,  // 1110
         Tag_invalid15      = 15,  // 1111
    }; // Tag

    public: typedef uint64  Bigit;
    public: typedef int64   SignedBigit;

    public: typedef uint64 BitEltT;
    public: typedef uint64 OffsetT;

    public: static const BitEltT k_rgfBit[64];
    public: static const BitEltT k_rgnLeaderMask[64];
    public: static const BitEltT k_rgnTrailerMask[64];
}; // Arch64


#define case_Tag_Cons       case Val_::Tag_Cons
#define case_Tag_Instance   case Val_::Tag_Instance
#define case_Tag_Record     case Val_::Tag_Record

#define case_Tag_Fixnum     case Val_::Tag_Fixnum: case Val_::Tag_Fixnum1

#if USE_SINGLE_FLOAT_TAG

    #define case_Tag_Immediate \
        case Val_::Tag_SingleFloat: \
        case_Tag_Fixnum: \
        case Val_::Tag_Null

#else // USE_SINGLE_FLOAT_TAG

    #define case_Tag_Immediate \
        case_Tag_Fixnum: \
        case Val_::Tag_Null

#endif // USE_SINGLE_FLOAT_TAG


class Arch : public Arch64 {};

} // Kernel

#endif //!defined(INCLUDE_arch_generic_kernel_arch64_h)
