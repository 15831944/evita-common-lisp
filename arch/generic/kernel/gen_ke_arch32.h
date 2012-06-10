//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - generic 32bit architecture
// arch/generic/ke_arch32.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/generic/kernel/gen_ke_arch32.h#7 $
//
#if !defined(INCLUDE_arch_generic_kernel_arch32_h)
#define INCLUDE_arch_generic_kernel_arch32_h

namespace Kernel
{

#define SIZEOF_VAL      4
                            //76543210
#define Kernel_Heap_Start   0x20000000
#define ADDRESS_OF_NIL      0x20000042


    // For __declspec(align(n))
#define Kernel_Arch_BinObj_Align    8
#define Kernel_Arch_Cons_Align      8
#define Kernel_Arch_FunObj_Align    16
#define Kernel_Arch_Instance_Align  8
#define Kernel_Arch_Record_Align    8
#define Kernel_Arch_ObStack_Align   16

//////////////////////////////////////////////////////////////////////
//
// Arch32 -- tag scheme for 32 bit machine.
//
//        | xx00        xx01        xx10        xx11
//   -----+--------------------------------------------
//   00xx | fixnum      symbol      not used    nil
//   01xx | fixnum      instance    record      cons
//   10xx | fixnum      function    not used    reserved for nil
//   11xx | fixnum      instance    record      cons
//
class Arch32
{
    public: static const size_t BinObj_Align    = Kernel_Arch_BinObj_Align;
    public: static const size_t Cons_Align      = Kernel_Arch_Cons_Align;
    public: static const size_t FunObj_Align    = Kernel_Arch_FunObj_Align;
    public: static const size_t Instance_Align  = Kernel_Arch_Instance_Align;
    public: static const size_t Record_Align    = Kernel_Arch_Record_Align;
    public: static const size_t ObStack_Align   = Kernel_Arch_ObStack_Align;

    public: static const int    Bits_Value      = 32;
    public: static const int    Bits_Tag        = 4;
    public: static const int    Bits_FixnumTag  = 2;

    public: static const ptrdiff_t Param_Start     = Kernel_Heap_Start;
    public: static const size_t    Param_AllocUnit = 64 * 1024;

    public: enum Tag
    {
         Tag_Fixnum     = 0,   // 0000
         Tag_Record     = 1,   // 0001
         Tag_Null       = 2,   // 0010
         Tag_invalid3   = 3,   // 0011

         Tag_Fixnum1    = 4,   // 0100
         Tag_Function   = 5,   // 0101
         Tag_Cons       = 6,   // 0110
         Tag_invalid7   = 7,   // 0101

         Tag_Fixnum2    = 8,   // 1000
         Tag_Record8    = 9,   // 1001
         Tag_reserved   = 10,  // 1010
         Tag_invalid11  = 11,  // 1011

         Tag_Fixnum3    = 12,  // 1100
         Tag_invalid13  = 13,  // 1101
         Tag_Cons8      = 14,  // 1110
         Tag_invalid15  = 15,  // 1111
    }; // Tag

    public: typedef uint32 Bigit;
    public: typedef int32  SignedBigit;

    public: typedef uint32 BitEltT;
    public: typedef uint32 OffsetT;

    public: static const BitEltT k_rgfBit[32];
    public: static const BitEltT k_rgnLeaderMask[32];
    public: static const BitEltT k_rgnTrailerMask[32];
}; // Arch32

#define case_Tag_Cons \
    case Val_::Tag_Cons: case Val_::Tag_Cons8

#define case_Tag_Record \
    case Val_::Tag_Record: case Val_::Tag_Record8

#define case_Tag_Fixnum \
    case Val_::Tag_Fixnum: case Val_::Tag_Fixnum1: \
    case Val_::Tag_Fixnum2: case Val_::Tag_Fixnum3

#define case_Tag_Immediate \
    case_Tag_Fixnum: \
    case Val_::Tag_Null

class Arch : public Arch32 {};

} // Kernel

#endif //!defined(INCLUDE_arch_generic_kernel_arch32_h)
