//////////////////////////////////////////////////////////////////////////////
//
// evcl - Kernel - Layout of Objects
// kernel/ke_layout.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_layout.h#20 $
//
#if !defined(INCLUDE_kernel_ke_layout_h)
#define INCLUDE_kernel_ke_layout_h

namespace Kernel
{

class Value_;
typedef Value_* Value;

//////////////////////////////////////////////////////////////////////
//
// Lisp Value
//
class Value_ :
    public AsInt,
    public Arch
{
    private: Value_() {}
    private: ~Value_() {}

    public: static const int
        Mask_Tag = (1 << Bits_Tag) - 1;

    public: Val    GetClassD() const;
    public: void*  GetPtr() const;
    public: size_t GetSize() const;

    public: Tag GetTag2() const
        { return static_cast<Tag>(ToInt() & 3); }

    public: Tag GetTag3() const
        { return static_cast<Tag>(ToInt() & 7); }

    public: Tag GetTag4() const
        { return static_cast<Tag>(ToInt() & Mask_Tag); }

    public: template<class T> T* Decode() const
        { return FromInt<T>(T::Decode_(this)); } 

    public: template<class T> bool Is() const
        { return T::Is_(this); }

    public: bool IsHeap() const;

    public: template<class T> T* StaticCast()
        { ASSERT(Is<Fixnum>()); return reinterpret_cast<T*>(this); }

    public: template<class T> T* Storage() const
        { return FromInt<T>(T::Storage_(this)); } 
}; // Value_


//////////////////////////////////////////////////////////////////////
//
// Fixnum
//
class Fixnum : public AsInt
{
    public: enum { Bits     = Arch::Bits_Value - Arch::Bits_FixnumTag };
    public: enum { TagBits  = Arch::Bits_FixnumTag };
    public: enum { Tag      = Arch::Tag_Fixnum };
    public: enum { TagMask  = (1 << TagBits) - 1 };

    public: enum Const
    {
        One = Val_::Tag_Fixnum1,
    }; // Const

    public: static const Int MostPositive =  
                (static_cast<Int>(1) << (Bits - 1)) - 1;

    public: static const Int MostNegative =  
                static_cast<Int>(-1) << (Bits - 1);

    public: static Int Decode_(const Value_ *const val)
    {
        ASSERT(Is_(val));
        return val->ToInt() >> TagBits;
    } // Decode_

    public: static Val Encode(Int iVal)
        { return FromInt<Value_>(iVal << TagBits ); }

    public: static Val Encode(const void* pv)
    {
        Int iVal = reinterpret_cast<Int>(pv);
        Val val = FromInt<Value_>(iVal);
        ASSERT(val->Is<Fixnum>());
        return val;
    } // Encode

    public: static bool Is_(const Value_ *const val)
        { return Tag == val->GetTag2(); }

    private: static Val Add(Val x, Val y)
        { return FromInt<Value_>(x->ToInt() + y->ToInt()); }

    private: static Val Truncate(Val x, Val y)
        { return Encode(Decode_(x) / Decode_(y)); }

    private: static Val Mul(Val x, Val y)
        { return FromInt<Value_>(x->ToInt() * Decode_(y)); }

    private: static Val Sub(Val x, Val y)
        { return FromInt<Value_>(x->ToInt() - y->ToInt()); }

    private: static Val Add(Val x, Int i) { return Add(x, Encode(i)); }
    private: static Val Truncate(Val x, Int i) { return Truncate(x, Encode(i)); }
    private: static Val Mul(Val x, Int i) { return Mul(x, Encode(i)); }
    private: static Val Sub(Val x, Int i) { return Sub(x, Encode(i)); }

    private: static bool Ge(Val x, Val y)
        { return x->ToInt() >= y->ToInt(); }

    private: static bool Gt(Val x, Val y)
        { return x->ToInt() > y->ToInt(); }

    private: static bool Le(Val x, Val y)
        { return x->ToInt() <= y->ToInt(); }

    private: static bool Lt(Val x, Val y)
        { return x->ToInt() < y->ToInt(); }

    private: static bool Ge(Val x, Int i) { return Ge(x, Encode(i)); }
    private: static bool Gt(Val x, Int i) { return Gt(x, Encode(i)); }
    private: static bool Le(Val x, Int i) { return Le(x, Encode(i)); }
    private: static bool Lt(Val x, Int i) { return Lt(x, Encode(i)); }


    #if SIZEOF_VAL == 8
        public: static Val Encode(int iVal)
            { return FromInt<Value_>(
                static_cast<Int>(iVal) << TagBits ); }

        public: static Val Encode(uint iVal)
            { return FromInt<Value_>(
                static_cast<Int>(iVal) << TagBits ); }

        public: static Val Encode(size_t iVal)
            { return FromInt<Value_>(
                static_cast<Int>(iVal) << TagBits ); }
    #endif // SIZEOF_VAL == 8
}; // Fixnum


namespace Layout
{
    class C_root   : public AsInt {};

    #define DEFCLASS(mp_name, mp_NAME, mp_super) \
        class C##_##mp_name : public C_##mp_super {

    #define FINALIZED_CLASS(mp_name) \
        public: static Val ClassD() { return CLASSD_##mp_name; }

    #define ENDCLASS() };

    #define DEFSLOT_(mp_n, mp_cname, mp_STR, mp_ctype, mp_ty) \
        public: mp_ctype mp_cname;

    #include "./ke_layout.inc"

    CASSERT(sizeof(C_cons)              == sizeof(Val) * 2);
    CASSERT(sizeof(C_simple_vector)     == sizeof(Val) * 4);
    CASSERT(sizeof(C_simple_string)     == sizeof(Val) * 4);
    CASSERT(sizeof(C_simple_bit_vector) == sizeof(Val) * 4);
} // Layout


//////////////////////////////////////////////////////////////////////
//
// Record_
//
template<class Layout_>
class Record_ : public Layout_
{
    public: enum { Align    = Kernel_Arch_Record_Align };
    public: enum { Tag      = Arch::Tag_Record };
    public: enum { TagMask  = Align - 1 };

    public: static Int Decode_(const Value_ *const val)
        { ASSERT(val->Is<Record>()); return val->ToInt() - Tag; }

    public: Val Encode() const
        { return FromInt<Value_>(ToInt() + Tag); }

    public: static bool Is_(const Value_ *const val)
    {
        return val->Is<Record>() && 
            Layout_::ClassD() == val->Decode<Record>()->m_classd;
    } // Is_

    public: operator Val() const
        { return Encode(); }
}; // Record_


//////////////////////////////////////////////////////////////////////
//
// Record
//
class Record : public Record_<Layout::C_record>
{
    // public: Val m_classd;    // [0]

    #if SIZEOF_VAL == 4
        public: static bool Is_(const Value_ *const val)
            { return Tag == val->GetTag3(); }
    #elif SIZEOF_VAL == 8
        public: static bool Is_(const Value_ *const val)
            { return Tag == val->GetTag4(); }
    #endif // SIZEOF_VAL
}; // Record
CASSERT(sizeof(Record) == sizeof(Val));


//////////////////////////////////////////////////////////////////////
//
// Data Vector
//
class DataVector : public Record_<Layout::C_data_vector>
{
    //public: Val m_classd;     // [0] record
    //public: Val m_length;     // [1] data-vector

    #if SIZEOF_VAL == 4
        public: static bool Is_(const Value_ *const val)
            { return Tag == val->GetTag3(); }
    #elif SIZEOF_VAL == 8
        public: static bool Is_(const Value_ *const val)
            { return Tag == val->GetTag4(); }
    #endif // SIZEOF_VAL
}; // DataVector


//////////////////////////////////////////////////////////////////////
//
// Class Description
//
//      none        0
//      fixed       object size in byte
//      function    0
//      vector      bit size of element
//      array       0
//      immediate   0
//
class ClassD : public Record_<Layout::C_class_description>
{
    // Instance format
    public: enum Format
    {
        Format_None                 = Fixnum::One * 0,      // 00
        Format_Fixed                = Fixnum::One * 1,      // 04
        Format_Function             = Fixnum::One * 2,      // 08
        Format_Vector               = Fixnum::One * 3,      // 0C
        Format_Array                = Fixnum::One * 4,      // 10
        Format_Immediate            = Fixnum::One * 5,      // 14
        Format_Mixed                = Fixnum::One * 6,      // 18
        Format_BinVec               = Fixnum::One * 7,      // 1C
        Format_String               = Fixnum::One * 8,      // 20
        Format_Storage              = Fixnum::One * 9,      // 24
        Format_Instance             = Fixnum::One * 10,     // 28
        Format_FuncallableInstance  = Fixnum::One * 11,     // 2C
        Format_Structure            = Fixnum::One * 12,     // 30
        Format_Cons                 = Fixnum::One * 13,     // 34
        Format_BinFixed             = Fixnum::One * 14,     // 38
        Format_Foreign              = Fixnum::One * 15,     // 3C
    }; // Format

    // public: Val   m_class;            // [1]
    // public: Val   m_hash_code;        // [2]
    // public: Val   m_slots;            // [3]
    // public: Val   m_format;           // [4]
    // public: Val   m_format_param;     // [5]
    // public: Val   m_format_misc;      // [6]
    // public: Val   m_tag_code;         // [7]

    public: static bool Is_(const Value_ *const val)
    {
        return val->Is<Record>() &&
               CLASSD_class_description ==
                val->Decode<Record>()->m_classd;
    } // Is_

    public: size_t ComputeSize() const
    {
        ASSERT(
            Format_Fixed         == m_format->ToInt() ||
            Format_BinFixed      == m_format->ToInt() ||
            Format_Mixed         == m_format->ToInt() ||
            Format_Structure     == m_format->ToInt() ||
            Format_Instance      == m_format->ToInt() );
        size_t cbObject = static_cast<uint>(m_format_param->ToInt());
        return ROUNDUP(cbObject, Value_::Record_Align);
    } // GetSize

    public: size_t ComputeSize(Val len) const;
    public: size_t ComputeSize(const Record*) const;
}; // ClassD


//////////////////////////////////////////////////////////////////////
//
// Waitable
//
class Waitable : public Record_<Layout::C_waitable>
{
    //public: Val   m_thread;   // [1]
    //public: Val   m_state;    // [2]
}; // Waitable

//////////////////////////////////////////////////////////////////////////////
//
// 05 Data and Control Flow
//

template<class Layout_>
class Function_ : public Layout_
{
   public: enum { Align     = Kernel_Arch_FunObj_Align };
   public: enum { Tag       = Arch::Tag_Function };
   public: enum { TagMask   = Align - 1 };

    public: static Int Decode_(const Value_ *const val)
    {
        ASSERT(val->Is<Funcallable>());
        return val->ToInt() - Function_::Tag;
    } // Decode_

    public: Val Encode() const
        { return FromInt<Value_>(ToInt() + Function_::Tag); }

    public: static bool Is_(const Value_ *const val)
        { return Function_::Tag == val->GetTag4(); }
}; // FunctionPtr


//////////////////////////////////////////////////////////////////////
//
// Funcallable
//
class Funcallable : public Function_<Layout::C_compiled_function>
{
    public: Int m_cbFunction;
    public: Val m_classd;
    public: Int m_nCookie;
}; // Funcallable

CASSERT(
    offsetof(Funcallable, m_nCookie) ==
    offsetof(Layout::C_native_code_object, m_nCookie) );

CASSERT(
    offsetof(Funcallable, m_classd) ==
    offsetof(Layout::C_native_code_object, m_classd) );

CASSERT(
    offsetof(Funcallable, m_cbFunction) ==
    offsetof(Layout::C_native_code_object, m_cbFunction) );

//////////////////////////////////////////////////////////////////////
//
// Native Code Function
//
// Slots:
//  m_xClassD - A class-description
//    A class description of native-code-function. This slot contains
//    class description of either native-code-function, native-closure,
//    or funcallable-instance.
//
//  m_nCookie - A fixnum.
//   Contains a magic integer used for sentinel for mapping return
//   address to function object. If this value is zero, function is being
//   initialized and is not ready for GC.
//
//  m_name - A symbol or cons.
//    Contains name of function.
//
// Note: m_xClassD and m_xId must be same location to byte-code-function.
// Note: m_xCookie must NOT be the first and second slot. Since, GC uses
// the first and second slot for GC forwarding.
//
// Function descritoin (FunDesc) is architecure dependent. See nm_exec.h
// for implementation.
//
class NativeCodeFunction :
    public Function_<Layout::C_native_code_function>
{
    // uint8                    m_rgbCode[m_oDesc.m_cbCodeLen];
    // uint8                    m_rgbAlignPad[16 - m_oDesc.m_cbCodeLen % 16];
    // NativeCodeFunctionDesc   m_oDesc;
}; // NativeCodeFunction

//////////////////////////////////////////////////////////////////////////////
//
// 07 Objects
//

//////////////////////////////////////////////////////////////////////
//
// Instance
//
class Instance : public Layout::C_instance
{
    public: enum { Align    = Kernel_Arch_Instance_Align };
    public: enum { Tag      = Arch::Tag_Record };
    public: enum { TagMask  = Align - 1 };

    //public: Val m_classd;     // [0]
    //public: Val m_storage;    // [1]

    public: static Int Decode_(const Value_ *const val)
        { ASSERT(Is_(val)); return val->ToInt() - Instance::Tag; }

    public: Val Encode() const
        { return FromInt<Value_>(ToInt() + Instance::Tag); }

    #if SIZEOF_VAL == 4
        public: static bool Is_(const Value_ *const val)
            { return Tag == val->GetTag3(); }
    #elif SIZEOF_VAL == 8
        public: static bool Is_(const Value_ *const val)
            { return Tag == val->GetTag4(); }
    #endif // SIZEOF_VAL
}; // Instance
CASSERT(sizeof(Val) * 2 == sizeof(Instance));


//////////////////////////////////////////////////////////////////////
//
// Data Vector
//
class Storage : public Record_<Layout::C_storage>
{
    //public: Val m_classd;     // [0]
    //public: Val m_storaged;   // [1]
    public: Val mv_element[1];
}; // Storage


//////////////////////////////////////////////////////////////////////
//
// Instance
//
template<class Layout_>
class Instance_ : public Layout_
{
    public: enum { Tag = Instance::Tag };

    //public: Val m_classd;     // [0]
    //public: Val m_storaged;   // [1]

    public: static Int Decode_(const Value_ *const val)
    {
        Val storage = val->Decode<Instance>()->m_storage;
        return storage->Decode<Storage>()->ToInt();
    } // Decode_

    public: Val Encode() const
        { return FromInt<Value_>(ToInt() + Instance_::Tag); }

    #if SIZEOF_VAL == 4
        public: static bool Is_(const Value_ *const val)
            { return Tag == val->GetTag3(); }
    #elif SIZEOF_VAL == 8
        public: static bool Is_(const Value_ *const val)
            { return Tag == val->GetTag4(); }
    #endif // SIZEOF_VAL
}; // Instance_


//////////////////////////////////////////////////////////////////////
//
// Class
//
class Class : public Instance_<Layout::C_class>
{
    //public: Val m_plist;                  // [2] dependee-mixin
    //public: Val m_flags;                  // [3]
    //public: Val m_direct_methods;         // [4]
    //public: Val m_name;                   // [5]
    //public: Val m_direct_super_classes;   // [6]
    //public: Val m_direct_sub_classes;     // [7]
    //public: Val m_class_precedence_list;  // [8]
    //public: Val m_instanced;              // [9]
    //public: Val m_direct_slots;           // [10]
    //public: Val m_slots;                  // [11]
    //public: Val m_prototype;              // [12]
}; // Class

class StandardClass :
    public Instance_<Layout::C_standard_class> {};

class BuiltInClass :
    public Instance_<Layout::C_built_in_class> {};


class DirectSlotD :
    public Instance_<Layout::C_standard_direct_slot_definition> {};

class EffectiveSlotD :
    public Instance_<Layout::C_standard_effective_slot_definition> {};


class FuncallableInstance :
    public Function_<Layout::C_funcallable_instance>
{
    //public: Val     m_classd;     // [0]
    //public: size_t  m_cbFunction; // [1]
    //public: Val     m_storage;    // [2]
    //public: Val     m_cookie;     // [3]
}; // FuncallableInstance
CASSERT(sizeof(FuncallableInstance) == sizeof(NativeCodeFunction));


template<class Layout_>
class FuncallableInstance_ : public Function_<Layout_>
{
    //public: Val m_classd;     // [0]
    //public: Val m_storaged;   // [1]

    public: static Int Decode_(const Value_ *const val)
    {
        Val storage = val->Decode<FuncallableInstance>()->m_storage;
        return storage->Decode<Storage>()->ToInt();
    } // Decode_
}; // FuncallableInstance_


class StandardGenericFunction :
    public FuncallableInstance_<Layout::C_standard_generic_function>
{
    //public: Val m_classd;             // [0] storage
    //public: Val m_storaged;           // [1]
    //public: Val m_plist;              // [2] dependee-mixin
    //public: Val m_name;               // [3] standard-generic-function
    //public: Val m_methods;            // [4]
    //public: Val m_method_class;       // [5]
    //public: Val m_method_combination; // [6]
    //public: Val m_param_info;           // [7]
}; // StandardGenericFunction

class StandardMethod :
    public Instance_<Layout::C_standard_method>
{
    //public: Val m_classd;             // [0] storage
    //public: Val m_storaged;           // [1]
    //public: Val m_plist;              // [2] dependee-mixin
    //public: Val m_generic_function;   // [3] standard-method
    //public: Val m_specializers;       // [4]
    //public: Val m_qualifiers;         // [5]
    //public: Val m_lambda_list;        // [6]
    //public: Val m_function;           // [7]
}; // StandardMethod

// ParamInfo
class ParamInfo : public Record_<Layout::C_param_info> {};

//////////////////////////////////////////////////////////////////////
//
// 08 Structures
//
class StructureClass :
    public Instance_<Layout::C_structure_class> {};

class StructureObject :
    public Record_<Layout::C_structure_object>
{
    //public:   m_classd;               // [0]
    public:   Val mv_slot[1];         // [1]
}; // StructureObject
CASSERT(sizeof(StructureObject) == sizeof(Val) * 2);

//////////////////////////////////////////////////////////////////////
//
// 09 Conditions
//

// For printer.

// SimpleError
class SimpleError : public Instance_<Layout::C_simple_error>
{
    //public: Val m_format_control;
    //public: Val m_format_arguments;
}; // SimpleError


// TypeError
class TypeError : public Instance_<Layout::C_type_error>
{
    //public: Val m_expected_type;
    //public: Val m_datum;
}; // TypeError

// CatchTagNotSeen
class CatchTagNotSeen :
    public Instance_<Layout::C_catch_tag_not_seen>
{
    //public: Val m_tag;
}; // CatchTagNotSeen


// UndefinedFunction
class UndefinedFunction :
    public Instance_<Layout::C_undefined_function>
{
    // no member
}; // UndefinedFunction

// NotFunction
class NotFunction :
    public Instance_<Layout::C_not_function>
{
    //public: Val   m_name; // [0]
}; // NotFunction


// TooFewArguments
class TooFewArguments :
    public Instance_<Layout::C_too_few_arguments>
{
    //public: Val m_function;       // [0]
    //public: Val m_arguments;      // [1]
}; // TooFewArguments


// TooManyArguments
class TooManyArguments :
    public Instance_<Layout::C_too_many_arguments>
{
    //public: Val m_function;       // [2]
    //public: Val m_arguments;      // [3]
}; // TooManyArguments

//////////////////////////////////////////////////////////////////////////////
//
// 10 Symbols
//

//////////////////////////////////////////////////////////////////////
//
// Symbol
//  For duarity of nil, tag check is NOT simple.
//
//  When symbol doesn't associate to function, m_function contains nil.
//
class Symbol : public Record_<Layout::C_symbol>
{
    //public: Val m_classd;
    //public: Val m_hash_code;
    //public: Val m_name;
    //public: Val m_package;
    //public: Val m_function;

    public: static Int Decode_(const Value_ *const val)
        { ASSERT(val->Is<Symbol>()); return val->ToInt() & ~Symbol::TagMask; }

    public: static bool Is_(const Value_ *const val)
    {
        if (val == nil) return true;
        return Record_<Layout::C_symbol>::Is_(val);
    } // Is_

    //public: Val GetHashCode() const { return m_hash_code; }
    //public: Val GetName()     const { return m_name; }
    //public: Val GetPackage()  const { return m_package; }
}; // Symbol

//////////////////////////////////////////////////////////////////////////////
//
// 12 Numbers
//

//////////////////////////////////////////////////////////////////////
//
// Bignum
//
class Bignum : public Record_<Layout::C_bignum>
{
    public: typedef Arch::Bigit  Bigit;
    public: typedef Arch::SignedBigit SignedBigit;

    //public: Val m_length;         // [1]
    //public: Val m_nbigits;        // [2]
    //public: Elt m_rgnBigit[1];    // [3]
}; // Bignum


//////////////////////////////////////////////////////////////////////
//
// DoubleFloat
//
//  Exponent    Significand
//  -------------------------------------------------------
//  2047        0000 0000 ... 0000   Infinity
//  2047        1uu0 uuuu ... uuuu   NaN        bit51 == 1
//  2047        0uu0 uuuu ... uuuu   SNaN       bit51 != 1
//     0        0000 0000 ... 0000   Zero
//     0        uuuu uuuu ... uuuu   Subnormal (f x 2^-1074)
//  others      uuuu uuuu ... uuuu   Normal    (2^52+f) x 2(be-1075)
//
//  Note: 1075 = ExponentBias(1023) + SignificandBits(52)
//
class DoubleFloat : public Record_<Layout::C_double_float>
{
    //public: double   m_dbl;             // [1]

    public: enum
    {
        ExponentMax         =  2047,
        ExponentMin         =  0,

        ExponentBias        =   1023,
        //ExponentNormal    =  -1075,   // = be-1023-52, where be=1
        ExponentSubnormal   =  -1074,   // = -1023-51

        SignificandBits     =  52,
        NormalPrecision     = SignificandBits + 1,
        SubnormalPrecision  = SignificandBits,
    }; // enum

    public: typedef Float64_Layout Layout;
}; // DoubleFloat


//////////////////////////////////////////////////////////////////////
//
// DoubleFloatComplex
//
class DoubleFloatComplex :
    public Record_<Layout::C_double_float_complex>
{
    //public: Val      m_pad1;     // [1]
    //public: double   m_dblReal;  // [2,3]
    //public: double   m_dblImag;  // [4,5]
    //public: Val      m_pad6;     // [6]
    //public: Val      m_pad7;     // [6]
}; // DoubleFloatComplex


//////////////////////////////////////////////////////////////////////
//
// RationalComplex
//
class RationalComplex :
    public Record_<Layout::C_rational_complex>
{
    //public: Val m_pad1;  // [1]
    //public: Val m_real;  // [2]
    //public: Val m_imag;  // [3]
}; // RationalComplex


//////////////////////////////////////////////////////////////////////
//
// Ratio
//
class Ratio :
    public Record_<Layout::C_ratio>
{
    //public: Val m_pad1;  // [1]
    //public: Val m_num;   // [2]
    //public: Val m_den;   // [3]
}; // Ratio


//////////////////////////////////////////////////////////////////////
//
// SingleFloat
//
//  Exponent    Significand
//  -------------------------------------------------------
//  255         000 0000 ... 0000   Infinity
//  255         1uu uuuu ... uuuu   NaN        bit22 == 1
//  255         0uu uuuu ... uuuu   SNaN       bit22 != 1
//    0         000 0000 ... 0000   Zero
//    0         uuu uuuu ... uuuu   Subnormal (f x 2^-149)
//  others      uuu uuuu ... uuuu   Normal    (2^23+f)x2(be-150)
//
//  Note: 150 = ExponentBias(127) + SignificandBits(23)
//
class SingleFloat :
    public Record_<Layout::C_single_float>
{
    //public: float m_flt; // [1]

    public: enum
    {
        ExponentMax         = 255,
        ExponentMin         = 0,

        ExponentBias        =  127,
        //ExponentNormal    = -150,     // = be-127-23, where be=1
        ExponentSubnormal   = -149,     // = -127-22

        SignificandBits        = 23,
        NormalPrecision     = SignificandBits + 1,
        SubnormalPrecision  = SignificandBits,
    }; // enum

    #if USE_SINGLE_FLOAT_TAG
        public: enum { ShiftCount = 8 };
    #endif // USE_SINGLE_FLOAT_TAG

    public: typedef Float32_Layout Layout;
}; // SingleFloat


//////////////////////////////////////////////////////////////////////
//
// SingleFloatComplex
//
class SingleFloatComplex :
    public Record_<Layout::C_single_float_complex>
{
    //public: Val      m_pad1;     // [1]
    //public: float    m_fltReal;  // [2]
    //public: float    m_fltImag;  // [3]
}; // SingleFloatComplex

#define CLASSD_float_min    CLASSD_double_float
#define CLASSD_float_max    CLASSD_single_float
#define CLASSD_real_min     CLASSD_bignum
#define CLASSD_real_max     CLASSD_single_float
#define CLASSD_complex_min  CLASSD_double_float_complex
#define CLASSD_complex_max  CLASSD_single_float_complex
#define CLASSD_number_min   CLASSD_real_min
#define CLASSD_number_max   CLASSD_complex_max

//////////////////////////////////////////////////////////////////////////////
//
// 13 Characters
//


//////////////////////////////////////////////////////////////////////
//
// Character class
//
//  32bit (SIZEOF_VAL == 4)
//  0..15 01234567 0..4     10       = 16 + 8 + 5 + 2 = 31
//  case  SULWGNA- Category Tag
//
//  64bit (SIZEOF_VAL == 8)
//  0..15 0..15 01234567 0..4     012   = 16+16+8+5+3 = 48
//  Upper Lower SULWGNA- Category Tag
//
class Character : public Record_<Layout::C_character>
{
    public: enum
    {
        Max     = 0xFFFF,
        Limit   = Max + 1,

        #if SIZEOF_VAL == 4
            ShiftCount      = 3,
            CaseShiftCount  = 8 + 5 + 2,
        #endif // SIZEOF_VAL == 4

        #if SIZEOF_VAL == 8
            ShiftCount      = 4,
            LowerShiftCount = 8 + 5 + 3,
            UpperShiftCount = LowerShiftCount + 16,
            Mask            = 0xFFFF,
        #endif // SIZEOF_VAL == 8
    }; // limits

    public: enum Attr
    {
        Attr_CategoryMask   = Fixnum::One * 31,
        Attr_Alpha          = Fixnum::One * 32,
        Attr_Numeric        = Fixnum::One * 64,
        Attr_Graphic        = Fixnum::One * 128,
        Attr_Whitespace     = Fixnum::One * 256,
        Attr_LowerCase      = Fixnum::One * 512,
        Attr_UpperCase      = Fixnum::One * 1024,
        Attr_Standard       = Fixnum::One * 2048,

        Attr_AlphaNumeric = Attr_Alpha | Attr_Numeric,
        Attr_BothCase     = Attr_LowerCase | Attr_UpperCase,
    }; // Attr

    // Unicode Category
    public: enum Category
    {
        // General Category Constants
        Category_ControlMin             = 0,
        Category_Unassigned             = 0,     // Cn
        Category_Control                = 1,     // Cc
        Category_Format                 = 2,     // Cf
        Category_PrivateUse             = 3,     // Co
        Category_Surrogate              = 4,     // Cs
        Category_ControlMax             = 4,

        // Note: For \p{L&}, we assign consecutive numbers for Ll, Lt, Lu.
        Category_LetterMin              = 5,
        Category_LowercaseLetter        = 5,     // Ll
        Category_TitlecaseLetter        = 6,     // Lt
        Category_UppercaseLetter        = 7,     // Lu
        Category_ModifierLetter         = 8,     // Lm
        Category_OtherLetter            = 9,     // Lo
        Category_LetterMax              = 9,

        Category_MarkMin                = 10,
        Category_CombiningSpacingMark   = 10,     // Mc
        Category_EnclosingMark          = 11,     // Me
        Category_NonSpacingMark         = 12,     // Mn
        Category_MarkMax                = 12,

        Category_NumberMin              = 13,
        Category_DecimalDigitNumber     = 13,     // Nd
        Category_LetterNumber           = 14,     // Nl
        Category_OtherNumber            = 15,     // No
        Category_NumberMax              = 15,

        Category_PunctuationMin         = 16,
        Category_ConnectorPunctuation   = 16,     // Pc
        Category_DashPunctuation        = 17,     // Pd
        Category_ClosePunctuation       = 18,     // Pe
        Category_FinalPunctuation       = 19,     // Pf
        Category_InitialPunctuation     = 20,     // Pi
        Category_OtherPunctuation       = 21,     // Po
        Category_OpenPunctuation        = 22,     // Ps
        Category_PunctuationMax         = 22,

        Category_SymbolMin              = 23,
        Category_CurrencySymbol         = 23,     // Sc
        Category_ModifierSymbol         = 24,     // Sk
        Category_MathSymbol             = 25,     // Sm
        Category_OtherSymbol            = 26,     // So
        Category_SymbolMax              = 26,

        Category_SeparatorMin           = 27,
        Category_LineSeparator          = 27,     // Zl
        Category_ParagraphSeparator     = 28,     // Zp
        Category_SpaceSeparator         = 29,     // Zs
        Category_SeparatorMax           = 29,
    }; // Category

    public: static Val Encode(char16 wchChar)
    {
        return FromInt<Value_>(
            QQchar_min->ToInt() + wchChar * sizeof(Character) | Tag );
    } // Decode_

    public: static char16 ToCode(const Value_* const val)
    {
        return static_cast<char16>(
            (val->ToInt() - QQchar_min->ToInt()) / sizeof(Character) );
    } // ToCode
}; // Character

//////////////////////////////////////////////////////////////////////////////
//
// 14 Conses
//

//////////////////////////////////////////////////////////////////////
//
// Cons
//
class Cons : 
    public Layout::C_cons
{
    public: enum { Align    = Kernel_Arch_Cons_Align };
    public: enum { TagMask  = Align - 1 };

    //public: Val m_car;
    //public: Val m_cdr;

    #if SIZEOF_VAL == 4
        // Note: We use Tag_Cons8 instead of Tag_Cons for nil duality.
        public: enum { Tag  = Val_::Tag_Cons8 };
        public: enum { TagX = Val_::Tag_Cons8 };

        public: static Int Decode_(const Val_ *const val)
            { return val->ToInt() - Cons::Tag; }

        public: Val Encode() const
            { return FromInt<Val_>(ToInt() + Cons::Tag); }

        public: static bool Is_(const Val_ *const val)
            { return Val_::Tag_Cons == val->GetTag3(); }
    #elif SIZEOF_VAL == 8
        // Note: We use Tag_Cons8 instead of Tag_Cons for nil duality.
        public: enum { Tag  = Val_::Tag_Cons };
        public: enum { TagX = Val_::Tag_Cons + 0x10 };

        public: static Int Decode_(const Val_ *const val)
            { return val->ToInt() - TagX; }

        public: Val Encode() const
            { return FromInt<Val_>(ToInt() + TagX); }

        public: static bool Is_(const Val_ *const val)
            { return Val_::Tag_Cons == val->GetTag4(); }
    #endif // SIZEOF_VAL == 4
}; // Cons

CASSERT(sizeof(Cons) == sizeof(Val) * 2);


//////////////////////////////////////////////////////////////////////
//
// List
//
class List : public Cons
{
    #if SIZEOF_VAL == 4
        public: enum { Tag      = Arch::Tag_Null };
        public: enum { TagMask  = 3 };
    #elif SIZEOF_VAL == 8
        public: enum { Tag      = Arch::Tag_Null };
        public: enum { TagMask  = 7 };
    #else
        #error SIZEOF_VAL
    #endif

    public: static int GetTag(const Value_ *const val)
        { return static_cast<int>(val->ToInt() & TagMask); }

    public: static bool Is_(const Value_ *const val)
        { return Tag == GetTag(val); }
}; // List


//////////////////////////////////////////////////////////////////////
//
// Null
//
//              +-----------------+
//  0x2000 0020 |   classd        | <= "this" pointer
//              +-----------------+
//  0x2000 0024 |   length        |
//              +-----------------+
//  0x2000 0028 |   not used      |
//              +-----------------+
//  0x2000 002C |   not used      |
//              +-----------------+
//  0x2000 0030 |   not used      |
//              +-----------------+
//  0x2000 0034 |   nil           | <= cons NIL
//              +-----------------+
//  0x2000 0038 |   nil           |
//              +-----------------+
//  0x2000 003C |   not used      |
//              +-----------------+
//  0x2000 0040 |   classd        | <= symbol NIL
//              +-----------------+
//  0x2000 0044 |   hash_code     |
//              +-----------------+
//  0x2000 0048 |   name          |
//              +-----------------+
//  0x2000 004C |   package       |
//              +-----------------+
//  0x2000 0050 |   function      |
//              +-----------------+
//  0x2000 0054 |   plist         |
//              +-----------------+
//
class Null : public AsInt
{
    public: enum { Tag = Arch::Tag_Null };

        public: Val     m_classd;           // +20 +20
        public: Val     m_length;           // +24 +28
        public: Val     m_not_used_28;      // +28 +30  [0]
        public: Val     m_not_used_2C;      // +2C +38  [1]
        public: Val     m_not_used_30;      // +30 +40  [2]
        public: Val     m_car;              // +34 +48  [3]
        public: Val     m_cdr;              // +38 +50  [4]
        public: Val     m_not_used_3C;      // +3C +58  [5]
        public: Symbol  m_Symbol;           // +40 +60

    public: static Int Decode_(const Value_ *const val)
    {
        return  val->ToInt() -
                offsetof(Null, m_Symbol) -
                Value_::Tag_Null;
    } //  Decode_

    public: Val Encode() const
    {
        return FromInt<Value_>(
            ToInt() +
            offsetof(Null, m_Symbol) +
            Value_::Tag_Null );
    } // Encode
}; // Null

//////////////////////////////////////////////////////////////////////
//
// 11 Packages
//

//////////////////////////////////////////////////////////////////////
//
// Package
//  m_internal_table[0] = number of symbols in fixnum.
//  m_external_table[0] = number of symbols in fixnum.
//
//  fixnum(0) = free slot
//  fixnum(1) = removed slot
//
class Package :
    public Record_<Layout::C_package>
{
    //public: Val   m_thread;             // [1]
    //public: Val   m_state;              // [2]
    //public: Val   m_internal_table;     // [3]
    //public: Val   m_external_table;     // [4]
    //public: Val   m_names;              // [5]
    //public: Val   m_use_list;           // [6]
    //public: Val   m_used_by_list;       // [7]
    //public: Val   m_shadowing_symbols;  // [8]
    //public: Val   m_protect;            // [9]
}; // Package
CASSERT(sizeof(Package) % Arch::Record_Align == 0);

//////////////////////////////////////////////////////////////////////////////
//
// 15 Arrays
//

//////////////////////////////////////////////////////////////////////
//
// Array
//
class Array :
    public Record_<Layout::C_general_array_object>
{
    public: enum { RankLimit = 8 };

    static const Int DimensionLimit = (Fixnum::MostPositive >> 1)+1;
    static const Int TotalSizeLimit = (Fixnum::MostPositive >> 1)+1;

    public: enum Attr
    {
        Attr_Displaced   = 1,
        Attr_FillPointer = 2,
    }; // Attr

    //public: Val  m_rank;             // [1]
    //public: Val  m_flags;            // [2]
    //public: Val  m_displaced_to;     // [3]
    //public: Val  m_offset;           // [4]
    //public: Val  m_total_size;       // [5]
    //public: Val  mv_dimension[2];    // [6]
                                       // [7]
}; // Array
CASSERT(sizeof(Array) % Arch::Record_Align == 0);


//////////////////////////////////////////////////////////////////////
//
// Adjustable Vector
//
class Vector :
    public Record_<Layout::C_general_vector_object>
{
    //public: Val  m_classd;           // [0]
    //public: Val  m_fill_pointer;     // [1]
    //public: Val  m_flags;            // [2]
    //public: Val  m_displaced_to;     // [3]
    //public: Val  m_offset;           // [4]
    //public: Val  m_length;           // [5]
}; // Vector

class For_Checking
{
    class Adjustable : public Vector { friend class For_Checking; };
    class Simple     : public DataVector { friend class For_Checking; };
    void foo()
    {
        CASSERT(
            offsetof(Adjustable, m_fill_pointer) ==
            offsetof(Simple, m_length));
    }
}; // For_Checking

// SimpleBitVector
class SimpleBitVector :
    public Record_<Layout::C_simple_bit_vector>
{
    public: typedef Arch::BitEltT Elt;

    //public: Elt  m_rgfElement[2];
    public: Elt* GetElements() { return m_rgfElement; }
    public: const Elt* GetElements() const { return m_rgfElement; }
}; // SimpleBitVector

class BitVector : public Vector {};


//////////////////////////////////////////////////////////////////////
//
// Simple Vector
//
class SimpleVector :
    public Record_<Layout::C_simple_vector>
{
    //public: Val   mv_element[2];
}; // SimpleVector


#define CLASSD_array_min            CLASSD_bit_array_object
#define CLASSD_array_max            CLASSD_vector_max

#define CLASSD_simple_array_min     CLASSD_simple_bit_array_object
#define CLASSD_simple_array_max     CLASSD_data_vector_max

#define CLASSD_data_vector_min      CLASSD_simple_bit_vector
#define CLASSD_data_vector_max      CLASSD_simple_vector

#define CLASSD_vector_min           CLASSD_data_vector_min
#define CLASSD_vector_max           CLASSD_vector_object_max

#define CLASSD_array_object_min     CLASSD_bit_array_object
#define CLASSD_array_object_max     CLASSD_simple_general_array_object

#define CLASSD_vector_object_min    CLASSD_bit_vector_object
#define CLASSD_vector_object_max    CLASSD_string_object

//////////////////////////////////////////////////////////////////////////////
//
// 16 Strings
//

//////////////////////////////////////////////////////////////////////
//
// Simple String
//
class SimpleString :
    public Record_<Layout::C_simple_string>
{
    //public: char16  m_rgwchElement[4];
    public: char16* GetElements() { return m_rgwchElement; }
    public: const char16* GetElements() const { return m_rgwchElement; }
}; // SimpleString


//////////////////////////////////////////////////////////////////////
//
// String
//
class String : public Record_<Layout::C_string_object> {};

//////////////////////////////////////////////////////////////////////////////
//
// 18 Hash Tables
//

//////////////////////////////////////////////////////////////////////
//
// Hash Table
//
//      (svref m_vector 0) = count
//      (svref m_vector 1) = (* rehash-threshold size)
//
class HashTable :
    public Record_<Layout::C_hash_table>
{
    public: static const Int MaxHashCode = (1 << 27) - 1;
    //public: Val   m_vector;           // [1]
    //public: Val   m_test;             // [2]
    //public: Val   m_rehash_size;      // [3]
}; // HashTable

//////////////////////////////////////////////////////////////////////////////
//
// 21 Streams
//

//////////////////////////////////////////////////////////////////////
//
// Stream
//
class Stream :
    public Instance_<Layout::C_stream>
{
    //public: Val   m_flags;        // [2]

    public: enum Flag
    {
        Flag_Probe          = Fixnum::One * 0,
        Flag_Input          = Fixnum::One * 1,
        Flag_Output         = Fixnum::One * 2,
        Flag_Both           = Fixnum::One * 3,
        Flag_Direction_Mask = Fixnum::One * 3,

        Flag_Closed         = Fixnum::One * 4,
        Flag_Interactive    = Fixnum::One * 8,
    }; // Flag

    public: bool IsInputStream() const
        { return 0 != (m_flags->ToInt() & Flag_Input); }

    public: bool IsOutputStream() const
        { return 0 != (m_flags->ToInt() & Flag_Output); }
}; // Stream


//////////////////////////////////////////////////////////////////////
//
// String Output Stream
//
class StringOutputStream :
    public Instance_<Layout::C_string_output_stream>
{
    //public: Val m_string;         // [3]
    //public: Val m_index;          // [4]
    //public: Val m_column;         // [5]

    public: static bool Is_(const Value_ *const val)
    {
        if (! val->Is<Instance>()) return false;
        Val classd = val->Decode<Instance>()->m_classd;
        return CLASSD_string_output_stream == classd;
    } // Is_
}; // StringOutputStream


//////////////////////////////////////////////////////////////////////
//
// Platform Stream
//
class PlatformStream :
    public Instance_<Layout::C_platform_stream>
{
    //public: Val m_blob;               // [1]
    //public: Val m_external_format;    // [2]
}; // PlatformStream


class Charset : public Record_<Layout::C_charset> {};
class ExternalFormat : public Record_<Layout::C_external_format> {};

//////////////////////////////////////////////////////////////////////
//
// File Stream
//
class FileStream : public Instance_<Layout::C_file_stream> {};

//////////////////////////////////////////////////////////////////////////////
//
// 23 Reader
//

//////////////////////////////////////////////////////////////////////
//
// Readtable
//
//  m_case
//    A keyword denotes case sensivity mode: upcase, downcase, preseving
//    or invert.
//
//  m_vector
//    A mapping table for ASCII character to character attributes.
//
//  m_table
//    A mapping table for non-ASCII character to character attributes.
//
//  Character attributes are represented by three form:
//    1. fixnum
//    2. (cons fixnum function)
//    3. (cons fixnum hash-table)
//
class Readtable :
    public Record_<Layout::C_readtable>
{
    //public: Val   m_case;         // [1]
    //public: Val   m_vector;       // [2]
    //public: Val   m_table;        // [3]

    public: enum Type
    {
        Type_Invalid    = 0,
        Type_Cons       = 1,    // constituent
        Type_Nmacro     = 2,    // non-terminating macro character
        Type_Tmacro     = 3,    // terminating macro character
        Type_Space      = 4,    // whitespace
        Type_Sescape    = 5,    // single escape
        Type_Mescape    = 6,    // multiple escape
        Type_Mask       = 7
    }; // Type

    public: enum Trait
    {
        Trait_Invalid     = 0x000000,
        Trait_Alphabetic  = 0x000100,   // bit 8
        Trait_Digit       = 0x000200,   // bit 9
        Trait_Package     = 0x000400,   // bit 10
        Trait_Dot         = 0x000800,   // bit 11
        Trait_Decimal     = 0x001000,   // bit 12
        Trait_Plus        = 0x002000,   // bit 13
        Trait_Minus       = 0x004000,   // bit 14
        Trait_Ratio       = 0x008000,   // bit 15
        Trait_Dmarker     = 0x010000,   // bit 16
        Trait_Emarker     = 0x020000,   // bit 17
        Trait_Fmarker     = 0x040000,   // bit 18
        Trait_Lmarker     = 0x080000,   // bit 19
        Trait_Smarker     = 0x100000,   // bit 20

        Trait_Mask        = 0x1FFF00,

        Trait_Alphadigit  = Trait_Alphabetic | Trait_Digit,
        Trait_Sign        = Trait_Plus | Trait_Minus,
        Trait_FloatMarker = Trait_Dmarker | Trait_Emarker | Trait_Fmarker |
                           Trait_Lmarker | Trait_Smarker,
    }; // Trait
}; // Readtable

//////////////////////////////////////////////////////////////////////////////
//
// 49 Internals
//

//////////////////////////////////////////////////////////////////////
//
// Environment
//
//   variables -- A hash-table
//     A mapping from variable name (symbol) to variable information.
//   functions -- A hash-table
//     A mapping from function name (symbol or value-cell) to function
//     information.
//   qualities -- A list
//   outer     -- A environment or null.
//   types     -- A hash-table
//      A mapping from type name (symbol) to type information.
//   classes   -- A hash-table
//      A mapping from class name (symbol) to class object.
//   others    -- A hash-table
//     Contains mapping to a symbol to a tuple which consists with kind
//     and datum. Where kind is one of following:
//       o declaration         t
//       o structure           class object. Used for typed-structure.
//       o method-combination  method-combination object
//
// Note: For mapping from class name (symbol) to class object, we have
// runtime environment (si::*runtime-environment*) in initial image.
//
class Environment :
    public Record_<Layout::C_environment>
{
    //public: Val m_variables;
    //public: Val m_functions;
    //public: Val m_qualities;
    //public: Val m_outer;
    //public: Val m_types;
    //public: Val m_classes;
    //public: Val m_others;
}; // Environment


//////////////////////////////////////////////////////////////////////
//
// TLV Record
//
// Note: We should have m_index instead of m_offset. Facilities prefer
// index.
//  o Compiler
//      Reclaiming Tlv slot by using index.
//  o FASD
//      Mapping offset to TlvRecord to retrive symbolic Tlv name.
//  o FASL
//      Mapping index to offset for embeding to instruction.
//  o symbol-value
//      Mapping index to offste to access value of TLV.
//
class TlvRecord :
    public Record_<Layout::C_tlv_record>
{
    //public: Val   m_name;    // [1]
    //public: Val   m_index;   // [2]
    //public: Val   m_value;   // [3]
    public: Val GetName()  const { return m_name; }
    public: Val GetIndex() const { return m_index; }
    public: Val GetValue() const { return m_value; }
}; // TlvRecord


//////////////////////////////////////////////////////////////////////
//
// Closed Cell
//
class ClosedCell :
    public Record_<Layout::C_closed_cell>
{
    //public: Val   m_classd;   // [0]
    //public: Val   m_value;    // [1]
}; // ClosedCell


//////////////////////////////////////////////////////////////////////
//
// Value Cell
//
class ValueCell :
    public Record_<Layout::C_value_cell>
{
    //public: Val   m_classd;   // [0]
    //public: Val   m_value;    // [1]
    //public: Val   m_name;     // [2]
    //public: Val   m_type;     // [3]
}; // ValueCell


//////////////////////////////////////////////////////////////////////
//
// Setf Cell
//
class SetfCell :
    public Record_<Layout::C_setf_cell>
{
    //public: Val   m_classd;       // [0]
    //public: Val   m_function;     // [1]
    //public: Val   m_name;         // [2]
}; // SetfCell

//////////////////////////////////////////////////////////////////////////////
//
// 50 Extensions
//

//////////////////////////////////////////////////////////////////////
//
// Mutex
//
// Descrption:
//  The mutex is the simplest synchronization object. Only one thread
//  can lock a mutex. Other threads are suspended until a mutex is
//  unlocked.
//
class Mutex :
    public Record_<Layout::C_mutex>
{
    //public: Val   m_thread;    // [1]
    //public: Val   m_state;     // [2]
    //public: Val m_name;          // [3]
}; // Mutex

//////////////////////////////////////////////////////////////////////
//
// Latch
//
// Description:
//  The latch has two modes exlusive and shared.
//
class Latch :
    public Record_<Layout::C_latch>
{
    //public: Val m_thread;     // [1]
    //public: Val m_state;      // [2]
    //public: Val m_name;       // [3]
    //public: Val m_lock_count; // [4]
    //public: Val m_spinlock;   // [5]

    public: bool IsLocked() const
        { return m_lock_count != Fixnum::Encode(static_cast<Int>(0)); }

    public: void Lock(Thread*, Val);
    public: void Unlock();
}; // Latch

#define VAR(name) VAR_##name->Decode<ValueCell>()->m_value

#define CLASSD_INDEX_OF(mp_classd) ( mp_classd - CLASSD_built_in_class )
#define CLASSD_(mp_name) CLASSD_INDEX_OF(CLASSD_##mp_name)

}; // Kernel

#endif //!defined(INCLUDE_kernel_ke_layout_h)
