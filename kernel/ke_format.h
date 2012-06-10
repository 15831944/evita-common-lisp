//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - format
// kernel/ke_format.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_format.h#2 $
//
#if !defined(INCLUDE_kernel_format_h)
#define INCLUDE_kernel_format_h

namespace Kernel
{

//////////////////////////////////////////////////////////////////////
//
// Formatter
//
class Formatter
{
    protected: virtual void write_char(char16) = 0;
    protected: virtual void write_string(const char16*) = 0;
    protected: virtual void write_string(const char16*, const char16*) = 0;
    protected: virtual void print_object(Val) = 0;

    public: void Run(const char16*, const char16*, va_list);

    public: void Run(const char16* p, va_list a)
        { Run(p, p + ::lstrlenW(p), a); }


    struct Param
    {
        bool    m_fAt;
        bool    m_fColon;
        int     m_cParams;
        Val     mv_value[5];

        Param() :
            m_fAt(false),
            m_fColon(false),
            m_cParams(0) {}
    }; // Param

    void format_chars(const Param*, char16);
    void format_A(const Param*, Val);
    void format_C(const Param*, Val);
    void format_R(const Param*, Val, int);
    void format_S(const Param*, Val);
}; // Formatter

} // Kernel

#endif //!defined(INCLUDE_kernel_format_h)
