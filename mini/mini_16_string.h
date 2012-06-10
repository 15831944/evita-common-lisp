//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_16_string.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_16_string.h#3 $
//
#if !defined(INCLUDE_mini_16_string_h)
#define INCLUDE_mini_16_string_h

//warning C4324: 'MiniLisp::StackString_<t_n>' : structure was padded due to __declspec(align())
#pragma warning(disable: 4324)

namespace CommonLisp
{
    bool simple_string_p(Val);
    bool stringp(Val);

    Val make_string(Int);
    Val make_string(LPCWSTR);
    Val make_string(LPCWSTR, size_t);

    Val schar(Val, Val);
    Val schar(Val, Int);
    Val setf_schar(Val, Val, Val);
    Val setf_schar(Val, Val, Int);
} // CommonLisp

namespace MiniLisp
{
    int string_cs_cmp(Val, Val);
    int string_ci_cmp(Val, Val);

    Val hash_string(Val);
    Val allocate_string(Val);
    Val allocate_string(Int);
    Val to_string(Val);

    Val string_data(Val, Val*);

    //////////////////////////////////////////////////////////////////////
    //
    // Dynamic string
    //
    template<int t_n = 100>
    class __declspec(align(Kernel_Arch_BinObj_Align)) StackString_ :
        public SimpleString
    {
        protected: char16 m_rgwchExtra[t_n];

        public: StackString_() {}

        public: StackString_(LPCWSTR pwszString)
        {
            Init(pwszString, ::lstrlenW(pwszString));
        } // StackString_

        public: StackString_(
            LPCWSTR     pwchString,
            size_t      cwchString )
        {
            Init(pwchString, cwchString);
        } // StackString_

        public: StackString_(
            LPCWSTR     pwchString,
            LPCWSTR     pwchEnd )
        {
            Init(pwchString, pwchEnd - pwchString);
        } // StackString_

        public: void Init(LPCWSTR pwszString)
        {
            Init(pwszString, ::lstrlenW(pwszString));
        } // Init

        public: void Init(LPCWSTR pwchString, size_t cwchString)
        {
            ASSERT(cwchString + 1 < lengthof(m_rgwchExtra));

            m_classd = CLASSD_simple_string;
            m_length = Fixnum::Encode(cwchString);

            ::CopyMemory(
                m_rgwchElement,
                pwchString,
                (cwchString + 1) * sizeof(char16) );

            m_rgwchElement[cwchString] = 0;
        } // Init

        public: operator LPWSTR()
        {
            return m_rgwchElement;
        } // LPWSTR

        public: operator Val()
        {
            return Encode();
        } // LPWSTR
    }; // StackString_

    typedef StackString_<256> StackString;
} // MiniLisp

#endif //!defined(INCLUDE_mini_16_string_h)
