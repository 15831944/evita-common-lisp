//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - pre-compiled header
// mini_21_stream.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_21_stream.h#2 $
//
#if !defined(INCLUDE_mini_21_stream_h)
#define INCLUDE_mini_21_stream_h

namespace CommonLisp
{
    bool input_stream_p(Val);
    bool output_stream_p(Val);
    bool streamp(Val);
    bool listen(Val = nil);

    Val close(Val);
    Val get_output_stream_string(Val);
    Val force_output(Val);
    Val make_string_output_stream();
    Val read_char(Val = nil, Val = Qt, Val = nil);
    Val read_char_no_hang(Val = nil, Val = Qt, Val = nil);
    Val unread_char(Val, Val = nil);
    Val write_char(Val, Val = nil);
    Val write_string(Val, Val = nil);

    void write_string(LPCWSTR, size_t, Val = nil);
    void write_char(char16, Val = nil);
    void write_string(LPCWSTR, Val = nil);
} // CommonLisp

namespace MiniLisp
{
    Val ensure_input_stream(Val);
    Val ensure_output_stream(Val);

    Val bind_standard_streams();

    Val stream_line_column(Val);
    Val stream_line_number(Val);

    // platform/{platform}/mini/{platform}_mini_21_stream.cpp
    Val make_file_stream_char_out(Val);
} // MiniLisp

#endif //!defined(INCLUDE_mini_21_stream_h)
