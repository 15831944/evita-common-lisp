//////////////////////////////////////////////////////////////////////////////
//
// evcl - platform - pre-compiled header
// platform/pl_21_stream.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/generic/mini/pl_mini_21_stream.h#4 $
//
#if !defined(INCLUDE_platform_pl_mini_21_stream_h)
#define INCLUDE_platform_pl_mini_21_stream_h

namespace MiniLisp
{

Val make_file_stream(HANDLE, Val, Val);


//////////////////////////////////////////////////////////////////////
//
// Stream Implementation
//
class StreamImpl
{
    public: class PlatformStream : public Kernel::PlatformStream
    {
        friend class StreamImpl;
    };

    protected: StreamImpl() {}
    protected: ~StreamImpl() {}

    public: static StreamImpl* StaticCast(Val stream)
    {
        return reinterpret_cast<StreamImpl*>(
            stream->Decode<PlatformStream>()->m_blob );
    } // StaticCast

    public: virtual void ClearInput(Val s)
        { unsupported(s, Qclear_input); }

    public: virtual void ClearOutput(Val s)
        { unsupported(s, Qclear_output); }

    public: virtual Val  Close(Val s, bool)
        { unsupported(s, Qclose); }

    public: virtual void FinishOutput(Val s)
        { unsupported(s, Qfinish_output); }

    public: virtual void ForceOutput(Val s)
        { unsupported(s, Qforce_output); }

    public: virtual Val  FreshLine(Val);


    public: virtual Val  LineColumn(Val s)
        { unsupported(s, Qstream_line_column); }

    public: virtual Val  LineNumber(Val s)
        { unsupported(s, Qstream_line_number); }

    public: virtual bool Listen(Val s)
        { unsupported(s, Qlisten); }

    public: virtual Val OutputWidth(Val)
        { return nil; }

    public: virtual Val PeekChar(Val);

    public: virtual Val ReadByte(Val s)
        { unsupported(s, Qread_byte); }

    public: virtual Val ReadBytes(Val, uint8*, size_t);

    public: virtual Val ReadChar(Val s)
        { unsupported(s, Qread_char); }

    public: virtual Val ReadCharNoHang(Val);

    public: virtual Val  StartLineP(Val);

    public: virtual void Terpri(Val s)
        { WriteChar(s, 0x0A); }

    public: virtual void UnreadChar(Val s, char16)
        { unsupported(s, Qunread_char); }

    public: virtual void WriteByte(Val stream, uint8 nByte)
    {
        uint8 rgbBuffer[1];
            rgbBuffer[0] = nByte;
        WriteBytes(stream, rgbBuffer, 1);
    } // WriteByte

    public: virtual void WriteChar(Val stream, char16 wchChar)
    {
        char16 rgwchBuffer[1];
            rgwchBuffer[0] = wchChar;
        WriteString(stream, rgwchBuffer, 1);
    } // WriteChar

    public: virtual void WriteBytes(Val s, const uint8*, size_t)
        { unsupported(s, Qwrite_sequence); }

    public: virtual void WriteString(Val s, const char16*, size_t)
        { unsupported(s, Qwrite_string); }

    protected: void __declspec(noreturn) unsupported(Val s, Val op)
    {
        error(Qunsupported_stream_operation,
            Kstream, s,
            Koperation, op );
    } // unsupported
}; // StreamImpl

} // MiniLisp

#endif //!defined(INCLUDE_platform_pl_mini_21_stream_h)
