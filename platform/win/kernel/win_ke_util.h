//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - pre-compiled header
// kernel_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/kernel/win_ke_util.h#2 $
//
#if !defined(INCLUDE_platform_win_ke_util_h)
#define INCLUDE_platform_win_ke_util_h

//////////////////////////////////////////////////////////////////////
//
// FileHandle
//
class FileHandle
{
    public: HANDLE h;
    public: FileHandle(HANDLE hFile = INVALID_HANDLE_VALUE)
    {
        h = hFile;
    } // FileHandle

    public: ~FileHandle()
    {
        Release();
    } // ~FileHandle

    public: operator HANDLE() const { return h; }

    public: FileHandle& operator =(HANDLE hHandle)
    {
        Attach(hHandle);
        return *this;
    } // operator =

    public: void Attach(HANDLE hHandle)
    {
        Release();
        h  = hHandle;
    } // Attach

    public: HANDLE Detach()
        { HANDLE h1 = h; h = INVALID_HANDLE_VALUE; return h1; }

    public: void Release()
    {
        if (INVALID_HANDLE_VALUE != h)
        {
            ::CloseHandle(h);
            h = INVALID_HANDLE_VALUE;
        }
    } // Release
}; // FileHandle

#endif //!defined(INCLUDE_platform_win_ke_util_h)
