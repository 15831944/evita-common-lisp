#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - initialization
// genesis/gs_init.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_init_21_stream.cpp#4 $
//
// Description:
//  Installs following C implemented lisp function for genesis.
//
#include "./gs_init.h"

namespace Genesis
{

namespace
{
    class Specializer : public Instance_<Layout::C_specializer> {};
} // namespace

// install_method
Val install_method(
    Val fname,
    Val snames,
    int iMin,
    int iMax,
    const char16*   pwsz,
    int iNVals = 1 )
{
    static Val s_names = nil;
    static Val s_specializers = nil;

    Val gf = fdefinition(fname);
        ASSERT(subclassp(class_of(gf), CLASS_standard_generic_function));

    Val mname = list(Qmethod, fname, nil, snames);

    if (! equal(snames, s_names))
    {
        s_names = snames;
        Collector oSpecializer(&s_specializers);
        foreach(EnumList, oEnum, snames)
        {
            oSpecializer.Add(find_class(oEnum.Get()));
        } // for each class-name
    }

    Val mt = MiniThread::Get()->AllocInstance(CLASSD_standard_method);
    StandardMethod* p = mt->Decode<StandardMethod>();
        p->m_plist = nil;
        p->m_generic_function = gf;
        p->m_qualifiers = nil;
        p->m_specializers = s_specializers;
        p->m_lambda_list  = gf->Decode<StandardGenericFunction>()->
            m_param_info->Decode<ParamInfo>()->m_lambda_list;
        p->m_function = make_wrapper(0, mname, iMin, iMax, pwsz, iNVals);

    push(mt, gf->Decode<StandardGenericFunction>()->m_methods);

    foreach (EnumList, oEnum, s_specializers)
    {
        Val specializer = oEnum.Get();
        push(mt, specializer->Decode<Specializer>()->m_direct_methods);
    } // for each class

    return mt;
} // install_method

// Initializer::init_21_Streams
void Initializer::init_21_Streams()
{
    Val specializers_1 = list(Qplatform_stream);
    Val specializers_2 = list(Qplatform_stream, Qt);

    {
        #define install_platform_method_1(mp_name) \
            install_method(Q##mp_name, \
                specializers_1, 1, 1, \
                L"platform_" L## #mp_name )

        install_platform_method_1(stream_clear_input);
        install_platform_method_1(stream_clear_output);
        install_platform_method_1(stream_finish_output);
        install_platform_method_1(stream_force_output);
        install_platform_method_1(stream_fresh_line);
        install_platform_method_1(stream_line_number);
        install_platform_method_1(stream_line_column);
        install_platform_method_1(stream_listen);
        install_platform_method_1(stream_output_width);
        install_platform_method_1(stream_peek_char);
        install_platform_method_1(stream_read_byte);
        install_platform_method_1(stream_read_char);
        install_platform_method_1(stream_read_char_no_hang);
        install_platform_method_1(stream_start_line_p);
    }

    install_method(Qclose, specializers_1,
        1, -1, L"platform_stream_close");

    install_method(Qstream_read_bytes, specializers_2,
        2, 4, L"platform_stream_read_bytes" );

    install_method(Qstream_unread_char, specializers_2,
        2, 2, L"platform_stream_unread_char" );

    install_method(Qstream_write_byte, specializers_2,
        2, 2, L"platform_stream_write_byte" );

    install_method(Qstream_write_bytes, specializers_2,
        2, 2, L"platform_stream_write_bytes" );

    install_method(Qstream_write_char, specializers_2,
        2, 2, L"platform_stream_write_char" );

    install_method(Qstream_write_string, specializers_2,
        2, 4, L"platform_stream_write_string" );

    ////////////////////////////////////////////////////////////
    //
    // realize-instance
    //
    install_method(Qrealize_instance, list(Qfile_stream),
        1, -1, L"realize_instance_file_stream" );

    #if _WIN32
    {
        install_method(Qrealize_instance, list(Qconsole_stream),
            1, -1, L"realize_instance_console_stream" );

        install_method(Qrealize_instance, list(Qdebug_output_stream),
            1, -1, L"realize_instance_debug_output_stream" );
    }
    #endif // _WIN32
} // Initializer::init_21_Streams

} // Genesis
