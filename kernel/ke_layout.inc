//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Class Definition
// kernel/ke_layout.inc
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE"INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/kernel/ke_layout.inc 29 2006-10-23 13:49:11 yosi $
//

#if ! defined(DEFCLASS)
    #define DEFCLASS(mp_cname, mp_NAME, mp_base)
#endif // ! defined(DEFCLASS)

#if ! defined(DEFSUPER)
    #define DEFSUPER(mp_super)
#endif // ! defined(DEFSUPER)

#if ! defined(DEFCLASSINFO)
    #define DEFCLASSINFO(mp_metaclass, mp_format, mp_param)
#endif // ! defined(DEFCLASSINFO)

#if ! defined(DEFCLASSINFO_)
    #define DEFCLASSINFO_(mp_metaclass, mp_format, mp_param, mp_misc)
#endif // ! defined(DEFCLASSINFO_)

#if ! defined(DEFSLOT)
    #define DEFSLOT(mp_n, mp_cname, mp_STR, mp_ty) \
        DEFSLOT_(mp_n, mp_cname, mp_STR, Val, mp_ty)
#endif // ! defined(DEFSLOT)

#if ! defined(DEFSLOT_)
    #define DEFSLOT_(mp_n, mp_cname, mp_STR, mp_ctype, mp_ty)
#endif // ! defined(DEFSLOT_)

#if ! defined(DEFSLOT_OVERRIDE)
    #define DEFSLOT_OVERRIDE(mp_name)
#endif // ! defined(DEFSLOT_OVERRIDE)

#if ! defined(ENDCLASS)
    #define ENDCLASS()
#endif // ! defined(ENDCLASS)

#if ! defined(FINALIZED_CLASS)
    #define FINALIZED_CLASS(mp_name)
#endif // ! defined(FINALIZED_CLASS)

DEFCLASS(t, "T", root)
    DEFSUPER("")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()

DEFCLASS(record, "SI:RECORD", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, None, 0)
    DEFSLOT(0, m_classd, "CLASSD", class_description)
ENDCLASS()


DEFCLASS(data_vector, "SI:DATA-VECTOR", t)
    DEFSUPER("VECTOR SIMPLE-ARRAY")
    DEFCLASSINFO(built_in_class, None, 0)

    DEFSLOT(0, m_classd, "CLASSD", class_description)
    DEFSLOT(1, m_length, "LENGTH", sequence_index)
ENDCLASS()


//////////////////////////////////////////////////////////////////////
//
// class-description
//
//  m_element_type
//    Contains element type of data-vector.
//
DEFCLASS(class_description, "SI:CLASS-DESCRIPTION", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class,
        Fixed, sizeof(Layout::C_class_description))

    FINALIZED_CLASS(class_description)

    DEFSLOT(0, m_classd,        "CLASSD",       class_description)
    DEFSLOT(1, m_class,         "CLASS",        class)
    DEFSLOT(2, m_hash_code,     "HASH-CODE",    fixnum)
    DEFSLOT(3, m_slots,         "SLOTS",        list)
    DEFSLOT(4, m_format,        "FORMAT",       fixnum)
    DEFSLOT(5, m_format_param,  "FORMAT-PARAM", fixnum)
    DEFSLOT(6, m_format_misc,   "FORMAT-MISC",  t)
    DEFSLOT(7, m_tag_code,      "TAG-CODE",     fixnum)
    DEFSLOT(8, m_typespec,      "TYPESPEC",     type_specifier)
    DEFSLOT(9, m_element_type,  "ELEMENT-TYPE", type_specifier)
ENDCLASS()


DEFCLASS(waitable, "WAITABLE", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, None, 0)

    DEFSLOT(0, m_classd,    "CLASSD",   class_description)
    DEFSLOT(1, m_thread,    "THREAD",   or_thread_null)
    DEFSLOT(2, m_state,     "STATE",    keyword)
ENDCLASS()


//////////////////////////////////////////////////////////////////////
//
// 5 Data and Control Flow
//
DEFCLASS(function, "FUNCTION", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()


DEFCLASS(closure, "SI:CLOSURE", function)
    DEFSUPER("FUNCTION")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()


DEFCLASS(compiled_function, "COMPILED-FUNCTION", function)
    DEFSUPER("FUNCTION")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()


// Note: Due to pointer tag scheme, location of m_classd of native-code-object
//       must be second word instead of the first word.
// Note: m_nCookie must be slot[2] for stack walking during GC.
DEFCLASS(native_code_object,
        "NATIVE-CODE-OBJECT", compiled_function )
    DEFSUPER("COMPILED-FUNCTION")
    DEFCLASSINFO(built_in_class, None, 0)

    DEFSLOT_(0, m_cbFunction,   "LENGTH",   size_t, int)
    DEFSLOT(1, m_classd,        "CLASSD",   class_description)
    DEFSLOT_(2, m_nCookie,      "COOKIE",   Int, int)
    DEFSLOT(3, m_frob,          "FROB",     t)
ENDCLASS()


DEFCLASS(native_code_function,
        "SI:NATIVE-CODE-FUNCTION", function )
    DEFSUPER("NATIVE-CODE-OBJECT")
    DEFCLASSINFO(built_in_class, Function, 0)
    FINALIZED_CLASS(native_code_function)

    DEFSLOT_(0, m_cbFunction,   "LENGTH",  size_t, int)
    DEFSLOT(1, m_classd,        "CLASSD", class_description)
    DEFSLOT_(2, m_nCookie,      "COOKIE", Int, int)
    DEFSLOT(3, m_name, "NAME",  or_symbol_cons)
ENDCLASS()


DEFCLASS(native_code_closure,
        "SI:NATIVE-CODE-CLOSURE", native_code_function )
    DEFSUPER("SI:CLOSURE SI:NATIVE-CODE-FUNCTION")
    DEFCLASSINFO(built_in_class, Function, 0)
    FINALIZED_CLASS(native_code_closure)
ENDCLASS()


DEFCLASS(not_function_function,
        "SI:NOT-FUNCTION-FUNCTION", native_code_function)
    DEFSUPER("SI:NATIVE-CODE-CLOSURE")
    DEFCLASSINFO(built_in_class, Function, 0)
    FINALIZED_CLASS(not_function_function)
ENDCLASS()


DEFCLASS(undefined_function_function,
        "SI:UNDEFINED-FUNCTION-FUNCTION", native_code_function )
    DEFSUPER("SI:NATIVE-CODE-CLOSURE")
    DEFCLASSINFO(built_in_class, Function, 0)
    FINALIZED_CLASS(undefined_function_function)
ENDCLASS()


//////////////////////////////////////////////////////////////////////
//
// 7 Objects
//

////////////////////////////////////////////////////////////
//
// Class
//
DEFCLASS(instance, "SI:INSTANCE", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, None, 0)

    DEFSLOT(0, m_classd,    "SI:CLASSD",  class_description)
    DEFSLOT(1, m_storage,   "SI:STORAGE", storage)
ENDCLASS()

DEFCLASS(storage, "SI:STORAGE", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, Storage, 0)

    DEFSLOT(0, m_classd, "CLASSD", class_description)
    DEFSLOT(1, m_storaged,  "STORAGE-DESCRIPTION", class_description)

    FINALIZED_CLASS(storage)
ENDCLASS()

DEFCLASS(metaobject, "CLOS:METAOBJECT", storage)
    DEFSUPER("STANDARD-OBJECT")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()

DEFCLASS(plist_mixin, "PLIST-MIXIN", metaobject)
    DEFSUPER("STANDARD-OBJECT")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT(2, m_plist, "PLIST", list)
ENDCLASS()


DEFCLASS(dependee_mixin, "DEPENDEE-MIXIN", plist_mixin)
    DEFSUPER("PLIST-MIXIN")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()


DEFCLASS(specializer, "CLOS:SPECIALIZER", dependee_mixin)
    DEFSUPER("CLOS:METAOBJECT DEPENDEE-MIXIN")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT(3, m_flags,             "FLAGS", fixnum)
    DEFSLOT(4, m_direct_methods,    "DIRECT-METHODS", list)
ENDCLASS()


DEFCLASS(eql_specializer, "CLOS:EQL-SPECIALIZER", specializer)
    DEFSUPER("CLOS:SPECIALIZER")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_eql_specializer))

    DEFSLOT(5, m_object, "OBJECT", t)
ENDCLASS()

DEFCLASS(class, "CLASS", specializer)
    DEFSUPER("CLOS:SPECIALIZER")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT( 5, m_name,                     "NAME", symbol)
    DEFSLOT( 6, m_direct_superclasses,      "DIRECT-SUPERCLASSES", list)
    DEFSLOT( 7, m_direct_subclasses,        "DIRECT-SUBCLASSES", list)
    DEFSLOT( 8, m_class_precedence_list,    "CLASS-PRECEDENCE-LIST", list)
    DEFSLOT( 9, m_instanced,                "INSTANCE-DESCRIPTION", or_class_description_null)
    DEFSLOT(10, m_direct_slots,             "DIRECT-SLOTS", list)
    DEFSLOT(11, m_slots,                    "SLOTS", list)
    DEFSLOT(12, m_prototype,                "PROTOTYPE", t)
ENDCLASS()


DEFCLASS(built_in_class, "BUILT-IN-CLASS", class)
    DEFSUPER("CLASS")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_built_in_class))
    FINALIZED_CLASS(built_in_class)
ENDCLASS()

DEFCLASS(standard_base_class, "CLOS:STANDARD-BASE-CLASS", class)
    DEFSUPER("CLASS")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()


DEFCLASS(standard_class, "STANDARD-CLASS", standard_base_class)
    DEFSUPER("CLOS:STANDARD-BASE-CLASS")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_standard_class))
    FINALIZED_CLASS(standard_class)
ENDCLASS()


DEFCLASS(standard_object, "STANDARD-OBJECT", instance)
    // Note: funcallable-standard-object is subclass of standard-object,
    // so if we use TYPEP with INSTANCE, we must check FUNCALLABLE-INSTANCE
    // before checking INSTANCE.
    //DEFSUPER("SI:INSTANCE")
    // Note: o10-defclass.lisp contains (defclass standard-object (t) ())
    DEFSUPER("T")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()


DEFCLASS(forward_referenced_class,
        "CLOS:FORWARD-REFERENCED-CLASS", class )
    DEFSUPER("CLASS")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_forward_referenced_class) )
    FINALIZED_CLASS(forward_referenced_class)
ENDCLASS()


////////////////////////////////////////////////////////////
//
// Generic Function
//

DEFCLASS(funcallable_standard_class,
        "CLOS:FUNCALLABLE-STANDARD-CLASS", standard_base_class )
    DEFSUPER("STANDARD-BASE-CLASS")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_funcallable_standard_class) )
    FINALIZED_CLASS(funcallable_standard_class)
ENDCLASS()


////////////////////////////////////////////////////////////
//
// Funcallable Standard Object
//
DEFCLASS(funcallable_instance,
        "SI:FUNCALLABLE-INSTANCE", function )
    DEFSUPER("NATIVE-CODE-OBJECT")
    DEFCLASSINFO(built_in_class, None, 0)

    DEFSLOT_(0, m_cbFunction,   "LENGTH",       size_t, int)
    DEFSLOT(1, m_classd,        "SI:CLASSD",    class_description)
    DEFSLOT_(2, m_nCookie,      "COOKIE",       Int, int)
    DEFSLOT(3, m_storage,       "SI:STORAGE",   storage)
ENDCLASS()


DEFCLASS(funcallable_standard_object,
        "CLOS:FUNCALLABLE-STANDARD-OBJECT", storage)
    DEFSUPER("STANDARD-OBJECT FUNCALLABLE-INSTANCE")
    DEFCLASSINFO(funcallable_standard_class, None, 0)
ENDCLASS()


DEFCLASS(generic_function,
        "GENERIC-FUNCTION", dependee_mixin )

    DEFSUPER("CLOS:METAOBJECT CLOS:FUNCALLABLE-STANDARD-OBJECT DEPENDEE-MIXIN")

    DEFCLASSINFO(funcallable_standard_class, None, 0)

    //DEFSLOT(0, m_plist,       "PLIST",      list)
    DEFSLOT(1, m_methods,       "METHODS",    list)
    DEFSLOT(2, m_name,          "NAME",       function_name)
    DEFSLOT(3, m_method_class,  "METHOD-CLASS",   class)
    DEFSLOT(4, m_method_combination,
        "METHOD-COMBINATION", method_combination )
    DEFSLOT(5, m_param_info, "PARAM-INFO", or_param_info_null)
ENDCLASS()

DEFCLASS(standard_generic_function,
        "STANDARD-GENERIC-FUNCTION", generic_function )
    DEFSUPER("GENERIC-FUNCTION")

    DEFCLASSINFO(funcallable_standard_class,
        FuncallableInstance,
        sizeof(Layout::C_standard_generic_function) )

    FINALIZED_CLASS(standard_generic_function)
ENDCLASS()


////////////////////////////////////////////////////////////
//
// Method
//
DEFCLASS(method, "METHOD", metaobject)
    DEFSUPER("CLOS:METAOBJECT")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()


DEFCLASS(standard_method, "STANDARD-METHOD", plist_mixin)
    DEFSUPER("METHOD PLIST-MIXIN")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_standard_method))
    FINALIZED_CLASS(standard_method)

    DEFSLOT(1, m_generic_function,  "GENERIC-FUNCTION", or_generic_function_null)
    DEFSLOT(2, m_specializers,      "SPECIALIZERS", list)
    DEFSLOT(3, m_qualifiers,        "QUALIFIERS", list)
    DEFSLOT(4, m_lambda_list,       "LAMBDA-LIST", list)
    DEFSLOT(5, m_function,          "FUNCTION", function)
ENDCLASS()


DEFCLASS(standard_accessor_method,
            "CLOS:STANDARD-ACCESSOR-METHOD", standard_method )
    DEFSUPER("STANDARD-METHOD")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_standard_accessor_method) )

    DEFSLOT(9, m_slot_definition,   "SLOT-DEFINITION", slot_definition)
ENDCLASS()


DEFCLASS(standard_reader_method,
            "CLOS:STANDARD-READER-METHOD", standard_accessor_method )
    DEFSUPER("CLOS:STANDARD-ACCESSOR-METHOD")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_standard_reader_method))
ENDCLASS()


DEFCLASS(standard_writer_method, "CLOS:STANDARD-WRITER-METHOD", standard_accessor_method)
    DEFSUPER("CLOS:STANDARD-ACCESSOR-METHOD")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_standard_writer_method))
ENDCLASS()


DEFCLASS(slot_definition, "CLOS:SLOT-DEFINITION", metaobject)
    DEFSUPER("CLOS:METAOBJECT")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT(2, m_name,          "NAME", symbol)
    DEFSLOT(3, m_allocation,    "ALLOCATION", member_Kinstance_Kclass)
    DEFSLOT(4, m_type,          "TYPE", t)
    DEFSLOT(5, m_initargs,      "INITARGS", list)
    DEFSLOT(6, m_initform,      "INITFORM", t)
    DEFSLOT(7, m_initfunction,  "INITFUNCTION", or_function_null)
ENDCLASS()


DEFCLASS(direct_slot_definition,
            "CLOS:DIRECT-SLOT-DEFINITION", slot_definition )
    DEFSUPER("CLOS:SLOT-DEFINITION")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT(8, m_readers, "READERS", list)
    DEFSLOT(9, m_writers, "WRITERS", list)
ENDCLASS()


DEFCLASS(effective_slot_definition,
        "CLOS:EFFECTIVE-SLOT-DEFINITION", slot_definition )
    DEFSUPER("CLOS:SLOT-DEFINITION")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT(8, m_location, "LOCATION", or_sequence_index_cons)
ENDCLASS()


DEFCLASS(standard_slot_definition,
            "CLOS:STANDARD-SLOT-DEFINITION", slot_definition )
    DEFSUPER("CLOS:SLOT-DEFINITION")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()


DEFCLASS(standard_direct_slot_definition,
    "CLOS:STANDARD-DIRECT-SLOT-DEFINITION",
    direct_slot_definition )

    DEFSUPER("CLOS:STANDARD-SLOT-DEFINITION CLOS:DIRECT-SLOT-DEFINITION")

    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_standard_direct_slot_definition) )

    FINALIZED_CLASS(standard_direct_slot_definition)
ENDCLASS()


DEFCLASS(standard_effective_slot_definition,
    "CLOS:STANDARD-EFFECTIVE-SLOT-DEFINITION",
    effective_slot_definition )

    DEFSUPER("CLOS:STANDARD-SLOT-DEFINITION CLOS:EFFECTIVE-SLOT-DEFINITION")

    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_standard_effective_slot_definition) )

    FINALIZED_CLASS(standard_effective_slot_definition)
ENDCLASS()


////////////////////////////////////////////////////////////
//
// Method Comobination
//
DEFCLASS(method_combination, "METHOD-COMBINATION", metaobject)
    DEFSUPER("CLOS:METAOBJECT")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()


DEFCLASS(standard_method_combination,
            "CLOS:STANDARD-METHOD-COMBINATION", method_combination)
    DEFSUPER("METHOD-COMBINATION")

    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_standard_method_combination) )

    FINALIZED_CLASS(standard_method_combination)

    DEFSLOT(2, m_type,      "TYPE", t)
    DEFSLOT(3, m_options,   "OPTIONS", list)
ENDCLASS()


DEFCLASS(long_method_combination,
        "CLOS:LONG-METHOD-COMBINATION", standard_method_combination )
    DEFSUPER("CLOS:STANDARD-METHOD-COMBINATION")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_long_method_combination))
    FINALIZED_CLASS(long_method_combination)
ENDCLASS()


DEFCLASS(short_method_combination,
        "CLOS:SHORT-METHOD-COMBINATION", standard_method_combination )
    DEFSUPER("CLOS:STANDARD-METHOD-COMBINATION")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_short_method_combination) )

    FINALIZED_CLASS(short_method_combination)

    DEFSLOT(4, m_operator,      "OPERATOR", symbol)
    DEFSLOT(5, m_identity_with_one_argument, "IDENTITY-WITH-ONE-ARGUMENT", t)
ENDCLASS()


//////////////////////////////////////////////////////////////////////
//
// 08 Structures
//
DEFCLASS(structure_class, "STRUCTURE-CLASS", class)
    DEFSUPER("CLASS")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_structure_class))
    FINALIZED_CLASS(structure_class)
ENDCLASS()

DEFCLASS(structure_object, "STRUCTURE-OBJECT", record)
    DEFSUPER("T")
    DEFCLASSINFO(
        structure_class,
        Structure,
        sizeof(Layout::C_structure_object) )

    FINALIZED_CLASS(structure_object)
ENDCLASS()

DEFCLASS(structure_slot_definition,
        "STRUCTURE-SLOT-DEFINITION", slot_definition)
    DEFSUPER("SLOT-DEFINITION")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()


DEFCLASS(structure_direct_slot_definition,
        "STRUCTURE-DIRECT-SLOT-DEFINITION", direct_slot_definition )
    DEFSUPER("STRUCTURE-SLOT-DEFINITION DIRECT-SLOT-DEFINITION")

    DEFCLASSINFO(standard_class,
        Instance,
        sizeof(Layout::C_structure_direct_slot_definition) )

    FINALIZED_CLASS(structure_direct_slot_definition)

    DEFSLOT(8, m_read_only, "READ-ONLY", t)
ENDCLASS()


DEFCLASS(structure_effective_slot_definition,
        "STRUCTURE-EFFECTIVE-SLOT-DEFINITION", effective_slot_definition)
    DEFSUPER("STRUCTURE-SLOT-DEFINITION EFFECTIVE-SLOT-DEFINITION")

    DEFCLASSINFO(standard_class,
        Instance,
        sizeof(Layout::C_structure_effective_slot_definition) )

    FINALIZED_CLASS(structure_effective_slot_definition)
ENDCLASS()


DEFCLASS(structure_type, "SI:STRUCTURE-TYPE", structure_class)
    DEFSUPER("STRUCTURE-CLASS")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT(13, m_initial_offset,    "INITIAL-OFFSET", fixnum)
    DEFSLOT(14, m_named,             "NAMED", symbol)
    DEFSLOT(15, m_type,              "TYPE", t)
ENDCLASS()


DEFCLASS(list_structure_type, "SI:LIST-STRUCTURE-TYPE", structure_type)
    DEFSUPER("SI:STRUCTURE-TYPE")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_list_structure_type))
    FINALIZED_CLASS(list_structure_type)
ENDCLASS()


DEFCLASS(vector_structure_type, "SI:VECTOR-STRUCTURE-TYPE", structure_type)
    DEFSUPER("SI:STRUCTURE-TYPE")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_vector_structure_type))
ENDCLASS()


DEFCLASS(param_info, "PARAM-INFO", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class,
        Structure, sizeof(Layout::C_param_info) )

    DEFSLOT(1, m_nreqs,         "NREQS", fixnum)
    DEFSLOT(2, m_nopts,         "NOPTS", fixnum)
    DEFSLOT(3, m_keys,          "KEYS",  or_list_eql_t)
    DEFSLOT(4, m_order,         "ORDER", list)
    DEFSLOT(5, m_lambda_list,   "LAMBDA-LIST",    list)
ENDCLASS()

//////////////////////////////////////////////////////////////////////
//
// 09 Conditions
//

DEFCLASS(restart, "RESTART", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class,
        Structure, sizeof(Layout::C_restart) )

    DEFSLOT(1, m_name,                  "NAME", symbol)
    DEFSLOT(2, m_function,              "FUNCTION", function)
    DEFSLOT(3, m_test_function,         "TEST-FUNCTION", or_function_null)
    DEFSLOT(4, m_report_function,       "REPORT-FUNCTION", or_function_null)
    DEFSLOT(5, m_interactive_function,  "INTERACTIVE-FUNCTION", or_function_null)
ENDCLASS()


DEFCLASS(condition, "CONDITION", standard_object)
    DEFSUPER("STANDARD-OBJECT")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()

DEFCLASS(serious_condition, "SERIOUS-CONDITION", condition)
    DEFSUPER("CONDITION")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()

DEFCLASS(error, "ERROR", serious_condition)
    DEFSUPER("SERIOUS-CONDITION")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()

DEFCLASS(simple_condition, "SIMPLE-CONDITION", condition)
    DEFSUPER("CONDITION")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT(2, m_format_control,    "FORMAT-CONTROL", format_control)
    DEFSLOT(3, m_format_arguments,  "FORMAT-ARGUMENTS", list)
ENDCLASS()

DEFCLASS(cell_error, "CELL-ERROR", error)
    DEFSUPER("ERROR")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT(2, m_name,  "NAME", symbol)
ENDCLASS()

DEFCLASS(control_error, "CONTROL-ERROR", error)
    DEFSUPER("ERROR")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()

DEFCLASS(parse_error, "PARSE-ERROR", error)
    DEFSUPER("ERROR")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()

DEFCLASS(program_error, "PROGRAM-ERROR", error)
    DEFSUPER("ERROR")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()

DEFCLASS(simple_error, "SIMPLE-ERROR", simple_condition)
    DEFSUPER("SIMPLE-CONDITION ERROR")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_simple_error))
ENDCLASS()

DEFCLASS(type_error, "TYPE-ERROR", error)
    DEFSUPER("ERROR")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_type_error))

    DEFSLOT(0, m_expected_type, "EXPECTED-TYPE", t)
    DEFSLOT(1, m_datum,         "DATUM", t)
ENDCLASS()


DEFCLASS(unbound_variable, "UNBOUND-VARIABLE", cell_error)
    DEFSUPER("CELL-ERROR")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_unbound_variable))
ENDCLASS()


DEFCLASS(undefined_function, "UNDEFINED-FUNCTION", cell_error)
    DEFSUPER("CELL-ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_undefined_function) )
ENDCLASS()


DEFCLASS(alter_constant_symbol,
            "SI:ALTER-CONSTANT-SYMBOL", cell_error )
    DEFSUPER("CELL-ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_alter_constant_symbol) )
ENDCLASS()


DEFCLASS(case_failure,
            "SI:CASE-FAILURE", type_error )
    DEFSUPER("TYPE-ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_case_failure) )

    DEFSLOT(2, m_name,          "NAME",             symbol)
    DEFSLOT(3, m_possibilities, "POSSIBILITIES",    list)
ENDCLASS()


DEFCLASS(catch_tag_not_seen,
            "SI:CATCH-TAG-NOT-SEEN", control_error )
    DEFSUPER("CONTROL-ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_catch_tag_not_seen) )

    DEFSLOT(2, m_tag,   "TAG", t)
ENDCLASS()


DEFCLASS(class_not_found,
            "SI:CLASS-NOT-FOUND", error )
    DEFSUPER("ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_class_not_found) )

    DEFSLOT(0, m_name, "NAME", symbol)
ENDCLASS()


DEFCLASS(destructuring_mismatch,
            "SI:DESTRUCTURING-MISMATCH", error )
    DEFSUPER("ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_destructuring_mismatch) )

    DEFSLOT(0, m_expected_form, "EXPECTED-FORM", list)
    DEFSLOT(1, m_form, "FORM", t)
    DEFSLOT(2, m_position, "POSITION", t)
ENDCLASS()


DEFCLASS(funcall_error, "SI:FUNCALL-ERROR", control_error)
    DEFSUPER("CONTROL-ERROR")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT(2, m_function,  "FUNCTION", function)
    DEFSLOT(3, m_arguments, "ARGUMENTS", list)
ENDCLASS()

DEFCLASS(not_function,
            "SI:NOT-FUNCTION", error )
    DEFSUPER("ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_not_function) )

    DEFSLOT(0, m_name, "NAME", function_name)
ENDCLASS()

DEFCLASS(too_few_arguments,
            "SI:TOO-FEW-ARGUMENTS", funcall_error )
    DEFSUPER("FUNCALL-ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_too_few_arguments) )
ENDCLASS()


DEFCLASS(too_many_arguments,
            "SI:TOO-MANY-ARGUMENTS", funcall_error )
    DEFSUPER("FUNCALL-ERROR")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_too_many_arguments))
ENDCLASS()


DEFCLASS(index_error, "SI:INDEX-ERROR", type_error)
    DEFSUPER("TYPE-ERROR")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()


DEFCLASS(bounding_index_error, "SI:BOUNDING-INDEX-ERROR", index_error)
    DEFSUPER("SI:INDEX-ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_bounding_index_error) )

    DEFSLOT(3, m_sequence,  "SEQUENCE", sequence)
    DEFSLOT(4, m_start,     "START",    sequence_index)
ENDCLASS()


DEFCLASS(sequence_index_error, "SI:SEQUENCE-INDEX-ERROR", index_error)
    DEFSUPER("SI:INDEX-ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_sequence_index_error) )

    DEFSLOT(3, m_sequence,  "SEQUENCE", sequence)
ENDCLASS()


DEFCLASS(vector_index_error, "SI:VECTOR-INDEX-ERROR", index_error)
    DEFSUPER("SI:INDEX-ERROR")
    DEFCLASSINFO(standard_class,
            Instance, sizeof(Layout::C_vector_index_error) )

    DEFSLOT(3, m_vector,    "VECTOR", vector)
ENDCLASS()

DEFCLASS(uninitialized_funcallable_instance,
        "SI::UNINITIALIZED-FUNCALLABLE-INSTANCE", funcall_error)
    DEFSUPER("FUNCALL-ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_uninitialized_funcallable_instance) )
ENDCLASS()


DEFCLASS(unrecognized_keyword_argument,
        "SI:UNRECOGNIZED-KEYWORD-ARGUMENT", program_error )
    DEFSUPER("PROGRAM-ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_unrecognized_keyword_argument) )

    DEFSLOT(0, m_keys,  "KEYS", sequence)
    DEFSLOT(1, m_key,   "KEY",  symbol)
ENDCLASS()


DEFCLASS(invalid_keyword_argument,
        "SI:INVALID-KEYWORD-ARGUMENT", program_error )
    DEFSUPER("PROGRAM-ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_invalid_keyword_argument) )
ENDCLASS()


DEFCLASS(odd_number_of_keyword_arguments,
        "SI:ODD-NUMBER-OF-KEYWORD-ARGUMENTS", program_error )
    DEFSUPER("PROGRAM-ERROR")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_odd_number_of_keyword_arguments) )

    DEFSLOT(0, m_arguments,  "ARGUMENTS", list)
ENDCLASS()


//////////////////////////////////////////////////////////////////////
//
// 10 Symbols
//
DEFCLASS(symbol, "SYMBOL", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, Fixed, sizeof(Layout::C_symbol))
    FINALIZED_CLASS(symbol)

    DEFSLOT(0, m_classd,    "CLASSD",       class_description)
    DEFSLOT(1, m_hash_code, "HASH-CODE",    fixnum)
    DEFSLOT(2, m_name,      "NAME",         simple_string)
    DEFSLOT(3, m_package,   "PACKAGE",      or_package_null)
    DEFSLOT(4, m_function,  "FUNCTION",     or_function_null)
    DEFSLOT(5, m_plist,     "PLIST",        list)
ENDCLASS()


//////////////////////////////////////////////////////////////////////
//
// 11 Packages
//
DEFCLASS(package, "PACKAGE", waitable)
    DEFSUPER("WAITABLE")
    DEFCLASSINFO(built_in_class, Fixed, sizeof(Layout::C_package))
    FINALIZED_CLASS(package)

    DEFSLOT(3, m_internal_table,    "INTERNAL-TABLE", simple_vector)
    DEFSLOT(4, m_external_table,    "EXTERNAL-TABLE", simple_vector)
    DEFSLOT(5, m_names,             "NAMES", list)
    DEFSLOT(6, m_use_list,          "USE-LIST", list)
    DEFSLOT(7, m_used_by_list,      "USED-BY-LIST", list)
    DEFSLOT(8, m_shadowing_symbols, "SHADOWING-SYMBOLS", list)
    DEFSLOT(9, m_protect,           "PROTECT", t)
ENDCLASS()


//////////////////////////////////////////////////////////////////////
//
// 12 Numbers
//
// Order of number classd is important!
//    fixnum                    None    0
//    bignum                    Vector  sizeof(Bignum::Elt)
//    ratio                     Fixed   sizeof(Ratio)
//    double_float              Fixed   sizeof(DoubleFloat)
//    single_float              Fixed   sizeof(SingleFloat)
//    si_double_float_complex   Fixed   sizeof(DoubleFloatComplex)
//    si_rational_complex       Fixed   sizeof(RationalComplex)
//    si_single_float_complex   Fixed   sizeof(SingleFloatComplex)
//
//    integer
//    rational
//    flot
//    real
//    complex

DEFCLASS(fixnum, "FIXNUM", t)
    DEFSUPER("INTEGER")

    DEFCLASSINFO_(built_in_class,
        Immediate, Fixnum::Bits, Fixnum::Encode(Fixnum::Tag) )
ENDCLASS()

DEFCLASS(bignum, "BIGNUM", t)
    DEFSUPER("INTEGER")

    DEFCLASSINFO_(built_in_class,
        BinVec, sizeof(Bignum::Bigit) * 8, Qunsigned_byte )

    FINALIZED_CLASS(bignum)

    DEFSLOT(0, m_classd, "CLASSD", class_description)
    DEFSLOT(1, m_length,      "LENGTH", fixnum)
    DEFSLOT_(2, m_rgBigit[2], "",  Arch::Bigit, t)
ENDCLASS()


DEFCLASS(ratio, "RATIO", t)
    DEFSUPER("RATIONAL")
    DEFCLASSINFO(built_in_class, Fixed, sizeof(Layout::C_ratio))
    FINALIZED_CLASS(ratio)

    DEFSLOT(0, m_classd, "CLASSD", class_description)
    DEFSLOT(1, m_pad1,  "PAD1", fixnum)
    DEFSLOT(2, m_num,   "NUMERATOR", integer)
    DEFSLOT(3, m_den,   "DENOMINATOR", integer)
ENDCLASS()


DEFCLASS(double_float, "DOUBLE-FLOAT", t)
    DEFSUPER("FLOAT")
    DEFCLASSINFO(built_in_class,
        BinFixed, sizeof(Layout::C_double_float) )
    FINALIZED_CLASS(double_float)

    DEFSLOT(0, m_classd, "CLASSD", class_description)
    DEFSLOT_(1, m_dbl, "", float64, float64)
ENDCLASS()


DEFCLASS(single_float, "SINGLE-FLOAT", t)
    DEFSUPER("FLOAT")
    DEFCLASSINFO(built_in_class,
        BinFixed, sizeof(Layout::C_single_float) )
    FINALIZED_CLASS(single_float)

    DEFSLOT(0, m_classd, "CLASSD", class_description)
    DEFSLOT_(1, m_flt, "", float32, float32)
ENDCLASS()

DEFCLASS(double_float_complex, "DOUBLE-FLOAT-COMPLEX", t)
    DEFSUPER("COMPLEX")
    DEFCLASSINFO(built_in_class,
        BinFixed, sizeof(Layout::C_double_float_complex) )
    FINALIZED_CLASS(double_float_complex)

    DEFSLOT(0, m_classd, "CLASSD", class_description)
    DEFSLOT(1, m_pad1, "PAD1", fixnum)
    DEFSLOT_(2, m_dblReal, "REALPART", float64, float64)
    DEFSLOT_(3, m_dblImag, "IMAGPART", float64, float64)
ENDCLASS()


DEFCLASS(rational_complex, "RATIONAL-COMPLEX", t)
    DEFSUPER("COMPLEX")
    DEFCLASSINFO(built_in_class, Fixed, sizeof(Layout::C_rational_complex))
    FINALIZED_CLASS(rational_complex)

    DEFSLOT(0, m_classd, "CLASSD",  class_description)
    DEFSLOT(1, m_pad1, "PAD1",      fixnum)
    DEFSLOT(2, m_real, "REALPART",  integer)
    DEFSLOT(3, m_imag, "IMAGPART",  integer)
ENDCLASS()

DEFCLASS(single_float_complex, "SINGLE-FLOAT-COMPLEX", t)
    DEFSUPER("COMPLEX")
    DEFCLASSINFO(built_in_class,
        BinFixed, sizeof(Layout::C_single_float_complex) )
    FINALIZED_CLASS(single_float_complex)

    DEFSLOT(0, m_classd, "CLASSD",      class_description)
    DEFSLOT(1, m_pad1, "PAD1", fixnum)
    DEFSLOT_(2, m_fltReal, "REALPART", float32, float32)
    DEFSLOT_(3, m_fltImag, "IMAGPART", float32, float32)
ENDCLASS()


DEFCLASS(integer, "INTEGER", t)
    DEFSUPER("RATIONAL")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()

DEFCLASS(rational, "RATIONAL", t)
    DEFSUPER("REAL")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()

DEFCLASS(float, "FLOAT", t)
    DEFSUPER("REAL")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()

DEFCLASS(real, "REAL", t)
    DEFSUPER("NUMBER")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()

DEFCLASS(complex, "COMPLEX", t)
    DEFSUPER("NUMBER")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()

DEFCLASS(number, "NUMBER", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()


//////////////////////////////////////////////////////////////////////////////
//
// 13 Characters
//
DEFCLASS(character, "CHARACTER", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, BinFixed, sizeof(Layout::C_character))
    FINALIZED_CLASS(character)
    DEFSLOT(0, m_classd,    "CLASSD",   class_description)
    DEFSLOT(1, m_data,      "DATA",     fixnum)
ENDCLASS()


//////////////////////////////////////////////////////////////////////////////
//
// 14 Conses
//
DEFCLASS(cons, "CONS", t)
    DEFSUPER("LIST")
    DEFCLASSINFO(built_in_class, Cons, sizeof(Layout::C_cons))

    DEFSLOT(1, m_car, "CAR", t)
    DEFSLOT(2, m_cdr, "CDR", t)
ENDCLASS()


DEFCLASS(list, "LIST", t)
    DEFSUPER("SEQUENCE")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()

DEFCLASS(null, "NULL", t)
    DEFSUPER("SYMBOL LIST")
    DEFCLASSINFO_(built_in_class,
        Fixed, sizeof(Symbol), Fixnum::Encode(Null::Tag) )
ENDCLASS()


//////////////////////////////////////////////////////////////////////////////
//
// 15 Arrays
//
// Order of array classd is important!
//    simple_array,  Vector, Fixnum::One * sizeof(Val) * 8
//    simple_vector, Vector, Fixnum::One * sizeof(Val) * 8
//    simple_bit_vector, BinVec, Fixnum::One * 1)
//    simple_string,     String, Fixnum::One * sizeof(char16) * 8
//
//    si_signed_byte_8_vector, BinVec, Fixnum::One * 8
//    si_signed_byte_16_vector, BinVec, Fixnum::One * 16
//    si_signed_byte_32_vector, BinVec, Fixnum::One * 32
//    si_signed_byte_64_vector, BinVec, Fixnum::One * 64
//
//    si_unsigned_byte_8_vector, BinVec, Fixnum::One * 8
//    si_unsigned_byte_16_vector, BinVec, Fixnum::One * 16
//    si_unsigned_byte_32_vector, BinVec, Fixnum::One * 32
//    si_unsigned_byte_64_vector, BinVec, Fixnum::One * 64
//
//    si_single_float_vector, BinVec, Fixnum::One * 32
//    si_single_float_complex_vector, BinVec, Fixnum::One * 64
//    si_double_float_vector, BinVec, Fixnum::One * 64
//    si_double_float_complex_vector, BinVec, Fixnum::One * 128
//
//    bit_vector, Fixed, sizeof(VectorObject))
//    string, Fixed, sizeof(VectorObject))
//    vector, Fixed, sizeof(VectorObject))
//    array,  Fixed, sizeof(Array))
//
// Note: If you add new vector class, you MUST update CLASSD_vector_min
// and CLASSD_vector_max, defined in kernel/ke_layout.h
//
DEFCLASS(array, "ARRAY", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()

DEFCLASS(simple_array, "SIMPLE-ARRAY", t)
    DEFSUPER("ARRAY")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()

DEFCLASS(vector, "VECTOR", t)
    DEFSUPER("ARRAY SEQUENCE")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()

DEFCLASS(bit_vector, "BIT-VECTOR", t)
    DEFSUPER("VECTOR")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()

DEFCLASS(string, "STRING", t)
    DEFSUPER("VECTOR")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()

DEFCLASS(array_object, "ARRAY-OBJECT", t)
    DEFSUPER("ARRAY")
    DEFCLASSINFO(built_in_class, None, 0)

    DEFSLOT(0, m_classd,        "CLASSD",       class_description)
    DEFSLOT(1, m_rank,          "RANK",         array_rank)
    DEFSLOT(2, m_flags,         "FLAGS",        fixnum)
    DEFSLOT(3, m_displaced_to,  "DISPLACED-TO", array)
    DEFSLOT(4, m_offset,        "OFFSET",       sequence_index)
    DEFSLOT(5, m_total_size,    "TOTAL-SIZE",   sequence_index)
    DEFSLOT(6, mv_dimension[2], "DIMENSIONS",   sequence_index)
ENDCLASS()

DEFCLASS(vector_object, "VECTOR-OBJECT", t)
    DEFSUPER("VECTOR")
    DEFCLASSINFO(built_in_class, None, 0)

    DEFSLOT(0, m_classd,        "CLASSD",       class_description)
    DEFSLOT(1, m_fill_pointer,  "FILL-POINTER", sequence_index)
    DEFSLOT(2, m_flags,         "FLAGS",        fixnum)
    DEFSLOT(3, m_displaced_to,  "DISPLACED-TO", array)
    DEFSLOT(4, m_offset,        "OFFSET",       sequence_index)
    DEFSLOT(5, m_total_size,    "TOTAL-SIZE",   sequence_index)
ENDCLASS()


////////////////////////////////////////////////////////////
//
// Array Family
//
DEFCLASS(
        bit_array_object,
        "BIT-ARRAY-OBJECT",
        array_object )
    DEFSUPER("ARRAY-OBJECT")
    DEFCLASSINFO(built_in_class, Array, 0)
    FINALIZED_CLASS(bit_array_object)
ENDCLASS()

DEFCLASS(
        general_array_object,
        "GENERAL-ARRAY-OBJECT",
        array_object )
    DEFSUPER("ARRAY-OBJECT")
    DEFCLASSINFO(built_in_class, Array, 0)
    FINALIZED_CLASS(general_array_object)
ENDCLASS()

DEFCLASS(
        simple_bit_array_object,
        "SIMPLE-BIT-ARRAY-OBJECT",
        array_object )
    DEFSUPER("ARRAY-OBJECT SIMPLE-ARRAY")
    DEFCLASSINFO(built_in_class, Array, 0)
    FINALIZED_CLASS(simple_bit_array_object)
ENDCLASS()

DEFCLASS(
        simple_general_array_object,
        "SIMPLE-GENERAL-ARRAY-OBJECT",
        array_object )
    DEFSUPER("ARRAY-OBJECT SIMPLE-ARRAY")
    DEFCLASSINFO(built_in_class, Array, 0)
    FINALIZED_CLASS(simple_general_array_object)
ENDCLASS()


////////////////////////////////////////////////////////////
//
// Vector Family
//
DEFCLASS(simple_bit_vector, "SIMPLE-BIT-VECTOR", t)
    DEFSUPER("BIT-VECTOR DATA-VECTOR")
    DEFCLASSINFO_(built_in_class, BinVec, 1, Qbit)
    FINALIZED_CLASS(simple_bit_vector)

    DEFSLOT(0, m_classd, "CLASSD", class_description)
    DEFSLOT(1, m_length, "LENGTH", sequence_index)
    DEFSLOT_(2, m_rgfElement[2], "", Arch::BitEltT, uint)
ENDCLASS()

DEFCLASS(simple_string, "SIMPLE-STRING", t)
    DEFSUPER("STRING DATA-VECTOR")
    DEFCLASSINFO_(built_in_class, String, sizeof(char16) * 8, Qcharacter)
    FINALIZED_CLASS(simple_string)

    DEFSLOT(0,  m_classd, "CLASSD", class_description)
    DEFSLOT(1,  m_length, "LENGTH", sequence_index)
    DEFSLOT_(2, m_rgwchElement[sizeof(Val) * 2 / sizeof(char16)],
        "", char16, uint16 )
ENDCLASS()

#define DEF_VECTOR_CLASS(mp_name, mp_NAME, mp_size, mp_elt) \
    DEFCLASS(##mp_name##_vector, \
            "SI:" L##mp_NAME L"-VECTOR", t) \
        DEFSUPER("DATA-VECTOR") \
        DEFCLASSINFO_(built_in_class, BinVec, mp_size, mp_elt) \
        FINALIZED_CLASS(##mp_name##_vector) \
        DEFSLOT(0, m_classd, "CLASSD", class_description) \
        DEFSLOT(1, m_length, "LENGTH", sequence_index) \
        DEFSLOT(2, m_element[sizeof(Val) / 2],   "", uint) \
    ENDCLASS()

#define DEF_SIGNED_VECTOR_CLASS(mp_sign, mp_SIGN, mp_size, mp_elt) \
    DEF_VECTOR_CLASS( \
        mp_sign##_byte_##mp_size, \
        mp_SIGN L"-BYTE-"L## #mp_size, \
        mp_size, mp_elt )

DEF_SIGNED_VECTOR_CLASS(unsigned, "UNSIGNED", 8, ty_unsigned_byte_8)
DEF_SIGNED_VECTOR_CLASS(unsigned, "UNSIGNED", 16, ty_unsigned_byte_16)
DEF_SIGNED_VECTOR_CLASS(unsigned, "UNSIGNED", 32, ty_unsigned_byte_32)

DEF_SIGNED_VECTOR_CLASS(signed, "SIGNED", 8,  ty_signed_byte_8)
DEF_SIGNED_VECTOR_CLASS(signed, "SIGNED", 16, ty_signed_byte_16)
DEF_SIGNED_VECTOR_CLASS(signed, "SIGNED", 32, ty_signed_byte_32)

#if SIZEOF_VAL == 8
DEF_SIGNED_VECTOR_CLASS(unsigned, "UNSIGNED", 64, ty_unsigned_byte_64)
DEF_SIGNED_VECTOR_CLASS(signed, "SIGNED", 64, ty_signed_byte_64)
#endif // SIZEOF_VAL == 8

DEF_VECTOR_CLASS(double_float, "DOUBLE-FLOAT", 64, ty_double_float)
DEF_VECTOR_CLASS(single_float, "SINGLE-FLOAT", 32, ty_single_float)

DEF_VECTOR_CLASS(double_float_complex, "DOUBLE-FLOAT-COMPLEX", 64*2,
    ty_complex_double_float )

DEF_VECTOR_CLASS(single_float_complex, "SINGLE-FLOAT-COMPLEX", 32*2,
    ty_complex_single_float )

DEFCLASS(simple_vector, "SIMPLE-VECTOR", t)
    DEFSUPER("DATA-VECTOR")
    DEFCLASSINFO_(built_in_class, Vector, sizeof(Val) * 8, t)
    FINALIZED_CLASS(simple_vector)

    DEFSLOT(0, m_classd,        "CLASSD", class_description)
    DEFSLOT(1, m_length,        "LENGTH", sequence_index)
    DEFSLOT(2, mv_element[2],   "",  t)
ENDCLASS()

#undef DEF_SIGNED_VECTOR_CLASS
#undef DEF_VECTOR_CLASS

//////////////////////////////////////////////////////////////////////
//
// Adjustable Vector Family
//
DEFCLASS(bit_vector_object, "BIT-VECTOR-OBJECT", vector_object)
    DEFSUPER("VECTOR-OBJECT BIT-VECTOR")

    DEFCLASSINFO(
        built_in_class,
        Fixed,
        sizeof(Layout::C_bit_vector_object) )

    FINALIZED_CLASS(bit_vector_object)
ENDCLASS()

DEFCLASS(general_vector_object, "GENERAL-VECTOR-OBJECT", vector_object)
    DEFSUPER("VECTOR-OBJECT")

    DEFCLASSINFO(built_in_class,
        Fixed, sizeof(Layout::C_general_vector_object) )

    FINALIZED_CLASS(general_vector_object)
ENDCLASS()

DEFCLASS(string_object, "STRING-OBJECT", vector_object)
    DEFSUPER("VECTOR-OBJECT STRING")

    DEFCLASSINFO(built_in_class,
        Fixed, sizeof(Layout::C_string_object) )

    FINALIZED_CLASS(string_object)
ENDCLASS()


//////////////////////////////////////////////////////////////////////////////
//
// 17 Sequences
//
DEFCLASS(sequence, "SEQUENCE", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, None, 0)
ENDCLASS()


//////////////////////////////////////////////////////////////////////////////
//
// 18 Hash Tables
//
DEFCLASS(hash_table, "HASH-TABLE", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class, Structure, sizeof(Layout::C_hash_table))
    FINALIZED_CLASS(hash_table)

    //DEFSLOT(0, m_classd, "CLASSD", class_description)
    DEFSLOT(1, m_vector,        "VECTOR",       simple_vector)
    DEFSLOT(2, m_test,          "TEST",         symbol)
    DEFSLOT(3, m_rehash_size,   "REHASH-SIZE",  real)
ENDCLASS()


//////////////////////////////////////////////////////////////////////////////
//
// 19 Filename
//
DEFCLASS(basic_host, "BASIC-HOST", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class, Structure, sizeof(Layout::C_basic_host))

    DEFSLOT(1, m_name,              "NAME",             string)
    DEFSLOT(2, m_local_case,        "LOCAL-CASE",       symbol)
        // (member :mixed :upcase :downcase :preserve)
    DEFSLOT(3, m_customary_case,    "CUSTOMARY-CASE",   symbol)
        // (member :uppder :lower)
    DEFSLOT(4, m_default_device,    "DEFAULT-DEVICE",   t) // nil
ENDCLASS()

DEFCLASS(logical_host, "LOGICAL-HOST", basic_host)
    DEFSUPER("BASIC-HOST")
    DEFCLASSINFO(structure_class,
        Structure, sizeof(Layout::C_logical_host) )
    DEFSLOT_OVERRIDE("LOCAL-CASE")
    DEFSLOT_OVERRIDE("CUSTOMARY-CASE")
    DEFSLOT(4, m_translations,      "TRANSLATIONS", list)
ENDCLASS()

DEFCLASS(pathname, "PATHNAME", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")

    DEFCLASSINFO(structure_class,
        Structure, sizeof(Layout::C_pathname) )

    DEFSLOT(1, m_host,      "HOST",         basic_host)
    DEFSLOT(2, m_device,    "DEVICE",       t)
    DEFSLOT(3, m_directory, "DIRECTORY",    list)
    DEFSLOT(4, m_name,      "NAME",         t)
    DEFSLOT(5, m_type,      "TYPE",         t)
    DEFSLOT(6, m_version,   "VERSION",      t)
ENDCLASS()


DEFCLASS(logical_pathname, "LOGICAL-PATHNAME", pathname)
    DEFSUPER("PATHNAME")
    DEFCLASSINFO(structure_class,
        Structure, sizeof(Layout::C_logical_pathname) )
ENDCLASS()


DEFCLASS(physical_pathname, "PHYSICAL-PATHNAME", pathname)
    DEFSUPER("PATHNAME")
    DEFCLASSINFO(structure_class,
        Structure, sizeof(Layout::C_physical_pathname) )
ENDCLASS()


#if defined(_WIN32)

DEFCLASS(windows_host, "WINDOWS-HOST", basic_host)
    DEFSUPER("BASIC-HOST")
    DEFCLASSINFO(structure_class,
        Structure, sizeof(Layout::C_windows_host) )
    DEFSLOT_OVERRIDE("LOCAL-CASE")
    DEFSLOT_OVERRIDE("CUSTOMARY-CASE")
    DEFSLOT_OVERRIDE("DEFAULT-DEVICE")
ENDCLASS()

DEFCLASS(windows_pathname, "WINDOWS-PATHNAME", pathname)
    DEFSUPER("PHYSICAL-PATHNAME")
    DEFCLASSINFO(structure_class,
        Structure, sizeof(Layout::C_windows_pathname))
ENDCLASS()

#endif // defined(_WIN32)


//////////////////////////////////////////////////////////////////////////////
//
// 21 Streams
//
DEFCLASS(stream, "STREAM", standard_object)
    DEFSUPER("STANDARD-OBJECT")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT(0, m_flags, "FLAGS", fixnum)
ENDCLASS()

DEFCLASS(input_stream, "EXT:INPUT-STREAM", stream)
    DEFSUPER("STREAM")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT_OVERRIDE("FLAGS")
ENDCLASS()


DEFCLASS(io_stream, "EXT:IO-STREAM", stream)
    DEFSUPER("STREAM")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT_OVERRIDE("FLAGS")
ENDCLASS()


DEFCLASS(output_stream, "EXT:OUTPUT-STREAM", stream)
    DEFSUPER("STREAM")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT_OVERRIDE("FLAGS")
ENDCLASS()


DEFCLASS(platform_stream, "PLATFORM-STREAM", stream)
    DEFSUPER("STREAM")
    DEFCLASSINFO(standard_class, None, 0)

    DEFSLOT(3, m_blob,              "BLOB", fixnum)
    DEFSLOT(4, m_external_format,   "EXTERNAL-FORMAT", t)
ENDCLASS()


DEFCLASS(broadcast_stream, "BROADCAST-STREAM", output_stream)
    DEFSUPER("OUTPUT-STREAM")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_broadcast_stream))

    DEFSLOT(3, m_streams,   "STREAMS", list)
ENDCLASS()


DEFCLASS(concatenated_stream, "CONCATENATED-STREAM", input_stream)
    DEFSUPER("INPUT-STREAM")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_concatenated_stream) )

    DEFSLOT(3, m_streams,   "STREAMS", list)
ENDCLASS()


DEFCLASS(echo_stream, "ECHO-STREAM", io_stream)
    DEFSUPER("IO-STREAM")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_echo_stream))

    DEFSLOT(1, m_input_stream,  "INPUT-STREAM", stream)
    DEFSLOT(2, m_output_stream, "OUTPUT-STREAM", stream)
    DEFSLOT(3, m_unread_p,      "UNREAD-P", t)
ENDCLASS()


DEFCLASS(string_stream, "STRING-STREAM", stream)
    DEFSUPER("STREAM")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()


DEFCLASS(synonym_stream, "SYNONYM-STREAM", stream)
    DEFSUPER("STREAM")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_synonym_stream))

    DEFSLOT(3, m_symbol,  "SYMBOL", symbol)
ENDCLASS()


DEFCLASS(two_way_stream, "TWO-WAY-STREAM", stream)
    DEFSUPER("IO-STREAM")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_two_way_stream))

    DEFSLOT(3, m_input_stream,  "INPUT-STREAM", stream)
    DEFSLOT(4, m_output_stream, "OUTPUT-STREAM", stream)
ENDCLASS()


DEFCLASS(file_stream, "FILE-STREAM", platform_stream)
    DEFSUPER("PLATFORM-STREAM")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_file_stream) )

    DEFSLOT(5, m_element_type,  "ELEMENT-TYPE", type_specifier)
    DEFSLOT(6, m_pathname,      "PATHNAME",     pathname)
    DEFSLOT(7, m_plist,         "PLIST",        list)
ENDCLASS()


DEFCLASS(pipe_stream, "PIPE-STREAM", platform_stream)
    DEFSUPER("PLATFORM-STREAM IO-STREAM")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_pipe_stream))
ENDCLASS()


DEFCLASS(socket_stream, "SOCKET-STREAM", platform_stream)
    DEFSUPER("PLATFORM-STREAM")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_socket_stream))
ENDCLASS()


DEFCLASS(string_input_stream, "STRING-INPUT-STREAM", string_stream)
    DEFSUPER("STRING-STREAM INPUT-STREAM")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_string_input_stream) )

    DEFSLOT(3, m_string,    "STRING",   simple_string)
    DEFSLOT(4, m_index,     "INDEX",    fixnum)
    DEFSLOT(5, m_end,       "END",      fixnum)
ENDCLASS()


DEFCLASS(string_output_stream, "STRING-OUTPUT-STREAM", string_stream)
    DEFSUPER("STRING-STREAM OUTPUT-STREAM")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_string_output_stream) )

    DEFSLOT(1, m_string,    "STRING", simple_string)
    DEFSLOT(2, m_column,    "COLUMN", fixnum)
ENDCLASS()


DEFCLASS(charset, "CHARSET", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(
        structure_class,
        Structure,
        sizeof(Layout::C_charset) )
    DEFSLOT(1, m_name,      "NAME",         symbol)
    DEFSLOT(2, m_aliases,   "ALIASES",       list)
    DEFSLOT(3, m_code_page, "CODE-PAGE",    fixnum)

    FINALIZED_CLASS(charset)
ENDCLASS()


DEFCLASS(external_format, "EXT:EXTERNAL-FORMAT", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(
        structure_class,
        Structure,
        sizeof(Layout::C_external_format) )
    DEFSLOT(1, m_charset,   "CHARSET",  charset)
    DEFSLOT(2, m_eol,       "EOL",      symbol)
    DEFSLOT(3, m_flags,     "FLAGS",    fixnum)

    FINALIZED_CLASS(external_format)
ENDCLASS()


#if _WIN32
//////////////////////////////////////////////////////////////////////
//
// Windows Specific Streams
//
DEFCLASS(console_stream, "CONSOLE-STREAM", platform_stream)
    DEFSUPER("PLATFORM-STREAM")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_console_stream) )
ENDCLASS()

DEFCLASS(debug_output_stream, "DEBUG-OUTPUT-STREAM", platform_stream)
    DEFSUPER("PLATFORM-STREAM")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_debug_output_stream) )
    DEFSLOT_OVERRIDE("FLAGS")
ENDCLASS()

DEFCLASS(listener_stream, "LISTENER-STREAM", platform_stream)
    DEFSUPER("PLATFORM-STREAM")
    DEFCLASSINFO(standard_class,
        Instance, sizeof(Layout::C_listener_stream) )
ENDCLASS()

// binary object
DEFCLASS(handle, "HANDLE", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, Fixed, sizeof(Layout::C_handle))
    FINALIZED_CLASS(handle)
    DEFSLOT(0, m_classd, "CLASSD", class_description)
    DEFSLOT(1, m_value,  "VALUE",  UInt)
ENDCLASS()
#endif // _WIN32


//////////////////////////////////////////////////////////////////////////////
//
// 22 Printer
//
DEFCLASS(pp_dispatch_table, "PP-DISPATCH-TABLE", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class,
        Structure, sizeof(Layout::C_pp_dispatch_table) )

    DEFSLOT(1, m_cons_entries,  "CONS-ENTRIES", hash_table)
    DEFSLOT(2, m_entries,       "ENTREIS",      list)
ENDCLASS()


//////////////////////////////////////////////////////////////////////////////
//
// 23 Reader
//
DEFCLASS(readtable, "READTABLE", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class, Structure, sizeof(Layout::C_readtable))
    FINALIZED_CLASS(readtable)

    //DEFSLOT(0, m_classd,    "CLASSD",   class_description)
    DEFSLOT(1, m_case,      "CASE",     symbol)
    DEFSLOT(2, m_vector,    "VECTOR",   simple_vector)
    DEFSLOT(3, m_table,     "TABLE",    hash_table)
ENDCLASS()


//////////////////////////////////////////////////////////////////////////////
//
// 48 Foreign Function Interface
//
DEFCLASS(foreign_class, "FOREIGN-CLASS", class)
    DEFSUPER("CLASS")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_foreign_class))
    FINALIZED_CLASS(foreign_class)
ENDCLASS()


DEFCLASS(foreign_object, "FOREIGN-OBJECT", t)
    DEFSUPER("T")
    DEFCLASSINFO(foreign_class, Foreign, 0)
    FINALIZED_CLASS(foreign_object)
ENDCLASS()


DEFCLASS(foreign_slot_definition,
        "FOREIGN-SLOT-DEFINITION", slot_definition)
    DEFSUPER("SLOT-DEFINITION")
    DEFCLASSINFO(standard_class, None, 0)
ENDCLASS()


DEFCLASS(foreign_direct_slot_definition,
        "FOREIGN-DIRECT-SLOT-DEFINITION", direct_slot_definition )
    DEFSUPER("FOREIGN-SLOT-DEFINITION DIRECT-SLOT-DEFINITION")

    DEFCLASSINFO(standard_class,
        Instance,
        sizeof(Layout::C_foreign_direct_slot_definition) )

    FINALIZED_CLASS(foreign_direct_slot_definition)
ENDCLASS()


DEFCLASS(foreign_effective_slot_definition,
        "FOREIGN-EFFECTIVE-SLOT-DEFINITION", effective_slot_definition)
    DEFSUPER("FOREIGN-SLOT-DEFINITION EFFECTIVE-SLOT-DEFINITION")

    DEFCLASSINFO(standard_class,
        Instance,
        sizeof(Layout::C_foreign_effective_slot_definition) )

    FINALIZED_CLASS(foreign_effective_slot_definition)
ENDCLASS()


//////////////////////////////////////////////////////////////////////////////
//
// 49 Internals
//
DEFCLASS(invalid_object, "INVALID-OBJECT", t)
    DEFSUPER("T")

    DEFCLASSINFO_(built_in_class,
        Immediate, Fixnum::Bits, Fixnum::Encode(Fixnum::Tag) )
ENDCLASS()

DEFCLASS(marker, "MARKER", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, Fixed, sizeof(Layout::C_marker))
    FINALIZED_CLASS(marker)
    DEFSLOT(0, m_classd,    "CLASSD",   class_description)
    DEFSLOT(1, m_name,      "NAME",     symbol)
ENDCLASS()

DEFCLASS(environment, "ENVIRONMENT", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class, Structure, sizeof(Layout::C_environment))
    FINALIZED_CLASS(environment)

    //DEFSLOT(0, m_classd,    "CLASSD",       class_description)
    DEFSLOT(1, m_latch,     "LATCH",        latch)
    DEFSLOT(2, m_variables, "VARIABLES",    hash_table)
    DEFSLOT(3, m_functions, "FUNCTIONS",    hash_table)
    DEFSLOT(4, m_outer,     "OUTER",        or_environment_null)
    DEFSLOT(5, m_types,     "TYPES",        or_hash_table_null)
    DEFSLOT(6, m_classes,   "CLASSES",      or_hash_table_null)
    DEFSLOT(7, m_others,    "OTHERS",       or_hash_table_null)
ENDCLASS()


DEFCLASS(tlv_record, "SI:TLV-RECORD", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class, Structure, sizeof(Layout::C_tlv_record))
    FINALIZED_CLASS(tlv_record)

    //DEFSLOT(0, m_classd, "CLASSD",      class_description)
    DEFSLOT(1, m_name,      "NAME",     symbol)
    DEFSLOT(2, m_index,     "INDEX",    fixnum)
    DEFSLOT(3, m_value,     "VALUE",    t)
ENDCLASS()

DEFCLASS(closed_cell, "SI:CLOSED-CELL", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class, Structure, sizeof(Layout::C_closed_cell))
    FINALIZED_CLASS(closed_cell)

    //DEFSLOT(0, m_classd,    "CLASSD", class_description)
    DEFSLOT(1, m_value,     "VALUE", t)
ENDCLASS()


DEFCLASS(value_cell, "SI:VALUE-CELL", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class, Structure, sizeof(Layout::C_value_cell))
    FINALIZED_CLASS(value_cell)

    //DEFSLOT(0, m_classd,    "CLASSD",   class_description)
    DEFSLOT(1, m_value,     "VALUE",    t)
    DEFSLOT(2, m_name,      "NAME",     symbol)
    DEFSLOT(3, m_type,      "TYPE",     symbol)
ENDCLASS()


DEFCLASS(setf_cell, "SI:SETF-CELL", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class, Structure, sizeof(Layout::C_setf_cell))
    FINALIZED_CLASS(setf_cell)

    //DEFSLOT(0, m_classd,    "CLASSD",   class_description)
    DEFSLOT(1, m_function,  "FUNCTION", or_function_null)
    DEFSLOT(2, m_name,      "NAME",     symbol)
ENDCLASS()


//////////////////////////////////////////////////////////////////////////////
//
// 50 Extensions
//

DEFCLASS(mutex, "EXT:MUTEX", waitable)
    DEFSUPER("WAITABLE")
    DEFCLASSINFO(built_in_class, Fixed, sizeof(Layout::C_mutex))
    FINALIZED_CLASS(mutex)

    DEFSLOT(3, m_name, "NAME", t)
ENDCLASS()


DEFCLASS(latch, "EXT:LATCH", waitable)
    DEFSUPER("WAITABLE")
    DEFCLASSINFO(built_in_class, Fixed, sizeof(Layout::C_latch))
    FINALIZED_CLASS(latch)

    DEFSLOT(3, m_name,          "NAME",         t)
    DEFSLOT(4, m_lock_count,    "LOCK-COUNT",   fixnum)
    DEFSLOT(5, m_spinlock,      "SPINLOCK",     member_Klocked_nil)
ENDCLASS()

DEFCLASS(weak_pointer, "EXT:WEAK-POINTER", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class, Structure, sizeof(Layout::C_weak_pointer))
    FINALIZED_CLASS(weak_pointer)

    //DEFSLOT(0, m_classd,    "CLASSD",   class_description)
    DEFSLOT(1, m_value,     "VALUE",    t)
ENDCLASS()

DEFCLASS(weak_vector_leader, "SI:WEAK-VECTOR-LEADER", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class,
        Fixed, sizeof(Layout::C_weak_vector_leader) )

    FINALIZED_CLASS(weak_vector_leader)

    DEFSLOT(0, m_classd, "CLASSD",  class_description)
    DEFSLOT(1, m_kind,      "KIND", symbol)
ENDCLASS()

////////////////////////////////////////////////////////////
//
// Dll Link
//
DEFCLASS(dll_proc_info, "SI:DLL-PROC-INFO", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class,
        Structure, sizeof(Layout::C_dll_proc_info) )

    DEFSLOT(1, m_proc_name, "PROC-NAME", simple_string)
    DEFSLOT(2, m_file_info, "FILE-INFO", dll_file_info)
    DEFSLOT(3, m_entry,     "ENTRY", fixnum)
ENDCLASS()


DEFCLASS(dll_file_info, "SI:DLL-FILE-INFO", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class,
        Structure, sizeof(Layout::C_dll_file_info) )

    DEFSLOT(1, m_handle,        "HANDLE", fixnum)
    DEFSLOT(2, m_filename,      "FILENAME", simple_string)
    DEFSLOT(3, m_proc_table,    "PROC-TABLE", hash_table)
ENDCLASS()


DEFCLASS(dll_link_error, "SI:DLL-LINK-ERROR", error)
    DEFSUPER("ERROR")
    DEFCLASSINFO(standard_class, Instance, sizeof(Layout::C_dll_link_error))

    DEFSLOT(1, m_arguments, "ARGUMENTS", list)
    DEFSLOT(2, m_filename,  "FILENAME", simple_string)
    DEFSLOT(3, m_name,      "NAME", simple_string)
    DEFSLOT(4, m_code,      "CODE", fixnum)
ENDCLASS()


DEFCLASS(thread, "SI:THREAD", t)
    DEFSUPER("T")
    DEFCLASSINFO(built_in_class, Fixed, sizeof(Layout::C_thread))
    FINALIZED_CLASS(thread)

    DEFSLOT(0, m_classd, "CLASSD", class_description)
    DEFSLOT(1, m_name,          "NAME", symbol)
    DEFSLOT(2, m_waiting,       "WAITING", waitable)
    DEFSLOT(3, m_next_waiter,   "NEXT-WAITER", or_thread_null)

    DEFSLOT(4, m_pConsArea,     "CONS-AREA", fixnum)
    DEFSLOT(5, m_pSymbolrea,    "SYMBOL-AREA", fixnum)
    DEFSLOT(6, m_pRecordArea,   "RECORD-AREA", fixnum)
    DEFSLOT(7, m_pBinObjArea,   "BINOBJ-AREA", fixnum)
    DEFSLOT(8, m_pFunObjArea,   "FUNOBJ-AREA", fixnum)

    DEFSLOT(9, m_pObStackArea,  "OBSTACK-AREA", fixnum)
ENDCLASS()

DEFCLASS(finalization, "FINALIZATION", structure_object)
    DEFSUPER("STRUCTURE-OBJECT")
    DEFCLASSINFO(structure_class, Structure, sizeof(Layout::C_finalization))
    FINALIZED_CLASS(finalization)

    //DEFSLOT(0, m_classd,    "CLASSD",   class_description)
    DEFSLOT(1, m_object,    "OBJECT",   t)
    DEFSLOT(2, m_function,  "FUNCTION", function)
    DEFSLOT(3, m_state,     "STATE",    t)
ENDCLASS()

#undef DEFCLASS
#undef ENDCLASS
#undef DEFSUPER
#undef DEFCLASSINFO_
#undef DEFCLASSINFO
#undef DEFSLOT_OVERRIDE
#undef DEFSLOT_
#undef DEFSLOT
#undef FINALIZED_CLASS
