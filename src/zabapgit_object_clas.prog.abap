*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_CLAS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_clas DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_object_oriented_object.
  METHODS:
    create
      IMPORTING
        iv_package    TYPE devclass
        iv_overwrite  TYPE seox_boolean DEFAULT seox_true
      CHANGING
        is_properties TYPE any
      RAISING
        lcx_exception,
    generate_locals
      IMPORTING
        is_key                   TYPE seoclskey
        iv_force                 TYPE seox_boolean DEFAULT seox_true
        it_local_definitions     TYPE seop_source_string OPTIONAL
        it_local_implementations TYPE seop_source_string OPTIONAL
        it_local_macros          TYPE seop_source_string OPTIONAL
        it_local_test_classes    TYPE seop_source_string OPTIONAL
      RAISING
        lcx_exception,
    deserialize_source
      IMPORTING
        is_key    TYPE seoclskey
        it_source TYPE ty_string_tt
      RAISING
        lcx_exception
        cx_sy_dyn_call_error,
    insert_text_pool
      IMPORTING
        iv_class_name TYPE seoclsname
        it_text_pool  TYPE textpool_table
        iv_language   TYPE spras
      RAISING
        lcx_exception,
    update_descriptions
      IMPORTING
        is_key          TYPE seoclskey
        it_descriptions TYPE ty_seocompotx_tt,
    add_to_activation_list
      IMPORTING
        is_item TYPE ty_item
      RAISING
        lcx_exception,
    create_sotr
      IMPORTING
        iv_package TYPE devclass
        it_sotr    TYPE ty_sotr_tt
      RAISING
        lcx_exception,
    create_documentation
      IMPORTING
        it_lines       TYPE tlinetab
        iv_object_name TYPE dokhl-object
        iv_language    TYPE spras
      RAISING
        lcx_exception.
ENDINTERFACE.

CLASS lcl_object_oriented_base DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES: lif_object_oriented_object.
  PRIVATE SECTION.
    METHODS deserialize_abap_source_old
      IMPORTING is_clskey TYPE seoclskey
                it_source TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS deserialize_abap_source_new
      IMPORTING is_clskey TYPE seoclskey
                it_source TYPE ty_string_tt
      RAISING   lcx_exception
                cx_sy_dyn_call_error.
ENDCLASS.

CLASS lcl_object_oriented_base IMPLEMENTATION.

  METHOD lif_object_oriented_object~create.
    "Subclass responsibility
    RETURN.
  ENDMETHOD.

  METHOD lif_object_oriented_object~deserialize_source.
    TRY.
        deserialize_abap_source_new(
          is_clskey = is_key
          it_source = it_source ).
      CATCH cx_sy_dyn_call_error.
        deserialize_abap_source_old(
          is_clskey = is_key
          it_source = it_source ).
    ENDTRY.
  ENDMETHOD.

  METHOD lif_object_oriented_object~generate_locals.
    "Subclass responsibility
    RETURN.
  ENDMETHOD.

  METHOD deserialize_abap_source_old.
    "for backwards compatability down to 702

    DATA: lo_source TYPE REF TO cl_oo_source.

    CREATE OBJECT lo_source
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from CL_OO_SOURCE' ).
    ENDIF.

    TRY.
        lo_source->access_permission( seok_access_modify ).
        lo_source->set_source( it_source ).
        lo_source->save( ).
        lo_source->access_permission( seok_access_free ).
      CATCH cx_oo_access_permission.
        lcx_exception=>raise( 'permission error' ).
      CATCH cx_oo_source_save_failure.
        lcx_exception=>raise( 'save failure' ).
    ENDTRY.

  ENDMETHOD.

  METHOD deserialize_abap_source_new.
    DATA: lo_factory TYPE REF TO object,
          lo_source  TYPE REF TO object.

    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_factory.

    CALL METHOD lo_factory->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = is_clskey-clsname
      RECEIVING
        result    = lo_source.

    TRY.
        CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~LOCK').
      CATCH cx_oo_access_permission.
        lcx_exception=>raise( 'source_new, access permission exception' ).
    ENDTRY.

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SET_SOURCE')
      EXPORTING
        source = it_source.

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SAVE').

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~UNLOCK').

  ENDMETHOD.
  METHOD lif_object_oriented_object~add_to_activation_list.
    lcl_objects_activation=>add_item( is_item ).
  ENDMETHOD.

  METHOD lif_object_oriented_object~update_descriptions.
    DELETE FROM seocompotx WHERE clsname = is_key-clsname.
    INSERT seocompotx FROM TABLE it_descriptions.
  ENDMETHOD.
  METHOD lif_object_oriented_object~insert_text_pool.
    "Subclass responsibility
    RETURN.
  ENDMETHOD.

  METHOD lif_object_oriented_object~create_sotr.
    "Subclass responsibility
    RETURN.
  ENDMETHOD.

  METHOD lif_object_oriented_object~create_documentation.
    CALL FUNCTION 'DOCU_UPD'
      EXPORTING
        id       = 'CL'
        langu    = iv_language
        object   = iv_object_name
      TABLES
        line     = it_lines
      EXCEPTIONS
        ret_code = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from DOCU_UPD' ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_object_oriented_class DEFINITION
  INHERITING FROM lcl_object_oriented_base.
  PUBLIC SECTION.
    METHODS:
      lif_object_oriented_object~create REDEFINITION,
      lif_object_oriented_object~generate_locals REDEFINITION,
      lif_object_oriented_object~insert_text_pool REDEFINITION,
      lif_object_oriented_object~create_sotr REDEFINITION.

ENDCLASS.
CLASS lcl_object_oriented_class IMPLEMENTATION.
  METHOD lif_object_oriented_object~create.
    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        devclass        = iv_package
        overwrite       = iv_overwrite
      CHANGING
        class           = is_properties
      EXCEPTIONS
        existing        = 1
        is_interface    = 2
        db_error        = 3
        component_error = 4
        no_access       = 5
        other           = 6
        OTHERS          = 7.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from SEO_CLASS_CREATE_COMPLETE' ).
    ENDIF.
  ENDMETHOD.
  METHOD lif_object_oriented_object~generate_locals.
    CALL FUNCTION 'SEO_CLASS_GENERATE_LOCALS'
      EXPORTING
        clskey                 = is_key
        force                  = iv_force
        locals_def             = it_local_definitions
        locals_imp             = it_local_implementations
        locals_mac             = it_local_macros
        locals_testclasses     = it_local_test_classes
      EXCEPTIONS
        not_existing           = 1
        model_only             = 2
        locals_not_generated   = 3
        locals_not_initialised = 4
        OTHERS                 = 5.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from generate_locals' ).
    ENDIF.
  ENDMETHOD.
  METHOD lif_object_oriented_object~insert_text_pool.
    DATA: lv_cp        TYPE program.

    lv_cp = cl_oo_classname_service=>get_classpool_name( iv_class_name ).

    INSERT TEXTPOOL lv_cp
      FROM it_text_pool
      LANGUAGE iv_language
      STATE 'I'.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from INSERT TEXTPOOL' ).
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'REPT'
                                 iv_name = lv_cp ).
  ENDMETHOD.

  METHOD lif_object_oriented_object~create_sotr.
    DATA: lt_sotr    TYPE ty_sotr_tt,
          lt_objects TYPE sotr_objects,
          ls_paket   TYPE sotr_pack,
          lv_object  LIKE LINE OF lt_objects.

    FIELD-SYMBOLS: <ls_sotr> LIKE LINE OF lt_sotr.

    LOOP AT it_sotr ASSIGNING <ls_sotr>.
      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sotr>-header-objid_vec
        IMPORTING
          objects          = lt_objects
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from SOTR_OBJECT_GET_OBJECTS' ).
      ENDIF.

      READ TABLE lt_objects INDEX 1 INTO lv_object.
      ASSERT sy-subrc = 0.

      ls_paket-paket = iv_package.

      CALL FUNCTION 'SOTR_CREATE_CONCEPT'
        EXPORTING
          paket                         = ls_paket
          crea_lan                      = <ls_sotr>-header-crea_lan
          alias_name                    = <ls_sotr>-header-alias_name
          object                        = lv_object
          entries                       = <ls_sotr>-entries
          concept_default               = <ls_sotr>-header-concept
        EXCEPTIONS
          package_missing               = 1
          crea_lan_missing              = 2
          object_missing                = 3
          paket_does_not_exist          = 4
          alias_already_exist           = 5
          object_type_not_found         = 6
          langu_missing                 = 7
          identical_context_not_allowed = 8
          text_too_long                 = 9
          error_in_update               = 10
          no_master_langu               = 11
          error_in_concept_id           = 12
          alias_not_allowed             = 13
          tadir_entry_creation_failed   = 14
          internal_error                = 15
          error_in_correction           = 16
          user_cancelled                = 17
          no_entry_found                = 18
          OTHERS                        = 19.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from SOTR_CREATE_CONCEPT' ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_oriented_interface DEFINITION
  INHERITING FROM lcl_object_oriented_base.
  PUBLIC SECTION.
    METHODS:
      lif_object_oriented_object~create REDEFINITION.
ENDCLASS.

CLASS lcl_object_oriented_interface IMPLEMENTATION.
  METHOD lif_object_oriented_object~create.
    CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
      EXPORTING
        devclass        = iv_package
        overwrite       = iv_overwrite
      CHANGING
        interface       = is_properties
      EXCEPTIONS
        existing        = 1
        is_class        = 2
        db_error        = 3
        component_error = 4
        no_access       = 5
        other           = 6
        OTHERS          = 7.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Error from SEO_INTERFACE_CREATE_COMPLETE' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lth_oo_factory_injector DEFINITION DEFERRED.

CLASS lcl_object_oriented_factory DEFINITION
  FRIENDS lth_oo_factory_injector.
  PUBLIC SECTION.
    CLASS-METHODS:
      make
        IMPORTING
          iv_object_type                   TYPE tadir-object
        RETURNING
          VALUE(ro_object_oriented_object) TYPE REF TO lif_object_oriented_object.
  PRIVATE SECTION.
    CLASS-DATA:
        go_object_oriented_object TYPE REF TO lif_object_oriented_object.
ENDCLASS.
CLASS lcl_object_oriented_factory IMPLEMENTATION.
  METHOD make.
    IF go_object_oriented_object IS BOUND.
      ro_object_oriented_object = go_object_oriented_object.
      RETURN.
    ENDIF.
    IF iv_object_type = 'CLAS'.
      CREATE OBJECT ro_object_oriented_object TYPE lcl_object_oriented_class.
    ELSEIF iv_object_type = 'INTF'.
      CREATE OBJECT ro_object_oriented_object TYPE lcl_object_oriented_interface.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lth_oo_factory_injector DEFINITION FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS:
      inject
        IMPORTING
          io_object_oriented_object TYPE REF TO lif_object_oriented_object.
ENDCLASS.
CLASS lth_oo_factory_injector IMPLEMENTATION.
  METHOD inject.
    lcl_object_oriented_factory=>go_object_oriented_object = io_object_oriented_object.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_object_clas DEFINITION INHERITING FROM lcl_objects_program.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.
  PROTECTED SECTION.
    METHODS deserialize_abap
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.

    METHODS deserialize_docu
      IMPORTING io_xml TYPE REF TO lcl_xml_input
      RAISING   lcx_exception.
    DATA mo_object_oriented_object TYPE REF TO lif_object_oriented_object.
  PRIVATE SECTION.
    DATA mv_skip_testclass TYPE abap_bool.

    METHODS deserialize_textpool
      IMPORTING io_xml TYPE REF TO lcl_xml_input
      RAISING   lcx_exception.

    METHODS deserialize_sotr
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.

    METHODS serialize_abap_old
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_abap_new
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception
                cx_sy_dyn_call_error.

    METHODS serialize_locals_imp
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_locals_def
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS read_include
      IMPORTING is_clskey        TYPE seoclskey
                iv_type          TYPE seop_include_ext_app
      RETURNING VALUE(rt_source) TYPE seop_source_string.

    METHODS serialize_testclasses
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_macros
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE ty_string_tt
      RAISING   lcx_exception.

    METHODS serialize_xml
      IMPORTING io_xml TYPE REF TO lcl_xml_output
      RAISING   lcx_exception.

    METHODS read_sotr
      RETURNING VALUE(rt_sotr) TYPE ty_sotr_tt
      RAISING   lcx_exception.

    METHODS remove_signatures
      CHANGING ct_source TYPE ty_string_tt.

    METHODS reduce
      CHANGING ct_source TYPE ty_string_tt.

    METHODS get_all_class_includes
      RETURNING VALUE(rt_includes) TYPE seoincl_t.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_clas IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_clas IMPLEMENTATION.

  METHOD get_all_class_includes.
* note: includes returned might not exist
* method cl_oo_classname_service=>GET_ALL_CLASS_INCLUDES does not exist in 702

    DATA: lv_clsname TYPE seoclsname,
          lt_methods TYPE seop_methods_w_include.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_methods.


    lv_clsname = ms_item-obj_name.

    APPEND cl_oo_classname_service=>get_ccdef_name( lv_clsname ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ccmac_name( lv_clsname ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ccimp_name( lv_clsname ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_cl_name( lv_clsname ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ccau_name( lv_clsname ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_pubsec_name( lv_clsname ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_prosec_name( lv_clsname ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_prisec_name( lv_clsname ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_classpool_name( lv_clsname ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ct_name( lv_clsname ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_cs_name( lv_clsname ) TO rt_includes.

    lt_methods = cl_oo_classname_service=>get_all_method_includes( lv_clsname ).
    LOOP AT lt_methods ASSIGNING <ls_method>.
      APPEND <ls_method>-incname TO rt_includes.
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_object~has_changed_since.

    DATA: lv_clsname TYPE seoclsname,
          lv_program TYPE program,
          lt_incl    TYPE seoincl_t.

    FIELD-SYMBOLS <incl> LIKE LINE OF lt_incl.

    lv_clsname = ms_item-obj_name.

    CASE ms_item-obj_type.
      WHEN 'CLAS'.
        TRY.
            CALL METHOD cl_oo_classname_service=>('GET_ALL_CLASS_INCLUDES')
              EXPORTING
                class_name = lv_clsname
              RECEIVING
                result     = lt_incl.
          CATCH cx_sy_dyn_call_illegal_method.
* method does not exist in 702, just report everything as changed
            rv_changed = abap_true.
        ENDTRY.
        LOOP AT lt_incl ASSIGNING <incl>.
          rv_changed = check_prog_changed_since(
            iv_program   = <incl>
            iv_timestamp = iv_timestamp
            iv_skip_gui  = abap_true ).
          IF rv_changed = abap_true.
            RETURN.
          ENDIF.
        ENDLOOP.
      WHEN 'INTF'.
        lv_program = cl_oo_classname_service=>get_interfacepool_name( lv_clsname ).
        rv_changed = check_prog_changed_since(
          iv_program   = lv_program
          iv_timestamp = iv_timestamp
          iv_skip_gui  = abap_true ).
      WHEN OTHERS.
        lcx_exception=>raise( 'class delete, unknown type' ).
    ENDCASE.

  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~changed_by.

    TYPES: BEGIN OF ty_includes,
             programm TYPE programm,
           END OF ty_includes.

    TYPES: BEGIN OF ty_reposrc,
             unam  TYPE reposrc-unam,
             udat  TYPE reposrc-udat,
             utime TYPE reposrc-utime,
           END OF ty_reposrc.

    DATA: lt_reposrc  TYPE STANDARD TABLE OF ty_reposrc,
          ls_reposrc  LIKE LINE OF lt_reposrc,
          lt_includes TYPE STANDARD TABLE OF ty_includes,
          lv_clsname  TYPE seoclsname.


    lv_clsname = ms_item-obj_name.

    CASE ms_item-obj_type.
      WHEN 'CLAS'.
        lt_includes = get_all_class_includes( ).
        ASSERT lines( lt_includes ) > 0.
      WHEN 'INTF'.
        APPEND cl_oo_classname_service=>get_interfacepool_name( lv_clsname ) TO lt_includes.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    SELECT unam udat utime FROM reposrc
      INTO TABLE lt_reposrc
      FOR ALL ENTRIES IN lt_includes
      WHERE progname = lt_includes-programm
      AND   r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ELSE.
      SORT lt_reposrc BY udat DESCENDING utime DESCENDING.
      READ TABLE lt_reposrc INDEX 1 INTO ls_reposrc.
      ASSERT sy-subrc = 0.
      rv_user = ls_reposrc-unam.
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: ls_clskey TYPE seoclskey.


    ls_clskey-clsname = ms_item-obj_name.

    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = ls_clskey
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    rv_bool = boolc( sy-subrc <> 2 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'CLAS'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: ls_clskey TYPE seoclskey.


    ls_clskey-clsname = ms_item-obj_name.

    CASE ms_item-obj_type.
      WHEN 'CLAS'.
        CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
          EXPORTING
            clskey       = ls_clskey
          EXCEPTIONS
            not_existing = 1
            is_interface = 2
            db_error     = 3
            no_access    = 4
            other        = 5
            OTHERS       = 6.
        IF sy-subrc <> 0.
          lcx_exception=>raise( 'Error from SEO_CLASS_DELETE_COMPLETE' ).
        ENDIF.
      WHEN 'INTF'.
        CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
          EXPORTING
            intkey       = ls_clskey
          EXCEPTIONS
            not_existing = 1
            is_class     = 2
            db_error     = 3
            no_access    = 4
            other        = 5
            OTHERS       = 6.
        IF sy-subrc <> 0.
          lcx_exception=>raise( 'Error from SEO_INTERFACE_DELETE_COMPLETE' ).
        ENDIF.
      WHEN OTHERS.
        lcx_exception=>raise( 'class delete, unknown type' ).
    ENDCASE.

  ENDMETHOD.                    "delete

  METHOD reduce.

    DATA: lv_source LIKE LINE OF ct_source,
          lv_found  TYPE sap_bool.


* skip files that only contain the standard comments
    lv_found = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF strlen( lv_source ) >= 3 AND lv_source(3) <> '*"*'.
        lv_found = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_found = abap_false.
      CLEAR ct_source[].
    ENDIF.

  ENDMETHOD.                    "reduce

  METHOD serialize_locals_imp.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_locals_imp ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_local

  METHOD serialize_locals_def.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_locals_def ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_locals_def

  METHOD read_include.

    DATA: ls_include TYPE progstruc.


    ASSERT iv_type = seop_ext_class_locals_def
      OR iv_type = seop_ext_class_locals_imp
      OR iv_type = seop_ext_class_macros
      OR iv_type = seop_ext_class_testclasses.

    ls_include-rootname = is_clskey-clsname.
    TRANSLATE ls_include-rootname USING ' ='.
    ls_include-categorya = iv_type(1).
    ls_include-codea = iv_type+1(4).

* it looks like there is an issue in function module SEO_CLASS_GET_INCLUDE_SOURCE
* on 750 kernels, where the READ REPORT without STATE addition does not
* return the active version, this method is a workaround for this issue
    READ REPORT ls_include INTO rt_source STATE 'A'.

  ENDMETHOD.

  METHOD serialize_testclasses.

    DATA: lv_line1 LIKE LINE OF rt_source,
          lv_line2 LIKE LINE OF rt_source.


    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_testclasses ).

* when creating classes in Eclipse it automatically generates the
* testclass include, but it is not needed, so skip to avoid
* creating an extra file in the repository.
* Also remove it if the content is manually removed, but
* the class still thinks it contains tests
    mv_skip_testclass = abap_false.
    IF lines( rt_source ) = 2.
      READ TABLE rt_source INDEX 1 INTO lv_line1.
      ASSERT sy-subrc = 0.
      READ TABLE rt_source INDEX 2 INTO lv_line2.
      ASSERT sy-subrc = 0.
      IF lv_line1(3) = '*"*' AND lv_line2 IS INITIAL.
        mv_skip_testclass = abap_true.
      ENDIF.
    ELSEIF lines( rt_source ) = 1.
      READ TABLE rt_source INDEX 1 INTO lv_line1.
      ASSERT sy-subrc = 0.
      IF lv_line1(3) = '*"*' OR lv_line1 IS INITIAL.
        mv_skip_testclass = abap_true.
      ENDIF.
    ELSEIF lines( rt_source ) = 0.
      mv_skip_testclass = abap_true.
    ENDIF.

  ENDMETHOD.                    "serialize_test

  METHOD serialize_macros.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_macros ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_macro

  METHOD serialize_abap_old.
* for old ABAP AS versions
    DATA: lo_source TYPE REF TO cl_oo_source.


    CREATE OBJECT lo_source
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from CL_OO_SOURCE' ).
    ENDIF.

    lo_source->read( 'A' ).
    rt_source = lo_source->get_old_source( ).
    remove_signatures( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_abap

  METHOD serialize_abap_new.

    DATA: lo_source   TYPE REF TO object,
          lo_instance TYPE REF TO object.

* do not call the class/methods statically, as it will
* give syntax errors on old versions
    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_instance.

    CALL METHOD lo_instance->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = is_clskey-clsname
        version   = 'A'
      RECEIVING
        result    = lo_source.

    CALL METHOD lo_source->('GET_SOURCE')
      IMPORTING
        source = rt_source.

  ENDMETHOD.

  METHOD remove_signatures.

* signatures messes up in CL_OO_SOURCE when deserializing and serializing
* within same session

    DATA: lv_begin  TYPE string,
          lv_end    TYPE string,
          lv_remove TYPE sap_bool,
          lv_source LIKE LINE OF ct_source.


    CONCATENATE '* <SIGNATURE>------------------------------------'
      '---------------------------------------------------+'
      INTO lv_begin.

    CONCATENATE '* +------------------------------------------------'
      '--------------------------------------</SIGNATURE>'
      INTO lv_end.

    lv_remove = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF lv_source = lv_begin.
        lv_remove = abap_true.
      ENDIF.
      IF lv_remove = abap_true.
        DELETE ct_source INDEX sy-tabix.
      ENDIF.
      IF lv_source = lv_end.
        lv_remove = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "remove_signatures

  METHOD lif_object~serialize.

    DATA: lt_source TYPE seop_source_string,
          ls_clskey TYPE seoclskey.


    ls_clskey-clsname = ms_item-obj_name.

    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_active
        force   = seox_true.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_inactive
        force   = seox_true.

    TRY.
        lt_source = serialize_abap_new( ls_clskey ).
      CATCH cx_sy_dyn_call_error.
        lt_source = serialize_abap_old( ls_clskey ).
    ENDTRY.

    mo_files->add_abap( lt_source ).

    IF ms_item-obj_type = 'CLAS'.
      lt_source = serialize_locals_def( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        mo_files->add_abap( iv_extra = 'locals_def'
                            it_abap  = lt_source ).         "#EC NOTEXT
      ENDIF.

      lt_source = serialize_locals_imp( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        mo_files->add_abap( iv_extra = 'locals_imp'
                            it_abap  = lt_source ).         "#EC NOTEXT
      ENDIF.

      lt_source = serialize_testclasses( ls_clskey ).
      IF NOT lt_source[] IS INITIAL AND mv_skip_testclass = abap_false.
        mo_files->add_abap( iv_extra = 'testclasses'
                            it_abap  = lt_source ).         "#EC NOTEXT
      ENDIF.

      lt_source = serialize_macros( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        mo_files->add_abap( iv_extra = 'macros'
                            it_abap  = lt_source ).         "#EC NOTEXT
      ENDIF.
    ENDIF.

    serialize_xml( io_xml ).

  ENDMETHOD.                    "serialize

  METHOD read_sotr.

    DATA: lv_concept    TYPE sotr_head-concept,
          lt_seocompodf TYPE STANDARD TABLE OF seocompodf WITH DEFAULT KEY,
          ls_header     TYPE sotr_head,
          lt_entries    TYPE sotr_text_tt.

    FIELD-SYMBOLS: <ls_sotr>       LIKE LINE OF rt_sotr,
                   <ls_seocompodf> LIKE LINE OF lt_seocompodf,
                   <ls_entry>      LIKE LINE OF lt_entries.


    SELECT * FROM seocompodf
      INTO TABLE lt_seocompodf
      WHERE clsname = ms_item-obj_name
      AND version = '1'
      AND exposure = '2'
      AND attdecltyp = '2'
      AND type = 'SOTR_CONC'
      ORDER BY PRIMARY KEY.

    LOOP AT lt_seocompodf ASSIGNING <ls_seocompodf>.

      lv_concept = translate( val = <ls_seocompodf>-attvalue from = '''' to = '' ).

      CALL FUNCTION 'SOTR_GET_CONCEPT'
        EXPORTING
          concept        = lv_concept
        IMPORTING
          header         = ls_header
        TABLES
          entries        = lt_entries
        EXCEPTIONS
          no_entry_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from SOTR_GET_CONCEPT' ).
      ENDIF.

      CLEAR: ls_header-paket,
             ls_header-crea_name,
             ls_header-crea_tstut,
             ls_header-chan_name,
             ls_header-chan_tstut.

      LOOP AT lt_entries ASSIGNING <ls_entry>.
        CLEAR: <ls_entry>-version,
               <ls_entry>-crea_name,
               <ls_entry>-crea_tstut,
               <ls_entry>-chan_name,
               <ls_entry>-chan_tstut.
      ENDLOOP.

      APPEND INITIAL LINE TO rt_sotr ASSIGNING <ls_sotr>.
      <ls_sotr>-header = ls_header.
      <ls_sotr>-entries = lt_entries.

    ENDLOOP.

  ENDMETHOD.

  METHOD serialize_xml.

    DATA: ls_vseoclass    TYPE vseoclass,
          lv_cp           TYPE program,
          lt_tpool        TYPE textpool_table,
          lv_object       TYPE dokhl-object,
          lv_state        TYPE dokhl-dokstate,
          lt_descriptions TYPE ty_seocompotx_tt,
          ls_vseointerf   TYPE vseointerf,
          ls_clskey       TYPE seoclskey,
          lt_sotr         TYPE ty_sotr_tt,
          lt_lines        TYPE tlinetab.


    ls_clskey-clsname = ms_item-obj_name.

    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = ls_clskey
        version      = seoc_version_active
      IMPORTING
        class        = ls_vseoclass
        interface    = ls_vseointerf
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 1.
      RETURN. " in case only inactive version exists
    ELSEIF sy-subrc <> 0.
      lcx_exception=>raise( 'error from seo_clif_get' ).
    ENDIF.

    CLEAR: ls_vseoclass-uuid,
           ls_vseoclass-author,
           ls_vseoclass-createdon,
           ls_vseoclass-changedby,
           ls_vseoclass-changedon,
           ls_vseoclass-r3release,
           ls_vseoclass-chgdanyby,
           ls_vseoclass-chgdanyon.

    IF mv_skip_testclass = abap_true.
      CLEAR ls_vseoclass-with_unit_tests.
    ENDIF.

    CLEAR: ls_vseointerf-uuid,
           ls_vseointerf-author,
           ls_vseointerf-createdon,
           ls_vseointerf-changedby,
           ls_vseointerf-changedon,
           ls_vseointerf-r3release.

    CASE ms_item-obj_type.
      WHEN 'CLAS'.
        io_xml->add( iv_name = 'VSEOCLASS'
                     ig_data = ls_vseoclass ).

        lv_cp = cl_oo_classname_service=>get_classpool_name( ls_clskey-clsname ).
        READ TEXTPOOL lv_cp INTO lt_tpool LANGUAGE mv_language. "#EC CI_READ_REP
        io_xml->add( iv_name = 'TPOOL'
                     ig_data = add_tpool( lt_tpool ) ).

        IF ls_vseoclass-category = seoc_category_exception.
          lt_sotr = read_sotr( ).
          IF lines( lt_sotr ) > 0.
            io_xml->add( iv_name = 'SOTR'
                         ig_data = lt_sotr ).
          ENDIF.
        ENDIF.
      WHEN 'INTF'.
        io_xml->add( iv_name = 'VSEOINTERF'
                     ig_data = ls_vseointerf ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    lv_object = ls_clskey-clsname.
    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id                = 'CL'
        langu             = mv_language
        object            = lv_object
      IMPORTING
        dokstate          = lv_state
      TABLES
        line              = lt_lines
      EXCEPTIONS
        no_docu_on_screen = 1
        no_docu_self_def  = 2
        no_docu_temp      = 3
        ret_code          = 4
        OTHERS            = 5.
    IF sy-subrc = 0 AND lv_state = 'R'.
      io_xml->add( iv_name = 'LINES'
                   ig_data = lt_lines ).
    ENDIF.

    SELECT * FROM seocompotx INTO TABLE lt_descriptions
      WHERE clsname = ls_clskey-clsname.
    DELETE lt_descriptions WHERE descript IS INITIAL.
    IF lines( lt_descriptions ) > 0.
      io_xml->add( iv_name = 'DESCRIPTIONS'
                   ig_data = lt_descriptions ).
    ENDIF.

  ENDMETHOD.                    "serialize_xml

  METHOD lif_object~deserialize.

    mo_object_oriented_object = lcl_object_oriented_factory=>make( iv_object_type = ms_item-obj_type ).

    deserialize_abap( io_xml     = io_xml
                      iv_package = iv_package ).

    deserialize_textpool( io_xml ).

    deserialize_sotr( io_xml     = io_xml
                      iv_package = iv_package ).

    deserialize_docu( io_xml ).
  ENDMETHOD.                    "deserialize

  METHOD deserialize_sotr.
    "OTR stands for Online Text Repository
    DATA: lt_sotr    TYPE ty_sotr_tt,
          lt_objects TYPE sotr_objects,
          ls_paket   TYPE sotr_pack,
          lv_object  LIKE LINE OF lt_objects.

    FIELD-SYMBOLS: <ls_sotr> LIKE LINE OF lt_sotr.


    io_xml->read( EXPORTING iv_name = 'SOTR'
                  CHANGING cg_data = lt_sotr ).

    IF lines( lt_sotr ) = 0.
      RETURN.
    ENDIF.

    mo_object_oriented_object->create_sotr(
      iv_package    = iv_package
      it_sotr       = lt_sotr ).
  ENDMETHOD.

  METHOD deserialize_docu.

    DATA: lt_lines  TYPE tlinetab,
          lv_object TYPE dokhl-object.

    io_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING cg_data = lt_lines ).

    IF lt_lines[] IS INITIAL.
      RETURN.
    ENDIF.

    lv_object = ms_item-obj_name.

    mo_object_oriented_object->create_documentation(
      it_lines       = lt_lines
      iv_object_name = lv_object
      iv_language    = mv_language ).
  ENDMETHOD.                    "deserialize_doku

  METHOD deserialize_textpool.

    DATA: lv_cp        TYPE program,
          lv_clsname   TYPE seoclsname,
          lt_tpool_ext TYPE ty_tpool_tt,
          lt_tpool     TYPE textpool_table.


    io_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    IF lt_tpool[] IS INITIAL.
      RETURN.
    ENDIF.

    lv_clsname = ms_item-obj_name.

    mo_object_oriented_object->insert_text_pool(
      iv_class_name = lv_clsname
      it_text_pool  = lt_tpool
      iv_language   = mv_language ).
  ENDMETHOD.                    "deserialize_textpool

  METHOD deserialize_abap.

    DATA: ls_vseoclass             TYPE vseoclass,
          lt_source                TYPE seop_source_string,
          lt_local_definitions     TYPE seop_source_string,
          lt_local_implementations TYPE seop_source_string,
          lt_local_macros          TYPE seop_source_string,
          lt_test_classes          TYPE seop_source_string,
          lt_descriptions          TYPE ty_seocompotx_tt,
          ls_class_key             TYPE seoclskey.


    lt_source = mo_files->read_abap( ).

    lt_local_definitions = mo_files->read_abap( iv_extra = 'locals_def'
                                                iv_error = abap_false ). "#EC NOTEXT

    lt_local_implementations = mo_files->read_abap( iv_extra = 'locals_imp'
                                                    iv_error = abap_false ). "#EC NOTEXT

    lt_local_macros = mo_files->read_abap( iv_extra = 'macros'
                                           iv_error = abap_false ). "#EC NOTEXT

    lt_test_classes = mo_files->read_abap( iv_extra = 'testclasses'
                                           iv_error = abap_false ). "#EC NOTEXT

    ls_class_key-clsname = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                  CHANGING cg_data = ls_vseoclass ).

    mo_object_oriented_object->create(
      EXPORTING
        iv_package    = iv_package
      CHANGING
        is_properties = ls_vseoclass ).
    mo_object_oriented_object->generate_locals(
      is_key                   = ls_class_key
      iv_force                 = seox_true
      it_local_definitions     = lt_local_definitions
      it_local_implementations = lt_local_implementations
      it_local_macros          = lt_local_macros
      it_local_test_classes    = lt_test_classes ).

    mo_object_oriented_object->deserialize_source(
      is_key               = ls_class_key
      it_source            = lt_source ).

    io_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING cg_data = lt_descriptions ).

    mo_object_oriented_object->update_descriptions(
      is_key          = ls_class_key
      it_descriptions = lt_descriptions ).

    mo_object_oriented_object->add_to_activation_list( is_item = ms_item  ).
  ENDMETHOD.                    "deserialize

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_CLAS IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_intf DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_intf DEFINITION INHERITING FROM lcl_object_clas FINAL.
* todo, CLAS + INTF to be refactored, see:
* https://github.com/larshp/abapGit/issues/21
  PUBLIC SECTION.
    METHODS:
      lif_object~deserialize REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      deserialize_abap REDEFINITION.
ENDCLASS.                    "lcl_object_intf DEFINITION
CLASS lcl_object_intf IMPLEMENTATION.
  METHOD lif_object~deserialize.
    mo_object_oriented_object = lcl_object_oriented_factory=>make( iv_object_type = ms_item-obj_type ).

    deserialize_abap( io_xml     = io_xml
                      iv_package = iv_package ).

    deserialize_docu( io_xml ).
  ENDMETHOD.
  METHOD deserialize_abap.
    DATA: ls_vseointerf   TYPE vseointerf,
          lt_source       TYPE seop_source_string,
          lt_descriptions TYPE ty_seocompotx_tt,
          ls_clskey       TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    lt_source = mo_files->read_abap( ).

    io_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                  CHANGING cg_data = ls_vseointerf ).

    mo_object_oriented_object->create(
     EXPORTING
       iv_package    = iv_package
     CHANGING
       is_properties = ls_vseointerf ).

    mo_object_oriented_object->deserialize_source(
      is_key               = ls_clskey
      it_source            = lt_source ).

    io_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING cg_data = lt_descriptions ).

    mo_object_oriented_object->update_descriptions(
      is_key          = ls_clskey
      it_descriptions = lt_descriptions ).

    mo_object_oriented_object->add_to_activation_list( is_item = ms_item ).
  ENDMETHOD.
ENDCLASS.


CLASS ltd_spy_oo_object DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: lif_object_oriented_object.
    DATA:
      mv_package               TYPE devclass,
      mv_overwrite             TYPE seox_boolean,
      ms_interface_properties  TYPE vseointerf,
      ms_class_properties      TYPE vseoclass,
      ms_locals_key            TYPE seoclskey,
      mt_local_definitions     TYPE rswsourcet,
      mt_local_implementations TYPE rswsourcet,
      mt_local_macros          TYPE rswsourcet,
      mt_local_test_classes    TYPE rswsourcet,
      mv_force                 TYPE seoflag,
      ms_deserialize_key       TYPE seoclskey,
      mt_source                TYPE ty_string_tt,
      ms_item_to_activate      TYPE ty_item,
      mt_descriptions          TYPE ty_seocompotx_tt,
      ms_description_key       TYPE seoclskey,
      mv_text_pool_class_name  TYPE seoclsname,
      mt_text_pool             TYPE textpool_table,
      mv_text_pool_inserted    TYPE abap_bool,
      mt_sotr                  TYPE ty_sotr_tt,
      mt_sotr_package          TYPE devclass,
      mv_docu_object_name      TYPE dokhl-object,
      mv_docu_language         TYPE spras,
      mt_docu_lines            TYPE tlinetab.

ENDCLASS.
CLASS ltd_spy_oo_object IMPLEMENTATION.
  METHOD lif_object_oriented_object~create.
    DATA lv_properties_structure_name TYPE string.
    lv_properties_structure_name = cl_abap_typedescr=>describe_by_data( is_properties )->absolute_name.
    IF lv_properties_structure_name = cl_abap_typedescr=>describe_by_data( ms_interface_properties )->absolute_name.
      ms_interface_properties = is_properties.
    ELSE.
      ms_class_properties     = is_properties.
    ENDIF.
    mv_package                = iv_package.
    mv_overwrite              = iv_overwrite.
  ENDMETHOD.
  METHOD lif_object_oriented_object~generate_locals.
    ms_locals_key            = is_key.
    mt_local_definitions     = it_local_definitions.
    mt_local_implementations = it_local_implementations.
    mt_local_macros          = it_local_macros.
    mt_local_test_classes    = it_local_test_classes.
    mv_force                 = iv_force.
  ENDMETHOD.

  METHOD lif_object_oriented_object~deserialize_source.
    ms_deserialize_key = is_key.
    mt_source          = it_source.
  ENDMETHOD.

  METHOD lif_object_oriented_object~add_to_activation_list.
    ms_item_to_activate = is_item.
  ENDMETHOD.

  METHOD lif_object_oriented_object~update_descriptions.
    ms_description_key = is_key.
    mt_descriptions    = it_descriptions.
  ENDMETHOD.

  METHOD lif_object_oriented_object~insert_text_pool.
    mv_text_pool_inserted = abap_true.
    mv_text_pool_class_name = iv_class_name.
    mt_text_pool = it_text_pool.
    cl_abap_unit_assert=>assert_equals(
      act = iv_language
      exp = sy-langu ).
  ENDMETHOD.

  METHOD lif_object_oriented_object~create_sotr.
    mt_sotr = it_sotr.
    mt_sotr_package = iv_package.
  ENDMETHOD.

  METHOD lif_object_oriented_object~create_documentation.
    mv_docu_object_name = iv_object_name.
    mv_docu_language    = iv_language.
    mt_docu_lines       = it_lines.
  ENDMETHOD.

ENDCLASS.

CLASS ltd_fake_object_files DEFINITION FOR TESTING
  INHERITING FROM  lcl_objects_files.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS read_abap REDEFINITION.
    DATA:
      mt_sources               TYPE seop_source_string,
      mt_local_definitions     TYPE seop_source_string,
      mt_local_implementations TYPE seop_source_string,
      mt_local_macros          TYPE seop_source_string,
      mt_local_test_classes    TYPE seop_source_string.
ENDCLASS.
CLASS ltd_fake_object_files IMPLEMENTATION.
  METHOD read_abap.
    CASE iv_extra.
      WHEN 'locals_def'.
        rt_abap = mt_local_definitions.
      WHEN 'locals_imp'.
        rt_abap = mt_local_implementations.
      WHEN 'macros'.
        rt_abap = mt_local_macros.
      WHEN 'testclasses'.
        rt_abap = mt_local_test_classes.
      WHEN OTHERS.
        rt_abap = mt_sources.
        RETURN.
    ENDCASE.

    cl_abap_unit_assert=>assert_false( iv_error ).
  ENDMETHOD.
  METHOD constructor.
    DATA ls_empty_item TYPE ty_item.
    super->constructor( ls_empty_item ).
    APPEND 'source'         TO me->mt_sources.
    APPEND 'definition'     TO me->mt_local_definitions.
    APPEND 'implementation' TO me->mt_local_implementations.
    APPEND 'macro'          TO me->mt_local_macros.
    APPEND 'test'           TO me->mt_local_test_classes.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_oo_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT .
  PROTECTED SECTION.
    DATA:
      mo_spy_oo_object     TYPE REF TO ltd_spy_oo_object,
      mo_fake_object_files TYPE REF TO ltd_fake_object_files,
      mo_xml_input         TYPE REF TO lcl_xml_input,
      mo_xml_out           TYPE REF TO lcl_xml_output,
      mo_oo_object         TYPE REF TO lif_object,
      ms_item              TYPE ty_item.
    METHODS: when_deserializing
      RAISING
        lcx_exception,
      then_should_deserialize_source,
      given_the_descriptions
        IMPORTING
          it_descriptions TYPE ty_seocompotx_tt
        RAISING
          lcx_exception,
      then_shuld_update_descriptions
        IMPORTING
          it_descriptions TYPE ty_seocompotx_tt,
      then_it_should_add_activation,
      given_documentation_in_xml_as
        IMPORTING
          it_lines TYPE tlinetab
        RAISING
          lcx_exception,
      then_docu_should_be_created
        IMPORTING
          it_lines TYPE tlinetab.

ENDCLASS.
CLASS ltc_oo_test IMPLEMENTATION.

  METHOD then_docu_should_be_created.
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mt_docu_lines
      exp = it_lines ).

    cl_abap_unit_assert=>assert_equals(
       act = mo_spy_oo_object->mv_docu_object_name
       exp = ms_item-obj_name ).

    cl_abap_unit_assert=>assert_equals(
       act = mo_spy_oo_object->mv_docu_language
       exp = sy-langu ).
  ENDMETHOD.

  METHOD given_documentation_in_xml_as.
    mo_xml_out->add(
      iv_name = 'LINES'
      ig_data = it_lines ).
  ENDMETHOD.

  METHOD then_it_should_add_activation.
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->ms_item_to_activate
      exp = ms_item ).
  ENDMETHOD.

  METHOD then_shuld_update_descriptions.
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mt_descriptions
      exp = it_descriptions ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->ms_description_key
      exp = ms_item-obj_name ).
  ENDMETHOD.

  METHOD given_the_descriptions.
    mo_xml_out->add(
      iv_name = 'DESCRIPTIONS'
      ig_data = it_descriptions ).
  ENDMETHOD.

  METHOD then_should_deserialize_source.
    cl_abap_unit_assert=>assert_equals(
       act = mo_spy_oo_object->mt_source
       exp = mo_fake_object_files->mt_sources ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->ms_deserialize_key
      exp = ms_item-obj_name ).
  ENDMETHOD.

  METHOD when_deserializing.
    CREATE OBJECT mo_xml_input
      EXPORTING
        iv_xml = mo_xml_out->render( ).
    mo_oo_object->deserialize(
      iv_package    = 'package_name'
      io_xml        = mo_xml_input ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_class_deserialization DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
INHERITING FROM ltc_oo_test.
  PRIVATE SECTION.
    METHODS:
      setup,
      given_a_class_properties
        RAISING
          lcx_exception,
      then_should_create_class,
      then_it_should_generate_locals,
      should_create_class        FOR TESTING RAISING cx_static_check,
      should_generate_locals     FOR TESTING RAISING cx_static_check,
      should_deserialize_source  FOR TESTING RAISING cx_static_check,
      should_update_descriptions FOR TESTING RAISING cx_static_check,
      should_add_to_activation   FOR TESTING RAISING cx_static_check,
      no_text_pool_no_insert     FOR TESTING RAISING cx_static_check,
      insert_text_pool           FOR TESTING RAISING cx_static_check,
      create_stor_from_xml       FOR TESTING RAISING cx_static_check,
      create_documentation       FOR TESTING RAISING cx_static_check.
    DATA:
      ms_class_properties  TYPE vseoclass.
ENDCLASS.

CLASS ltc_class_deserialization IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_fake_object_files.
    CREATE OBJECT mo_spy_oo_object.
    CREATE OBJECT mo_xml_out.
    lth_oo_factory_injector=>inject( mo_spy_oo_object ).

    ms_item-devclass = 'package_name'.
    ms_item-obj_name = 'zcl_class'.
    ms_item-obj_type = 'CLAS'.

    CREATE OBJECT mo_oo_object TYPE lcl_object_clas
      EXPORTING
        is_item     = ms_item
        iv_language = sy-langu.
    mo_oo_object->mo_files = mo_fake_object_files.
  ENDMETHOD.

  METHOD should_create_class.
    ms_class_properties-clsname = ms_item-obj_name.

    given_a_class_properties( ).

    when_deserializing( ).

    then_should_create_class( ).
  ENDMETHOD.

  METHOD should_generate_locals.
    given_a_class_properties( ).

    when_deserializing( ).

    then_it_should_generate_locals( ).
  ENDMETHOD.

  METHOD should_deserialize_source.
    given_a_class_properties( ).

    when_deserializing( ).

    then_should_deserialize_source( ).
  ENDMETHOD.

  METHOD should_update_descriptions.
    DATA:
      ls_description  TYPE seocompotx,
      lt_descriptions TYPE ty_seocompotx_tt.

    given_a_class_properties( ).

    ls_description-clsname =  ms_item-obj_name.
    ls_description-cmpname = 'a_method'.
    APPEND ls_description TO lt_descriptions.
    given_the_descriptions( lt_descriptions ).

    when_deserializing( ).

    then_shuld_update_descriptions( lt_descriptions ).
  ENDMETHOD.

  METHOD should_add_to_activation.
    given_a_class_properties( ).

    when_deserializing( ).

    then_it_should_add_activation( ).
  ENDMETHOD.

  METHOD given_a_class_properties.
    mo_xml_out->add(
      iv_name = 'VSEOCLASS'
      ig_data = ms_class_properties ).
  ENDMETHOD.

  METHOD then_should_create_class.
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->ms_class_properties
      exp = ms_class_properties ).

    cl_abap_unit_assert=>assert_true( mo_spy_oo_object->mv_overwrite ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mv_package
      exp = 'package_name' ).
  ENDMETHOD.


  METHOD then_it_should_generate_locals.
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->ms_locals_key
      exp = ms_item-obj_name ).
    cl_abap_unit_assert=>assert_true( mo_spy_oo_object->mv_force ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mt_local_definitions
      exp = mo_fake_object_files->mt_local_definitions  ).

    cl_abap_unit_assert=>assert_equals(
       act = mo_spy_oo_object->mt_local_implementations
       exp = mo_fake_object_files->mt_local_implementations ).

    cl_abap_unit_assert=>assert_equals(
       act = mo_spy_oo_object->mt_local_macros
       exp = mo_fake_object_files->mt_local_macros ).

    cl_abap_unit_assert=>assert_equals(
       act = mo_spy_oo_object->mt_local_test_classes
       exp = mo_fake_object_files->mt_local_test_classes ).
  ENDMETHOD.
  METHOD no_text_pool_no_insert.
    given_a_class_properties( ).

    when_deserializing( ).

    cl_abap_unit_assert=>assert_false( mo_spy_oo_object->mv_text_pool_inserted ).
  ENDMETHOD.

  METHOD insert_text_pool.
    DATA: lt_pool_external TYPE textpool_table,
          ls_pool_external TYPE ty_tpool.
    ls_pool_external-id = 'ID'.
    ls_pool_external-key = 'KEY'.
    APPEND ls_pool_external TO lt_pool_external.

    given_a_class_properties( ).

    mo_xml_out->add(
      iv_name = 'TPOOL'
      ig_data = lt_pool_external ).

    when_deserializing( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mt_text_pool
      exp = lt_pool_external ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mv_text_pool_class_name
      exp = 'zcl_class' ).
  ENDMETHOD.

  METHOD create_stor_from_xml.
    DATA:
      lt_sotr TYPE ty_sotr_tt,
      ls_sotr LIKE LINE OF lt_sotr.

    given_a_class_properties( ).

    ls_sotr-header-concept = 'HEADER'.
    APPEND ls_sotr TO lt_sotr.
    mo_xml_out->add(
      iv_name = 'SOTR'
      ig_data = lt_sotr ).

    when_deserializing( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mt_sotr
      exp = lt_sotr ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mt_sotr_package
      exp = 'package_name' ).
  ENDMETHOD.

  METHOD create_documentation.
    DATA: lt_lines TYPE tlinetab,
          ls_line  TYPE LINE OF tlinetab.
    ls_line-tdline = 'Class Line Doc'.
    APPEND ls_line TO lt_lines.

    given_a_class_properties( ).

    given_documentation_in_xml_as( lt_lines ).

    when_deserializing( ).

    then_docu_should_be_created( lt_lines ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_interface_deserialization DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
INHERITING FROM ltc_oo_test.
  PRIVATE SECTION.
    METHODS:
      setup,
      given_an_interface_properties
        RAISING
          lcx_exception,
      then_should_create_interface,
      create_interface    FOR TESTING RAISING cx_static_check,
      update_descriptions FOR TESTING RAISING cx_static_check,
      add_to_activation   FOR TESTING RAISING cx_static_check,
      deserialize_source  FOR TESTING RAISING cx_static_check,
      create_documentation FOR TESTING RAISING cx_static_check.
    DATA:
          ms_interface_properties TYPE vseointerf.
ENDCLASS.
CLASS ltc_interface_deserialization IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_fake_object_files.
    CREATE OBJECT mo_spy_oo_object.
    CREATE OBJECT mo_xml_out.
    lth_oo_factory_injector=>inject( mo_spy_oo_object ).

    ms_item-devclass = 'package_name'.
    ms_item-obj_name = 'zif_interface'.
    ms_item-obj_type = 'INTF'.

    CREATE OBJECT mo_oo_object TYPE lcl_object_intf
      EXPORTING
        is_item     = ms_item
        iv_language = sy-langu.
    mo_oo_object->mo_files = mo_fake_object_files.
  ENDMETHOD.

  METHOD create_interface.
    ms_interface_properties-clsname = ms_item-obj_name.
    given_an_interface_properties( ).

    when_deserializing( ).

    then_should_create_interface( ).
  ENDMETHOD.

  METHOD update_descriptions.
    DATA:
      ls_description  TYPE seocompotx,
      lt_descriptions TYPE ty_seocompotx_tt.

    given_an_interface_properties( ).

    ls_description-clsname =  ms_item-obj_name.
    ls_description-cmpname = 'a_method'.
    APPEND ls_description TO lt_descriptions.
    given_the_descriptions( lt_descriptions ).

    when_deserializing( ).

    then_shuld_update_descriptions( lt_descriptions ).
  ENDMETHOD.

  METHOD add_to_activation.
    given_an_interface_properties( ).

    when_deserializing( ).

    then_it_should_add_activation( ).
  ENDMETHOD.

  METHOD deserialize_source.
    given_an_interface_properties( ).

    when_deserializing( ).

    then_should_deserialize_source( ).
  ENDMETHOD.

  METHOD given_an_interface_properties.
    mo_xml_out->add(
      iv_name = 'VSEOINTERF'
      ig_data = ms_interface_properties ).
  ENDMETHOD.

  METHOD then_should_create_interface.
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->ms_interface_properties
      exp = ms_interface_properties ).

    cl_abap_unit_assert=>assert_true( mo_spy_oo_object->mv_overwrite ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mv_package
      exp = 'package_name' ).
  ENDMETHOD.

  METHOD create_documentation.
    DATA: lt_lines TYPE tlinetab,
          ls_line  TYPE LINE OF tlinetab.
    ls_line-tdline = 'Interface Line Doc'.
    APPEND ls_line TO lt_lines.

    given_an_interface_properties( ).

    given_documentation_in_xml_as( lt_lines ).

    when_deserializing( ).

    then_docu_should_be_created( lt_lines ).
  ENDMETHOD.

ENDCLASS.