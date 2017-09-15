*&---------------------------------------------------------------------*
*& Include          ZABAPGIT_OBJECT_CLAS_NEW
*&---------------------------------------------------------------------*

* todo: refactoring
* todo: testing, possible from master branch via experimental switch

CLASS lcl_oo_class_new DEFINITION INHERITING FROM lcl_oo_class.

  PUBLIC SECTION.
    METHODS:
      lif_oo_object_fnc~create REDEFINITION,
      lif_oo_object_fnc~generate_locals REDEFINITION,
      lif_oo_object_fnc~deserialize_source REDEFINITION.

  PRIVATE SECTION.
    CLASS-METHODS:
      update_report
        IMPORTING
          iv_program        TYPE programm
          it_source         TYPE string_table
        RETURNING
          VALUE(rv_updated) TYPE abap_bool,
      generate_classpool
        IMPORTING
          iv_name TYPE seoclsname
        RAISING
          lcx_exception,
      update_meta
        IMPORTING
          iv_name     TYPE seoclsname
          iv_exposure TYPE seoexpose
          it_source   TYPE rswsourcet
        RAISING
          lcx_exception,
      determine_method_include
        IMPORTING
          iv_name           TYPE seoclsname
          iv_method         TYPE seocpdname
        RETURNING
          VALUE(rv_program) TYPE programm
        RAISING
          lcx_exception,
      init_scanner
        IMPORTING
          it_source         TYPE lif_defs=>ty_string_tt
          iv_name           TYPE seoclsname
        RETURNING
          VALUE(ro_scanner) TYPE REF TO cl_oo_source_scanner_class.

ENDCLASS.

CLASS lcl_oo_class_new IMPLEMENTATION.

  METHOD determine_method_include.

    DATA: ls_mtdkey TYPE seocpdkey.


    ls_mtdkey-clsname = iv_name.
    ls_mtdkey-cpdname = iv_method.

    cl_oo_classname_service=>get_method_include(
      EXPORTING
        mtdkey              = ls_mtdkey
      RECEIVING
        result              = rv_program
      EXCEPTIONS
        method_not_existing = 1 ).
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_METHOD_GENERATE_INCLUDE'
      EXPORTING
        suppress_mtdkey_check          = seox_true
        mtdkey                         = ls_mtdkey
      EXCEPTIONS
        not_existing                   = 1
        model_only                     = 2
        include_existing               = 3
        method_imp_not_generated       = 4
        method_imp_not_initialised     = 5
        _internal_class_not_existing   = 6
        _internal_method_overflow      = 7
        cancelled                      = 8
        method_is_abstract_implemented = 9
        method_is_final_implemented    = 10
        internal_error_insert_report   = 11
        OTHERS                         = 12.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from SEO_METHOD_GENERATE_INCLUDE' ).
    ENDIF.

    rv_program = cl_oo_classname_service=>get_method_include( ls_mtdkey ).

  ENDMETHOD.

  METHOD lif_oo_object_fnc~create.
* same as in super class, but with "version = seoc_version_active"

    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        devclass        = iv_package
        overwrite       = iv_overwrite
        version         = seoc_version_active
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

  METHOD init_scanner.

    ro_scanner = cl_oo_source_scanner_class=>create_class_scanner(
      clif_name = iv_name
      source    = it_source ).
    ro_scanner->scan( ).

  ENDMETHOD.

  METHOD update_report.

    DATA: lt_old TYPE string_table.

    READ REPORT iv_program INTO lt_old.
    ASSERT sy-subrc = 0. " include should have been created previously
    IF lt_old <> it_source.
      INSERT REPORT iv_program FROM it_source.
      ASSERT sy-subrc = 0.
      rv_updated = abap_true.
    ELSE.
      rv_updated = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD lif_oo_object_fnc~generate_locals.

    DATA: lv_program TYPE programm.


    lv_program = cl_oo_classname_service=>get_ccdef_name( is_key-clsname ).
    update_report( iv_program = lv_program
                   it_source  = it_local_definitions ).

    lv_program = cl_oo_classname_service=>get_ccimp_name( is_key-clsname ).
    update_report( iv_program = lv_program
                   it_source  = it_local_implementations ).

    lv_program = cl_oo_classname_service=>get_ccmac_name( is_key-clsname ).
    update_report( iv_program = lv_program
                   it_source  = it_local_macros ).

    IF lines( it_local_test_classes ) > 0.
      lv_program = cl_oo_classname_service=>get_ccau_name( is_key-clsname ).
      update_report( iv_program = lv_program
                     it_source  = it_local_test_classes ).
    ENDIF.

  ENDMETHOD.

  METHOD update_meta.

    DATA: lo_update     TYPE REF TO cl_oo_class_section_source,
          ls_clskey     TYPE seoclskey,
          lv_scan_error TYPE seox_boolean.


    ls_clskey-clsname = iv_name.

    CREATE OBJECT lo_update
      EXPORTING
        clskey                        = ls_clskey
        exposure                      = iv_exposure
        state                         = 'A'
        source                        = it_source
        suppress_constrctr_generation = seox_true
      EXCEPTIONS
        class_not_existing            = 1
        read_source_error             = 2
        OTHERS                        = 3.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error instantiating CL_OO_CLASS_SECTION_SOURCE' ).
    ENDIF.

    lo_update->set_dark_mode( seox_true ).
    TRY.
        CALL METHOD lo_update->('SET_AMDP_SUPPORT')
          EXPORTING
            enabled = abap_true.
      CATCH cx_sy_dyn_call_illegal_method.
* AMDP not supported in this system, ignore error
    ENDTRY.
    lo_update->scan_section_source(
      RECEIVING
        scan_error             = lv_scan_error
      EXCEPTIONS
        scan_abap_source_error = 1
        OTHERS                 = 2 ).
    IF sy-subrc <> 0 OR lv_scan_error = abap_true.
      lcx_exception=>raise( 'CLAS, error while scanning source' ).
    ENDIF.

* this will update the SEO* database tables
    lo_update->revert_scan_result( ).

    IF iv_exposure = seoc_exposure_public.
      generate_classpool( iv_name ).
    ENDIF.

  ENDMETHOD.

  METHOD generate_classpool.

    DATA: ls_clskey TYPE seoclskey.

    ls_clskey-clsname = iv_name.

    CALL FUNCTION 'SEO_CLASS_GENERATE_CLASSPOOL'
      EXPORTING
        clskey                        = ls_clskey
        suppress_corr                 = seox_true
      EXCEPTIONS
        not_existing                  = 1
        model_only                    = 2
        class_pool_not_generated      = 3
        class_stment_not_generated    = 4
        locals_not_generated          = 5
        macros_not_generated          = 6
        public_sec_not_generated      = 7
        protected_sec_not_generated   = 8
        private_sec_not_generated     = 9
        typeref_not_generated         = 10
        class_pool_not_initialised    = 11
        class_stment_not_initialised  = 12
        locals_not_initialised        = 13
        macros_not_initialised        = 14
        public_sec_not_initialised    = 15
        protected_sec_not_initialised = 16
        private_sec_not_initialised   = 17
        typeref_not_initialised       = 18
        _internal_class_overflow      = 19
        OTHERS                        = 20.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from SEO_CLASS_GENERATE_CLASSPOOL' ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_oo_object_fnc~deserialize_source.

    DATA: lv_updated TYPE abap_bool,
          lv_program TYPE program,
          lo_scanner TYPE REF TO cl_oo_source_scanner_class,
          lt_methods TYPE cl_oo_source_scanner_class=>type_method_implementations,
          lv_method  LIKE LINE OF lt_methods,
          lt_source  TYPE seop_source_string.


    lo_scanner = init_scanner(
      it_source = it_source
      iv_name   = is_key-clsname ).

* public
    lt_source = lo_scanner->get_public_section_source( ).
    lv_program = cl_oo_classname_service=>get_pubsec_name( is_key-clsname ).
    lv_updated = update_report( iv_program = lv_program
                                it_source  = lt_source ).
    IF lv_updated = abap_true.
      update_meta( iv_name     = is_key-clsname
                   iv_exposure = seoc_exposure_public
                   it_source   = lt_source ).
    ENDIF.

* protected
    lt_source = lo_scanner->get_protected_section_source( ).
    lv_program = cl_oo_classname_service=>get_prosec_name( is_key-clsname ).
    lv_updated = update_report( iv_program = lv_program
                                it_source  = lt_source ).
    IF lv_updated = abap_true.
      update_meta( iv_name     = is_key-clsname
                   iv_exposure = seoc_exposure_protected
                   it_source   = lt_source ).
    ENDIF.

* private
    lt_source = lo_scanner->get_private_section_source( ).
    lv_program = cl_oo_classname_service=>get_prisec_name( is_key-clsname ).
    lv_updated = update_report( iv_program = lv_program
                                it_source  = lt_source ).
    IF lv_updated = abap_true.
      update_meta( iv_name     = is_key-clsname
                   iv_exposure = seoc_exposure_private
                   it_source   = lt_source ).
    ENDIF.

* methods
    lt_methods = lo_scanner->get_method_implementations( ).

    LOOP AT lt_methods INTO lv_method.
      TRY.
          lt_source = lo_scanner->get_method_impl_source( lv_method ).
        CATCH cx_oo_clif_component.
          lcx_exception=>raise( 'error from GET_METHOD_IMPL_SOURCE' ).
      ENDTRY.
      lv_program = determine_method_include(
        iv_name   = is_key-clsname
        iv_method = lv_method ).

      update_report(
        iv_program = lv_program
        it_source  = lt_source ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_clas_new DEFINITION INHERITING FROM lcl_object_clas.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item     TYPE lif_defs=>ty_item
        iv_language TYPE spras.

  PROTECTED SECTION.
    METHODS:
      deserialize_abap REDEFINITION.

ENDCLASS.

CLASS lcl_object_clas_new IMPLEMENTATION.

  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    CREATE OBJECT mo_object_oriented_object_fct TYPE lcl_oo_class_new.
  ENDMETHOD.

  METHOD deserialize_abap.
* same as in lcl_object_clas, but without "mo_object_oriented_object_fct->add_to_activation_list"

    DATA: ls_vseoclass             TYPE vseoclass,
          lt_source                TYPE seop_source_string,
          lt_local_definitions     TYPE seop_source_string,
          lt_local_implementations TYPE seop_source_string,
          lt_local_macros          TYPE seop_source_string,
          lt_test_classes          TYPE seop_source_string,
          lt_descriptions          TYPE lif_defs=>ty_seocompotx_tt,
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

    mo_object_oriented_object_fct->create(
      EXPORTING
        iv_package    = iv_package
      CHANGING
        is_properties = ls_vseoclass ).

    mo_object_oriented_object_fct->generate_locals(
      is_key                   = ls_class_key
      iv_force                 = seox_true
      it_local_definitions     = lt_local_definitions
      it_local_implementations = lt_local_implementations
      it_local_macros          = lt_local_macros
      it_local_test_classes    = lt_test_classes ).

    mo_object_oriented_object_fct->deserialize_source(
      is_key    = ls_class_key
      it_source = lt_source ).

    io_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING cg_data = lt_descriptions ).

    mo_object_oriented_object_fct->update_descriptions(
      is_key          = ls_class_key
      it_descriptions = lt_descriptions ).

  ENDMETHOD.

ENDCLASS.
