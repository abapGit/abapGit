CLASS zcl_abapgit_oo_class_new DEFINITION PUBLIC INHERITING FROM zcl_abapgit_oo_class.

  PUBLIC SECTION.
    METHODS:
      zif_abapgit_oo_object_fnc~create REDEFINITION,
      zif_abapgit_oo_object_fnc~generate_locals REDEFINITION,
      zif_abapgit_oo_object_fnc~deserialize_source REDEFINITION.

  PRIVATE SECTION.

    CLASS-METHODS update_source_index
      IMPORTING
        !iv_clsname TYPE csequence
        !io_scanner TYPE REF TO cl_oo_source_scanner_class .
    CLASS-METHODS update_report
      IMPORTING
        !iv_program       TYPE programm
        !it_source        TYPE string_table
      RETURNING
        VALUE(rv_updated) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS generate_classpool
      IMPORTING
        !iv_name TYPE seoclsname
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS update_meta
      IMPORTING
        !iv_name     TYPE seoclsname
        !iv_exposure TYPE seoexpose
        !it_source   TYPE rswsourcet
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS determine_method_include
      IMPORTING
        !iv_name          TYPE seoclsname
        !iv_method        TYPE seocpdname
      RETURNING
        VALUE(rv_program) TYPE programm
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS init_scanner
      IMPORTING
        !it_source        TYPE zif_abapgit_definitions=>ty_string_tt
        !iv_name          TYPE seoclsname
      RETURNING
        VALUE(ro_scanner) TYPE REF TO cl_oo_source_scanner_class
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS update_full_class_include
      IMPORTING
        !iv_classname TYPE seoclsname
        !it_source    TYPE string_table
        !it_methods   TYPE cl_oo_source_scanner_class=>type_method_implementations .
    CLASS-METHODS create_report
      IMPORTING
        !iv_program      TYPE programm
        !it_source       TYPE string_table
        !iv_extension    TYPE sychar02
        !iv_program_type TYPE sychar01
        !iv_version      TYPE r3state .
    CLASS-METHODS update_cs_number_of_methods
      IMPORTING
        !iv_classname              TYPE seoclsname
        !iv_number_of_impl_methods TYPE i .
ENDCLASS.



CLASS ZCL_ABAPGIT_OO_CLASS_NEW IMPLEMENTATION.


  METHOD create_report.
    INSERT REPORT iv_program FROM it_source EXTENSION TYPE iv_extension STATE iv_version PROGRAM TYPE iv_program_type.
    ASSERT sy-subrc = 0.
  ENDMETHOD.


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
      zcx_abapgit_exception=>raise( 'error from SEO_METHOD_GENERATE_INCLUDE' ).
    ENDIF.

    rv_program = cl_oo_classname_service=>get_method_include( ls_mtdkey ).

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
      zcx_abapgit_exception=>raise( 'error from SEO_CLASS_GENERATE_CLASSPOOL' ).
    ENDIF.

  ENDMETHOD.


  METHOD init_scanner.

    TRY.
        ro_scanner = cl_oo_source_scanner_class=>create_class_scanner(
          clif_name = iv_name
          source    = it_source ).
        ro_scanner->scan( ).
      CATCH cx_clif_scan_error.
        zcx_abapgit_exception=>raise( 'error initializing CLAS scanner' ).
    ENDTRY.

  ENDMETHOD.


  METHOD update_cs_number_of_methods.

    " Indirect access to keep downward compatibility
    DATA lr_cache_entry TYPE REF TO data.

    FIELD-SYMBOLS: <lg_cache_entry> TYPE any,
                   <lg_field>       TYPE any.


    TRY.
        CREATE DATA lr_cache_entry TYPE ('SEO_CS_CACHE').
      CATCH cx_sy_create_data_error.
* does not exist in some older systems
        RETURN.
    ENDTRY.

    ASSIGN lr_cache_entry->* TO <lg_cache_entry>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'CLSNAME' OF STRUCTURE <lg_cache_entry>
           TO <lg_field>.
    ASSERT sy-subrc = 0.
    <lg_field> = iv_classname.

    ASSIGN COMPONENT 'NO_OF_METHOD_IMPLS' OF STRUCTURE <lg_cache_entry>
           TO <lg_field>.
    ASSERT sy-subrc = 0.
    <lg_field> = iv_number_of_impl_methods.

    MODIFY ('SEO_CS_CACHE') FROM <lg_cache_entry>.

  ENDMETHOD.


  METHOD update_full_class_include.

    CONSTANTS: lc_class_source_extension TYPE sychar02 VALUE 'CS',
               lc_include_program_type   TYPE sychar01 VALUE 'I',
               lc_active_version         TYPE r3state VALUE 'A'.


    create_report( iv_program      = cl_oo_classname_service=>get_cs_name( iv_classname )
                   it_source       = it_source
                   iv_extension    = lc_class_source_extension
                   iv_program_type = lc_include_program_type
                   iv_version      = lc_active_version ).

    " Assuming that all methods that were scanned are implemented
    update_cs_number_of_methods( iv_classname              = iv_classname
                                 iv_number_of_impl_methods = lines( it_methods ) ).

  ENDMETHOD.


  METHOD update_meta.

    DATA: lo_update     TYPE REF TO cl_oo_class_section_source,
          ls_clskey     TYPE seoclskey,
          lv_scan_error TYPE seox_boolean.


    ls_clskey-clsname = iv_name.

    TRY.
        CREATE OBJECT lo_update TYPE ('CL_OO_CLASS_SECTION_SOURCE')
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
      CATCH cx_sy_dyn_call_param_not_found.
* downport to 702, see https://github.com/larshp/abapGit/issues/933
* this will READ REPORT instead of using it_source, which should be okay
        CREATE OBJECT lo_update TYPE cl_oo_class_section_source
          EXPORTING
            clskey             = ls_clskey
            exposure           = iv_exposure
            state              = 'A'
          EXCEPTIONS
            class_not_existing = 1
            read_source_error  = 2
            OTHERS             = 3.
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error instantiating CL_OO_CLASS_SECTION_SOURCE' ).
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
      zcx_abapgit_exception=>raise( 'CLAS, error while scanning source' ).
    ENDIF.

* this will update the SEO* database tables
    lo_update->revert_scan_result( ).

    IF iv_exposure = seoc_exposure_public.
      generate_classpool( iv_name ).
    ENDIF.

  ENDMETHOD.


  METHOD update_report.

    DATA: lt_old TYPE string_table.

    READ REPORT iv_program INTO lt_old.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Fatal error. Include { iv_program } should have been created previously!| ).
    ENDIF.

    IF lt_old <> it_source.
      INSERT REPORT iv_program FROM it_source.
      ASSERT sy-subrc = 0.
      rv_updated = abap_true.
    ELSE.
      rv_updated = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD update_source_index.

    CONSTANTS:
      lc_version_active   TYPE r3state VALUE 'A',           "#EC NOTEXT
      lc_version_inactive TYPE r3state VALUE 'I'.           "#EC NOTEXT

    "    dynamic invocation, IF_OO_SOURCE_POS_INDEX_HELPER doesn't exist in 702.
    DATA lo_index_helper TYPE REF TO object.

    TRY.
        CREATE OBJECT lo_index_helper TYPE ('CL_OO_SOURCE_POS_INDEX_HELPER').

        CALL METHOD lo_index_helper->('IF_OO_SOURCE_POS_INDEX_HELPER~CREATE_INDEX_WITH_SCANNER')
          EXPORTING
            class_name = iv_clsname
            version    = lc_version_active
            scanner    = io_scanner.

        CALL METHOD lo_index_helper->('IF_OO_SOURCE_POS_INDEX_HELPER~DELETE_INDEX')
          EXPORTING
            class_name = iv_clsname
            version    = lc_version_inactive.

      CATCH cx_root.
        " it's probably okay to no update the index
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create.
* same as in super class, but with "version = seoc_version_active"

    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        devclass        = iv_package
        overwrite       = iv_overwrite
        version         = seoc_version_active
      CHANGING
        class           = cg_properties
      EXCEPTIONS
        existing        = 1
        is_interface    = 2
        db_error        = 3
        component_error = 4
        no_access       = 5
        other           = 6
        OTHERS          = 7.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SEO_CLASS_CREATE_COMPLETE' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~deserialize_source.

    DATA: lv_updated TYPE abap_bool,
          lv_program TYPE program,
          lo_scanner TYPE REF TO cl_oo_source_scanner_class,
          lt_methods TYPE cl_oo_source_scanner_class=>type_method_implementations,
          lv_method  LIKE LINE OF lt_methods,
          lt_source  TYPE seop_source_string.


    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_key
        version = seoc_version_inactive.

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
          zcx_abapgit_exception=>raise( 'error from GET_METHOD_IMPL_SOURCE' ).
      ENDTRY.
      lv_program = determine_method_include(
        iv_name   = is_key-clsname
        iv_method = lv_method ).

      update_report(
        iv_program = lv_program
        it_source  = lt_source ).
    ENDLOOP.

* full class include
    update_full_class_include( iv_classname = is_key-clsname
                               it_source    = it_source
                               it_methods   = lt_methods ).

    update_source_index(
      iv_clsname = is_key-clsname
      io_scanner = lo_scanner ).

* TODO, perhaps move this call to somewhere else, to be done while cleaning up the CLAS deserialization
    zcl_abapgit_objects_activation=>add(
      iv_type = 'CLAS'
      iv_name = is_key-clsname ).

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~generate_locals.

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
ENDCLASS.
