CLASS zcl_abapgit_oo_deserializer DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS deserialize_abap_clif_source
      IMPORTING
        !iv_object_type TYPE tadir-object
        !is_key         TYPE seoclskey
        !it_source      TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !is_key    TYPE seoclskey
        !it_source TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_definition
      RAISING
        zcx_abapgit_exception
        cx_sy_dyn_call_error .
    METHODS deserialize_implementation
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_full_class_include
      IMPORTING
        !it_source TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception .
    METHODS update_source_index
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-METHODS init_scanner
      IMPORTING
        !it_source        TYPE zif_abapgit_definitions=>ty_string_tt
        !iv_name          TYPE seoclsname
      RETURNING
        VALUE(ro_scanner) TYPE REF TO cl_oo_source_scanner_class
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS update_report
      IMPORTING
        !iv_program       TYPE programm
        !it_source        TYPE string_table
      RETURNING
        VALUE(rv_updated) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS update_meta
      IMPORTING
        !iv_name     TYPE seoclsname
        !iv_exposure TYPE seoexpose
        !it_source   TYPE rswsourcet
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS generate_classpool
      IMPORTING
        !iv_name TYPE seoclsname
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

    DATA:
      ms_key     TYPE seoclskey,
      mo_scanner TYPE REF TO cl_oo_source_scanner_class.

ENDCLASS.



CLASS zcl_abapgit_oo_deserializer IMPLEMENTATION.


  METHOD deserialize_abap_clif_source.

    DATA: lo_deserializer TYPE REF TO zcl_abapgit_oo_deserializer,
          lv_done         TYPE abap_bool.


    lv_done = zcl_abapgit_exit=>get_instance( )->custom_deserialize_abap_clif(
                                              iv_object_type   = iv_object_type
                                              is_key           = is_key
                                              it_source        = it_source ).
    IF lv_done = abap_true.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_deserializer
      EXPORTING
        is_key    = is_key
        it_source = it_source.

    lo_deserializer->deserialize_definition( ).

    lo_deserializer->deserialize_implementation( ).

    lo_deserializer->deserialize_full_class_include( it_source = it_source ).

    lo_deserializer->update_source_index( ).

  ENDMETHOD.

  METHOD constructor.

    me->ms_key = is_key.

    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_key
        version = seoc_version_inactive.

    mo_scanner = init_scanner(
      it_source = it_source
      iv_name   = is_key-clsname ).

  ENDMETHOD.


  METHOD create_report.
    INSERT REPORT iv_program FROM it_source EXTENSION TYPE iv_extension STATE iv_version PROGRAM TYPE iv_program_type.
    ASSERT sy-subrc = 0.
  ENDMETHOD.


  METHOD deserialize_definition.

    DATA: lv_updated TYPE abap_bool,
          lv_program TYPE program,
          lt_public  TYPE seop_source_string,
          lt_auxsrc  TYPE seop_source_string,
          lt_source  TYPE seop_source_string.

* public
    lt_public = mo_scanner->get_public_section_source( ).
    IF lt_public IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_pubsec_name( ms_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  it_source  = lt_public ).
      IF lv_updated = abap_true.
        update_meta( iv_name     = ms_key-clsname
                     iv_exposure = seoc_exposure_public
                     it_source   = lt_public ).
      ENDIF.
    ENDIF.

* protected
    lt_source = mo_scanner->get_protected_section_source( ).
    IF lt_source IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_prosec_name( ms_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  it_source  = lt_source ).
      IF lv_updated = abap_true.
        lt_auxsrc = lt_public.
        APPEND LINES OF lt_source TO lt_auxsrc.

        update_meta( iv_name     = ms_key-clsname
                     iv_exposure = seoc_exposure_protected
                     it_source   = lt_auxsrc ).
      ENDIF.
    ENDIF.

* private
    lt_source = mo_scanner->get_private_section_source( ).
    IF lt_source IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_prisec_name( ms_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  it_source  = lt_source ).
      IF lv_updated = abap_true.
        lt_auxsrc = lt_public.
        APPEND LINES OF lt_source TO lt_auxsrc.

        update_meta( iv_name     = ms_key-clsname
                     iv_exposure = seoc_exposure_private
                     it_source   = lt_auxsrc ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD deserialize_full_class_include.

    DATA: lt_methods TYPE cl_oo_source_scanner_class=>type_method_implementations.
    CONSTANTS: lc_class_source_extension TYPE sychar02 VALUE 'CS',
               lc_include_program_type   TYPE sychar01 VALUE 'I',
               lc_active_version         TYPE r3state VALUE 'A'.


    create_report( iv_program      = cl_oo_classname_service=>get_cs_name( ms_key-clsname )
                   it_source       = it_source
                   iv_extension    = lc_class_source_extension
                   iv_program_type = lc_include_program_type
                   iv_version      = lc_active_version ).

    lt_methods = mo_scanner->get_method_implementations( ).

    " Assuming that all methods that were scanned are implemented
    update_cs_number_of_methods( iv_classname              = ms_key-clsname
                                 iv_number_of_impl_methods = lines( lt_methods ) ).

  ENDMETHOD.


  METHOD deserialize_implementation.

    DATA: lv_program TYPE program,
          lt_methods TYPE cl_oo_source_scanner_class=>type_method_implementations,
          lv_method  LIKE LINE OF lt_methods,
          lt_source  TYPE seop_source_string.

* methods
    lt_methods = mo_scanner->get_method_implementations( ).

    LOOP AT lt_methods INTO lv_method.
      TRY.
          lt_source = mo_scanner->get_method_impl_source( lv_method ).
        CATCH cx_oo_clif_component.
          zcx_abapgit_exception=>raise( 'error from GET_METHOD_IMPL_SOURCE' ).
      ENDTRY.
      lv_program = determine_method_include(
        iv_name   = ms_key-clsname
        iv_method = lv_method ).

      update_report(
        iv_program = lv_program
        it_source  = lt_source ).
    ENDLOOP.

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
        suppress_mtdkey_check          = abap_true
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
      zcx_abapgit_exception=>raise( |Error from SEO_METHOD_GENERATE_INCLUDE. Subrc = { sy-subrc }| ).
    ENDIF.

    rv_program = cl_oo_classname_service=>get_method_include( ls_mtdkey ).

  ENDMETHOD.


  METHOD generate_classpool.

    DATA: ls_clskey TYPE seoclskey.

    ls_clskey-clsname = iv_name.

    CALL FUNCTION 'SEO_CLASS_GENERATE_CLASSPOOL'
      EXPORTING
        clskey                        = ls_clskey
        suppress_corr                 = abap_true
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
      zcx_abapgit_exception=>raise( |Error from SEO_CLASS_GENERATE_CLASSPOOL. Subrc = { sy-subrc }| ).
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


  METHOD update_meta.

    DATA: lo_update     TYPE REF TO cl_oo_class_section_source,
          ls_clskey     TYPE seoclskey,
          lv_scan_error TYPE abap_bool.


    ls_clskey-clsname = iv_name.

    TRY.
        CREATE OBJECT lo_update TYPE ('CL_OO_CLASS_SECTION_SOURCE')
          EXPORTING
            clskey                        = ls_clskey
            exposure                      = iv_exposure
            state                         = 'A'
            source                        = it_source
            suppress_constrctr_generation = abap_true
          EXCEPTIONS
            class_not_existing            = 1
            read_source_error             = 2
            OTHERS                        = 3 ##SUBRC_OK.
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
      zcx_abapgit_exception=>raise( |Error instantiating CL_OO_CLASS_SECTION_SOURCE. Subrc = { sy-subrc }| ).
    ENDIF.

    lo_update->set_dark_mode( abap_true ).
    TRY.
        CALL METHOD lo_update->('SET_AMDP_SUPPORT')
          EXPORTING
            enabled = abap_true.
      CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
* AMDP not supported in this system, ignore error
    ENDTRY.
    lo_update->scan_section_source(
      RECEIVING
        scan_error             = lv_scan_error
      EXCEPTIONS
        scan_abap_source_error = 1
        OTHERS                 = 2 ).
    IF sy-subrc <> 0 OR lv_scan_error = abap_true.
      zcx_abapgit_exception=>raise( |CLAS, error while scanning source. Subrc = { sy-subrc }| ).
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
            class_name = ms_key-clsname
            version    = lc_version_active
            scanner    = mo_scanner.

        CALL METHOD lo_index_helper->('IF_OO_SOURCE_POS_INDEX_HELPER~DELETE_INDEX')
          EXPORTING
            class_name = ms_key-clsname
            version    = lc_version_inactive.

      CATCH cx_root.
        " it's probably okay to no update the index
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
