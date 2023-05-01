CLASS zcl_abapgit_oo_class DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_oo_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_abapgit_oo_object_fnc~create
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~create_sotr
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~delete
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~deserialize_source
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~generate_locals
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~get_class_properties
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~get_includes
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~insert_text_pool
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~read_sotr
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~read_text_pool
        REDEFINITION .
    METHODS zif_abapgit_oo_object_fnc~exists
        REDEFINITION .
  PROTECTED SECTION.

    TYPES:
      ty_char1 TYPE c LENGTH 1 .
    TYPES:
      ty_char2 TYPE c LENGTH 2 .
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
        !iv_extension    TYPE ty_char2
        !iv_program_type TYPE ty_char1
        !iv_version      TYPE r3state .
    CLASS-METHODS update_cs_number_of_methods
      IMPORTING
        !iv_classname              TYPE seoclsname
        !iv_number_of_impl_methods TYPE i .
    CLASS-METHODS delete_report
      IMPORTING
        !iv_program TYPE programm .
    CLASS-METHODS get_method_includes
      IMPORTING
        !iv_classname      TYPE seoclsname
      RETURNING
        VALUE(rt_includes) TYPE seop_methods_w_include.
    CLASS-METHODS repair_classpool
      IMPORTING
        !is_key TYPE seoclskey
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS repair_redefinitions
      IMPORTING
        !is_key TYPE seoclskey
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_oo_class IMPLEMENTATION.


  METHOD create_report.
    INSERT REPORT iv_program FROM it_source EXTENSION TYPE iv_extension STATE iv_version PROGRAM TYPE iv_program_type.
    ASSERT sy-subrc = 0.
  ENDMETHOD.


  METHOD delete_report.
    DELETE REPORT iv_program ##SUBRC_OK.
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
      zcx_abapgit_exception=>raise_t100( ).
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
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_method_includes.
    " get method includes for implemented interfaces
    " this will contain also leftover includes for deleted interface methods
    rt_includes = cl_oo_classname_service=>get_all_method_includes( iv_classname ).
  ENDMETHOD.


  METHOD init_scanner.

    DATA: lx_exc       TYPE REF TO cx_root,
          lv_message   TYPE string,
          lv_classname TYPE abap_abstypename.
    FIELD-SYMBOLS: <lv_line> TYPE i.

    TRY.
        ro_scanner = cl_oo_source_scanner_class=>create_class_scanner(
          clif_name = iv_name
          source    = it_source ).
        ro_scanner->scan( ).
      CATCH cx_clif_scan_error.
        zcx_abapgit_exception=>raise( 'error initializing CLAS scanner' ).
      CATCH cx_root INTO lx_exc.
        lv_classname = cl_abap_classdescr=>get_class_name( lx_exc ).
        IF lv_classname = '\CLASS=CX_OO_CLIF_SCAN_ERROR_DETAIL'.
          ASSIGN lx_exc->('SOURCE_POSITION-LINE') TO <lv_line>.
          ASSERT sy-subrc = 0.
          lv_message = |{ lx_exc->get_text( ) }, line { <lv_line> }|.
        ELSE.
          lv_message = lx_exc->get_text( ).
        ENDIF.
        zcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.


  METHOD repair_classpool.

    CALL FUNCTION 'SEO_CLASS_REPAIR_CLASSPOOL'
      EXPORTING
        clskey       = is_key
      EXCEPTIONS
        not_existing = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error repairing class { is_key-clsname }| ).
    ENDIF.

  ENDMETHOD.


  METHOD repair_redefinitions.

    " Same logic as SE24 > Utilities > Clean-up > Redefinitions (LSEODCCO)

    DATA:
      lt_inheritance     TYPE vseoextend,
      lt_redefinitions   TYPE seor_redefinitions_r,
      ls_cpdkey          TYPE seocpdkey,
      lv_tabix           TYPE sy-tabix,
      lv_exposure        TYPE n LENGTH 1,
      lv_update          TYPE abap_bool,
      lv_local_component TYPE abap_bool.

    FIELD-SYMBOLS <ls_redef> TYPE seoredef.

    CALL FUNCTION 'SEO_CLASS_TYPEINFO_GET'
      EXPORTING
        clskey        = is_key
        version       = seoc_version_active
      IMPORTING
        inheritance   = lt_inheritance
        redefinitions = lt_redefinitions
      EXCEPTIONS
        not_existing  = 1
        is_interface  = 2
        model_only    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " check redefinitions validity
    LOOP AT lt_redefinitions ASSIGNING <ls_redef>.
      lv_tabix = sy-tabix.

      ls_cpdkey-clsname = is_key-clsname.
      ls_cpdkey-cpdname = <ls_redef>-mtdname.

      CALL FUNCTION 'SEO_COMPONENT_BY_INHERITANCE'
        EXPORTING
          cpdkey             = ls_cpdkey
          version            = seoc_version_active
        IMPORTING
          exposure           = lv_exposure
          is_local_component = lv_local_component
        EXCEPTIONS
          not_existing       = 1
          model_only         = 2
          OTHERS             = 3.
      IF sy-subrc <> 0.
        DELETE lt_redefinitions INDEX lv_tabix.
        lv_update = abap_true.
      ELSEIF <ls_redef>-exposure <> lv_exposure.
        <ls_redef>-exposure = lv_exposure.
        lv_update = abap_true.
      ELSEIF lv_local_component = abap_true AND <ls_redef>-attvalue IS INITIAL AND
             <ls_redef>-mtdabstrct IS INITIAL AND <ls_redef>-mtdfinal IS INITIAL.
        DELETE lt_redefinitions INDEX lv_tabix.
        lv_update = abap_true.
      ENDIF.
    ENDLOOP.

    IF lv_update = abap_true.
      CALL FUNCTION 'SEO_INHERITANC_CHANGE_F_DATA'
        EXPORTING
          save            = abap_false
        CHANGING
          inheritance     = lt_inheritance
          redefinitions   = lt_redefinitions
        EXCEPTIONS
          not_existing    = 1
          deleted         = 2
          is_comprising   = 3
          is_implementing = 4
          not_changed     = 5
          db_error        = 6
          OTHERS          = 7.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error repairing redefinitions for { is_key-clsname }| ).
      ENDIF.

      CALL FUNCTION 'SEO_CLIF_SAVE_ALL'
        EXPORTING
          cifkey                   = is_key
        EXCEPTIONS
          not_existing             = 1
          nothing_to_do            = 2
          access_error             = 3
          db_error                 = 4
          error_in_code_generation = 5
          OTHERS                   = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error repairing redefinitions for { is_key-clsname }| ).
      ENDIF.
    ENDIF.

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

    CONSTANTS: lc_class_source_extension TYPE c LENGTH 2 VALUE 'CS',
               lc_include_program_type   TYPE c LENGTH 1 VALUE 'I',
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
          lx_error      TYPE REF TO cx_oo_source_save_failure,
          ls_clskey     TYPE seoclskey,
          lv_scan_error TYPE abap_bool.


    ls_clskey-clsname = iv_name.

    TRY.
        CALL FUNCTION 'SEO_BUFFER_REFRESH'
          EXPORTING
            cifkey  = ls_clskey
            version = seoc_version_active.
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
* downport to 702, see https://github.com/abapGit/abapGit/issues/933
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
      zcx_abapgit_exception=>raise_t100( ).
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
    TRY.
        lo_update->revert_scan_result( ).
      CATCH cx_oo_source_save_failure INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

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
      lc_version_active   TYPE r3state VALUE 'A',
      lc_version_inactive TYPE r3state VALUE 'I'.

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

    DATA:
      lt_vseoattrib TYPE seoo_attributes_r,
      ls_class_key  TYPE seoclskey,
      ls_properties TYPE vseoclass,
      lt_attributes TYPE zif_abapgit_definitions=>ty_obj_attribute_tt.

    FIELD-SYMBOLS: <lv_clsname> TYPE seoclsname.

    ASSIGN COMPONENT 'CLSNAME' OF STRUCTURE cg_properties TO <lv_clsname>.
    ASSERT sy-subrc = 0.

    " Get existing class properties and attributes and check if the class
    " needs to be created/updated (or is the same)
    IF iv_check = abap_true.
      ls_class_key-clsname = <lv_clsname>.
      ls_properties = zif_abapgit_oo_object_fnc~get_class_properties( ls_class_key ).
      lt_attributes = zif_abapgit_oo_object_fnc~read_attributes( <lv_clsname> ).

      IF ls_properties = cg_properties AND lt_attributes = it_attributes.
        RETURN.
      ENDIF.
    ENDIF.

    lt_vseoattrib = convert_attrib_to_vseoattrib(
                      iv_clsname    = <lv_clsname>
                      it_attributes = it_attributes ).

    " Hardcode STATE (#2612)
    ls_properties = cg_properties.
    ls_properties-state = seoc_state_implemented.

    TRY.
        CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = abap_true
            version         = seoc_version_active
            suppress_dialog = abap_true " Parameter missing in 702
          CHANGING
            class           = ls_properties
            attributes      = lt_vseoattrib
          EXCEPTIONS
            existing        = 1
            is_interface    = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
      CATCH cx_sy_dyn_call_param_not_found.
        CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = abap_true
            version         = seoc_version_active
          CHANGING
            class           = ls_properties
            attributes      = lt_vseoattrib
          EXCEPTIONS
            existing        = 1
            is_interface    = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
    ENDTRY.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create_sotr.
    zcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = ii_xml ).
    zcl_abapgit_sots_handler=>create_sots(
      iv_package = iv_package
      io_xml     = ii_xml ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~delete.

    " SEO_CLASS_DELETE_COMPLETE deletes OTR usage, only
    " Use handler to also delete OTR header and texts
    zcl_abapgit_sotr_handler=>delete_sotr(
      iv_pgmid    = 'LIMU'
      iv_object   = 'CPUB'
      iv_obj_name = is_deletion_key-clsname ).
    zcl_abapgit_sots_handler=>delete_sots(
      iv_pgmid    = 'LIMU'
      iv_object   = 'CPUB'
      iv_obj_name = is_deletion_key-clsname ).

    CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
      EXPORTING
        clskey       = is_deletion_key
      EXCEPTIONS
        not_existing = 1
        is_interface = 2
        db_error     = 3
        no_access    = 4
        other        = 5
        OTHERS       = 6.
    IF sy-subrc = 1.
* ignore deletion of objects that does not exist
* this can happen when the SXCI object is deleted before the implementing CLAS
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~deserialize_source.

    DATA: lv_updated TYPE abap_bool,
          lv_program TYPE program,
          lo_scanner TYPE REF TO cl_oo_source_scanner_class,
          lt_methods TYPE cl_oo_source_scanner_class=>type_method_implementations,
          lt_incls   TYPE seop_methods_w_include,
          lv_method  LIKE LINE OF lt_methods,
          lt_public  TYPE seop_source_string,
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
    lt_public = lo_scanner->get_public_section_source( ).
    IF lt_public IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_pubsec_name( is_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  it_source  = lt_public ).
      IF lv_updated = abap_true.
        update_meta( iv_name     = is_key-clsname
                     iv_exposure = seoc_exposure_public
                     it_source   = lt_public ).
      ENDIF.
    ENDIF.

* protected
    lt_source = lo_scanner->get_protected_section_source( ).
    IF lt_source IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_prosec_name( is_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  it_source  = lt_source ).
      IF lv_updated = abap_true.
        update_meta( iv_name     = is_key-clsname
                     iv_exposure = seoc_exposure_protected
                     it_source   = lt_source ).
      ENDIF.
    ENDIF.

* private
    lt_source = lo_scanner->get_private_section_source( ).
    IF lt_source IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_prisec_name( is_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  it_source  = lt_source ).
      IF lv_updated = abap_true.
        update_meta( iv_name     = is_key-clsname
                     iv_exposure = seoc_exposure_private
                     it_source   = lt_source ).
      ENDIF.
    ENDIF.

* methods
    lt_methods = lo_scanner->get_method_implementations( ).

    lt_incls = get_method_includes( is_key-clsname ).

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

      " If method was implemented before, remove from list
      DELETE lt_incls WHERE cpdkey-clsname = is_key-clsname AND cpdkey-cpdname = lv_method.
    ENDLOOP.

* full class include
    update_full_class_include( iv_classname = is_key-clsname
                               it_source    = it_source
                               it_methods   = lt_methods ).

    " If there are leftover method includes, then class needs to be repaired
    " which will delete the obsolete includes
    IF lt_incls IS NOT INITIAL.
      repair_classpool( is_key ).
      repair_redefinitions( is_key ).
    ENDIF.

    update_source_index(
      iv_clsname = is_key-clsname
      io_scanner = lo_scanner ).

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~exists.
    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = is_object_name
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    rv_exists = boolc( sy-subrc = 0 OR sy-subrc = 4 ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~generate_locals.

    DATA: lv_program TYPE programm.

    IF lines( it_local_definitions ) > 0.
      lv_program = cl_oo_classname_service=>get_ccdef_name( is_key-clsname ).
      update_report( iv_program = lv_program
                     it_source  = it_local_definitions ).
    ENDIF.

    IF lines( it_local_implementations ) > 0.
      lv_program = cl_oo_classname_service=>get_ccimp_name( is_key-clsname ).
      update_report( iv_program = lv_program
                     it_source  = it_local_implementations ).
    ENDIF.

    IF lines( it_local_macros ) > 0.
      lv_program = cl_oo_classname_service=>get_ccmac_name( is_key-clsname ).
      update_report( iv_program = lv_program
                     it_source  = it_local_macros ).
    ENDIF.

    lv_program = cl_oo_classname_service=>get_ccau_name( is_key-clsname ).
    IF lines( it_local_test_classes ) > 0.
      update_report( iv_program = lv_program
                     it_source  = it_local_test_classes ).
    ELSE.
      " Drop the include to remove left-over test classes
      delete_report( lv_program ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_class_properties.
    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = is_class_key
        version      = seoc_version_active
      IMPORTING
        class        = rs_class_properties
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 1.
      RETURN. " in case only inactive version exists
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CLEAR:
      " TODO 2023-08-01: Clear rs_class_properties-state (#2612)
      rs_class_properties-uuid,
      rs_class_properties-author,
      rs_class_properties-createdon,
      rs_class_properties-changedby,
      rs_class_properties-changedon,
      rs_class_properties-r3release,
      rs_class_properties-chgdanyby,
      rs_class_properties-chgdanyon,
      rs_class_properties-clsfinal,
      rs_class_properties-clsabstrct,
      rs_class_properties-exposure,
      rs_class_properties-version.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_includes.
* note: includes returned might not exist
* method cl_oo_classname_service=>GET_ALL_CLASS_INCLUDES does not exist in 702

    DATA: lv_class_name TYPE seoclsname,
          lt_methods    TYPE seop_methods_w_include.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_methods.

    lv_class_name = iv_object_name.

    APPEND cl_oo_classname_service=>get_ccdef_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ccmac_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ccimp_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_cl_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ccau_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_pubsec_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_prosec_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_prisec_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_classpool_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ct_name( lv_class_name ) TO rt_includes.

* skip the CS include, as it is sometimes generated on the fly instead of
* when the methods are changed

    cl_oo_classname_service=>get_all_method_includes(
      EXPORTING
        clsname            = lv_class_name
      RECEIVING
        result             = lt_methods
      EXCEPTIONS
        class_not_existing = 1 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Class { lv_class_name } not existing| ).
    ENDIF.

    LOOP AT lt_methods ASSIGNING <ls_method>.
      APPEND <ls_method>-incname TO rt_includes.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~insert_text_pool.
    DATA: lv_cp TYPE program.

    lv_cp = cl_oo_classname_service=>get_classpool_name( iv_class_name ).

    INSERT TEXTPOOL lv_cp
      FROM it_text_pool
      LANGUAGE iv_language
      STATE iv_state.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from INSERT TEXTPOOL' ).
    ENDIF.

    zcl_abapgit_objects_activation=>add( iv_type = 'REPT'
                                         iv_name = lv_cp ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_sotr.
    zcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'LIMU'
      iv_object   = 'CPUB'
      iv_obj_name = iv_object_name
      io_xml      = ii_xml ).
    zcl_abapgit_sots_handler=>read_sots(
      iv_pgmid    = 'LIMU'
      iv_object   = 'CPUB'
      iv_obj_name = iv_object_name
      io_xml      = ii_xml ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_text_pool.
    DATA: lv_cp TYPE program.

    lv_cp = cl_oo_classname_service=>get_classpool_name( iv_class_name ).
    READ TEXTPOOL lv_cp INTO rt_text_pool LANGUAGE iv_language. "#EC CI_READ_REP
  ENDMETHOD.
ENDCLASS.
