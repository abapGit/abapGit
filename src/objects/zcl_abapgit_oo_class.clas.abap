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
    METHODS zif_abapgit_oo_object_fnc~deserialize_source
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_oo_class IMPLEMENTATION.


  METHOD zif_abapgit_oo_object_fnc~create.

    DATA: lt_vseoattrib TYPE seoo_attributes_r.
    FIELD-SYMBOLS: <lv_clsname> TYPE seoclsname.

* same as in super class, but with "version = seoc_version_active"

    ASSIGN COMPONENT 'CLSNAME' OF STRUCTURE cg_properties TO <lv_clsname>.
    ASSERT sy-subrc = 0.

    lt_vseoattrib = convert_attrib_to_vseoattrib(
                      iv_clsname    = <lv_clsname>
                      it_attributes = it_attributes ).

    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        devclass        = iv_package
        overwrite       = iv_overwrite
        version         = seoc_version_active
        suppress_dialog = abap_true
      CHANGING
        class           = cg_properties
        attributes      = lt_vseoattrib
      EXCEPTIONS
        existing        = 1
        is_interface    = 2
        db_error        = 3
        component_error = 4
        no_access       = 5
        other           = 6
        OTHERS          = 7.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from SEO_CLASS_CREATE_COMPLETE. Subrc = { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~create_sotr.
    DATA: lt_sotr    TYPE zif_abapgit_definitions=>ty_sotr_tt,
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
        zcx_abapgit_exception=>raise( |error from SOTR_OBJECT_GET_OBJECTS. Subrc = { sy-subrc }| ).
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
      IF sy-subrc <> 0 AND sy-subrc <> 5.
        zcx_abapgit_exception=>raise( |Error from SOTR_CREATE_CONCEPT. Subrc = { sy-subrc }| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~delete.
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
      zcx_abapgit_exception=>raise( |Error from SEO_CLASS_DELETE_COMPLETE. Subrc = { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~deserialize_source.

    DATA: lv_updated         TYPE abap_bool,
          lv_program         TYPE program,
          lo_scanner         TYPE REF TO cl_oo_source_scanner_class,
          lt_methods         TYPE cl_oo_source_scanner_class=>type_method_implementations,
          lv_method          LIKE LINE OF lt_methods,
          lt_public          TYPE seop_source_string,
          lt_auxsrc          TYPE seop_source_string,
          lt_source          TYPE seop_source_string,
          lo_oo_deserializer TYPE REF TO zcl_abapgit_oo_class_deserial.

    CREATE OBJECT lo_oo_deserializer
      EXPORTING
        is_key    = is_key
        it_source = it_source.


    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_key
        version = seoc_version_inactive.

    lo_scanner = lo_oo_deserializer->init_scanner(
      it_source = it_source
      iv_name   = is_key-clsname ).

* public
    lt_public = lo_scanner->get_public_section_source( ).
    IF lt_public IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_pubsec_name( is_key-clsname ).
      lv_updated = lo_oo_deserializer->update_report( iv_program = lv_program
                                                      it_source  = lt_public ).
      IF lv_updated = abap_true.
        lo_oo_deserializer->update_meta(
                     iv_name     = is_key-clsname
                     iv_exposure = seoc_exposure_public
                     it_source   = lt_public ).
      ENDIF.
    ENDIF.

* protected
    lt_source = lo_scanner->get_protected_section_source( ).
    IF lt_source IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_prosec_name( is_key-clsname ).
      lv_updated = lo_oo_deserializer->update_report( iv_program = lv_program
                                  it_source  = lt_source ).
      IF lv_updated = abap_true.
        lt_auxsrc = lt_public.
        APPEND LINES OF lt_source TO lt_auxsrc.

        lo_oo_deserializer->update_meta(
                     iv_name     = is_key-clsname
                     iv_exposure = seoc_exposure_protected
                     it_source   = lt_auxsrc ).
      ENDIF.
    ENDIF.

* private
    lt_source = lo_scanner->get_private_section_source( ).
    IF lt_source IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_prisec_name( is_key-clsname ).
      lv_updated = lo_oo_deserializer->update_report( iv_program = lv_program
                                  it_source  = lt_source ).
      IF lv_updated = abap_true.
        lt_auxsrc = lt_public.
        APPEND LINES OF lt_source TO lt_auxsrc.

        lo_oo_deserializer->update_meta(
                     iv_name     = is_key-clsname
                     iv_exposure = seoc_exposure_private
                     it_source   = lt_auxsrc ).
      ENDIF.
    ENDIF.

* methods
    lt_methods = lo_scanner->get_method_implementations( ).

    LOOP AT lt_methods INTO lv_method.
      TRY.
          lt_source = lo_scanner->get_method_impl_source( lv_method ).
        CATCH cx_oo_clif_component.
          zcx_abapgit_exception=>raise( 'error from GET_METHOD_IMPL_SOURCE' ).
      ENDTRY.
      lv_program = lo_oo_deserializer->determine_method_include(
        iv_name   = is_key-clsname
        iv_method = lv_method ).

      lo_oo_deserializer->update_report(
        iv_program = lv_program
        it_source  = lt_source ).
    ENDLOOP.

* full class include
    lo_oo_deserializer->update_full_class_include(
                               iv_classname = is_key-clsname
                               it_source    = it_source
                               it_methods   = lt_methods ).

    lo_oo_deserializer->update_source_index(
      iv_clsname = is_key-clsname
      io_scanner = lo_scanner ).

* TODO, perhaps move this call to somewhere else, to be done while cleaning up the CLAS deserialization
    zcl_abapgit_objects_activation=>add(
      iv_type = 'CLAS'
      iv_name = is_key-clsname ).

  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~generate_locals.

    DATA: lv_program TYPE programm.


    IF lines( it_local_definitions ) > 0.
      lv_program = cl_oo_classname_service=>get_ccdef_name( is_key-clsname ).
      zcl_abapgit_oo_class_deserial=>update_report(
                     iv_program = lv_program
                     it_source  = it_local_definitions ).
    ENDIF.

    IF lines( it_local_implementations ) > 0.
      lv_program = cl_oo_classname_service=>get_ccimp_name( is_key-clsname ).
      zcl_abapgit_oo_class_deserial=>update_report(
                     iv_program = lv_program
                     it_source  = it_local_implementations ).
    ENDIF.

    IF lines( it_local_macros ) > 0.
      lv_program = cl_oo_classname_service=>get_ccmac_name( is_key-clsname ).
      zcl_abapgit_oo_class_deserial=>update_report(
                     iv_program = lv_program
                     it_source  = it_local_macros ).
    ENDIF.

    IF lines( it_local_test_classes ) > 0.
      lv_program = cl_oo_classname_service=>get_ccau_name( is_key-clsname ).
      zcl_abapgit_oo_class_deserial=>update_report(
                     iv_program = lv_program
                     it_source  = it_local_test_classes ).
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
      zcx_abapgit_exception=>raise( |Error from SEO_CLIF_GET. Subrc = { sy-subrc }| ).
    ENDIF.
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
*    APPEND cl_oo_classname_service=>get_cs_name( lv_class_name ) TO rt_includes.

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
      STATE 'I'.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from INSERT TEXTPOOL' ).
    ENDIF.

    zcl_abapgit_objects_activation=>add( iv_type = 'REPT'
                                         iv_name = lv_cp ).
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~read_sotr.
    DATA: lv_concept    TYPE sotr_head-concept,
          lt_seocompodf TYPE STANDARD TABLE OF seocompodf WITH DEFAULT KEY,
          ls_header     TYPE sotr_head,
          lt_entries    TYPE sotr_text_tt.

    FIELD-SYMBOLS: <ls_sotr>       LIKE LINE OF rt_sotr,
                   <ls_seocompodf> LIKE LINE OF lt_seocompodf,
                   <ls_entry>      LIKE LINE OF lt_entries.


    SELECT * FROM seocompodf
      INTO TABLE lt_seocompodf
      WHERE clsname = iv_object_name
      AND version = '1'
      AND exposure = '2'
      AND attdecltyp = '2'
      AND type = 'SOTR_CONC'
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC

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
        CONTINUE.
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


  METHOD zif_abapgit_oo_object_fnc~read_text_pool.
    DATA: lv_cp TYPE program.

    lv_cp = cl_oo_classname_service=>get_classpool_name( iv_class_name ).
    READ TEXTPOOL lv_cp INTO rt_text_pool LANGUAGE iv_language. "#EC CI_READ_REP
  ENDMETHOD.
ENDCLASS.
