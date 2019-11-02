CLASS zcl_abapgit_oo_interface DEFINITION PUBLIC
  INHERITING FROM zcl_abapgit_oo_base.
  PUBLIC SECTION.
    METHODS:
      zif_abapgit_oo_object_fnc~create REDEFINITION,
      zif_abapgit_oo_object_fnc~get_includes REDEFINITION,
      zif_abapgit_oo_object_fnc~get_interface_properties REDEFINITION,
      zif_abapgit_oo_object_fnc~delete REDEFINITION,
      zif_abapgit_oo_object_fnc~deserialize_source REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS deserialize_abap_source_old
      IMPORTING is_clskey TYPE seoclskey
                it_source TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING   zcx_abapgit_exception.

    METHODS deserialize_abap_source_new
      IMPORTING is_clskey TYPE seoclskey
                it_source TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING   zcx_abapgit_exception
                cx_sy_dyn_call_error.
ENDCLASS.



CLASS zcl_abapgit_oo_interface IMPLEMENTATION.


  METHOD zif_abapgit_oo_object_fnc~create.
    DATA: lt_vseoattrib TYPE seoo_attributes_r.
    FIELD-SYMBOLS: <lv_clsname> TYPE seoclsname.

    ASSIGN COMPONENT 'CLSNAME' OF STRUCTURE cg_properties TO <lv_clsname>.
    ASSERT sy-subrc = 0.

    lt_vseoattrib = convert_attrib_to_vseoattrib(
                      iv_clsname    = <lv_clsname>
                      it_attributes = it_attributes ).

    CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
      EXPORTING
        devclass        = iv_package
        overwrite       = iv_overwrite
        suppress_dialog = abap_true
      CHANGING
        interface       = cg_properties
        attributes      = lt_vseoattrib
      EXCEPTIONS
        existing        = 1
        is_class        = 2
        db_error        = 3
        component_error = 4
        no_access       = 5
        other           = 6
        OTHERS          = 7.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from SEO_INTERFACE_CREATE_COMPLETE. Subrc = { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~delete.
    CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
      EXPORTING
        intkey       = is_deletion_key
      EXCEPTIONS
        not_existing = 1
        is_class     = 2
        db_error     = 3
        no_access    = 4
        other        = 5
        OTHERS       = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from SEO_INTERFACE_DELETE_COMPLETE. Subrc = { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_includes.
    DATA lv_interface_name TYPE seoclsname.
    lv_interface_name = iv_object_name.
    APPEND cl_oo_classname_service=>get_interfacepool_name( lv_interface_name ) TO rt_includes.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~get_interface_properties.
    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = is_interface_key
        version      = seoc_version_active
      IMPORTING
        interface    = rs_interface_properties
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 1.
      RETURN. " in case only inactive version exists
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from seo_clif_get. Subrc = { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_oo_object_fnc~deserialize_source.

    DATA: lv_done TYPE abap_bool.


    lv_done = zcl_abapgit_exit=>get_instance( )->custom_deserialize_abap_clif(
                                              iv_object_type = 'INTF'
                                              is_key         = is_key
                                              it_source      = it_source ).
    IF lv_done = abap_true.
      RETURN.
    ENDIF.

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


  METHOD deserialize_abap_source_new.
    DATA: lo_factory  TYPE REF TO object,
          lo_source   TYPE REF TO object,
          lo_settings TYPE REF TO object,
          lr_settings TYPE REF TO data.

    FIELD-SYMBOLS <lg_settings> TYPE any.


    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_clskey
        version = seoc_version_inactive.

    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_factory.

    "Enable modification mode to avoid exception CX_OO_ACCESS_PERMISSON when
    "dealing with objects in foreign namespaces (namespace role = C)
    CALL METHOD lo_factory->('CREATE_SETTINGS')
      EXPORTING
        modification_mode_enabled = abap_true
      RECEIVING
        result                    = lo_settings.

    CREATE DATA lr_settings TYPE REF TO ('IF_OO_CLIF_SOURCE_SETTINGS').
    ASSIGN lr_settings->* TO <lg_settings>.

    <lg_settings> ?= lo_settings.

    CALL METHOD lo_factory->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = is_clskey-clsname
        settings  = <lg_settings>
      RECEIVING
        result    = lo_source.

    TRY.
        CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~LOCK').
      CATCH cx_oo_access_permission.
        zcx_abapgit_exception=>raise( 'source_new, access permission exception' ).
    ENDTRY.

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SET_SOURCE')
      EXPORTING
        source = it_source.

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~SAVE').

    CALL METHOD lo_source->('IF_OO_CLIF_SOURCE~UNLOCK').

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
      zcx_abapgit_exception=>raise( |Error from CL_OO_SOURCE. Subrc = { sy-subrc }| ).
    ENDIF.

    TRY.
        lo_source->access_permission( seok_access_modify ).
        lo_source->set_source( it_source ).
        lo_source->save( ).
        lo_source->access_permission( seok_access_free ).
      CATCH cx_oo_access_permission.
        zcx_abapgit_exception=>raise( 'permission error' ).
      CATCH cx_oo_source_save_failure.
        zcx_abapgit_exception=>raise( 'save failure' ).
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
