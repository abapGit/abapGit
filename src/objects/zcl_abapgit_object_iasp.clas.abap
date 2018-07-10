CLASS zcl_abapgit_object_iasp DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PRIVATE SECTION.
    METHODS:
      read
        EXPORTING es_attr       TYPE w3servattr
                  et_parameters TYPE w3servpara_tabletype
        RAISING   zcx_abapgit_exception,
      save
        IMPORTING is_attr       TYPE w3servattr
                  it_parameters TYPE w3servpara_tabletype
        RAISING   zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_object_iasp IMPLEMENTATION.

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.

  METHOD read.

    DATA: li_service TYPE REF TO if_w3_api_service,
          lv_name    TYPE itsappl.


    lv_name = ms_item-obj_name.

    cl_w3_api_service=>if_w3_api_service~load(
      EXPORTING
        p_service_name     = lv_name
      IMPORTING
        p_service          = li_service
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from w3api_service~load' ).
    ENDIF.

    li_service->get_attributes( IMPORTING p_attributes = es_attr ).

    CLEAR: es_attr-chname,
           es_attr-tdate,
           es_attr-ttime,
           es_attr-devclass.

    li_service->get_parameters( IMPORTING p_parameters = et_parameters ).

  ENDMETHOD.

  METHOD zif_abapgit_object~serialize.

    DATA: ls_attr       TYPE w3servattr,
          lt_parameters TYPE w3servpara_tabletype.


    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    read( IMPORTING es_attr       = ls_attr
                    et_parameters = lt_parameters ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).
    io_xml->add( iv_name = 'PARAMETERS'
                 ig_data = lt_parameters ).

  ENDMETHOD.

  METHOD save.

    DATA: li_service TYPE REF TO if_w3_api_service.


    cl_w3_api_service=>if_w3_api_service~create_new(
      EXPORTING p_service_data = is_attr
      IMPORTING p_service = li_service ).

    li_service->set_attributes( is_attr ).
    li_service->set_parameters( it_parameters ).

    li_service->if_w3_api_object~save( ).

  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize.

    DATA: ls_attr       TYPE w3servattr,
          lt_parameters TYPE w3servpara_tabletype.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING cg_data = ls_attr ).
    io_xml->read( EXPORTING iv_name = 'PARAMETERS'
                  CHANGING cg_data = lt_parameters ).

    ls_attr-devclass = iv_package.
    save( is_attr       = ls_attr
          it_parameters = lt_parameters ).

  ENDMETHOD.

  METHOD zif_abapgit_object~delete.

    DATA: li_service TYPE REF TO if_w3_api_service,
          lv_name    TYPE itsappl.


    lv_name = ms_item-obj_name.

    cl_w3_api_service=>if_w3_api_service~load(
      EXPORTING
        p_service_name      = lv_name
      IMPORTING
        p_service           = li_service
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from if_w3_api_service~load' ).
    ENDIF.

    li_service->if_w3_api_object~set_changeable( abap_true ).
    li_service->if_w3_api_object~delete( ).
    li_service->if_w3_api_object~save( ).

  ENDMETHOD.

  METHOD zif_abapgit_object~exists.

    DATA: lv_name TYPE itsappl.


    lv_name = ms_item-obj_name.

    cl_w3_api_service=>if_w3_api_service~load(
      EXPORTING
        p_service_name      = lv_name
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc = 1.
      rv_bool = abap_false.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from w3_api_service~load' ).
    ELSE.
      rv_bool = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_name = ms_item-obj_name
        object_type = ms_item-obj_type.

  ENDMETHOD.

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.

ENDCLASS.
