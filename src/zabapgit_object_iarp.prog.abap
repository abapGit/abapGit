*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_IARP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_iarp DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_iarp DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    METHODS:
      read
        EXPORTING es_attr       TYPE w3resoattr
                  et_parameters TYPE w3resopara_tabletype
        RAISING   lcx_exception,
      save
        IMPORTING is_attr       TYPE w3resoattr
                  it_parameters TYPE w3resopara_tabletype
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_iarp IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_iarp IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD read.

    DATA: li_resource TYPE REF TO if_w3_api_resource,
          ls_name     TYPE w3resokey.


    ls_name = ms_item-obj_name.

    cl_w3_api_resource=>if_w3_api_resource~load(
      EXPORTING
        p_resource_name     = ls_name
      IMPORTING
        p_resource          = li_resource
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from w3api_resource~load' ).
    ENDIF.

    li_resource->get_attributes( IMPORTING p_attributes = es_attr ).

    CLEAR: es_attr-chname,
           es_attr-tdate,
           es_attr-ttime,
           es_attr-devclass.

    li_resource->get_parameters( IMPORTING p_parameters = et_parameters ).

  ENDMETHOD.                    "read

  METHOD lif_object~serialize.

    DATA: ls_attr       TYPE w3resoattr,
          lt_parameters TYPE w3resopara_tabletype.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    read( IMPORTING es_attr       = ls_attr
                    et_parameters = lt_parameters ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).
    io_xml->add( iv_name = 'PARAMETERS'
                 ig_data = lt_parameters ).

  ENDMETHOD.                    "lif_object~serialize

  METHOD save.

    DATA: li_resource TYPE REF TO if_w3_api_resource.


    cl_w3_api_resource=>if_w3_api_resource~create_new(
      EXPORTING p_resource_data = is_attr
      IMPORTING p_resource = li_resource ).

    li_resource->set_attributes( is_attr ).
    li_resource->set_parameters( it_parameters ).

    li_resource->if_w3_api_object~save( ).

  ENDMETHOD.                    "save

  METHOD lif_object~deserialize.

    DATA: ls_attr       TYPE w3resoattr,
          lt_parameters TYPE w3resopara_tabletype.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING cg_data = ls_attr ).
    io_xml->read( EXPORTING iv_name = 'PARAMETERS'
                  CHANGING cg_data = lt_parameters ).

    ls_attr-devclass = iv_package.
    save( is_attr       = ls_attr
          it_parameters = lt_parameters ).

  ENDMETHOD.                    "lif_object~deserialize

  METHOD lif_object~delete.

    DATA: li_resource TYPE REF TO if_w3_api_resource,
          ls_name     TYPE w3resokey.


    ls_name = ms_item-obj_name.

    cl_w3_api_resource=>if_w3_api_resource~load(
      EXPORTING
        p_resource_name     = ls_name
      IMPORTING
        p_resource          = li_resource
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from if_w3_api_resource~load' ).
    ENDIF.

    li_resource->if_w3_api_object~set_changeable( abap_true ).
    li_resource->if_w3_api_object~delete( ).
    li_resource->if_w3_api_object~save( ).

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    DATA: ls_name TYPE w3resokey.


    ls_name = ms_item-obj_name.

    cl_w3_api_resource=>if_w3_api_resource~load(
      EXPORTING
        p_resource_name     = ls_name
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc = 1.
      rv_bool = abap_false.
    ELSEIF sy-subrc <> 0.
      lcx_exception=>raise( 'error from w3_api_resource~load' ).
    ELSE.
      rv_bool = abap_true.
    ENDIF.

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.
    lcx_exception=>raise( 'todo, IARP, jump' ).
  ENDMETHOD.                    "lif_object~jump

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_iarp IMPLEMENTATION
