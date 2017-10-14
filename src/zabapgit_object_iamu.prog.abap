*&---------------------------------------------------------------------*
*& Include zabapgit_object_iamu
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_iamu DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_iamu DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_iamu,
             attributes TYPE w3mimeattr,
             source     TYPE w3mimetabtype,
             length     TYPE i,
           END OF ty_iamu.
    DATA: mo_mime_api TYPE REF TO if_w3_api_mime.

    METHODS:
      load
        RAISING
          zcx_abapgit_exception,

      release_lock
        RAISING
          zcx_abapgit_exception.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_object_iamu IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_iamu IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.

  METHOD lif_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.

  METHOD lif_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: ls_iamu TYPE ty_iamu.

    load( ).

    mo_mime_api->get_attributes(
      IMPORTING
        p_attributes   = ls_iamu-attributes
      EXCEPTIONS
        object_invalid = 1
        mime_deleted   = 2
        error_occured  = 3
        OTHERS         = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~get_attributes| ).
    ENDIF.

    CLEAR: ls_iamu-attributes-chname,
           ls_iamu-attributes-tdate,
           ls_iamu-attributes-ttime,
           ls_iamu-attributes-devclass.

    mo_mime_api->get_source(
      IMPORTING
        p_source       = ls_iamu-source
        p_datalength   = ls_iamu-length
      EXCEPTIONS
        object_invalid = 1
        mime_deleted   = 2
        error_occured  = 3
        OTHERS         = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~get_source| ).
    ENDIF.

    io_xml->add( iv_name = 'IAMU'
                 ig_data = ls_iamu ).

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: ls_iamu TYPE ty_iamu.

    io_xml->read(
      EXPORTING
        iv_name = 'IAMU'
      CHANGING
        cg_data = ls_iamu ).

    ls_iamu-attributes-devclass = iv_package.

    cl_w3_api_mime=>if_w3_api_mime~create_new(
      EXPORTING
        p_mime_data             = ls_iamu-attributes
        p_mime_content          = ls_iamu-source
        p_datalength            = ls_iamu-length
      IMPORTING
        p_mime                  = mo_mime_api
      EXCEPTIONS
        object_already_existing = 1
        object_just_created     = 2
        not_authorized          = 3
        undefined_name          = 4
        author_not_existing     = 5
        action_cancelled        = 6
        error_occured           = 7
        OTHERS                  = 8 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~create_ne| ).
    ENDIF.

    mo_mime_api->set_attributes(
      EXPORTING
        p_attributes          = ls_iamu-attributes
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        author_not_existing   = 4
        authorize_failure     = 5
        error_occured         = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~set_attributes| ).
    ENDIF.

    mo_mime_api->set_source(
      EXPORTING
        p_source              = ls_iamu-source
        p_datalength          = ls_iamu-length
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        authorize_failure     = 4
        invalid_content       = 5
        error_occured         = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~set_attributes| ).
    ENDIF.

    mo_mime_api->if_w3_api_object~save(
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        action_cancelled      = 3
        permission_failure    = 4
        not_changed           = 5
        data_invalid          = 6
        error_occured         = 7
        OTHERS                = 8 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~save| ).
    ENDIF.

    release_lock( ).

  ENDMETHOD.

  METHOD lif_object~delete.

    load( ).

    mo_mime_api->if_w3_api_object~set_changeable(
      EXPORTING
        p_changeable                 = abap_true
      EXCEPTIONS
        action_cancelled             = 1
        object_locked_by_other_user  = 2
        permission_failure           = 3
        object_already_changeable    = 4
        object_already_unlocked      = 5
        object_just_created          = 6
        object_deleted               = 7
        object_modified              = 8
        object_not_existing          = 9
        object_invalid               = 10
        error_occured                = 11
        content_data_error           = 12
        OTHERS                       = 13 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~set_changeable| ).
    ENDIF.

    mo_mime_api->if_w3_api_object~delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        error_occured         = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~delete| ).
    ENDIF.

    mo_mime_api->if_w3_api_object~save(
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        action_cancelled      = 3
        permission_failure    = 4
        not_changed           = 5
        data_invalid          = 6
        error_occured         = 7
        OTHERS                = 8 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~save| ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: ls_mime_name TYPE iacikeym.

    ls_mime_name = ms_item-obj_name.

    TRY.
        cl_w3_api_mime=>s_check_exist(
          EXPORTING
            p_mime_name = ls_mime_name
          IMPORTING
            p_exists    = rv_bool ).

      CATCH zcx_abapgit_exception.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_name = ms_item-obj_name
        object_type = ms_item-obj_type.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.

  ENDMETHOD.

  METHOD load.

    DATA: ls_mime_name TYPE iacikeym.

    ls_mime_name = ms_item-obj_name.

    cl_w3_api_mime=>if_w3_api_mime~load(
      EXPORTING
        p_mime_name         = ls_mime_name
      IMPORTING
        p_mime              = mo_mime_api
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        data_corrupt        = 3
        error_occured       = 4
        OTHERS              = 6 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from if_w3_api_mime~load' ).
    ENDIF.

  ENDMETHOD.

  METHOD release_lock.

    mo_mime_api->if_w3_api_object~set_changeable(
      EXPORTING
        p_changeable                 = abap_false
      EXCEPTIONS
        action_cancelled             = 1
        object_locked_by_other_user  = 2
        permission_failure           = 3
        object_already_changeable    = 4
        object_already_unlocked      = 5
        object_just_created          = 6
        object_deleted               = 7
        object_modified              = 8
        object_not_existing          = 9
        object_invalid               = 10
        error_occured                = 11
        content_data_error           = 12
        OTHERS                       = 13 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~set_changeable| ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
