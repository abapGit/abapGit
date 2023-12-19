CLASS zcl_abapgit_object_iamu DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_internet_appl_comp_binary,
             attributes TYPE w3mimeattr,
             source     TYPE w3mimetabtype,
             length     TYPE i,
             extension  TYPE string,
           END OF ty_internet_appl_comp_binary.

    DATA: mi_mime_api TYPE REF TO if_w3_api_mime.

    METHODS:
      get_extension
        IMPORTING
          iv_name             TYPE csequence
          iv_data             TYPE xstring
        RETURNING
          VALUE(rv_extension) TYPE string,

      load_mime_api
        RAISING
          zcx_abapgit_exception,

      read
        RETURNING
          VALUE(rs_internet_appl_comp_binary) TYPE ty_internet_appl_comp_binary
        RAISING
          zcx_abapgit_exception,

      save
        IMPORTING
          is_internet_appl_comp_binary TYPE ty_internet_appl_comp_binary
        RAISING
          zcx_abapgit_exception,

      lock
        IMPORTING
          iv_changable TYPE abap_bool
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_iamu IMPLEMENTATION.


  METHOD get_extension.

    CONSTANTS:
      lc_jpg TYPE xstring VALUE 'FFD8FF',
      lc_png TYPE xstring VALUE '89504E470D0A1A0A',
      lc_gif TYPE xstring VALUE '47494638',
      lc_bmp TYPE xstring VALUE '424D'.

    DATA lv_len TYPE i.

    " Try to derive type of MIME object from the long name
    FIND REGEX '\.(\w)$' IN iv_name SUBMATCHES rv_extension.
    IF sy-subrc = 0.
      rv_extension = to_lower( rv_extension ).
    ELSEIF zcl_abapgit_utils=>is_binary( iv_data ) = abap_true.
      " Use magic numbers to detect common file types
      lv_len = xstrlen( iv_data ).
      IF lv_len > 3 AND iv_data(3) = lc_jpg.
        rv_extension = 'jpg'.
      ELSEIF lv_len > 8 AND iv_data(8) = lc_png.
        rv_extension = 'png'.
      ELSEIF lv_len > 4 AND iv_data(4) = lc_gif.
        rv_extension = 'git'.
      ELSEIF lv_len > 2 AND iv_data(2) = lc_bmp.
        rv_extension = 'bmp'.
      ELSE.
        rv_extension = 'bin'.
      ENDIF.
    ELSE.
      rv_extension = 'txt'.
    ENDIF.

  ENDMETHOD.


  METHOD load_mime_api.

    DATA: ls_mime_name TYPE iacikeym.

    ls_mime_name = ms_item-obj_name.

    cl_w3_api_mime=>if_w3_api_mime~load(
      EXPORTING
        p_mime_name         = ls_mime_name
      IMPORTING
        p_mime              = mi_mime_api
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


  METHOD lock.

    " As a side effect this method removes also existing locks
    mi_mime_api->if_w3_api_object~set_changeable(
      EXPORTING
        p_changeable                 = iv_changable
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
        OTHERS                       = 12 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~set_changeable| ).
    ENDIF.

  ENDMETHOD.


  METHOD read.

    load_mime_api( ).

    mi_mime_api->get_attributes(
      IMPORTING
        p_attributes   = rs_internet_appl_comp_binary-attributes
      EXCEPTIONS
        object_invalid = 1
        mime_deleted   = 2
        error_occured  = 3
        OTHERS         = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~get_attributes| ).
    ENDIF.

    CLEAR: rs_internet_appl_comp_binary-attributes-chname,
           rs_internet_appl_comp_binary-attributes-tdate,
           rs_internet_appl_comp_binary-attributes-ttime,
           rs_internet_appl_comp_binary-attributes-devclass.

    mi_mime_api->get_source(
      IMPORTING
        p_source       = rs_internet_appl_comp_binary-source
        p_datalength   = rs_internet_appl_comp_binary-length
      EXCEPTIONS
        object_invalid = 1
        mime_deleted   = 2
        error_occured  = 3
        OTHERS         = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~get_source| ).
    ENDIF.

  ENDMETHOD.


  METHOD save.

    IF zif_abapgit_object~exists( ) = abap_true.
      load_mime_api( ).
      lock( abap_true ).

      mi_mime_api->set_source(
        EXPORTING
          p_source     = is_internet_appl_comp_binary-source
          p_datalength = is_internet_appl_comp_binary-length
        EXCEPTIONS
          object_not_changeable = 1
          object_deleted        = 2
          object_invalid        = 3
          authorize_failure     = 4
          invalid_content       = 5
          error_occured         = 6
          OTHERS                = 7 ).

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error { sy-subrc } from set_source| ).
      ENDIF.
    ELSE.
      cl_w3_api_mime=>if_w3_api_mime~create_new(
        EXPORTING
          p_mime_data             = is_internet_appl_comp_binary-attributes
          p_mime_content          = is_internet_appl_comp_binary-source
          p_datalength            = is_internet_appl_comp_binary-length
        IMPORTING
          p_mime                  = mi_mime_api
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
        zcx_abapgit_exception=>raise( |Error { sy-subrc } from create_new| ).
      ENDIF.
    ENDIF.

    " Create_new does not update text, so set attributes explicitly
    mi_mime_api->set_attributes(
      EXPORTING
        p_attributes          = is_internet_appl_comp_binary-attributes
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        author_not_existing   = 4
        authorize_failure     = 5
        error_occured         = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error { sy-subrc } from set_attributes| ).
    ENDIF.

    mi_mime_api->if_w3_api_object~save(
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
      zcx_abapgit_exception=>raise( |Error { sy-subrc } from save| ).
    ENDIF.

    lock( abap_false ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = read( )-attributes-chname.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    load_mime_api( ).

    mi_mime_api->if_w3_api_object~set_changeable(
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
        OTHERS                       = 12 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~set_changeable| ).
    ENDIF.

    mi_mime_api->if_w3_api_object~delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        error_occured         = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_mime~delete| ).
    ENDIF.

    mi_mime_api->if_w3_api_object~save(
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


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_internet_appl_comp_binary TYPE ty_internet_appl_comp_binary.
    DATA lv_xstring TYPE xstring.

    io_xml->read(
      EXPORTING
        iv_name = 'IAMU'
      CHANGING
        cg_data = ls_internet_appl_comp_binary ).

    ls_internet_appl_comp_binary-attributes-devclass = iv_package.

    IF io_xml->get_metadata( )-version = 'v2.0.0'.
      lv_xstring = mo_files->read_raw( ls_internet_appl_comp_binary-extension ).

      zcl_abapgit_convert=>xstring_to_bintab(
        EXPORTING
          iv_xstr   = lv_xstring
        IMPORTING
          et_bintab = ls_internet_appl_comp_binary-source
          ev_size   = ls_internet_appl_comp_binary-length ).
    ENDIF.

    save( ls_internet_appl_comp_binary ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: ls_mime_name TYPE iacikeym.

    ls_mime_name = ms_item-obj_name.

    cl_w3_api_mime=>s_check_exist(
      EXPORTING
        p_mime_name = ls_mime_name
      IMPORTING
        p_exists    = rv_bool ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata         = get_metadata( ).
    rs_metadata-version = 'v2.0.0'. " Serialization v2, separate data file
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_internet_appl_comp_binary TYPE ty_internet_appl_comp_binary.
    DATA lv_xstring TYPE xstring.

    FIELD-SYMBOLS: <lv_data> LIKE LINE OF ls_internet_appl_comp_binary-source.

    ls_internet_appl_comp_binary = read( ).

    " Seriazation v2, separate data file
    LOOP AT ls_internet_appl_comp_binary-source ASSIGNING <lv_data>.
      lv_xstring = lv_xstring && <lv_data>-line.
    ENDLOOP.
    lv_xstring = lv_xstring(ls_internet_appl_comp_binary-length).

    CLEAR: ls_internet_appl_comp_binary-source, ls_internet_appl_comp_binary-length.

    ls_internet_appl_comp_binary-extension = get_extension(
      iv_name = ls_internet_appl_comp_binary-attributes-longname
      iv_data = lv_xstring ).

    mo_files->add_raw(
      iv_data = lv_xstring
      iv_ext  = ls_internet_appl_comp_binary-extension ).

    io_xml->add( iv_name = 'IAMU'
                 ig_data = ls_internet_appl_comp_binary ).

  ENDMETHOD.
ENDCLASS.
