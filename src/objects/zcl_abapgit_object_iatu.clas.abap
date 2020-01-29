CLASS zcl_abapgit_object_iatu DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      read
        EXPORTING es_attr   TYPE w3tempattr
                  ev_source TYPE string
        RAISING   zcx_abapgit_exception,
      save
        IMPORTING is_attr   TYPE w3tempattr iv_source TYPE string
        RAISING   zcx_abapgit_exception,
      w3_api_load
        IMPORTING is_name     TYPE iacikeyt
        EXPORTING eo_template TYPE REF TO if_w3_api_template
        RAISING   zcx_abapgit_exception,
      w3_api_set_changeable
        IMPORTING iv_changeable TYPE abap_bool
                  io_template   TYPE REF TO if_w3_api_template
        RAISING   zcx_abapgit_exception,
      w3_api_delete
        IMPORTING io_template TYPE REF TO if_w3_api_template
        RAISING   zcx_abapgit_exception,
      w3_api_save
        IMPORTING io_template TYPE REF TO if_w3_api_template
        RAISING   zcx_abapgit_exception,
      w3_api_get_attributes
        IMPORTING io_template   TYPE REF TO if_w3_api_template
        EXPORTING es_attributes TYPE w3tempattr
        RAISING   zcx_abapgit_exception,
      w3_api_get_source
        IMPORTING io_template TYPE REF TO if_w3_api_template
        EXPORTING et_source   TYPE w3htmltabtype
        RAISING   zcx_abapgit_exception,
      w3_api_create_new
        IMPORTING is_template_data TYPE w3tempattr
        EXPORTING eo_template      TYPE REF TO if_w3_api_template
        RAISING   zcx_abapgit_exception,
      w3_api_set_attributes
        IMPORTING io_template TYPE REF TO if_w3_api_template
                  is_attr     TYPE w3tempattr
        RAISING   zcx_abapgit_exception,
      w3_api_set_source
        IMPORTING io_template TYPE REF TO if_w3_api_template
                  it_source   TYPE w3htmltabtype
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_iatu IMPLEMENTATION.


  METHOD read.

    DATA: lo_template TYPE REF TO if_w3_api_template,
          lt_source   TYPE w3htmltabtype,
          ls_name     TYPE iacikeyt.


    ls_name = ms_item-obj_name.

    w3_api_load( EXPORTING is_name     = ls_name
                 IMPORTING eo_template = lo_template ).

    w3_api_get_attributes( EXPORTING io_template   = lo_template
                           IMPORTING es_attributes = es_attr ).

    CLEAR: es_attr-chname,
           es_attr-tdate,
           es_attr-ttime,
           es_attr-devclass.

    w3_api_get_source( EXPORTING io_template = lo_template
                       IMPORTING et_source   = lt_source ).

    CONCATENATE LINES OF lt_source INTO ev_source RESPECTING BLANKS.

  ENDMETHOD.


  METHOD save.

    DATA: lt_source   TYPE w3htmltabtype,
          lv_source   TYPE string,
          lo_template TYPE REF TO if_w3_api_template.


    w3_api_create_new( EXPORTING is_template_data = is_attr
                       IMPORTING eo_template      = lo_template ).

    w3_api_set_attributes( io_template = lo_template
                           is_attr     = is_attr ).

    lv_source = iv_source.
    WHILE strlen( lv_source ) >= 255.
      APPEND lv_source(255) TO lt_source.
      lv_source = lv_source+255.
    ENDWHILE.
    IF NOT lv_source IS INITIAL.
      APPEND lv_source TO lt_source.
    ENDIF.

    w3_api_set_source( io_template = lo_template
                       it_source   = lt_source ).

    w3_api_save( lo_template ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lo_template TYPE REF TO if_w3_api_template,
          ls_name     TYPE iacikeyt.


    ls_name = ms_item-obj_name.

    w3_api_load( EXPORTING is_name     = ls_name
                 IMPORTING eo_template = lo_template ).

    w3_api_set_changeable( io_template   = lo_template
                           iv_changeable = abap_true ).

    w3_api_delete( io_template = lo_template ).

    w3_api_save( io_template = lo_template ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_attr   TYPE w3tempattr,
          lv_source TYPE string.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING cg_data = ls_attr ).

    lv_source = mo_files->read_string( 'html' ) ##NO_TEXT.

    ls_attr-devclass = iv_package.
    save( is_attr   = ls_attr
          iv_source = lv_source ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: ls_name TYPE iacikeyt.


    ls_name = ms_item-obj_name.

    cl_w3_api_template=>s_check_exist( EXPORTING p_template_name = ls_name
                                       IMPORTING p_exists        = rv_bool ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_name = ms_item-obj_name
        object_type = ms_item-obj_type.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_attr   TYPE w3tempattr,
          lv_source TYPE string.


    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    read( IMPORTING es_attr   = ls_attr
                    ev_source = lv_source ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).

    mo_files->add_string( iv_ext    = 'html'
                          iv_string = lv_source ) ##NO_TEXT.

  ENDMETHOD.


  METHOD w3_api_load.

    cl_w3_api_template=>if_w3_api_template~load(
      EXPORTING
        p_template_name     = is_name
      IMPORTING
        p_template          = eo_template
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from if_w3_api_template~load subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_set_changeable.

    io_template->if_w3_api_object~set_changeable(
      EXPORTING
        p_changeable                 = iv_changeable
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
      zcx_abapgit_exception=>raise( |Error from w3_api_template~set_changeable subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_delete.

    io_template->if_w3_api_object~delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        error_occured         = 4
        OTHERS                = 5 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from w3_api_template~delete subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_save.

    io_template->if_w3_api_object~save(
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
      zcx_abapgit_exception=>raise( |Error from w3_api_template~save subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.

  METHOD w3_api_get_attributes.

    io_template->get_attributes(
      IMPORTING
        p_attributes     = es_attributes
      EXCEPTIONS
        object_invalid   = 1
        template_deleted = 2
        error_occured    = 3
        OTHERS           = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from w3_api_template~get_attributes subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_get_source.

    io_template->get_source(
      IMPORTING
        p_source         = et_source
      EXCEPTIONS
        object_invalid   = 1
        template_deleted = 2
        error_occured    = 3
        OTHERS           = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from w3_api_template~get_source subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_create_new.

    cl_w3_api_template=>if_w3_api_template~create_new(
      EXPORTING
        p_template_data          = is_template_data
        p_program_name           = is_template_data-programm
      IMPORTING
        p_template               = eo_template
      EXCEPTIONS
        object_already_existing  = 1
        object_just_created      = 2
        not_authorized           = 3
        undefined_name           = 4
        author_not_existing      = 5
        action_cancelled         = 6
        error_occured            = 7
        user_error               = 8
        OTHERS                   = 9 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from w3_api_template~create_new subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.

  METHOD w3_api_set_attributes.

    io_template->set_attributes(
      EXPORTING
        p_attributes          = is_attr
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        author_not_existing   = 4
        authorize_failure     = 5
        error_occured         = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from w3_api_template~set_attributes subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_set_source.

    io_template->set_source(
      EXPORTING
        p_source              = it_source
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        authorize_failure     = 4
        invalid_parameter     = 5
        error_occured         = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from w3_api_template~set_source subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
