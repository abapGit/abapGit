CLASS zcl_abapgit_object_iaxu DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mc_source_style_2006 TYPE w3style VALUE 'XML',
          mc_generator_class   TYPE w3styleclass VALUE 'CL_ITS_GENERATE_XML3'.

    METHODS:
      read
        EXPORTING es_attr TYPE w3tempattr
        RAISING   zcx_abapgit_exception,
      save
        IMPORTING is_attr TYPE w3tempattr
        RAISING   zcx_abapgit_exception,
      w3_api_load
        IMPORTING is_name    TYPE iacikeyt
        EXPORTING eo_xml_api TYPE REF TO cl_w3_api_xml3
                  es_attr    TYPE w3tempattr
        RAISING   zcx_abapgit_exception,
      w3_api_set_changeable
        IMPORTING io_xml_api    TYPE REF TO cl_w3_api_xml3
                  iv_changeable TYPE abap_bool
        RAISING   zcx_abapgit_exception,
      w3_api_delete
        IMPORTING io_xml_api TYPE REF TO cl_w3_api_xml3
        RAISING   zcx_abapgit_exception,
      w3_api_save
        IMPORTING io_xml_api TYPE REF TO cl_w3_api_xml3
        RAISING   zcx_abapgit_exception,
      w3_api_create_new
        IMPORTING is_attr    TYPE w3tempattr
        EXPORTING eo_xml_api TYPE REF TO cl_w3_api_xml3
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_iaxu IMPLEMENTATION.


  METHOD read.

    DATA: ls_name TYPE iacikeyt.


    ls_name = ms_item-obj_name.

    w3_api_load( EXPORTING is_name = ls_name
                 IMPORTING es_attr = es_attr ).

    CLEAR: es_attr-chname,
           es_attr-tdate,
           es_attr-ttime,
           es_attr-devclass.

  ENDMETHOD.


  METHOD save.

    DATA: lo_xml_api TYPE REF TO cl_w3_api_xml3.


    w3_api_create_new( EXPORTING is_attr    = is_attr
                       IMPORTING eo_xml_api = lo_xml_api ).

    w3_api_save( io_xml_api = lo_xml_api ).

    w3_api_set_changeable( io_xml_api    = lo_xml_api
                           iv_changeable = abap_false ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lo_xml_api TYPE REF TO cl_w3_api_xml3,
          ls_name    TYPE iacikeyt,
          lx_root    TYPE REF TO zcx_abapgit_exception.


    ls_name = ms_item-obj_name.

    w3_api_load( EXPORTING is_name    = ls_name
                 IMPORTING eo_xml_api = lo_xml_api ).

    w3_api_set_changeable( io_xml_api    = lo_xml_api
                           iv_changeable = abap_true ).

    w3_api_delete( io_xml_api = lo_xml_api ).

    w3_api_save( io_xml_api = lo_xml_api ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_attr TYPE w3tempattr.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING  cg_data = ls_attr ).

    ls_attr-devclass = iv_package.

    IF zif_abapgit_object~exists( ) = abap_true.
      zif_abapgit_object~delete( ).
    ENDIF.

    save( is_attr = ls_attr ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: ls_name  TYPE iacikeyt,
          lv_subrc TYPE sysubrc,
          lx_exc   TYPE REF TO zcx_abapgit_exception.


    ls_name = ms_item-obj_name.

    cl_w3_api_xml3=>s_check_exist( EXPORTING p_xml_name = ls_name
                                   IMPORTING p_exists   = rv_bool ).

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

    DATA: ls_attr TYPE w3tempattr.


    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    read( IMPORTING es_attr = ls_attr ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).

  ENDMETHOD.


  METHOD w3_api_load.

    cl_w3_api_xml3=>load(
      EXPORTING
        p_xml_name          = is_name
      IMPORTING
        p_attributes        = es_attr
        p_xml               = eo_xml_api
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        data_corrupt        = 3
        error_occured       = 4
        OTHERS              = 5 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from w3_api_xml3~load subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_set_changeable.

    io_xml_api->if_w3_api_object~set_changeable(
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
        content_data_error           = 12
        OTHERS                       = 13 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from w3_api_xml3~set_changeable subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_create_new.

    cl_w3_api_xml3=>create_new(
      EXPORTING
        p_source_style_2006       = mc_source_style_2006
        p_xml_data                = is_attr
        p_generator_class         = mc_generator_class
        p_program_name            = is_attr-programm
      IMPORTING
        p_xml                     = eo_xml_api
      EXCEPTIONS
        undefined_name            = 1
        error_occured             = 2
        object_already_existing   = 3
        not_authorized            = 4
        action_cancelled          = 5
        OTHERS                    = 6 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from w3_api_xml3~create_new subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_delete.

    io_xml_api->if_w3_api_object~delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        error_occured         = 4
        OTHERS                = 5 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from w3_api_xml3~delete subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD w3_api_save.

    io_xml_api->if_w3_api_object~save(
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
      zcx_abapgit_exception=>raise( |Error from w3_api_xml3~save subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
