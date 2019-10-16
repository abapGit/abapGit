CLASS zcl_abapgit_object_iaxu DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mo_xml_api           TYPE REF TO cl_w3_api_xml3,
          mc_source_style_2006 TYPE w3style VALUE 'XML',
          mc_generator_class   TYPE w3styleclass VALUE 'CL_ITS_GENERATE_XML3'.

    METHODS:
      read
        EXPORTING es_attr TYPE w3tempattr
        RAISING   zcx_abapgit_exception,
      save
        IMPORTING is_attr TYPE w3tempattr
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_iaxu IMPLEMENTATION.


  METHOD read.

    DATA: li_service TYPE REF TO cl_w3_api_xml3,
          lv_name    TYPE iacikeyt.


    lv_name = ms_item-obj_name.

    cl_w3_api_xml3=>load(
      EXPORTING
        p_xml_name          = lv_name
      IMPORTING
        p_xml               = mo_xml_api
        p_attributes        = es_attr
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        data_corrupt        = 3
        error_occured       = 4
        OTHERS              = 5 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from w3_api_xml3~load' ).
    ENDIF.

    CLEAR: es_attr-chname,
           es_attr-tdate,
           es_attr-ttime,
           es_attr-devclass.

  ENDMETHOD.


  METHOD save.

    cl_w3_api_xml3=>create_new(
      EXPORTING
        p_source_style_2006       = mc_source_style_2006
        p_xml_data                = is_attr
        p_generator_class         = mc_generator_class
        p_program_name            = is_attr-programm
      IMPORTING
        p_xml                     = mo_xml_api
      EXCEPTIONS
        undefined_name            = 1
        error_occured             = 2
        object_already_existing   = 3
        not_authorized            = 4
        action_cancelled          = 5
        OTHERS                    = 6
    ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from w3_api_xml3~create_new| ).
    ENDIF.

    mo_xml_api->if_w3_api_object~save( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lo_xml_api TYPE REF TO cl_w3_api_xml3,
          lv_name    TYPE iacikeyt.


    lv_name = ms_item-obj_name.

    cl_w3_api_xml3=>load(
      EXPORTING
        p_xml_name          = lv_name
      IMPORTING
        p_xml               = lo_xml_api
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        data_corrupt        = 3
        error_occured       = 4
        OTHERS              = 5 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from w3_api_xml3~load' ).
    ENDIF.

    lo_xml_api->get_source(
      IMPORTING
        p_source       = DATA(lv_source)
      EXCEPTIONS
        object_invalid = 1
        xml_deleted    = 2
        error_occured  = 3
        OTHERS         = 4
    ).

    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_xml_api->if_w3_api_object~set_changeable( abap_true ).
    lo_xml_api->if_w3_api_object~delete( ).
    lo_xml_api->if_w3_api_object~save( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_attr TYPE w3tempattr.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING cg_data = ls_attr ).

    ls_attr-devclass = iv_package.
    save( is_attr = ls_attr ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_name TYPE iacikeyt.


    lv_name = ms_item-obj_name.

    cl_w3_api_xml3=>load(
      EXPORTING
        p_xml_name          = lv_name
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        data_corrupt        = 3
        error_occured       = 4
        OTHERS              = 5 ).

    IF sy-subrc = 1.
      rv_bool = abap_false.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from w3_api_xml3~load' ).
    ELSE.
      rv_bool = abap_true.
    ENDIF.

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
ENDCLASS.
