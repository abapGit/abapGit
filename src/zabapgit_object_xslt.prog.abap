*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_XSLT
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_xslt DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_xslt DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    METHODS:
      get
        RETURNING VALUE(ro_xslt) TYPE REF TO cl_o2_api_xsltdesc
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_xslt DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_xslt IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_xslt IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD get.

    DATA: lv_name TYPE cxsltdesc.


    lv_name = ms_item-obj_name.

    cl_o2_api_xsltdesc=>load(
      EXPORTING
        p_xslt_desc        = lv_name
      IMPORTING
        p_obj              = ro_xslt
      EXCEPTIONS
        not_existing       = 1
        permission_failure = 2
        OTHERS             = 3 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from cl_o2_api_xsltdesc=>load' ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lo_xslt       TYPE REF TO cl_o2_api_xsltdesc,
          lv_source     TYPE string,
          ls_attributes TYPE o2xsltattr.


    lo_xslt = get( ).

    ls_attributes = lo_xslt->get_attributes( ).

    CLEAR: ls_attributes-author,
           ls_attributes-createdon,
           ls_attributes-changedby,
           ls_attributes-changedon,
           ls_attributes-devclass.

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = ls_attributes ).

    lv_source = lo_xslt->get_source_string( ).

    mo_files->add_string( iv_extra  = 'source'
                          iv_ext    = 'xml'
                          iv_string = lv_source ) ##NO_TEXT.

  ENDMETHOD.                    "lif_object~serialize

  METHOD lif_object~deserialize.

    DATA: lv_source     TYPE string,
          lo_xslt       TYPE REF TO cl_o2_api_xsltdesc,
          lv_len        TYPE i,
          ls_attributes TYPE o2xsltattr.


    IF lif_object~exists( ) = abap_true.
      lif_object~delete( ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).

    ls_attributes-devclass = iv_package.

    lv_source = mo_files->read_string( iv_extra = 'source'
                                       iv_ext   = 'xml' ) ##NO_TEXT.

* workaround: somewhere additional linefeeds are added
    lv_len = strlen( lv_source ) - 2.
    IF lv_source+lv_len(2) = cl_abap_char_utilities=>cr_lf.
      lv_source = lv_source(lv_len).
    ENDIF.

    cl_o2_api_xsltdesc=>create_new_from_string(
      EXPORTING
        p_source                = lv_source
        p_attr                  = ls_attributes
      IMPORTING
        p_obj                   = lo_xslt
      EXCEPTIONS
        action_cancelled        = 1
        error_occured           = 2
        not_authorized          = 3
        object_already_existing = 4
        undefined_name          = 5
        OTHERS                  = 6 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from cl_o2_api_xsltdesc=>create_new_from_string' ).
    ENDIF.

    lo_xslt->activate( ).

    lo_xslt->save( ).

    lo_xslt->set_changeable( abap_false ).

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "lif_object~deserialize

  METHOD lif_object~delete.

    DATA: lo_xslt TYPE REF TO cl_o2_api_xsltdesc,
          lv_name TYPE cxsltdesc.


    lv_name = ms_item-obj_name.

    cl_o2_api_xsltdesc=>load(
      EXPORTING
        p_xslt_desc        = lv_name
      IMPORTING
        p_obj              = lo_xslt
      EXCEPTIONS
        error_occured      = 1
        not_existing       = 2
        permission_failure = 3
        version_not_found  = 4
        OTHERS             = 5 ).
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from cl_o2_api_xsltdesc=>load' ).
    ENDIF.

    lo_xslt->set_changeable( abap_true ).
    lo_xslt->delete( ).
    lo_xslt->save( ).

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    DATA: lv_name TYPE cxsltdesc.


    lv_name = ms_item-obj_name.

    rv_bool = cl_o2_api_xsltdesc=>exists( lv_name ).
    IF rv_bool = '1'.
      rv_bool = abap_true.
    ELSE.
      rv_bool = abap_false.
    ENDIF.

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.
    lcx_exception=>raise( 'XSLT, jump, todo' ).
  ENDMETHOD.                    "lif_object~jump

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_xslt IMPLEMENTATION
