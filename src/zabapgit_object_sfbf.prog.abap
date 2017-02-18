*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_SFBF
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_SFBF DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_sfbf DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    METHODS:
      get
        RETURNING VALUE(ro_bf) TYPE REF TO cl_sfw_bf
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_SFBF DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_SFBF IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_sfbf IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.

    DATA: ls_data TYPE sfw_bf.

    ls_data = get( )->get_header_data( ).

    rv_user = ls_data-changedby.

    IF rv_user IS INITIAL.
      rv_user = ls_data-author.
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: ls_tadir TYPE tadir,
          lv_bf    TYPE sfw_bfunction.

    lv_bf = ms_item-obj_name.
    IF cl_sfw_bf=>check_existence( lv_bf ) = abap_false.
      RETURN.
    ENDIF.

    ls_tadir = lcl_tadir=>read_single(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name ).
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.
  ENDMETHOD.                    "lif_object~exists

  METHOD get.

    DATA: lv_bf TYPE sfw_bfunction.


    lv_bf = ms_item-obj_name.

    TRY.
* make sure to clear cache, method GET_BF_FROM_DB does not exist in 702
        ro_bf = cl_sfw_bf=>get_bf( lv_bf ).
        ro_bf->free( ).
        ro_bf = cl_sfw_bf=>get_bf( lv_bf ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        lcx_exception=>raise( 'Error from CL_SFW_BF=>GET_BF' ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lo_bf                TYPE REF TO cl_sfw_bf,
          ls_header            TYPE sfw_bf,
          lv_name_32           TYPE sfw_name32,
          lv_name_80           TYPE sfw_name80,
          lt_assigned_switches TYPE sfw_swbf_outtab,
          lt_dependancies      TYPE sfw_depend_outtab,
          ls_sfw_bfc_kw        TYPE sfw_bfc_kw,
          ls_sfw_bfc_tc        TYPE sfw_bfc_tc,
          ls_sfw_bfc_rn        TYPE sfw_bfc_rn,
          lt_parent_bfs        TYPE sfw_bs_bf_outtab.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_bf = get( ).

    ls_header = lo_bf->get_header_data( ).
    CLEAR: ls_header-author,
           ls_header-createdon,
           ls_header-changedby,
           ls_header-changedon,
           ls_header-timestamp.

    lo_bf->get_texts(
      IMPORTING
        p_32 = lv_name_32
        p_80 = lv_name_80 ).

    lt_assigned_switches = lo_bf->get_assigned_switches( ).
    lt_dependancies = lo_bf->get_excluded_bf( ).
    lo_bf->get_content_data(
      IMPORTING
        ex_sfw_bfc_kw = ls_sfw_bfc_kw
        ex_sfw_bfc_tc = ls_sfw_bfc_tc
        ex_sfw_bfc_rn = ls_sfw_bfc_rn ).
    lt_parent_bfs = lo_bf->get_parent_bfs( ).

    io_xml->add( ig_data = ls_header
                 iv_name = 'HEADER' ).
    io_xml->add( ig_data = lv_name_32
                 iv_name = 'NAME32' ).
    io_xml->add( ig_data = lv_name_80
                 iv_name = 'NAME80' ).

    io_xml->add( ig_data = lt_assigned_switches
                 iv_name = 'ASSIGNED_SWITCHES' ).
    io_xml->add( ig_data = lt_dependancies
                 iv_name = 'DEPENDANCIES' ).
    io_xml->add( ig_data = ls_sfw_bfc_kw
                 iv_name = 'CONTENT_KW' ).
    io_xml->add( ig_data = ls_sfw_bfc_tc
                 iv_name = 'CONTENT_TC' ).
    io_xml->add( ig_data = ls_sfw_bfc_rn
                 iv_name = 'CONTENT_RN' ).
    io_xml->add( ig_data = lt_parent_bfs
                 iv_name = 'PARENT_BFS' ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_bf                TYPE sfw_bfunction,
          lo_bf                TYPE REF TO cl_sfw_bf,
          ls_header            TYPE sfw_bf,
          lv_name_32           TYPE sfw_name32,
          lv_name_80           TYPE sfw_name80,
          lt_assigned_switches TYPE sfw_swbf_outtab,
          lt_dependancies      TYPE sfw_depend_outtab,
          ls_sfw_bfc_kw        TYPE sfw_bfc_kw,
          ls_sfw_bfc_tc        TYPE sfw_bfc_tc,
          ls_sfw_bfc_rn        TYPE sfw_bfc_rn,
          lt_parent_bfs        TYPE sfw_bs_bf_outtab.


    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'NAME32'
                  CHANGING cg_data = lv_name_32 ).
    io_xml->read( EXPORTING iv_name = 'NAME80'
                  CHANGING cg_data = lv_name_80 ).

    io_xml->read( EXPORTING iv_name = 'ASSIGNED_SWITCHES'
                  CHANGING cg_data = lt_assigned_switches ).
    io_xml->read( EXPORTING iv_name = 'DEPENDANCIES'
                  CHANGING cg_data = lt_dependancies ).
    io_xml->read( EXPORTING iv_name = 'CONTENT_KW'
                  CHANGING cg_data = ls_sfw_bfc_kw ).
    io_xml->read( EXPORTING iv_name = 'CONTENT_TC'
                  CHANGING cg_data = ls_sfw_bfc_tc ).
    io_xml->read( EXPORTING iv_name = 'CONTENT_RN'
                  CHANGING cg_data = ls_sfw_bfc_rn ).
    io_xml->read( EXPORTING iv_name = 'PARENT_BFS'
                  CHANGING cg_data = lt_parent_bfs ).

    lv_bf = ms_item-obj_name.
    TRY.
        lo_bf = cl_sfw_bf=>create_bf( lv_bf ).
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        lcx_exception=>raise( 'error in CL_SFW_BF=>CREATE_BF' ).
    ENDTRY.

    ls_header-author = sy-uname.
    ls_header-createdon = sy-datum.
    lo_bf->set_header_data( ls_header ).

    lo_bf->set_texts( p_32 = lv_name_32
                      p_80 = lv_name_80 ).

    lo_bf->set_assigned_switches( lt_assigned_switches ).
    lo_bf->set_excluded_bf( lt_dependancies ).
    lo_bf->set_content_data(
        im_sfw_bfc_kw = ls_sfw_bfc_kw
        im_sfw_bfc_rn = ls_sfw_bfc_rn
        im_sfw_bfc_tc = ls_sfw_bfc_tc ).
    lo_bf->set_parent_bfs( lt_parent_bfs ).

* magic, see function module RS_CORR_INSERT, FORM get_current_devclass
    SET PARAMETER ID 'EUK' FIELD iv_package.
    lo_bf->save_all( ).
    SET PARAMETER ID 'EUK' FIELD ''.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_bf     TYPE sfw_bfunction,
          lt_delete TYPE sfw_bftab,
          lt_msgtab TYPE sprot_u_tab.


    lv_bf = ms_item-obj_name.
    APPEND lv_bf TO lt_delete.

    cl_sfw_activate=>delete_sfbf( EXPORTING p_bfuncts = lt_delete
                                  IMPORTING p_msgtab = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lcx_exception=>raise( 'Error deleting SFBF' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'SFBF'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_SFBF IMPLEMENTATION
