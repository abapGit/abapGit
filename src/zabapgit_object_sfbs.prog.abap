*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_SFBS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_SFBS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_sfbs DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    METHODS:
      get
        RETURNING VALUE(ro_bfs) TYPE REF TO cl_sfw_bfs
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_SFBS DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_SFBS IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_sfbs IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.

    rv_user = get( )->get_header_data( )-changedby.

  ENDMETHOD.

  METHOD get.

    DATA: lv_bfset TYPE sfw_bset.


    lv_bfset = ms_item-obj_name.

    TRY.
        ro_bfs = cl_sfw_bfs=>get_bfs( lv_bfset ).
        ro_bfs->free( ).
        ro_bfs = cl_sfw_bfs=>get_bfs( lv_bfset ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        lcx_exception=>raise( 'Error from CL_SFW_BFS=>GET_BFS' ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: ls_tadir TYPE tadir,
          lv_bfset TYPE sfw_bset.


    lv_bfset = ms_item-obj_name.
    IF cl_sfw_bfs=>check_existence( lv_bfset ) = abap_false.
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

  METHOD lif_object~serialize.

    DATA: lo_bfs         TYPE REF TO cl_sfw_bfs,
          ls_header      TYPE sfw_bs,
          lv_name_32     TYPE sfw_name32,
          lv_name_80     TYPE sfw_name80,
          lt_assigned_bf TYPE sfw_bfbs_outtab,
          lt_nested_bfs  TYPE sfw_bsbs_outtab,
          lt_parent_bfs  TYPE sfw_bs_bs_parent_outtab.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_bfs = get( ).

    ls_header = lo_bfs->get_header_data( ).
    CLEAR: ls_header-author,
           ls_header-createdon,
           ls_header-changedby,
           ls_header-changedon,
           ls_header-timestamp.

    lo_bfs->get_texts(
      IMPORTING
        p_32 = lv_name_32
        p_80 = lv_name_80 ).

    lt_assigned_bf = lo_bfs->get_assigned_bf( ).
    lt_nested_bfs = lo_bfs->get_nested_bfs( ).
    lt_parent_bfs = lo_bfs->get_nested_parent( ).

    io_xml->add( ig_data = ls_header
                 iv_name = 'HEADER' ).
    io_xml->add( ig_data = lv_name_32
                 iv_name = 'NAME32' ).
    io_xml->add( ig_data = lv_name_80
                 iv_name = 'NAME80' ).

    io_xml->add( ig_data = lt_assigned_bf
                 iv_name = 'ASSIGNED_BF' ).
    io_xml->add( ig_data = lt_nested_bfs
                 iv_name = 'NESTED_BFS' ).
    io_xml->add( ig_data = lt_parent_bfs
                 iv_name = 'PARENT_BFS' ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_bfset       TYPE sfw_bset,
          lo_bfs         TYPE REF TO cl_sfw_bfs,
          ls_header      TYPE sfw_bs,
          lv_name_32     TYPE sfw_name32,
          lv_name_80     TYPE sfw_name80,
          lt_assigned_bf TYPE sfw_bfbs_outtab,
          lt_nested_bfs  TYPE sfw_bsbs_outtab,
          lt_parent_bfs  TYPE sfw_bs_bs_parent_outtab.


    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'NAME32'
                  CHANGING cg_data = lv_name_32 ).
    io_xml->read( EXPORTING iv_name = 'NAME80'
                  CHANGING cg_data = lv_name_80 ).

    io_xml->read( EXPORTING iv_name = 'ASSIGNED_BF'
                  CHANGING cg_data = lt_assigned_bf ).
    io_xml->read( EXPORTING iv_name = 'NESTED_BFS'
                  CHANGING cg_data = lt_nested_bfs ).
    io_xml->read( EXPORTING iv_name = 'PARENT_BFS'
                  CHANGING cg_data = lt_parent_bfs ).

    lv_bfset = ms_item-obj_name.
    TRY.
        lo_bfs = cl_sfw_bfs=>create_bfs( lv_bfset ).
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        lcx_exception=>raise( 'error in CL_SFW_BFS=>CREATE_BFS' ).
    ENDTRY.

    ls_header-author = sy-uname.
    ls_header-createdon = sy-datum.
    lo_bfs->set_header_data( ls_header ).

    lo_bfs->set_texts( p_32 = lv_name_32
                       p_80 = lv_name_80 ).

    lo_bfs->set_assigned_bf( lt_assigned_bf ).
    lo_bfs->set_assigned_bfs( lt_nested_bfs ).
    lo_bfs->set_nested_parent( lt_parent_bfs ).

    lo_bfs->save_all( ).

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_bfset TYPE sfw_bset,
          lo_bfs   TYPE REF TO cl_sfw_bfs.


    lv_bfset = ms_item-obj_name.
    TRY.
        lo_bfs = cl_sfw_bfs=>get_bfs( lv_bfset ).
        lo_bfs->set_delete_flag( lv_bfset ).
        lo_bfs->save_all( ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        lcx_exception=>raise( 'Error deleting BF' ).
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'SFBS'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD lif_object~compare_to_previous_version.

  ENDMETHOD.

ENDCLASS.                    "lcl_object_SFBS IMPLEMENTATION