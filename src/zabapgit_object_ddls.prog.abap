*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_VIEW
*&---------------------------------------------------------------------*

* todo:
* - downport

*----------------------------------------------------------------------*
*       CLASS lcl_object_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ddls DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ddls IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
* todo
    rv_user = c_user_unknown.
  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).

    rs_metadata-ddic         = abap_true.
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.
* todo
    rv_bool = abap_true.
  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.
    lcx_exception=>raise( 'todo, DDLS jump' ).
  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: li_ddl TYPE REF TO if_dd_ddl_handler.


    li_ddl = cl_dd_ddl_handler_factory=>create( ).
    TRY.
        li_ddl->delete( ms_item-obj_name ).
      CATCH cx_dd_ddl_delete.
* todo
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: li_ddl       TYPE REF TO if_dd_ddl_handler,
          ls_ddddlsrcv TYPE ddddlsrcv.


    li_ddl = cl_dd_ddl_handler_factory=>create( ).

    TRY.
        li_ddl->read(
          EXPORTING
            name         = ms_item-obj_name
            get_state    = 'A'
          IMPORTING
            ddddlsrcv_wa = ls_ddddlsrcv ).
      CATCH cx_dd_ddl_read.
* todo
        BREAK-POINT.
    ENDTRY.

    CLEAR ls_ddddlsrcv-as4user.
    CLEAR ls_ddddlsrcv-as4date.
    CLEAR ls_ddddlsrcv-as4time.

    mo_files->add_string( iv_ext    = 'asddls'
                          iv_string = ls_ddddlsrcv-source ) ##NO_TEXT.

    CLEAR ls_ddddlsrcv-source.

    io_xml->add( iv_name = 'DDLS'
                 ig_data = ls_ddddlsrcv ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: li_ddl       TYPE REF TO if_dd_ddl_handler,
          ls_ddddlsrcv TYPE ddddlsrcv.


    io_xml->read( EXPORTING iv_name = 'DDLS'
                  CHANGING cg_data  = ls_ddddlsrcv ).

    ls_ddddlsrcv-source = mo_files->read_string( 'asddls' ) ##NO_TEXT.

    li_ddl = cl_dd_ddl_handler_factory=>create( ).

    TRY.
        li_ddl->save(
          EXPORTING
            name         = ms_item-obj_name
            put_state    = 'N'
            ddddlsrcv_wa = ls_ddddlsrcv ).

        li_ddl->write_tadir(
          objectname = ms_item-obj_name
          devclass   = iv_package
          prid       = 0 ).
      CATCH cx_dd_ddl_save.
* todo
        BREAK-POINT.
    ENDTRY.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_view IMPLEMENTATION
