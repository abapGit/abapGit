*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_DOCT
*&---------------------------------------------------------------------*

CLASS lcl_object_docv DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    CONSTANTS: c_typ     TYPE dokhl-typ VALUE 'E',
               c_version TYPE dokhl-dokversion VALUE '0001',
               c_name    TYPE string VALUE 'DOC'.

    TYPES: BEGIN OF ty_data,
             doctitle TYPE dsyst-doktitle,
             head     TYPE thead,
             lines    TYPE tline_tab,
           END OF ty_data.

    METHODS: read
      RETURNING value(rs_data) TYPE ty_data.

ENDCLASS.                    "lcl_object_msag DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_docv IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
    rv_user = read( )-head-tdluser.
  ENDMETHOD.                    "lif_object~changed_by

  METHOD read.

    DATA: lv_object TYPE dokhl-object,
          lv_id     TYPE dokhl-id.


    lv_id = ms_item-obj_name(2).
    lv_object = ms_item-obj_name+2.

    CALL FUNCTION 'DOCU_READ'
      EXPORTING
        id       = lv_id
        langu    = mv_language
        object   = lv_object
        typ      = c_typ
        version  = c_version
      IMPORTING
        doktitle = rs_data-doctitle
        head     = rs_data-head
      TABLES
        line     = rs_data-lines.

  ENDMETHOD.                    "read

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_id     TYPE dokhl-id,
          lv_object TYPE dokhl-object.


    lv_id = ms_item-obj_name(2).
    lv_object = ms_item-obj_name+2.

    SELECT SINGLE id FROM dokil INTO lv_id
      WHERE id     = lv_id
        AND object = lv_object.                         "#EC CI_GENBUFF

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    lcx_exception=>raise( 'todo, jump DOCT' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_id     TYPE dokhl-id,
          lv_object TYPE dokhl-object.


    lv_id = ms_item-obj_name(2).
    lv_object = ms_item-obj_name+2.

    CALL FUNCTION 'DOCU_DEL'
      EXPORTING
        id       = lv_id
        langu    = mv_language
        object   = lv_object
        typ      = c_typ
      EXCEPTIONS
        ret_code = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from DOCU_DEL' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~deserialize.

    DATA: ls_data TYPE ty_data.


    io_xml->read( EXPORTING iv_name = c_name
                  CHANGING cg_data = ls_data ).

    CALL FUNCTION 'DOCU_UPDATE'
      EXPORTING
        head    = ls_data-head
        state   = 'A'
        typ     = c_typ
        version = c_version
      TABLES
        line    = ls_data-lines.

  ENDMETHOD.                    "deserialize

  METHOD lif_object~serialize.

    DATA: ls_data   TYPE ty_data.


    ls_data = read( ).

    CLEAR: ls_data-head-tdfuser,
           ls_data-head-tdfreles,
           ls_data-head-tdfdate,
           ls_data-head-tdftime,
           ls_data-head-tdluser,
           ls_data-head-tdlreles,
           ls_data-head-tdldate,
           ls_data-head-tdltime.

    io_xml->add( iv_name = c_name
                 ig_data = ls_data ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_msag IMPLEMENTATION
