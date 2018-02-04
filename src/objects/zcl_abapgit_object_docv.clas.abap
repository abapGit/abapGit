CLASS zcl_abapgit_object_docv DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

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
      RETURNING VALUE(rs_data) TYPE ty_data.

ENDCLASS.

CLASS zcl_abapgit_object_docv IMPLEMENTATION.

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "zif_abapgit_object~has_changed_since

  METHOD zif_abapgit_object~changed_by.
    rv_user = read( )-head-tdluser.
    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.                    "zif_abapgit_object~changed_by

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

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.                    "zif_abapgit_object~get_metadata

  METHOD zif_abapgit_object~exists.

    DATA: lv_id     TYPE dokhl-id,
          lv_object TYPE dokhl-object.


    lv_id = ms_item-obj_name(2).
    lv_object = ms_item-obj_name+2.

    SELECT SINGLE id FROM dokil INTO lv_id
      WHERE id     = lv_id
        AND object = lv_object.                         "#EC CI_GENBUFF

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "zif_abapgit_object~exists

  METHOD zif_abapgit_object~jump.

    zcx_abapgit_exception=>raise( 'todo, jump DOCV' ).

  ENDMETHOD.                    "jump

  METHOD zif_abapgit_object~delete.

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
      zcx_abapgit_exception=>raise( 'error from DOCU_DEL' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD zif_abapgit_object~deserialize.

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

  METHOD zif_abapgit_object~serialize.

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

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

ENDCLASS.                    "zcl_abapgit_object_msag IMPLEMENTATION
