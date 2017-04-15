*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_STYL
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_styl DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_styl DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    TYPES:
      ty_thead_tt TYPE STANDARD TABLE OF thead WITH DEFAULT KEY,
      ty_tline_tt TYPE STANDARD TABLE OF tline WITH DEFAULT KEY,
      BEGIN OF ty_text,
        head  TYPE thead,
        lines TYPE ty_tline_tt,
      END OF ty_text,
      ty_texts_tt TYPE STANDARD TABLE OF ty_text WITH DEFAULT KEY.

    METHODS:
      list_texts
        RETURNING VALUE(rt_headers) TYPE ty_thead_tt,
      serialize_text
        IMPORTING is_head        TYPE thead
        RETURNING VALUE(rs_text) TYPE ty_text.

ENDCLASS.                    "lcl_object_TRAN DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_styl IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_styl IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lt_texts TYPE ty_thead_tt.

    lt_texts = list_texts( ).
    rv_bool = boolc( lines( lt_texts ) <> 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

* todo

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lt_texts TYPE ty_thead_tt.

    lt_texts = list_texts( ).

* todo

  ENDMETHOD.                    "delete

  METHOD lif_object~deserialize.

* todo

  ENDMETHOD.                    "deserialize

  METHOD lif_object~serialize.

    DATA: lt_texts  TYPE ty_thead_tt,
          lt_result TYPE ty_texts_tt.

    FIELD-SYMBOLS: <ls_text> LIKE LINE OF lt_texts.


    lt_texts = list_texts( ).
    LOOP AT lt_texts ASSIGNING <ls_text>.
      APPEND serialize_text( <ls_text> ) TO lt_result.
    ENDLOOP.

    io_xml->add( iv_name = 'TEXTS'
                 ig_data = lt_result ).

  ENDMETHOD.                    "serialize

  METHOD serialize_text.

    rs_text-head = is_head.

    CLEAR rs_text-head-tdfuser.
    CLEAR rs_text-head-tdfreles.
    CLEAR rs_text-head-tdfdate.
    CLEAR rs_text-head-tdftime.
    CLEAR rs_text-head-tdluser.
    CLEAR rs_text-head-tdlreles.
    CLEAR rs_text-head-tdldate.
    CLEAR rs_text-head-tdltime.
    CLEAR rs_text-head-mandt.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id       = is_head-tdid
        language = is_head-tdspras
        name     = is_head-tdname
        object   = is_head-tdobject
      TABLES
        lines    = rs_text-lines.

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

  METHOD list_texts.

    DATA: lv_name TYPE thead-tdname.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'SELECT_TEXT'
      EXPORTING
        database_only = abap_true
        name          = lv_name
        object        = 'STYLE'
      TABLES
        selections    = rt_headers.

  ENDMETHOD.

ENDCLASS.                    "lcl_object_styl IMPLEMENTATION
