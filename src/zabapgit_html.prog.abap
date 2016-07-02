*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_HTML
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_html_action_utils DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_html_action_utils DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_commit_fields, "TODO refactor ! Move to normal place
             repo_key TYPE lcl_persistence_repo=>ty_repo-key,
             username TYPE string,
             email    TYPE string,
             comment  TYPE string,
             body     TYPE string,
           END OF ty_commit_fields.

    CLASS-METHODS jump_encode
      IMPORTING iv_obj_type      TYPE tadir-object
                iv_obj_name      TYPE tadir-obj_name
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS jump_decode
      IMPORTING iv_string   TYPE clike
      EXPORTING ev_obj_type TYPE tadir-object
                ev_obj_name TYPE tadir-obj_name
      RAISING   lcx_exception.

    CLASS-METHODS file_encode
      IMPORTING iv_key           TYPE lcl_persistence_repo=>ty_repo-key
                ig_file          TYPE any "ty_repo_file
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS file_decode
      IMPORTING iv_string TYPE clike
      EXPORTING ev_key    TYPE lcl_persistence_repo=>ty_repo-key
                eg_file   TYPE any "ty_repo_file
      RAISING   lcx_exception.

    CLASS-METHODS dbkey_encode
      IMPORTING is_key           TYPE lcl_persistence_db=>ty_content
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS dbkey_decode
      IMPORTING iv_string     TYPE clike
      RETURNING VALUE(rs_key) TYPE lcl_persistence_db=>ty_content.

    CLASS-METHODS parse_commit_request
      IMPORTING it_postdata      TYPE cnht_post_data_tab
      RETURNING VALUE(rs_fields) TYPE ty_commit_fields.

    CLASS-METHODS repo_key_encode
      IMPORTING iv_key           TYPE lcl_persistence_repo=>ty_repo-key
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS repo_key_decode
      IMPORTING iv_string     TYPE clike
      RETURNING VALUE(rv_key) TYPE lcl_persistence_repo=>ty_repo-key.

ENDCLASS.       "lcl_html_action_utils DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_html_action_utils IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html_action_utils IMPLEMENTATION.

  METHOD jump_encode.

    DATA: lt_fields TYPE tihttpnvp,
          ls_field  LIKE LINE OF lt_fields.


    ls_field-name = 'TYPE'.
    ls_field-value = iv_obj_type.
    APPEND ls_field TO lt_fields.

    ls_field-name = 'NAME'.
    ls_field-value = iv_obj_name.
    APPEND ls_field TO lt_fields.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "jump_encode

  METHOD jump_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    lv_string = iv_string.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'TYPE'.
    IF sy-subrc = 0.
      ev_obj_type = <ls_field>-value.
    ELSE.
      CLEAR ev_obj_type.
    ENDIF.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'NAME'.
    IF sy-subrc = 0.
      ev_obj_name = <ls_field>-value.
    ELSE.
      CLEAR ev_obj_name.
    ENDIF.

  ENDMETHOD.                    "jump_decode

  METHOD file_encode.

    DATA: lt_fields TYPE tihttpnvp,
          ls_field  LIKE LINE OF lt_fields.

    FIELD-SYMBOLS <lv_field> TYPE string.

    ls_field-name = 'KEY'.
    ls_field-value = iv_key.
    APPEND ls_field TO lt_fields.

    ls_field-name = 'PATH'.
    ASSIGN COMPONENT ls_field-name OF STRUCTURE ig_file TO <lv_field>.
    ASSERT <lv_field> IS ASSIGNED.
    ls_field-value = <lv_field>.
    APPEND ls_field TO lt_fields.

    ls_field-name = 'FILENAME'.
    ASSIGN COMPONENT ls_field-name OF STRUCTURE ig_file TO <lv_field>.
    ASSERT <lv_field> IS ASSIGNED.
    ls_field-value = <lv_field>.
    APPEND ls_field TO lt_fields.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "file_encode

  METHOD file_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields,
                   <lv_field> TYPE string.

    CLEAR: ev_key, eg_file.
    lv_string = iv_string.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'KEY'.
    IF sy-subrc = 0.
      ev_key = <ls_field>-value.
    ENDIF.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'PATH'.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'PATH' OF STRUCTURE eg_file TO <lv_field>.
      ASSERT <lv_field> IS ASSIGNED.
      <lv_field> = <ls_field>-value.
    ENDIF.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'FILENAME'.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'FILENAME' OF STRUCTURE eg_file TO <lv_field>.
      ASSERT <lv_field> IS ASSIGNED.
      <lv_field> = <ls_field>-value.
    ENDIF.

  ENDMETHOD.                    "file_decode

  METHOD dbkey_encode.

    DATA: lt_fields TYPE tihttpnvp,
          ls_field  LIKE LINE OF lt_fields.

    ls_field-name = 'TYPE'.
    ls_field-value = is_key-type.
    APPEND ls_field TO lt_fields.

    ls_field-name = 'VALUE'.
    ls_field-value = is_key-value.
    APPEND ls_field TO lt_fields.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "dbkey_encode

  METHOD dbkey_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

    lv_string = iv_string.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'TYPE'.
    IF sy-subrc = 0.
      rs_key-type = <ls_field>-value.
    ENDIF.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'VALUE'.
    IF sy-subrc = 0.
      rs_key-value = <ls_field>-value.
    ENDIF.

  ENDMETHOD.                    "dbkey_decode

  METHOD parse_commit_request.

    CONSTANTS: lc_replace TYPE string VALUE '<<new>>'.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CONCATENATE LINES OF it_postdata INTO lv_string.

    REPLACE ALL OCCURRENCES OF gc_newline IN lv_string WITH lc_replace.

    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'key' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-repo_key = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'username' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-username = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'email' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-email = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'comment' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-comment = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'body' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_fields-body = <ls_field>-value.
    REPLACE ALL OCCURRENCES OF lc_replace IN rs_fields-body WITH gc_newline.

  ENDMETHOD.                    "parse_commit_request

  METHOD repo_key_encode.

    DATA: lt_fields TYPE tihttpnvp,
          ls_field  LIKE LINE OF lt_fields.

    ls_field-name = 'KEY'.
    ls_field-value = iv_key.
    APPEND ls_field TO lt_fields.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "repo_key_encode

  METHOD repo_key_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

    lv_string = iv_string.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'KEY'.
    IF sy-subrc = 0.
      rv_key = <ls_field>-value.
    ENDIF.

  ENDMETHOD.                    "repo_key_decode

ENDCLASS.       "lcl_html_action_utils IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_html_helper DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_html_helper DEFINITION FINAL.
  PUBLIC SECTION.
    CONSTANTS: c_indent_size TYPE i VALUE 2.

    DATA mv_html   TYPE string READ-ONLY.
    DATA mv_indent TYPE i READ-ONLY.

    METHODS add IMPORTING iv_chunk TYPE any.
    METHODS reset.

    METHODS add_anchor IMPORTING iv_txt   TYPE string
                                 iv_act   TYPE string
                                 iv_opt   TYPE char1  OPTIONAL
                                 iv_typ   TYPE char1  DEFAULT gc_action_type-sapevent
                                 iv_class TYPE string OPTIONAL.

  PRIVATE SECTION.
    METHODS _add_str IMPORTING iv_str  TYPE csequence.
    METHODS _add_htm IMPORTING io_html TYPE REF TO lcl_html_helper.

ENDCLASS.                    "lcl_html_helper DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_html_helper IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html_helper IMPLEMENTATION.
  METHOD add.
    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lo_html TYPE REF TO lcl_html_helper.

    lo_type = cl_abap_typedescr=>describe_by_data( iv_chunk ).

    CASE lo_type->type_kind.
      WHEN cl_abap_typedescr=>typekind_char
          OR cl_abap_typedescr=>typekind_string.
        IF strlen( iv_chunk ) = 0.
          RETURN.
        ENDIF.
        _add_str( iv_chunk ).
      WHEN cl_abap_typedescr=>typekind_oref.
        ASSERT iv_chunk IS BOUND. " Dev mistake
        TRY.
            lo_html ?= iv_chunk.
          CATCH cx_sy_move_cast_error.
            ASSERT 1 = 0. " Dev mistake
        ENDTRY.
        _add_htm( lo_html ).
      WHEN OTHERS.
        ASSERT 1 = 0. " Dev mistake
    ENDCASE.

  ENDMETHOD.  " add

  METHOD reset.
    CLEAR: me->mv_html, me->mv_indent.
  ENDMETHOD.                    "reset

  METHOD _add_str.
    CONSTANTS lc_single_tags_re TYPE string " HTML5 singleton tags
      VALUE '<(area|base|br|col|command|embed|hr|img|input|link|meta|param|source|!)'.

    DATA lv_tags        TYPE i.
    DATA lv_tags_open   TYPE i.
    DATA lv_tags_close  TYPE i.
    DATA lv_tags_single TYPE i.
    DATA lv_close_offs  TYPE i.
    DATA lv_shift_back  TYPE i.

    FIND FIRST OCCURRENCE OF '</' IN iv_str MATCH OFFSET lv_close_offs.
    IF sy-subrc = 0 AND lv_close_offs = 0 AND mv_indent > 0. " Found close tag @beginning
      lv_shift_back = 1.
    ENDIF.

    mv_html =   mv_html
            &&  repeat( val = ` ` occ = ( mv_indent - lv_shift_back ) * c_indent_size )
            &&  iv_str
            &&  gc_newline.

    FIND ALL OCCURRENCES OF '<'  IN iv_str MATCH COUNT lv_tags.
    FIND ALL OCCURRENCES OF '</' IN iv_str MATCH COUNT lv_tags_close.
    FIND ALL OCCURRENCES OF REGEX lc_single_tags_re IN iv_str MATCH COUNT lv_tags_single.

    lv_tags_open = lv_tags - lv_tags_close - lv_tags_single.

    " More-less logic chosen due to possible double tags in a line '<a><b>'
    IF lv_tags_open > lv_tags_close.
      mv_indent = mv_indent + 1.
    ELSEIF lv_tags_open < lv_tags_close AND mv_indent > 0.
      mv_indent = mv_indent - 1.
    ENDIF.

  ENDMETHOD.                    "_add_str

  METHOD _add_htm.

    DATA lt_strtab TYPE TABLE OF string.
    DATA lv_str    TYPE string.

    SPLIT io_html->mv_html AT gc_newline INTO TABLE lt_strtab.
    LOOP AT lt_strtab INTO lv_str.
      SHIFT lv_str LEFT  DELETING LEADING space.
      _add_str( lv_str ).
    ENDLOOP.

  ENDMETHOD.                    "_add_htm

  METHOD add_anchor.
    DATA: lv_class TYPE string,
          lv_href  TYPE string.

    lv_class = iv_class.

    IF iv_opt = gc_html_opt-emphas.
      lv_class = lv_class && ' emphasis' ##NO_TEXT.
    ENDIF.
    IF iv_opt = gc_html_opt-cancel.
      lv_class = lv_class && ' attention' ##NO_TEXT.
    ENDIF.
    IF lv_class IS NOT INITIAL.
      SHIFT lv_class LEFT DELETING LEADING space.
      lv_class = | class="{ lv_class }"|.
    ENDIF.

    IF iv_act IS NOT INITIAL.
      CASE iv_typ.
        WHEN gc_action_type-url.
          lv_href = | href="{ iv_act }"|.
        WHEN gc_action_type-sapevent.
          lv_href = | href="sapevent:{ iv_act }"|.
        WHEN gc_action_type-onclick.
          lv_href = | onclick="{ iv_act }"|.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    _add_str( |<a{ lv_class }{ lv_href }>{ iv_txt }</a>| ).

  ENDMETHOD.                    "add_action

ENDCLASS.                    "lcl_html_helper IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_html_toolbar DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_html_toolbar DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS add    IMPORTING iv_txt TYPE string
                             io_sub TYPE REF TO lcl_html_toolbar OPTIONAL
                             iv_act TYPE string    OPTIONAL
                             iv_opt TYPE c         OPTIONAL
                             iv_typ TYPE c         DEFAULT gc_action_type-sapevent.

    METHODS render IMPORTING iv_as_droplist_with_label TYPE string OPTIONAL
                             iv_no_separator           TYPE abap_bool OPTIONAL
                             iv_vertical               TYPE abap_bool OPTIONAL
                   RETURNING VALUE(ro_html)            TYPE REF TO lcl_html_helper.

    METHODS reset.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_item,
             txt TYPE string,
             act TYPE string,
             sub TYPE REF TO lcl_html_toolbar,
             opt TYPE char1,
             typ TYPE char1,
           END OF ty_item.
    TYPES:  tt_items TYPE STANDARD TABLE OF ty_item.

    DATA    mt_items TYPE tt_items.

ENDCLASS. "lcl_html_toolbar DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_html_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html_toolbar IMPLEMENTATION.

  METHOD reset.
    CLEAR mt_items.
  ENDMETHOD.  "reset

  METHOD add.
    DATA ls_item TYPE ty_item.

    ASSERT iv_act IS INITIAL AND io_sub IS NOT INITIAL
      OR   iv_act IS NOT INITIAL AND io_sub IS INITIAL. " Only one supplied

    ls_item-txt = iv_txt.
    ls_item-act = iv_act.
    ls_item-sub = io_sub.
    ls_item-opt = iv_opt.
    ls_item-typ = iv_typ.
    APPEND ls_item TO mt_items.
  ENDMETHOD.  "add

  METHOD render.
    DATA:
      lv_class TYPE string,
      lv_last  TYPE abap_bool.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF mt_items.

    CREATE OBJECT ro_html.

    IF iv_as_droplist_with_label IS INITIAL.
      IF iv_vertical = abap_true.
        lv_class = 'menu_vertical' ##NO_TEXT.
      ELSE.
        lv_class = 'menu' ##NO_TEXT.
      ENDIF.
    ELSE.
      lv_class = 'dropdown' ##NO_TEXT.
    ENDIF.

    ro_html->add( |<div class="{ lv_class }">| ).

    IF iv_as_droplist_with_label IS NOT INITIAL.
      lv_class = 'dropbtn'.
      IF iv_no_separator = abap_true.
        lv_class = lv_class && ' menu_end' ##NO_TEXT.
      ENDIF.
      ro_html->add( |<a class="{ lv_class }">{ iv_as_droplist_with_label }</a>| ).
      ro_html->add( '<div class="dropdown_content">' ).
    ENDIF.

    LOOP AT mt_items ASSIGNING <ls_item>.
      lv_last = boolc( sy-tabix = lines( mt_items ) ).

      IF <ls_item>-sub IS INITIAL.
        CLEAR lv_class.
        IF iv_no_separator = abap_true
            OR lv_last = abap_true
            AND iv_as_droplist_with_label IS INITIAL.
          lv_class = 'menu_end'.
        ENDIF.
        ro_html->add_anchor( iv_txt   = <ls_item>-txt
                             iv_act   = <ls_item>-act
                             iv_opt   = <ls_item>-opt
                             iv_typ   = <ls_item>-typ
                             iv_class = lv_class ).
      ELSE.
        ro_html->add( <ls_item>-sub->render(
          iv_as_droplist_with_label = <ls_item>-txt
          iv_no_separator           = lv_last ) ).
      ENDIF.

    ENDLOOP.

    IF iv_as_droplist_with_label IS NOT INITIAL.
      ro_html->add( '</div>' ).
    ENDIF.

    ro_html->add( '</div>' ).

  ENDMETHOD.  "render

ENDCLASS. "lcl_html_toolbar IMPLEMENTATION