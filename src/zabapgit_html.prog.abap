*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_HTML
*&---------------------------------------------------------------------*

DEFINE _add.
  ro_html->add( &1 ) ##NO_TEXT.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS lcl_html_helper DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_html_helper DEFINITION FINAL.
  PUBLIC SECTION.
    CONSTANTS: c_indent_size TYPE i VALUE 2.

    DATA mv_html         TYPE string READ-ONLY.
    DATA mv_indent       TYPE i READ-ONLY.
    DATA mv_within_style TYPE i READ-ONLY.
    DATA mv_within_js    TYPE i READ-ONLY.

    METHODS add IMPORTING iv_chunk TYPE any.
    METHODS reset.

    METHODS add_anchor IMPORTING iv_txt   TYPE string
                                 iv_act   TYPE string
                                 iv_opt   TYPE clike  OPTIONAL
                                 iv_typ   TYPE char1  DEFAULT gc_action_type-sapevent
                                 iv_class TYPE string OPTIONAL
                                 iv_id    TYPE string OPTIONAL
                                 iv_style TYPE string OPTIONAL.

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

    DATA lv_tags            TYPE i.
    DATA lv_tags_open       TYPE i.
    DATA lv_tags_close      TYPE i.
    DATA lv_tags_single     TYPE i.
    DATA lv_close_offs      TYPE i.
    DATA lv_shift_back      TYPE i.
    DATA lv_style_tag_open  TYPE i.
    DATA lv_style_tag_close TYPE i.
    DATA lv_js_tag_open     TYPE i.
    DATA lv_js_tag_close    TYPE i.
    DATA lv_curly           TYPE i.

    FIND FIRST OCCURRENCE OF '</' IN iv_str MATCH OFFSET lv_close_offs.
    IF sy-subrc = 0 AND lv_close_offs = 0 AND mv_indent > 0. " Found close tag @beginning
      lv_shift_back = 1.
    ENDIF.

    FIND FIRST OCCURRENCE OF '}' IN iv_str MATCH OFFSET lv_close_offs. " Find close } @beginning
    IF ( mv_within_style > 0 OR mv_within_js > 0 )
      AND sy-subrc = 0 AND lv_close_offs = 0 AND mv_indent > 0.
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

    FIND ALL OCCURRENCES OF '<style'   IN iv_str MATCH COUNT lv_style_tag_open IGNORING CASE.
    FIND ALL OCCURRENCES OF '</style>' IN iv_str MATCH COUNT lv_style_tag_close IGNORING CASE.
    mv_within_style = mv_within_style + lv_style_tag_open - lv_style_tag_close.

    FIND ALL OCCURRENCES OF '<script'   IN iv_str MATCH COUNT lv_js_tag_open IGNORING CASE.
    FIND ALL OCCURRENCES OF '</script>' IN iv_str MATCH COUNT lv_js_tag_close IGNORING CASE.
    mv_within_js = mv_within_js + lv_js_tag_open - lv_js_tag_close.

    IF mv_within_style > 0 OR mv_within_js > 0.
      FIND ALL OCCURRENCES OF '{'  IN iv_str MATCH COUNT lv_curly.
      lv_tags_open  = lv_tags_open + lv_curly.
      FIND ALL OCCURRENCES OF '}'  IN iv_str MATCH COUNT lv_curly.
      lv_tags_close = lv_tags_close + lv_curly.
    ENDIF.

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
          lv_href  TYPE string,
          lv_id    TYPE string,
          lv_style TYPE string.

    lv_class = iv_class.

    IF iv_opt CA gc_html_opt-emphas.
      lv_class = lv_class && ' emphasis' ##NO_TEXT.
    ENDIF.
    IF iv_opt CA gc_html_opt-cancel.
      lv_class = lv_class && ' attention' ##NO_TEXT.
    ENDIF.
    IF iv_opt CA gc_html_opt-crossout.
      lv_class = lv_class && ' crossout grey' ##NO_TEXT.
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
      ENDCASE.
    ENDIF.

    IF iv_id IS NOT INITIAL.
      lv_id = | id="{ iv_id }"|.
    ENDIF.

    IF iv_style IS NOT INITIAL.
      lv_style = | style="{ iv_style }"|.
    ENDIF.

    _add_str( |<a{ lv_id }{ lv_class }{ lv_href }{ lv_style }>{ iv_txt }</a>| ).

  ENDMETHOD.                    "add_action

ENDCLASS.                    "lcl_html_helper IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_html_toolbar DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_html_toolbar DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      add
        IMPORTING
          iv_txt TYPE string
          io_sub TYPE REF TO lcl_html_toolbar OPTIONAL
          iv_act TYPE string    OPTIONAL
          iv_ico TYPE string    OPTIONAL
          iv_opt TYPE c         OPTIONAL
          iv_typ TYPE c         DEFAULT gc_action_type-sapevent,
      count
        RETURNING VALUE(rv_count) TYPE i,
      render
        IMPORTING
          iv_as_droplist_with_label TYPE string OPTIONAL
          iv_no_separator           TYPE abap_bool OPTIONAL
          iv_vertical               TYPE abap_bool OPTIONAL
          iv_sort                   TYPE abap_bool OPTIONAL
          iv_as_angle               TYPE abap_bool OPTIONAL
          iv_with_icons             TYPE abap_bool OPTIONAL
          iv_add_minizone           TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(ro_html)            TYPE REF TO lcl_html_helper.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_item,
             txt TYPE string,
             act TYPE string,
             ico TYPE string,
             sub TYPE REF TO lcl_html_toolbar,
             opt TYPE char1,
             typ TYPE char1,
           END OF ty_item.

    TYPES tt_items TYPE STANDARD TABLE OF ty_item.

    DATA mt_items TYPE tt_items.

ENDCLASS. "lcl_html_toolbar DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_html_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html_toolbar IMPLEMENTATION.

  METHOD count.
    rv_count = lines( mt_items ).
  ENDMETHOD.

  METHOD add.
    DATA ls_item TYPE ty_item.

    ASSERT iv_act IS INITIAL AND io_sub IS NOT INITIAL
      OR   iv_act IS NOT INITIAL AND io_sub IS INITIAL. " Only one supplied

    ls_item-txt = iv_txt.
    ls_item-act = iv_act.
    ls_item-ico = iv_ico.
    ls_item-sub = io_sub.
    ls_item-opt = iv_opt.
    ls_item-typ = iv_typ.
    APPEND ls_item TO mt_items.
  ENDMETHOD.  "add

  METHOD render. "TODO refactor

    DATA: lv_class   TYPE string,
          lv_is_drop TYPE abap_bool,
          lv_last    TYPE abap_bool.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF mt_items.


    CREATE OBJECT ro_html.
    lv_is_drop = boolc( iv_as_droplist_with_label IS NOT INITIAL OR iv_as_angle IS NOT INITIAL ).

    IF lv_is_drop = abap_false. " Normal menu
      IF iv_vertical = abap_true.
        lv_class = 'menu_vertical' ##NO_TEXT.
      ELSE.
        lv_class = 'menu' ##NO_TEXT.
      ENDIF.
    ELSEIF iv_as_angle IS NOT INITIAL.
      lv_class = 'dropdown dropdown_angle' ##NO_TEXT.
    ELSE.
      lv_class = 'dropdown' ##NO_TEXT.
    ENDIF.

    ro_html->add( |<div class="{ lv_class }">| ).

    IF lv_is_drop = abap_true. " Dropdown
      IF iv_as_angle = abap_true.
        ro_html->add( '<div class="dropbtn_angle"></div>' ).
      ELSE.
        lv_class = 'dropbtn'.
        IF iv_no_separator = abap_true.
          lv_class = lv_class && ' menu_end' ##NO_TEXT.
        ENDIF.
        ro_html->add( |<a class="{ lv_class }">{ iv_as_droplist_with_label }</a>| ).
      ENDIF.

      IF iv_add_minizone = abap_true.
        ro_html->add( '<div class="minizone"></div>' ).
      ENDIF.

      ro_html->add( '<div class="dropdown_content">' ).
      ro_html->add( '<div class="box">' ).
    ENDIF.

    IF iv_sort = abap_true.
      SORT mt_items BY txt ASCENDING AS TEXT.
    ENDIF.

    IF iv_with_icons = abap_true.
      ro_html->add( '<table>' ).
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

        IF iv_with_icons = abap_true.
          ro_html->add( '<tr>' ).
          ro_html->add( |<td class="icon">{ <ls_item>-ico }</td>| ).
          ro_html->add( '<td width="100%">' ).
        ENDIF.

        ro_html->add_anchor( iv_txt   = <ls_item>-txt
                             iv_act   = <ls_item>-act
                             iv_opt   = <ls_item>-opt
                             iv_typ   = <ls_item>-typ
                             iv_class = lv_class ).

        IF iv_with_icons = abap_true.
          ro_html->add( '</td>' ).
          ro_html->add( '</tr>' ).
        ENDIF.

      ELSE.
        ro_html->add( <ls_item>-sub->render(
          iv_as_droplist_with_label = <ls_item>-txt
          iv_no_separator           = lv_last ) ).
      ENDIF.

    ENDLOOP.

    IF iv_with_icons = abap_true.
      ro_html->add( '</table>' ).
    ENDIF.

    IF lv_is_drop = abap_true. " Dropdown
      ro_html->add( '</div></div>' ).
    ENDIF.

    ro_html->add( '</div>' ).

  ENDMETHOD.  "render

ENDCLASS. "lcl_html_toolbar IMPLEMENTATION