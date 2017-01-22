*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_HTML
*&---------------------------------------------------------------------*

DEFINE _add.
  ro_html->add( &1 ) ##NO_TEXT.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS lcl_html DEFINITION
*----------------------------------------------------------------------*

CLASS lcl_html DEFINITION FINAL.
  PUBLIC SECTION.
    CONSTANTS: c_indent_size TYPE i VALUE 2.

    CLASS-METHODS class_constructor.
    METHODS reset.
    METHODS add
      IMPORTING iv_chunk TYPE any.
    METHODS render
      IMPORTING iv_no_indent_jscss TYPE abap_bool OPTIONAL
      RETURNING VALUE(rv_html)     TYPE string.
    METHODS is_empty
      RETURNING VALUE(rv_yes) TYPE abap_bool.

    METHODS add_a
      IMPORTING
        iv_txt   TYPE string
        iv_act   TYPE string
        iv_typ   TYPE char1  DEFAULT gc_action_type-sapevent
        iv_opt   TYPE clike  OPTIONAL
        iv_class TYPE string OPTIONAL
        iv_id    TYPE string OPTIONAL
        iv_style TYPE string OPTIONAL.

    METHODS add_icon
      IMPORTING
        iv_name  TYPE string
        iv_hint  TYPE string OPTIONAL
        iv_alt   TYPE string OPTIONAL
        iv_class TYPE string OPTIONAL.

    CLASS-METHODS a
      IMPORTING
        iv_txt                TYPE string
        iv_act                TYPE string
        iv_typ                TYPE char1  DEFAULT gc_action_type-sapevent
        iv_opt                TYPE clike  OPTIONAL
        iv_class              TYPE string OPTIONAL
        iv_id                 TYPE string OPTIONAL
        iv_style              TYPE string OPTIONAL
      RETURNING VALUE(rv_str) TYPE string.

    CLASS-METHODS icon
      IMPORTING
        iv_name               TYPE string
        iv_hint               TYPE string OPTIONAL
        iv_alt                TYPE string OPTIONAL
        iv_class              TYPE string OPTIONAL
      RETURNING VALUE(rv_str) TYPE string.

  PRIVATE SECTION.
    CLASS-DATA go_single_tags_re TYPE REF TO cl_abap_regex.
    DATA       mt_buffer         TYPE string_table.

    TYPES:
      BEGIN OF ty_indent_context,
        no_indent_jscss TYPE abap_bool,
        within_style    TYPE abap_bool,
        within_js       TYPE abap_bool,
        indent          TYPE i,
        indent_str      TYPE string,
      END OF ty_indent_context,

      BEGIN OF ty_study_result,
        style_open    TYPE abap_bool,
        style_close   TYPE abap_bool,
        script_open   TYPE abap_bool,
        script_close  TYPE abap_bool,
        tag_close     TYPE abap_bool,
        curly_close   TYPE abap_bool,
        openings      TYPE i,
        closings      TYPE i,
        singles       TYPE i,
      END OF ty_study_result.

    METHODS indent_line
      CHANGING
        cs_context TYPE ty_indent_context
        cv_line    TYPE string.

    METHODS study_line
      IMPORTING
        iv_line                  TYPE string
        is_context               TYPE ty_indent_context
      RETURNING VALUE(rs_result) TYPE ty_study_result.

ENDCLASS.                    "lcl_html DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_html IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html IMPLEMENTATION.

  METHOD add.

    DATA: lv_type TYPE c,
          lo_html TYPE REF TO lcl_html.

    FIELD-SYMBOLS: <tab> TYPE string_table,
                   <str> LIKE LINE OF <tab>.

    DESCRIBE FIELD iv_chunk TYPE lv_type. " Describe is faster than RTTI classes

    CASE lv_type.
      WHEN 'C' OR 'g'.  " Char or string
        APPEND iv_chunk TO mt_buffer.
      WHEN 'h'.         " Table
        ASSIGN iv_chunk TO <tab>. " Assuming table of strings ! Will dump otherwise
        APPEND LINES OF <tab> TO mt_buffer.
      WHEN 'r'.         " Object ref
        ASSERT iv_chunk IS BOUND. " Dev mistake
        TRY.
            lo_html ?= iv_chunk.
          CATCH cx_sy_move_cast_error.
            ASSERT 1 = 0. " Dev mistake
        ENDTRY.
        APPEND LINES OF lo_html->mt_buffer TO mt_buffer.
      WHEN OTHERS.
        ASSERT 1 = 0. " Dev mistake
    ENDCASE.

  ENDMETHOD.  " add

  METHOD reset.
    CLEAR me->mt_buffer.
  ENDMETHOD.                    "reset

  METHOD is_empty.
    rv_yes = boolc( lines( mt_buffer ) = 0 ).
  ENDMETHOD. "is_empty

  METHOD class_constructor.
    CREATE OBJECT go_single_tags_re
      EXPORTING
        pattern     = '<(AREA|BASE|BR|COL|COMMAND|EMBED|HR|IMG|INPUT|LINK|META|PARAM|SOURCE|!)'
        ignore_case = abap_false.
  ENDMETHOD. "class_constructor

  METHOD study_line.

    DATA: lv_line TYPE string,
          lv_len  TYPE i.

    lv_line = to_upper( shift_left( val = iv_line sub = ` ` ) ).
    lv_len  = strlen( lv_line ).

    " Some assumptions for simplification and speed
    " - style & scripts tag should be opened/closed in a separate line
    " - style & scripts opening and closing in one line is possible but only once

    " TODO & Issues
    " - What if the string IS a well formed html already not just single line ?

    IF is_context-within_js = abap_true OR is_context-within_style = abap_true.

      IF is_context-within_js = abap_true AND lv_len >= 8 AND lv_line(8) = '</SCRIPT'.
        rs_result-script_close = abap_true.
      ELSEIF is_context-within_style = abap_true AND lv_len >= 7 AND lv_line(7) = '</STYLE'.
        rs_result-style_close = abap_true.
      ENDIF.

      IF is_context-no_indent_jscss = abap_false.
        IF lv_len >= 1 AND lv_line(1) = '}'.
          rs_result-curly_close = abap_true.
        ENDIF.

        FIND ALL OCCURRENCES OF '{' IN lv_line MATCH COUNT rs_result-openings.
        FIND ALL OCCURRENCES OF '}' IN lv_line MATCH COUNT rs_result-closings.
      ENDIF.

    ELSE.
      IF lv_len >= 7 AND lv_line(7) = '<SCRIPT'.
        FIND FIRST OCCURRENCE OF '</SCRIPT' IN lv_line.
        IF sy-subrc > 0. " Not found
          rs_result-script_open = abap_true.
        ENDIF.
      ENDIF.
      IF lv_len >= 6 AND lv_line(6) = '<STYLE'.
        FIND FIRST OCCURRENCE OF '</STYLE' IN lv_line.
        IF sy-subrc > 0. " Not found
          rs_result-style_open = abap_true.
        ENDIF.
      ENDIF.
      IF lv_len >= 2 AND lv_line(2) = '</'.
        rs_result-tag_close = abap_true.
      ENDIF.

      FIND ALL OCCURRENCES OF '<'  IN lv_line MATCH COUNT rs_result-openings.
      FIND ALL OCCURRENCES OF '</' IN lv_line MATCH COUNT rs_result-closings.
      FIND ALL OCCURRENCES OF REGEX go_single_tags_re IN lv_line MATCH COUNT rs_result-singles.
      rs_result-openings = rs_result-openings - rs_result-closings - rs_result-singles.

    ENDIF.

  ENDMETHOD. "study_line

  METHOD indent_line.

    DATA: ls_study TYPE ty_study_result,
          lv_x_str TYPE string.

    ls_study = study_line(
      is_context = cs_context
      iv_line    = cv_line ).

    " First closing tag - shift back exceptionally
    IF (  ls_study-script_close = abap_true
       OR ls_study-style_close = abap_true
       OR ls_study-curly_close = abap_true
       OR ls_study-tag_close = abap_true )
       AND cs_context-indent > 0.
      lv_x_str = repeat( val = ` ` occ = ( cs_context-indent - 1 ) * c_indent_size ).
      cv_line  = lv_x_str && cv_line.
    ELSE.
      cv_line = cs_context-indent_str && cv_line.
    ENDIF.

    " Context status update
    CASE abap_true.
      WHEN ls_study-script_open.
        cs_context-within_js    = abap_true.
        cs_context-within_style = abap_false.
      WHEN ls_study-style_open.
        cs_context-within_js    = abap_false.
        cs_context-within_style = abap_true.
      WHEN ls_study-script_close OR ls_study-style_close.
        cs_context-within_js    = abap_false.
        cs_context-within_style = abap_false.
        ls_study-closings       = ls_study-closings + 1.
    ENDCASE.

    " More-less logic chosen due to possible double tags in a line '<a><b>'
    IF ls_study-openings <> ls_study-closings.
      IF ls_study-openings > ls_study-closings.
        cs_context-indent = cs_context-indent + 1.
      ELSEIF cs_context-indent > 0. " AND ls_study-openings < ls_study-closings
        cs_context-indent = cs_context-indent - 1.
      ENDIF.
      cs_context-indent_str = repeat( val = ` ` occ = cs_context-indent * c_indent_size ).
    ENDIF.

  ENDMETHOD. "indent_line

  METHOD render.

    DATA: ls_context TYPE ty_indent_context,
          lt_temp    TYPE string_table.

    FIELD-SYMBOLS: <line>   LIKE LINE OF lt_temp,
                   <line_c> LIKE LINE OF lt_temp.

    ls_context-no_indent_jscss = iv_no_indent_jscss.

    LOOP AT mt_buffer ASSIGNING <line>.
      APPEND <line> TO lt_temp ASSIGNING <line_c>.
      indent_line( CHANGING cs_context = ls_context cv_line = <line_c> ).
    ENDLOOP.

    CONCATENATE LINES OF lt_temp INTO rv_html SEPARATED BY gc_newline.

  ENDMETHOD.                    "render

  METHOD add_a.

    add( a( iv_txt   = iv_txt
            iv_act   = iv_act
            iv_typ   = iv_typ
            iv_opt   = iv_opt
            iv_class = iv_class
            iv_id    = iv_id
            iv_style = iv_style ) ).

  ENDMETHOD.                    "add_a

  METHOD a.

    DATA: lv_class TYPE string,
          lv_href  TYPE string,
          lv_id    TYPE string,
          lv_style TYPE string.

    lv_class = iv_class.

    IF iv_opt CA gc_html_opt-strong.
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

    rv_str = |<a{ lv_id }{ lv_class }{ lv_href }{ lv_style }>{ iv_txt }</a>|.

  ENDMETHOD. "a

  METHOD add_icon.

    add( icon( iv_name  = iv_name
               iv_class = iv_class
               iv_alt   = iv_alt
               iv_hint  = iv_hint ) ).

  ENDMETHOD.                    "add_icon

  METHOD icon.

    DATA: lv_hint  TYPE string,
          lv_name  TYPE string,
          lv_color TYPE string,
          lv_class TYPE string.

    SPLIT iv_name AT '/' INTO lv_name lv_color.

    IF iv_hint IS NOT INITIAL.
      lv_hint  = | title="{ iv_hint }"|.
    ENDIF.
    IF iv_class IS NOT INITIAL.
      lv_class = | { iv_class }|.
    ENDIF.
    IF lv_color IS NOT INITIAL.
      lv_color = | { lv_color }|.
    ENDIF.

    rv_str = |<i class="octicon octicon-{ lv_name }{ lv_color }{ lv_class }"{ lv_hint }></i>|.

  ENDMETHOD. "icon

ENDCLASS.                    "lcl_html IMPLEMENTATION

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
          VALUE(ro_html)            TYPE REF TO lcl_html.

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
          lv_is_drop TYPE abap_bool.

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
        ro_html->add_a( iv_txt   = iv_as_droplist_with_label
                        iv_class = 'dropbtn'
                        iv_act   = '' ).
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

      IF <ls_item>-sub IS INITIAL.

        IF iv_with_icons = abap_true.
          ro_html->add( '<tr>' ).
          ro_html->add( |<td class="icon">{ lcl_html=>icon( <ls_item>-ico ) }</td>| ).
          ro_html->add( '<td class="text">' ).
        ENDIF.

        ro_html->add_a( iv_txt   = <ls_item>-txt
                        iv_act   = <ls_item>-act
                        iv_opt   = <ls_item>-opt
                        iv_typ   = <ls_item>-typ ).

        IF iv_with_icons = abap_true.
          ro_html->add( '</td>' ).
          ro_html->add( '</tr>' ).
        ENDIF.

      ELSE.
        ro_html->add( <ls_item>-sub->render( iv_as_droplist_with_label = <ls_item>-txt ) ).
      ENDIF.

    ENDLOOP.

    IF iv_with_icons = abap_true.
      ro_html->add( '</table>' ).
    ENDIF.

    IF lv_is_drop = abap_true. " Dropdown
      ro_html->add( '</div>' ).
      ro_html->add( '</div>' ).
    ENDIF.

    ro_html->add( '</div>' ).

  ENDMETHOD.  "render

ENDCLASS. "lcl_html_toolbar IMPLEMENTATION