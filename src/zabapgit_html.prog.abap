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
        iv_class TYPE string OPTIONAL.

    CLASS-METHODS a
      IMPORTING
                iv_txt        TYPE string
                iv_act        TYPE string
                iv_typ        TYPE char1  DEFAULT gc_action_type-sapevent
                iv_opt        TYPE clike  OPTIONAL
                iv_class      TYPE string OPTIONAL
                iv_id         TYPE string OPTIONAL
                iv_style      TYPE string OPTIONAL
      RETURNING VALUE(rv_str) TYPE string.

    CLASS-METHODS icon
      IMPORTING
                iv_name       TYPE string
                iv_hint       TYPE string OPTIONAL
                iv_class      TYPE string OPTIONAL
      RETURNING VALUE(rv_str) TYPE string.

  PRIVATE SECTION.
    CLASS-DATA: go_single_tags_re TYPE REF TO cl_abap_regex.

    DATA: mt_buffer TYPE string_table.

    TYPES:
      BEGIN OF ty_indent_context,
        no_indent_jscss TYPE abap_bool,
        within_style    TYPE abap_bool,
        within_js       TYPE abap_bool,
        indent          TYPE i,
        indent_str      TYPE string,
      END OF ty_indent_context,

      BEGIN OF ty_study_result,
        style_open   TYPE abap_bool,
        style_close  TYPE abap_bool,
        script_open  TYPE abap_bool,
        script_close TYPE abap_bool,
        tag_close    TYPE abap_bool,
        curly_close  TYPE abap_bool,
        openings     TYPE i,
        closings     TYPE i,
        singles      TYPE i,
      END OF ty_study_result.

    METHODS indent_line
      CHANGING
        cs_context TYPE ty_indent_context
        cv_line    TYPE string.

    METHODS study_line
      IMPORTING iv_line          TYPE string
                is_context       TYPE ty_indent_context
      RETURNING VALUE(rs_result) TYPE ty_study_result.

ENDCLASS.                    "lcl_html DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_html IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html IMPLEMENTATION.

  METHOD add.

    DATA: lv_type TYPE c,
          lo_html TYPE REF TO lcl_html.

    FIELD-SYMBOLS: <tab> TYPE string_table.

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
          lv_click TYPE string,
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

    lv_href  = ' href="#"'. " Default, dummy
    IF iv_act IS NOT INITIAL OR iv_typ = gc_action_type-dummy.
      CASE iv_typ.
        WHEN gc_action_type-url.
          lv_href  = | href="{ iv_act }"|.
        WHEN gc_action_type-sapevent.
          lv_href  = | href="sapevent:{ iv_act }"|.
        WHEN gc_action_type-onclick.
          lv_href  = ' href="#"'.
          lv_click = | onclick="{ iv_act }"|.
        WHEN gc_action_type-dummy.
          lv_href  = ' href="#"'.
      ENDCASE.
    ENDIF.

    IF iv_id IS NOT INITIAL.
      lv_id = | id="{ iv_id }"|.
    ENDIF.

    IF iv_style IS NOT INITIAL.
      lv_style = | style="{ iv_style }"|.
    ENDIF.

    rv_str = |<a{ lv_id }{ lv_class }{ lv_href }{ lv_click }{ lv_style }>{ iv_txt }</a>|.

  ENDMETHOD. "a

  METHOD add_icon.

    add( icon( iv_name  = iv_name
               iv_class = iv_class
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
      constructor
        IMPORTING
          iv_id TYPE string OPTIONAL,
      add
        IMPORTING
          iv_txt TYPE string
          io_sub TYPE REF TO lcl_html_toolbar OPTIONAL
          iv_typ TYPE c         DEFAULT gc_action_type-sapevent
          iv_act TYPE string    OPTIONAL
          iv_ico TYPE string    OPTIONAL
          iv_cur TYPE abap_bool OPTIONAL
          iv_opt TYPE c         OPTIONAL
          iv_chk TYPE abap_bool DEFAULT abap_undefined
          iv_aux TYPE string    OPTIONAL
          iv_id  TYPE string    OPTIONAL,
      count
        RETURNING VALUE(rv_count) TYPE i,
      render
        IMPORTING
          iv_right       TYPE abap_bool OPTIONAL
          iv_sort        TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(ro_html) TYPE REF TO lcl_html,
      render_as_droplist
        IMPORTING
          iv_label       TYPE string
          iv_right       TYPE abap_bool OPTIONAL
          iv_sort        TYPE abap_bool OPTIONAL
          iv_corner      TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(ro_html) TYPE REF TO lcl_html.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_item,
        txt TYPE string,
        act TYPE string,
        ico TYPE string,
        sub TYPE REF TO lcl_html_toolbar,
        opt TYPE char1,
        typ TYPE char1,
        cur TYPE abap_bool,
        chk TYPE abap_bool,
        aux TYPE string,
        id  TYPE string,
      END OF ty_item.

    TYPES tt_items TYPE STANDARD TABLE OF ty_item.

    DATA: mt_items TYPE tt_items,
          mv_id    TYPE string.

    METHODS:
      render_items
        IMPORTING
          iv_sort        TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(ro_html) TYPE REF TO lcl_html.

ENDCLASS. "lcl_html_toolbar DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_html_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html_toolbar IMPLEMENTATION.

  METHOD constructor.
    mv_id = iv_id.
  ENDMETHOD. "constructor

  METHOD count.
    rv_count = lines( mt_items ).
  ENDMETHOD.

  METHOD add.
    DATA ls_item TYPE ty_item.

    ASSERT iv_typ = gc_action_type-separator  " sep doesn't have action
      OR iv_typ = gc_action_type-onclick      " click may have no action (assigned in JS)
      OR iv_typ = gc_action_type-dummy        " dummy may have no action
      OR iv_act IS INITIAL AND io_sub IS NOT INITIAL
      OR iv_act IS NOT INITIAL AND io_sub IS INITIAL. " Only one supplied

    ASSERT NOT ( iv_chk <> abap_undefined AND io_sub IS NOT INITIAL ).

    ls_item-txt = iv_txt.
    ls_item-act = iv_act.
    ls_item-ico = iv_ico.
    ls_item-sub = io_sub.
    ls_item-opt = iv_opt.
    ls_item-typ = iv_typ.
    ls_item-cur = iv_cur.
    ls_item-chk = iv_chk.
    ls_item-aux = iv_aux.
    ls_item-id  = iv_id.

    APPEND ls_item TO mt_items.

  ENDMETHOD.  "add

  METHOD render.

    DATA: lv_class TYPE string.

    CREATE OBJECT ro_html.

    lv_class = 'nav-container' ##NO_TEXT.
    IF iv_right = abap_true.
      lv_class = lv_class && ' float-right'.
    ENDIF.

    ro_html->add( |<div class="{ lv_class }">| ).
    ro_html->add( render_items( iv_sort = iv_sort ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render

  METHOD render_as_droplist.

    DATA: lv_class TYPE string.

    CREATE OBJECT ro_html.

    lv_class = 'nav-container' ##NO_TEXT.
    IF iv_right = abap_true.
      lv_class = lv_class && ' float-right'.
    ENDIF.
    IF iv_corner = abap_true.
      lv_class = lv_class && ' corner'.
    ENDIF.

    ro_html->add( |<div class="{ lv_class }">| ).
    ro_html->add( '<ul><li>' ).
    ro_html->add_a( iv_txt = iv_label
                    iv_typ = gc_action_type-dummy
                    iv_act = '' ).
    ro_html->add( '<div class="minizone"></div>' ).
    ro_html->add( render_items( iv_sort = iv_sort ) ).
    ro_html->add( '</li></ul>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD. "render_as_droplist

  METHOD render_items.

    DATA: lv_class     TYPE string,
          lv_icon      TYPE string,
          lv_id        TYPE string,
          lv_check     TYPE string,
          lv_aux       TYPE string,
          lv_has_icons TYPE abap_bool.

    FIELD-SYMBOLS <item> LIKE LINE OF mt_items.

    CREATE OBJECT ro_html.

    IF iv_sort = abap_true.
      SORT mt_items BY txt ASCENDING AS TEXT.
    ENDIF.

    " Check has icons or check boxes
    LOOP AT mt_items ASSIGNING <item> WHERE ico IS NOT INITIAL OR chk <> abap_undefined.
      lv_has_icons = abap_true.
      lv_class     = ' class="with-icons"'.
      EXIT.
    ENDLOOP.

    IF mv_id IS NOT INITIAL.
      lv_id = | id="{ mv_id }"|.
    ENDIF.

    ro_html->add( |<ul{ lv_id }{ lv_class }>| ).

    " Render items
    LOOP AT mt_items ASSIGNING <item>.
      CLEAR: lv_class, lv_icon.

      IF <item>-typ = gc_action_type-separator.
        ro_html->add( |<li class="separator">{ <item>-txt }</li>| ).
        CONTINUE.
      ENDIF.

      IF lv_has_icons = abap_true.
        IF <item>-chk = abap_true.
          lv_icon  = lcl_html=>icon( 'check/blue' ).
          lv_check = ' data-check="X"'.
        ELSEIF <item>-chk = abap_false.
          lv_icon = lcl_html=>icon( 'check/grey' ).
          lv_check = ' data-check=""'.
        ELSE. " abap_undefined -> not a check box
          lv_icon = lcl_html=>icon( <item>-ico ).
        ENDIF.
      ENDIF.

      IF <item>-cur = abap_true.
        lv_class = ' class="current-menu-item"'.
      ENDIF.

      IF <item>-aux IS NOT INITIAL.
        lv_aux = | data-aux="{ <item>-aux }"|.
      ENDIF.

      ro_html->add( |<li{ lv_class }{ lv_check }{ lv_aux }>| ).
      IF <item>-sub IS INITIAL.
        ro_html->add_a( iv_txt   = lv_icon && <item>-txt
                        iv_typ   = <item>-typ
                        iv_act   = <item>-act
                        iv_id    = <item>-id
                        iv_opt   = <item>-opt ).
      ELSE.
        ro_html->add_a( iv_txt   = lv_icon && <item>-txt
                        iv_typ   = gc_action_type-dummy
                        iv_act   = ''
                        iv_id    = <item>-id
                        iv_opt   = <item>-opt ).
        ro_html->add( <item>-sub->render_items( iv_sort = iv_sort ) ).
      ENDIF.
      ro_html->add( '</li>' ).

    ENDLOOP.

    ro_html->add( '</ul>' ).

  ENDMETHOD.  "render_items


ENDCLASS. "lcl_html_toolbar IMPLEMENTATION
