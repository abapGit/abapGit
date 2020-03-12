CLASS zcl_abapgit_html DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_html.

    ALIASES:
      add          FOR zif_abapgit_html~add,
      render       FOR zif_abapgit_html~render,
      is_empty     FOR zif_abapgit_html~is_empty,
      add_a        FOR zif_abapgit_html~add_a,
      add_checkbox FOR zif_abapgit_html~add_checkbox,
      a            FOR zif_abapgit_html~a,
      icon         FOR zif_abapgit_html~icon.

    CONSTANTS c_indent_size TYPE i VALUE 2 ##NO_TEXT.

    CLASS-METHODS class_constructor.
    METHODS add_icon
      IMPORTING
        !iv_name    TYPE string
        !iv_hint    TYPE string OPTIONAL
        !iv_class   TYPE string OPTIONAL
        !iv_onclick TYPE string OPTIONAL.
  PROTECTED SECTION.
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
      IMPORTING
        iv_line          TYPE string
        is_context       TYPE ty_indent_context
      RETURNING
        VALUE(rs_result) TYPE ty_study_result.
    METHODS checkbox
      IMPORTING
        iv_id          TYPE string
        iv_checked     TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_html) TYPE string.

ENDCLASS.



CLASS zcl_abapgit_html IMPLEMENTATION.


  METHOD add_icon.

    add( icon( iv_name    = iv_name
               iv_class   = iv_class
               iv_hint    = iv_hint
               iv_onclick = iv_onclick  ) ).

  ENDMETHOD.


  METHOD checkbox.

    DATA: lv_checked TYPE string.

    IF iv_checked = abap_true.
      lv_checked = |checked|.
    ENDIF.

    rv_html = |<input type="checkbox" id="{ iv_id }" { lv_checked }>|.

  ENDMETHOD.


  METHOD class_constructor.
    CREATE OBJECT go_single_tags_re
      EXPORTING
        pattern     = '<(AREA|BASE|BR|COL|COMMAND|EMBED|HR|IMG|INPUT|LINK|META|PARAM|SOURCE|!)'
        ignore_case = abap_false.
  ENDMETHOD.


  METHOD indent_line.

    DATA: ls_study TYPE ty_study_result,
          lv_x_str TYPE string.

    ls_study = study_line(
      is_context = cs_context
      iv_line    = cv_line ).

    " First closing tag - shift back exceptionally
    IF ( ls_study-script_close = abap_true
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

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD a.

    DATA: lv_class TYPE string,
          lv_href  TYPE string,
          lv_click TYPE string,
          lv_id    TYPE string,
          lv_style TYPE string,
          lv_title TYPE string.

    lv_class = iv_class.

    IF iv_opt CA zif_abapgit_html=>c_html_opt-strong.
      lv_class = lv_class && ' emphasis' ##NO_TEXT.
    ENDIF.
    IF iv_opt CA zif_abapgit_html=>c_html_opt-cancel.
      lv_class = lv_class && ' attention' ##NO_TEXT.
    ENDIF.
    IF iv_opt CA zif_abapgit_html=>c_html_opt-crossout.
      lv_class = lv_class && ' crossout grey' ##NO_TEXT.
    ENDIF.
    IF lv_class IS NOT INITIAL.
      SHIFT lv_class LEFT DELETING LEADING space.
      lv_class = | class="{ lv_class }"|.
    ENDIF.

    lv_href  = ' href="#"'. " Default, dummy
    IF ( iv_act IS NOT INITIAL OR iv_typ = zif_abapgit_html=>c_action_type-dummy )
        AND iv_opt NA zif_abapgit_html=>c_html_opt-crossout.
      CASE iv_typ.
        WHEN zif_abapgit_html=>c_action_type-url.
          lv_href  = | href="{ iv_act }"|.
        WHEN zif_abapgit_html=>c_action_type-sapevent.
          lv_href  = | href="sapevent:{ iv_act }"|.
        WHEN zif_abapgit_html=>c_action_type-onclick.
          lv_href  = ' href="#"'.
          lv_click = | onclick="{ iv_act }"|.
        WHEN zif_abapgit_html=>c_action_type-dummy.
          lv_href  = ' href="#"'.
      ENDCASE.
    ENDIF.

    IF iv_id IS NOT INITIAL.
      lv_id = | id="{ iv_id }"|.
    ENDIF.

    IF iv_style IS NOT INITIAL.
      lv_style = | style="{ iv_style }"|.
    ENDIF.

    IF iv_title IS NOT INITIAL.
      lv_title = | title="{ iv_title }"|.
    ENDIF.

    rv_str = |<a{ lv_id }{ lv_class }{ lv_href }{ lv_click }{ lv_style }{ lv_title }>|
          && |{ iv_txt }</a>|.

  ENDMETHOD.


  METHOD add.

    DATA: lv_type TYPE c,
          lo_html TYPE REF TO zcl_abapgit_html.

    FIELD-SYMBOLS: <lt_tab> TYPE string_table.

    DESCRIBE FIELD ig_chunk TYPE lv_type. " Describe is faster than RTTI classes

    CASE lv_type.
      WHEN 'C' OR 'g'.  " Char or string
        APPEND ig_chunk TO mt_buffer.
      WHEN 'h'.         " Table
        ASSIGN ig_chunk TO <lt_tab>. " Assuming table of strings ! Will dump otherwise
        APPEND LINES OF <lt_tab> TO mt_buffer.
      WHEN 'r'.         " Object ref
        ASSERT ig_chunk IS BOUND. " Dev mistake
        TRY.
            lo_html ?= ig_chunk.
          CATCH cx_sy_move_cast_error.
            ASSERT 1 = 0. " Dev mistake
        ENDTRY.
        APPEND LINES OF lo_html->mt_buffer TO mt_buffer.
      WHEN OTHERS.
        ASSERT 1 = 0. " Dev mistake
    ENDCASE.

  ENDMETHOD.


  METHOD add_a.

    add( a( iv_txt   = iv_txt
            iv_act   = iv_act
            iv_typ   = iv_typ
            iv_opt   = iv_opt
            iv_class = iv_class
            iv_id    = iv_id
            iv_style = iv_style
            iv_title = iv_title ) ).

  ENDMETHOD.


  METHOD zif_abapgit_html~add_checkbox.

    add( checkbox( iv_id      = iv_id
                   iv_checked = iv_checked ) ).

  ENDMETHOD.


  METHOD icon.

    DATA: lv_hint       TYPE string,
          lv_name       TYPE string,
          lv_color      TYPE string,
          lv_class      TYPE string,
          lv_large_icon TYPE string,
          lv_xpixel     TYPE i,
          lv_onclick    TYPE string.

    SPLIT iv_name AT '/' INTO lv_name lv_color.

    IF iv_hint IS NOT INITIAL.
      lv_hint  = | title="{ iv_hint }"|.
    ENDIF.
    IF iv_onclick IS NOT INITIAL.
      lv_onclick = | onclick="{ iv_onclick }"|.
    ENDIF.
    IF iv_class IS NOT INITIAL.
      lv_class = | { iv_class }|.
    ENDIF.
    IF lv_color IS NOT INITIAL.
      lv_color = | { lv_color }|.
    ENDIF.

    lv_xpixel = cl_gui_cfw=>compute_pixel_from_metric( x_or_y = 'X' in = 1 ).
    IF lv_xpixel >= 2.
      lv_large_icon = ' large'.
    ENDIF.

    rv_str = |<i class="icon{ lv_large_icon } icon-{ lv_name }{ lv_color }|.
    rv_str = |{ rv_str }{ lv_class }"{ lv_onclick }{ lv_hint }></i>|.

  ENDMETHOD.


  METHOD is_empty.
    rv_yes = boolc( lines( mt_buffer ) = 0 ).
  ENDMETHOD.


  METHOD render.

    DATA: ls_context TYPE ty_indent_context,
          lt_temp    TYPE string_table.

    FIELD-SYMBOLS: <lv_line>   LIKE LINE OF lt_temp,
                   <lv_line_c> LIKE LINE OF lt_temp.

    ls_context-no_indent_jscss = iv_no_indent_jscss.

    LOOP AT mt_buffer ASSIGNING <lv_line>.
      APPEND <lv_line> TO lt_temp ASSIGNING <lv_line_c>.
      indent_line( CHANGING cs_context = ls_context cv_line = <lv_line_c> ).
    ENDLOOP.

    CONCATENATE LINES OF lt_temp INTO rv_html SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.
ENDCLASS.
